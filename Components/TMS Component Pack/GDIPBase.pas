{*************************************************************************}
{ TGDIPBase base list and appearance class                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2010 - 2015                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit GDIPBase;

interface

{$I TMSDEFS.INC}

uses
  Windows, Forms, Messages, Controls, Graphics, Types, Classes,
  GDIPFill, AdvGDIP, contnrs, Math, SysUtils, Dialogs,
  GDIPCustomItem, GDIPPictureContainer, ImgList;

type

  TArrayOfCustomItems = array of TCustomItem;

  TCustomItems = array of TCustomItem;

  TCustomBaseList = class;

  TCustomListObjects = class(TObjectList)
  private
    FOwner: TCustomBaseList;
  protected
    procedure QuickSort(SortList: PPointerList; L, R: Integer);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure UpdateIndexes;
    procedure SortListItems;
  end;

  TCustomBaseListAppearance = class(TPersistent)
  private
    FHovered: TGDIPFill;
    FDown: TGDIPFill;
    FNormal: TGDIPFill;
    FDisabled: TGDIPFill;
    FSelected: TGDIPFill;
    FOnChange: TNotifyEvent;
    FNormalFont: TFont;
    FDisabledFont: TFont;
    FSelectedFont: TFont;
    FHoveredFont: TFont;
    FDownFont: TFont;
    FButtonHovered: TGDIPFill;
    FButtonDown: TGDIPFill;
    FButtonNormal: TGDIPFill;
    FButtonDisabled: TGDIPFill;
    FButtonSelected: TGDIPFill;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    procedure SetDisabled(const Value: TGDIPFill);
    procedure SetDown(const Value: TGDIPFill);
    procedure SetHovered(const Value: TGDIPFill);
    procedure SetNormal(const Value: TGDIPFill);
    procedure SetSelected(const Value: TGDIPFill);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetDownFont(const Value: TFont);
    procedure SetHoveredFont(const Value: TFont);
    procedure SetNormalFont(const Value: TFont);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetButtonDisabled(const Value: TGDIPFill);
    procedure SetButtonDown(const Value: TGDIPFill);
    procedure SetButtonHovered(const Value: TGDIPFill);
    procedure SetButtonNormal(const Value: TGDIPFill);
    procedure SetButtonSelected(const Value: TGDIPFill);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure Assign(Source: TPersistent); override;
  published
    property Normal: TGDIPFill read FNormal write SetNormal;
    property ButtonNormal: TGDIPFill read FButtonNormal write SetButtonNormal;
    property Down: TGDIPFill read FDown write SetDown;
    property ButtonDown: TGDIPFill read FButtonDown write SetButtonDown;
    property Disabled: TGDIPFill read FDisabled write SetDisabled;
    property ButtonDisabled: TGDIPFill read FButtonDisabled write SetButtonDisabled;
    property Hovered: TGDIPFill read FHovered write SetHovered;
    property ButtonHovered: TGDIPFill read FButtonHovered write SetButtonHovered;
    property Selected: TGDIPFill read FSelected write SetSelected;
    property ButtonSelected: TGDIPFill read FButtonSelected write SetButtonSelected;
    property NormalFont: TFont read FNormalFont write SetNormalFont;
    property DownFont: TFont read FDownFont write SetDownFont;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;
    property HoveredFont: TFont read FHoveredFont write SetHoveredFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write FPictureContainer;
    property ImageList: TCustomImageList read FImageList write FImageList;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TItemCompareEvent = procedure(Sender: TObject; Item1, Item2: TCustomItem; var Result: integer) of object;

  TItemDesignTimeSelect = procedure(Sender: TObject; Index: integer) of object;

  TItemAppearanceEvent = procedure(Sender: TObject; Item: TCustomItem; Appearance: TItemAppearance) of object;

  TItemReorderEvent = procedure(Sender: TObject; AItem, ADropItem: TCustomItem; var Allow: Boolean) of object;

  TWinControlAccess = class(TWinControl);

  TCustomBaseList = class(TPersistent)
  private
    FFocusedItemIndex: integer;
    FFirstVisibleIndex, FLastVisibleIndex: integer;
    GlobalCheck: Boolean;
    FOwnerComponent: TComponent;
    FUpdateCount: integer;
    FItems: TCustomListObjects;
    FAppearance: TCustomBaseListAppearance;
    FOnChange: TNotifyEvent;
    FMultiSelect: Boolean;
    FOnInternalChange: TNotifyEvent;
    FOnNotifyItemChange: TNotifyEvent;
    FOnNotifyItemDestroy: TNotifyEvent;
    FOnNotifyListDestroy: TNotifyEvent;
    FOnInternalRefresh: TNotifyEvent;
    FOnRefresh: TNotifyEvent;
    FWidth: integer;
    FHeight: integer;
    FOnItemCompare: TItemCompareEvent;
    FOnItemSelect: TItemSelectEvent;
    FOnNotifyItemDesignTimeSelect: TItemDesignTimeSelect;
    FOnItemDeSelect: TItemSelectEvent;
    FOnItemDestroy: TItemEvent;
    FOnItemAppearance: TItemAppearanceEvent;
    FMetroStyle: Boolean;
    FHTMLCache: Boolean;
    procedure SetAppearance(const Value: TCustomBaseListAppearance);
    procedure SetItems(const Value: TCustomListObjects);
    procedure SetMultiSelect(const Value: Boolean);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
  protected
    function ItemInteraction(pX, pY: integer; it: TCustomItem): TItemInteractionType;
    function Compare(Item1, Item2: Pointer): integer;
    function GetTopParent(AControl: TWinControl): TWinControl;
    function CreateAppearance: TItemAppearance;
    procedure CopyAppearance(Appearance: TItemAppearance; var CopyAppearance: TItemAppearance);
    procedure DestroyAppearance(Appearance: TItemAppearance);
  public
    property MetroStyle: Boolean read FMetroStyle write FMetroStyle;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignList(Source: TCustomBaseList);
    procedure Changed;
    procedure RefreshObjects;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateVisibleIndexes;
    procedure DoGlobalCheckChanged(Sender: TObject; Item: TCustomItem; Checked: Boolean);
    procedure DoItemIndexChange(Sender: TObject; Item: TCustomItem; OldIndex, NewIndex: integer);
    procedure DoItemRefresh(Sender: TObject);
    procedure DoItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
    procedure DoItemDeSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
    procedure DoItemChanged(Sender: TObject);
    procedure DoItemFocus(Sender: TObject; Item: TCustomItem);
    procedure DoItemDestroy(Sender: TObject; Item: TCustomItem);
    procedure AppearanceChanged(Sender: TObject);
    procedure DoExit;
    procedure DoEnter;
    procedure DoCMMouseLeave(var Message: TMessage);
    procedure DoCMDialogChar(var Message: TCMDialogChar);
    procedure DoWMKeyDown(var Message: TWMKeyDown);
    procedure DoCMHintShow(var Message: TMessage);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; pX, pY: Integer);
    procedure DoDblClick(Sender: TObject; Pos: TPoint);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; pX, pY: Integer);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; pX, pY: Integer);
    procedure DoKeyDownGrid(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyDownBox(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ProcessKeyGrid(Key: Word; var SelRow, SelCol: integer);
    procedure ProcessKeyBox(Key: Word; var ItemIndex: integer);    
    procedure DrawItems(g: TGPGraphics; Focused: Boolean);
    procedure ConvertItem(it: TCustomItem; AItemClass: TCustomItemClass);
    procedure RemoveItem(AIndex: Integer);
    procedure SelectItem(AIndex: Integer);
    procedure UnSelectItem(AIndex: Integer);
    procedure ResetItemStatus;
    procedure ClearItemState;
    procedure Sort;
    procedure ShowShortCutHints;
    procedure HideShortCutHints;
    function FirstVisibleIdx: integer;
    function LastVisibleIdx: integer;
    procedure HideItemsByLevel(StartIndex: integer; ALevel: integer; ALevelStatus: TItemLevelStatus);
    procedure ShowItemsByLevel(StartIndex: integer; ALevel: integer; ALevelStatus: TItemLevelStatus);
    procedure AssignEvents(it: TCustomItem);
    procedure SetOwnerComponent(AOwner: TComponent);
    function AddItem(it: TCustomItem): TCustomItem;
    function InsertItem(Index: integer; it: TCustomItem): TCustomItem;
    function GetItemClassByCustomName(ACustomName: String): TComponentClass;
    function ItemInteractionAtXY(pX, pY: integer): TItemInteraction;
    function ItemAtXY(pX, pY: integer): TCustomItem;
    function SelectedItem: TCustomItem;
    procedure FocusNextItem(backwards: Boolean);
    function ItemInRectangle(It: TCustomItem; DisplayRect: TRect): Boolean;
    function GetFirstVisibleIndex: integer;
    function GetLastVisibleIndex: integer;
    function IsItemSelectable(it: TCustomItem): Boolean;
    function IsItemFocusable(it: TCustomItem): Boolean;
    function GetCountSelectableItems: Integer;
    function GetCountFocusableItems: Integer;
    function ItemAtRowCol(Row, Col: integer): TCustomItem;
    function GetItemsByTag(ATag: Integer): TCustomItems;
    function GetOwnerComponent: TComponent;
    function MaxSelectableRowIndex: integer;
    function MaxSelectableColIndex: integer;
    function MinSelectableRowIndex: integer;
    function MinSelectableColIndex: integer;
    function MaxRowIndex: integer;
    function MaxColIndex: integer;
    function MaxRowItem: TCustomItem;
    function MaxColumnItem: TCustomItem;
    function MaxBoxRowItem: TCustomItem;
    function MaxBoxColumnItem: TCustomItem;
    function MinSelectableItemIndex: integer;
    function MaxSelectableItemIndex: integer;
    function MinFocusableItemIndex: integer;
    function MaxFocusableItemIndex: integer;
    function RowCount: integer;
    function ColumnCount: integer;
    function GetItemAppearance: TItemAppearance;
    property Width: integer read FWidth write SetWidth default 150;
    property Height: integer read FHeight write SetHeight default 200;
    property OnInternalRefresh: TNotifyEvent read FOnInternalRefresh write FOnInternalRefresh;
    property OnInternalChange: TNotifyEvent read FOnInternalChange write FOnInternalChange;
    property OnNotifyItemDestroy: TNotifyEvent read FOnNotifyItemDestroy write FOnNotifyItemDestroy;
    property OnNotifyListDestroy: TNotifyEvent read FOnNotifyListDestroy write FOnNotifyListDestroy;
    property OnNotifyItemChange: TNotifyEvent read FOnNotifyItemChange write FOnNotifyItemChange;
    property OnNotifyItemDesignTimeSelect: TItemDesignTimeSelect read FOnNotifyItemDesignTimeSelect write FOnNotifyItemDesignTimeSelect;
    property OnItemCompare: TItemCompareEvent read FOnItemCompare write FOnItemCompare;
    property OnItemSelect: TItemSelectEvent read FOnItemSelect write FOnItemSelect;
    property OnItemDeSelect: TItemSelectEvent read FOnItemDeSelect write FOnItemDeSelect;
    property OnItemDestroy: TItemEvent read FOnItemDestroy write FOnItemDestroy;
    property OnItemAppearance: TItemAppearanceEvent read FOnItemAppearance write FOnItemAppearance;
    property FocusedItemIndex: Integer read FFocusedItemIndex write FFocusedItemIndex;
    property HTMLCache: Boolean read FHTMLCache write FHTMLCache default True;
  published
    property Items: TCustomListObjects read FItems write SetItems;
    property Appearance: TCustomBaseListAppearance read FAppearance write SetAppearance;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default false;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  
  TSubClassList = class(TClassList)
  private
    FParentClass: TPersistentClass;
    procedure CallbackClass(AClass: TPersistentClass);
  public
    procedure Execute(const aClass:TPersistentClass);
  end;

implementation

{ TCustomBaseListAppearance }

procedure TCustomBaseListAppearance.Assign(Source: TPersistent);
begin
  if (Source is TCustomBaseListAppearance) then
  begin
    FHovered.Assign((Source as TCustomBaseListAppearance).Hovered);
    FDown.Assign((Source as TCustomBaseListAppearance).Down);
    FNormal.Assign((Source as TCustomBaseListAppearance).Normal);
    FDisabled.Assign((Source as TCustomBaseListAppearance).Disabled);
    FSelected.Assign((Source as TCustomBaseListAppearance).Selected);
    FNormalFont.Assign((Source as TCustomBaseListAppearance).NormalFont);
    FDisabledFont.Assign((Source as TCustomBaseListAppearance).DisabledFont);
    FSelectedFont.Assign((Source as TCustomBaseListAppearance).SelectedFont);
    FHoveredFont.Assign((Source as TCustomBaseListAppearance).HoveredFont);
    FDownFont.Assign((Source as TCustomBaseListAppearance).DownFont);
    FButtonHovered.Assign((Source as TCustomBaseListAppearance).ButtonHovered);
    FButtonDown.Assign((Source as TCustomBaseListAppearance).ButtonDown);
    FButtonNormal.Assign((Source as TCustomBaseListAppearance).ButtonNormal);
    FButtonDisabled.Assign((Source as TCustomBaseListAppearance).ButtonDisabled);
    FButtonSelected.Assign((Source as TCustomBaseListAppearance).ButtonSelected);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TCustomBaseListAppearance.Create;
begin
  FHovered := TGDIPFill.Create;
  FHovered.OnChange := FillChanged;
  FNormal := TGDIPFill.Create;
  FNormal.OnChange := FillChanged;
  FDisabled := TGDIPFill.Create;
  FDisabled.OnChange := FillChanged;
  FSelected := TGDIPFill.Create;
  FSelected.OnChange := FillChanged;
  FDown := TGDIPFill.Create;
  FDown.OnChange := FillChanged;

  FButtonHovered := TGDIPFill.Create;
  FButtonHovered.OnChange := FillChanged;
  FButtonNormal := TGDIPFill.Create;
  FButtonNormal.OnChange := FillChanged;
  FButtonDisabled := TGDIPFill.Create;
  FButtonDisabled.OnChange := FillChanged;
  FButtonSelected := TGDIPFill.Create;
  FButtonSelected.OnChange := FillChanged;
  FButtonDown := TGDIPFill.Create;
  FButtonDown.OnChange := FillChanged;

  FHoveredFont := TFont.Create;
  FHoveredFont.OnChange := FontChanged;
  FNormalFont := TFont.Create;
  FNormalFont.OnChange := FontChanged;
  FDisabledFont := TFont.Create;
  FDisabledFont.OnChange := FontChanged;
  FSelectedFont := TFont.Create;
  FSelectedFont.OnChange := FontChanged;
  FDownFont := TFont.Create;
  FDownFont.OnChange := FontChanged;


  {$IFNDEF DELPHI9_LVL}
  FHoveredFont.Name := 'Tahoma';
  FNormalFont.Name := 'Tahoma';
  FDisabledFont.Name := 'Tahoma';
  FSelectedFont.Name := 'Tahoma';
  FDownFont.Name := 'Tahoma';
  {$ENDIF}
end;

destructor TCustomBaseListAppearance.Destroy;
begin
  FHovered.Free;
  FNormal.Free;
  FDisabled.Free;
  FSelected.Free;
  FDown.Free;
  FButtonHovered.Free;
  FButtonNormal.Free;
  FButtonDisabled.Free;
  FButtonSelected.Free;
  FButtonDown.Free;
  FHoveredFont.Free;
  FNormalFont.Free;
  FDisabledFont.Free;
  FSelectedFont.Free;
  FDownFont.Free;
  inherited;
end;

procedure TCustomBaseListAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomBaseListAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomBaseListAppearance.SetButtonDisabled(
  const Value: TGDIPFill);
begin
  if FButtonDisabled <> Value then
  begin
    FButtonDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetButtonDown(const Value: TGDIPFill);
begin
  if FButtonDown <> Value then
  begin
    FButtonDown.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetButtonHovered(
  const Value: TGDIPFill);
begin
  if FButtonHovered <> Value then
  begin
    FButtonHovered.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetButtonNormal(
  const Value: TGDIPFill);
begin
  if FButtonNormal <> Value then
  begin
    FButtonNormal.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetButtonSelected(
  const Value: TGDIPFill);
begin
  if FButtonSelected <> Value then
  begin
    FButtonSelected.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetDisabled(const Value: TGDIPFill);
begin
  if FDisabled <> value then
  begin
    FDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetDisabledFont(const Value: TFont);
begin
  if FDisabledFont <> Value then
  begin
    FDisabledFont.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetDown(const Value: TGDIPFill);
begin
  if FDown <> value then
  begin
    FDown.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetDownFont(const Value: TFont);
begin
  if FDownFont <> Value then
  begin
    FDownFont.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetHovered(const Value: TGDIPFill);
begin
  if FHovered <> value then
  begin
    FHovered.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetHoveredFont(const Value: TFont);
begin
  if FHoveredFont <> Value then
  begin
    FHoveredFont.Assign(value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetNormal(const Value: TGDIPFill);
begin
  if FNormal <> value then
  begin
    FNormal.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetNormalFont(const Value: TFont);
begin
  if FNormalFont <> Value then
  begin
    FNormalFont.Assign(value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetSelected(const Value: TGDIPFill);
begin
  if FSelected <> value then
  begin
    FSelected.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseListAppearance.SetSelectedFont(const Value: TFont);
begin
  if FSelectedFont <> Value then
  begin
    FSelectedFont.Assign(Value);
    Changed;
  end;
end;

function TCustomBaseList.ItemInteraction(pX, pY: integer; it: TCustomItem): TItemInteractionType;
begin
  Result := itNone;
  if Assigned(it) then
    Result := it.GetItemInteraction(pX, pY);
end;

function TCustomBaseList.AddItem(it: TCustomItem): TCustomItem;
begin
  BeginUpdate;
  AssignEvents(it);
  Items.Add(it);
  Result := it;
  EndUpdate;
end;

procedure TCustomBaseList.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomBaseList.Assign(Source: TPersistent);
begin
  if (Source is TCustomBaseList) then
  begin
    AssignList((Source as TCustomBaseList));
    FAppearance.Assign((Source as TCustomBaseList).Appearance);
    FMultiSelect := (Source as TCustomBaseList).MultiSelect;
    Changed;
  end;
end;

procedure TCustomBaseList.AssignEvents(it: TCustomItem);
begin
  it.OnInternalIndexChange := DoItemIndexChange;
  it.OnInternalRefresh := DoItemRefresh;
  it.OnInternalChange := DoItemChanged;
  it.OnGlobalCheckChanged := DoGlobalCheckChanged;
  it.OnInternalItemSelect := DoItemSelect;
  it.OnInternalItemDeSelect := DoItemDeSelect;
  it.OnInternalFocus := DoItemFocus;
  it.OnInternalDestroy := DoItemDestroy;
end;

procedure TCustomBaseList.AssignList(Source: TCustomBaseList);
var
  I: Integer;
  res: TCustomItem;
begin
  Items.clear;
  for I := 0 to Source.Items.Count - 1 do
  begin
    if Source.Items[i] is TCustomItem then
    begin
      res := TCustomItem(Source.Items[i]).CreateNewItem(FOwnerComponent);
      res.Assign(TCustomItem(Source.Items[i]));
      AssignEvents(res);
      Items.Add(res);
    end;
  end;
end;

procedure TCustomBaseList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomBaseList.Changed;
begin
  if Assigned(FOnChange) and (FUpdateCount = 0) then
    FOnChange(Self);

  if Assigned(FOnInternalChange) and (FUpdateCount = 0) then
    FOnInternalChange(Self);
end;

procedure TCustomBaseList.ClearItemState;
var
  i: integer;
begin
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).ClearItemState;
    end;
  end;
end;

procedure TCustomBaseList.ConvertItem(it: TCustomItem;
  AItemClass: TCustomItemClass);
var
  idx: integer;
  itnew: TCustomItem;
begin
  BeginUpdate;
  idx := it.Index;
  itnew := AItemClass.Create(Self.FOwnerComponent);
  AddItem(itnew);
  itnew.Assign(it);
  it.Free;
  itnew.Index := idx;
  EndUpdate;
end;

constructor TCustomBaseList.Create(AOwner: TComponent);
begin
  FFirstVisibleIndex := 0;
  FLastVisibleIndex := -1;
  FOwnerComponent := AOwner;
  FAppearance := TCustomBaseListAppearance.Create;
  FAppearance.OnChange := AppearanceChanged;
  FItems := TCustomListObjects.Create;
  FItems.FOwner := Self;
  FMultiSelect := false;
  FWidth := 150;
  FHeight := 200;
  FFocusedItemIndex := 0;
  FHTMLCache := True;
end;


function TCustomBaseList.CreateAppearance: TItemAppearance;
begin
  Result.Disabled := TGDIPFill.Create;
  Result.Normal := TGDIPFill.Create;
  Result.Down := TGDIPFill.Create;
  Result.Hover := TGDIPFill.Create;
  Result.Selected := TGDIPFill.Create;

  Result.ButtonDisabled := TGDIPFill.Create;
  Result.ButtonNormal := TGDIPFill.Create;
  Result.ButtonDown := TGDIPFill.Create;
  Result.ButtonHover := TGDIPFill.Create;
  Result.ButtonSelected := TGDIPFill.Create;

  Result.DisabledFont := TFont.Create;
  Result.NormalFont := TFont.Create;
  Result.DownFont := TFont.Create;
  Result.HoverFont := TFont.Create;
  Result.SelectedFont := TFont.Create;
  Result.PictureContainer := nil;
  Result.ImageList := nil;
  Result.IsMetroStyle := MetroStyle;
  Result.DrawHTMLCache := True;
end;

procedure TCustomBaseList.CopyAppearance(Appearance: TItemAppearance;var CopyAppearance: TItemAppearance);
begin
  CopyAppearance.Disabled.Assign(Appearance.Disabled);
  CopyAppearance.Normal.Assign(Appearance.Normal);
  CopyAppearance.Down.Assign(Appearance.Down);
  CopyAppearance.Hover.Assign(Appearance.Hover);
  CopyAppearance.Selected.Assign(Appearance.Selected);

  CopyAppearance.ButtonDisabled.Assign(Appearance.ButtonDisabled);
  CopyAppearance.ButtonNormal.Assign(Appearance.ButtonNormal);
  CopyAppearance.ButtonDown.Assign(Appearance.ButtonDown);
  CopyAppearance.ButtonHover.Assign(Appearance.ButtonHover);
  CopyAppearance.ButtonSelected.Assign(Appearance.ButtonSelected);

  CopyAppearance.DisabledFont.Assign(Appearance.DisabledFont);
  CopyAppearance.NormalFont.Assign(Appearance.NormalFont);
  CopyAppearance.DownFont.Assign(Appearance.DownFont);
  CopyAppearance.HoverFont.Assign(Appearance.HoverFont);
  CopyAppearance.SelectedFont.Assign(Appearance.SelectedFont);

  CopyAppearance.Focus := Appearance.Focus;
  CopyAppearance.FocusedItem := Appearance.FocusedItem;
  CopyAppearance.PictureContainer := Appearance.PictureContainer;
  CopyAppearance.ImageList := Appearance.ImageList;
  CopyAppearance.IsMetroStyle := Appearance.IsMetroStyle;
  CopyAppearance.DrawHTMLCache := Appearance.DrawHTMLCache;
end;

destructor TCustomBaseList.Destroy;
begin
  FAppearance.Free;
  BeginUpdate;
  FItems.Free;
  EndUpdate;
  inherited;
end;

procedure TCustomBaseList.DestroyAppearance(Appearance: TItemAppearance);
begin
  Appearance.Disabled.Free;
  Appearance.Normal.Free;
  Appearance.Down.Free;
  Appearance.Hover.Free;
  Appearance.Selected.Free;

  Appearance.ButtonDisabled.Free;
  Appearance.ButtonNormal.Free;
  Appearance.ButtonDown.Free;
  Appearance.ButtonHover.Free;
  Appearance.ButtonSelected.Free;

  Appearance.DisabledFont.Free;
  Appearance.NormalFont.Free;
  Appearance.DownFont.Free;
  Appearance.HoverFont.Free;
  Appearance.SelectedFont.Free;
end;

procedure TCustomBaseList.DoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sel: TCustomItem;
begin
  if (FFocusedItemIndex >= 0) and (FFocusedItemIndex <= Items.Count - 1) then
  begin
    if Items[FFocusedItemIndex] is TCustomItem then
    begin
      sel := TCustomItem(Items[FFocusedItemIndex]);
      if Assigned(sel) then
        sel.DoKeyDown(Sender, Key, Shift);
    end;
  end;
end;

procedure TCustomBaseList.DoKeyDownBox(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  idx, sel, prev, selidx: integer;
  Item: TCustomItem;
begin
  if Key in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
  begin
    prev := -1;
    selidx := -1;
    if SelectedItem <> nil then
    begin
      sel := SelectedItem.Index;
      prev := sel;
      selidx := sel;
    end;

    ProcessKeyBox(Key, selidx);

    if GetCountSelectableItems > 0 then
    begin         
      Item := nil;
      if (selidx >= 0) and (selidx <= Items.Count - 1) then         
        Item := TCustomItem(Items[selidx]);
      idx := -1;
      if Assigned(item) then
        idx := item.Index;

      while (idx = -1) or not IsItemSelectable(TCustomItem(Items[idx])) do
      begin
        ProcessKeyBox(Key, selidx); 
        Item := nil;
        if (selidx >= 0) and (selidx <= Items.Count - 1) then         
          Item := TCustomItem(Items[selidx]);

        if Assigned(item) then
          idx := item.Index;
      end;

      sel := idx;

      if (prev <> sel) then
        SelectItem(sel);
    end;
  end;
end;

procedure TCustomBaseList.DoKeyDownGrid(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  idx, sel, prev, selcol, selrow: integer;
  Item: TCustomItem;
  maxrow, maxcol: integer;
begin
  if Key in [VK_LEFT, VK_UP, VK_RIGHT, VK_DOWN, VK_HOME, VK_END, VK_PRIOR, VK_NEXT] then
  begin
    prev := -1;
    selcol := -1;
    selrow := -1;
    if SelectedItem <> nil then
    begin
      sel := SelectedItem.Index;
      prev := sel;
      selcol := Selecteditem.ColumnIndex;
      selrow := SelectedItem.RowIndex;
    end;

    maxrow := MaxRowIndex;
    maxcol := MaxColIndex;

    ProcessKeyGrid(Key, selRow, selCol);

    selrow := Max(0, Min(selrow, maxrow));
    selcol := Max(0, Min(selcol, maxcol));

    if GetCountSelectableItems > 0 then
    begin
      Item := ItemAtRowCol(selrow, selcol);
      idx := -1;
      if Assigned(item) then
        idx := item.Index;

      while (idx = -1) or not IsItemSelectable(TCustomItem(Items[idx])) do
      begin
        ProcessKeyGrid(Key, selRow, selCol);
        selrow := Max(0, Min(selrow, maxrow));
        selcol := Max(0, Min(selcol, maxcol));
        Item := ItemAtRowCol(selrow, selcol);
        if Assigned(item) then
          idx := item.Index;
      end;

      sel := idx;

      if (prev <> sel) then
        SelectItem(sel);
    end;
  end;
end;

procedure TCustomBaseList.DoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  sel: TCustomItem;
begin
  if (FFocusedItemIndex >= 0) and (FFocusedItemIndex <= Items.Count - 1) then
  begin
    if Items[FFocusedItemIndex] is TCustomItem then
    begin
      sel := TCustomItem(Items[FFocusedItemIndex]);
      if Assigned(sel) then
        sel.DoKeyUp(Sender, Key, Shift);
    end;
  end;
end;

procedure TCustomBaseList.DoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; pX, pY: Integer);
var
  I: Integer;
  itr: TItemInteraction;
begin
  if Button = mbRight then
    Exit;

  itr := ItemInteractionAtXY(pX, pY);
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).DoMouseDown(Sender, Button, Shift, pX, pY, itr, GetItemAppearance);
    end;
  end;
end;

procedure TCustomBaseList.DoMouseMove(Sender: TObject; Shift: TShiftState;
  pX, pY: Integer);
var
  I: Integer;
  itr: TItemInteraction;
begin
  itr := ItemInteractionAtXY(pX, pY);
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if (Items[i] is TCustomItem) then
        TCustomItem(Items[i]).DoMouseMove(Sender, Shift, pX, pY, itr, GetItemAppearance);
    end;
  end;
end;

procedure TCustomBaseList.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; pX, pY: Integer);
var
  i: integer;
  itr: TItemInteraction;
begin

  itr := ItemInteractionAtXY(pX, pY);
  for I := 0 to Items.Count - 1 do
  begin
    if (I >= FFirstVisibleIndex) and (I <= LastVisibleIdx) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).DoMouseUp(Sender, Button, Shift, pX, pY, itr, GetItemAppearance);
    end
    else
    begin
      if (Items[i] is TCustomitem) then
      begin
        if TCustomItem(Items[i]).State = isSelected then
          TCustomItem(Items[i]).State := isNormal;
      end;
    end;
  end;
end;

procedure TCustomBaseList.DoWMKeyDown(var Message: TWMKeyDown);
var
  it: TCustomItem;
  backwards: boolean;
  f: TWinControl;
  cf: TWinControl;
begin
  if Message.CharCode = VK_TAB then
  begin
    backwards := GetKeyState(VK_SHIFT) and $8000 = $8000;
    if GetCountFocusableItems > 0 then
    begin
      if (FocusedItemIndex >= 0) and (FocusedItemIndex <= Items.Count - 1) then
      begin
        if Items[FocusedItemIndex] is TCustomItem then
        begin
          it := TCustomItem(Items[FocusedItemIndex]);
          if Assigned(it) then
          begin
            if it.ProcessTab(backwards) then
            begin
              if (backwards and (it.Index = MinFocusableItemIndex))
                or (not backwards and (it.Index = MaxFocusableItemIndex)) then
              begin
                if Assigned(it.ItemOwner) then
                begin
                  if (it.ItemOwner is TWinControl) then
                  begin
                    f := GetTopParent(it.ItemOwner as TWinControl);
                    if Assigned(f) then
                    begin
                      cf := TWinControlAccess(f).FindNextControl(it.ItemOwner as TWinControl, not backwards, True, False);
                      if Assigned(cf) then
                      begin
                        if cf.Visible and cf.TabStop then
                        begin
                          cf.SetFocus;
                          FocusedItemIndex := MinFocusableItemIndex;
                        end;
                      end;
                    end;
                  end;
                end;
              end
              else
              begin
                FocusNextItem(backwards);
                if (FocusedItemIndex >= 0) and (FocusedItemIndex <= Items.Count - 1) then
                begin
                  if Items[FocusedItemIndex] is TCustomItem then
                  begin
                    if TCustomItem(Items[FocusedItemIndex]).FirstTab(backwards) then
                      FocusNextItem(backwards);
                  end;
                end;
                Changed;
              end;
            end;
          end;
        end;
      end;
    end;
  end
  else
    inherited;
end;

procedure TCustomBaseList.DrawItems(g: TGPGraphics; Focused: Boolean);
var
  i: integer;
  itappgeneral: TItemAppearance;
  itappcopy: TItemAppearance;
  it: TCustomItem;
begin
  itappgeneral := GetItemAppearance;
  itappgeneral.Focus := Focused;
  itappgeneral.FocusedItem := FocusedItemIndex;
  itappcopy := CreateAppearance;

  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if (Items[i] is TCustomItem) then
      begin
        if TCustomItem(Items[i]).Visible then
        begin
          it := TCustomItem(Items[i]);
          CopyAppearance(itappgeneral, itappcopy);
          if Assigned(OnItemAppearance) then
            OnItemAppearance(Self, it, itappcopy);
          it.Draw(g, itappcopy);
        end;
      end;
    end;
  end;

  DestroyAppearance(itappcopy);
end;

procedure TCustomBaseList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

function TCustomBaseList.LastVisibleIdx: integer;
begin
  Result := -1;
  if Items.Count > 0 then
    Result := FLastVisibleIndex;
end;

function TCustomBaseList.GetLastVisibleIndex: integer;
var
  I: Integer;
  it: TCustomItem;
begin
  Result := -1;
  for I := Items.Count - 1 downto 0 do
  begin
    if Items[i] is TCustomItem then
    begin
      it := Items[i] as TCustomItem;
      if ItemInRectangle(it, Bounds(0, 0, Width, Height)) then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.ColumnCount: integer;
var
  I: Integer;
  col: Integer;
begin
  col := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      if TCustomItem(Items[i]).ColumnIndex > col then
        col := TCustomItem(Items[i]).ColumnIndex;
    end;
  end;

  Result := col + 1;
end;

function TCustomBaseList.Compare(Item1, Item2: Pointer): integer;
begin
  if Assigned(OnItemCompare) then
    OnItemCompare(Self, TCustomItem(Item1), TCustomItem(Item2), Result)
  else
    Result := AnsiCompareStr(TCustomItem(item1).CustomClassName, TCustomItem(item2).CustomClassName);
end;

function TCustomBaseList.GetCountFocusableItems: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[i] is TCustomItem then
      if IsItemFocusable(TCustomItem(items[i])) then
        Inc(result);
end;

function TCustomBaseList.GetCountSelectableItems: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
    if Items[i] is TCustomItem then
      if IsItemSelectable(TCustomItem(items[i])) then
        Inc(result);
end;

function TCustomBaseList.GetItemAppearance: TItemAppearance;
begin
  Result.Normal := Appearance.Normal;
  Result.Down := Appearance.Down;
  Result.Disabled := Appearance.Disabled;
  Result.Hover := Appearance.Hovered;
  Result.Selected := Appearance.Selected;
  Result.ButtonNormal := Appearance.ButtonNormal;
  Result.ButtonDown := Appearance.ButtonDown;
  Result.ButtonDisabled := Appearance.ButtonDisabled;
  Result.ButtonHover := Appearance.ButtonHovered;
  Result.ButtonSelected := Appearance.ButtonSelected;
  Result.NormalFont := Appearance.NormalFont;
  Result.DownFont := Appearance.DownFont;
  Result.DisabledFont := Appearance.DisabledFont;
  Result.HoverFont := Appearance.HoveredFont;
  Result.SelectedFont := Appearance.SelectedFont;
  Result.PictureContainer := Appearance.PictureContainer;
  Result.ImageList := Appearance.ImageList;
  Result.IsMetroStyle := MetroStyle;
  Result.DrawHTMLCache := HTMLCache;
end;

function TCustomBaseList.GetItemClassByCustomName(
  ACustomName: String): TComponentClass;
var
  l: TSubClassList;
  i: Integer;
  b: TObject;
begin
  Result := TCustomItem;
  l := TSubclassList.Create;
  l.Execute(TCustomItem);

  for i := 0 to l.Count - 1 do
  begin
    b := l[i].NewInstance;
    if UpperCase(TCustomItem(b).CustomClassName) = UpperCase(ACustomName) then
    begin
      Result := TComponentClass(l[i]);
      FreeAndNil(b);
      break;
    end;
    b.free;
  end;
  FreeAndNil(l);
end;

function TCustomBaseList.GetItemsByTag(ATag: Integer): TCustomItems;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      if TCustomItem(Items[I]).Tag = ATag then
      begin
        SetLength(result, Length(Result) + 1);
        result[Length(Result) - 1] := TCustomItem(Items[i]);
      end;
    end;
  end;
end;

function TCustomBaseList.MaxColumnItem: TCustomItem;
var
  I: Integer;
  it: TCustomItem;
  maxrow, maxcol: integer;
begin
  maxrow := 0;
  maxcol := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      it := TCustomItem(Items[I]);
      if it.Visible then
      begin
        if it.ColumnIndex >= maxcol then
        begin
          maxcol := it.ColumnIndex;
          maxrow := it.RowIndex;
        end;
      end;
    end;
  end;
  Result := ItemAtRowCol(maxrow, maxcol);
end;

function TCustomBaseList.MaxFocusableItemIndex: integer;
var
  I: Integer;
begin
  Result := Items.Count - 1;
  for I := Items.Count - 1 downto 0 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemFocusable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).Index;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxRowItem: TCustomItem;
var
  I: Integer;
  it: TCustomItem;
  maxrow, maxcol: integer;
begin
  maxrow := 0;
  maxcol := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      it := TCustomItem(Items[I]);
      if it.Visible then
      begin
        if it.RowIndex >= maxrow then
        begin
          maxrow := it.RowIndex;
          maxcol := it.ColumnIndex;
        end;
      end;
    end;
  end;
  Result := ItemAtRowCol(maxrow, maxcol);
end;

function TCustomBaseList.GetOwnerComponent: TComponent;
begin
  Result := FOwnerComponent;
end;

function TCustomBaseList.GetTopParent(AControl: TWinControl): TWinControl;
begin
  if (AControl.Parent is TCustomForm) or (AControl.Parent is TCustomFrame) then
    Result := AControl.Parent
  else
    Result := GetTopParent(AControl.Parent);
end;

function TCustomBaseList.RowCount: integer;
var
  I: Integer;
  row: Integer;
begin
  row := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      if TCustomItem(Items[i]).RowIndex > row then
        row := TCustomItem(Items[i]).RowIndex;
    end;
  end;

  Result := row + 1;
end;

function TCustomBaseList.FirstVisibleIdx: integer;
begin
  Result := 0;
  if Items.Count > 0 then
    Result := FFirstVisibleIndex;
end;

procedure TCustomBaseList.FocusNextItem(backwards: Boolean);
begin
  if GetCountFocusableItems > 0 then
  begin         
    if backwards then
    begin
      Dec(FFocusedItemIndex);
      if FFocusedItemIndex < 0 then
        FFocusedItemIndex := Items.Count - 1;
    end
    else
    begin
      Inc(FFocusedItemIndex);
      if FFocusedItemIndex > Items.Count - 1 then
        FFocusedItemIndex := 0;        
    end;

    while (FFocusedItemIndex = -1) or not IsItemFocusable(TCustomItem(Items[FFocusedItemIndex])) do
    begin
      if backwards then
      begin
        Dec(FFocusedItemIndex);
        if FFocusedItemIndex < 0 then
          FFocusedItemIndex := Items.Count - 1;
      end
      else
      begin
        Inc(FFocusedItemIndex);
        if FFocusedItemIndex > Items.Count - 1 then
          FFocusedItemIndex := 0;
      end;
    end;        
  end;
end;

function TCustomBaseList.GetFirstVisibleIndex: integer;
var
  I: Integer;
  it: TCustomItem;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      it := Items[i] as TCustomItem;
      if ItemInRectangle(it, Bounds(0, 0, Width, Height)) then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

procedure TCustomBaseList.HideItemsByLevel(StartIndex: integer; ALevel: integer;
  ALevelStatus: TItemLevelStatus);
var
  I: Integer;
begin
  BeginUpdate;
  for I := StartIndex to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      if (ALevelStatus = hsWithinMainLevel) and (TCustomItem(Items[i]).Level = ALevel - 1) then
        Break
      else if TCustomItem(Items[i]).Level = ALevel then
        TCustomItem(Items[i]).Visible := false;
    end;
  end;
  EndUpdate;
end;

procedure TCustomBaseList.HideShortCutHints;
var
  i: integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
      TCustomItem(Items[i]).HideShortCutHint;
  end;
end;

function TCustomBaseList.InsertItem(Index: integer;
  it: TCustomItem): TCustomItem;
begin
  BeginUpdate;
  AssignEvents(it);
  Items.Insert(Index, it);
  Result := it;
  EndUpdate;
end;

function TCustomBaseList.IsItemFocusable(it: TCustomItem): Boolean;
begin
  result := it.TabStop;
  result := result and it.Visible and it.Enabled and not it.ReadOnly and it.IsFocusable;
end;

function TCustomBaseList.IsItemSelectable(it: TCustomItem): Boolean;
begin      
  result := it.Selectable;
  result := result and it.Visible and it.Enabled and not it.ReadOnly;
end;

function TCustomBaseList.ItemInteractionAtXY(pX, pY: integer): TItemInteraction;
var
  I: Integer;
begin
  Result.InteractionItem := nil;
  Result.InteractionType := itNone;
  for I := LastVisibleIdx downto FirstVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
      begin
        if TCustomItem(Items[i]).Visible and TCustomItem(Items[i]).Enabled and not TCustomItem(Items[i]).ReadOnly then
        begin
          Result.InteractionItem := TCustomItem(Items[i]);
          Result.InteractionType := ItemInteraction(pX, pY, TCustomItem(Items[i]));
          if Result.InteractionType <> itNone then
          begin
            Break;
          end
          else
          begin
            Result.InteractionItem := nil;
            Result.InteractionType := itNone;
          end;
        end;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxBoxRowItem: TCustomItem;
var
  I: Integer;
  it: TCustomItem;
  maxy: integer;
begin
  maxy := 0;
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      it := Items[i] as TCustomItem;
      if it.Visible then
      begin
        if it.Y > maxy then
        begin
          maxy := it.Y;
          result := it;
        end;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxBoxColumnItem: TCustomItem;
var
  I: Integer;
  it: TCustomItem;
  maxx: integer;
begin
  maxx := 0;
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      it := Items[i] as TCustomItem;
      if it.Visible then
      begin
        if it.X > maxx then
        begin
          maxx := it.X;
          result := it;
        end;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxColIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if (Items[i] as TCustomItem).Visible then
      begin
        if ((Items[i] as TCustomItem).ColumnIndex > Result) then
        begin
          Result := (Items[i] as TCustomItem).ColumnIndex;
        end;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxRowIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if (Items[i] as TCustomItem).Visible then
      begin
        if ((Items[i] as TCustomItem).RowIndex > Result) then
        begin
          Result := (Items[i] as TCustomItem).RowIndex;
        end;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxSelectableColIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Items.Count - 1 downto 0 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemSelectable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).ColumnIndex;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxSelectableRowIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Items.Count - 1 downto 0 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemSelectable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).RowIndex;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MaxSelectableItemIndex: integer;
var
  I: Integer;
begin
  Result := Items.Count - 1;
  for I := Items.Count - 1 downto 0 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemSelectable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).Index;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MinFocusableItemIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemFocusable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).Index;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MinSelectableColIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemSelectable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).ColumnIndex;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MinSelectableItemIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemSelectable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).Index;
        break;
      end;
    end;
  end;
end;

function TCustomBaseList.MinSelectableRowIndex: integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if IsItemSelectable((items[i] as TCustomItem)) then
      begin
        Result := (Items[i] as TCustomItem).RowIndex;
        break;
      end;
    end;
  end;
end;

procedure TCustomBaseList.ProcessKeyBox(Key: Word; var ItemIndex: integer);
var
  maxindex, minindex: integer;
begin
  maxindex := MaxSelectableItemIndex;
  minindex := MinSelectableItemIndex;
  case Key of
    VK_HOME:
    begin
      ItemIndex := minindex;
    end;
    VK_END:
    begin
      ItemIndex := maxindex;
    end;
    VK_NEXT:
    begin
      //
    end;
    VK_PRIOR:
    begin
      //
    end;
    VK_LEFT, VK_UP:
    begin
      if ItemIndex < minindex then
        ItemIndex := maxindex
      else
        ItemIndex := ItemIndex - 1;
    end;
    VK_RIGHT, VK_DOWN:
    begin
      if ItemIndex > maxindex then
        ItemIndex := minindex
      else
        ItemIndex := ItemIndex + 1;
    end;
  end;
end;

procedure TCustomBaseList.ProcessKeyGrid(Key: Word; var SelRow,
  SelCol: integer);
var
  maxcol, maxrow, maxselcol, maxselrow,
  minselcol, minselrow: integer;
begin
  maxselcol := MaxSelectableColIndex;
  maxselrow := MaxSelectableRowIndex;
  minselrow := MinSelectableRowIndex;
  minselcol := MinSelectableColIndex;
  maxrow := MaxRowIndex;
  maxcol := MaxColIndex;
  case Key of
    VK_HOME:
    begin
      selrow := minselrow;
      selcol := minselcol;
    end;
    VK_END:
    begin
      SelRow := maxselrow;
      SelCol := maxselcol;
    end;
    VK_NEXT:
    begin
      //
    end;
    VK_PRIOR:
    begin
      //
    end;
    VK_LEFT:
    begin
      SelCol := SelCol - 1;
      if SelCol < 0 then
      begin
        SelRow := SelRow - 1;
        if SelRow < 0 then
          SelRow := maxrow;

      SelCol := maxcol;
      end;
    end;
    VK_RIGHT:
    begin
      SelCol := SelCol + 1;
      if SelCol > maxcol then
      begin
        SelRow := SelRow + 1;
        if SelRow > maxrow then
          SelRow := 0;

        SelCol := 0;
      end;
    end;
    VK_UP:
    begin
      SelRow := SelRow - 1;
      if SelRow < 0 then
      begin
        SelCol := SelCol - 1;
        if SelCol < 0 then
          SelCol := maxcol;

        SelRow := maxrow;
      end;
    end;
    VK_DOWN:
    begin
      SelRow := SelRow + 1;
      if SelRow > maxrow then
      begin
        SelCol := SelCol + 1;
        if SelCol > maxcol then
          SelCol := 0;

        SelRow := 0;
      end;
    end;
  end;
end;

function TCustomBaseList.ItemAtRowCol(Row, Col: integer): TCustomItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if (Items[i] as TCustomItem).Visible then
      begin
        if ((Items[i] as TCustomItem).ColumnIndex = Col) and ((Items[i] as TCustomItem).RowIndex = Row) then
        begin
          Result := Items[i] as TCustomItem;
          break;
        end;
      end;
    end;
  end;
end;

function TCustomBaseList.ItemAtXY(pX, pY: integer): TCustomItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Items.Count - 1 do
  begin
    if TCustomItem(Items[i]).IsItemAtXY(pX, pY) and TCustomItem(Items[i]).Visible then
    begin
      Result := TCustomItem(Items[i]);
      Break;
    end;
  end;
end;

procedure TCustomBaseList.DoCMDialogChar(var Message: TCMDialogChar);
var
  i: integer;
begin
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).DoCMDialogChar(Message);
    end;
  end;
end;

procedure TCustomBaseList.DoCMHintShow(var Message: TMessage);
type
  PHintInfo = ^THintInfo;
var
  hi: PHintInfo;
  I: Integer;
  itr: TItemInteraction;
begin
  hi := PHintInfo(Message.LParam);
  itr := ItemInteractionAtXY(hi^.cursorPos.x,hi^.cursorpos.y);
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).DoCMHintShow(Message, itr);
    end;
  end;
  inherited;
end;

procedure TCustomBaseList.DoCMMouseLeave(var Message: TMessage);
var
  i: integer;
begin
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).DoCMMouseLeave(Message);
    end;
  end;
end;

procedure TCustomBaseList.DoDblClick(Sender: TObject; Pos: TPoint);
var
  i: integer;
  itr: TItemInteraction;
begin
  itr := ItemInteractionAtXY(Pos.X, Pos.Y);
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).DoDblClick(Sender, itr, GetItemAppearance);
    end;
  end;
end;

procedure TCustomBaseList.DoEnter;
var
  it: Integer;
  item: TCustomItem;
begin
  if GetCountFocusableItems > 0 then
  begin
    it := FocusedItemIndex;
    if (it >= 0) and (it <= items.Count - 1) then
    begin
      if Items[it] is TCustomItem then
      begin
        if not TCustomItem(Items[it]).TabStop or not IsItemFocusable(TCustomItem(Items[it])) then
        begin
          FocusNextItem(false);
          if (FocusedItemIndex >= 0) and (FocusedItemIndex <= Items.Count - 1) then
          begin
            if Items[FocusedItemIndex] is TCustomItem then
            begin
              item := TCustomItem(Items[FocusedItemIndex]);
              item.FirstTab(False);
            end;
          end;
        end
        else
          TCustomItem(Items[it]).FirstTab(False);
      end;
    end
    else
      FocusedItemIndex := -1;

    Changed;
  end;
end;

procedure TCustomBaseList.DoExit;
var
  i: integer;
begin
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
      TCustomItem(Items[i]).ResetTab;
  end;
  Changed;
end;

procedure TCustomBaseList.DoGlobalCheckChanged(Sender: TObject;
  Item: TCustomItem; Checked: Boolean);
var
  I: Integer;
  inf: IGDIPGlobalCheck;
begin
  if GlobalCheck then
    Exit;

  GlobalCheck := true;
  for I := 0 to Items.Count - 1 do
  begin
    if (Items[i] is TCustomItem) then
    begin
      if Items[i] <> Item then
      begin
        if (Item.GetClassType = TCustomItem(Items[i]).GetClassType) then
        begin
          if Items[i].GetInterface(IGDIPGlobalCheck, inf) then
            inf.GlobalCheck(Item);
        end;
      end;
    end;
  end;
  GlobalCheck := false;
end;

procedure TCustomBaseList.DoItemChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCustomBaseList.DoItemDeSelect(Sender: TObject; Item: TCustomItem;
  var Allow: Boolean);
begin
  if Assigned(OnItemDeSelect) then
    OnItemDeSelect(Sender, Item, Allow);
end;

procedure TCustomBaseList.DoItemDestroy(Sender: TObject; Item: TCustomItem);
begin
  if Assigned(OnItemDestroy) then
    OnItemDestroy(Sender, Item);
end;

procedure TCustomBaseList.DoItemFocus(Sender: TObject; Item: TCustomItem);
begin
  if IsItemFocusable(Item) then
  begin
    FocusedItemIndex := Item.Index;
    Item.FirstTab(False);
    Changed;
  end;
end;

procedure TCustomBaseList.DoItemIndexChange(Sender: TObject;
  Item: TCustomItem; OldIndex, NewIndex: integer);
var
  I: Integer;
begin
  if (Item.Index < 0) then
    Item.ApplyIndex(0)
  else if Item.Index > Items.Count - 1 then
    Item.ApplyIndex(Items.Count - 1);

  Items.Move(OldIndex, Item.Index);
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
      TCustomItem(Items[i]).ApplyIndex(I);
  end;
  Changed;
end;

procedure TCustomBaseList.DoItemRefresh(Sender: TObject);
begin
  RefreshObjects;
end;

procedure TCustomBaseList.DoItemSelect(Sender: TObject; Item: TCustomItem; var Allow: Boolean);
begin
  if Item.TabStop then
    FFocusedItemIndex := Item.Index;

  if Assigned(OnItemSelect) then
    OnItemSelect(Sender, Item, Allow);
end;

function TCustomBaseList.ItemInRectangle(It: TCustomItem; DisplayRect: TRect): Boolean;
var
  rOut, ir: TRect;
begin
  ir := Bounds(Round(it.x), Round(it.y), Round(it.width), Round(it.height));
  result := IntersectRect(rOut, ir, DisplayRect);
end;

procedure TCustomBaseList.RefreshObjects;
begin
  if Assigned(FOnRefresh) and (FUpdateCount = 0) then
    FOnRefresh(Self);

  if Assigned(FOnInternalRefresh) and (FUpdateCount = 0) then
    FOnInternalRefresh(Self);
end;

procedure TCustomBaseList.RemoveItem(AIndex: Integer);
begin
  Items.Remove(Items[AIndex]);
  Changed;
end;

procedure TCustomBaseList.ResetItemStatus;
var
  i: integer;
begin
  for I := 0 to Items.Count - 1 do
    if Items[i] is TCustomItem then
        TCustomItem(Items[i]).State := isNormal;
end;

function TCustomBaseList.SelectedItem: TCustomItem;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[i] is TCustomItem then
    begin
      if (TCustomItem(Items[i]).State = isSelected) then
      begin
        Result := TCustomItem(Items[i]);
        Break;
      end;
    end;
  end;
end;

procedure TCustomBaseList.SelectItem(AIndex: Integer);
var
  it: TCustomItem;
begin
  ResetItemStatus;
  if GetCountSelectableItems > 0 then
  begin
    AIndex := Min(Items.Count - 1, Max(0, AIndex));
    if Items[AIndex] is TCustomItem then
    begin
      it := TCustomItem(Items[AIndex]);
      if IsItemSelectable(it) then
      begin
        it.State := isSelected;
        if it.TabStop then
          FFocusedItemIndex := it.Index;
      end;
    end;
  end;
end;

procedure TCustomBaseList.SetAppearance(
  const Value: TCustomBaseListAppearance);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseList.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TCustomBaseList.SetItems(const Value: TCustomListObjects);
begin
  if FItems <> value then
  begin
    FItems.Assign(Value);
    Changed;
  end;
end;

procedure TCustomBaseList.SetMultiSelect(const Value: Boolean);
begin
  if FMultiSelect <> value then
  begin
    FMultiSelect := Value;
    Changed;
  end;
end;

procedure TCustomBaseList.SetOwnerComponent(AOwner: TComponent);
begin
  FOwnerComponent := AOwner;
end;

procedure TCustomBaseList.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TCustomBaseList.ShowItemsByLevel(StartIndex: integer; ALevel: integer;
  ALevelStatus: TItemLevelStatus);
var
  I: Integer;
begin
  BeginUpdate;
  for I := StartIndex to Items.Count - 1 do
  begin
    if Items[I] is TCustomItem then
    begin
      if (ALevelStatus = hsWithinMainLevel) and (TCustomItem(Items[i]).Level = ALevel - 1) then
        Break
      else if TCustomItem(Items[i]).Level = ALevel then
        TCustomItem(Items[i]).Visible := true;
    end;
  end;
  EndUpdate;
end;

procedure TCustomBaseList.ShowShortCutHints;
var
  i: integer;
begin
  for I := FirstVisibleIdx to LastVisibleIdx do
  begin
    if (I >= 0) and (I <= Items.Count - 1) then
    begin
      if Items[i] is TCustomItem then
        TCustomItem(Items[i]).ShowShortCutHint;
    end;
  end;
end;

procedure TCustomBaseList.Sort;
begin
  BeginUpdate;
  Items.SortListItems;
  Items.UpdateIndexes;
  EndUpdate;
end;

procedure TCustomBaseList.UnSelectItem(AIndex: Integer);
var
  it: TCustomItem;
begin
  if GetCountSelectableItems > 0 then
  begin
    AIndex := Min(Items.Count - 1, Max(0, AIndex));
    if Items[AIndex] is TCustomItem then
    begin
      it := TCustomItem(Items[AIndex]);
      if IsItemSelectable(it) then
      begin
        it.State := isNormal;
        if it.TabStop then
          FFocusedItemIndex := it.Index;
      end;
    end;
  end;
end;

procedure TCustomBaseList.UpdateVisibleIndexes;
begin
  FFirstVisibleIndex := GetFirstVisibleIndex;
  FLastVisibleIndex := GetLastVisibleIndex;
end;

{ TCustomListObjects }

procedure TCustomListObjects.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if FOwner.FUpdateCount = 0 then
    UpdateIndexes;
end;

procedure TCustomListObjects.QuickSort(SortList: PPointerList; L, R: Integer);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while FOwner.Compare(SortList^[I], P) < 0 do
        Inc(I);
      while FOwner.Compare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J);
    L := I;
  until I >= R;
end;

procedure TCustomListObjects.SortListItems;
begin
  if (List <> nil) and (Count > 1) then
  {$IFDEF DELPHIXE2_LVL}
    QuickSort(@List, 0, Count - 1);
  {$ENDIF}
  {$IFNDEF DELPHIXE2_LVL}
    QuickSort(List, 0, Count - 1);
  {$ENDIF}
end;

procedure TCustomListObjects.UpdateIndexes;
var
  i: integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Assigned(Items[i]) then
    begin
      if Items[i] is TCustomItem then
      begin
        TCustomItem(Items[I]).ApplyIndex(I);
        if Assigned(FOwner) then
          FOwner.AssignEvents(TCustomItem(Items[I]));
      end;
    end;
  end;
end;

{ TSubclassList }

procedure TSubClassList.CallbackClass(AClass: TPersistentClass);
begin
  if AClass.InheritsFrom(FParentClass) and (AClass <> FParentClass) then
    Self.Add(AClass);
end;

procedure TSubClassList.Execute(const aClass: TPersistentClass);
var
  f: TClassFinder;
begin
  FParentClass := aClass;

  f := TClassFinder.Create(nil);
  try
    f.GetClasses(CallbackClass);
  finally
    FreeAndNil(f);
  end;
end;

end.
