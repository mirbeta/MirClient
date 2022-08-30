{$I TMSDEFS.INC}
{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 2003 - 2012                                    }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit PlannerWaitList;

interface

{$I TMSDEFS.INC}

uses
  Planner, Classes, Windows, StdCtrls, Dialogs, Controls, Messages,
  Graphics, Forms, PictureContainer, AdvStyleIF;

const
  MAJ_VER = 3; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 2.0.0.1 : improved OnItemSelect event handling
  // 2.5.0.0 : compatible with TPlanner v2.5
  //         : Added method CopyToPlanner
  // 2.5.0.3 : Change for showing richedit text
  // 2.5.0.4 : Fixed painting issue in Delphi 2007
  // 3.0.0.0 : New : Support for drag image
  // 3.0.0.1 : Fixed : Issue with turning off drag image
  // 3.0.0.2 : Fixed : Issue with handling item in MoveFromPlanner method
  // 3.1.0.0 : New : SaveToFile/LoadFromFile methods
  //         : New : SaveToStream/LoadFromStream methods
  // 3.2.0.0 : New : ITMSStyle interface support added to TPlannerWaitList

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TItemHintEvent = procedure(Sender: TObject; Item: TPlannerItem; var AHint: string) of object;

  TItemEvent = procedure(Sender: TObject; Item: TPlannerItem) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerWaitList = class(TCustomListbox,ITMSStyle)
  private
    FPlanner: TPlanner;
    FOnItemHint: TItemHintEvent;
    FOnItemSelect: TItemEvent;
    FOldIdx: Integer;
    FOldHintIdx: Integer;
    FShowSelection: Boolean;
    FOnItemRightClick: TItemEvent;
    FOnItemClick: TItemEvent;
    FOnItemDblClick: TItemEvent;
    FSkin: TPlannerSkin;
    FDragImage: TDragImageList;
    FDragItemImage: boolean;
    function GetItems: TPlannerItems;
    procedure SetItems(const Value: TPlannerItems);
    procedure SetDefaultItem(const Value: TPlannerItem);
    function GetDefaultItem: TPlannerItem;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    function GetImages: TImageList;
    procedure SetImages(const Value: TImageList);
    function GetPictureContainer: TPictureContainer;
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetPlannerSkin(const Value: TPlannerSkin);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    function GetSelectedItem: TPlannerItem;
    procedure SetDragItemImage(const Value: boolean);
  protected
    procedure ItemsChanged(Sender: TObject);
    procedure ItemsUpdated(Sender: TObject);
    procedure DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    function CreatePlanner: TPlanner; virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure DeleteSelected; override;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure InsertFromStream(Stream: TStream);

    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    function MoveToPlanner(APlanner: TCustomPlanner; ItemIndex: Integer; X,Y: Integer): TPlannerItem;
    function CopyToPlanner(APlanner: TCustomPlanner; ItemIndex: Integer; X,Y: Integer): TPlannerItem;
    procedure MoveFromPlanner(APlanner: TCustomPlanner; AItem: TPlannerItem);
    property SelectedItem: TPlannerItem read GetSelectedItem;
    procedure SetComponentStyle(AStyle: TTMSStyle);
  published
    property DefaultItem: TPlannerItem read GetDefaultItem write SetDefaultItem;
    property DragItemImage: boolean read FDragItemImage write SetDragItemImage default false;
    property Images: TImageList read GetImages write SetImages;
    property Items: TPlannerItems read GetItems write SetItems;
    property ItemHeight;
    property PictureContainer: TPictureContainer read GetPictureContainer write SetPictureContainer;
    property ShowSelection: Boolean read FShowSelection write FShowSelection;
    property Skin: TPlannerSkin read FSkin write SetPlannerSkin;
    property OnItemHint: TItemHintEvent read FOnItemHint write FOnItemHint;
    property OnItemSelect: TItemEvent read FOnItemSelect write FOnItemSelect;
    property OnItemClick: TItemEvent read FOnItemClick write FOnItemClick;
    property OnItemRightClick: TItemEvent read FOnItemRightClick write FOnItemRightClick;
    property OnItemDblClick: TItemEvent read FOnItemDblClick write FOnItemDblClick;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;

    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;

    property Font;
    property ImeMode;
    property ImeName;
    property MultiSelect;

    property ShowHint;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    {$IFDEF DELPHI6_LVL}
    property OnData;
    property OnDataFind;
    property OnDataObject;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Version: string read GetVersion write SetVersion;
  end;

  TPlannerItemCrack = class(TPlannerItem);


implementation

uses
  SysUtils, PlanUtil;

{ TPlannerWaitList }

procedure TPlannerWaitList.CMHintShow(var Message: TMessage);
var
  Idx: Integer;
  hi: PHintInfo;
  AHint: string;

begin
  hi := PHintInfo(Message.LParam);
  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(Hi^.CursorPos.X,Hi^.CursorPos.Y));
  if (Idx >= 0) and (Idx < Items.Count) then
  begin
    AHint := Items[Idx].Hint;
    if Assigned(FOnItemHint) then
      FOnItemHint(Self, Items[Idx],AHint);
    Hi^.HintStr := AHint;
  end;
end;

constructor TPlannerWaitList.Create(AOwner: TComponent);
begin
  inherited;

  FPlanner := CreatePlanner;
  FPlanner.OnConflictUpdate := ItemsChanged;
  FPlanner.OnItemUpdate := ItemsUpdated;

  Style := lbOwnerDrawFixed;
  ItemHeight := 32;
  FOldIdx := -1;
  DoubleBuffered := True;
  FDragImage := TDragImageList.Create(Self);

//  ControlStyle := ControlStyle + [csDisplayDragImage];
  FSkin := TPlannerSkin.Create(FPlanner);
end;

function TPlannerWaitList.CreatePlanner: TPlanner;
begin
  Result := TPlanner.Create(Self);
end;

procedure TPlannerWaitList.DeleteSelected;
var
  i: integer;
begin
  if MultiSelect then
  begin
    for i := Items.Count - 1 downto 0 do
      begin
        if Selected[i] then
          Items.Delete(i);
      end;
  end
  else
    if ItemIndex >= 0 then
      Items.Delete(ItemIndex);
end;

destructor TPlannerWaitList.Destroy;
begin
  FPlanner.OnConflictUpdate := nil;
  FPlanner.OnItemUpdate := nil;
  FPlanner.Free;
  FDragImage.Free;
  FSkin.Free; 
  inherited;
end;

procedure TPlannerWaitList.CreateWnd;
begin
  inherited;
  // required change for richedit support
  FPlanner.Parent := self;
  FPLanner.Width :=0;
  FPlanner.Height := 0;
end;

procedure TPlannerWaitList.DragDrop(Source: TObject; X, Y: Integer);
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Self,Source, X, Y)
  else
    if Source is TCustomPlanner then
    begin
      MoveFromPlanner((Source as TCustomPlanner),(Source as TCustomPlanner).Items.Selected);
    end;
end;

procedure TPlannerWaitList.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TPlanner);
  if Assigned(OnDragOver) then
    OnDragOver(Self,Source, X,Y, State, Accept);
end;

procedure TPlannerWaitList.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if (Index >= 0) and (Index < FPLanner.Items.Count) then
  begin
    FPlanner.ItemGap := 0;
    FPlanner.Items[Index].Selected := ShowSelection and (odSelected in State);
    FPlanner.Items[Index].Repainted := False;
    TPlannerItemCrack(FPlanner.Items[Index]).Preview := true;
    FPlanner.PreviewPaint(FPlanner.Items[Index], Canvas, Rect, False, False);
  end;
end;

function TPlannerWaitList.GetDefaultItem: TPlannerItem;
begin
  Result := FPlanner.DefaultItem;
end;

function TPlannerWaitList.GetDragImages: TDragImageList;
begin
  Result := FDragImage;
end;

function TPlannerWaitList.GetImages: TImageList;
begin
  Result := FPlanner.PlannerImages;
end;

function TPlannerWaitList.GetItems: TPlannerItems;
begin
  Result := FPlanner.Items;
end;

function TPlannerWaitList.GetPictureContainer: TPictureContainer;
begin
  Result := FPlanner.PictureContainer;
end;

function TPlannerWaitList.GetSelectedItem: TPlannerItem;
begin
  Result := nil;
  if ItemIndex >= 0 then
    Result := Items[ItemIndex];
end;

procedure TPlannerWaitList.InsertFromStream(Stream: TStream);
begin
  FPlanner.InsertFromStream(Stream);
end;

procedure TPlannerWaitList.ItemsChanged(Sender: TObject);
begin
  if FPlanner.Items.Count = 0 then
  begin
    inherited Items.Clear;
    Exit;
  end;

  while FPlanner.Items.Count > inherited Items.Count do
  begin
    inherited Items.Add(inttostr(inherited items.Count));
  end;

  while FPlanner.Items.Count < inherited Items.Count do
  begin
    inherited Items.Delete(inherited Items.Count - 1);
  end;
end;

procedure TPlannerWaitList.ItemsUpdated(Sender: TObject);
begin
  Invalidate;
end;

procedure TPlannerWaitList.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (FOldIdx <> ItemIndex) and (ItemIndex >= 0) and (ItemIndex < Items.Count) then
  begin
    if Assigned(FOnItemSelect) then
      FOnItemSelect(Self, Items[ItemIndex]);
  end;
  
  FOldIdx := ItemIndex;
end;

procedure TPlannerWaitList.Loaded;
begin
  inherited;
  FPlanner.Skin.Assign(Skin);
end;

procedure TPlannerWaitList.LoadFromFile(FileName: string);
begin
  FPlanner.LoadFromFile(FileName);
end;

procedure TPlannerWaitList.LoadFromStream(Stream: TStream);
begin
  FPlanner.LoadFromStream(Stream);
end;

procedure TPlannerWaitList.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited;
end;

procedure TPlannerWaitList.DoExit;
begin
  inherited;
  FOldIdx := -1;
end;

procedure TPlannerWaitList.DoStartDrag(var DragObject: TDragObject);
var
  b: TBitmap;
begin
  inherited;

  if (Items.Count > 0) and (ItemIndex >= 0) and DragItemImage then
  begin
    FDragImage.Width := Self.Width;
    FDragImage.Height := ItemHeight;

    b := TBitmap.Create;
    b.Width := FDragImage.Width ;
    b.Height := FDragImage.Height;

    FPlanner.PreviewPaint(Items[ItemIndex],b.Canvas,Rect(0,0,FDragImage.Width, FDragImage.Height), true,false);

    FDragImage.Clear;
    FDragImage.Add(b,nil);
    b.Free;
  end;
end;

procedure TPlannerWaitList.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;

  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(X,Y));

  if (FOldIdx <> Idx) and (Idx >= 0) and (Idx < Items.Count) then
  begin
    if Assigned(FOnItemSelect) then
      FOnItemSelect(Self, Items[Idx]);
  end;

  if (Idx >= 0) and (Idx < Items.Count) then
  begin
    if (Button = mbRight) and Assigned(FOnItemRightClick) then
      FOnItemRightClick(Self, Items[Idx]);

    if (Button = mbLeft) and Assigned(FOnItemClick) then
      FOnItemClick(Self, Items[Idx]);
  end;

  FOldIdx := Idx;
end;

procedure TPlannerWaitList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
begin
  inherited;

  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(X,Y));

  if (Idx <> FOldHintIdx) and ShowHint then
  begin
    Application.CancelHint;
  end;
  FOldHintIdx := Idx;
end;

procedure TPlannerWaitList.MoveFromPlanner(APlanner: TCustomPlanner;
  AItem: TPlannerItem);
var
  bi,bp: integer;
begin
  with Items.Add do
  begin
    bi := AItem.ItemBegin;
    bp := AItem.ItemPos;

    Assign(AItem);
    APlanner.FreeItem(AItem);
    APlanner.Items.Selected := nil;
    if not APlanner.SelectionAlways then
    begin
      APlanner.SelectCells(bi,bi,bp);
    end;
  end;
end;

function TPlannerWaitList.MoveToPlanner(APlanner: TCustomPlanner;
  ItemIndex: Integer; X, Y: Integer): TPlannerItem;
var
  pt: TPoint;
  plIt: TPlannerItem;
  delta: Integer;
begin
  pt := APlanner.XYToCell(X,Y);

  Result := APlanner.CreateItem;

  plIt := Items[ItemIndex];
  delta := plIt.ItemEnd - plIt.ItemBegin;
  Result.ItemObject:= Items[ItemIndex].ItemObject;

  Result.Assign(plIt);

  if APlanner.SideBar.Position = spTop then
  begin
    Result.ItemBegin := pt.X;
    Result.ItemEnd := pt.X + delta;
    Result.ItemPos := pt.Y;
  end
  else
  begin
    Result.ItemBegin := pt.Y;
    Result.ItemEnd := pt.Y + delta;
    Result.ItemPos := pt.X;
  end;

  Result.Update;

  Items[ItemIndex].Free;
end;

function TPlannerWaitList.CopyToPlanner(APlanner: TCustomPlanner;
  ItemIndex: Integer; X, Y: Integer): TPlannerItem;
var
  pt: TPoint;
  plIt: TPlannerItem;
  delta: Integer;
begin
  pt := APlanner.XYToCell(X,Y);

  Result := APlanner.CreateItem;
  plIt := Items[ItemIndex];

  delta := plIt.ItemEnd - plIt.ItemBegin;
  Result.ItemObject:= Items[ItemIndex].ItemObject;
  Result.Assign(plIt);

  if APlanner.SideBar.Position = spTop then
  begin
    Result.ItemBegin := pt.X;
    Result.ItemEnd := pt.X + delta;
    Result.ItemPos := pt.Y;
  end
  else
  begin
    Result.ItemBegin := pt.Y;
    Result.ItemEnd := pt.Y + delta;
    Result.ItemPos := pt.X;
  end;

  Result.Update;
end;

procedure TPlannerWaitList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if csDestroying in ComponentState then
    Exit;

  if (AOperation = opRemove) and Assigned(DefaultItem) then
  begin
    if DefaultItem.Alarm.Handler = AComponent then
      DefaultItem.Alarm.Handler := nil;
    if DefaultItem.Editor = AComponent then
      DefaultItem.Editor := nil;
    if DefaultItem.PopupMenu = AComponent then
      DefaultItem.PopupMenu := nil;
    if DefaultItem.DrawTool = AComponent then
      DefaultItem.DrawTool := nil;
  end;
end;

procedure TPlannerWaitList.SaveToFile(FileName: string);
begin
  FPlanner.SaveToFile(FileName);
end;

procedure TPlannerWaitList.SaveToStream(Stream: TStream);
begin
  FPlanner.SaveToStream(Stream);
end;

procedure TPlannerWaitList.SetComponentStyle(AStyle: TTMSStyle);
begin
  if Assigned(FPlanner) then
    FPlanner.SetComponentStyle(AStyle);
end;

procedure TPlannerWaitList.SetDefaultItem(const Value: TPlannerItem);
begin
  FPlanner.DefaultItem.Assign(Value);
end;

procedure TPlannerWaitList.SetDragItemImage(const Value: boolean);
begin
  if (FDragItemImage <> Value) then
  begin
    FDragItemImage := Value;
    if Value then
      FixControlStyles(GetParentForm(self));
  end;
end;

procedure TPlannerWaitList.SetImages(const Value: TImageList);
begin
  FPlanner.PlannerImages := Value;
end;

procedure TPlannerWaitList.SetItems(const Value: TPlannerItems);
begin
  FPlanner.Items.Assign(Value); 
end;


procedure TPlannerWaitList.SetPictureContainer(
  const Value: TPictureContainer);
begin
  FPlanner.PictureContainer := Value;
end;

procedure TPlannerWaitList.SetPlannerSkin(const Value: TPlannerSkin);
begin
  FSkin.Assign(Value);
end;

procedure TPlannerWaitList.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  Idx: Integer;
begin
  inherited;

  Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(Message.XPos ,Message.YPos));

  if (Idx >= 0) and (Idx < Items.Count) and Assigned(FOnItemDblClick) then
  begin
    FOnItemDblClick(Self, Items[Idx]);
  end;
end;

function TPlannerWaitList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TPlannerWaitList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TPlannerWaitList.SetVersion(const Value: string);
begin

end;


end.
