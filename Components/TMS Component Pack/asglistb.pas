{************************************************************************}
{ THEADERLISTBOX component                                               }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ Copyright © 2011 - 2015                                                }
{   TMS Software                                                         }
{   Email : info@tmssoftware.com                                         }
{   Web : http://www.tmssoftware.com                                     }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit AsgListb;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, Messages, Controls, SysUtils, Graphics, ExtCtrls,
  AsgDD, ActiveX, AdvGrid, Grids, Forms, AdvObj
  {$IFDEF DELPHI7_LVL}
  , Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // v1.0.0.0 : First release
  // v1.0.1.0 : New : ItemHeight exposed in TAdvGridHeaderPopupList


type

  TOleDragStartEvent = procedure (Sender:TObject; DropIndex: integer) of object;
  TOleDragStopEvent =  procedure (Sender:TObject; OLEEffect: integer) of object;
  TOleDragOverEvent = procedure (Sender:TObject; var Allow:boolean) of object;
  TOleDropEvent = procedure (Sender:TObject; DropIndex: integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridHeaderList = class(TCustomListBox)
  private
    { Private declarations }
    FGrid: TAdvStringGrid;
    FDragging: boolean;
    FItemIndex: integer;
    FMoveButton: THeaderDragButton;
    FClickPos: TPoint;
    FMouseDown: boolean;
    FOleDropTargetAssigned: boolean;
    FOnOleDrop: TOleDropEvent;
    FOnOleDragStart: TOleDragStartEvent;
    FOnOleDragStop: TOleDragStopEvent;
    FOnOleDragOver: TOleDragOverEvent;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetGrid(const Value: TAdvStringGrid);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawItem(Index: Integer; ARect: TRect;State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure WndProc(var Message:tMessage); override;

    procedure MoveFromGridToList(ColumnIndex: integer);
    procedure MoveFromListToGrid(ItemIndex: integer);
    procedure MoveAllFromGridToList;
    procedure MoveAllFromListToGrid;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ParentCtl3D;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnOleDrop: TOleDropEvent read FOnOleDrop write FOnOleDrop;
    property OnOleDragStart: TOleDragStartEvent read FOnOleDragStart write FOnOleDragStart;
    property OnOleDragStop: TOleDragStopEvent read FOnOleDragStop write FOnOleDragStop;
    property OnOleDragOver: TOleDragOverEvent read FOnOleDragOver write FOnOleDragOver;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGridHeaderPopupList = class(TComponent)
  private
    FForm: TForm;
    FHeaderList: TAdvGridHeaderList;
    FColList: TStringList;
    FGrid: TAdvStringGrid;
    FCaption: string;
    FOnCloseQuery: TCloseQueryEvent;
    FOnClose: TCloseEvent;
    FFormTop: integer;
    FFormLeft: integer;
    FFormHeight: integer;
    FFormWidth: integer;
    FHint: string;
    FShowHint: boolean;
    FItemHeight: integer;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetGrid(const Value: TAdvStringGrid);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure DoFormClose(Sender: TObject; var Action: TCloseAction);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Show(AOwner: TComponent);
    procedure ShowAtXY(AOwner: TComponent; X,Y: integer);
    procedure MoveFromGridToList(ColumnIndex: integer);
    procedure MoveFromListToGrid(ItemIndex: integer);
    procedure MoveAllFromGridToList;
    procedure MoveAllFromListToGrid;
  published
    property Caption: string read FCaption write FCaption;
    property FormWidth: integer read FFormWidth write FFormWidth default 200;
    property FormHeight: integer read FFormHeight write FFormHeight default 300;
    property FormTop: integer read FFormTop write FFormTop default -1;
    property FormLeft: integer read FFormLeft write FFormLeft default -1;
    property Grid: TAdvStringGrid read FGrid write SetGrid;
    property Hint: string read FHint write FHint;
    property ItemHeight: integer read FItemHeight write FItemHeight default 16;
    property ShowHint: boolean read FShowHint write FShowHint;
    property Version: string read GetVersion write SetVersion;
    property OnClose: TCloseEvent read FOnClose write FOnClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;


  TListDropTarget = class(TASGDropTarget)
  private
    FList: TAdvGridHeaderList;
  public
    constructor Create(AList: TAdvGridHeaderList);
    procedure DropText(pt:TPoint; s:string); override;
    procedure DropCol(pt:TPoint; col:integer); override;
    procedure DragMouseMove(pt:TPoint; var Allow:boolean; DropFormats: TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TListDropSource = class(TASGDropSource)
  private
    FList: TAdvGridHeaderList;
    FLastEffect: integer;
  protected
    procedure DragDropStop; override;
  public
    constructor Create(AList: TAdvGridHeaderList);
    procedure CurrentEffect(dwEffect: Longint); override;
    procedure QueryDrag; override;
    property LastEffect: integer read FLastEffect;
  end;

implementation

uses
  AsgHTMLE;

const
  Effect3DSize = 3;

procedure TAdvGridHeaderList.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  with Message.DrawItemStruct^ do
  begin
    State := TOwnerDrawState(LongRec(itemState).Lo);
    Canvas.Handle := hDC;
    Canvas.Font := Font;
    Canvas.Brush := Brush;
    if Integer(itemID) >= 0 then
      DrawItem(itemID, rcItem, State);
    Canvas.Handle := 0;
  end;
end;


procedure TAdvGridHeaderList.DrawItem(Index: Integer; ARect: TRect;
  State: TOwnerDrawState);
var
  v, a, fa, ah: string;
  xs, ys, ml, hl: Integer;
  cid, cv, ct: string;
  cr, hr: TRect;
  FCaption: string;

begin
  if Index = FItemIndex then
    FGrid.PaintCellExt(Canvas, 0, 0, ARect, [gdSelected])
  else
    FGrid.PaintCellExt(Canvas, 0, 0, ARect, []);

  Canvas.Brush.Style := bsClear;

  ARect.Left := ARect.Left + 2;
  ARect.Top := ARect.Top + 2;

  if (Index >= 0) then
  begin
    FCaption := Items[Index];

    if Pos('</',FCaption) > 0 then
    begin
      HTMLDrawEx(Canvas,FCaption, ARect, FGrid.GridImages, 2, 2, -1, -1, 2, false, false, false, false, false, false, false, false, '', 1.0, clBlue, clNone, clNone,
        clGray, v, a, fa, ah, xs, ys, hl, ml, hr, cr, cid, cv, ct, nil, nil, Handle)
    end
    else
      DrawText(Canvas.Handle,pchar(FCaption),length(FCaption),ARect,DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TAdvGridHeaderList.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHeight;
end;

constructor TAdvGridHeaderList.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);

  FDragging := false;

  Style := lbOwnerDrawVariable;
  FItemIndex := -1;
  FMouseDown := false;

  if not (csDesigning in ComponentState) then
  begin
    FMoveButton := THeaderDragButton.Create(Self);
    FMoveButton.Parent := Self;
    FMoveButton.Enabled := false;
    FMoveButton.Visible := false;
  end;
end;

procedure TAdvGridHeaderList.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
   begin
     FOleDropTargetAssigned := RegisterDragDrop(Handle, TListDropTarget.Create(Self) ) = s_OK;
   end;
end;

procedure TAdvGridHeaderList.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ItemIndex := loword(Sendmessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));
  FClickPos := Point(X,Y);
  FMouseDown := true;
end;

procedure TAdvGridHeaderList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseDown := False;
  FMoveButton.Visible := False;
end;

procedure TAdvGridHeaderList.MoveAllFromGridToList;
var
  i: integer;
begin
  if Assigned(Grid) then
  begin
    for i := Grid.ColCount - 1 downto Grid.FixedCols do
      MoveFromGridToList(i);
  end;
end;

procedure TAdvGridHeaderList.MoveAllFromListToGrid;
var
  i: integer;
begin
  if Assigned(Grid) then
  begin
    for i := Items.Count - 1 downto 0 do
      MoveFromListToGrid(i);
  end;
end;

procedure TAdvGridHeaderList.MoveFromGridToList(ColumnIndex: integer);
var
  rc: integer;
begin
  if Assigned(Grid) then
  begin
    if not Grid.IsHiddenColumn(ColumnIndex) then
    begin
      rc := Grid.ColumnAtPosition(ColumnIndex);
      Items.InsertObject(0,Grid.Cells[ColumnIndex,0], TObject(rc));
      Grid.HideColumn(ColumnIndex);
    end;
  end;
end;

procedure TAdvGridHeaderList.MoveFromListToGrid(ItemIndex: integer);
var
  rc: Integer;
begin
  if Assigned(Grid) then
  begin
    rc := Grid.ColumnPosition(integer(Items.Objects[ItemIndex]));
    Grid.UnHideColumn(rc);
    Items.Delete(ItemIndex);
  end;
end;

procedure TAdvGridHeaderList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FGrid) then
    Grid := nil;

end;

procedure TAdvGridHeaderList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx, Col: Integer;
  DropSource: TListDropSource;
  dwEffects: Integer;
  pt : TPoint;

begin
  inherited;

  Idx := loword(SendMessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));

  if ((Abs(FClickPos.X-X) > 3) or (Abs(FClickPos.Y-Y) > 3))
      and FMouseDown and (Idx >= 0) and (Items.Count > 0) then
  begin
    FMoveButton.Caption := Items[Idx];

    Col := integer(Items.Objects[idx]);

    pt := ClientToScreen(point(x,y));

    FMoveButton.Top := pt.y;
    FMoveButton.Left := pt.x;

    FMoveButton.Width := Width;
    FMoveButton.Height := ItemHeight;
    FMoveButton.Visible := true;

    Grid.OleColumnDragStart(Col,Idx, Self);

    if Assigned(FOnOleDragStart) then
      FOnOleDragStart( Self, Col);

    DropSource := TListDropSource.Create(Self);

    FDragging := True;

    StartColDoDragDrop(DropSource,Col,DROPEFFECT_COPY or DROPEFFECT_MOVE,dwEffects);

    if Assigned(FOnOleDragStop) then
      FOnOleDragStop( Self, dwEffects);

    FMoveButton.Visible := false;
    FMouseDown := false;
  end;
end;

procedure TAdvGridHeaderList.SetGrid(const Value: TAdvStringGrid);
begin
  FGrid := Value;
  if Assigned(FGrid) then
    ItemHeight := FGrid.DefaultRowHeight;

  if Assigned(FMoveButton) then
    FMoveButton.Grid := FGrid;
end;

destructor TAdvGridHeaderList.Destroy;
begin

  if not (csDesigning in ComponentState) then
  begin
    FMoveButton.Free;
    FMoveButton := nil;
  end;

  inherited;
end;

function TAdvGridHeaderList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvGridHeaderList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvGridHeaderList.SetVersion(const Value: string);
begin
end;

procedure TAdvGridHeaderList.WndProc(var Message: tMessage);
begin
  inherited;
  if (message.msg = WM_DESTROY) then
  begin
    if FOleDropTargetAssigned then
      RevokeDragDrop(self.Handle);
  end;
end;

{ TListDropSource }

constructor TListDropSource.Create(AList: TAdvGridHeaderList);
begin
  inherited Create;
  FList := AList;
end;

procedure TListDropSource.CurrentEffect(dwEffect: Integer);
begin
  inherited;
end;

procedure TListDropSource.DragDropStop;
begin
  inherited;
  FList.FDragging := false;
end;

procedure TListDropSource.QueryDrag;
var
 pt: TPoint;
begin
  inherited;
  GetCursorPos(pt);
  FList.FMoveButton.Left := pt.x;
  FList.FMoveButton.Top := pt.y - FList.FMoveButton.Height;
end;

{ TListDropTarget }

constructor TListDropTarget.Create(aList: TAdvGridHeaderList);
begin
  inherited Create;
  FList := AList;
end;

procedure TListDropTarget.DragMouseLeave;
begin
  inherited;
end;

procedure TListDropTarget.DragMouseMove(pt: TPoint; var Allow: boolean; DropFormats:TDropFormats);
begin
  inherited;

  Allow := (dfCol in DropFormats) and not FList.FDragging;

  if Assigned(FList.FOnOleDragOver) then
    FList.FOnOleDragOver(FList, Allow);
end;

procedure TListDropTarget.DropCol(pt: TPoint; Col: integer);
var
  rc: integer;
begin
  inherited;

  if Assigned(FList) and Assigned(FList.Grid) then
  begin
    rc := FList.Grid.ColumnAtPosition(col);
    if Assigned(FList.OnOleDrop) then
      FList.OnOleDrop(FList,Col);
    FList.Items.AddObject(FList.Grid.Cells[Col,0], TObject(rc));
    FList.Grid.HideColumn(Col);
  end;
end;

procedure TListDropTarget.DropText(pt: tpoint; s: string);
begin
  inherited;
end;


{ TAdvGridHeaderPopupList }

constructor TAdvGridHeaderPopupList.Create(AOwner: TComponent);
begin
  inherited;
  FFormWidth := 200;
  FFormHeight := 300;
  FFormLeft := -1;
  FFormTop := -1;
  FItemHeight := 16;
  FColList := TStringList.Create;
end;

destructor TAdvGridHeaderPopupList.Destroy;
begin
  FColList.Free;
  inherited;
end;

procedure TAdvGridHeaderPopupList.DoFormClose(Sender: TObject; var Action: TCloseAction);
begin
  FColList.Assign(FHeaderList.Items);
  Action := caFree;
  if Assigned(OnClose) then
    OnClose(Sender, Action);
end;

procedure TAdvGridHeaderPopupList.DoFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if Assigned(OnCloseQuery) then
    OnCloseQuery(Sender, CanClose);
end;

function TAdvGridHeaderPopupList.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvGridHeaderPopupList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvGridHeaderPopupList.MoveAllFromGridToList;
var
  i: integer;
begin
  if Assigned(Grid) then
  begin
    for i := Grid.ColCount - 1 downto Grid.FixedCols do
      MoveFromGridToList(i);
  end;
end;

procedure TAdvGridHeaderPopupList.MoveAllFromListToGrid;
var
  i: integer;
begin
  if Assigned(Grid) then
  begin
    for i := FColList.Count - 1 downto 0 do
      MoveFromListToGrid(i);
  end;
end;

procedure TAdvGridHeaderPopupList.MoveFromGridToList(ColumnIndex: integer);
var
  rc: integer;
begin
  if Assigned(Grid) then
  begin
    if not Grid.IsHiddenColumn(ColumnIndex) then
    begin
      rc := Grid.ColumnAtPosition(ColumnIndex);
      FColList.AddObject(Grid.Cells[ColumnIndex,0], TObject(rc));
      Grid.HideColumn(ColumnIndex);
    end;
  end;end;

procedure TAdvGridHeaderPopupList.MoveFromListToGrid(ItemIndex: integer);
var
  rc: Integer;
begin
  if Assigned(Grid) then
  begin
    rc := Grid.ColumnPosition(integer(FColList.Objects[ItemIndex]));
    Grid.UnHideColumn(rc);
    FColList.Delete(ItemIndex);
  end;
end;

procedure TAdvGridHeaderPopupList.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FGrid) then
    FGrid := nil;
end;

procedure TAdvGridHeaderPopupList.SetGrid(const Value: TAdvStringGrid);
begin
  FGrid := Value;
end;

procedure TAdvGridHeaderPopupList.SetVersion(const Value: string);
begin

end;

procedure TAdvGridHeaderPopupList.ShowAtXY(AOwner: TComponent; X,Y: integer);
begin
  FForm := TForm.CreateNew(AOwner);

  FForm.Width := FFormWidth;
  FForm.Height := FFormHeight;
  FForm.BorderStyle := bsSizeToolWin;
  FForm.Caption := FCaption;
  FForm.OnCloseQuery := DoFormCloseQuery;
  FForm.OnClose := DoFormClose;

  FHeaderList := TAdvGridHeaderList.Create(FForm);
  FHeaderList.Align := alClient;
  FHeaderList.Parent := FForm;
  FHeaderList.BorderStyle := bsNone;
  FHeaderList.Grid := FGrid;
  FHeaderList.Hint := FHint;
  FHeaderList.ShowHint := FShowHint;
  FHeaderList.ItemHeight := ItemHeight;

  FHeaderList.Items.Assign(FColList);

  FForm.Show;

  if (Y <> -1) and (X <> -1) then
  begin
    FForm.Left := X;
    FForm.Top := Y;
  end;

end;

procedure TAdvGridHeaderPopupList.Show(AOwner: TComponent);
begin
  ShowAtXY(AOwner, FormLeft, FormTop);
end;

end.
