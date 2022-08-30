{************************************************************************}
{ TBUTTONLISTBOX component                                               }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ Copyright © 2001-2015                                                  }
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

unit BtnListB;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, StdCtrls, Messages, Controls, SysUtils, Graphics, ExtCtrls,
  AsgDD, ActiveX, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type

  TOleDragStartEvent = procedure (Sender:TObject; DropIndex: integer) of object;
  TOleDragStopEvent =  procedure (Sender:TObject; OLEEffect: integer) of object;
  TOleDragOverEvent = procedure (Sender:TObject; var Allow:boolean) of object;
  TOleDropEvent = procedure (Sender:TObject; DropIndex: integer) of object;

  TPopupButton = class(TCustomControl)
  private
   fCaption:string;
   fFlat:boolean;
  protected
   procedure CreateParams(var Params: TCreateParams); override;
  public
   procedure Paint; override;
  published
   property Caption:string read fCaption write fCaption;
   property Flat:boolean read fFlat write fFlat;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TButtonListbox = class(TCustomListBox)
  private
    { Private declarations }
    FItemIndex: integer;
    FMoveButton: TPopupButton;
    FClickPos: TPoint;
    FMouseDown: boolean;
    FOleDropTargetAssigned: boolean;
    FOnOleDrop: TOleDropEvent;
    FOnOleDragStart: TOleDragStartEvent;
    FOnOleDragStop: TOleDragStopEvent;
    FOnOleDragOver: TOleDragOverEvent;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure SetItemIndexEx(const Value: integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message:tMessage); override;
    procedure AddItemInt(s:string;idx:integer);
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
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex: integer read fItemIndex write SetItemIndexEx;
    property Items;
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

  TListDropTarget = class(TASGDropTarget)
  private
    FList:TButtonListBox;
  public
    constructor Create(AList: TButtonListBox);
    procedure DropText(pt:TPoint; s:string); override;
    procedure DropCol(pt:TPoint; col:integer); override;
    procedure DragMouseMove(pt:TPoint; var Allow:boolean; DropFormats:TDropFormats); override;
    procedure DragMouseLeave; override;
  end;

  TListDropSource = class(TASGDropSource)
  private
    FList: TButtonListBox;
    FLastEffect: integer;
  public
    constructor Create(AList: TButtonListBox);
    procedure CurrentEffect(dwEffect: Longint); override;
    procedure QueryDrag; override;
    property LastEffect: integer read FLastEffect;
  end;

implementation


const
  Effect3DSize = 3;

procedure TButtonListbox.CNDrawItem(var Message: TWMDrawItem);
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


procedure TButtonListbox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
begin
  if Index = fItemIndex then
    Canvas.Brush.Color:=clGray
  else
    Canvas.Brush.Color:=clBtnFace;

  Canvas.FillRect(Rect);
  Frame3D(canvas,rect,clWhite,clGray,1);

  rect.left := rect.left + 2;
  rect.top := rect.top + 2;
  if (Index>=0) then
    DrawText(Canvas.Handle,pchar(Items[Index]),length(Items[Index]),rect,DT_VCENTER or DT_SINGLELINE);
end;

procedure TButtonListbox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := ItemHeight;
end;

constructor TButtonListBox.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  Style := lbOwnerDrawVariable;
  FItemIndex := -1;
  FMouseDown := false;

  if not (csDesigning in ComponentState) then
  begin
    FMoveButton := TPopupButton.Create(Self);
    FMoveButton.Parent := Self;
    FMoveButton.Enabled := false;
    FMoveButton.Visible := false;
  end;
end;

procedure TButtonListbox.CreateWnd;
begin
  inherited;

  if not (csDesigning in ComponentState) then
   begin
     FOleDropTargetAssigned := RegisterDragDrop(Handle, TListDropTarget.Create(Self) ) = s_OK;
   end;
end;

procedure TButtonListbox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  ItemIndex := loword(Sendmessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));
  FClickPos := Point(X,Y);
  FMouseDown := true;
end;

procedure TButtonListbox.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseDown := false;
  FMoveButton.Visible := false;
end;


procedure TButtonListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Idx, Col: Integer;
  DropSource: TListDropSource;
  dwEffects: Integer;
  pt : TPoint;

begin
  inherited;

  Idx := loword(SendMessage(Handle,LB_ITEMFROMPOINT,0,makelparam(X,Y)));

  if ((abs(FClickPos.X-X)>3) or (abs(FClickPos.Y-Y)>3))
      and FMouseDown and (Idx>=0) and (Items.Count>0) then
  begin
    FMoveButton.Caption := Items[Idx];

    Col := integer(Items.Objects[idx]);

    pt := ClientToScreen(point(x,y));

    FMoveButton.Top := pt.y;
    FMoveButton.Left := pt.x;

    FMoveButton.Width := Width;
    FMoveButton.Height := ItemHeight;
    FMoveButton.Visible := true;

    if Assigned(FOnOleDragStart) then
      FOnOleDragStart( Self, Col);

    DropSource := TListDropSource.Create(Self);

    StartColDoDragDrop(DropSource, Col, DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffects);

    if Assigned(FOnOleDragStop) then
      FOnOleDragStop( Self, dwEffects);

    FMoveButton.Visible := false;
    FMouseDown := false;
  end;
end;


procedure TButtonListbox.SetItemIndexEx(const Value: integer);
var
  R:TRect;
begin
  if fItemIndex<>Value then
  begin
    if fItemIndex>=0 then
    begin
      SendMessage(Handle,lb_getitemrect,fItemIndex,LParam(@r));
      InvalidateRect(Handle,@r,TRUE);
    end;

    fItemIndex := Value;

    if fItemIndex>=0 then
    begin
      SendMessage(Handle,lb_getitemrect,fItemIndex,LParam(@r));
      InvalidateRect(Handle,@r,TRUE);
    end;
  end;
end;

destructor TButtonListbox.Destroy;
begin
  if not (csDesigning in ComponentState) then
    FMoveButton.Free;

  inherited;
end;

function TButtonListbox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TButtonListbox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TButtonListbox.SetVersion(const Value: string);
begin

end;

{ TPopupButton }

procedure TPopupButton.CreateParams(var Params: TCreateParams);
begin
  inherited;

  with Params do
  begin
    Style := WS_POPUP or WS_BORDER;
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
  end;

  Color := clBtnFace;
end;


procedure TPopupButton.Paint;
var
  r: TRect;
begin
  r := GetClientRect;

  if not fFlat then
    Frame3D(canvas,r,clWhite,clGray,1);

  SetBkMode(Canvas.handle,TRANSPARENT);
  DrawTextEx(Canvas.handle,pchar(fCaption),length(fCaption),r,DT_CENTER or DT_END_ELLIPSIS,nil);
end;


{ TListDropTarget }

constructor TListDropTarget.Create(aList: TButtonListBox);
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

  Allow := dfCol in DropFormats;

  if Assigned(FList.FOnOleDragOver) then
    FList.FOnOleDragOver(FList, Allow);
end;

procedure TListDropTarget.DropCol(pt: TPoint; col: integer);
begin
  inherited;
  if Assigned(FList.OnOleDrop) then
    FList.OnOleDrop(FList,col);
end;

procedure TListDropTarget.DropText(pt: TPoint; s: string);
begin
  inherited;
end;

{ TListDropSource }

constructor TListDropSource.Create(AList: TButtonListBox);
begin
  inherited Create;
  FList := AList;
end;

procedure TListDropSource.CurrentEffect(dwEffect: Integer);
begin
  inherited;
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

procedure TButtonListbox.Loaded;
begin
  inherited;
end;

procedure TButtonListbox.WndProc(var Message: tMessage);
begin
  inherited;
  if (message.msg = WM_DESTROY) then
  begin
    if FOleDropTargetAssigned then
      RevokeDragDrop(Handle);
  end;
end;

procedure TButtonListbox.AddItemInt(s: string; idx: integer);
begin
  Items.AddObject(s,TObject(idx));
end;


end.
