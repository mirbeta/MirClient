{**************************************************************************}
{ TUniHTMListBox component                                                 }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2002 - 2008                                                  }
{   TMS Software                                                           }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit UHTMListbox;

{$I TMSDEFS.INC}
{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Comobj, Activex, PictureContainer;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with font colors on selection change

type
  EUniHTMListboxError = class(Exception);

  TAnchorClick = procedure(Sender:TObject;Index: Integer;Anchor: widestring) of object;

  TAnchorHintEvent = procedure(Sender:TObject; Index: Integer; var Anchor: widestring) of Object;

  TOwnerDrawState = Windows.TOwnerDrawState;
  {$NODEFINE TOwnerDrawState}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TUniHTMListbox = class(TCustomListBox)
  private
    { Private declarations }
    FBlinking: Boolean;
    FOldCursor: Integer;
    FOldAnchor: string;
    FOnAnchorClick: TAnchorClick;
    FOnAnchorEnter: TAnchorClick;
    FOnAnchorExit: TAnchorClick;
    FOnAnchorHint: TAnchorHintEvent;
    FImages: TImageList;
    FMultiLine: Boolean;
    FURLColor: TColor;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FIsMeasuring: Boolean;
    FTimerID: Integer;
    FEnableBlink: Boolean;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FAnchorHint: boolean;
    FSortedEx: boolean;
    FIncrLookup: boolean;
    FLookup: string;
    FScrollHorizontal: boolean;
    FMaxExtent: integer;
    FUpdateCount: integer;
    FImageCache:THTMLPictureCache;
    FTimerCount: Integer;
    FEllipsis: Boolean;
    FLastHintPos: Integer;
    FItemHint: Boolean;
    FHTMLHint: Boolean;
    FContainer: TPictureContainer;
    FSortWithHTML: Boolean;
    FHideSelection: Boolean;
    procedure ReMeasure;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
//    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure SetImages(value : TImageList);
    procedure SetMultiLine(value : boolean);
    procedure SetURLColor(const Value : tColor);
    procedure SetSelectionColor(const Value : tColor);
    procedure SetSelectionFontColor(const Value : tColor);
    function GetTextItem(index:integer):string;
    procedure SetEnableBlink(const Value: boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    function IsAnchor(x,y:integer;var Idx:integer):string;
    function GetSortedEx: boolean;
    procedure SetSortedEx(const Value: boolean);
    procedure SetScrollHorizontal(const Value: boolean);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetContainer(const Value: TPictureContainer);
    function GetWideItem(Index: Integer): widestring;
    procedure SetWideItem(Index: Integer; const Value: widestring);
    procedure SetHideSelection(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    { Protected declarations }
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure Loaded; override;
    procedure DoEnter; override;
    function MaxHorizontalExtent: Integer;
    procedure UpdateHScrollExtent(maxextent: Integer);
    procedure QuickSortList(List:TStringList;Left,Right: Integer);    
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    property TextItems[index:integer]:string read GetTextItem;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure HilightInList(HiText: widestring; DoCase: Boolean);
    procedure HilightInItem(Index: Integer; HiText: widestring; DoCase: Boolean);
    procedure UnHilightInList;
    procedure UnHilightInItem(Index: Integer);
    procedure MarkInList(HiText: widestring; DoCase: Boolean);
    procedure MarkInItem(Index: Integer; HiText: widestring; DoCase: Boolean);
    procedure UnMarkInList;
    procedure UnMarkInItem(Index: Integer);
    procedure AddWideItem(Value: widestring);
    procedure DeleteWideItem(Index: Integer);
    property WideItems[Index: Integer]: widestring read GetWideItem write SetWideItem;
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
    property AnchorHint: Boolean read FAnchorHint write FAnchorHint default False;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property EnableBlink: Boolean read FEnableBlink write SetEnableBlink default False;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property HideSelection: Boolean read FHideSelection write SetHideSelection;
    property HTMLHint: Boolean read FHTMLHint write FHTMLHint default False;
    property Images:TImageList read FImages write SetImages;
    property IncrLookup:boolean read FIncrLookup write FIncrLookup default False;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemHint: Boolean read FItemHint write FItemHint default False;
    property MultiSelect;
    property Multiline: Boolean read FMultiLine write SetMultiline;
    property ParentCtl3D;
    property ParentColor;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property PopupMenu;
    property ScrollHorizontal: Boolean read FScrollHorizontal write SetScrollHorizontal default False;
    property ShowHint;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor;
    property SelectionFontColor: TColor read FSelectionFontColor write SetSelectionFontColor;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset;
    property Sorted: Boolean read GetSortedEx write SetSortedEx;
    property SortWithHTML: Boolean read FSortWithHTML write FSortWithHTML;
    property TabOrder;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
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
    property OnStartDrag;
    property OnAnchorClick:TAnchorClick read FOnAnchorClick write FOnAnchorClick;
    property OnAnchorEnter:TAnchorClick read FOnAnchorEnter write FOnAnchorEnter;
    property OnAnchorExit:TAnchorClick read FOnAnchorExit write FOnAnchorExit;
    property OnAnchorHint:TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl, ShellApi, Forms ,ImgList;

{$I htmlengu.pas}

function WideSet(s: widestring): string;
var
  i,j: Integer;
  wc: widechar;
  d: string;
begin
  for i := 1 to length(s) do
  begin
    wc := s[i];
    j := ((smallint(wc) and $FF00) shr 8) + 1;
    if (j = 256) or (j = 255) then
    begin
      d := d + chr(255);
      if (j = 255) then
        d := d + chr(1);
      if (j = 256) then
        d := d + chr(2);
    end
    else
      d := d + chr(j);

    j := (smallint(wc) and $FF) + 1;
    if (j = 256) or (j = 255) then
    begin
      d := d + chr(255);
      if (j = 255) then
        d := d + chr(1);
      if (j = 256) then
        d := d + chr(2);
    end
    else
      d := d + chr(j);

  end;
  Result := d;
end;

function WideGet(s: string): widestring;
var
  ws: widestring;
  wsi: Integer;
  wc: widechar;
  r: string;
begin
  ws := '';

  r := '';
  wsi := 1;
  while (wsi <= length(s)) do
  begin
    if ord(s[wsi]) = 255 then
    begin
      inc(wsi);
      if ord(s[wsi]) = 1 then
        r := r + chr(255)
      else
        if ord(s[wsi]) = 2 then
          r := r + chr(0);
    end
    else
      r := r + s[wsi];
    inc(wsi);
  end;

  for wsi := 1 to length(r) div 2 do
  begin
    wc := widechar(smallint(ord(r[wsi*2])-1+(ord(r[wsi*2-1])-1) shl 8));
    ws := ws + wc;
  end;

  Result := ws;
end;


procedure TUniHTMListbox.DrawItem(Index: Integer; Rect: TRect;State: TOwnerDrawState);
var
  a,s,f: widestring;
  xsize,ysize,ml,hl: Integer;
  urlcol: TColor;
  hrect,hr: TRect;
  pt: TPoint;
  w : widestring;
  sel: boolean;
begin
  hrect := Rect;

  if Index = Self.Items.Count -1 then
    if rect.Bottom < Height then
    begin
      rect.bottom := Height;
      Canvas.Brush.Color := self.Color;
      Canvas.Pen.Color := self.Color;
      Canvas.Rectangle(rect.Left,rect.Top,rect.Right,rect.Bottom);
    end;

  Rect := hrect;

  if (odSelected in State) and ((GetFocus = Handle) or not HideSelection) then
  begin
    sel := true;
    Canvas.Brush.Color := FSelectionColor;
    Canvas.Pen.Color := FSelectionColor;
    Canvas.Font.Color := FSelectionFontColor;
    Urlcol := FSelectionFontColor;
  end
  else
  begin
    sel := false;
    Canvas.Brush.Color := self.Color;
    Canvas.Pen.Color := self.Color;
    Canvas.Font.Color := self.Font.Color;
    Urlcol := FURLColor;
  end;

  Canvas.Rectangle(rect.Left,rect.Top,rect.Right,rect.Bottom);

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  // Correction for border
  hrect.Left := hrect.Left + 2;
  pt.X := pt.X + 2;

  // Unlimited width simulation if horiz. scroll
  if FScrollHorizontal then
    hrect.Right := hrect.Left + 4096;

  w := WideGet(Items[Index]);


  HTMLDrawEx(Canvas,w,hrect,FImages,pt.x,pt.y,-1,-1,FShadowOffset,False,False,False,sel,True,False,not FEllipsis,1.0,
             urlcol,clNone,clNone,FShadowColor,a,s,f,XSize,YSize,hl,ml,hr,FImageCache,FContainer);

  if odFocused in State then
    DrawFocusRect(Canvas.Handle,Rect);
end;


Procedure TUniHTMListbox.CMHintShow(Var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor: widestring;
  Res,Idx: Integer;
  R: TRect;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);
  Anchor :='';

  if FAnchorHint then
  begin
    Anchor := IsAnchor(hi^.cursorPos.x,hi^.cursorpos.y,res);
    if Anchor <> '' then
    begin
      if Assigned(FOnAnchorHint) then
        FOnAnchorHint(self,res,Anchor);
      hi^.HintPos := ClientToScreen(hi^.CursorPos);
      hi^.Hintpos.y := hi^.Hintpos.Y - 10;
      hi^.Hintpos.x := hi^.Hintpos.X + 10;
      hi^.HintStr := Anchor;
    end;
  end;

  if FItemHint and (Anchor ='') then
  begin
    Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(Hi^.CursorPos.X,Hi^.CursorPos.Y));

    SendMessage(Handle,LB_GETITEMRECT,Idx,LParam(@R));

    if PtInRect(R,Point(Hi^.CursorPos.X,Hi^.CursorPos.Y)) then
    begin
      if HTMLHint then
        Hi^.HintStr := Items[Idx]
      else
        Hi^.HintStr := TextItems[Idx];  
      Hi^.Hintpos.X := 0;
      Hi^.Hintpos.Y := R.Top;
      Hi^.HintPos := ClientToScreen(Hi^.HintPos);
    end;
  end;

  Msg.Result := Ord(Not CanShow);
end;


procedure TUniHTMListbox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := SendMessage(Handle,LB_GETITEMHEIGHT,Index,0);
end;

constructor TUniHTMListbox.Create(aOwner: tComponent);
begin
  inherited Create(aOwner);
  Style := lbOwnerDrawVariable;
  FIsMeasuring := False;
  FURLColor := clBlue;
  FSelectionColor := clHighLight;
  FSelectionFontColor := clHighLightText;
  FTimerID:=0;
  FEnableBlink := False;
  FShadowColor := clGray;
  FShadowOffset := 1;
  FOldAnchor := '';
  FScrollHorizontal := False;
  FImageCache := THTMLPictureCache.Create;
  FLastHintPos := -1;
  DoubleBuffered := True;
end;

procedure TUniHTMListbox.Loaded;
begin
  inherited;
  FOldCursor := self.Cursor;
  if FEnableBlink and (FTimerID = 0) then
    FTimerID := SetTimer(self.Handle,1,100,nil);
  if not FEnableBlink and (FTimerID <> 0) then
    KillTimer(self.Handle,FTimerID);
  ReMeasure;
end;

procedure TUniHTMListbox.SetImages(value:TImagelist);
begin
  FImages := Value;
  ReMeasure;
end;

procedure TUniHTMListbox.SetURLColor(const Value:tColor);
begin
  if Value <> FURLColor then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure TUniHTMListbox.SetSelectionColor(const Value: tColor);
begin
  if Value <> FSelectionColor then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;

procedure TUniHTMListbox.SetSelectionFontColor(const Value: tColor);
begin
  if Value <> fSelectionFontColor then
  begin
    FSelectionFontColor := Value;
    Invalidate;
  end;
end;

procedure TUniHTMListbox.SetMultiLine(value:boolean);
begin
  if Value <> FMultiline then
  begin
    FMultiline := Value;
    ReMeasure;
  end;
end;

function TUniHTMListbox.GetTextItem(index:integer):string;
begin
  if (index >= 0) and (Index < self.Items.Count) then
  begin
    Result := HTMLStrip(self.Items[index]);
  end
  else
    raise EUniHTMListboxError.Create('Item index out of range');
end;

procedure TUniHTMListbox.WndProc(var Message: TMessage);
var
  r,hr: TRect;
  xsize,ysize,ml,hl: Integer;
  a,s,f: widestring;
  w: widestring;
begin
  inherited;

  if Message.msg = WM_DESTROY then
  begin
    if FEnableBlink and (FTimerID <> 0) then
      KillTimer(Handle,fTimerID);
  end;

  if (Message.msg = LB_DELETESTRING) or
     (Message.msg = LB_RESETCONTENT) then
  begin
    if FScrollHorizontal then
      UpdateHScrollExtent(0);
  end;

  if (Message.msg = LB_ADDSTRING) or
     (Message.msg = LB_INSERTSTRING) then
  begin

    SendMessage(Handle,LB_GETITEMRECT,Message.Result,LParam(@r));

    if FScrollHorizontal then
      r.Right := r.Left + 4096
    else
      r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL);

    r.Bottom := r.Top + 4096;

    w := WideGet(Items[message.Result]);

    Canvas.Font.Assign(Font);

    HTMLDrawEx(Canvas,w,r,FImages,0,0,-1,-1,FShadowOffset,True,True,False,True,True,False,not FEllipsis,1.0,
               FURLColor,clNone,clNone,FShadowColor,a,s,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer);

    if YSize > Height then
      YSize := Height - 4;

    SendMessage(Handle,LB_SETITEMHEIGHT,Message.Result,YSize + 4);

    {
    Height := Height + 1;
    Height := Height - 1;
    }

    if FScrollHorizontal and (XSize + 6 > FMaxExtent) then
    begin
      FMaxExtent := XSize + 6;
      UpdateHScrollExtent(fMaxExtent);
    end;

  end;
end;

function TUniHTMListbox.MaxHorizontalExtent:Integer;
var
  r,hr: TRect;
  xsize,ysize,ml,hl,i: Integer;
  a,s,f: widestring;
  w: WideString;
begin
  FMaxExtent := 0;
  for i := 1 to Items.Count do
  begin
    w := WideGet(Items[i-1]);
    HTMLDrawEx(Canvas,w,r,fImages,0,0,-1,-1,fShadowOffset,true,true,false,true,true,false,not FEllipsis,1.0,
               FURLColor,clNone,clNone,fShadowColor,a,s,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer);
    if (XSize + 6 > FMaxExtent) then
      FMaxExtent := XSize + 6;
  end;
  Result := FMaxExtent;
end;

procedure TUniHTMListbox.UpdateHScrollExtent(MaxExtent:Integer);
var
  max,w: Integer;
  r: TRect;
begin
  if FUpdateCount > 0 then
   Exit;

  if (Items.count <= 0) or (FScrollHorizontal = false) then
  begin
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0 );
    SendMessage(Handle, WM_HSCROLL, SB_TOP, 0 );
    Exit;
  end;

  if MaxExtent > 0 then
    Max := MaxExtent
  else
    Max := MaxHorizontalExtent;

  SendMessage(self.Handle,LB_GETITEMRECT,0,LParam(@r));

  w := r.right-r.left;

  if Max > w then
  begin
    SendMessage(Handle,LB_SETHORIZONTALEXTENT,Max,0);
  end
  else
  begin
    SendMessage(Handle,LB_SETHORIZONTALEXTENT,0,0);
    SendMessage(Handle,WM_HSCROLL,SB_TOP,0);
    ShowScrollBar(Handle,SB_HORZ,False);
  end;
end;

procedure TUniHTMListbox.ReMeasure;
var
  i: Integer;
  sel: Boolean;
begin
  FIsMeasuring := True;
  sel := False;

  for i := 1 to Items.Count do
  begin
    if MultiSelect then
      sel := Selected[i - 1];
    Items[i - 1] := Items[i - 1];
    if sel and MultiSelect then
      Selected[i - 1] := sel;
  end;

  FIsMeasuring := False;
  MaxHorizontalExtent;
end;

function TUniHTMListbox.IsAnchor(x,y:integer;var idx:integer):string;
var
  res: Integer;
  r,hr: TRect;
  anchor,stripped,f: widestring;
  xsize,ysize,ml,hl: Integer;
  w: widestring;

begin
  Result := '';
  idx := -1;
  res := loword(SendMessage(self.handle,LB_ITEMFROMPOINT,0,MakeLParam(X,Y)));
  if (res >= 0) and (res < self.Items.Count) then
  begin
    idx := res;
    SendMessage(self.Handle,LB_GETITEMRECT,Res,LParam(@r));

    w := WideGet(Items[res]);

    if HTMLDrawEx(canvas,w,r,fImages,X,Y,-1,-1,fShadowOffset,true,false,false,true,true,false,not FEllipsis,1.0,
                  FURLColor,clNone,clNone,fShadowColor,anchor,stripped,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer) then
    Result := Anchor;
  end;
end;

procedure TUniHTMListbox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  Anchor:string;
  idx: Integer;
begin
  Anchor := IsAnchor(X,Y,idx);
  if Anchor <> '' then
  begin
    if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
      ShellExecute(0,'open',PChar(Anchor),nil,nil,SW_NORMAL)
    else
      begin
        if Assigned(FOnAnchorClick) then
          FOnAnchorClick(Self,idx,Anchor);
      end;
    Exit;
  end;
  inherited MouseDown(Button,Shift,X,Y);
end;

procedure TUniHTMListbox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
  Idx: integer;
  R: TRect;
begin
  inherited MouseMove(Shift,X,Y);

  if FItemHint then
  begin
    Idx := SendMessage(Handle,LB_ITEMFROMPOINT,0,MakeLParam(X,Y));
    SendMessage(Handle,LB_GETITEMRECT,Idx,LParam(@R));
    if PtInRect(R,Point(X,Y)) then
    begin
      if Idx <> FLastHintPos then
      begin
        Application.CancelHint;
        FLastHintPos := Idx;
      end
    end
    else
    begin
      if FLastHintPos >= 0 then
      begin
        Application.CancelHint;
        FLastHintPos := -1;
      end;
    end;
  end;

  Anchor := IsAnchor(x,y,idx);

  if Anchor <> '' then
  begin
    //Invalidate;
    if FOldAnchor <> Anchor then
    begin
      Application.Cancelhint;
      if Assigned(FOnAnchorExit) then
        FOnAnchorExit(self,idx,FOldAnchor);
    end;

    if self.Cursor <> crHandPoint then
    begin
      FOldCursor := self.Cursor;
      self.Cursor := crHandPoint;
    end;

    if Assigned(FOnAnchorEnter) then
      FOnAnchorEnter(self,idx,Anchor);

    FOldAnchor:=anchor;
  end
  else
  begin
    if self.Cursor = crHandPoint then
    begin
      //Invalidate;
      if (FOldAnchor <> '') then
      begin
        Application.CancelHint;
        self.Cursor := FOldCursor;
        if Assigned(FOnAnchorExit) then
          FOnAnchorExit(self,idx,fOldAnchor);
        FOldAnchor := '';
      end;
    end;
  end;
end;

procedure TUniHTMListbox.WMSize(var Msg: TWMSize);
begin
  inherited;
  if not FIsMeasuring then
    ReMeasure;
    
  if FScrollHorizontal then
    UpdateHScrollExtent(0);
end;

procedure TUniHTMListbox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
  inherited;
end;

procedure TUniHTMListbox.WMTimer(var Msg: TWMTimer);
var
  i,i1,i2: Integer;
  r: TRect;
  a,s,f: widestring;
  xsize,ysize: Integer;
  sel: Boolean;
  hr:trect;
  hl,ml:integer;
  DoAnim: Boolean;
  w: widestring;

begin
  if (Items.count = 0) or not FEnableBlink then exit;

  DoAnim := False;

  if Assigned(FImageCache) then
    if FImageCache.Animate then
      DoAnim := True;

  if Assigned(FContainer) then
    if FContainer.Items.Animate then
      DoAnim := True;

  if DoAnim then
    Invalidate;

  inc(FTimerCount);

  if FTimerCount mod 5 <>0 then Exit;

  r := Getclientrect;
  i1 := SendMessage(handle,LB_ITEMFROMPOINT,0,makelparam(0,r.Top));
  i2 := SendMessage(handle,LB_ITEMFROMPOINT,0,makelparam(0,r.Bottom));

  if i1 < 0 then i1 := 0;
  if i2 > Items.Count - 1 then
    i2 := Items.Count - 1;

  for i:=i1 to i2 do
  begin
    w := WideGet(Items[i]);
    {only redraw items with blinking}
    if Pos('<BLINK',w)>0 then
    begin
      SendMessage(handle,LB_GETITEMRECT,i,LParam(@r));
      sel := SendMessage(handle,LB_GETSEL,i,0) > 0;
      r.left := r.Left + 2;
      if not sel then
      begin
        Canvas.Brush.color := self.Color;
        Canvas.Font.color := self.Font.Color;
      end
      else
      begin
        Canvas.Brush.Color := self.SelectionColor;
        Canvas.Font.Color := self.SelectionFontColor;
      end;

      HTMLDrawEx(Canvas,w,r,FImages,0,0,-1,-1,FShadowOffset,False,False,False,Selected[i],FBlinking,False,not FEllipsis,1.0,fURLColor,clNone,clNone,fShadowColor,
                 a,s,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer);
    end;
  end;
  FBlinking := not FBlinking;
end;

procedure TUniHTMListbox.SetEnableBlink(const Value: boolean);
begin
  FEnableBlink := Value;
  if not (csLoading in ComponentState) then
  begin
    if FEnableBlink and (FTimerID = 0) then
      FTimerID := SetTimer(self.Handle,1,100,nil);
    if not FEnableBlink and (FTimerID <> 0) then
    begin
      KillTimer(self.handle,fTimerID);
      FTimerID := 0;
      FBlinking := False;
      Invalidate;
    end;
  end;
end;

procedure TUniHTMListbox.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  self.Invalidate;
end;

procedure TUniHTMListbox.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  self.Invalidate;
end;

function TUniHTMListbox.GetSortedEx: boolean;
begin
  Result := FSortedEx;
end;

function DoStrip(s:string; NoStrip: Boolean): string;
begin
  if NoStrip then
    Result := s
  else
    Result := HTMLStrip(s);
end;

procedure TUniHTMListbox.QuickSortList(List:TStringList;left,right:integer);
var
  i,j:integer;
  s,sw: string;

  begin
  i := Left;
  j := Right;

  {get middle item here}
  s := DoStrip(List.Strings[(left+right) shr 1],FSortWithHTML);

  repeat
    {$IFDEF VER90}
    while (StrComp(pchar(s),PChar(DoStrip(List.Strings[i],FSortWithHTML))) > 0) and (i<right) do inc(i);
    while (StrComp(pchar(s),PChar(DoStrip(List.Strings[j],FSortWithHTML))) < 0) and (j>left) do dec(j);
    {$ELSE}
    while (AnsiStrComp(pchar(s),PChar(DoStrip(List.Strings[i],FSortWithHTML))) > 0) and (i<right) do inc(i);
    while (AnsiStrComp(pchar(s),PChar(DoStrip(List.Strings[j],FSortWithHTML))) < 0) and (j>left) do dec(j);
    {$ENDIF}
    if (i<=j) then
    begin
      if (i<>j) then
      begin
        {$IFDEF VER90}
        if StrComp(pchar(DoStrip(List.Strings[i],FSortWithHTML)),pchar(DoStrip(List.Strings[j],FSortWithHTML)))<>0 then
        {$ELSE}
        if AnsiStrComp(pchar(DoStrip(List.Strings[i],FSortWithHTML)),pchar(DoStrip(List.Strings[j],FSortWithHTML)))<>0 then
        {$ENDIF}
        begin
          sw := List.Strings[i];
          List.Strings[i] := List.Strings[j];
          List.Strings[j] := sw;
        end;
      end;
      inc(i);
      dec(j);
    end;
  until i > j;

  if Left < j then QuicksortList(List,Left,j);
  if i < Right then QuickSortList(List,i,Right);
end;

procedure TUniHTMListbox.SetSortedEx(const Value: boolean);
var
  sl: TStringList;
begin
  FSortedEx := Value;

  if Value then
  begin
    sl := TStringList.Create;
    sl.Assign(Items);

    if sl.Count > 1 then
      QuickSortList(sl,0,sl.Count-1);

    Items.Assign(sl);
    sl.Free;
  end;
end;

procedure TUniHTMListbox.KeyDown(var Key: Word; Shift: TShiftState);
var
  i: Integer;
  s: String;

  function Max(a,b: Integer):Integer;
  begin
    if a > b then Result := a else Result := b;
  end;

begin
  inherited;
  if Key in [vk_up,vk_down,vk_left,vk_right,vk_next,vk_prior,vk_home,vk_end,vk_escape] then
  begin
    FLookup := '';
    Exit;
  end;

  if (key = vk_back) and (Length(FLookup)>0) then
    Delete(FLookup,Length(FLookup),1)
  else
  if not FIncrLookup then
    FLookup := chr(key)
  else
    if (key>31) and (key<=255) then FLookup := fLookup+chr(key);

  if (ItemIndex>=0) or (FIncrLookup) then
  begin
    for i := Max(1,ItemIndex+1) to Items.Count do
    begin
      s := TextItems[i-1];
      if s <> '' then
        if Pos(UpperCase(FLookup),Uppercase(s)) = 1 then
        begin
          ItemIndex := i-1;
          Exit;
        end;
    end;
  end;

  for i := 1 to Items.Count do
  begin
    s := TextItems[i-1];
    if s <> '' then
      if Pos(UpperCase(FLookup),Uppercase(s)) = 1 then
    begin
      ItemIndex := i-1;
      Exit;
    end;
  end;

  if FIncrLookup then
  begin
    FLookup := chr(key);
    for i := 1 to Items.Count do
    begin
      s := TextItems[i-1];
      if s <> '' then
        if Pos(Uppercase(FLookup),Uppercase(s)) = 1 then
        begin
          ItemIndex := i-1;
          Exit;
        end;
    end;
  end;
end;

procedure TUniHTMListbox.DoEnter;
begin
  inherited;
  FLookup := '';
end;

procedure TUniHTMListbox.SetScrollHorizontal(const Value: boolean);
begin
  if FScrollHorizontal <> Value then
  begin
    FScrollHorizontal := Value;
    UpdateHScrollExtent(0);
  end;
end;

procedure TUniHTMListbox.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TUniHTMListbox.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      UpdateHScrollExtent(0);
    end;  
  end;
end;

destructor TUniHTMListbox.Destroy;
begin
  FImageCache.Free;
  inherited;
end;

procedure TUniHTMListbox.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure TUniHTMListbox.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  ReMeasure;
end;

procedure TUniHTMListbox.HilightInItem(Index: Integer; HiText: widestring;
  DoCase: Boolean);
begin
  WideItems[Index] := Hilight(WideItems[Index],HiText,'hi',DoCase);
end;

procedure TUniHTMListbox.HilightInList(HiText: widestring; DoCase: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    WideItems[i - 1] := Hilight(WideItems[i - 1],HiText,'hi',DoCase);
  EndUpdate;
end;

procedure TUniHTMListbox.MarkInItem(Index: Integer; HiText: widestring;
  DoCase: Boolean);
begin
  WideItems[Index] := Hilight(WideItems[Index],HiText,'e',DoCase);
end;

procedure TUniHTMListbox.MarkInList(HiText: widestring; DoCase: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    WideItems[i - 1] := Hilight(WideItems[i - 1],HiText,'e',DoCase);
  EndUpdate;
end;

procedure TUniHTMListbox.UnHilightInItem(Index: Integer);
begin
  WideItems[Index] := UnHilight(WideItems[Index],'hi');
end;

procedure TUniHTMListbox.UnHilightInList;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    WideItems[i - 1] := UnHilight(WideItems[i - 1],'hi');
  EndUpdate;
end;

procedure TUniHTMListbox.UnMarkInItem(Index: Integer);
begin
  WideItems[Index] := UnHilight(WideItems[Index],'e');
end;

procedure TUniHTMListbox.UnMarkInList;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    WideItems[i - 1] := UnHilight(WideItems[i - 1],'e');
  EndUpdate;
end;

function TUniHTMListbox.GetWideItem(Index: Integer): widestring;
begin
  Result := WideGet(Items[Index]);
end;

procedure TUniHTMListbox.SetWideItem(Index: Integer;
  const Value: widestring);
begin
  Items[Index] := WideSet(Value);
end;

procedure TUniHTMListbox.AddWideItem(Value: widestring);
begin
  Items.Add(WideSet(Value));
end;

procedure TUniHTMListbox.DeleteWideItem(Index: Integer);
begin
  Items.Delete(Index);
end;

procedure TUniHTMListbox.SetHideSelection(const Value: Boolean);
begin
  FHideSelection := Value;
  Invalidate;
end;

function TUniHTMListbox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TUniHTMListbox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TUniHTMListbox.SetVersion(const Value: string);
begin

end;

end.
