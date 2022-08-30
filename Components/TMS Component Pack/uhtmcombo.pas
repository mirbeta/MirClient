{************************************************************************}
{ TUniHTMLComboBox component                                             }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ Copyright © 2002-2011                                                  }
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

unit uhtmcombo;

{$I TMSDEFS.INC}
{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  PictureContainer;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release


type
  EHTMLComboBoxError = class(Exception);

  TAnchorClick = procedure(Sender:TObject;Index:Integer;Anchor:widestring) of object;

  TUniBaseCustomComboBox = class(TCustomComboBox)
  private
    FAutoFocus: boolean;
    FFlat: Boolean;
    FEtched: Boolean;
    FOldColor: TColor;
    FOldParentColor: Boolean;
    FButtonWidth: Integer;
    FFocusBorder: Boolean;
    FMouseInControl: Boolean;
    fDropWidth: integer;
    procedure SetEtched(const Value: Boolean);
    procedure SetFlat(const Value: Boolean);
    procedure SetButtonWidth(const Value: Integer);
    procedure DrawButtonBorder(DC:HDC);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawBorders;
    function  Is3DBorderControl: Boolean;
    function  Is3DBorderButton: Boolean;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CNCommand (var Message: TWMCommand); message CN_COMMAND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure SetDropWidth(const Value: integer);
  protected
    property ButtonWidth: integer read fButtonWidth write SetButtonWidth;
    property Flat: Boolean read FFlat write SetFlat;
    property Etched: Boolean read FEtched write SetEtched;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder;
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus;
    property DropWidth: integer read FDropWidth write SetDropWidth;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TUniHTMLCombobox = class(TUniBaseCustomComboBox)
  private
    FURLColor:TColor;
    FIncrLookup:boolean;
    FImages:TImageList;
    FDropHeight:integer;
    FOldAnchor:string;
    FLookup:string;
    FItemIndex:integer;
    FSortedEx: boolean;
    FDropped: boolean;
    FAnchorClick:TAnchorClick;
    FAnchorEnter:TAnchorClick;
    FAnchorExit:TAnchorClick;
    FEllipsis: Boolean;
    FContainer: TPictureContainer;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FImageCache:THTMLPictureCache;
    FItemHint: Boolean;
    FHTMLHint: Boolean;
    procedure SetDropWidth(value:integer);
    function GetDropWidth:integer;
    procedure SetEditHeight(value:integer);
    function GetEditHeight:integer;
    procedure SetImages(const Value: TImageList);
    procedure SetURLColor(const Value: TColor);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg:TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMChar(var Msg:TWMChar); message WM_CHAR;
    function GetTextItem(index:integer): widestring;
    procedure SetItemIndexP(const Value : integer);
    function GetItemIndexP:integer;
    function GetSortedEx: boolean;
    procedure SetSortedEx(const Value: Boolean);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    function GetWideItem(Index: Integer): widestring;
    procedure SetWideItem(Index: Integer; const Value: widestring);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure SetStyle(Value: TComboBoxStyle); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    property TextItems[index: Integer]: widestring read GetTextItem;
    property Text;
  published
    property Anchors;
    property Constraints;
    property AutoFocus;
    property ButtonWidth;
    property Style;
    property Flat;
    property Etched;
    property FocusBorder;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropWidth: Integer read GetDropWidth write SetDropWidth;
    property DropHeight: Integer read FDropHeight write FDropHeight;
    property EditHeight: Integer read GetEditheight write SetEditHeight;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property Enabled;
    property Font;
    property HTMLHint: Boolean read FHTMLHint write FHTMLHint;
    property Images:TImageList read FImages write SetImages;
    property IncrLookup: Boolean read FIncrLookup write FIncrLookup default false;
    property ItemHeight;
    property ItemHint: Boolean read FItemHint write FItemHint;
    property ItemIndex: Integer read GetItemIndexP write SetItemIndexP;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset;
    property ShowHint;
    property Sorted: Boolean read GetSortedEx write SetSortedEx;
    property TabOrder;
    property TabStop;
    property URLColor: TColor read fURLColor write SetURLColor;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

implementation
uses
  Extctrls,ShellApi,CommCtrl,Forms ,ImgList;

{$I htmlengu.pas}

function WideSet(s: widestring): string;
var
  i: Integer;
  wc: widechar;
  d: string;
begin
  for i := 1 to length(s) do
  begin
    wc := s[i];
    d := d + chr(((smallint(wc) and $FF00) shr 8)+1);
    d := d + chr((smallint(wc) and $FF));
  end;
  Result := d;
end;

function WideGet(s: string): widestring;
var
  ws: widestring;
  wsi: Integer;
  wc: widechar;
begin
  ws := '';
  for wsi := 1 to length(s) div 2 do
  begin
    wc := widechar(smallint(ord(s[wsi*2])+(ord(s[wsi*2-1])-1) shl 8));
    ws := ws + wc;
  end;
  Result := ws;
end;


procedure TUniHTMLCombobox.SetStyle(Value: TComboBoxStyle);
begin
//  inherited SetStyle(csOwnerDrawVariable);
  inherited SetStyle(csOwnerDrawFixed);
end;

procedure TUniHTMLCombobox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r,hr: TRect;
  a,s,fa : widestring;
  xsize,ysize,hoverlink,mouselink : Integer;
  urlcol: TColor;
  w: widestring;
begin
  r := Rect;
  if odSelected in State then
  begin
    Canvas.Brush.Color := clHighLight;
    Canvas.Pen.Color := clHighLight;
    Canvas.Font.Color := clHighLightText;
    Urlcol := clHighLightText;
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.Pen.Color := Color;
    Canvas.Font.Color := Font.Color;
    Urlcol := FURLColor;
   end;

  Canvas.Rectangle(r.left,r.top,r.right,r.bottom);

  Offsetrect(r,2,0);

  w := WideItems[Index];

  HTMLDrawEx(Canvas,w,r,FImages,0,0,-1,-1,FShadowOffset,False,False,False,(odSelected in State),True,False,not Ellipsis,
             1.0,URLCol,clNone,clNone,FShadowColor,a,s,fa,xsize,ysize,hoverlink,mouselink,hr,FImageCache,FContainer);

  Canvas.Brush.Color := self.Color;
  Canvas.Pen.Color := self.Color;
end;

procedure TUniHTMLCombobox.CreateWnd;
begin
  inherited CreateWnd;
end;

constructor TUniHTMLCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawFixed;
  FURLColor := clBlue;
  FDropHeight := 200;
  FOldAnchor := '';
  FLookup := '';
  FItemIndex := -1;
  FImageCache := THTMLPictureCache.Create;
end;

procedure TUniHTMLCombobox.MeasureItem(Index: Integer; var Height: Integer);
var
  res,xsize,ysize,hoverlink,mouselink: Integer;
  r,hr: TRect;
  a,s,fa: widestring;
  Canvas: TCanvas;
  w: widestring;
begin
  height := 40;
  if (index >= 0) and (index < self.Items.Count) then
  begin
    r := GetClientRect;
    r.bottom := r.top + 1000;
    Canvas := TCanvas.Create;
    Canvas.handle := GetDC(self.Handle);

    w := WideItems[Index];

    HTMLDrawEx(Canvas,w,r,FImages,0,0,-1,-1,FShadowOffset,True,True,False,True,True,False,not Ellipsis,
               1.0,FURLColor,clNone,clNone,FShadowColor,a,s,fa,xsize,ysize,hoverlink,mouselink,hr,FImageCache,FContainer);

    ReleaseDC(Handle,Canvas.Handle);
    Canvas.Free;
    res := ysize + 4;
    SendMessage(self.handle,CB_SETITEMHEIGHT,index,res);
  end
  else
    Res := EditHeight;
  Height := Res;
end;

function TUniHTMLCombobox.GetDropWidth: Integer;
begin
  Result := SendMessage(self.handle,CB_GETDROPPEDWIDTH,0,0);
end;

procedure TUniHTMLCombobox.SetDropWidth(value: Integer);
begin
  SendMessage(self.Handle,CB_SETDROPPEDWIDTH,value,0);
end;

function TUniHTMLCombobox.GetEditHeight: integer;
begin
  Result := SendMessage(self.Handle,CB_GETITEMHEIGHT,-1,0);
end;

procedure TUniHTMLCombobox.SetEditHeight(value: integer);
begin
  SendMessage(self.Handle,CB_SETITEMHEIGHT,-1,value);
  SendMessage(self.Handle,CB_SETITEMHEIGHT,0,value);
end;

procedure TUniHTMLCombobox.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Repaint;
end;

procedure TUniHTMLCombobox.CNCommand(var Message: TWMCommand);
begin
  case message.NotifyCode of
  CBN_DROPDOWN:
  begin
    MoveWindow(self.Handle,self.left,self.top,width,EditHeight + FDropheight,True);
    DropDown;
    message.Result := 0;
    FDropped := True;
    if Assigned(OnClick) then
      OnClick(Self);
  end;
  CBN_SELCHANGE:
  begin
    FDropped := False;
    FItemIndex := SendMessage(self.Handle,CB_GETCURSEL,0,0);
    if Assigned(OnChange) then
      OnChange(Self);
  end;
  else
    inherited;
  end;
end;

procedure TUniHTMLCombobox.WMLButtonUp(var Msg:TWMLButtonDown);
begin
  inherited;
  if FDropped and (FItemIndex <> -1) then
  begin
    ItemIndex := FItemIndex;
    if SendMessage(self.Handle,CB_GETDROPPEDSTATE,0,0) = 0 then
      FDropped := false;
  end;
end;

procedure TUniHTMLCombobox.WMLButtonDown(var Msg:TWMLButtonDown);
var
  res: Integer;
  r,hr: TRect;
  anchor,stripped,fa: widestring;
  xsize,ysize,hoverlink,mouselink: Integer;
  Canvas: TCanvas;
  w: widestring;
begin
  res := self.ItemIndex;
                                            
  if (res >= 0) and (res < self.Items.Count) then
  begin
    r := ClientRect;
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(self.Handle);

    r.Left := r.Left + 6;

    w := WideItems[res];

    if HTMLDrawEx(Canvas,w,r,FImages,msg.XPos,msg.YPos,-1,-1,FShadowOffset,True,False,False,True,True,False,not Ellipsis,
             1.0,FURLColor,clNone,clNone,FShadowColor,anchor,stripped,fa,xsize,ysize,hoverlink,mouselink,hr,FImageCache,FContainer) then
    begin
      ReleaseDC(self.Handle,Canvas.Handle);
      Canvas.Free;

      if (Pos('://',Anchor)>0) or (Pos('mailto:',Anchor)>0) then
        ShellExecuteW(0,'open',PWideChar(Anchor),nil,nil,SW_NORMAL)
      else
      begin
        if Assigned(FAnchorClick) then
          FAnchorClick(self,Res,Anchor);
      end;
      msg.Result:=0;
      Exit;
    end;
    ReleaseDC(self.Handle,Canvas.Handle);
    Canvas.Free;
  end;

 inherited;
end;



procedure TUniHTMLCombobox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  res: Integer;
  r,hr: TRect;
  a,s,fa: widestring;
  xsize,ysize,hoverlink,mouselink: Integer;
  canvas: TCanvas;
  w: widestring;
begin
  inherited MouseMove(Shift,X,Y);

  res := self.ItemIndex;

  if (res>=0) and (res<self.Items.Count) then
  begin
    r := ClientRect;
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(self.Handle);

    w := WideItems[res];

    r.Left := r.Left + 6;

    if HTMLDrawEx(Canvas,w,r,FImages,X,Y,-1,-1,FShadowOffset,True,False,False,True,True,False,not Ellipsis,
             1.0,FURLColor,clNone,clNone,FShadowColor,a,s,fa,xsize,ysize,hoverlink,mouselink,hr,FImageCache,FContainer) then
    begin
      if (a <> fOldAnchor) then
        if Assigned(FAnchorExit) then
          FAnchorEnter(self,Res,FOldAnchor);
      if Assigned(FAnchorEnter) then
        FAnchorEnter(self,res,a);
      self.Cursor := crHandPoint;
      FOldAnchor := a;
    end
    else
    begin
      if (self.Cursor <> crDefault) or (a <> FOldAnchor) then
        if Assigned(FAnchorExit) then
          FAnchorExit(self,res,FOldAnchor);
      self.Cursor := crDefault;
      FOldAnchor := '';
    end;

    ReleaseDC(self.Handle,Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TUniHTMLCombobox.SetURLColor(const Value: TColor);
begin
  FURLColor := Value;
  Invalidate;
end;

function TUniHTMLCombobox.GetTextItem(Index: Integer): widestring;
var
  xsize,ysize,hoverlink,mouselink:integer;
  a,s,fa:widestring;
  r,hr: TRect;
  Canvas: TCanvas;
  w: widestring;
begin
  Result := '';

  if (Index >= 0) and (Index < self.Items.Count) then
  begin
    r := self.ClientRect;
    Canvas := TCanvas.Create;
    Canvas.Handle := GetDC(self.Handle);

    w := WideItems[Index];

    HTMLDrawEx(Canvas,Items[index],r,FImages,0,0,-1,-1,FShadowOffset,True,True,False,True,True,False,not Ellipsis,
               1.0,FURLColor,clNone,clNone,FShadowColor,a,s,fa,xsize,ysize,hoverlink,mouselink,hr,FImageCache,FContainer);

    ReleaseDC(self.Handle,Canvas.Handle);
    Canvas.Free;
    Result := s;
  end
  else
    raise EHTMLComboboxError.Create('Item index out of range');
end;


function TUniHTMLCombobox.GetItemIndexP: integer;
begin
  Result := SendMessage(self.Handle,CB_GETCURSEL,0,0);
end;

procedure TUniHTMLCombobox.SetItemIndexP(const Value: integer);
begin
  if FDropped then
    FItemIndex := Value;
  SendMessage(self.handle,CB_SETCURSEL,value,0);
end;

procedure TUniHTMLCombobox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;

procedure TUniHTMLCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  Inherited;

  if key in [vk_up,vk_down,vk_left,vk_right,vk_next,vk_prior,vk_home,vk_end,vk_escape] then
      fLookup:='';

  if (key=vk_back) and (length(fLookup)>0) then delete(fLookup,length(fLookup),1);
end;

procedure TUniHTMLCombobox.DoEnter;
begin
  inherited;
  fLookup:='';
end;

procedure TUniHTMLCombobox.WMChar(var Msg: TWMChar);
var
  i:integer;
  s:string;
  Key: Char;

  function Max(a,b:integer):integer;
  begin
   if a>b then result:=a else result:=b;
  end;

begin
  Inherited;

  Key := Chr(Msg.CharCode);

  if not fIncrLookup then fLookup:=Key else
  fLookup:=fLookup+Key;

  if (ItemIndex>=0) or (fIncrLookup) then
   begin
      for i:=Max(1,ItemIndex+1) to Items.Count do
       begin
        s:=TextItems[i-1];
        if (s<>'') then
        if (pos(uppercase(fLookup),uppercase(s))=1) then
          begin
           ItemIndex:=i-1;
           Exit;
          end;
       end;
   end;

  for i:=1 to Items.Count do
   begin
    s:=TextItems[i-1];
    if (s<>'') then
    if (pos(uppercase(fLookup),uppercase(s))=1) then
      begin
       ItemIndex:=i-1;
       Exit;
      end;
   end;

  if fIncrLookup then
   begin
    fLookup:=Key;
    for i:=1 to Items.Count do
     begin
      s:=TextItems[i-1];
      if (s<>'') then
      if (pos(uppercase(fLookup),uppercase(s))=1) then
       begin
        ItemIndex:=i-1;
        Exit;
       end;
     end;
   end;
end;

function TUniHTMLCombobox.GetSortedEx: boolean;
begin
  Result := FSortedEx;
end;

function HTMLCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiStrComp(pchar(HTMLStrip(List.Strings[Index1])),pchar(HTMLStrip(List.Strings[Index2])));
end;


procedure TUniHTMLCombobox.SetSortedEx(const Value: boolean);
var
 sl: TStringList;
 idx: integer;
begin
 fsortedEx := Value;

 if Value then
  begin
   idx := ItemIndex;
   sl := TStringList.Create;
   sl.Assign(Items);

   sl.CustomSort(HTMLCompare);

   Items.Assign(sl);
   sl.Free;
   ItemIndex := idx;
  end;

end;


procedure TUniHTMLCombobox.SetEllipsis(const Value: Boolean);
begin
  FEllipsis := Value;
  Invalidate;
end;

procedure TUniHTMLCombobox.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure TUniHTMLCombobox.SetShadowOffset(const Value: Integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

destructor TUniHTMLCombobox.Destroy;
begin
  FImageCache.Free;
  inherited;
end;

procedure TUniHTMLCombobox.CMHintShow(var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor: string;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);
  Anchor :='';

  if FItemHint then
  begin
    if HTMLHint then
      Hi^.HintStr := Items[ItemIndex]
    else
      Hi^.HintStr := TextItems[ItemIndex];
    Hi^.Hintpos.X := 0;
    Hi^.Hintpos.Y := 0;
    Hi^.HintPos := ClientToScreen(Hi^.HintPos);
  end;
  Msg.Result := Ord(Not CanShow);
end;

procedure TUniHTMLCombobox.HilightInItem(Index: Integer; HiText: widestring;
  DoCase: Boolean);
begin
  WideItems[Index] := Hilight(Items[Index],HiText,'hi',DoCase);
end;

procedure TUniHTMLCombobox.HilightInList(HiText: widestring; DoCase: Boolean);
var
  i: Integer;
begin
  for i := 1 to Items.Count do
    WideItems[i - 1] := Hilight(WideItems[i - 1],HiText,'hi',DoCase);
end;

procedure TUniHTMLCombobox.MarkInItem(Index: Integer; HiText: widestring;
  DoCase: Boolean);
begin
  WideItems[Index] := Hilight(WideItems[Index],HiText,'e',DoCase);
end;

procedure TUniHTMLCombobox.MarkInList(HiText: widestring; DoCase: Boolean);
var
  i: Integer;
begin
  for i := 1 to Items.Count do
    WideItems[i - 1] := Hilight(WideItems[i - 1],HiText,'e',DoCase);
end;

procedure TUniHTMLCombobox.UnHilightInItem(Index: Integer);
begin
  WideItems[Index] := UnHilight(WideItems[Index],'hi');
end;

procedure TUniHTMLCombobox.UnHilightInList;
var
  i: Integer;
begin
  for i := 1 to Items.Count do
    WideItems[i - 1] := UnHilight(WideItems[i - 1],'hi');
end;

procedure TUniHTMLCombobox.UnMarkInItem(Index: Integer);
begin
  WideItems[Index] := UnHilight(WideItems[Index],'e');
end;

procedure TUniHTMLCombobox.UnMarkInList;
var
  i: Integer;
begin
  for i := 1 to Items.Count do
    WideItems[i - 1] := UnHilight(WideItems[i - 1],'e');
end;

{ TUniBaseCustomComboBox }
constructor TUniBaseCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
  FOldColor := inherited Color;
  FOldParentColor := inherited ParentColor;
  FFlat := False;
  FMouseInControl := false;
end;

procedure TUniBaseCustomComboBox.SetButtonWidth(const Value: integer);
begin
  if (value<14) or (value>32) then
    Exit;

  FButtonWidth:=value;
  Invalidate;
end;

procedure TUniBaseCustomComboBox.SetFlat(const Value: Boolean);
begin
  if Value<>FFlat then
  begin
    FFlat:=Value;
    Ctl3D:=not Value;
    Invalidate;
  end;
end;

procedure TUniBaseCustomComboBox.SetEtched(const Value: Boolean);
begin
  if Value<>FEtched then
  begin
    FEtched:=Value;
    Invalidate;
  end;
end;

procedure TUniBaseCustomComboBox.CMEnter(var Message: TCMEnter);
begin
  inherited;
  if not (csDesigning in ComponentState) then DrawBorders;
end;

procedure TUniBaseCustomComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  if not (csDesigning in ComponentState) then DrawBorders;
end;

procedure TUniBaseCustomComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
    begin
     FMouseInControl := True;
     DrawBorders;
    end;
  if fAutoFocus then self.SetFocus;
end;

procedure TUniBaseCustomComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
    begin
     FMouseInControl := False;
     DrawBorders;
    end;
end;

procedure TUniBaseCustomComboBox.CMEnabledChanged(var Msg: TMessage);
begin
  if FFlat then
   begin
    if Enabled then
      begin
       inherited ParentColor:=FOldParentColor;
       inherited Color:=FOldColor;
      end
    else
      begin
       FOldParentColor:=inherited Parentcolor;
       FOldColor:=inherited Color;
       inherited ParentColor:=True;
      end;
   end;
 inherited;
end;

procedure TUniBaseCustomComboBox.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if FFlat then DrawBorders;
end;

procedure TUniBaseCustomComboBox.WMPaint(var Message: TWMPaint);
var
   DC: HDC;
   PS: TPaintStruct;

   procedure DrawButton;
   var
     ARect: TRect;
   begin
     GetWindowRect(Handle, ARect);
     OffsetRect(ARect, -ARect.Left, -ARect.Top);
     Inc(ARect.Left, ClientWidth - FButtonWidth);
     InflateRect(ARect, -1, -1);
     DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT );
     ExcludeClipRect(DC, ClientWidth - FButtonWidth - 2, 0, ClientWidth, ClientHeight);
   end;

begin

  if not FFlat then
    begin
     inherited;
     Exit;
    end;

  if Message.DC = 0 then
    DC:=BeginPaint(Handle, PS)
  else
    DC:=Message.DC;
  try
    if (Style<>csSimple) then
      begin
       FillRect(DC, ClientRect, Brush.Handle);
       DrawButton;
      end;
    PaintWindow(DC);
  finally
    if Message.DC=0 then
      EndPaint(Handle, PS);
  end;
  DrawBorders;
end;

function TUniBaseCustomComboBox.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (GetFocus = Handle);

  Result := Result and FFocusBorder;
end;

function TUniBaseCustomComboBox.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (GetFocus = Handle);
end;

procedure TUniBaseCustomComboBox.DrawButtonBorder(DC: HDC);
const
   Flags: array[Boolean] of Integer = (0, BF_FLAT);
   Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);
var
   ARect: TRect;
   BtnFaceBrush: HBRUSH;
begin

  ExcludeClipRect(DC, ClientWidth - FButtonWidth + 4, 4, ClientWidth - 4, ClientHeight - 4);

  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth - 2);
  InflateRect(ARect, -2, -2);

  if Is3DBorderButton then
   DrawEdge(DC, ARect, Edge[Etched], BF_RECT or Flags[DroppedDown])
  else
    begin
     BtnFaceBrush:=CreateSolidBrush(GetSysColor(COLOR_BTNFACE));
     InflateRect(ARect, 0, -1);
     arect.right:=arect.right-1;
     FillRect(DC, ARect, BtnFaceBrush);
     DeleteObject(BtnFaceBrush);
    end;

  ExcludeClipRect(DC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TUniBaseCustomComboBox.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin

  if Is3DBorderControl then
   BtnFaceBrush:=CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
   BtnFaceBrush:=CreateSolidBrush(ColorToRGB((parent as TWinControl).brush.color));

  //WindowBrush:=CreateSolidBrush(GetSysColor(COLOR_WINDOW));
  WindowBrush:=CreateSolidBrush(ColorToRGB(self.Color));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
     begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
     end
    else
     begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
     end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

procedure TUniBaseCustomComboBox.DrawBorders;
var
  DC: HDC;
begin
  if not FFlat then Exit;
  DC := GetWindowDC(Handle);
  try
   DrawControlBorder(DC);
   if (Style<>csSimple) then DrawButtonBorder(DC);
  finally
   ReleaseDC(DC, Handle);
  end;
end;

procedure TUniBaseCustomComboBox.CNCommand(var Message: TWMCommand);
var
  r:TRect;
begin
  inherited;
 
  if (Message.NotifyCode in [CBN_CLOSEUP,CBN_DROPDOWN]) then
  begin
    r := GetClientRect;
    r.Left := r.Right - Fbuttonwidth;
    InvalidateRect(Handle,@r,FALSE);
  end;
end;


procedure TUniBaseCustomComboBox.SetDropWidth(const Value: integer);
begin
  FDropWidth := Value;
  if Value > 0 then
    SendMessage(self.Handle,CB_SETDROPPEDWIDTH,FDropWidth,0);
end;

procedure TUniHTMLCombobox.AddWideItem(Value: widestring);
begin
  Items.Add(WideSet(Value));
end;

procedure TUniHTMLCombobox.DeleteWideItem(Index: Integer);
begin
  Items.Delete(Index);
end;

function TUniHTMLCombobox.GetWideItem(Index: Integer): widestring;
begin
  Result := WideGet(Items[Index]);
end;

procedure TUniHTMLCombobox.SetWideItem(Index: Integer;
  const Value: widestring);
begin
  Items[Index] := WideSet(Value);
end;

function TUniHTMLCombobox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TUniHTMLCombobox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TUniHTMLCombobox.SetVersion(const Value: string);
begin

end;

end.
