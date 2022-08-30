{**************************************************************************}
{ THTMListBox component                                                    }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ Copyright © 2001 - 2015                                                  }
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

unit HTMListB;

{$I TMSDEFS.INC}
{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Comobj, ActiveX, PictureContainer, AdvGradient, AdvStyleIF, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.8.0.1 : Fixed issue with item sizing
  // 1.8.0.2 : Fixed issue with initial scrollbar display on component load
  // 1.9.0.0 : New property AutoItemHeight added
  //         : New styler interface added with Office 2007 gradient selection colors
  //         : New ShowFocus property added
  // 1.9.0.1 : Fixed issue with AutoItemHeight
  // 1.9.1.0 : New added support for Office 2007 silver style
  // 1.9.1.1 : Fixed issue with ItemHeight
  // 1.9.1.2 : Fixed issue with sizing
  // 1.9.1.3 : Fixed small issue with item autosize height
  // 1.9.1.4 : Fixed issue with AutoItemHeight
  // 1.9.1.5 : Fixed issue with AutoItemHeight = false
  // 1.9.1.6 : Fixed issue with showing focused item in Delphi 2009

  // 2.0.0.0 : New : AutoFocus property
  //         : New : SelectionColors property
  //         : New : Windows Vista, Windows 7 color style
  // 2.0.0.1 : Fixed : small issue with size calculation on inserting items
  // 2.0.0.2 : Fixed : small issue with AutoItemHeight = true and height calculation
  // 2.0.1.0 : New : support for customizing bullets in HTML UL lists
  // 2.1.0.0 : New : support for Office 2010 color styles added
  // 2.1.1.0 : New : Attribute Color added for HR tag
  // 2.1.1.1 : Fixed : Issue with DoubleBuffered = true in older Delphi versions
  // 2.2.0.0 : New : Support for PNG images via images in associated PictureContainer
  // 2.2.0.1 : Improved : AutoItemHeight automatically turned off when setting ItemHeight at design-time
  //         : Improved : Vertical alignment handling of images with text
  // 2.2.1.0 : New : Property SelectionHTMLColors added


type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  EHTMListBoxError = class(Exception);

  TAnchorClick = procedure(Sender:TObject;index:integer;anchor:string) of object;

  TAnchorHintEvent = procedure(Sender:TObject; Index: Integer; var Anchor:string) of object;

  TOwnerDrawState = Windows.TOwnerDrawState;
  {$NODEFINE TOwnerDrawState}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMListBox = class(TCustomListBox, ITMSStyle)
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
    FSelectionColors: TGradientStyle;
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
    FLineSpacing: Integer;
    FShowSelection: Boolean;
    FAutoItemHeight: Boolean;
    FShowFocus: Boolean;
    FAutoFocus: Boolean;
    FSelectionHTMLColors: Boolean;
    procedure ReMeasure;
    procedure DoMeasureList;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure SetImages(value : TImageList);
    procedure SetMultiLine(value : boolean);
    procedure SetURLColor(const Value : tColor);
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
    procedure SetSelectionColors(const Value: TGradientStyle);
    procedure SetLineSpacing(const Value: Integer);
    procedure SetAutoItemHeight(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    function GetItemHeight: integer;
    procedure SetItemHeight(const value: integer);
  protected
    { Protected declarations }
    procedure WndProc(var Message: TMessage); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
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
    procedure HilightInList(HiText: string; DoCase: Boolean);
    procedure HilightInItem(Index: Integer; HiText: string; DoCase: Boolean);
    procedure UnHilightInList;
    procedure UnHilightInItem(Index: Integer);
    procedure MarkInList(HiText: string; DoCase: Boolean);
    procedure MarkInItem(Index: Integer; HiText: string; DoCase: Boolean);
    procedure UnMarkInList;
    procedure UnMarkInItem(Index: Integer);
    procedure SetComponentStyle(AStyle: TTMSStyle);
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
    property AutoFocus: Boolean read FAutoFocus write FAutoFocus default False;
    property AutoItemHeight: boolean read FAutoItemHeight write SetAutoItemHeight default True;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property EnableBlink: Boolean read FEnableBlink write SetEnableBlink default False;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property HTMLHint: Boolean read FHTMLHint write FHTMLHint default False;
    property Images:TImageList read FImages write SetImages;
    property IncrLookup:boolean read FIncrLookup write FIncrLookup default False;
    property ImeMode;
    property ImeName;
    property ItemHeight read GetItemHeight write SetItemHeight;
    property ItemHint: Boolean read FItemHint write FItemHint default False;
    property Items;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property MultiSelect;
    property Multiline: Boolean read FMultiLine write SetMultiline default False;
    property ParentCtl3D;
    property ParentColor;
    property ParentShowHint;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property PopupMenu;
    property ScrollHorizontal: Boolean read fScrollHorizontal write SetScrollHorizontal default False;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default True;
    property ShowHint;
    property SelectionColors: TGradientStyle read FSelectionColors write SetSelectionColors;
    property SelectionFontColor: TColor read fSelectionFontColor write SetSelectionFontColor default clHighLightText;
    property SelectionHTMLColors: Boolean read FSelectionHTMLColors write FSelectionHTMLColors default false;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowSelection: Boolean read FShowSelection write FShowSelection default true;
    property Sorted: Boolean read GetSortedEx write SetSortedEx;
    property SortWithHTML: Boolean read FSortWithHTML write FSortWithHTML default false;
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
    property OnContextPopup;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl, ShellApi, Forms;

{$I htmlengo.pas}

procedure THTMListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  a,s,f: string;
  xsize,ysize,ml,hl: Integer;
  urlcol: TColor;
  hrect,hr: TRect;
  pt: TPoint;
  dx: Integer;
  dc: HDC;
  ACanvas: TCanvas;

begin
  dx := 0;

  if FScrollHorizontal then
    dx := GetScrollPos(Handle,SB_HORZ);

  dc := GetDC(handle);

  try
    ACanvas := TCanvas.Create;
    try
      if dx = 0 then
        ACanvas.handle := Canvas.Handle
      else
        ACanvas.Handle := dc;

      ACanvas.Font.Assign(Font);

      hrect := Rect;

      if Index = Self.Items.Count - 1 then
      begin
        if rect.Bottom < Height then
        begin
          rect.bottom := Height;
          ACanvas.Brush.Color := self.Color;
          ACanvas.Pen.Color := self.Color;
          ACanvas.Rectangle(rect.Left,rect.Top,rect.Right,rect.Bottom);
        end;
      end;

      Rect := hrect;

      if (odSelected in State) and (ShowSelection) then
      begin
        Urlcol := FSelectionFontColor;
        ACanvas.Font.Color := FSelectionFontColor;
        SelectionColors.Draw(ACanvas, Rect);
      end
      else
      begin
        ACanvas.Brush.Color := Color;
        ACanvas.Pen.Color := Color;
        ACanvas.Font.Color := Font.Color;
        Urlcol := FURLColor;
        ACanvas.Rectangle(rect.Left,rect.Top,rect.Right,rect.Bottom);
      end;

      if (odFocused in State) and ShowFocus and ShowSelection then
      begin
        if (odSelected in State) then
        begin
          ACanvas.Pen.Style := psClear;
          ACanvas.Brush.Style := bsClear;
        end;

        if SelectionColors.Rounded then
          InflateRect(Rect,-1,-1);

        ACanvas.DrawFocusRect(Rect);

        if SelectionColors.Rounded then
          InflateRect(Rect,+1,+1);
      end;

      GetCursorPos(pt);
      pt := ScreenToClient(pt);

      // Correction for border
      hrect.Left := hrect.Left + 2 - dx;
      pt.X := pt.X + 2;

      if SelectionColors.Rounded then
      begin
        hrect.Top := hrect.Top + 1;
        hrect.Left := hrect.Left + 1;
      end;

      // Unlimited width simulation if horiz. scroll
      if FScrollHorizontal then
        hrect.Right := hrect.Left + 4096;

      //if not index = 0 then
      if (Index <= Items.Count - 1) then
        HTMLDrawEx(ACanvas, Items[Index], hrect, FImages, pt.x, pt.y,-1,-1, FShadowOffset,False,False,False,not SelectionHTMLColors and (odSelected in State) and ShowSelection,
          True,False,not FEllipsis,1.0,urlcol,clNone,clNone,FShadowColor,a,s,f,XSize,YSize,hl,ml,hr,FImageCache,FContainer,FLineSpacing);

    finally
      ACanvas.Free;
    end;
  finally
    ReleaseDC(handle,dc);
  end;
end;

procedure THTMListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FAutoFocus then
    SetFocus;
end;


Procedure THTMListBox.CMHintShow(Var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor: string;
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

procedure THTMListBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  Height := SendMessage(Handle, LB_GETITEMHEIGHT, Index ,0);
end;

constructor THTMListBox.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  Style := lbOwnerDrawVariable;
  FIsMeasuring := False;
  FURLColor := clBlue;
  FSelectionFontColor := clHighLightText;
  FTimerID:=0;
  FEnableBlink := False;
  FShadowColor := clGray;
  FShadowOffset := 1;
  FOldAnchor := '';
  FScrollHorizontal := False;
  FImageCache := THTMLPictureCache.Create;
  FLastHintPos := -1;
//  DoubleBuffered := True;
  FSelectionColors := TGradientStyle.Create;
  FShowSelection := True;
  FAutoItemHeight := True;
  FShowFocus := True;
end;

procedure THTMListBox.Loaded;
begin
  inherited;
  FOldCursor := self.Cursor;
  if FEnableBlink and (FTimerID = 0) then
    FTimerID := SetTimer(self.Handle,1,100,nil);
  if not FEnableBlink and (FTimerID <> 0) then
    KillTimer(self.Handle,FTimerID);

  ReMeasure;
  Width := Width - 1;
  Width := Width + 1;
end;

procedure THTMListBox.SetImages(value:TImagelist);
begin
  FImages := Value;
  ReMeasure;
end;

procedure THTMListBox.SetURLColor(const Value:tColor);
begin
  if Value <> FURLColor then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMListBox.SetSelectionFontColor(const Value: tColor);
begin
  if Value <> fSelectionFontColor then
  begin
    FSelectionFontColor := Value;
    Invalidate;
  end;
end;

procedure THTMListBox.SetMultiLine(value:boolean);
begin
  if Value <> FMultiline then
  begin
    FMultiline := Value;
    ReMeasure;
  end;
end;

procedure THTMListBox.SetAutoItemHeight(const Value: Boolean);
begin
  if (Value <> FAutoItemHeight) then
  begin
    FAutoItemHeight := value;
    Remeasure;
    Invalidate;
  end;
end;

function THTMListBox.GetTextItem(index:integer):string;
begin
  if (index >= 0) and (Index < self.Items.Count) then
  begin
    Result := HTMLStrip(self.Items[index]);
  end
  else
    raise EHTMListBoxError.Create('Item index out of range');
end;

procedure THTMListBox.DoMeasureList;
var
  r,hr: TRect;
  i: Integer;
  MaxX: integer;
  xsize,ysize,ml,hl: Integer;
  a,s,f: string;

begin
  if Items.Count = 0 then
    Exit;

  SendMessage(Handle,LB_GETITEMRECT,0,LParam(@r));

  MaxX := 0;

  if FScrollHorizontal then
    r.Right := r.Left + 4096
  else
  begin
    if items.Count * ItemHeight > Height then
      r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL)
    else
      r.Right := r.Right - 4;
  end;

  Canvas.Font.Assign(Font);

  r.Bottom := r.Top + Height;

  for i := 0 to Items.Count - 1 do
  begin
    if FAutoItemHeight then
    begin
      HTMLDrawEx(Canvas,Items[i],r,FImages,0,0,-1,-1,FShadowOffset,True,True,False,True,True,False,not FEllipsis,1.0,
        FURLColor,clNone,clNone,FShadowColor,a,s,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer,FLineSpacing);

      if YSize > ClientRect.Bottom - ClientRect.Top then
        YSize := ClientRect.Bottom - ClientRect.Top;

      if YSize > 254 then
      begin
        YSize := 254;
      end;

      SendMessage(Handle,LB_SETITEMHEIGHT,i,YSize + 1);

      if XSize + 6 > MaxX then
        MaxX := XSize + 6;
    end
    else
    begin
      SendMessage(Handle,LB_SETITEMHEIGHT,i,ItemHeight);
      MaxX := ItemHeight;
    end;

  end;

  if FScrollHorizontal and (MaxX > FMaxExtent) then
  begin
    FMaxExtent := MaxX;
    UpdateHScrollExtent(FMaxExtent);
  end;
end;

procedure THTMListBox.WndProc(var Message: TMessage);
var
  r,hr: TRect;
  xsize,ysize,ml,hl: Integer;
  a,s,f: string;
begin
  inherited;

  if Message.msg = WM_DESTROY then
  begin
    if FEnableBlink and (FTimerID <> 0) then
      KillTimer(Handle,FTimerID);
  end;

  if (Message.msg = LB_DELETESTRING) or
     (Message.msg = LB_RESETCONTENT) then
  begin
    if FScrollHorizontal and (FUpdateCount = 0) then
      UpdateHScrollExtent(0);
  end;


  if (Message.msg = LB_ADDSTRING) or
     (Message.msg = LB_INSERTSTRING) then
  begin
    if FAutoItemHeight and (ItemHeight > 0) then
    begin
      SendMessage(Handle,LB_GETITEMRECT,Message.Result,LParam(@r));

      if FScrollHorizontal then
        r.Right := r.Left + 4096
      else
      begin
        if items.Count * ItemHeight > Height then
          r.Right := r.Right - GetSystemMetrics(SM_CXVSCROLL)
        else
          r.Right := r.Right - 2;
      end;

      Canvas.Font.Assign(Font);

      r.Bottom := r.Top + Height;

      HTMLDrawEx(Canvas, Items[message.Result], r, FImages, 0, 0, -1, -1, FShadowOffset, True, True, False, True, True, False, not FEllipsis,1.0,
                 FURLColor, clNone, clNone, FShadowColor, a, s, f, xsize, ysize, hl, ml, hr, FImageCache, FContainer, FLineSpacing);

      if YSize > ClientRect.Bottom - ClientRect.Top then
        YSize := ClientRect.Bottom - ClientRect.Top;

      if YSize > 254 then
         YSize := 254;

      SendMessage(Handle,LB_SETITEMHEIGHT,Message.Result,YSize + 1);

      if FScrollHorizontal and (XSize + 6 > FMaxExtent) then
      begin
        FMaxExtent := XSize + 6;
        UpdateHScrollExtent(FMaxExtent);
      end;
    end
    else
    begin
       SendMessage(Handle,LB_SETITEMHEIGHT,Message.Result,ItemHeight);
    end;

  end;

end;

function THTMListBox.MaxHorizontalExtent: Integer;
var
  r,hr: TRect;
  xsize,ysize,ml,hl,i: Integer;
  a,s,f: string;
begin
  FMaxExtent := 0;
  for i := 1 to Items.Count do
  begin

    HTMLDrawEx(Canvas,Items[i-1],r,fImages,0,0,-1,-1,fShadowOffset,true,true,false,true,true,false,not FEllipsis,1.0,
               FURLColor,clNone,clNone,fShadowColor,a,s,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer,0);
    if (XSize + 6 > FMaxExtent) then
      FMaxExtent := XSize + 6;
  end;
  Result := FMaxExtent;
end;

procedure THTMListBox.UpdateHScrollExtent(MaxExtent:Integer);
var
  max,w: Integer;
  r: TRect;
begin
  if FUpdateCount > 0 then
   Exit;

  if (Items.Count <= 0) or (FScrollHorizontal = False) then
  begin
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0 );
    SendMessage(Handle, WM_HSCROLL, SB_TOP, 0 );
    Exit;
  end;

  if MaxExtent > 0 then
    Max := MaxExtent
  else
    Max := MaxHorizontalExtent;

  SendMessage(Handle,LB_GETITEMRECT,0,LParam(@r));

  w := r.Right - r.Left;

  inc(FUpdateCount);
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

  dec(FUpdateCount);
end;

procedure THTMListBox.ReMeasure;
//var
//  i: Integer;
//  sel: Boolean;
begin
  if csLoading in ComponentState then
    Exit;

  FIsMeasuring := True;

  DoMeasureList;
  {
  sel := False;
  for i := 1 to Items.Count do
  begin
    if MultiSelect then
      sel := Selected[i - 1];
    Items[i - 1] := Items[i - 1];
    if sel and MultiSelect then
      Selected[i - 1] := sel;
  end;
  }
  FIsMeasuring := False;
  MaxHorizontalExtent;
end;

function THTMListBox.IsAnchor(x,y:integer;var idx:integer):string;
var
  res: Integer;
  r,hr: TRect;
  anchor,stripped,f: string;
  xsize,ysize,ml,hl: Integer;

begin
  Result := '';
  idx := -1;
  res := loword(SendMessage(self.handle,LB_ITEMFROMPOINT,0,MakeLParam(X,Y)));
  if (res >= 0) and (res < self.Items.Count) then
  begin
    idx := res;
    SendMessage(Handle,LB_GETITEMRECT,Res,LParam(@r));

    if HTMLDrawEx(canvas,self.items[res],r,FImages,X,Y,-1,-1,FShadowOffset,true,false,false,true,true,false,not FEllipsis,1.0,
                  FURLColor,clNone,clNone,fShadowColor,anchor,stripped,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer,0) then

    Result := Anchor;
  end;
end;

procedure THTMListBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
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

procedure THTMListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
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
    if (FOldAnchor <> Anchor) and (FOldAnchor <> '') then
    begin
      Application.Cancelhint;
      if Assigned(FOnAnchorExit) then
        FOnAnchorExit(self,idx,FOldAnchor);
    end;

    if Cursor <> crHandPoint then
    begin
      FOldCursor := Cursor;
      Cursor := crHandPoint;
    end;

    if FOldAnchor <> Anchor then
      if Assigned(FOnAnchorEnter) then
        FOnAnchorEnter(self,idx,Anchor);

    FOldAnchor := Anchor;
  end
  else
  begin
    if Cursor = crHandPoint then
    begin
      //Invalidate;
      if (FOldAnchor <> '') then
      begin
        Application.CancelHint;
        Cursor := FOldCursor;
        if Assigned(FOnAnchorExit) then
          FOnAnchorExit(Self, idx, FOldAnchor);
        FOldAnchor := '';
      end;
    end;
  end;
end;

procedure THTMListBox.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  inherited;
  Exit;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap(DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

procedure THTMListBox.WMSize(var Msg: TWMSize);
begin
  inherited;

  if not FScrollHorizontal then
    if not FIsMeasuring then
      ReMeasure;

  if FScrollHorizontal then
    UpdateHScrollExtent(0);

  Invalidate;
end;

procedure THTMListBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
  inherited;
end;

procedure THTMListBox.WMTimer(var Msg: TWMTimer);
var
  i,i1,i2: Integer;
  r: TRect;
  a,s,f: string;
  xsize,ysize: Integer;
  sel: Boolean;
  hr:trect;
  hl,ml:integer;
  DoAnim: Boolean;
  IsSelect: Boolean;

begin
  if (Items.Count = 0) or not FEnableBlink then
    Exit;

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

  if FTimerCount mod 5 <>0 then
    Exit;

  r := GetClientRect;
  i1 := SendMessage(handle,LB_ITEMFROMPOINT,0,makelparam(0,r.Top));
  i2 := SendMessage(handle,LB_ITEMFROMPOINT,0,makelparam(0,r.Bottom));

  if i1 < 0 then i1 := 0;
  if i2 > Items.Count - 1 then
    i2 := Items.Count - 1;

  for i := i1 to i2 do
  begin
    //only redraw items with blinking
    if Pos('<BLINK',UpperCase(items[i])) > 0 then
    begin
      SendMessage(Handle,LB_GETITEMRECT,i,LParam(@r));

      sel := SendMessage(Handle,LB_GETSEL,i,0) > 0;
      r.left := r.Left + 2;
      if not sel then
      begin
        Canvas.Brush.Color := Color;
        Canvas.Font.Color := Font.Color;
      end
      else
      begin
        Canvas.Brush.Color := FSelectionColors.ColorFrom;
        Canvas.Font.Color := FSelectionFontColor;
      end;

      IsSelect := (ItemIndex = i) or (MultiSelect and Selected[i]);

      HTMLDrawEx(Canvas,Items[i],r,FImages,0,0,-1,-1,FShadowOffset,False,False,False,IsSelect,FBlinking,False,not FEllipsis,1.0,fURLColor,clNone,clNone,fShadowColor,
                 a,s,f,xsize,ysize,hl,ml,hr,FImageCache,FContainer,0);
    end;
  end;
  FBlinking := not FBlinking;
end;

procedure THTMListBox.SetEnableBlink(const Value: boolean);
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

procedure THTMListBox.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  self.Invalidate;
end;

procedure THTMListBox.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  self.Invalidate;
end;

function THTMListBox.GetSortedEx: boolean;
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

procedure THTMListBox.QuickSortList(List:TStringList;left,right:integer);
var
  i,j:integer;
  s,sw: string;
  obj: TObject;

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
          obj := List.Objects[i];
          List.Strings[i] := List.Strings[j];
          List.Objects[i] := List.Objects[j];
          List.Strings[j] := sw;
          List.Objects[j] := obj;
        end;
      end;
      inc(i);
      dec(j);
    end;
  until i > j;

  if Left < j then QuicksortList(List,Left,j);
  if i < Right then QuickSortList(List,i,Right);
end;

procedure THTMListBox.SetSortedEx(const Value: boolean);
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

procedure THTMListBox.KeyDown(var Key: Word; Shift: TShiftState);
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

procedure THTMListBox.DoEnter;
begin
  inherited;
  FLookup := '';
end;

procedure THTMListBox.SetScrollHorizontal(const Value: boolean);
begin
  if FScrollHorizontal <> Value then
  begin
    FScrollHorizontal := Value;
    UpdateHScrollExtent(0);
  end;
end;

procedure THTMListBox.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THTMListBox.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      ReMeasure;
      Invalidate;
      UpdateHScrollExtent(0);
    end;  
  end;
end;

destructor THTMListBox.Destroy;
begin
  FSelectionColors.Free;
  FImageCache.Free;
  inherited;
end;

procedure THTMListBox.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMListBox.SetComponentStyle(AStyle: TTMSStyle);
begin
  SelectionColors.SetStyle(AStyle);
  SelectionFontColor := clHighlightText;
  if not (AStyle in [tsCustom, tsWindowsXP]) then
    SelectionFontColor := clBlack;
end;

procedure THTMListBox.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  ReMeasure;
end;

procedure THTMListBox.HilightInItem(Index: Integer; HiText: string;
  DoCase: Boolean);
begin
  Items[Index] := Hilight(Items[Index],HiText,'hi',DoCase);
end;

procedure THTMListBox.HilightInList(HiText: string; DoCase: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := Hilight(Items[i - 1],HiText,'hi',DoCase);
  EndUpdate;
end;

procedure THTMListBox.MarkInItem(Index: Integer; HiText: string;
  DoCase: Boolean);
begin
  Items[Index] := Hilight(Items[Index],HiText,'e',DoCase);
end;

procedure THTMListBox.MarkInList(HiText: string; DoCase: Boolean);
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := Hilight(Items[i - 1],HiText,'e',DoCase);
  EndUpdate;
end;

procedure THTMListBox.UnHilightInItem(Index: Integer);
begin
  Items[Index] := UnHilight(Items[Index],'hi');
end;

procedure THTMListBox.UnHilightInList;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := UnHilight(Items[i - 1],'hi');
  EndUpdate;
end;

procedure THTMListBox.UnMarkInItem(Index: Integer);
begin
  Items[Index] := UnHilight(Items[Index],'e');
end;

procedure THTMListBox.UnMarkInList;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 1 to Items.Count do
    Items[i - 1] := UnHilight(Items[i - 1],'e');
  EndUpdate;
end;

procedure THTMListBox.SetSelectionColors(const Value: TGradientStyle);
begin
  FSelectionColors.Assign(Value);
end;

procedure THTMListBox.SetLineSpacing(const Value: Integer);
begin
  if (FLineSpacing <> Value) then
  begin
    FLineSpacing := Value;
    Remeasure;
    Invalidate;
  end;
end;

function THTMListBox.GetItemHeight: integer;
begin
  Result := inherited ItemHeight;
end;

procedure THTMListBox.SetItemHeight(const Value: integer);
begin
  inherited ItemHeight := Value;

  if (csDesigning in ComponentState) then
  begin
    if Value <> 16 then
      FAutoItemHeight := false;
  end;

  if not FAutoItemHeight then
    Remeasure;
end;


function THTMListBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMListBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMListBox.SetVersion(const Value: string);
begin

end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
