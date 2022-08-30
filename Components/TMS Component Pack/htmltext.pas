{*************************************************************************}
{ THTMLStaticText component                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1999-2014                                        }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published,given or sold in any form as such. No parts of the source     }
{ can be included in any other component or application without           }
{ written authorization of the author.                                    }
{*************************************************************************}

unit HTMLText;

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Shellapi, Extctrls, ComObj, ActiveX, PictureContainer, ImgList
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.2.0.1 : Fixed issue with mouse hovering background color painting
  // v1.2.0.2 : Fixed HTMLHeight property returns full height in all circumstances
  // v1.3.0.0 : New : property LineSpacing added
  //          : New : OnAnchorHint event added
  //          : Improved : property persistence in DFM file
  //          : Improved : OnAnchorClick event triggered before default handling
  // v1.3.0.1 : Fixed : issue with vertical alignment
  // v1.3.0.2 : Fixed : issue with auto sizing
  // v1.3.1.0 : New : support for customizing bullets in HTML UL lists
  // v1.3.1.1 : Fixed : Issue with bsSingle border drawing
  // v1.3.1.2 : Fixed : Issue with reparenting & blinking
  // v1.4.0.0 : New : Support for PNG images via images in associated PictureContainer
  // v1.4.0.1 : Fixed : Issue with VCL styles
  // v1.4.0.2 : Fixed : Issue the HTML rendering for wordwrapped text in design-time editor
  // v1.4.0.3 : Fixed : Issue with font rendering when MiniScroll = true
  // v1.4.1.0 : New : OnAnchorRightClick event added

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}


  TRichText = string;

  TOnNewSize = procedure (Sender:TObject; NewWidth, NewHeight : Integer) of object;

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;

  TAnchorHintEvent = procedure (Sender:TObject; var Anchor:string) of object;

  TVAlignment = (tvaTop,tvaCenter,tvaBottom);

  TAutoSizeType = (asVertical,asHorizontal,asBoth);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLStaticText = class(TCustomStaticText)
  private
    { Private declarations }
    FBlinking:boolean;
    FAnchor:string;
    FCurrHoverRect:trect;
    FAutoSizing:boolean;
    FHTMLText:TStrings;
    FAnchorHint:boolean;
    FOnAnchorClick:TAnchorClick;
    FOnAnchorRightClick: TAnchorClick;
    FOnAnchorEnter:TAnchorClick;
    FOnAnchorExit:TAnchorClick;
    FOnAnchorKeypress:TAnchorClick;
    FOnAnchorHint:TAnchorHintEvent;
    FImages:TImageList;
    FImageCache:THTMLPictureCache;
    FHover: Boolean;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FShadowColor: TColor;
    FShadowOffset: Integer;
    Fupdatecount: Integer;
    FTimerID: Integer;
    FURLColor: TColor;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: TBevelWidth;
    FBorderWidth: TBorderWidth;
    FBorderStyle: TBorderStyle;
    FFocusHyperLink: Integer;
    FHoverHyperLink: Integer;
    FOldHoverHyperlink: Integer;
    FFocusAnchor:string;
    FNumHyperLinks: Integer;
    FEnableBlink: boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FVAlignment: TVAlignment;
    FTimerCount: Integer;
    FAutoSizeType: TAutoSizeType;
    FEllipsis: Boolean;
    FContainer: TPictureContainer;
    FVOffset: Integer;
    FMiniScroll: Boolean;
    FUpScroll: Boolean;
    FDownScroll: Boolean;
    FMouseDown: Boolean;
    FAutoScroll: Boolean;
    FHTMLWidth: integer;
    FHTMLHeight: integer;
    FLineSpacing: integer;
    FOnNewSize: TOnNewSize;
    procedure SetHTMLText(value : TStrings);
    procedure SetImages(value : TImageList);
    procedure SetURLColor(value : TColor);
    procedure SetAutoSizeP(value : boolean);
    procedure HTMLChanged(sender:tObject);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetHover(Value: Boolean);
    function IsAnchor(x,y:integer;var hoverrect:trect):string;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMTimer(var Msg: TWMTimer); message WM_Timer;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMKeyDown(var Msg:TWMKeydown); message wm_keydown;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure DoPaint(bkg: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetEnableBlink(const Value: boolean);
    function GetText: string;

    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetAutoSizeType(const Value: TAutoSizeType);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetVOffset(const Value: Integer);
    procedure SetMiniScroll(const Value: Boolean);
    procedure SetAutoScroll(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetLineSpacing(const Value: Integer);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure WndProc(var Message:tMessage); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyPress(var Key: Char); override;
    function GetDisplText: string; virtual;
    procedure DoAnchorClick(Anchor: string); virtual;
    procedure DoAnchorRightClick(Anchor: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Doit;
    property Text:string read GetText;
    property VOffset: Integer read FVOffset write SetVOffset;
    procedure HilightText(HiText: string; DoCase: Boolean);
    procedure UnHilightText;
    procedure MarkText(HiText: string; DoCase: Boolean);
    procedure UnMarkText;
    property HTMLWidth: integer read FHTMLWidth;
    property HTMLHeight: integer read FHTMLHeight;
  published
    { Published declarations }
    property Align;
    property AutoScroll: Boolean read FAutoScroll write SetAutoScroll default false;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write SetAutoSizeType default asVertical;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property AnchorHint: Boolean read fAnchorHint write FAnchorHint default False;
    property AutoSizing: Boolean read fAutoSizing write SetAutoSizeP default False;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property EnableBlink: Boolean read FEnableBlink write SetEnableBlink default False;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property FocusControl;
    property Font;
    property Hover: Boolean read FHover write SetHover default False;
    property HoverColor:TColor read FHoverColor write FHoverColor default clNone;
    property HoverFontColor:TColor read FHoverFontColor write FHoverFontColor default clNone;
    property Hint;
    property HTMLText: TStrings read FHTMLText write SetHTMLText;
    property Images: TImageList read FImages write SetImages;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property MiniScroll: Boolean read FMiniScroll write SetMiniScroll default False;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment default tvaTop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnAnchorClick: TAnchorClick read FOnAnchorClick write FOnAnchorClick;
    property OnAnchorRightClick: TAnchorClick read FOnAnchorRightClick write FOnAnchorRightClick;
    property OnAnchorEnter: TAnchorClick read FOnAnchorEnter write FOnAnchorEnter;
    property OnAnchorExit: TAnchorClick read FOnAnchorExit write FOnAnchorExit;
    property OnAnchorKeypress: TAnchorClick read FOnAnchorKeypress write FOnAnchorKeypress;
    property OnAnchorHint: TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnNewSize: TOnNewSize read FOnNewSize write FOnNewSize;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl;

{$I HTMLENGO.PAS}

procedure THTMLStaticText.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THTMLStaticText.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure THTMLStaticText.DoPaint(bkg: Boolean);
var
  R,CR,HR: TRect;
  x,y,hl,fhl,ml: Integer;
  s,Anchor,Stripped,Focusanchor:string;
  TopColor, BottomColor: TColor;
  Canvas: TCanvas;
  pt: TPoint;
  hrgn: THandle;
  bmp: tbitmap;
  newsize: boolean;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then
      TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then
      BottomColor := clBtnHighlight;
  end;

begin
  bmp := TBitmap.Create;
  bmp.Width := Width;
  bmp.Height := Height;

  Canvas := bmp.Canvas;

  {
  Canvas := TCanvas.Create;
  Canvas.Handle := GetDC(self.Handle);
  Canvas.Brush.Color := Self.Color;
  }

  if Assigned(Font) then
    Canvas.Font.Assign(Font);

  R := GetClientRect;

  if bkg then
  begin
    if BevelOuter <> bvNone then
    begin
      AdjustColors(BevelOuter);
      Frame3D(Canvas, R, TopColor, BottomColor, BevelWidth);
    end;

    Frame3D(Canvas, R, Color, Color, BorderWidth);

    if BevelInner <> bvNone then
    begin
      AdjustColors(BevelInner);
      Frame3D(Canvas, R, TopColor, BottomColor, BevelWidth);
    end;

    Canvas.Pen.Color := Color;
    Canvas.Pen.Width := 0;
    Canvas.Brush.Color := Color;

    if (FBorderStyle = bsSingle) and (FBorderWidth > 0) then
    begin
      Canvas.Pen.Width := FBorderWidth;
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Rectangle(r.Left - 1,r.Top - 1,r.Right + 1,r.Bottom + 1);
    end;
  end;


  R := GetClientRect;

  if (BevelOuter <> bvNone) or (BevelInner <> bvNone) or
     (BorderStyle <> bsNone) then
    InflateRect(R,-BevelWidth,-BevelWidth);

  if (FBorderStyle <> bsSingle) or (FBorderWidth <= 0) then
    Canvas.Rectangle(r.left,r.top,r.right,r.bottom);

  if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
  begin
    InflateRect(R,-BevelWidth,-BevelWidth);
  end;

  if FBorderStyle = bsSingle then
  begin
    InflateRect(R,-BorderWidth,-BorderWidth);
  end;

  CR := R;

  s := GetDisplText;

  if FAutoSizing then
  begin
    if ((Align=alLeft) or (Align=alRight) or (Align=alNone)) and
       (FAutoSizeType in [asHorizontal,asBoth]) then
      r.Right := r.Right + $FFFF;

    if ((Align=alTop) or (Align=alBottom) or (Align=alNone)) and
       (FAutoSizeType in [asVertical,asBoth]) then
      r.Bottom := r.Bottom + $FFFF;
  end;

  // to ensure HTMLHeight always reflects the full height
  r.Bottom := $FFFF;

  if GetFocus <> self.Handle then
    fhl := -1
  else
    fhl := FFocusHyperlink;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);

  if FMiniScroll then
  begin
    R.Top := R.Top - VOffset;
    R.Right := R.Right - 10;
  end;

  if FMiniScroll then
  begin
    FDownScroll := False;
    FUpScroll := False;

    HTMLDrawEx(Canvas,s,r,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,True,True,False,False,FBlinking,FHover,not FEllipsis,1.0,
      FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);

    if (y >= Height + FVOffset ) then
    begin
      HR := CR;
      HR.Top := CR.Bottom - 10;
      HR.Left := HR.Right - 10;
      DrawFrameControl (Canvas.Handle,HR,DFC_SCROLL,DFCS_SCROLLDOWN or DFCS_FLAT);
      FDownScroll := True;
    end;

    if VOffset > 0 then
    begin
      HR := CR;
      HR.Bottom := CR.Top + 10;
      HR.Left := HR.Right - 10;
      DrawFrameControl (Canvas.Handle,HR,DFC_SCROLL,DFCS_SCROLLUP or DFCS_FLAT);
      FUpScroll := True;
    end;

    Canvas.Font.Assign(Font);
  end;

  if (FVAlignment in [tvaCenter,tvaBottom]) then
  begin
    r := ClientRect;
    
    HTMLDrawEx(Canvas,s,r,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,True,False,False,False,FBlinking,FHover,not FEllipsis,1.0,
      FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);

    if (y < Height) then
    case FVAlignment of
    tvaCenter:r.top := r.top + ((r.bottom - r.top - y) shr 1);
    tvaBottom:r.top := r.bottom - y;
    end;
  end;

  hrgn := CreateRectRgn(r.left, r.top, r.right, r.bottom);
  SelectClipRgn(Canvas.Handle, hrgn);

  if not Enabled then
  begin
    OffsetRect(r,1,1);
    Canvas.Font.Color := clWhite;
    HTMLDrawEx(Canvas,s,r,nil,0,0,fhl,FHoverHyperlink,FShadowOffset,False,False,false,False,FBlinking,FHover,not FEllipsis,1.0,
      clWhite,clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);
    Canvas.Font.Color := clGray;

    OffsetRect(r,-1,-1);
    HTMLDrawEx(Canvas,s,r,FImages,0,0,fhl,FHoverHyperlink,FShadowOffset,False,False,false,False,FBlinking,FHover,not FEllipsis,1.0,
      clGray,clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);
   end
  else
   HTMLDrawEx(Canvas,s,R,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,False,False,false,False,FBlinking,FHover,not FEllipsis,1.0,
     FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);

  FHTMLHeight := y + 2;
  FHTMLWidth := x;

  SelectClipRgn(Canvas.handle, 0);
  DeleteObject(hrgn);

  FNumHyperlinks := hl;
  FFocusAnchor := FocusAnchor;

  newsize := false;

  if FAutoSizing then
  begin
    if ((Align = alTop) or (Align = alBottom) or (Align = alNone)) and
       (FAutoSizeType in [asVertical,asBoth]) then
      if y + 6 <> Height then
      begin
        Height := y + 6;
        newsize := true;
      end;

    if ((Align = alLeft) or (Align = alRight) or (Align = alNone)) and
       (FAutoSizeType in [asHorizontal,asBoth]) then
      if x + 6 <> Width then
      begin
        Width := x + 6;
        newsize := true;
      end;
  end;



  Canvas := TCanvas.Create;
  Canvas.Handle := GetDC(self.Handle);
  //Canvas.Brush.Color := Self.Color;
  Canvas.Draw(0,0,bmp);


  ReleaseDC(self.Handle,Canvas.Handle);
  Canvas.Free;

  bmp.Free;

  if FAutoSizing and Assigned(FOnNewSize) and newsize then
    FOnNewSize(self, Width, Height);
end;

procedure THTMLStaticText.WMPaint(var Message: TWMPaint);
var
  lpPaint: tagPaintStruct;
begin
  BeginPaint(Handle,lpPaint);
  if FUpdateCount > 0 then
    Exit;

  DoPaint(True);
  EndPaint(Handle,lpPaint);
end;

constructor THTMLStaticText.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  FAutoSizing := False;
  FHTMLText := TStringList.Create;
  FImageCache := THTMLPictureCache.Create;
  (FHTMLText as TStringList).OnChange := HTMLChanged;
  Caption := '';
  AutoSize := False;
  FUpdateCount := 0;
  FURLColor := clBlue;
  BevelWidth := 1;
  FBorderStyle := bsNone;
  FHoverHyperLink := -1;
  FFocusHyperlink := -1;
  FHoverColor := clNone;
  FHoverFontColor := clNone;
  FShadowColor := clGray;
  FShadowOffset := 1;
  FTimerID := 0;
  FEnableBlink := False;
  Width := 150;
  Height := 20;
  FTimerCount := 0;
  DoubleBuffered := True;
  FMiniScroll := False;
  FUpScroll := False;
  FDownScroll := False;
  FBlinking := True;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    HTMLText.Add('TMS <b>HTML</b> StaticText');

  {$IFDEF DELPHIXE3_LVL}
  StyleElements  := [];
  {$ENDIF}
end;

destructor THTMLStaticText.Destroy;
begin
  FImageCache.ClearPictures;
  FImageCache.Free;
  FHTMLText.Free;
  inherited;
end;

procedure THTMLStaticText.HTMLChanged(sender:TObject);
begin
  FHoverHyperLink := -1;
  FFocusHyperlink := -1;
  Invalidate;
end;

procedure THTMLStaticText.SetAutoSizeP(value : boolean);
begin
  FAutoSizing := value;
  Invalidate;
end;

procedure THTMLStaticText.SetHTMLText(Value: TStrings);
begin
  if Assigned(Value) then
    FHTMLText.Assign(Value);
    
  Invalidate;
end;

procedure THTMLStaticText.SetImages(value:TImagelist);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetLineSpacing(const Value: Integer);
begin
  if (FLineSpacing <> Value) then
  begin
    FLineSpacing := Value;
    Invalidate;
  end;
end;

procedure THTMLStaticText.SetURLColor(Value:TColor);
begin
  if (Value <> FURLColor) then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMLStaticText.SetHover(Value:boolean);
begin
  if Value <> FHover then
  begin
    FHover := Value;
    Invalidate;
  end;
end;

procedure THTMLStaticText.Loaded;
begin
  inherited;
  Caption := '';

  if (FEnableBlink or FAutoScroll or MiniScroll) and (FTimerID = 0) then
    FTimerID := SetTimer(self.Handle,1,100,nil);
  if not (FEnableBlink or FAutoScroll or MiniScroll) and (FTimerID <> 0) then
  begin
    KillTimer(self.handle,FTimerID);
    FTimerID := 0;
  end;
end;

function THTMLStaticText.IsAnchor(x,y:integer;var HoverRect:TRect):string;
var
  r,hr: TRect;
  XSize, YSize: Integer;
  s: string;
  Anchor,Stripped,FocusAnchor: string;
  Canvas: TCanvas;
  hl,ml: Integer;
  pt: TPoint;

begin
  Result := '';
  r := ClientRect;

  if FMiniScroll then
  begin
    r.Top := r.Top - VOffset;
    r.Right := r.Right - 10;
  end;

  HoverRect := Rect(-1,-1,-1,-1);

  if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
    Inflaterect(r,-BevelWidth,-BevelWidth);

  if FBorderStyle = bsSingle then
    Inflaterect(r,-BorderWidth,-BorderWidth);

  s := GetDisplText;

  Anchor := '';

  Canvas := TCanvas.Create;
  Canvas.Handle := GetDC(self.Handle);

  if Assigned(self.Font) then
    Canvas.Font.Assign(self.Font);

  if (FVAlignment in [tvaCenter,tvaBottom]) then
  begin
    HTMLDrawEx(Canvas,s,r,FImages,pt.x,pt.y,-1,-1,FShadowOffset,True,False,False,False,False,FHover,not FEllipsis,1.0,
      FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,xsize,ysize,hl,ml,HR,FImageCache,FContainer,FLineSpacing);

    if ysize < Height then
    case FVAlignment of
    tvaCenter:r.top := r.top + ((r.bottom - r.top - ysize) shr 1);
    tvaBottom:r.top := r.bottom - ysize;
    end;
  end;


  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,FHover,not FEllipsis,1.0,
    clWhite,clNone,clNone,clNone,Anchor,Stripped,FocusAnchor,xsize,ysize,hl,FHoverHyperlink,HoverRect,FImageCache,FContainer,FLineSpacing) then
  begin
    Result := Anchor;
  end
  else
    FHoverHyperLink := -1;

  Releasedc(self.Handle,Canvas.Handle);
  Canvas.Free;
end;

procedure THTMLStaticText.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
  hr: TRect;

begin
  Anchor := IsAnchor(x,y,hr);

  if Anchor <> '' then
  begin
    if hr.Left = -1 then
      DoPaint(True);

    FFocusHyperlink := -1;

    if (FAnchor <> Anchor) or not EqualRect(FCurrHoverRect,hr) or
       (FHoverHyperlink = -1) or (FOldHoverHyperLink <> FHoverHyperLink) then
    begin
      if FHover then
        Invalidaterect(self.Handle,@FCurrHoverRect,True);
    end;

    if ((Cursor = crDefault) or (Anchor <> FAnchor)) then
    begin
      FAnchor := Anchor;
      if FAnchorHint then
        Application.CancelHint;

      self.Cursor := crHandPoint;

      if Assigned(FOnAnchorEnter) then
        FOnAnchorEnter(self,Anchor);

      if FHover and (hr.Left <> -1) then
        Invalidaterect(self.Handle,@hr,False);

      FOldHoverHyperLink := FHoverHyperLink;
      FCurrHoverRect := hr;
    end;
  end
  else
  begin
    if Cursor = crHandPoint then
    begin
      self.Cursor := crDefault;
      FFocusHyperlink := -1;

      if FHover then
      begin
        Invalidaterect(self.Handle,@FCurrHoverRect,True);
        FHoverHyperLink := -1;
       // DoPaint(False);
      end;

      if Assigned(FOnAnchorExit) then
        FOnAnchorExit(self,Anchor);
    end;
  end;
  inherited;
end;

procedure THTMLStaticText.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Anchor:string;
  hr: TRect;
begin
  inherited MouseDown(Button,Shift,X,Y);

  FMouseDown := True;

  if FMiniScroll and (FUpScroll or FDownScroll) then
  begin
    HR := GetClientRect;
    if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
    begin
      InflateRect(HR,-BevelWidth,-BevelWidth);
    end;

    if FBorderStyle = bsSingle then
    begin
      InflateRect(HR,-BorderWidth,-BorderWidth);
    end;

    if FUpScroll then
    begin
      if (X > HR.Right - 10) and (Y < HR.Top + 10) then
         VOffset := VOffset - 4;
    end;

    if FDownScroll then
    begin
      if (X > HR.Right - 10) and (Y > HR.Bottom - 10) then
         VOffset := VOffset + 4;
    end;
  end;

  Anchor := IsAnchor(X,Y,hr);

  if Anchor <> '' then
  begin
    if Button = mbLeft then
    begin
      DoAnchorClick(Anchor);
      if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
        ShellExecute(0,'open',pchar(Anchor),nil,nil,SW_NORMAL)
    end;

    if Button = mbRight then
      DoAnchorRightClick(Anchor);
  end;
end;

procedure THTMLStaticText.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetBevelWidth(Value: TBevelWidth);
begin
  FBevelWidth := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

Procedure THTMLStaticText.CMHintShow(Var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor: string;
  hr: TRect;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  if FAnchorHint then
  begin
    Anchor := IsAnchor(hi^.cursorPos.x,hi^.cursorpos.y,hr);

    if Anchor <> '' then
    begin
      if Assigned(FOnAnchorHint) then
        FOnAnchorHint(Self,Anchor);

      hi^.HintPos := ClientToScreen(hi^.CursorPos);
      hi^.hintpos.y := hi^.hintpos.y - 10;
      hi^.hintpos.x := hi^.hintpos.x + 10;
      hi^.HintStr := Anchor;
    end;
  end;
  Msg.Result := Ord(Not CanShow);
end;


procedure THTMLStaticText.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;


procedure THTMLStaticText.WMTimer(var Msg: TWMTimer);
var
  s: string;
  HR: TRect;
  pt: TPoint;
  DoAnim: Boolean;
begin
  if not (FEnableBlink or FAutoScroll or MiniScroll) then
    Exit;

  Inc(FTimerCount);

  DoAnim := False;

  if Assigned(FImageCache) then
    if FImageCache.Animate then
      DoAnim := True;
  if Assigned(FContainer) then
    if FContainer.Items.Animate then
      DoAnim := True;

  if DoAnim then
  begin
    DoPaint(false);
  end;

  if FMiniScroll and (FUpScroll or FDownScroll) and (FMouseDown or FAutoScroll or FMiniScroll) then
  begin
    if not AutoScroll and not FMouseDown then
      Exit;

    GetCursorPos(pt);
    pt := ScreenToClient(pt);

    HR := GetClientRect;

    if PtInRect(HR,pt) then
    begin

      if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
      begin
        InflateRect(HR,-BevelWidth,-BevelWidth);
      end;

      if FBorderStyle = bsSingle then
      begin
        InflateRect(HR,-BorderWidth,-BorderWidth);
      end;

      if FUpScroll then
      begin
        if (pt.X > HR.Right - 10) and (pt.Y < HR.Top + 10) then
           VOffset := VOffset - 4;
      end;

      if FDownScroll then
      begin
        if (pt.X > HR.Right - 10) and (pt.Y > HR.Bottom - 10) then
           VOffset := VOffset + 4;
      end;
    end;
  end;

  if not (FTimerCount mod 5 = 0)  then
    Exit;

  s := GetDisplText;

  if Pos('<BLINK',UpperCase(s)) = 0 then
    Exit;

  DoPaint(true);
  FBlinking := not FBlinking;
end;

procedure THTMLStaticText.WMSize(var Msg: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure THTMLStaticText.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLStaticText.WMKillFocus(var Msg: TWMKillFocus);
begin
  Invalidate;
end;

procedure THTMLStaticText.WMSetFocus(var Msg: TWMSetFocus);
begin
  if FFocusHyperLink < 0 then
    FFocusHyperLink:=0;
  Invalidate;
end;

procedure THTMLStaticText.WMKeyDown(var Msg: TWMKeydown);
begin
  if msg.CharCode in [vk_up,vk_left] then
  begin
     DoPaint(False);
     if FFocusHyperLink > 0 then
       Dec(FFocusHyperlink)
     else
       FFocusHyperlink := FNumHyperlinks - 1;
     Msg.CharCode := 0;
     DoPaint(False);
  end;

  if Msg.CharCode in [vk_down,vk_right] then
  begin
     DoPaint(False);
     if FFocusHyperLink < FNumHyperLinks - 1 then
       Inc(FFocusHyperlink)
     else
       FFocusHyperlink := 0;
     Msg.CharCode := 0;
     DoPaint(False);
  end;
  inherited;
end;

procedure THTMLStaticText.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if Msg.CharCode in [vk_up,vk_down,vk_left,vk_right] then
    Msg.Result := 1;
end;


procedure THTMLStaticText.Keypress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
  begin
    if Assigned(FOnAnchorKeypress) then
      FOnAnchorKeypress(self,fFocusAnchor);

    if (Pos('://',FFocusAnchor) > 0) or (Pos('mailto:',FFocusAnchor) > 0) then
      ShellExecute(0,'open',pchar(FFocusAnchor),nil,nil,SW_NORMAL)
  end;
end;

procedure THTMLStaticText.CMMouseLeave(var Message: TMessage);
begin
  if FHoverHyperlink >= 0 then
  begin
    FHoverHyperlink := -1;
    if FHover then
      InvalidateRect(self.Handle,@FCurrHoverRect,True);
  end;
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

procedure THTMLStaticText.CMMouseEnter(var Message: TMessage);
var
  pt: TPoint;
  hr: TRect;
  Anchor: string;
begin

  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  Anchor := IsAnchor(pt.x,pt.y,hr);

  if ((self.Cursor = crDefault) or (Anchor <> FAnchor)) and (Anchor <> '') then
  begin
    FAnchor := Anchor;
    if FAnchorHint then
      Application.CancelHint;

    self.Cursor := crHandPoint;
    if Assigned(FOnAnchorEnter) then
      FOnAnchorEnter(self,anchor);
    {$IFDEF TMSDEBUG}
     outputdebugstring(pchar('in anchor rect for '+anchor+'= ['+inttostr(hr.left)+':'+inttostr(hr.top)+'] ['+inttostr(hr.right)+':'+inttostr(hr.bottom)+']'));
   {$ENDIF}
    if FHover then
      DoPaint(False);
    //InvalidateRect(self.Handle,@hr,false);
    FCurrHoverRect := hr;
  end;
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure THTMLStaticText.WndProc(var Message: tMessage);
begin
  if message.Msg = WM_DESTROY then
  begin
    if (FEnableBlink or FAutoScroll) and (FTimerID<>0) then
    begin
      KillTimer(Handle,FTimerID);
      FTimerID := 0;
    end;
  end;
  if message.Msg = WM_CREATE then
  begin
    if not (csDesigning in ComponentState) and (FEnableBlink or FAutoScroll or MiniScroll) and (FTimerID = 0) then
      FTimerID := SetTimer(Handle,1,100,nil);
  end;
  inherited;
end;

procedure THTMLStaticText.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
//  if (csDesigning in ComponentState) then
//    inherited
//  else
  Message.Result := 1;
end;

procedure THTMLStaticText.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure THTMLStaticText.SetEnableBlink(const Value: boolean);
begin
  FEnableBlink := Value;

  if not (csLoading in ComponentState) then
  begin
    if (FEnableBlink or FAutoScroll) and (FTimerID = 0) then
      FTimerID := SetTimer(Handle,1,100,nil);

    if not (FEnableBlink or FAutoScroll) and (FTimerID <> 0) then
    begin
      KillTimer(Handle,FTimerID);
      FTimerID := 0;
    end;

    Invalidate;
    FBlinking := False;
  end;
end;

function THTMLStaticText.GetText: string;
begin
  Result := HTMLStrip(GetDisplText);
end;

function THTMLStaticText.GetDisplText: string;
var
  i: Integer;
begin
  Result := '';

  for i := 1 to FHTMLText.Count do
    Result := Result + FHTMLText.Strings[i - 1];

  {
  for i := 1 to FHTMLText.Count do
  begin
    if Result = '' then
      Result := FHTMLText.Strings[i - 1]
    else
    begin
      if pos('<BR>',Uppercase(FHTMLText.Strings[i - 2])) = Length(FHTMLText.Strings[i - 2]) - 3 then
        Result := Result + FHTMLText.Strings[i - 1]
      else
        Result := Result + FHTMLText.Strings[i - 1];
    end;
  end;
  }
end;


procedure THTMLStaticText.SetVAlignment(const Value: TVAlignment);
begin
  if FVAlignment <> Value then
  begin
    FVAlignment := Value;
    if FVAlignment <> tvaTop then
      FMiniScroll := False;
    Invalidate;
  end;
end;

procedure THTMLStaticText.DoAnchorClick(Anchor: string);
begin
  if Assigned(FOnAnchorClick) then
    FOnAnchorClick(self,Anchor);
end;

procedure THTMLStaticText.DoAnchorRightClick(Anchor: string);
begin
  if Assigned(FOnAnchorRightClick) then
    FOnAnchorRightClick(self,Anchor);
end;

procedure THTMLStaticText.Doit;
begin
  DoPaint(false);
end;

procedure THTMLStaticText.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FAutoSizeType := Value;
end;


procedure THTMLStaticText.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMLStaticText.SetVOffset(const Value: Integer);
begin
  if not FMiniScroll then Exit;
  if csDesigning in ComponentState then Exit;

  FVOffset := Value;
  if FVOffset < 0 then
    FVOffset := 0
  else
    Invalidate;
end;

procedure THTMLStaticText.SetMiniScroll(const Value: Boolean);
begin
  if FMiniScroll <> Value then
  begin
    if not Value then
      FVOffset := 0;
    if Value then
      FVAlignment := tvaTop;

    FMiniScroll := Value;
    Invalidate;
  end;
end;

procedure THTMLStaticText.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseDown := False;
end;

procedure THTMLStaticText.HilightText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text, HiText,'hi',DoCase);
end;

procedure THTMLStaticText.MarkText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text,HiText,'e',DoCase);
end;

procedure THTMLStaticText.UnHilightText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'hi');
end;

procedure THTMLStaticText.UnMarkText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'e');
end;


procedure THTMLStaticText.SetAutoScroll(const Value: Boolean);
begin
  FAutoScroll := Value;

  if not (csLoading in ComponentState) then
  begin
    if (FEnableBlink or FAutoScroll) and (FTimerID = 0) then
      FTimerID := SetTimer(self.Handle,1,100,nil);
    if not (FEnableBlink or FAutoScroll) and (FTimerID <> 0) then
    begin
      KillTimer(self.Handle,FTimerID);
      FTimerID := 0;
    end;
  end;

end;

function THTMLStaticText.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLStaticText.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLStaticText.SetVersion(const Value: string);
begin

end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
