{*************************************************************************}
{ THTMLCredit component                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2003 - 2013                                      }
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

unit HTMLCredit;

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Shellapi, Extctrls, ComObj, ActiveX,
  PictureContainer, ImgList
  , Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // 1.0.1.0 : Added property Loop
  // 1.0.1.1 : Fixed : issue with continuous scroll
  // 1.1.0.0 : Improved : DFM property persistence
  //         : New : LineSpacing property added
  //         : New : OnAnchorHint event added
  // 1.1.1.0 : New : support for customizing bullets in HTML UL lists
  // 1.1.2.0 : Improved : partial line display at bottom of HTMLCredit
  // 1.1.2.1 : Fixed : Issue with cursor set to crHandpoint
  // 1.2.0.0 : New : Support for PNG images via images in associated PictureContainer
  // 1.2.0.1 : Fixed : Small issue with scrolling to end of single line text
  // 1.2.0.2 : Fixed : Issue with reparenting



type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TRichText = string;

  TVAlignment = (tvaTop,tvaCenter,tvaBottom);

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;

  TAnchorHintEvent = procedure (Sender:TObject; var Anchor:string) of object;

  TAutoSizeType = (asVertical,asHorizontal,asBoth);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLCredit = class(TCustomStaticText)
  private
    { Private declarations }
    FIsCRHand: boolean;
    FLineSpacing: integer;
    FBlinking:boolean;
    FAnchor:string;
    FCurrHoverRect:trect;
    FAutoSizing:boolean;
    FHTMLText:TStrings;
    FAnchorHint:boolean;
    FOnAnchorClick: TAnchorClick;
    FOnAnchorEnter: TAnchorClick;
    FOnAnchorExit: TAnchorClick;
    FOnAnchorKeypress: TAnchorClick;
    FOnAnchorHint: TAnchorHintEvent;
    FImages:TImageList;
    FImageCache:THTMLPictureCache;
    FHover: Boolean;
    FHoverColor: TColor;
    FHoverFontColor: TColor;
    FShadowColor: TColor;
    FShadowOffset: Integer;
    Fupdatecount: Integer;
    FTimerID: Integer;
    FLoop: boolean;
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
    FUpScroll: Boolean;
    FDownScroll: Boolean;
    FMouseDown: Boolean;
    FAutoScroll: Boolean;
    FHTMLWidth: integer;
    FHTMLHeight: integer;
    FScrollStep: Integer;
    FScrollSpeed: Integer;
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
    procedure SetAutoScroll(const Value: Boolean);
    procedure SetScrollSpeed(const Value: Integer);
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
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Doit;
    property Text:string read GetText;
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
    property AutoSizing: Boolean read fAutoSizing write SetAutoSizeP default false;
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
    property Loop: Boolean read FLoop write FLoop default false;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ScrollSpeed: Integer read FScrollSpeed write SetScrollSpeed default 100;
    property ScrollStep: Integer read FScrollStep write FScrollStep default 4;
    property ScrollPosition: Integer read FVOffset write SetVOffset default 0;    
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property VAlignment: TVAlignment read fVAlignment write SetVAlignment default tvaTop;
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
    property OnAnchorEnter: TAnchorClick read FOnAnchorEnter write FOnAnchorEnter;
    property OnAnchorExit: TAnchorClick read FOnAnchorExit write FOnAnchorExit;
    property OnAnchorHint: TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;
    property OnAnchorKeypress: TAnchorClick read FOnAnchorKeypress write FOnAnchorKeypress;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl;

{$I HTMLENGO.PAS}

procedure THTMLCredit.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THTMLCredit.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure THTMLCredit.DoPaint(bkg: Boolean);
var
  R,CR,HR: TRect;
  x,y,hl,fhl,ml: Integer;
  s,Anchor,Stripped,Focusanchor:string;
  TopColor, BottomColor: TColor;
  Canvas: TCanvas;
  pt: TPoint;
  hrgn: THandle;
  bmp: TBitmap;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  bmp := TBitmap.Create;
  bmp.Width := Width;
  bmp.Height := Height;

  Canvas := bmp.Canvas;

  if Assigned(self.Font) then
    Canvas.Font.Assign(self.Font);

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
      Canvas.Pen.Color := clBlack;
          Canvas.Rectangle(r.left,r.top,r.right,r.bottom);
    end;

  end;

  R := GetClientRect;

  Canvas.Rectangle(r.Left,r.Top,r.Right,r.Bottom);

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

  if GetFocus <> self.Handle then
    fhl := -1
  else
    fhl := FFocusHyperlink;

  GetCursorPos(pt);
  pt := ScreenToClient(pt);


  R.Top := R.Top - FVOffset;

  if (FVAlignment = tvaBottom) Then
    R.Bottom := R.Bottom + Canvas.TextHeight('gh');

  FDownScroll := False;
  FUpScroll := False;

  HTMLDrawEx(Canvas,s,r,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,True,True,False,False,FBlinking,FHover,not FEllipsis,1.0,
    FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);

  FHTMLHeight := y;
  FHTMLWidth := x;

  if (FVAlignment in [tvaCenter,tvaBottom]) then
  begin
    HTMLDrawEx(Canvas,s,r,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,True,False,False,False,FBlinking,FHover,not FEllipsis,1.0,
      FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,hl,ml,HR,FImageCache,FContainer,FLineSpacing);

    if y < Height then
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


  SelectClipRgn(Canvas.handle, 0);
  DeleteObject(hrgn);

  FNumHyperlinks := hl;
  FFocusAnchor := FocusAnchor;

  if FAutoSizing then
  begin
    if ((Align = alTop) or (Align = alBottom) or (Align = alNone)) and
       (FAutoSizeType in [asVertical,asBoth]) then
      if y + 6 <> Height then Height := y + 6;
    if ((Align = alLeft) or (Align = alRight) or (Align = alNone)) and
       (FAutoSizeType in [asHorizontal,asBoth]) then
      if x + 6 <> Width then Width := x + 6;
  end;

  Canvas := TCanvas.Create;
  Canvas.Handle := GetDC(self.Handle);
  Canvas.Draw(0,0,bmp);

  ReleaseDC(self.Handle,Canvas.Handle);
  Canvas.Free;

  bmp.Free;
end;

procedure THTMLCredit.WMPaint(var Message: TWMPaint);
{
var
  lpPaint: tagPaintStruct;
}
begin
  inherited;
  {
  BeginPaint(Handle,lpPaint);
  if FUpdateCount > 0 then
    Exit;

  DoPaint(True);

  EndPaint(Handle,lpPaint);
  }
end;

constructor THTMLCredit.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  FAutoSizing := False;
  FHTMLText := TStringList.Create;
  FImageCache := THTMLPictureCache.Create;
  (fHTMLText as TStringList).OnChange := HTMLChanged;
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
  Height := 100;
  FTimerCount := 0;
  DoubleBuffered := True;
  FUpScroll := False;
  FDownScroll := False;
  FBlinking := True;
  FScrollStep := 4;
  FScrollSpeed := 100;
  FVOffset := 0;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    HTMLText.Add('TMS <b>HTML</b> Credit '+ Name);  

end;

destructor THTMLCredit.Destroy;
begin
  FImageCache.Free;
  FHTMLText.Free;
  inherited;
end;

procedure THTMLCredit.HTMLChanged(sender:TObject);
begin
  FHoverHyperLink := -1;
  FFocusHyperlink := -1;  
  Invalidate;
end;

procedure THTMLCredit.SetAutoSizeP(value : boolean);
begin
  FAutoSizing := value;
  Invalidate;
end;

procedure THTMLCredit.SetHTMLText(value:TStrings);
begin
  if Assigned(Value) then
    FHTMLText.Assign(Value);
  Invalidate;
end;

procedure THTMLCredit.SetImages(value:TImagelist);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLCredit.SetLineSpacing(const Value: Integer);
begin
  if  (FLineSpacing <> Value) then
  begin
    FLineSpacing := Value;
    Invalidate;
  end;
end;

procedure THTMLCredit.SetURLColor(Value:TColor);
begin
  if Value <> FURLColor then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMLCredit.SetHover(Value:boolean);
begin
  if Value <> FHover then
  begin
    FHover := Value;
    Invalidate;
  end;
end;

procedure THTMLCredit.Loaded;
begin
  inherited;

  FIsCRHand := (Cursor = crHandPoint);
  Caption := '';

  if (FEnableBlink or FAutoScroll) and (FTimerID = 0) then
    FTimerID := SetTimer(Handle,1,FScrollSpeed,nil);
    
  if not (FEnableBlink or FAutoScroll) and (FTimerID <> 0) then
    KillTimer(self.handle,FTimerID);
end;

function THTMLCredit.IsAnchor(x,y:integer;var HoverRect:TRect):string;
var
  r: TRect;
  XSize, YSize: Integer;
  s: string;
  Anchor,Stripped,FocusAnchor: string;
  Canvas: TCanvas;
  hl: Integer;
begin
  Result := '';
  r := ClientRect;

  r.Bottom := $FFFF;

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
  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,false,FHover,not FEllipsis,1.0,
    clWhite,clNone,clNone,clNone,Anchor,Stripped,FocusAnchor,xsize,ysize,hl,FHoverHyperlink,HoverRect,FImageCache,FContainer,FLineSpacing) then
  begin
    Result := Anchor;
  end
  else
    FHoverHyperLink := -1;

  Releasedc(self.Handle,Canvas.Handle);
  Canvas.Free;
end;

procedure THTMLCredit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Anchor: string;
  hr: TRect;

begin
  Anchor := IsAnchor(x,y,hr);

  if Anchor <> '' then
  begin
    if hr.Left = -1 then
      DoPaint(False);

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
    if (Cursor = crHandPoint) and not (FIsCRHand) then
    begin
      self.Cursor := crDefault;
      FFocusHyperlink := -1;

      if FHover then
      begin
        Invalidaterect(self.Handle,@FCurrHoverRect,True);
        FHoverHyperLink := -1;
        DoPaint(False);
      end;

      if Assigned(FOnAnchorExit) then
        FOnAnchorExit(self,Anchor);
    end;
  end;
end;

procedure THTMLCredit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Anchor:string;
  hr: TRect;
begin
  inherited MouseDown(Button,Shift,X,Y);

  FMouseDown := True;

  {
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
  }
  
  Anchor := IsAnchor(X,Y + FVOffset,hr);

  if Anchor <> '' then
  begin
    if Assigned(FOnAnchorClick) then
      FOnAnchorClick(self,Anchor);

    if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
      ShellExecute(0,'open',pchar(Anchor),nil,nil,SW_NORMAL)
  end;
end;

procedure THTMLCredit.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

procedure THTMLCredit.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Invalidate;
end;

procedure THTMLCredit.SetBevelWidth(Value: TBevelWidth);
begin
  FBevelWidth := Value;
  Invalidate;
end;

procedure THTMLCredit.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Invalidate;
end;

procedure THTMLCredit.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

Procedure THTMLCredit.CMHintShow(Var Msg: TMessage);
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

    if Assigned(FOnAnchorHint) then
      FOnAnchorHint(Self,Anchor);

    if Anchor <> '' then
    begin
      hi^.HintPos := ClientToScreen(hi^.CursorPos);
      hi^.hintpos.y := hi^.hintpos.y-10;
      hi^.hintpos.x := hi^.hintpos.x+10;
      hi^.HintStr := Anchor;
    end;
  end;
  Msg.Result := Ord(Not CanShow);
end;


procedure THTMLCredit.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;


procedure THTMLCredit.CreateWnd;
begin
  inherited;

  if FAutoScroll then
  begin
    AutoScroll := false;
    AutoScroll := true;
  end;
end;

procedure THTMLCredit.WMTimer(var Msg: TWMTimer);
var
  s: string;
  DoAnim: Boolean;
begin
  if not (FEnableBlink or FAutoScroll) then
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
    DoPaint(false);

  if FVOffset  < HTMLHeight then
  begin
    ScrollPosition := ScrollPosition + ScrollStep;
  end
  else
  begin
    if FLoop then
      ScrollPosition := - Height;
  end;

  if not (FTimerCount mod 5 = 0)  then
    Exit;

  s := GetDisplText;

  if Pos('<BLINK',UpperCase(s)) = 0 then
    Exit;

  DoPaint(true);
  FBlinking := not FBlinking;
end;

procedure THTMLCredit.WMSize(var Msg: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure THTMLCredit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLCredit.WMKillFocus(var Msg: TWMKillFocus);
begin
  Invalidate;
end;

procedure THTMLCredit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if FFocusHyperLink < 0 then
    FFocusHyperLink:=0;
  Invalidate;
end;

procedure THTMLCredit.WMKeyDown(var Msg: TWMKeydown);
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

procedure THTMLCredit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if Msg.CharCode in [vk_up,vk_down,vk_left,vk_right] then
    Msg.Result := 1;
end;


procedure THTMLCredit.Keypress(var Key: Char);
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

procedure THTMLCredit.CMMouseLeave(var Message: TMessage);
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

procedure THTMLCredit.CMMouseEnter(var Message: TMessage);
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

procedure THTMLCredit.WndProc(var Message: tMessage);
var
  lpPaint: tagPaintStruct;

begin
  if message.Msg = WM_PAINT then
  begin
    BeginPaint(Handle,lpPaint);
    if FUpdateCount > 0 then
      Exit;

    DoPaint(True);

    EndPaint(Handle,lpPaint);
  end;

  if message.Msg = WM_DESTROY then
  begin
    if (FEnableBlink or FAutoScroll) and (FTimerID<>0) then
      KillTimer(Handle,FTimerID);
  end;
  inherited;
end;

procedure THTMLCredit.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
//  if (csDesigning in ComponentState) then
//    inherited
//  else
  Message.Result := 1;
end;

procedure THTMLCredit.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure THTMLCredit.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure THTMLCredit.SetEnableBlink(const Value: boolean);
begin
  FEnableBlink := Value;

  if not (csLoading in ComponentState) then
  begin
    if (FEnableBlink or FAutoScroll) and (FTimerID = 0) then
      FTimerID := SetTimer(Handle,1,FScrollSpeed,nil);
      
    if not (FEnableBlink or FAutoScroll) and (FTimerID <> 0) then
    begin
      KillTimer(Handle,FTimerID);
      FTimerID := 0;
      Invalidate;
      FBlinking := False;
    end;
  end;

end;

function THTMLCredit.GetText: string;
begin
  Result := HTMLStrip(GetDisplText);
end;

function THTMLCredit.GetDisplText: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FHTMLText.Count - 1 do
  begin
    if i = 0 then
      Result := Result + FHTMLText.Strings[i]
    else
      Result := Result + ' ' + FHTMLText.Strings[i];
  end;
end;


procedure THTMLCredit.SetVAlignment(const Value: TVAlignment);
begin
  if FVAlignment <> Value then
  begin
    FVAlignment := Value;
    Invalidate;
  end;
end;

procedure THTMLCredit.Doit;
begin
  DoPaint(false);
end;

procedure THTMLCredit.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FAutoSizeType := Value;
end;


procedure THTMLCredit.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMLCredit.SetVOffset(const Value: Integer);
begin
  FVOffset := Value;
  Invalidate;
end;

procedure THTMLCredit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  FMouseDown := False;
end;

procedure THTMLCredit.HilightText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text, HiText,'hi',DoCase);
end;

procedure THTMLCredit.MarkText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text,HiText,'e',DoCase);
end;

procedure THTMLCredit.UnHilightText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'hi');
end;

procedure THTMLCredit.UnMarkText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'e');
end;


procedure THTMLCredit.SetAutoScroll(const Value: Boolean);
begin
  FAutoScroll := Value;

  if not (csLoading in ComponentState) then
  begin
    if (FEnableBlink or FAutoScroll) and (FTimerID = 0) then
      FTimerID := SetTimer(self.Handle,1,FScrollSpeed,nil);
    if not (FEnableBlink or FAutoScroll) and (FTimerID <> 0) then
    begin
      KillTimer(self.Handle,FTimerID);
      FTimerID := 0;
    end;
  end;

end;

procedure THTMLCredit.SetScrollSpeed(const Value: Integer);
begin
  FScrollSpeed := Value;

  if not (csLoading in ComponentState) and (FScrollSpeed <> Value) and (Value > 0) then
  begin
    if FTimerID <> 0 then
     KillTimer(self.Handle,FTimerID);
    FTimerID := 0;
    if (FEnableBlink or FAutoScroll) and (FTimerID = 0) then
      FTimerID := SetTimer(self.Handle,1,FScrollSpeed,nil);
  end;
end;

function THTMLCredit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLCredit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLCredit.SetVersion(const Value: string);
begin

end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
