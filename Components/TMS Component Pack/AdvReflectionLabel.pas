{**************************************************************************}
{ TAdvReflectionLabel component                                            }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2007 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Website : http://www.tmssoftware.com/                         }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvReflectionLabel;

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ComObj, ActiveX, PictureContainer, AdvGDIP, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with reflection painting for centered HTML text
  // v1.0.0.2 : Fixed : issue with width of reflection
  // v1.0.0.3 : Fixed : issue with HTML text right align

  // v1.5.0.0 : New : Property ReflectionSize added
  //          : New : Method SaveToFile added
  //          : New : Properties to control start & end reflection opacity

  // v1.5.1.0 : New : Support for customizing bullets in HTML UL lists
  // v1.5.1.1 : Fixed : Issue with HTML rendering issue on <P> tags



type
  TImageType = (itPNG, itBMP, itJPEG, itTIFF, itGIF);

  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}
  
  TRichText = string;

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;

  TAnchorHintEvent = procedure (Sender:TObject; var Anchor:string) of object;

  TAutoSizeType = (asVertical,asHorizontal,asBoth);

  TGradientType = (gtFullHorizontal, gtFullVertical, gtBottomLine, gtCenterLine, gtTopLine);

  TVAlignment = (tvaTop,tvaCenter,tvaBottom);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvReflectionLabel = class(TCustomLabel)
  private
    { Private declarations }
    FAnchor: string;
    FAutoSizing: Boolean;
    FAutoSizeType: TAutoSizeType;
    FHTMLText: TStringList;
    FAnchorClick: TAnchorClick;
    FAnchorHint: Boolean;
    FAnchorEnter: TAnchorClick;
    FAnchorExit: TAnchorClick;
    FImages: TImageList;
    FImageCache: THTMLPictureCache;
    FUpdateCount: Integer;
    FURLColor: TColor;
    FBevelInner: TPanelBevel;
    FBevelOuter: TPanelBevel;
    FBevelWidth: TBevelWidth;
    FBorderWidth: TBorderWidth;
    FBorderStyle: TBorderStyle;
    FShadowOffset: integer;
    FShadowColor: TColor;
    FHover:boolean;
    FHoverHyperLink: Integer;
    FOldHoverHyperLink: Integer;
    FHoverColor:TColor;
    FHoverFontColor:TColor;
    FVAlignment: TVAlignment;
    FEllipsis: Boolean;
    FCurrHoverRect: TRect;
    FContainer: TPictureContainer;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnAnchorHint: TAnchorHintEvent;
    FGradientType: TGradientType;
    FBorderColor: TColor;
    FLineWidth: Integer;
    FReflectionAxis: Integer;
    FHTMLHint: Boolean;
    FColorTo: TColor;
    FHintShowFull: Boolean;
    FReflectionOpacityStart: Integer;
    FReflectionOpacityEnd: integer;
    FILChangeLink: TChangeLink;
    FXSize: Integer;
    FYSize: Integer;
    FXStart: Integer;
    FReflectionPic: TGPBitmap;
    Fx, FY: Integer;
    FUpdateReflection: Boolean;
    FReflectionSize: Integer;

    procedure SetHTMLText(value : TStringList);
    procedure SetImages(value : TImageList);
    procedure SetURLColor(value : TColor);
    procedure SetAutoSizing(value : boolean);
    procedure HTMLChanged(sender:tObject);
    procedure ImageListChanged(sender:TObject);
    procedure SetBevelInner(Value: TPanelBevel);
    procedure SetBevelOuter(Value: TPanelBevel);
    procedure SetBevelWidth(Value: TBevelWidth);
    procedure SetBorderWidth(Value: TBorderWidth);
    procedure SetBorderStyle(Value: TBorderStyle);
    function IsAnchor(x,y: Integer;var hoverrect:trect):string;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(Var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetHover(const Value: boolean);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverFontColor(const Value: TColor);
    procedure HoverInvalidate(r:trect);
    function GetText: string;
    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetAutoSizeType(const Value: TAutoSizeType);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetGradientType(const Value: TGradientType);
    procedure SetLineWidth(const Value: Integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetSize: TSize;
    procedure SetReflection(Value: Integer);
    procedure SetReflectionAxis(Value: Integer);
    procedure SetReflectionOpacityEnd(const Value: Integer);
    procedure SetReflectionOpacityStart(const Value: Integer);
  protected
    function GetVersionNr: Integer; virtual;
    { Protected declarations }
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetDisplText:string; virtual;
    procedure UpdateDisplText; virtual;
    function HTMLPaint(Canvas:TCanvas;s:string;fr:TRect;
                       FImages:TImageList;
                       xpos,ypos,focuslink,hoverlink,shadowoffset: Integer;
                       checkhotspot,checkheight,print,selected,blink,hoverstyle:boolean;
                       resfactor:double;
                       urlcolor,hovercolor,hoverfontColor,shadowcolor:TColor;
                       var anchorval,stripval,focusanchor:string;
                       var xsize,ysize,hyperlinks,mouselink: Integer;
                       var hoverrect:TRect):boolean; virtual;
    procedure UpdateReflection;
    procedure PaintToBitmap(bmp: TBitmap);
    procedure PaintText(ACanvas: TCanvas);
    procedure SetName(const Value: TComponentName); override;

    property ColorTo: TColor read FColorTo write SetColor;
    property AnchorHint: Boolean read FAnchorHint write FAnchorHint;
    property AutoSizing: Boolean read FAutoSizing write SetAutoSizing;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write SetAutoSizeType;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property FocusControl;
    property GradientType: TGradientType read FGradientType write SetGradientType;
    property HintShowFull: Boolean read FHintShowFull write FHintShowFull;
    property Hover:boolean read fHover write SetHover;
    property HoverColor:TColor read fHoverColor write SetHoverColor;
    property HoverFontColor:TColor read FHoverFontColor write SetHoverFontColor;
    property HTMLHint: Boolean read FHTMLHint write FHTMLHint;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
    property ShadowColor:TColor read fShadowColor write SetShadowColor;
    property ShadowOffset: Integer read fShadowOffset write SetShadowOffset;
    property VAlignment:TVAlignment read fVAlignment write SetVAlignment;
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property OnAnchorHint: TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;

    procedure Doit;
    property ImageCache: THTMLPictureCache read FImageCache;
    procedure HTMLPrint(Canvas:TCanvas;r:TRect);
    procedure HilightText(HiText: string; DoCase: Boolean);
    procedure UnHilightText;
    procedure MarkText(HiText: string; DoCase: Boolean);
    procedure UnMarkText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Resize; override;
    property Text:string read GetText;
    procedure UpdateChange;
    procedure SaveToFile(FileName: string; ImageType: TImageType = itPng);
    property HTMLSize: TSize read GetSize;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property HTMLText: TStringList read FHTMLText write SetHTMLText;
    property Images: TImageList read FImages write SetImages;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ReflectionOpacityStart: Integer read FReflectionOpacityStart write SetReflectionOpacityStart default 128;
    property ReflectionOpacityEnd: Integer read FReflectionOpacityEnd write SetReflectionOpacityEnd default 0;
    property ReflectionSize: Integer read FReflectionSize write SetReflection default 100;
    property ReflectionAxis: Integer read FReflectionAxis write SetReflectionAxis default -7;
    property ShowHint;
    property Transparent default True;
    property URLColor:TColor read fURLColor write SetURLColor;
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
    property OnMouseLeave: TNotifyEvent read fOnMouseLeave write fOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl, ShellApi;

{$I HTMLENGO.PAS}

//------------------------------------------------------------------------------

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function RemoveSpace(s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    if (s[i] <> ' ')  then
    begin
      Result := Result + s[i];
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.BeginUpdate;
begin
  inc(FUpdateCount);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.Paint;
var
  r,mr:trect;
  x,y,hyperlinks,mouselink: Integer;
  s,anchor,stripped,focusanchor:string;
  TopColor, BottomColor: TColor;
  pt:TPoint;
  gsteps,bw1,bw2: Integer;
  graphics : TGPGraphics;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Caption := '';

  R := GetClientRect;

  gsteps := (R.Right - R.Left) div 4;

  if not Transparent then
  begin
    if (ColorTo <> clNone) and (GradientType in [gtFullHorizontal,gtFullVertical]) then
    begin
      DrawGradient(Canvas,Color,ColorTo,gsteps,R,GradientType = gtFullHorizontal);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
    end;
  end;

  case GradientType of
  gtBottomLine: R.Top := R.Bottom - 2;
  gtTopLine: R.Bottom := R.Top + 2;
  gtCenterLine:
    begin
      R.Top := R.Top + (R.Bottom - R.Top) div 2;
      R.Bottom := R.Top + 2;
    end;
  end;

  if GradientType in [gtBottomLine, gtTopLine, gtCenterLine] then
  begin
    DrawGradient(Canvas,Color,ColorTo,gsteps,R,true);
  end;

  //inherited Paint;
  Canvas.Font.Assign(Font);

  if FUpdateCount > 0 then
    Exit;

  R := GetClientRect;

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

  if (FBorderStyle = bsSingle) and (FBorderWidth > 0) and (FBorderColor <> clNone) then
  begin
    Canvas.Pen.Width := FBorderWidth;
    Canvas.Pen.Color := FBorderColor;
    Canvas.Brush.Color := clBlack;
    Canvas.Brush.Style := bsClear;


    bw1 := Trunc((FBorderWidth + 1) / 2);
    bw2 := Trunc(FBorderWidth / 2);

    Canvas.MoveTo(r.Left - bw1, r.Top - bw1);
    Canvas.LineTo(r.Right + bw2, r.Top - bw1);
    Canvas.LineTo(r.Right + bw2, r.Bottom + bw2);
    Canvas.LineTo(r.Left - bw1, r.Bottom + bw2);
    Canvas.LineTo(r.Left - bw1, r.Top - bw1);
  end;

  if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
  begin
    InflateRect(r,-BevelWidth,-BevelWidth);
  end;

  if (FBorderStyle = bsSingle) then
  begin
    InflateRect(r,-BorderWidth,-BorderWidth);
  end;

  s := GetDisplText;

  Canvas.Brush.Color := self.Color;

  if FAutoSizing then
  begin
    if ((Align = alLeft) or (Align = alRight) or (Align = alNone)) and
       (FAutoSizeType in [asHorizontal,asBoth]) then
      r.Right := r.Right + $FFFF;

    if ((Align = alTop) or (Align = alBottom) or (Align = alNone)) and
       (FAutoSizeType in [asVertical,asBoth]) then
      r.Bottom := r.Bottom + $FFFF;
  end;

  if Assigned(FReflectionPic) and not FUpdateReflection then
  begin
    x := FXStart;
    y := FYSize;
    graphics := TGPgraphics.Create(Canvas.Handle);
    graphics.DrawImageRect(FReflectionPic, x, y + FReflectionAxis, FReflectionPic.GetWidth, FReflectionPic.GetHeight);
    graphics.Free;
  end;

  GetCursorPos(pt);
  pt := self.ScreenToClient(pt);

  if FVAlignment in [tvaCenter,tvaBottom] then
  begin
    HTMLPaint(Canvas,s,r,FImages,pt.x,pt.y,-1,FHoverHyperLink,FShadowOffset,True,False,False,False,False,FHover,1.0,fURLcolor,FHoverColor,FHoverFontColor,FShadowColor,anchor,stripped,focusanchor,x,y,hyperlinks,mouselink,mr);
    if y < Height then
    case FVAlignment of
    tvaCenter:r.Top := r.Top+((r.Bottom - r.Top - y) div 2);
    tvaBottom:r.Top := r.Bottom - y;
    end;
  end;


  HTMLPaint(Canvas,s,r,FImages,pt.x,pt.y,-1,FHoverHyperLink,FShadowOffset,False,False,False,False,False,FHover,
            1.0,FURLcolor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,HyperLinks,mouselink,mr);


  if FAutoSizing then
  begin
    if ((Align = alTop) or (Align = alBottom) or (Align = alNone)) and
       (FAutoSizeType in [asVertical,asBoth]) then
      if (y + 6 <> Height) then Height := y + 6;
    if ((Align = alLeft) or (Align = alRight) or (Align = alNone)) and
       (FAutoSizeType in [asHorizontal,asBoth]) then
      if (x + 6 <> Width) then Width := x + 6;
  end;

  if FUpdateReflection and not (csLoading in ComponentState) and (FUpdateCount <= 0) and (FYSize > 0) then
  begin
    UpdateReflection;
    Paint;
  end;
end;

//------------------------------------------------------------------------------

constructor TAdvReflectionLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizing := False;
  FImageCache := THTMLPictureCache.Create;
  FHTMLText := TStringList.Create;
  Font.Size := 12;
  Font.Style := [fsBold];
  FHTMLText.OnChange := HTMLChanged;
  FILChangeLink := TChangeLink.Create;
  FILChangeLink.OnChange := ImageListChanged;
  Caption := '';
  AutoSize := False;
  FUpdateCount := 0;
  FURLColor := clBlue;
  FShadowColor := clGray;
  FShadowOffset := 2;
  BevelWidth := 1;
  FBorderStyle := bsNone;
  FHover := False;
  FHoverHyperLink := -1;
  FHoverColor := clNone;
  FHoverFontColor := clNone;
  FColorTo := clNone;
  FBorderColor := clBlack;
  FReflectionPic := nil;
  FReflectionSize := 20;
  FReflectionAxis := -7;
  FReflectionOpacityStart := 128;
  FReflectionOpacityEnd := 0;
  Transparent := True;
  Width := 185;
  Height := 40;
end;

//------------------------------------------------------------------------------

destructor TAdvReflectionLabel.Destroy;
begin
  FImageCache.ClearPictures;
  FImageCache.Free;
  FHTMLText.Free;
  if Assigned(FReflectionPic) then
    FReflectionPic.Free;
  FILChangeLink.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.HTMLChanged(sender:TObject);
begin
  FImageCache.Clear;
  UpdateChange;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.ImageListChanged(sender:TObject);
begin
  UpdateChange;
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.GetSize: TSize;
var
  r: TRect;
  x, y: Integer;
  mr:trect;
  hyperlinks,mouselink: Integer;
  s,anchor,stripped,focusanchor:string;
begin
  r := Rect(0,0,4096,4096);
  s := GetDisplText;
  Canvas.Font.Assign(self.Font);

  HTMLPaint(Canvas,s,r,FImages,0,0,-1,FHoverHyperLink,FShadowOffset,True,False,False,False,False,FHover,1.0,
    FURLcolor,FHoverColor,FHoverFontColor,FShadowColor,anchor,stripped,focusanchor,x,y,hyperlinks,mouselink,mr);

  Result.cx := X;
  Result.cy := Y;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetAutoSizing(Value : boolean);
begin
  FAutoSizing := Value;
  if FAutoSizing then
  begin
    if (Align = alLeft) or (Align = alRight) then
      Width := 6;
    if (Align = alTop) or (Align = alBottom) then
      Height := 6;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.UpdateDisplText;
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetImages(value:TImagelist);
begin
  FImages := Value;
  if Assigned(FImages) then
    FImages.RegisterChanges(FILChangeLink);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetURLColor(value:TColor);
begin
  if Value <> FURLColor then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.Loaded;
begin
  inherited;
  Caption := '';
  UpdateChange;
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.IsAnchor(x,y: Integer;var HoverRect:TRect):string;
var
  r: TRect;
  xsize,ysize: Integer;
  s: string;
  Anchor,Stripped,Focusanchor: string;
  hl: Integer;
begin
  r := ClientRect;

  if (bevelInner <> bvNone) or (bevelOuter <> bvNone) then
  begin
    Inflaterect(r,-BevelWidth,-BevelWidth);
  end;

//  if FBorderStyle = bsSingle then
  begin
    InflateRect(r,-BorderWidth,-BorderWidth);
  end;

  s := GetDisplText;

  Anchor := '';
  HoverRect := Rect(-1,-1,-1,-1);

  if FVAlignment in [tvaCenter,tvaBottom] then
  begin
    HTMLPaint(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,FHover,1.0,
      fURLcolor,FHoverColor,FHoverFontColor,FShadowColor,anchor,stripped,focusanchor,XSize,YSize,hl,FHOverHyperLink,HoverRect);

    if ysize < Height then
      case FVAlignment of
      tvaCenter:r.Top := r.Top + ((r.Bottom - r.Top - ysize) div 2);
      tvaBottom:r.Top := r.Bottom - ysize;
      end;
  end;


  if HTMLPaint(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,FHover,1.0,
     clWhite,clNone,clNone,clNone,Anchor,Stripped,FocusAnchor,XSize,YSize,hl,FHoverHyperlink,HoverRect) then
    Result := Anchor
  else
    FHoverHyperLink := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.HoverInvalidate(r:trect);
begin
  if Assigned(Parent) and (Parent is TWinControl) then
  begin
    Offsetrect(r,self.Left,self.Top);
    Invalidaterect((Parent as TWinControl).Handle,@r,True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Anchor:string;
  hr: TRect;
begin
  inherited;
  Anchor := IsAnchor(x,y,hr);

  if Anchor <> '' then
  begin
    if (FAnchor <> Anchor) or not Equalrect(FCurrHoverRect,hr) or (FHoverHyperlink = -1) then
    begin
      if FHover then
      begin
        if hr.Left <> -1 then
          HoverInvalidate(FCurrHoverRect)
      end;
    end;

    if (Cursor = crDefault) or (FAnchor <> Anchor) or (FOldHoverHyperLink <> FHoverHyperLink) then
    begin
      if FAnchorHint then
        Application.CancelHint;
      Cursor := crHandPoint;

      if Assigned(FAnchorEnter) then
        FAnchorEnter(self,anchor);

      if FHover then
      begin
        if hr.Left <> -1 then
          HoverInvalidate(FCurrHoverRect)
        else
          Invalidate;
      end;
    end;

     FAnchor := Anchor;
     FOldHoverHyperLink := FHoverHyperLink;
     FCurrHoverRect := hr;

     if FHover then
       HoverInvalidate(FCurrHoverRect)
  end
  else
  begin
    if self.Cursor = crHandPoint then
    begin
      self.Cursor := crDefault;
      if Assigned(FAnchorExit) then
        FAnchorExit(self,anchor);

      if FHover then
      begin
        if FCurrHoverRect.Left <> -1 then
          HoverInvalidate(FCurrHoverRect)
        else
          Invalidate;
      end;

      FCurrHoverRect := hr;
      if FHover then
        HoverInvalidate(FCurrHoverRect)
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Anchor:string;
  hr: TRect;
begin
  inherited MouseDown(Button,Shift,X,Y);

  Anchor := IsAnchor(X,Y,hr);
  if Anchor <> '' then
  begin
    if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
     shellexecute(0,'open',PChar(Anchor),nil,nil,SW_NORMAL)
    else
    begin
      if Assigned(FAnchorClick) then
         FAnchorClick(self,Anchor);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetBevelWidth(Value: TBevelWidth);
begin
  FBevelWidth := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

Procedure TAdvReflectionLabel.CMHintShow(Var Msg: TMessage);
{$IFDEF DELPHI2_LVL}
type
  PHintInfo = ^THintInfo;
{$ENDIF}
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor:string;
  hr:trect;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);
  Anchor := '';

  if FAnchorHint then
  begin
    Anchor := IsAnchor(hi^.cursorPos.x,hi^.cursorpos.y,hr);
    if Anchor <> '' then
    begin
      if Assigned(FOnAnchorHint) then
        FOnAnchorHint(Self,Anchor);

      hi^.HintPos := clienttoscreen(hi^.CursorPos);
      hi^.hintpos.y := hi^.hintpos.y - 10;
      hi^.hintpos.x := hi^.hintpos.x + 10;
      hi^.HintStr := Anchor;
    end
    else

  end;

  if FHintShowFull and not ((Anchor <> '') and FAnchorHint)  then
  begin
    if FHTMLHint then
    hi^.HintStr := GetDisplText
    else
    hi^.HintStr := HTMLStrip(GetDisplText);
  end;

  Msg.Result := Ord(Not CanShow);
end;


//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetHover(const Value: boolean);
begin
  FHover := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetHoverColor(const Value: TColor);
begin
  FHoverColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetHoverFontColor(const Value: TColor);
begin
  FHoverFontColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.CMMouseLeave(var Msg: TMessage);
begin
  if FHover and (FHoverHyperLink <> -1) then
    HoverInvalidate(FCurrHoverRect);
  fHoverHyperLink := -1;
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then FOnMouseEnter(self);
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.GetDisplText: string;
var
  i: Integer;
begin
  Result:='';
  for i:=1 to FHTMLText.Count do
    Result := Result + FHTMLText.Strings[i - 1];
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.GetText: string;
begin
  Result := HTMLStrip(GetDisplText);
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.HTMLPaint(canvas: TCanvas; s: string; fr: TRect;
  FImages: TImageList; xpos, ypos, Focuslink, Hoverlink,
  shadowoffset: integer; checkhotspot, checkheight, print, selected, blink,
  hoverstyle: boolean; resfactor: double; urlcolor, hovercolor,
  hoverfontColor, shadowcolor: TColor; var anchorval, stripval,
  focusanchor: string; var xsize, ysize, hyperlinks, mouselink: integer;
  var hoverrect: trect): boolean;
begin
  Result := HTMLDrawEx(Canvas,s,fr,FImages,xpos,ypos,-1,HoverLink,ShadowOffset,checkhotspot,checkheight,print,selected,Blink,
                       Hoverstyle,not FEllipsis,Resfactor,urlcolor,hovercolor,hoverfontColor,shadowcolor,anchorval,stripval,focusanchor,
                       XSize,YSize,HyperLinks,MouseLink,HoverRect,FImageCache,FContainer,0);
  FXSize := XSize; //Width;
  FYSize := YSize;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.HTMLPrint(Canvas: TCanvas;r: TRect);
var
  a,st,fa,s:string;
  xs,ys,hl,ml: Integer;
  mr: TRect;
begin
  s := GetDisplText;
  HTMLPaint(Canvas,s,r,FImages,0,0,-1,-1,1,False,False,True,False,False,False,
            1.0,URLColor,clNone,clNone,clGray,a,st,fa,xs,ys,hl,ml,mr);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetReflection(Value: Integer);
begin
  if (FReflectionSize <> Value) then
  begin
    FReflectionSize := Value;
    UpdateChange;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetReflectionAxis(Value: Integer);
begin
  if (FReflectionAxis <> Value) then
  begin
    FReflectionAxis := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetReflectionOpacityEnd(const Value: Integer);
begin
  if (Value < 256) and (Value >= 0) then
  begin
    if (Value <> FReflectionOpacityEnd) then
    begin
      FReflectionOpacityEnd := Value;
      UpdateReflection;
      Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetReflectionOpacityStart(const Value: Integer);
begin
  if (Value < 256) and (Value >= 0) then
  begin
    if (Value <> FReflectionOpacityStart) then
    begin
      FReflectionOpacityStart := Value;
      UpdateReflection;
      Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.PaintText(ACanvas: TCanvas);
var
  hyperlinks, mouselink: Integer;
  s, anchor, stripped, focusanchor:string;
  pt: TPoint;
  r, mr: TRect;
begin
  if not Assigned(aCanvas) or (FUpdateCount > 0) then
    Exit;

  ACanvas.Font.Assign(Self.Font);
  R := GetClientRect;

  if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
    InflateRect(r,-BevelWidth,-BevelWidth);
  if (FBorderStyle = bsSingle) then
    InflateRect(r,-BorderWidth,-BorderWidth);

  s := GetDisplText;

  ACanvas.Brush.Color := self.Color;

  if FAutoSizing then
  begin
    if ((Align = alLeft) or (Align = alRight) or (Align = alNone)) and
       (FAutoSizeType in [asHorizontal,asBoth]) then
      r.Right := r.Right + $FFFF;

    if ((Align = alTop) or (Align = alBottom) or (Align = alNone)) and
       (FAutoSizeType in [asVertical,asBoth]) then
      r.Bottom := r.Bottom + $FFFF;
  end;

  GetCursorPos(pt);
  pt := self.ScreenToClient(pt);

  ACanvas.Brush.Style := bsclear;
  HTMLPaint(ACanvas,s,r,FImages,pt.x,pt.y,-1,FHoverHyperLink,FShadowOffset,False,False,False,False,False,FHover,
          1.0,FURLcolor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,Fx,Fy,HyperLinks,mouselink,mr);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.PaintToBitmap(bmp: TBitmap);
var
  w, h: integer;
  s: string;
begin
  h := FYSize;
  w := FXSize;

  FXStart := 0;
  s := LowerCase(RemoveSpace(GetDisplText));
  if (Pos('align="center"', s) > 0) then
  begin
    FXStart := ((Width - FXSize) div 2);
  end
  else if (Pos('align="right"', s) > 0) then
    FXStart := (Width - FXSize);


  bmp.Height := h;
  bmp.Width := w;
  bmp.TransparentMode := tmAuto;
  bmp.Transparent := True;
  bmp.Canvas.CopyMode := cmSrcCopy;
  bmp.Canvas.CopyRect(Rect(0, 0, FXSize, FYSize), Canvas, Rect(FXStart, 0, FXStart +FXSize, FYSize));
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.UpdateReflection;
var
  bmp: Tbitmap;
  gpbmp: TGPBitmap;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  w, h, x, y, op, alph: integer;
  clr, clrTemp: TGPColor;
  a: byte;
  hr: HResult;

begin
  if (csLoading in ComponentState) or (FUpdateCount > 0) or (FYSize <= 0) then
    Exit;

  FUpdateReflection := False;

  h := FYSize;
  w := FXSize;

  bmp := TBitmap.Create;

  PaintToBitmap(bmp);

  //bmp.Canvas.Font.Assign(Self.Font);
  //bmp.Canvas.TextOut(1, 50, GetDisplText);
  //bmp.LoadFromFile('add.bmp');

  ms := TMemoryStream.Create;
  bmp.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for reflection Label');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if (hr = S_OK) then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      gpbmp := TGPBitmap.Create(pstm);
      gpbmp.RotateFlip(RotateNoneFlipY);

      if Assigned(FReflectionPic) then
      begin
        FReflectionPic.Free;
        FReflectionPic := nil;
      end;

      FReflectionPic := TGPBitmap.Create(w, h{, PixelFormat32bppARGB});

      for y := 0 to h do
      begin
        //op := Round((255.0 / h) * (h - y)) - FReflectionSize;

        if (y < FReflectionSize) then
        begin
          op := Round( ((FReflectionSize - y)/FReflectionSize * FReflectionOpacityStart) +
                         y/ReflectionSize * FReflectionOpacityEnd);
        end
        else
          op := 0;

        if (op < 0) then
          op := 0;
        if (op > 255) then
          op := 255;


        for x := 0 to w do
        begin
          gpbmp.GetPixel(x, y, clr);
          a := GetAlpha(clr);
          if (a = 0) then
            continue;

          alph := Round((op / 255) * a);
          clrTemp := MakeColor(alph, GetRed(clr), GetGreen(clr), GetBlue(clr));
          FRefLectionPic.SetPixel(x, y, clrTemp);
        end;
      end;
      gpbmp.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
  bmp.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetVAlignment(const Value: TVAlignment);
begin
  if fVAlignment<>Value then
   begin
    FVAlignment := Value;
    Invalidate;
   end;
end;

//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

function GetCLSID(ImageType: TImageType): TCLSID;
var
  I: integer;
  num, numi, size: Cardinal;
  clsId: TCLSID;
  pinfo: PImageCodecInfo;
  infoarr: array[0..100] of TImageCodecInfo;
  str: String;
begin
  GdipGetImageEncodersSize(num, size);

  pinfo := AllocMem(size);

  numi := num;

  GdipGetImageEncoders(num, size, pinfo);

  move(pinfo^, infoarr[0], size);

  case ImageType of
    itPNG: str := 'image/png';
    itBMP: str := 'image/bmp';
    itJPEG: str := 'image/jpeg';
    itTIFF: str := 'image/tiff';
    itGIF: str := 'image/gif';
  end;

  for I := 0 to numi - 1 do
  begin
    if infoarr[i].MimeType = str then
    begin
      clsid := infoarr[i].Clsid;
      break;
    end;
  end;

  FreeMem(pinfo);

  Result := clsid;
end;


procedure TAdvReflectionLabel.SaveToFile(FileName: string;
  ImageType: TImageType);
var
  gpbmp, gpbmpout: TGPBitmap;
  g: TGPGraphics;
  enc: TEncoderParameters;
  ms: TMemoryStream;
  bmp: TBitmap;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  hr: HResult;

begin
  if Assigned(FReflectionPic) then
  begin
    // create temp bitmap and paint normal content to bitmap
    bmp := TBitmap.Create;
    bmp.Height := Height;
    bmp.Width := Width;
    bmp.TransparentMode := tmAuto;
    bmp.Transparent := True;
    bmp.Canvas.CopyMode := cmSrcCopy;
    bmp.Canvas.CopyRect(Rect(0, 0, Width, Height), Canvas, Rect(0, 0, Width, Height));

    ms := TMemoryStream.Create;
    bmp.SaveToStream(ms);
    hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);

    if (hGlobal = 0) then
    begin
      ms.Free;
      bmp.Free;
      raise Exception.Create('Could not allocate memory for reflection Label');
    end;

    gpbmpout := TGPBitmap.Create(Width, Height);
    g := TGPGraphics.Create(gpbmpout);

    pstm := nil;
    pcbWrite := 0;

    // Create IStream* from global memory
    hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

    if (hr = S_OK) then
    begin
      pstm.Write(ms.Memory, ms.Size,@pcbWrite);

      if (ms.Size = pcbWrite) then
      begin
        gpbmp := TGPBitmap.Create(pstm);
        try
          g.DrawImage(gpbmp, 0,0);
          enc := GetEncoderQualityParameters(100);
          gpbmp.Save(filename, GetCLSID(ImageType), @enc);
        finally
          gpbmp.Free;
        end;
      end;
      pstm := nil;
    end
    else
      GlobalFree(hGlobal);

    gpbmpout.Free;
    ms.Free;
    bmp.Free;
    g.Free;
  end;
end;

procedure TAdvReflectionLabel.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FAutoSizeType := Value;
  AutoSizing := AutoSizing;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.Doit;
begin
  Paint;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.HilightText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text, HiText,'hi',DoCase);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.MarkText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text,HiText,'e',DoCase);
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.UnHilightText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'hi');
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.UnMarkText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'e');
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetColor(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetGradientType(const Value: TGradientType);
begin
  FGradientType := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetLineWidth(const Value: Integer);
begin
  FLineWidth := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvReflectionLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  UpdateChange;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := not (csLoading in ComponentState) and (Name = FHTMLText.Text) and
    ((Owner = nil) or not (Owner is TControl) or not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then FHTMLText.Text := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.Resize;
begin
  inherited;
  if not (csLoading in ComponentState) then
    UpdateChange;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.WMSize(var Message: TWMSize);
begin
  inherited;
//  if not (csLoading in ComponentState) then
//    UpdateChange;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.UpdateChange;
begin
  FUpdateReflection := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvReflectionLabel.SetHTMLText(Value: TStringlist);
begin
  if Assigned(Value) then
    FHTMLText.Text := CRLFStrip(Value.Text,False);
  UpdateChange;  
end;



end.
