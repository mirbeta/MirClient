{**************************************************************************}
{ THTMLabel component                                                      }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1999-2015                                         }
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

unit HTMLabel;

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ComObj, ActiveX, PictureContainer, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 1; // Build nr.


// version history
// v1.7.1.0 : Added HTMLSize public property
// v1.7.1.1 : Fixed issue with MouseMove event handler
// v1.7.5.0 : Added BorderColor property
// v1.7.5.1 : Improved border width > 1 handling
// v1.7.5.2 : Improved URL color printing
// v1.7.5.3 : Fixed issue with anchors for vertical center & bottom alignment
// v1.7.5.4 : Fixed issue with AutoSizing = true & VAlignment <> tvaTop
// v1.7.5.5 : Improved double buffered painting when not transparent
// v1.7.5.6 : Fixed : issue with anchor detection and non XP themed apps
// v1.8.0.0 : New : property LineSpacing added
//          : Improved : property persistence in DFM file
//          : Improved : LineWidth property to set width of gradient line
//          : Improved : OnAnchorClick event triggered before default handling
// v1.8.0.1 : Fixed : issue with Constraints & AutoSizing
// v1.8.0.2 : Fixed : issue with sizing to size = 0
// v1.8.1.0 : New : support for customizing bullets in HTML UL lists
// v1.8.2.0 : New : Attribute Color added for HR tag
// v1.8.3.0 : New : Sizing with aspect ratio of images in HTML formatted text
// v1.8.3.1 : Fixed : Issue with auto sizing and BorderWidth > 1
// v1.8.3.2 : Fixed : OnAnchorClick only triggered for mbLeft mouse click
// v1.8.3.3 : Fixed : Issue with <p> centering on images with width attribute
// v1.8.3.4 : Fixed : Issue with Transparent property for old Delphi versions
// v1.8.3.5 : Fixed : paragraph background & font color issue in rendering
// v1.8.4.0 : New : Event OnNewSize added
// v1.9.0.0 : New : Support for PNG images via images in associated PictureContainer
// v1.9.0.1 : Fixed : Issue with Focus control handling
// v1.9.0.2 : Improved : reading property HTMLSize doesn't invoke a repaint anymore
// v1.9.1.0 : New : Support for anchor hints (set with title="" attribute)
// v1.9.1.1 : Fixed : Issue with font handling & autosizing
// v1.9.1.2 : Fixed : Issue with HTML engine font color handling for specific combinations of URL & font tag
// v1.9.1.3 : Fixed : Issue with ordered list rendering
// v1.9.2.0 : Improved : Compatibility with TAdvRichEditor HTML output
// v1.9.2.1 : Fixed : Issue with right-aligned text anchor detection


type

  TRichText = string;

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;

  TAnchorHintEvent = procedure (Sender:TObject; var Anchor:string) of object;

  TAutoSizeType = (asVertical,asHorizontal,asBoth);

  TGradientType = (gtFullHorizontal, gtFullVertical, gtBottomLine, gtCenterLine, gtTopLine);

  TVAlignment = (tvaTop,tvaCenter,tvaBottom);

  TNewSizeEvent = procedure (Sender:TObject; NewWidth, NewHeight : Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLabel = class(TCustomLabel)
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
    FLineSpacing: Integer;
    FHTMLHint: Boolean;
    FHintShowFull: Boolean;
    FILChangeLink: TChangeLink;
    FXSize: Integer;
    FYSize: Integer;
    FColorTo: TColor;
    FOnNewSize: TNewSizeEvent;
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
    function IsAnchor(x,y: Integer;var hoverrect: TRect):string;
    procedure CMHintShow(Var Msg: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(Var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
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
    procedure SetLineSpacing(const Value: Integer);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function GetDisplText:string; virtual;
    procedure DoAnchorClick(Anchor:string); virtual;
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
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Doit;
    property Text:string read GetText;
    property ImageCache: THTMLPictureCache read FImageCache;
    procedure HTMLPrint(Canvas:TCanvas;r:TRect);
    procedure HilightText(HiText: string; DoCase: Boolean);
    procedure UnHilightText;
    procedure MarkText(HiText: string; DoCase: Boolean);
    procedure UnMarkText;
    function GetSizeWithMaxWidth(AMaxWidth : integer) : TSize;
    property HTMLSize: TSize read GetSize;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property BiDiMode;
    property ColorTo: TColor read FColorTo write SetColor default clNone;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property AnchorHint: Boolean read FAnchorHint write FAnchorHint default false;
    property AutoSizing: Boolean read FAutoSizing write SetAutoSizing default false;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write SetAutoSizeType default asVertical;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default false;
    property FocusControl;
    property GradientType: TGradientType read FGradientType write SetGradientType default gtFullHorizontal;
    property Font;
    property Hint;
    property HintShowFull: Boolean read FHintShowFull write FHintShowFull default false;
    property Hover:boolean read fHover write SetHover default false;
    property HoverColor:TColor read fHoverColor write SetHoverColor default clNone;
    property HoverFontColor:TColor read FHoverFontColor write SetHoverFontColor default clNone;
    property HTMLHint: Boolean read FHTMLHint write FHTMLHint default False;
    property HTMLText: TStringList read FHTMLText write SetHTMLText;
    property Images: TImageList read FImages write SetImages;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 2;
    property LineSpacing: Integer read FLineSpacing write SetLineSpacing default 0;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ShadowColor:TColor read fShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read fShadowOffset write SetShadowOffset default 2;
    property ShowHint;
    property Transparent;
    property URLColor:TColor read fURLColor write SetURLColor default clBlue;
    property VAlignment:TVAlignment read fVAlignment write SetVAlignment default tvaTop;
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
    property OnAnchorClick: TAnchorClick read FAnchorClick write FAnchorClick;
    property OnAnchorEnter: TAnchorClick read FAnchorEnter write FAnchorEnter;
    property OnAnchorExit: TAnchorClick read FAnchorExit write FAnchorExit;
    property OnAnchorHint: TAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnNewSize: TNewSizeEvent read FOnNewSize write FOnNewSize;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

uses
  CommCtrl, ShellApi;

{$IFDEF DELPHI_UNICODE}
type
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
{$ENDIF}

{$I HTMLENGO.PAS}

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

function THTMLabel.GetSizeWithMaxWidth(AMaxWidth : integer) : TSize;
var
  r: TRect;
  x, y: Integer;
  mr:trect;
  hyperlinks,mouselink: Integer;
  s,anchor,stripped,focusanchor:string;
  bmp: TBitmap;

begin
  bmp := TBitmap.Create;

  try
    r := Rect(0,0,AMaxWidth,4096);
    s := GetDisplText;
    bmp.Canvas.Font.Assign(Font);

    HTMLPaint(bmp.Canvas,s,r,FImages,0,0,-1,FHoverHyperLink,FShadowOffset,True,False,False,False,False,FHover,1.0,
      FURLcolor,FHoverColor,FHoverFontColor,FShadowColor,anchor,stripped,focusanchor,x,y,hyperlinks,mouselink,mr);

    Result.cx := X;
    Result.cy := Y;
  finally
    bmp.Free;
  end;
end;

procedure THTMLabel.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure THTMLabel.EndUpdate;
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


procedure THTMLabel.Paint;
var
  r,mr:trect;
  x,y,hyperlinks,mouselink: Integer;
  s,anchor,stripped,focusanchor:string;
  TopColor, BottomColor: TColor;
  pt:TPoint;
  gsteps,bw1,bw2: Integer;
  DVAlignment : TVAlignment;
  newsize: boolean;
  Buffer : TBitmap;
  tmpCanvas : TCanvas; // Canvas painted to - preserve transparency

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Buffer := nil;
  bw1 := 0;
  NewSize := false;
  R := GetClientRect;

  if R.Right <= R.Left then
    Exit;

  if R.Bottom <= R.Top then
    Exit;

  try
    tmpCanvas := Canvas;

    Caption := '';

    gsteps := (R.Right - R.Left) div 4;

    if not Transparent then
    begin
      Buffer := TBitmap.Create;
      Buffer.Width := R.Right;
      Buffer.Height := R.Bottom;

      tmpCanvas := Buffer.Canvas;
      if (ColorTo <> clNone) and (GradientType in [gtFullHorizontal,gtFullVertical]) then
      begin
        DrawGradient(tmpCanvas,Color,ColorTo,gsteps,R,GradientType = gtFullHorizontal);
      end
      else
      begin
        tmpCanvas.Brush.Color := Color;
        tmpCanvas.Pen.Color := Color;
        tmpCanvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
      end;
    end;

    case GradientType of
    gtBottomLine: R.Top := R.Bottom - LineWidth;
    gtTopLine: R.Bottom := R.Top + LineWidth;
    gtCenterLine:
      begin
        R.Top := R.Top + (R.Bottom - R.Top - LineWidth) div 2;
        R.Bottom := R.Top + LineWidth;
      end;
    end;

    if GradientType in [gtBottomLine, gtTopLine, gtCenterLine] then
    begin
      {clip out region}
      {
      if TransparentText then
      begin
        LR := R;
        OffsetRect(LR,Left,Top);

        rgn3 := CreateRectRgn(LR.Left,LR.Top,LR.Right,LR.Bottom);
        case Alignment of
        taLeftJustify:
          begin
            rgn1 := CreateRectRgn(LR.Left,LR.Top,LR.Left + Indent,LR.Bottom);
            rgn2 := CreateRectRgn(LR.Left + Indent + tw,LR.Top, LR.Right,LR.Bottom);
            CombineRgn( rgn3,rgn1,rgn2, RGN_OR);
          end;
        taRightJustify:
          begin
            rgn1 := CreateRectRgn(LR.Left,LR.Top,LR.Right - tw - Indent,LR.Bottom);
            rgn2 := CreateRectRgn(LR.Right - Indent,LR.Top, LR.Right,LR.Bottom);
            CombineRgn( rgn3,rgn1,rgn2, RGN_OR);
          end;
        taCenter:
          begin
            rgn1 := CreateRectRgn(LR.Left,LR.Top,LR.Left + ((LR.Right - LR.Left - tw) div 2),LR.Bottom);
            rgn2 := CreateRectRgn(LR.Left + tw + ((LR.Right - LR.Left -tw) div 2),LR.Top, LR.Right,LR.Bottom);
            CombineRgn( rgn3,rgn1,rgn2, RGN_OR);
          end;
        end;

        SelectClipRgn(Canvas.Handle,rgn3);

      end;
      }
      DrawGradient(tmpCanvas,Color,ColorTo,gsteps,R,true);

      {
      if TransparentText then
      begin
        SelectClipRgn(Canvas.Handle,0);
        DeleteObject(rgn1);
        DeleteObject(rgn2);
        DeleteObject(rgn3);
      end;
      }
    end;

    //inherited Paint;
    tmpCanvas.Font.Assign(Font);

    if FUpdateCount > 0 then
      Exit;

    R := GetClientRect;

    if BevelOuter <> bvNone then
    begin
      AdjustColors(BevelOuter);
      Frame3D(tmpCanvas, R, TopColor, BottomColor, BevelWidth);
    end;
      Frame3D(tmpCanvas, R, Color, Color, BorderWidth);

    if BevelInner <> bvNone then
    begin
      AdjustColors(BevelInner);
      Frame3D(tmpCanvas, R, TopColor, BottomColor, BevelWidth);
    end;

    if (FBorderStyle = bsSingle) and (FBorderWidth > 0) and (FBorderColor <> clNone) then
    begin
      tmpCanvas.Pen.Width := FBorderWidth;
      tmpCanvas.Pen.Color := FBorderColor;
      tmpCanvas.Brush.Color := clBlack;
      tmpCanvas.Brush.Style := bsClear;


      bw1 := Trunc((FBorderWidth + 1) / 2);
      bw2 := Trunc(FBorderWidth / 2);

      tmpCanvas.MoveTo(r.Left - bw1, r.Top - bw1);
      tmpCanvas.LineTo(r.Right + bw2, r.Top - bw1);
      tmpCanvas.LineTo(r.Right + bw2, r.Bottom + bw2);
      tmpCanvas.LineTo(r.Left - bw1, r.Bottom + bw2);
      tmpCanvas.LineTo(r.Left - bw1, r.Top - bw1);
      //Canvas.Rectangle(r.left + bw,r.top + bw,r.right - bw,r.bottom - bw);
    end;

    if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
    begin
      InflateRect(r,-BevelWidth,-BevelWidth);
    end;

    if (FBorderStyle = bsSingle) then
    begin
      InflateRect(r,-bw1,-bw1);
    end;

    s := GetDisplText;

    tmpCanvas.Brush.Color := self.Color;

    DVAlignment := FVAlignment;

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
    pt := Self.ScreenToClient(pt);

    if FAutoSizing or (DVAlignment in [tvaCenter,tvaBottom]) then
    begin
      // get size of the text to perform vertical alignment
      HTMLPaint(tmpCanvas,s,r,FImages,pt.x,pt.y,-1,FHoverHyperLink,FShadowOffset,True,False,False,False,False,FHover,1.0,fURLcolor,FHoverColor,FHoverFontColor,FShadowColor,anchor,stripped,focusanchor,x,y,hyperlinks,mouselink,mr);
    end;

    tmpCanvas.Font.Assign(Font);


    if FAutoSizing then
    begin
      case FVAlignment of
        tvaCenter: r.Top := r.Top + 3;
        tvaBottom: r.Top := r.Top + 6;
      end;

      if BorderStyle = bsSingle then
      begin
        x := x + BorderWidth * 2;
        y := y + BorderWidth * 2;
      end;

      if ((Align = alTop) or (Align = alBottom) or (Align = alNone)) and
         (FAutoSizeType in [asVertical,asBoth]) then
        if (y + 6 <> Height) then
        begin
          Height := y + 6;
          NewSize := true;
        end;
      if ((Align = alLeft) or (Align = alRight) or (Align = alNone)) and
         (FAutoSizeType in [asHorizontal,asBoth]) then
        if (x + 6 <> Width) then
        begin
          Width := x + 6;
          NewSize := true;
        end;
    end;

    // adapt vertical text position
    if DVAlignment in [tvaCenter,tvaBottom] then
    begin
      case FVAlignment of
        tvaCenter: r.Top := r.Top + (Height - y) div 2;
        tvaBottom: r.Top := Height - y;
      end;
    end;

    HTMLPaint(tmpCanvas,s,r,FImages,pt.x,pt.y,-1,FHoverHyperLink,FShadowOffset,False,False,False,False,False,FHover,
              1.0,FURLcolor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,x,y,HyperLinks,mouselink,mr);


    if not Transparent then
    begin
      Canvas.CopyRect(ClientRect, tmpCanvas, ClientRect);
    end;

  finally
    if (Buffer <> nil) then
    begin
    Buffer.Free;
    end;
  end;

  if FAutoSizing and Assigned(FOnNewSize) and NewSize then
    FOnNewSize(Self, Width, Height);
end;

constructor THTMLabel.Create(AOwner: TComponent);
var
  FDesignTime: boolean;
begin
  inherited;
  {$IFDEF DELPHI7_LVL}
  Transparent := true;
  {$ENDIF}
  FAutoSizing := False;
  FImageCache := THTMLPictureCache.Create;
  FHTMLText := TStringList.Create;
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
  Width := 120;
  FLineWidth := 2;
  FLineSpacing := 0;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    HTMLText.Add('TMS <b>HTML</b> label '+ Name);  
end;

destructor THTMLabel.Destroy;
begin
  FImageCache.ClearPictures;
  FImageCache.Free;
  FHTMLText.Free;
  FILChangeLink.Free;
  inherited;
end;

procedure THTMLabel.HTMLChanged(sender:TObject);
begin
  FImageCache.Clear;
  Invalidate;
end;

procedure THTMLabel.ImageListChanged(sender:TObject);
begin
  Invalidate;
end;

function THTMLabel.GetSize: TSize;
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

procedure THTMLabel.SetAutoSizing(Value : boolean);
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

procedure THTMLabel.SetHTMLText(Value: TStringlist);
begin
  if Assigned(Value) then
    FHTMLText.Text := CRLFStrip(Value.Text,False);
  UpdateDisplText;
end;

procedure THTMLabel.UpdateDisplText;
begin
  Invalidate;
end;

procedure THTMLabel.SetImages(value:TImagelist);
begin
  FImages := Value;
  if Assigned(FImages) then
    FImages.RegisterChanges(FILChangeLink);

  Invalidate;
end;

procedure THTMLabel.SetURLColor(value:TColor);
begin
  if Value <> FURLColor then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.Loaded;
begin
  inherited;
  Caption := '';
end;

function THTMLabel.IsAnchor(x,y: Integer;var HoverRect:TRect): string;
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
  FocusAnchor := '';
  HoverRect := Rect(-1,-1,-1,-1);

  Canvas.Font.Assign(Font);

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
  begin
    Result := Anchor;
    if (FocusAnchor <> '') then
      Result := FocusAnchor;
  end
  else
    FHoverHyperLink := -1;
end;

procedure THTMLabel.HoverInvalidate(r:trect);
begin
  if Assigned(Parent) and (Parent is TWinControl) then
  begin
    Offsetrect(r,self.Left,self.Top);
    Invalidaterect((Parent as TWinControl).Handle,@r,True);
  end;
end;

procedure THTMLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure THTMLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Anchor:string;
  hr: TRect;
begin
  inherited MouseDown(Button,Shift,X,Y);

  Anchor := IsAnchor(X,Y,hr);
  if (Anchor <> '') and (Button = mbLeft) then
  begin
    DoAnchorClick(Anchor);

    if (Pos('://',Anchor) > 0) or (Pos('mailto:',Anchor) > 0) then
     ShellExecute(0,'open',PChar(Anchor),nil,nil,SW_NORMAL)
  end;
end;

procedure THTMLabel.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

procedure THTMLabel.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Invalidate;
end;

procedure THTMLabel.SetBevelWidth(Value: TBevelWidth);
begin
  FBevelWidth := Value;
  Invalidate;
end;

procedure THTMLabel.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Invalidate;
end;

procedure THTMLabel.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

procedure THTMLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FocusControl <> nil) and Enabled and ShowAccelChar and
    IsAccel(Message.CharCode, HTMLText.Text) then
    with FocusControl do
      if CanFocus then
      begin
        SetFocus;
        Message.Result := 1;
      end;
end;

Procedure THTMLabel.CMHintShow(Var Msg: TMessage);
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


procedure THTMLabel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;

procedure THTMLabel.SetShadowColor(const Value: TColor);
begin
  if (FShadowColor <> Value) then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.SetShadowOffset(const Value: integer);
begin
  if (FShadowOffset <> Value) then
  begin
    FShadowOffset := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.SetHover(const Value: boolean);
begin
  FHover := Value;
end;

procedure THTMLabel.SetHoverColor(const Value: TColor);
begin
  FHoverColor := Value;
end;

procedure THTMLabel.SetHoverFontColor(const Value: TColor);
begin
  FHoverFontColor := Value;
end;

procedure THTMLabel.CMMouseLeave(var Msg: TMessage);
begin
  if FHover and (FHoverHyperLink <> -1) then
    HoverInvalidate(FCurrHoverRect);
  FHoverHyperLink := -1;
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

procedure THTMLabel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;


function THTMLabel.GetDisplText: string;
var
  i: Integer;
  s: string;

  function LastChar(s: string): char;
  begin
    Result := #0;
    if length(s) = 0 then
      Exit;
    Result := s[length(s)];
  end;

  function FirstChar(s: string): char;
  begin
    Result := #0;
    if length(s) = 0 then
      Exit;
    Result := s[1];
  end;

begin
  Result := '';
  for i := 0 to FHTMLText.Count - 1 do
  begin
    if i = 0 then
      Result := Result + FHTMLText.Strings[i]
    else
    begin
      s := FHTMLText.Strings[i];
      if (LastChar(Result) <> '>') or (FirstChar(s) <> '<') then
        Result := Result + ' '+ s
      else
        Result := Result + s;
    end;
  end;
end;


function THTMLabel.GetText: string;
begin
  Result := HTMLStrip(GetDisplText);
end;

function THTMLabel.HTMLPaint(canvas: TCanvas; s: string; fr: TRect;
  FImages: TImageList; xpos, ypos, Focuslink, Hoverlink,
  shadowoffset: integer; checkhotspot, checkheight, print, selected, blink,
  hoverstyle: boolean; resfactor: double; urlcolor, hovercolor,
  hoverfontColor, shadowcolor: TColor; var anchorval, stripval,
  focusanchor: string; var xsize, ysize, hyperlinks, mouselink: integer;
  var hoverrect: trect): boolean;
begin
  Result := HTMLDrawEx(Canvas,s,fr,FImages,xpos,ypos,-1,HoverLink,ShadowOffset,checkhotspot,checkheight,print,selected,Blink,
                       Hoverstyle,not FEllipsis,Resfactor,urlcolor,hovercolor,hoverfontColor,shadowcolor,anchorval,stripval,focusanchor,
                       XSize,YSize,HyperLinks,MouseLink,HoverRect,FImageCache,FContainer,FLineSpacing);
  FXSize := XSize;
  FYSize := YSize;
end;


procedure THTMLabel.HTMLPrint(Canvas: TCanvas;r: TRect);
var
  a,st,fa,s:string;
  xs,ys,hl,ml: Integer;
  mr: TRect;
begin
  s := GetDisplText;
  HTMLPaint(Canvas,s,r,FImages,0,0,-1,-1,1,False,False,True,False,False,False,
            1.0,URLColor,clNone,clNone,clGray,a,st,fa,xs,ys,hl,ml,mr);
end;

procedure THTMLabel.SetVAlignment(const Value: TVAlignment);
begin
  if fVAlignment<>Value then
   begin
    FVAlignment := Value;
    Invalidate;
   end;
end;

procedure THTMLabel.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FAutoSizeType := Value;
  AutoSizing := AutoSizing;
end;

procedure THTMLabel.DoAnchorClick(Anchor: string);
begin
  if Assigned(FAnchorClick) then
    FAnchorClick(self,Anchor);
end;

procedure THTMLabel.Doit;
begin
  Paint;
end;

procedure THTMLabel.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.HilightText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text, HiText,'hi',DoCase);
end;

procedure THTMLabel.MarkText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text,HiText,'e',DoCase);
end;

procedure THTMLabel.UnHilightText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'hi');
end;

procedure THTMLabel.UnMarkText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'e');
end;

procedure THTMLabel.SetColor(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.SetGradientType(const Value: TGradientType);
begin
  if (FGradientType <> Value) then
  begin
    FGradientType := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.SetLineSpacing(const Value: Integer);
begin
  if (FLineSpacing <> Value) then
  begin
    FLineSpacing := Value;
    Invalidate;
  end;
end;

procedure THTMLabel.SetLineWidth(const Value: Integer);
begin
  if (FLineWidth <> Value) then
  begin
    FLineWidth := Value;
    Invalidate;
  end;
end;


function THTMLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure THTMLabel.SetVersion(const Value: string);
begin

end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.
