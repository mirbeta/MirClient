{*************************************************************************}
{ THTMLForm component                                                     }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1999-2013                                        }
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

unit HTMLForm;

{$I TMSDEFS.INC}

{$DEFINE REMOVEDRAW}
{$DEFINE HILIGHT}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Shellapi, Extctrls, PictureContainer, AsgCombo, AsgHTMLE,
  Mask;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.1.0.0 : Improved : Anchor detection for vertical center & bottom align
  // v1.2.0.0 : New : Support for PNG images via images in associated PictureContainer
  // v1.2.1.0 : New : OnControlEditMaskError event added

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TRichText = string;

  TAnchorClick = procedure (Sender:TObject; Anchor:string) of object;

  TVAlignment = (tvaTop,tvaCenter,tvaBottom);

  TAutoSizeType = (asVertical,asHorizontal,asBoth);

  THTMLCombo = class(TASGCombobox)
  private
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    procedure KeyPress(var Key: Char); override;
  end;

  THTMLEdit = class(TMaskEdit)
  private
    FOnMaskError: TNotifyEvent;
    procedure WMKeyDown(var Msg:TWMKeydown); message WM_KEYDOWN;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure ValidateError; override;
    property OnMaskError: TNotifyEvent read FOnMaskError write FOnMaskError;
  end;

  THTMLComboSelectEvent = procedure(Sender: TObject; ItemIndex: Integer; CtrlID, CtrlValue: string) of object;

  THTMLComboControlEvent = procedure(Sender: TObject; CtrlID,CtrlType,CtrlVal: string;
    Values: TStringList; var Edit: Boolean; var DropCount: Integer) of object;

  THTMLControlEvent = procedure(Sender: TObject; CtrlID,CtrlType,CtrlVal: string) of object;

  THTMLAnchorClickEvent = procedure(Sender:TObject;Anchor:string; var AutoHandle: Boolean) of object;

  THTMLAnchorHintEvent = procedure(Sender:TObject;var Anchor:string) of object;

  THTMLAnchorEvent = procedure(Sender:TObject;Anchor:string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  THTMLForm = class(TCustomControl)
  private
    FAnchorHint: Boolean;
    FIsWinXP: Boolean;
    { Private declarations }
    FCtrlDown: Boolean;
    FCtrlID: string;
    FCtrlType: string;
    FCtrlEditing: Boolean;
    FBlinking:boolean;
    FAnchor:string;
    FCurrHoverRect: TRect;
    FAutoSizing:boolean;
    FHTMLText:TStringList;
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
    FFocusAnchor: string;
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
    FAutoScroll: Boolean;
    FEditControl: THTMLEdit;
    FComboControl: THTMLCombo;
    FOnControlComboSelect: THTMLComboSelectEvent;
    FOnControlComboList: THTMLComboControlEvent;
    FOnControlClick: THTMLControlEvent;
    FOnControlEditDone: THTMLControlEvent;
    FOnControlEditMaskError: THTMLControlEvent;
    FOnAnchorClick: THTMLAnchorClickEvent;
    FOnAnchorEnter: THTMLAnchorEvent;
    FOnAnchorExit: THTMLAnchorEvent;
    FOnAnchorHint: THTMLAnchorHintEvent;
    FOnAnchorKeyPress: THTMLAnchorEvent;
    procedure SetHTMLText(value : TStringList);
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
    function IsAnchor(x,y:Integer;var HoverRect,CtrlRect:TRect;var CID,CV,CT:string):string;
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
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: integer);
    procedure SetEnableBlink(const Value: boolean);
    function GetText: string;
    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetAutoSizeType(const Value: TAutoSizeType);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetVOffset(const Value: Integer);
    procedure SetMiniScroll(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
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
    procedure ControlComboSelect(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure ControlEnter(S, CT,CID,CV:string; CR: TRect; X,RX,Y: Integer);
    procedure ControlMaskError(Sender: TObject);
    procedure AdvanceHTMLEdit(CtrlID: string);
    procedure Paint; override;
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResetUpdate;
    procedure Doit;
    property Text:string read GetText;
    property VOffset: Integer read FVOffset write SetVOffset;
    procedure HilightText(HiText: string; DoCase: Boolean);
    procedure UnHilightText;
    procedure MarkText(HiText: string; DoCase: Boolean);
    procedure UnMarkText;
    function SetValue(ID,Value:string): Boolean;
    function GetValue(ID:string; var Value:string): Boolean;
    procedure DoPaint(CCanvas:TCanvas; bkg: Boolean = false);
  published
    { Published declarations }
    property Align;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property AutoSizeType: TAutoSizeType read FAutoSizeType write SetAutoSizeType;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property AutoSizing: Boolean read FAutoSizing write SetAutoSizeP;
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
    //property FocusControl;
    property Font;
    property Hover: Boolean read FHover write SetHover default False;
    property HoverColor:TColor read FHoverColor write FHoverColor default clNone;
    property HoverFontColor:TColor read FHoverFontColor write FHoverFontColor default clNone;
    property Hint;
    property HTMLText: TStringList read FHTMLText write SetHTMLText;
    property Images: TImageList read FImages write SetImages;
    property MiniScroll: Boolean read FMiniScroll write SetMiniScroll;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default -1;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property VAlignment: TVAlignment read FVAlignment write SetVAlignment;
    property Version: string read GetVersion write SetVersion;
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

    property OnControlComboSelect: THTMLComboSelectEvent read FOnControlComboSelect write FOnControlComboSelect;
    property OnControlComboList: THTMLComboControlEvent read FOnControlComboList write FOnControlComboList;
    property OnControlClick: THTMLControlEvent read FOnControlClick write FOnControlClick;
    property OnControlEditDone: THTMLControlEvent read FOnControlEditDone write FOnControlEditDone;
    property OnControlEditMaskError: THTMLControlEvent read FOnControlEditMaskError write FOnControlEditMaskError;
    property OnAnchorClick: THTMLAnchorClickEvent read FOnAnchorClick write FOnAnchorClick;
    property OnAnchorEnter: THTMLAnchorEvent read FOnAnchorEnter write FOnAnchorEnter;
    property OnAnchorExit: THTMLAnchorEvent read FOnAnchorExit write FOnAnchorExit;
    property OnAnchorKeyPress: THTMLAnchorEvent read FOnAnchorKeyPress write FOnAnchorKeyPress;
    property OnAnchorHint: THTMLAnchorHintEvent read FOnAnchorHint write FOnAnchorHint;
  end;


implementation

uses
  CommCtrl ,ImgList ;

procedure THTMLForm.AdvanceHTMLEdit(CtrlID: string);
var
  NewID: string;
  ml,hl,XPos,YPos,XSize, YSize: Integer;
  r, hr,CR: TRect;
  CV,CT: string;
  s,Anchor,Stripped,FocusAnchor,AnchorHint: string;
  Canvas: TCanvas;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetDC(self.Handle);
  Canvas.Brush.Color := Self.Color;

  s := GetDisplText;
  NewID := GetNextControlID(s,CtrlID);

  if (NewID <> '') then
  begin
    XPos := -1;
    YPos := -1;

    r := Self.ClientRect;

    if FMiniScroll then
    begin
      r.Top := r.Top - VOffset;
      r.Right := r.Right - 10;
    end;

    if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
      Inflaterect(r,-BevelWidth,-BevelWidth);

    if FBorderStyle = bsSingle then
      Inflaterect(r,-BorderWidth,-BorderWidth);

    if Assigned(self.Font) then
      Canvas.Font.Assign(self.Font);


    HTMLDrawEx(Canvas,s,r,Images,XPos,YPos,-1,0,1,
               True,False,False,False,False,False,not FEllipsis,False, '',
               0.0,FURLColor,clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,AnchorHint,
               XSize,YSize,ml,hl,hr,CR,NewID,CV,CT,FImageCache,FContainer,self.Handle);

    ControlEnter(s,CT,NewID,CV,CR, XPos, XPos, YPos);

  end
  else
    SetFocus;
end;

procedure THTMLForm.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure THTMLForm.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Invalidate;
  end;
end;

procedure THTMLForm.ResetUpdate;
begin
  FUpdateCount := 0;
end;

procedure THTMLForm.DoPaint(CCanvas: TCanvas; bkg: Boolean = false);
var
  R,CR,HR,CtrlRect: TRect;
  x,y,hl,fhl,ml: Integer;
  s,Anchor,Stripped,Focusanchor,CID,CV,CT, AnchorHint:string;
  TopColor, BottomColor: TColor;
  ACanvas: TCanvas;
  Bitmap: TBitmap;
  pt: TPoint;
  hrgn: THandle;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Bitmap := TBitmap.Create;

  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    ACanvas := Bitmap.Canvas;

    ACanvas.Brush.Color := Color;
    ACanvas.Pen.Color := Color;
    ACanvas.Rectangle(0,0, Width, Height);

    if Assigned(Font) then
      ACanvas.Font.Assign(Font);

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

      ACanvas.Pen.Color := Color;
      ACanvas.Pen.Width := 0;
      ACanvas.Brush.Color := Color;

      if (FBorderStyle = bsSingle) and (FBorderWidth > 0) then
      begin
        ACanvas.Pen.Width := FBorderWidth;
        ACanvas.Pen.Color := clBlack;
        ACanvas.Rectangle(r.left,r.top,r.right,r.bottom);
      end;
    end;

    R := GetClientRect;

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
      if (Align in [alLeft,alRight,alNone]) and
         (FAutoSizeType in [asHorizontal,asBoth]) then
        r.Right := r.Right + $FFFF;

      if (Align in [alTop,alBottom,alNone]) and
         (FAutoSizeType in [asVertical,asBoth]) then
        r.Bottom := r.Bottom + $FFFF;
    end;

    if GetFocus <> Handle then
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

      HTMLDrawEx(ACanvas,s,r,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,True,True,False,False,FBlinking,FHover,not FEllipsis,FCtrlDown,'',1.0,
        FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,AnchorHint,x,y,hl,ml,HR,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle);

      if (y >= Height + FVOffset ) then
      begin
        HR := CR;
        HR.Top := CR.Bottom - 10;
        HR.Left := HR.Right - 10;
        DrawFrameControl (ACanvas.Handle,HR,DFC_SCROLL,DFCS_SCROLLDOWN or DFCS_FLAT);
        FDownScroll := True;
      end;

      if VOffset > 0 then
      begin
        HR := CR;
        HR.Bottom := CR.Top + 10;
        HR.Left := HR.Right - 10;
        DrawFrameControl (ACanvas.Handle,HR,DFC_SCROLL,DFCS_SCROLLUP or DFCS_FLAT);
        FUpScroll := True;
      end;

    end;

    if (FVAlignment in [tvaCenter,tvaBottom]) then
    begin
      HTMLDrawEx(ACanvas,s,r,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,True,False,False,False,FBlinking,FHover,not FEllipsis,FCtrlDown,'',1.0,
        FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,AnchorHint,x,y,hl,ml,HR,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle);

      if y < Height then
      case FVAlignment of
      tvaCenter:r.top := r.top + ((r.bottom - r.top - y) shr 1);
      tvaBottom:r.top := r.bottom - y;
      end;
    end;

    hrgn := CreateRectRgn(r.left, r.top, r.right, r.bottom);
    SelectClipRgn(ACanvas.Handle, hrgn);

    if not Enabled then
    begin
      OffsetRect(r,1,1);
      ACanvas.Font.Color := clWhite;
      HTMLDrawEx(ACanvas,s,r,nil,0,0,fhl,FHoverHyperlink,FShadowOffset,False,False,false,False,FBlinking,FHover,not FEllipsis,FCtrlDown,'',1.0,
        clWhite,clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,AnchorHint,x,y,hl,ml,HR,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle);
      ACanvas.Font.Color := clGray;

      OffsetRect(r,-1,-1);
      HTMLDrawEx(ACanvas,s,r,FImages,0,0,fhl,FHoverHyperlink,FShadowOffset,False,False,false,False,FBlinking,FHover,not FEllipsis,FCtrlDown,'',1.0,
        clGray,clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,AnchorHint,x,y,hl,ml,HR,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle);
     end
    else
     HTMLDrawEx(ACanvas,s,R,FImages,pt.x,pt.y,fhl,FHoverHyperlink,FShadowOffset,False,False,false,False,FBlinking,FHover,not FEllipsis,FCtrlDown,'',1.0,
       FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,AnchorHint,x,y,hl,ml,HR,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle);

    SelectClipRgn(ACanvas.Handle, 0);
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

    CCanvas.Draw(0,0,Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure THTMLForm.Paint;
begin
  DoPaint(Canvas);
end;

procedure THTMLForm.WMPaint(var Message: TWMPaint);
begin
  if FUpdateCount > 0 then
    Exit;
  inherited;
end;


constructor THTMLForm.Create(AOwner: TComponent);
var
  VerInfo: TOSVersionInfo;
begin
  inherited;
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

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
  FTimerCount := 0;
  DoubleBuffered := True;
  FMiniScroll := False;
  FUpScroll := False;
  FDownScroll := False;
  FEditControl := THTMLEdit.Create(Self);
  FComboControl := THTMLCombo.Create(Self);
  FComboControl.Flat := false;
  ControlStyle := ControlStyle + [csAcceptsControls];
  DoubleBuffered := true;
  Width := 180;
  Height := 150;
end;

destructor THTMLForm.Destroy;
begin
  FEditControl.Free;
  FComboControl.Free;
  FImageCache.ClearPictures;
  FImageCache.Free;
  FHTMLText.Free;
  inherited;
end;

procedure THTMLForm.HTMLChanged(sender:TObject);
begin
  FHoverHyperLink := -1;
  FFocusHyperlink := -1;
  if FUpdateCount = 0 then
    Invalidate;
end;

procedure THTMLForm.SetAutoSizeP(value : boolean);
begin
  FAutoSizing := value;
  Invalidate;
end;

procedure THTMLForm.SetHTMLText(value:TStringList);
begin
  if Assigned(Value) then
  FHTMLText.Assign(Value);
  Invalidate;
end;

procedure THTMLForm.SetImages(value:TImagelist);
begin
  FImages := Value;
  Invalidate;
end;

procedure THTMLForm.SetURLColor(Value:TColor);
begin
  if Value <> FURLColor then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure THTMLForm.SetHover(Value:boolean);
begin
  if Value <> FHover then
  begin
    FHover := Value;
    Invalidate;
  end;
end;

procedure THTMLForm.Loaded;
begin
  inherited;
  Caption := '';
  if FEnableBlink and (FTimerID = 0) then
    FTimerID := SetTimer(self.Handle,1,100,nil);
  if not FEnableBlink and (FTimerID <> 0) then
    KillTimer(self.handle,FTimerID);
end;

function THTMLForm.IsAnchor(x,y:integer;var HoverRect,CtrlRect:TRect;var CID,CV,CT:string):string;
var
  r: TRect;
  XSize, YSize: Integer;
  s: string;
  Anchor,Stripped,FocusAnchor, AnchorHint: string;
  Canvas: TCanvas;
  hl: Integer;
begin
  r := Clientrect;

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
  Canvas.Handle := GetDC(Handle);

  if Assigned(Font) then
    Canvas.Font.Assign(Font);

  if (FVAlignment in [tvaCenter,tvaBottom]) then
  begin
    HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,FHoverHyperlink,FShadowOffset,True,False,False,False,FBlinking,FHover,not FEllipsis,FCtrlDown,'',1.0,
        FURLColor,FHoverColor,FHoverFontColor,FShadowColor,Anchor,Stripped,FocusAnchor,AnchorHint,xsize,ysize,hl,FHoverHyperlink,HoverRect,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle);

    if y < Height then
    case FVAlignment of
    tvaCenter:r.Top := r.Top + ((r.Bottom - r.Top - ysize) shr 1);
    tvaBottom:r.Top := r.Bottom - ysize;
    end;
  end;

  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,false,FHover,not FEllipsis,FCtrlDown,'',1.0,
    clWhite,clNone,clNone,clNone,Anchor,Stripped,FocusAnchor, AnchorHint,xsize,ysize,hl,FHoverHyperlink,HoverRect,CtrlRect,CID,CV,CT,FImageCache,FContainer,Handle) then
  begin
    Result := Anchor;
  end
  else
    FHoverHyperLink := -1;

  Releasedc(self.Handle,Canvas.Handle);
  Canvas.Free;
end;

procedure THTMLForm.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Anchor,CID,CV,CT: string;
  hr,cr: TRect;

begin
  inherited;

  Anchor := IsAnchor(x,y,hr,cr,CID,CV,CT);

  if Anchor <> '' then
  begin
    if hr.Left = -1 then
      DoPaint(Canvas,False);

    FFocusHyperlink := -1;

    if (FAnchor <> Anchor) or not EqualRect(FCurrHoverRect,hr) or
       (FHoverHyperlink = -1) or (FOldHoverHyperLink <> FHoverHyperLink) then
    begin
      if FHover then
        Invalidate;
        //Invalidaterect(Handle,@FCurrHoverRect,True);
    end;

    if ((Cursor = crDefault) or (Anchor <> FAnchor)) then
    begin
      FAnchor := Anchor;
      if FAnchorHint then
        Application.CancelHint;

      Cursor := crHandPoint;

      if Assigned(FOnAnchorEnter) then
        FOnAnchorEnter(self,Anchor);

      if FHover and (hr.Left <> -1) then
        Invalidate;
      //  Invalidaterect(Handle,@hr,False);

      FOldHoverHyperLink := FHoverHyperLink;
      FCurrHoverRect := hr;
    end;
  end
  else
  begin
    if Cursor = crHandPoint then
    begin
      Cursor := crDefault;
      FFocusHyperlink := -1;

      if FHover then
      begin
        Invalidate;
        //Invalidaterect(Handle,@FCurrHoverRect,True);
        FHoverHyperLink := -1;
        //DoPaint(False);
      end;

      if Assigned(FOnAnchorExit) then
        FOnAnchorExit(Self,Anchor);
    end;
  end;
end;

procedure THTMLForm.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if FCtrlDown = True then
  begin
    FCtrlDown := False;
    DoPaint(Canvas,false);
    invalidate;
  end;
end;


procedure THTMLForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Anchor,CID,CV,CT, s:string;
  hr,cr: TRect;
  Handle, CanEdit: Boolean;
begin
  inherited MouseDown(Button,Shift,X,Y);

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

  Anchor := IsAnchor(X,Y,hr,cr,CID,CV,CT);
  if Anchor <> '' then
  begin
    Handle := True;

    CanEdit := true;

    if (CID <> '') and CanEdit then
    begin
      if FCtrlEditing then
      begin
        ControlExit(Self);
      end;

      s := GetDisplText;

      if CT = 'BUTTON' then
      begin
        FCtrlDown := True;
        DoPaint(Canvas,false);
      end;

      if CT = 'CHECK' then
      begin
        if CV = 'TRUE' then
          SetControlValue(s,CID,'FALSE')
        else
          SetControlValue(s,CID,'TRUE');
        HTMLText.Text := s;
      end;

      if CT = 'RADIO' then
      begin
        s := ClearRadioControls(s);
        SetControlValue(s,CID,'TRUE');
        HTMLText.Text := s;
      end;

      if Assigned(FOnControlClick) then
        FOnControlClick(Self,CID,CT,CV);

      ControlEnter(S, CT, CID, CV, CR, x,x,y);
      Handle := false;
    end
    else
    begin
      if Assigned(FOnAnchorClick) then
        FOnAnchorClick(Self,Anchor,Handle);
    end;

    if Handle then
    begin
      ShellExecute(Application.Handle,'open',PChar(Anchor), nil, nil, SW_NORMAL);
    end;
  end;
end;

procedure THTMLForm.SetBevelInner(Value: TPanelBevel);
begin
  FBevelInner := Value;
  Invalidate;
end;

procedure THTMLForm.SetBevelOuter(Value: TPanelBevel);
begin
  FBevelOuter := Value;
  Invalidate;
end;

procedure THTMLForm.SetBevelWidth(Value: TBevelWidth);
begin
  FBevelWidth := Value;
  Invalidate;
end;

procedure THTMLForm.SetBorderWidth(Value: TBorderWidth);
begin
  FBorderWidth := Value;
  Invalidate;
end;

procedure THTMLForm.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

Procedure THTMLForm.CMHintShow(Var Msg: TMessage);
var
  CanShow: Boolean;
  hi: PHintInfo;
  Anchor,CID,CV,CT: string;
  hr,cr: TRect;

Begin
  CanShow := True;
  hi := PHintInfo(Msg.LParam);

  if FAnchorHint then
  begin
    Anchor := IsAnchor(hi^.cursorPos.x,hi^.cursorpos.y,hr,cr,CID,CV,CT);
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

procedure THTMLForm.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  inherited;
end;


procedure THTMLForm.WMTimer(var Msg: TWMTimer);
var
  s: string;
  HR: TRect;
  pt: TPoint;
  DoAnim: Boolean;
begin
  if not FEnableBlink then
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
    DoPaint(Canvas,False);
  end;

  if FMiniScroll and (FUpScroll or FDownScroll) and (FCtrlDown or FAutoScroll) then
  begin
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

  DoPaint(Canvas,true);
  FBlinking := not FBlinking;
end;

procedure THTMLForm.WMSize(var Msg: TWMSize);
begin
  inherited;
  Invalidate;
end;

procedure THTMLForm.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure THTMLForm.WMKillFocus(var Msg: TWMKillFocus);
begin
  Invalidate;
end;

procedure THTMLForm.WMSetFocus(var Msg: TWMSetFocus);
begin
  if FFocusHyperLink < 0 then
    FFocusHyperLink:=0;
  Invalidate;
end;

procedure THTMLForm.WMKeyDown(var Msg: TWMKeydown);
begin
  if msg.CharCode in [vk_up,vk_left] then
  begin
     DoPaint(Canvas,False);
     if FFocusHyperLink > 0 then
       Dec(FFocusHyperlink)
     else
       FFocusHyperlink := FNumHyperlinks - 1;
     Msg.CharCode := 0;
     DoPaint(Canvas,False);
  end;

  if Msg.CharCode in [vk_down,vk_right] then
  begin
     DoPaint(Canvas,False);
     if FFocusHyperLink < FNumHyperLinks - 1 then
       Inc(FFocusHyperlink)
     else
       FFocusHyperlink := 0;
     Msg.CharCode := 0;
     DoPaint(Canvas,False);
  end;
  inherited;
end;

procedure THTMLForm.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if Msg.CharCode in [vk_up,vk_down,vk_left,vk_right] then
    Msg.Result := 1;
end;


procedure THTMLForm.ControlComboSelect(Sender: TObject);
begin
  if Assigned(FOnControlComboSelect) then
    FOnControlComboSelect(Self,(Sender as TCustomComboBox).ItemIndex, FCtrlID, (Sender as TCustomComboBox).Items[(Sender as TCustomComboBox).ItemIndex]);
end;

procedure THTMLForm.ControlEnter(S, CT, CID, CV: string; CR: TRect; X, RX,
  Y: Integer);
var
  ComboEdit: Boolean;
  DropHeight: Integer;
begin
  if (CT = 'EDIT') or (CT = 'PASSWORD') or (CT = 'MASK') then
  begin
    FCtrlID := CID;
    FCtrlType := CT;
    FCtrlEditing := True;
    FEditControl.Width := 0;

    if (CT = 'PASSWORD') then
      FEditControl.PasswordChar := '*'
    else
      FEditControl.PasswordChar := #0;

    if (CT = 'MASK') then
      FEditControl.EditMask := GetControlProp(s,CID)
    else
      FEditControl.EditMask := '';

    FEditControl.OnExit := ControlExit;
    FEditControl.OnMaskError := ControlMaskError;
    FEditControl.Text := CV;
    FEditControl.BorderStyle := bsNone;
    FEditControl.Left := CR.Left + 1;
    FEditControl.Width := CR.Right - CR.Left - 3;
    FEditControl.Top := CR.Top + 7;
    FEditControl.Height := CR.Bottom - CR.Top - 6;
    FEditControl.MaxLength := GetControlMaxLen(s,CID);
    FEditControl.Parent := Self;
    FEditControl.Visible := True;

    BringWindowToTop(FEditControl.Handle);
    FEditControl.SetFocus;
  end;

  if CT = 'COMBO' then
  begin
    FCtrlID := CID;
    FCtrlType := CT;

    FCtrlEditing := True;

    FComboControl.IsWinXP := FIsWinXP;
    FComboControl.Width := 0;

    ComboEdit := True;
    DropHeight := 8;

    FComboControl.Left := CR.Left - 1;
    FComboControl.Width := CR.Right - CR.Left;
    FComboControl.Top := CR.Top + 6;

    {$IFDEF DELPHI7_LVL}
    FComboControl.OnSelect := ControlComboSelect;
    {$ENDIF}

    FComboControl.Parent := Self;

    if Assigned(FOnControlComboList) then
      FOnControlComboList(Self,CID,CT,CV,TStringList(FComboControl.Items),ComboEdit,DropHeight);

    if ComboEdit then
      FComboControl.Style := csDropDown
    else
      FComboControl.Style := csDropDownList;

    if FComboControl.Items.IndexOf(CV) <> -1 then
      FComboControl.ItemIndex := FComboControl.Items.IndexOf(CV);

    FComboControl.Text := CV;
    FComboControl.DropDownCount := DropHeight;

    FComboControl.OnExit := ControlExit;

    FComboControl.Height := FComboControl.ItemHeight * (DropHeight + 2);
    FComboControl.Height := 0;
    FComboControl.MaxLength := GetControlMaxLen(s,CID);

    FComboControl.Visible := True;
    FComboControl.DroppedDown := True;
    FComboControl.SetFocus;
  end;
end;

procedure THTMLForm.ControlExit(Sender: TObject);
var
  s,CV:string;

begin
  s := GetDisplText;

  if (FCtrlType = 'EDIT') or (FCtrlType = 'PASSWORD') or (FCtrlType = 'MASK') then
  begin
    FEditControl.ValidateEdit;
    CV := FEditControl.Text;
    SetControlValue(s,FCtrlID,CV);
    FEditControl.Visible := False;
    HTMLText.Text := s;
  end;

  if FCtrlType = 'COMBO' then
  begin
    CV := FComboControl.Text;
    SetControlValue(s,FCtrlID,CV);
    FComboControl.Visible := False;
    HTMLText.Text := s;
  end;

  if Assigned(FOnControlEditDone) then
    FonControlEditDone(Self,FCtrlID,FCtrlType,CV);

  FCtrlType := '';
end;

procedure THTMLForm.ControlMaskError(Sender: TObject);
var
  CV:string;

begin

  if (FCtrlType = 'MASK') then
  begin
    CV := FEditControl.Text;
    if Assigned(FOnControlEditMaskError) then
      FOnControlEditMaskError(Self,FCtrlID,FCtrlType,CV);
  end;
end;
procedure THTMLForm.Keypress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
  begin
    if (Pos('://',FFocusAnchor) > 0) or (Pos('mailto:',FFocusAnchor) > 0) then
      ShellExecute(0,'open',pchar(FFocusAnchor),nil,nil,SW_NORMAL)
    else
      if Assigned(FOnAnchorKeypress) then
        FOnAnchorKeypress(self,fFocusAnchor);
  end;
end;

procedure THTMLForm.CMMouseLeave(var Message: TMessage);
begin
  if FHoverHyperlink >= 0 then
  begin
    FHoverHyperlink := -1;
    if FHover then
      Invalidate;
      //InvalidateRect(self.Handle,@FCurrHoverRect,True);
  end;
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

procedure THTMLForm.CMMouseEnter(var Message: TMessage);
var
  pt: TPoint;
  hr,cr: TRect;
  Anchor,CID,CV,CT: string;
begin
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  Anchor := IsAnchor(pt.x,pt.y,hr,cr,CID,CV,CT);

  if ((self.Cursor = crDefault) or (Anchor <> FAnchor)) and (Anchor <> '') then
  begin
    FAnchor := Anchor;
    if FAnchorHint then
      Application.CancelHint;

    self.Cursor := crHandPoint;
    if Assigned(FOnAnchorEnter) then
      FOnAnchorEnter(Self,Anchor);

    if FHover then
      DoPaint(Canvas,False);
    //InvalidateRect(self.Handle,@hr,false);
    FCurrHoverRect := hr;
  end;
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure THTMLForm.WndProc(var Message: tMessage);
begin
  if message.Msg = WM_DESTROY then
  begin
    if FEnableBlink and (FTimerID<>0) then
      KillTimer(Handle,FTimerID);
  end;
  inherited;
end;

procedure THTMLForm.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure THTMLForm.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  Invalidate;
end;

procedure THTMLForm.SetShadowOffset(const Value: integer);
begin
  FShadowOffset := Value;
  Invalidate;
end;

procedure THTMLForm.SetEnableBlink(const Value: boolean);
begin
  FEnableBlink := Value;
  if not (csLoading in ComponentState) then
  begin
    if FEnableBlink and (FTimerID = 0) then
      FTimerID := SetTimer(self.Handle,1,100,nil);
    if not FEnableBlink and (FTimerID <> 0) then
    begin
      KillTimer(self.Handle,FTimerID);
      FTimerID := 0;
      Invalidate;
      FBlinking := False;
    end;
  end;
end;

function THTMLForm.GetText: string;
begin
  Result := HTMLStrip(GetDisplText);
end;

function THTMLForm.GetDisplText: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to FHTMLText.Count do
    Result := Result + FHTMLText.Strings[i-1];
end;


procedure THTMLForm.SetVAlignment(const Value: TVAlignment);
begin
  if FVAlignment <> Value then
  begin
    FVAlignment := Value;
    if FVAlignment <> tvaTop then
      FMiniScroll := False;
    Invalidate;
  end;
end;

procedure THTMLForm.Doit;
begin
  Invalidate;
end;

procedure THTMLForm.SetAutoSizeType(const Value: TAutoSizeType);
begin
  FAutoSizeType := Value;
end;


procedure THTMLForm.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure THTMLForm.SetVOffset(const Value: Integer);
begin
  if not FMiniScroll then Exit;
  if csDesigning in ComponentState then Exit;

  FVOffset := Value;
  if FVOffset < 0 then
    FVOffset := 0
  else
    Invalidate;
end;

procedure THTMLForm.SetMiniScroll(const Value: Boolean);
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


procedure THTMLForm.HilightText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text, HiText,'hi',DoCase, false);
end;

procedure THTMLForm.MarkText(HiText: string; DoCase: Boolean);
begin
  HTMLText.Text := Hilight(HTMLText.Text,HiText,'e',DoCase, false);
end;

procedure THTMLForm.UnHilightText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'hi');
end;

procedure THTMLForm.UnMarkText;
begin
  HTMLText.Text := UnHilight(HTMLText.Text,'e');
end;


function THTMLForm.GetValue(ID: string; var Value: string): Boolean;
begin
  Result := GetControlValue(HTMLText.Text,ID,Value);
end;

function THTMLForm.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function THTMLForm.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function THTMLForm.SetValue(ID, Value: string): Boolean;
var
  s:string;
begin
  s := HTMLText.Text;
  Result := SetControlValue(s,ID,Value);
  if Result then
    HTMLText.Text := s;
end;


procedure THTMLForm.SetVersion(const Value: string);
begin

end;

{ THTMLCombo }

procedure THTMLCombo.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  if (Msg.CharCode = VK_TAB) then
  begin
    Msg.Result := 1;
    Exit;
  end
  else
    inherited;
end;

procedure THTMLCombo.KeyPress(var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    DoExit;
    with (Parent as THTMLForm) do SetFocus;
    Exit;
  end;

  if Key = #9 then
  begin
    Key := #0;
    DoExit;

    with (Parent as THTMLForm) do
    begin
      AdvanceHTMLEdit(FCtrlID);
    end;
    Exit;
  end;
  inherited;
end;

{ THTMLEdit }

procedure THTMLEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  if (Msg.CharCode = VK_RETURN) then
  begin
    Msg.Result := 0;
    Exit;
  end;

  if (Msg.CharCode = VK_TAB) then
  begin
    Msg.Result := 1;
    Exit;
  end
  else
    inherited;
end;

procedure THTMLEdit.KeyPress(var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    DoExit;
    with (Parent as THTMLForm) do SetFocus;
    Exit;
  end;

  if Key = #9 then
  begin
    Key := #0;
    DoExit;

    with (Parent as THTMLForm) do
    begin
      AdvanceHTMLEdit(FCtrlID);
    end;

    Exit;
  end;

  inherited;
end;

procedure THTMLEdit.ValidateError;
begin
  if Assigned(FOnMaskError) then
    FOnMaskError(Self);
  inherited;
end;

procedure THTMLEdit.WMKeyDown(var Msg: TWMKeydown);
begin
  inherited;
end;


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.
