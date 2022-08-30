{ ************************************************************************* }
{ TAdvSmoothPopup component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{ copyright © 2010 - 2015                                                   }
{ Email : info@tmssoftware.com                                              }
{ Website : http://www.tmssoftware.com/                                     }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{ ************************************************************************* }

unit AdvSmoothPopup;
{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Graphics, Messages, Forms, Controls,
  AdvGDIP, AdvStyleIF, Math, GDIPFill, stdctrls,
  GDIPPictureContainer, ImgList, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with flashing popup in older Delphi versions
  //          : Fixed : issue with setting Footer and Header Caption in designtime
  // v1.0.0.2 : Fixed : issue with position of control in Delphi 2006 or older
  // v1.0.1.0 : New : property autosize to automatically adapt to the control size
  // v1.0.1.1 : Fixed : Issue with Button and Caption colors when using office styles
  // v1.0.1.2 : Fixed : Issue with autosize and positioning of controls
  // v1.0.2.0 : New : Event OnBeforeClose added
  //          : New : Event OnCloseQuery added
  // v1.0.2.1 : Fixed : Issue with destroying form in Delphi 7
  // v1.1.0.0 : New : Metro Style Support
  // v1.1.0.1 : Improved : FormStyle property exposed
  // v1.1.1.0 : New : Innerborder, innerbordercolor and innerborderwidth exposed
  //          : Fixed : Issue with innerborder not showing at first show
  // v1.1.1.1 : Fixed : Issue with popup in older delphi versions
  // v1.1.2.0 : New : Header and Footer click events
  // v1.2.0.0 : New : Windows 8, Office 2013 styles added
  //          : Improved : HideFromWindows public property to hide the popup window from Windows ALT+Tab manager
  // v1.2.1.0 : New : ButtonBorderColor
  // v1.2.1.1 : Fixed : Issue with reverse AnimationFactor
  // v1.3.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothPopup = class;

  TAdvSmoothPopupFormPlaceHolder = class(TForm)
  private
    FOwner: TAdvSmoothPopup;
  public
    procedure Init;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
  end;

  TAdvSmoothPopupForm = class(TForm)
  private
    FMainBuffer: TGPBitmap;
    FPopup: TAdvSmoothPopup;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
  protected
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Paint; override;

    // ---- Paint proc
    procedure Draw(Graphics: TGPGraphics);

    // ---- Paint buffer
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(Graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;

    // ---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateLayered;
    procedure UpdateMainWindow;
    procedure UpdateWindow;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property Popup: TAdvSmoothPopup read FPopup write FPopup;
  end;

  TAdvSmoothPopupArrowPosition = (paLeftTop, paLeftCenter, paLeftBottom,
    paTopLeft, paTopCenter, paTopRight, paRightTop, paRightCenter,
    paRightBottom, paBottomLeft, paBottomCenter, paBottomRight);

  TAdvSmoothPopupDirection = (pdLeftTop, pdLeftCenter, pdLeftBottom, pdTopLeft,
    pdTopCenter, pdTopRight, pdRightTop, pdRightCenter, pdRightBottom,
    pdBottomLeft, pdBottomCenter, pdBottomRight);

  TPopupButtonPosition = (bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight);

  TPopupButton = class(TCollectionItem)
  private
    FRect, FTextR: TGPRectF;
    FOwner: TAdvSmoothPopup;
    FCaption: string;
    FEnabled: Boolean;
    FImage: TAdvGDIPPicture;
    FImageName: String;
    FImageIndex: Integer;
    FVisible: Boolean;
    FPosition: TPopupButtonPosition;
    FTag: Integer;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageIndex(const Value: Integer);
    procedure SetImageName(const Value: String);
    procedure SetPosition(const Value: TPopupButtonPosition);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure ImageChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property ImageName: String read FImageName write SetImageName;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Position : TPopupButtonPosition read FPosition write SetPosition default bpTopLeft;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TPopupButtons = class(TCollection)
  private
    FOwner: TAdvSmoothPopup;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TPopupButton;
    procedure SetItem(Index: Integer; const Value: TPopupButton);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TAdvSmoothPopup);
    property Items[Index: Integer]: TPopupButton read GetItem write SetItem; default;
    function Add: TPopupButton;
    function Insert(Index: Integer): TPopupButton;
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

  TButtonClick = procedure(Sender: TObject; Index: integer) of object;

  TPopupEvent = procedure(Sender: TObject) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothPopup = class(TComponent, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FFlatStyle: Boolean;
    FFooterR, FheaderR: TGPRectF;
    FHeaderCaptionRect, FFooterCaptionRect: TGPRectF;
    FClosing: Boolean;
    FFocusedElement: Integer;
    FHoveredElement, FDownElement: TPopupButton;
    FPrevParent: TWinControl;
    FPrevAlign: TAlign;
    FPrevLeft, FPrevTop, FPrevWidth, FPrevHeight: Integer;
    frmC: TAdvSmoothPopupFormPlaceHolder;
    frm: TAdvSmoothPopupForm;
    FBorderColor: TColor;
    FColor: TColor;
    FShadowColor: TColor;
    FHeaderHeight: Integer;
    FWidth: Integer;
    FFooterHeight: Integer;
    FHeight: Integer;
    FShadowIntensity: Double;
    FControl: TControl;
    FArrowSize: Integer;
    FArrowPosition: TAdvSmoothPopupArrowPosition;
    FAnimation: Boolean;
    FButtons: TPopupButtons;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FButtonDownColor: TColor;
    FButtonDisabledColor: TColor;
    FButtonColor: TColor;
    FButtonFont: TFont;
    FButtonHoverColor: TColor;
    FCloseOnDeactivate: Boolean;
    FShadowSize: Integer;
    FOnButtonClick: TButtonClick;
    FFooterCaption: String;
    FHeaderCaption: String;
    FFooterFont: TFont;
    FHeaderFont: TFont;
    FOpacity: Byte;
    FOnShow: TPopupEvent;
    FOnClose: TPopupEvent;
    FOnBeforeClose: TNotifyEvent;
    FVisible: Boolean;
    FAnimationFactor: Integer;
    FAutoPositioning: Boolean;
    FAutoSize: Boolean;
    FOnCloseQuery: TCloseQueryEvent;
    FFormStyle: TFormStyle;
    FInnerBorder: Boolean;
    FInnerBorderColor: TColor;
    FInnerBorderWidth: Integer;
    FOnHeaderClick: TNotifyEvent;
    FOnFooterClick: TNotifyEvent;
    FHideFromWindows: Boolean;
    FButtonBorderColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetShadowColor(const Value: TColor);
    procedure SetHeaderHeight(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetFooterHeight(const Value: Integer);
    procedure SetShadowIntensity(const Value: Double);
    procedure SetControl(const Value: TControl);
    procedure SetArrowPosition(const Value: TAdvSmoothPopupArrowPosition);
    procedure SetArrowSize(const Value: Integer);
    procedure SetAnimation(const Value: Boolean);
    procedure SetButtons(const Value: TPopupButtons);
    procedure SetButtonColor(const Value: TColor);
    procedure SetButtonDisabledColor(const Value: TColor);
    procedure SetButtonDownColor(const Value: TColor);
    procedure SetButtonFont(const Value: TFont);
    procedure SetButtonHoverColor(const Value: TColor);
    procedure SetCloseOnDeactivate(const Value: Boolean);
    procedure SetShadowSize(const Value: Integer);
    procedure SetFooterCaption(const Value: String);
    procedure SetHeaderCaption(const Value: String);
    procedure SetFooterFont(const Value: TFont);
    procedure SetHeaderFont(const Value: TFont);
    procedure SetOpacity(const Value: Byte);
    procedure SetAnimationFactor(const Value: Integer);
    procedure SetAutoPositioning(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetFormStyle(const Value: TFormStyle);
    procedure SetInnerBorder(const Value: Boolean);
    procedure SetInnerBorderColor(const Value: TColor);
    procedure SetInnerBorderWidth(const Value: Integer);
    procedure SetButtonBorderColor(const Value: TColor);
  protected
    procedure Changed;
    procedure Notification(AComponent: TComponent; AOperation: TOperation);
      override;
    procedure CloseWithAnimation;
    procedure ShowWithAnimation;
    procedure ButtonsChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure DoButtonClick(Sender: TObject; AIndex: Integer);
    procedure InitializeRects;
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormOrigClose(Sender: TObject; var Action: TCloseAction);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    property HideFromWindows: Boolean read FHideFromWindows write FHideFromWindows default False;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoPopup(X, Y: Integer; UsedControl: Boolean);
    procedure PopupAt(X, Y: Integer);
    procedure PopupAtControl(Control: TWinControl;
      Direction: TAdvSmoothPopupDirection);
    procedure ClosePopup;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure DrawPopup(g: TGPGraphics; R: TGPRectF);
    procedure DrawButtons(g: TGPGraphics; R: TGPRectF);
    procedure DrawHeaderAndFooter(g: TGPGraphics);
    function ButtonAtXY(pX, pY: Integer): TPopupButton;
    procedure SetGlobalColor(AColor: TColor);
    function GetMaxMarginSize: Integer;
    procedure SetDefaultStyle;
  published
    property FormStyle: TFormStyle read FFormStyle write SetFormStyle;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property AutoPositioning: Boolean read FAutoPositioning write SetAutoPositioning default False;
    property AnimationFactor: Integer read FAnimationFactor write SetAnimationFactor default 250;
    property Visible: Boolean read FVisible;
    property Opacity: Byte read FOpacity write SetOpacity default 175;
    property CloseOnDeactivate: Boolean read FCloseOnDeactivate write SetCloseOnDeactivate default true;
    property Animation: Boolean read FAnimation write SetAnimation default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $2D1606;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default $2D1606;
    property ShadowSize: Integer read FShadowSize write SetShadowSize default 20;
    property ShadowIntensity: Double read FShadowIntensity write SetShadowIntensity;
    property Color: TColor read FColor write SetColor default $2D1606;
    property InnerBorder: Boolean read FInnerBorder write SetInnerBorder default True;
    property InnerBorderColor: TColor read FInnerBorderColor write SetInnerBorderColor default clBlack;
    property InnerBorderWidth: Integer read FInnerBorderWidth write SetInnerBorderWidth default 1;
    property Height: Integer read FHeight write SetHeight default 300;
    property HeaderHeight: Integer read FHeaderHeight write SetHeaderHeight default 35;
    property FooterHeight: Integer read FFooterHeight write SetFooterHeight default 35;
    property Width: Integer read FWidth write SetWidth default 250;
    property ArrowPosition: TAdvSmoothPopupArrowPosition read FArrowPosition write SetArrowPosition default paBottomCenter;
    property ArrowSize: Integer read FArrowSize write SetArrowSize default 17;
    property Control: TControl read FControl write SetControl;
    property Buttons: TPopupButtons read FButtons write SetButtons;
    property ButtonFont: TFont read FButtonFont write SetButtonFont;
    property ButtonColor: TColor read FButtonColor write SetButtonColor default $2D1606;
    property ButtonBorderColor: TColor read FButtonBorderColor write SetButtonBorderColor default clBlack;
    property ButtonHoverColor: TColor read FButtonHoverColor write SetButtonHoverColor default $00AB5216;
    property ButtonDownColor: TColor read FButtonDownColor write SetButtonDownColor default $00783910;
    property ButtonDisabledColor: TColor read FButtonDisabledColor write SetButtonDisabledColor default clSilver;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write FPictureContainer;
    property ImageList: TCustomImageList read FImageList write FImageList;
    property OnButtonClick: TButtonClick read FOnButtonClick write FOnButtonClick;
    property OnHeaderClick: TNotifyEvent read FOnHeaderClick write FOnHeaderClick;
    property OnFooterClick: TNotifyEvent read FOnFooterClick write FOnFooterClick;
    property HeaderCaption: String read FHeaderCaption write SetHeaderCaption;
    property FooterCaption: String read FFooterCaption write SetFooterCaption;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property OnShow: TPopupEvent read FOnShow write FOnShow;
    property OnClose: TPopupEvent read FOnClose write FOnClose;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
  end;

implementation

uses
  SysUtils;

function Lighter(Color: TColor; Percent: Byte): TColor;
var
  R, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  R := R + muldiv(255 - R, Percent, 100); // Percent% closer to white
  g := g + muldiv(255 - g, Percent, 100);
  b := b + muldiv(255 - b, Percent, 100);
  result := RGB(R, g, b);
end;

function Darker(Color: TColor; Percent: Byte): TColor;
var
  R, g, b: Byte;
begin
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  R := R - muldiv(R, Percent, 100); // Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(R, g, b);
end;

function PtInGPRect(R: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= R.X) and (pt.X <= R.X + R.Width)) and
    ((pt.Y >= R.Y) and (pt.Y <= R.Y + R.Height));
end;

function RectanglesInterSect(r1, r2: TGPRectF): Boolean;
var
  X, Y, w, h: Double;
begin
  X := max(r1.X, r2.X);
  Y := max(r1.Y, r2.Y);
  w := min(r1.X + r1.Width, r2.X + r2.Width);
  h := min(r1.Y + r1.Height, r2.Y + r2.Height);

  result := ((w > X) and (h > Y));
end;

{ TAdvSmoothPopup }

procedure TAdvSmoothPopup.InitializeRects;
var
  rt: TGPRectF;
  i: Integer;
  mgr, spc: Integer;
  img: TAdvGDIPPicture;
  bmp: TBitmap;
  size: Double;
  ft: TGPFont;
  textr: TGPRectF;
  sf: TGPStringFormat;
  ir: TGPRectF;
  ptl, ptr, pbl, pbr: TGPPointF;
  p: TGPPointF;
  ht: Double;
  r: TGPRectF;
  g: TGPGraphics;
  gpbmp: TGPBitmap;
  imgs: Double;
  hr, fr: TGPRectF;
  hrs, frs: Double;
  totalh, totalf: Double;
  mxs: Integer;
begin
  gpbmp := TGPBitmap.Create(1, 1);
  g := TGPGraphics.Create(gpbmp);

  mxs := GetMaxMarginSize;
  R := MakeRect(0, 0, GetWidth, GetHeight);

  rt := MakeRect(R.X + mxs / 2, R.Y + mxs / 2,
    R.Width - 1 - mxs, R.Height - 1 - mxs);

  spc := 4;
  mgr := 5;
  ptl := MakePoint(rt.X, rt.Y);
  ptr := MakePoint(rt.X + rt.Width - mgr, rt.Y);
  pbl := MakePoint(rt.X, rt.Y + rt.Height - FooterHeight);
  pbr := MakePoint(rt.X + rt.Width - mgr, rt.Y + rt.Height - FooterHeight);

  for i := 0 to Buttons.Count - 1 do
  begin
    with Buttons[i] do
    begin
      if Visible then
      begin
        size := 0;
        if not Image.Empty then
          size := Image.Width + mgr * 2;

        if Assigned(PictureContainer) then
        begin
          if ImageName <> '' then
          begin
            img := PictureContainer.FindPicture(ImageName);
            if Assigned(img) then
              if not img.Empty then
                size := img.Width + mgr * 2;
          end;
        end;

        if Assigned(ImageList) then
        begin
          if (ImageIndex >= 0) and (ImageIndex <= ImageList.Count - 1) then
          begin
            bmp := TBitmap.Create;
            ImageList.GetBitmap(ImageIndex, bmp);
            if not bmp.Empty then
              size := bmp.Width + mgr * 2;
            bmp.Free;
          end;
        end;

        if size > 0 then
          imgs := size
        else
          imgs := mgr;

        case Position of
          bpTopLeft:
            p := ptl;
          bpTopRight:
            p := ptr;
          bpBottomLeft:
            p := pbl;
          bpBottomRight:
            p := pbr;
        end;

        ht := 0;
        case Position of
          bpTopRight, bpTopLeft:
            ht := HeaderHeight;
          bpBottomRight, bpBottomLeft:
            ht := FooterHeight;
        end;

        if Caption <> '' then
        begin
          ft := g.MakeFont(ButtonFont);
          sf := TGPStringFormat.Create;
          g.MeasureString(Caption, Length(Caption), ft, MakeRect
              (0, 0, 10000, 10000), sf, textr);
          sf.Free;
          ft.Free;

          size := size + textr.Width + mgr * 2;
        end;

        if size = 0 then
          size := ht - (mgr * 2);

        case Position of
          bpTopRight, bpBottomRight:
            begin
              p.X := p.X - size - spc;
            end;
        end;

        ir := MakeRect(p.X + mgr, p.Y + mgr, size, ht - (mgr * 2));

        FRect := ir;
        FTextR := MakeRect(ir.X + imgs, ir.Y + (ir.Height - textr.Height) / 2, textr.Width, ir.Height);

        case Position of
          bpTopRight:
            ptr := p;
          bpBottomRight:
            pbr := p;
          bpTopLeft:
            ptl.X := ptl.X + size + spc;
          bpBottomLeft:
            pbl.X := pbl.X + size + spc;
        end;
      end;
    end;
  end;


  totalh := ptr.X - ptl.X;
  totalf := pbr.X - pbl.X;

  hrs := 0;
  if HeaderCaption <> '' then
  begin
    ft := g.MakeFont(HeaderFont);
    sf := TGPStringFormat.Create;
    g.MeasureString(HeaderCaption, Length(HeaderCaption), ft, MakeRect
        (0, 0, 10000, 10000), sf, hr);
    sf.Free;
    ft.Free;

    hrs := hr.Width + mgr * 2;
  end;

  frs := 0;
  if FooterCaption <> '' then
  begin
    ft := g.MakeFont(HeaderFont);
    sf := TGPStringFormat.Create;
    g.MeasureString(FooterCaption, Length(FooterCaption), ft, MakeRect
        (0, 0, 10000, 10000), sf, fr);
    sf.Free;
    ft.Free;

    frs := fr.Width + mgr * 2;
  end;

  FFooterR := MakeRect(rt.X, rt.Y + rt.Height - FooterHeight, rt.Width, FooterHeight);

  FHeaderR := MakeRect(rt.X, rt.Y, rt.Width, HeaderHeight);

  if hrs > totalh then
  begin
    for I := 0 to Buttons.Count - 1 do
    begin
      with Buttons[i] do
      begin
        case Position of
          bpTopLeft, bpTopRight:
          begin
            FRect := MakeRect(FRect.X, FRect.Y, FRect.Width, FRect.Height / 2);
            FTextR := MakeRect(FTextR.X, FTextR.Y - (FRect.Height / 2) , FTextR.Width, FTextR.Height / 2);
          end;
        end;
      end;
    end;

    FHeaderCaptionRect := MakeRect(FHeaderR.X + mgr , FHeaderR.Y + (FHeaderR.Height / 2) + mgr , FHeaderR.Width - mgr * 2, (FHeaderR.Height / 2) - mgr * 2);
  end
  else
    FHeaderCaptionRect := MakeRect(ptl.X + mgr, FHeaderR.Y + (FHeaderR.Height - hr.Height) / 2, totalh - mgr, hr.Height);

  if frs > totalf then
  begin
    for I := 0 to Buttons.Count - 1 do
    begin
      with Buttons[i] do
      begin
        case Position of
          bpBottomLeft, bpBottomRight:
          begin
            FRect := MakeRect(FRect.X, FRect.Y + FRect.Height / 2, FRect.Width, FRect.Height / 2);
            FTextR := MakeRect(FTextR.X, FTextR.Y + FRect.Height / 2 , FTextR.Width, FTextR.Height / 2);
          end;
        end;
      end;
    end;

    FFooterCaptionRect := MakeRect(FFooterR.X + mgr , FFooterR.Y + mgr , FFooterR.Width - mgr * 2, (FFooterR.Height / 2) - mgr * 2);
  end
  else
    FFooterCaptionRect := MakeRect(pbl.X + mgr, FFooterR.Y + (FFooterR.Height - fr.Height) / 2, totalf - mgr, fr.Height);

  g.Free;
  gpbmp.Free;
end;

function TAdvSmoothPopup.ButtonAtXY(pX, pY: Integer): TPopupButton;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to Buttons.Count - 1 do
  begin
    with Buttons[i] do
    begin
      if Visible then
      begin
        if Enabled and PtInGPRect(FRect, Point(pX, pY)) then
        begin
          Result := Buttons[i];
          break;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothPopup.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAdvSmoothPopup.FormDeactivate(Sender: TObject);
begin
  if CloseOnDeactivate then
    ClosePopup;
end;

procedure TAdvSmoothPopup.FormOrigClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAdvSmoothPopup.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothPopup.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothPopup.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothPopup.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothPopup.GetHeight: Integer;
begin
  if AutoSize and Assigned(Control) then
    Result := Control.Height + GetMaxMarginSize + HeaderHeight + FooterHeight
  else
    Result := Height + GetMaxMarginSize;
end;

function TAdvSmoothPopup.GetMaxMarginSize: Integer;
begin
  Result := Max(ArrowSize * 2, ShadowSize * 2);
end;

function TAdvSmoothPopup.GetWidth: Integer;
begin
  if AutoSize and Assigned(Control) then
    Result := Control.Width + GetMaxMarginSize
  else
    Result := Width + GetMaxMarginSize;
end;

procedure TAdvSmoothPopup.ButtonsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothPopup.Changed;
begin
  if csDestroying in ComponentState then
    Exit;

  InitializeRects;

  if Assigned(frm) then
  begin
    frm.UpdateWindow;
  end;
end;

procedure TAdvSmoothPopup.ClosePopup;
var
  CanClose: Boolean;
begin
  if FClosing then
    Exit;

  CanClose := True;
  if Assigned(OnCloseQuery) then
    OnCloseQuery(Self, CanClose);

  if CanClose then
  begin
    if Assigned(OnBeforeClose) then
      OnBeforeClose(Self);

    FClosing := True;
    if Animation then
      CloseWithAnimation;

    if Assigned(frmC) then
    begin
      if Assigned(Control) then
      begin
        Control.Visible := False;
        Control.Parent := FPrevParent;
        Control.Align := FPrevAlign;
        FControl.SetBounds(FPrevLeft, FPrevTop, FPrevWidth, FPrevHeight);
      end;

      frmC.Close;
      frmC := nil;
    end;

    if Assigned(frm) then
    begin
      frm.Close;
      frm := nil;
    end;

    if Assigned(OnClose) then
      OnClose(Self);

    FVisible := False;
  end;
end;

procedure TAdvSmoothPopup.CloseWithAnimation;
begin
  while Assigned(frm) and Assigned(frmC) and (frmC.AlphaBlendValue > 0) do
  begin
    frmC.AlphaBlendValue := max(0, frmC.AlphaBlendValue - (255 - AnimationFactor));
    frm.UpdateMainWindow;
    Sleep(1);
  end;
end;

constructor TAdvSmoothPopup.Create(AOwner: TComponent);
begin
  inherited;
  FHideFromWindows := False;
  FAutoSize := False;
  FFormStyle := fsStayOnTop;
  FAutoPositioning := False;
  FOpacity := 175;
  FAnimationFactor := 250;
  FAnimation := true;
  FInnerBorder := True;
  FInnerBorderColor := clBlack;
  FInnerBorderWidth := 1;
  FColor := $2D1606;
  FBorderColor := $2D1606;
  FShadowColor := $2D1606;
  FShadowIntensity := 0.9;
  FFooterHeight := 35;
  FHeaderHeight := 35;
  FWidth := 250;
  FHeight := 300;
  FShadowSize := 20;
  FArrowSize := 17;
  FArrowPosition := paBottomCenter;
  FButtons := TPopupButtons.Create(Self);
  FButtons.OnChange := ButtonsChanged;
  FCloseOnDeactivate := true;
  FButtonColor := $2D1606;
  FButtonHoverColor := $00AB5216;
  FButtonDownColor := $00783910;
  FButtonBorderColor := clBlack;
  FButtonDisabledColor := clSilver;
  FButtonFont := TFont.Create;
  FButtonFont.Name := 'Tahoma';
  FButtonFont.Color := clWhite;
  FHeaderFont := TFont.Create;
  FHeaderFont.Name := 'Tahoma';
  FHeaderFont.Color := clWhite;
  FHeaderFont.Size := 12;
  FHeaderFont.Style := [fsBold];
  FFooterFont := TFont.Create;
  FFooterFont.Name := 'Tahoma';
  FFooterFont.Color := clWhite;
  FFooterFont.Size := 12;
  FFooterFont.Style := [fsBold];

  if Assigned(AOwner) then
  begin
    if (csDesigning in AOwner.ComponentState) and not (csLoading in AOwner.ComponentState) then
    begin
      FHeaderCaption := 'Header';
      FFooterCaption := 'Footer';
    end;
  end;

  FButtonFont.OnChange := FontChanged;
  FHeaderFont.OnChange := FontChanged;
  FFooterFont.OnChange := FontChanged;
end;

destructor TAdvSmoothPopup.Destroy;
begin
  FFooterFont.Free;
  FHeaderFont.Free;
  FButtonFont.Free;
  FButtons.Free;
  inherited;
end;

procedure TAdvSmoothPopup.DoButtonClick(Sender: TObject; AIndex: Integer);
begin
  if Assigned(OnButtonClick) then
    OnButtonClick(Sender, AIndex);
end;

procedure TAdvSmoothPopup.DoPopup(X, Y: Integer; UsedControl: Boolean);
var
  rt, ir: TGPRectF;
  ExtendedStyle: Integer;
begin
  if not Assigned(frm) and not Assigned(frmC) then
  begin
    InitializeRects;

    frm := TAdvSmoothPopupForm.CreateNew(Application);
    frm.OnClose := FormOrigClose;
    frmC := TAdvSmoothPopupFormPlaceHolder.CreateNew(frm);
    frmC.OnDeactivate := FormDeactivate;
    frmC.OnClose := FormClose;
    frmC.FOwner := Self;
    frmC.Init;
    frmC.FormStyle := FormStyle;

    if Animation then
      frmC.AlphaBlendValue := 0;

    frm.Popup := Self;
    frm.Width := GetWidth;
    frm.Height := GetHeight;
    frm.Init;
    {$IFDEF DELPHI2006_LVL}
    frm.FormStyle := FormStyle;
    {$ENDIF}

    rt := MakeRect(GetMaxMarginSize / 2, GetMaxMarginSize / 2, frm.Width - 1 - GetMaxMarginSize,
      frm.Height - 1 - GetMaxMarginSize);
    ir := MakeRect(rt.X + 7, rt.Y + HeaderHeight, rt.Width - 13,
      rt.Height - FooterHeight - HeaderHeight);

    if Assigned(Control) and not (csDesigning in ComponentState) then
    begin
      Control.Align := alClient;
      Control.Parent := frmC;
      Control.Visible := True;
    end;

    if HideFromWindows then
    begin
      ExtendedStyle := GetWindowLong(frm.Handle, GWL_EXSTYLE);
      SetWindowLong(frm.Handle, GWL_EXSTYLE, ExtendedStyle OR WS_EX_TOOLWINDOW
                                                   AND NOT WS_EX_APPWINDOW);

      ExtendedStyle := GetWindowLong(frmC.Handle, GWL_EXSTYLE);
      SetWindowLong(frmC.Handle, GWL_EXSTYLE, ExtendedStyle OR WS_EX_TOOLWINDOW
                                                   AND NOT WS_EX_APPWINDOW);
    end;

    frmC.Show;
    frmC.Width := Round(ir.Width - 1);
    frmC.Height := Round(ir.Height - 1);
    frmC.Left := Round(ir.X + X);
    frmC.Top := Round(ir.Y + Y);

    SetWindowPos(frm.Handle, 0, X, Y, frm.Width, frm.Height, SWP_SHOWWINDOW or SWP_NOACTIVATE);

    if Animation then
      ShowWithAnimation;

    if Assigned(frmC) and not FClosing then
      frmc.SetFocus;

    FClosing := False;

    if Assigned(OnShow) then
      OnShow(Self);

    FVisible := True;
  end;
end;

procedure TAdvSmoothPopup.DrawButtons(g: TGPGraphics; R: TGPRectF);
var
  i: Integer;
  mgr: Integer;
  img: TAdvGDIPPicture;
  bmp: TBitmap;
  h: THandle;
  ca: TCanvas;
  ft: TGPFont;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  f: TGDIPFill;
  fc: Boolean;
  pthf: TGPGraphicsPath;
  lb: TGPLinearGradientBrush;
  pen: TGPPen;
  irt: TGPRectF;
begin
  mgr := 5;
  fc := False;
  if Assigned(frmC) then
    fc := frmC.Focused;
  f := TGDIPFill.Create;
  f.BorderColor := BorderColor;
  f.GradientType := gtSolid;
  if FFlatStyle then
    f.Opacity := 255
  else
    f.Opacity := 100;

  if not FFlatStyle then
    f.Rounding := 3
  else
  begin
    f.Rounding := 0;
    f.BorderColor := clSilver;
  end;

  f.RoundingType := rtBoth;

  for i := 0 to Buttons.Count - 1 do
  begin
    with Buttons[i] do
    begin
      if Visible then
      begin
        if Enabled then
        begin
          f.Focus := (FFocusedElement = i) and fc;
          if FDownElement = Buttons[i] then
            f.Color := ButtonDownColor
          else if FHoveredElement = Buttons[i] then
            f.Color := ButtonHoverColor
          else
            f.Color := ButtonColor;
        end
        else
          f.Color := ButtonDisabledColor;

        f.BorderColor := ButtonBorderColor;
        f.Fill(g, FRect);

        if not FFlatStyle then
        begin
          irt := MakeRect(FRect.X, FRect.Y, FRect.Width, FRect.Height / 2);
          pthf := CreateRoundRectangle(irt, f.Rounding, rtTop, False);
          lb := TGPLinearGradientBrush.Create
            (MakeRect(irt.X - 1, irt.Y - 1, irt.Width + 2, irt.Height + 2),
            MakeColor(100, clWhite), MakeColor(20, clWhite),
            LinearGradientModeVertical);
          g.FillPath(lb, pthf);
          lb.Free;
          pthf.Free;
        end;

        pthf := CreateRoundRectangle
          (MakeRect(FRect.X + 1, FRect.Y + 1, FRect.Width - 2, FRect.Height - 2),
          f.Rounding, rtBoth, False);
        pen := TGPPen.Create(MakeColor(20, clWhite), 1);
        g.DrawPath(pen, pthf);
        pen.Free;
        pthf.Free;

        if not Image.Empty then
          Image.GDIPDraw(g, MakeRect(FRect.X + mgr, FRect.Y + (FRect.Height - Image.Height)
                / 2, Image.Width, Image.Height));

        if Assigned(PictureContainer) then
        begin
          if ImageName <> '' then
          begin
            img := PictureContainer.FindPicture(ImageName);
            if Assigned(img) then
              if not img.Empty then
                img.GDIPDraw
                  (g, MakeRect(FRect.X + mgr, FRect.Y + (FRect.Height - img.Height) / 2,
                    img.Width, img.Height));
          end;
        end;

        if Assigned(ImageList) then
        begin
          if (ImageIndex >= 0) and (ImageIndex <= ImageList.Count - 1) then
          begin
            bmp := TBitmap.Create;
            ImageList.GetBitmap(ImageIndex, bmp);
            if not bmp.Empty then
            begin
              h := g.GetHDC;
              ca := TCanvas.Create;
              ca.Handle := h;
              ca.Draw(Round(FRect.X + mgr), Round(FRect.Y + (FRect.Height - bmp.Height) / 2),
                bmp);
              ca.Free;
              g.ReleaseHDC(h);
            end;
            bmp.Free;
          end;
        end;

        if Caption <> '' then
        begin
          ft := g.MakeFont(ButtonFont);
          sf := TGPStringFormat.Create;
          b := TGPSolidBrush.Create(MakeColor(255, ButtonFont.Color));
          g.DrawString(Caption, Length(Caption), ft, FTextR, sf, b);
          b.Free;
          sf.Free;
          ft.Free;
        end;
      end;
    end;
  end;

  f.Free;
end;

procedure TAdvSmoothPopup.DrawHeaderAndFooter(g: TGPGraphics);
var
  ft: TGPFont;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
begin
  if HeaderCaption <> '' then
  begin
    ft := g.MakeFont(HeaderFont);
    sf := TGPStringFormat.Create;
    sf.SetAlignment(StringAlignmentCenter);
    sf.SetLineAlignment(StringAlignmentCenter);
    b := TGPSolidBrush.Create(MakeColor(255, HeaderFont.Color));
    g.DrawString(HeaderCaption, Length(HeaderCaption), ft, FHeaderCaptionRect, sf, b);
    b.Free;
    sf.Free;
    ft.Free;
  end;

  if FooterCaption <> '' then
  begin
    ft := g.MakeFont(FooterFont);
    sf := TGPStringFormat.Create;
    sf.SetAlignment(StringAlignmentCenter);
    sf.SetLineAlignment(StringAlignmentCenter);
    b := TGPSolidBrush.Create(MakeColor(255, FooterFont.Color));
    g.DrawString(FooterCaption, Length(FooterCaption), ft, FFooterCaptionRect, sf, b);
    b.Free;
    sf.Free;
    ft.Free;
  end;
end;

procedure TAdvSmoothPopup.DrawPopup(g: TGPGraphics; R: TGPRectF);
var
  f: TGDIPFill;
  ir: TGPRectF;
  pth, pthf: TGPGraphicsPath;
  rgn: TGPRegion;
  hr, fr, rt: TGPRectF;
  lb: TGPLinearGradientBrush;
  l, t, w, d, h: Double;
  radius: Integer;
  p: TGPPen;
  pthb: TGPPathGradientBrush;
  cb: array [0 .. 2] of TGPColor;
  pb: array [0 .. 2] of single;
  arrowpos, arrowposend: TGPPointF;
  target, origin: TGPPointF;
  aw, ah: Double;
  quarter: byte; // quadrant (tl, tr, br, bl) ?
  fx, pX: Double;
  fy, pY: Double;
  X, Y, ht: Double;
  arx, ary: Double;
  arrowpts: array [0 .. 3] of TGPPointF;
  pt: TGPPointF;
  ar: TGPPointF;
  sb: TGPBrush;
  art: TGPRectF;
begin
  if FFlatStyle then
    radius := 0
  else
    radius := 4;
  rt := MakeRect(R.X + ShadowSize, R.Y + ShadowSize,
    R.Width - 1 - ShadowSize * 2, R.Height - 1 - ShadowSize * 2);
  f := TGDIPFill.Create;
  f.Rounding := radius;
  f.RoundingType := rtBoth;
  f.Color := Color;
  f.GradientType := gtSolid;
  f.Opacity := Opacity;

  ir := MakeRect(rt.X + 7, rt.Y + HeaderHeight, rt.Width - 13,
    rt.Height - FooterHeight - HeaderHeight);
  pth := CreateRoundRectangle(ir, radius, rtBoth, False);
  rgn := TGPRegion.Create(pth);
  g.SetClip(rgn, CombineModeExclude);

  pthf := CreateRoundRectangle(R, radius, rtBoth, False);
  pthb := TGPPathGradientBrush.Create(pthf);
  pthb.SetWrapMode(WrapModeClamp);
  cb[0] := MakeColor(0, 0, 0, 0);
  cb[1] := MakeColor(12, ShadowColor);
  cb[2] := MakeColor(Opacity + 55, ShadowColor);

  pb[0] := 0;
  pb[1] := 1 - ShadowIntensity;
  pb[2] := 1;

  pthb.SetInterpolationColors(@cb, @pb, 3);

  g.FillPath(pthb, pthf);

  pthb.Free;
  pthf.Free;

  f.Fill(g, rt);

  fr := MakeRect(rt.X + 1, rt.Y + rt.Height - FooterHeight - 7, rt.Width - 1,
    (FooterHeight / 2) + 7);
  pthf := CreateRoundRectangle(fr, radius, rtNone, False);
  lb := TGPLinearGradientBrush.Create
    (MakeRect(fr.X - 1, fr.Y - 1, fr.Width + 2, fr.Height + 2), MakeColor
      (60, clWhite), MakeColor(10, clWhite), LinearGradientModeVertical);
  g.FillPath(lb, pthf);
  lb.Free;
  pthf.Free;

  g.ResetClip;
  rgn.Free;
  pth.Free;
  f.Free;

  hr := MakeRect(rt.X + 1, rt.Y + 1, rt.Width - 1, HeaderHeight / 2);
  pth := CreateRoundRectangle(hr, radius, rtTop, False);
  lb := TGPLinearGradientBrush.Create
    (MakeRect(hr.X - 1, hr.Y - 1, hr.Width + 2, hr.Height + 2), MakeColor
      (100, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
  g.FillPath(lb, pth);
  lb.Free;
  pth.Free;

  l := hr.X;
  t := hr.Y;
  w := hr.Width - 1;
  d := radius shl 1;

  pth := TGPGraphicsPath.Create;
  pth.AddArc(l, t, d, d, 180, 90); // topleft
  pth.AddLine(l + radius, t, l + w - radius, t); // top
  pth.AddArc(l + w - d, t, d, d, 270, 90); // topright

  p := TGPPen.Create(MakeColor(Opacity - 80, clWhite));
  g.DrawPath(p, pth);
  p.Free;

  pth.Free;

  if InnerBorder then
  begin
    pth := CreateRoundRectangle(MakeRect(ir.X, ir.Y, ir.Width - 1, ir.Height - 1)
        , radius - 1, rtBoth, False);
    p := TGPPen.Create(MakeColor(Opacity - 50, InnerBorderColor), InnerBorderWidth);
    g.DrawPath(p, pth);
    p.Free;
    pth.Free;
  end;

  // arrow
  ah := ArrowSize;
  aw := ArrowSize;
  case ArrowPosition of
    paLeftTop:
      begin
        arrowpos := MakePoint(rt.X, rt.Y + ah);
        arrowposend := MakePoint(arrowpos.X - ArrowSize, arrowpos.Y);
        ah := ArrowSize * 3 / 2;
        aw := ArrowSize;
      end;
    paLeftCenter:
      begin
        arrowpos := MakePoint(rt.X, rt.Y + rt.Height / 2);
        arrowposend := MakePoint(arrowpos.X - ArrowSize, arrowpos.Y);
        ah := ArrowSize * 3 / 2;
        aw := ArrowSize;
      end;
    paLeftBottom:
      begin
        arrowpos := MakePoint(rt.X, rt.Y + rt.Height - ah);
        arrowposend := MakePoint(arrowpos.X - ArrowSize, arrowpos.Y);
        ah := ArrowSize * 3 / 2;
        aw := ArrowSize;
      end;
    paRightTop:
      begin
        arrowpos := MakePoint(rt.X + rt.Width, rt.Y + ah);
        arrowposend := MakePoint(arrowpos.X + ArrowSize, arrowpos.Y);
        ah := ArrowSize * 3 / 2;
        aw := ArrowSize;
      end;
    paRightCenter:
      begin
        arrowpos := MakePoint(rt.X + rt.Width, rt.Y + rt.Height / 2);
        arrowposend := MakePoint(arrowpos.X + ArrowSize, arrowpos.Y);
        ah := ArrowSize * 3 / 2;
        aw := ArrowSize;
      end;
    paRightBottom:
      begin
        arrowpos := MakePoint(rt.X + rt.Width, rt.Y + rt.Height - ah);
        arrowposend := MakePoint(arrowpos.X + ArrowSize, arrowpos.Y);
        ah := ArrowSize * 3 / 2;
        aw := ArrowSize;
      end;
    paTopLeft:
      begin
        arrowpos := MakePoint(rt.X + aw, rt.Y);
        arrowposend := MakePoint(arrowpos.X, arrowpos.Y - ArrowSize);
        aw := ArrowSize * 3 / 2;
        ah := ArrowSize;
      end;
    paTopCenter:
      begin
        arrowpos := MakePoint(rt.X + rt.Width / 2, rt.Y);
        arrowposend := MakePoint(arrowpos.X, arrowpos.Y - ArrowSize);
        aw := ArrowSize * 3 / 2;
        ah := ArrowSize;
      end;
    paTopRight:
      begin
        arrowpos := MakePoint(rt.X + rt.Width - aw, rt.Y);
        arrowposend := MakePoint(arrowpos.X, arrowpos.Y - ArrowSize);
        aw := ArrowSize * 3 / 2;
        ah := ArrowSize;
      end;
    paBottomLeft:
      begin
        arrowpos := MakePoint(rt.X + aw, rt.Y + rt.Height);
        arrowposend := MakePoint(arrowpos.X, arrowpos.Y + ArrowSize);
        aw := ArrowSize * 3 / 2;
        ah := ArrowSize;
      end;
    paBottomCenter:
      begin
        arrowpos := MakePoint(rt.X + rt.Width / 2, rt.Y + rt.Height);
        arrowposend := MakePoint(arrowpos.X, arrowpos.Y + ArrowSize);
        aw := ArrowSize * 3 / 2;
        ah := ArrowSize;
      end;
    paBottomRight:
      begin
        arrowpos := MakePoint(rt.X + rt.Width - aw, rt.Y + rt.Height);
        arrowposend := MakePoint(arrowpos.X, arrowpos.Y + ArrowSize);
        aw := ArrowSize * 3 / 2;
        ah := ArrowSize;
      end;
  end;

  target := arrowposend;
  origin := arrowpos;

  arx := aw;
  ary := ah;
  arrowpts[0].X := target.X;
  arrowpts[0].Y := target.Y;

  X := target.X - origin.X;
  Y := target.Y - origin.Y;
  ht := sqrt(sqr(X) + sqr(Y));

  if ht = 0 then
    ht := 1;

  // quarter?
  if origin.X < target.X then
  begin
    if origin.Y < target.Y then
      quarter := 1
    else
      quarter := 3;
  end
  else
  begin
    if origin.Y < target.Y then
      quarter := 2
    else
      quarter := 4;
  end;

  // calculate the actual P position using the adjustments px and py.
  pX := X * arx / ht;
  pY := Y * ary / ht;
  case quarter of
    1:
      begin
        pt.X := target.X - pX;
        pt.Y := target.Y - pY;
        ar.X := target.X - (X * arx / ht);
        ar.Y := target.Y - (Y * ary / ht);
      end;
    2:
      begin
        pt.X := target.X - pX;
        pt.Y := target.Y - pY;
        ar.X := target.X - (X * arx / ht);
        ar.Y := target.Y - (Y * ary / ht);
      end;
    3:
      begin
        pt.X := target.X - pX;
        pt.Y := target.Y - pY;
        ar.X := target.X - (X * arx / ht);
        ar.Y := target.Y - (Y * ary / ht);
      end;
    4:
      begin
        pt.X := target.X - pX;
        pt.Y := target.Y - pY;
        ar.X := target.X - (X * arx / ht);
        ar.Y := target.Y - (Y * ary / ht);
      end;
  end;

  // calculate pts[1] and pts[2] from the P position to give us the back of the arrow.
  fx := Y * (arx / 2) / ht;
  fy := X * (ary / 2) / ht;
  case quarter of
    1:
      begin
        arrowpts[1].X := pt.X - fx;
        arrowpts[1].Y := pt.Y + fy;
        arrowpts[3].X := pt.X + fx;
        arrowpts[3].Y := pt.Y - fy;
      end;
    2:
      begin
        arrowpts[1].X := pt.X + fx;
        arrowpts[1].Y := pt.Y - fy;
        arrowpts[3].X := pt.X - fx;
        arrowpts[3].Y := pt.Y + fy;
      end;
    3:
      begin
        arrowpts[1].X := pt.X + fx;
        arrowpts[1].Y := pt.Y - fy;
        arrowpts[3].X := pt.X - fx;
        arrowpts[3].Y := pt.Y + fy;
      end;
    4:
      begin
        arrowpts[1].X := pt.X + fx;
        arrowpts[1].Y := pt.Y - fy;
        arrowpts[3].X := pt.X - fx;
        arrowpts[3].Y := pt.Y + fy;
      end;
  end;

  arrowpts[2].X := ar.X;
  arrowpts[2].Y := ar.Y;

  l := rt.X;
  t := rt.Y;
  w := rt.Width;
  h := rt.Height;
  d := radius shl 1;

  pth := TGPGraphicsPath.Create;

  case ArrowPosition of
    paRightBottom, paRightCenter, paRightTop, paLeftTop, paLeftCenter, paLeftBottom:art := MakeRect(origin.X - 1, origin.Y - ah / 2, aw + 2, ah);
    paTopLeft, paTopCenter, paTopRight, paBottomLeft, paBottomCenter, paBottomRight:art := MakeRect(origin.X - aw / 2, origin.Y - 1, aw, ah + 2);
  end;

  pth.AddRectangle(art);
  rgn := TGPRegion.Create(pth);
  g.SetClip(pth, CombineModeExclude);
  pth.Free;

  pth := TGPGraphicsPath.Create;
  pth.AddArc(l, t, d, d, 180, 90); // topleft
  pth.AddLine(l + radius, t, l + w - radius, t); // top
  pth.AddArc(l + w - d, t, d, d, 270, 90); // topright
  pth.AddLine(l + w, t + radius, l + w, t + h - radius); // right
  pth.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  pth.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
  pth.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  pth.AddLine(l, t + h - radius, l, t + radius); // left

  p := TGPPen.Create(MakeColor(Opacity - 25, BorderColor));
  g.DrawPath(p, pth);

  pth.Free;

  g.ResetClip;
  rgn.Free;

  pth := TGPGraphicsPath.Create;
  pth.AddPolygon(PGPPointF(@arrowpts), 4);

  sb := TGPSolidBrush.Create(MakeColor(Opacity - 25, Color));

  g.FillPath(sb, pth);

  pth.Free;
  sb.Free;

  pth := TGPGraphicsPath.Create;

  pth.AddLine(arrowpts[0], arrowpts[1]);
  pth.AddLine(arrowpts[0], arrowpts[3]);

  g.DrawPath(p, pth);

  pth.Free;
  p.Free;

  p := TGPPen.Create(MakeColor(Max(0, Opacity - 125), Color));
  g.DrawLine(p, arrowpts[1].X, arrowpts[1].Y, arrowpts[3].X, arrowpts[3].Y);
  p.Free;

end;

procedure TAdvSmoothPopup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) and (AComponent = FControl) then
    FControl := nil;

  if (AOperation = opRemove) and (AComponent = PictureContainer) then
    PictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = ImageList) then
    ImageList := nil;
end;

procedure TAdvSmoothPopup.PopupAt(X, Y: Integer);
begin
  DoPopup(X, Y, False);
end;

procedure TAdvSmoothPopup.PopupAtControl(Control: TWinControl;
  Direction: TAdvSmoothPopupDirection);
var
  X, Y: Integer;
  p: TPoint;
  r: TRect;
begin
  if Assigned(Control) then
  begin
    if Assigned(Control.Parent) then
    begin
      X := Control.Left;
      Y := Control.Top;

      p := Point(X, Y);
      p := Control.Parent.ClientToScreen(p);
      X := p.X;
      Y := p.Y;
      SystemParametersInfo(SPI_GETWORKAREA, 0,@r,0);

      case Direction of
        pdLeftTop, pdLeftCenter, pdLeftBottom, pdRightTop, pdRightCenter,
          pdRightBottom:
          begin
            case Direction of
              pdLeftTop, pdLeftCenter, pdLeftBottom:
                X := X - GetWidth;
              pdRightTop, pdRightCenter, pdRightBottom:
                X := X + Control.Width;
            end;
            case Direction of
              pdLeftTop, pdRightTop:
                Y := Y + ShadowSize + 5 + Control.Height - GetHeight;
              pdLeftCenter, pdRightCenter:
                Y := Y + (Control.Height div 2) - (GetHeight div 2);
              pdLeftBottom, pdRightBottom:
                Y := Y - ShadowSize - 5;
            end;
          end;
        pdTopLeft, pdTopCenter, pdTopRight, pdBottomLeft, pdBottomCenter,
          pdBottomRight:
          begin
            case Direction of
              pdTopLeft, pdTopCenter, pdTopRight:
                Y := Y - GetHeight;
              pdBottomLeft, pdBottomCenter, pdBottomRight:
                Y := Y + Control.Height;
            end;
            case Direction of
              pdTopLeft, pdBottomLeft:
                X := X + (Control.Width div 2) - GetWidth - 5 + ShadowSize * 2;
              pdTopCenter, pdBottomCenter:
                X := X + (Control.Width div 2) - (GetWidth div 2);
              pdTopRight, pdBottomRight:
                X := X + (Control.Width div 2) + 5 - ShadowSize * 2;
            end;
          end;
      end;

      if AutoPositioning then
      begin

      end;

      case Direction of
        pdLeftTop:
          ArrowPosition := paRightBottom;
        pdLeftCenter:
          ArrowPosition := paRightCenter;
        pdLeftBottom:
          ArrowPosition := paRightTop;
        pdTopLeft:
          ArrowPosition := paBottomRight;
        pdTopCenter:
          ArrowPosition := paBottomCenter;
        pdTopRight:
          ArrowPosition := paBottomLeft;
        pdRightTop:
          ArrowPosition := paLeftBottom;
        pdRightCenter:
          ArrowPosition := paLeftCenter;
        pdRightBottom:
          ArrowPosition := paLeftTop;
        pdBottomLeft:
          ArrowPosition := paTopRight;
        pdBottomCenter:
          ArrowPosition := paTopCenter;
        pdBottomRight:
          ArrowPosition := paTopLeft;
      end;

      DoPopup(X, Y, True);
    end;
  end;
end;

procedure TAdvSmoothPopup.SetCloseOnDeactivate(const Value: Boolean);
begin
  if FCloseOnDeactivate <> Value then
  begin
    FCloseOnDeactivate := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetColorTones(ATones: TColorTones);
begin
  FFlatStyle := True;
  Color := ATones.Background.BrushColor;
  BorderColor := ATones.Background.BorderColor;
  ButtonFont.Color := ATones.Background.TextColor;
  HeaderFont.Color := ATones.Background.TextColor;
  FooterFont.Color := ATones.Background.TextColor;

  ButtonColor := ATones.Background.BrushColor;
  ButtonDisabledColor := ATones.Disabled.BrushColor;
  ButtonDownColor :=  ATones.Selected.BrushColor;
  ButtonHoverColor := ATones.Hover.BrushColor;
end;

procedure TAdvSmoothPopup.SetAnimation(const Value: Boolean);
begin
  if FAnimation <> Value then
  begin
    FAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetAnimationFactor(const Value: Integer);
begin
  if FAnimationFactor <> Value then
  begin
    FAnimationFactor := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetArrowPosition
  (const Value: TAdvSmoothPopupArrowPosition);
begin
  if FArrowPosition <> Value then
  begin
    FArrowPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetArrowSize(const Value: Integer);
begin
  if FArrowSize <> Value then
  begin
    FArrowSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetAutoPositioning(const Value: Boolean);
begin
  if FAutoPositioning <> Value then
  begin
    FAutoPositioning := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtonBorderColor(const Value: TColor);
begin
  if FButtonBorderColor <> Value then
  begin
    FButtonBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtonColor(const Value: TColor);
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtonDisabledColor(const Value: TColor);
begin
  if FButtonDisabledColor <> Value then
  begin
    FButtonDisabledColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtonDownColor(const Value: TColor);
begin
  if FButtonDownColor <> Value then
  begin
    FButtonDownColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtonFont(const Value: TFont);
begin
  if FButtonFont <> Value then
  begin
    FButtonFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtonHoverColor(const Value: TColor);
begin
  if FButtonHoverColor <> Value then
  begin
    FButtonHoverColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetButtons(const Value: TPopupButtons);
begin
  if FButtons <> Value then
  begin
    FButtons.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  FFlatStyle := False;
  case astyle of
    tsOffice2003Blue:
      Color := $00E3B28D;
    tsOffice2003Silver:
      Color := $00927476;
    tsOffice2003Olive:
      Color := $447A63;
    tsOffice2003Classic:
      Color := $00C9D1D5;
    tsOffice2007Luna:
      Color := $00FDEADA;
    tsOffice2007Obsidian:
    begin
      Color := $006E6E6D;
      ButtonFont.Color := clWhite;
      HeaderFont.Color := clWhite;
      FooterFont.Color := clWhite;
    end;
    tsWindowsXP:
      Color := $B9D8DC;
    tsWhidbey:
      Color := $00828F92;
    tsCustom: ;
    tsOffice2007Silver:
      Color := $00E7DCD5;
    tsWindowsVista:
      Color := $FDF8F1;
    tsWindows7:
      Color := $FCEBDC;
    tsTerminal:
      Color := clBtnFace;
    tsOffice2010Blue:
      Color := $F0DAC7;
    tsOffice2010Silver:
      Color := $EDE5E0;
    tsOffice2010Black:
      Color := $919191;
  tsWindows8, tsWindows10:
    begin
      FFlatStyle := True;
      Color := $F7F6F5;
    end;
  tsOffice2013White:
    begin
      FFlatStyle := True;
      Color := clWhite;
    end;
  tsOffice2013LightGray:
    begin
      FFlatStyle := True;
      Color := $F6F6F6;
    end;
  tsOffice2013Gray:
    begin
      FFlatStyle := True;
      Color := $E5E5E5;
    end;
  tsOffice2016White:
    begin
      FFlatStyle := True;
      Color := clWhite;
    end;
  tsOffice2016Gray:
    begin
      FFlatStyle := True;
      Color := $B2B2B2;
    end;
  tsOffice2016Black:
    begin
      FFlatStyle := True;
      Color := $363636;
    end;
  end;


  case AStyle of
      tsWindows8, tsWindows10:
      begin
        ButtonColor := $F7F6F5;
        ButtonHoverColor := $F7EFE8;
        ButtonDownColor := $F7E0C9;
        HeaderFont.Color := clBlack;
        FooterFont.Color := clBlack;
        ButtonFont.Color := clBlack;
      end;
      tsOffice2013White:
      begin
        ButtonColor := clWhite;
        ButtonHoverColor := $FCF0E4;
        ButtonDownColor := $FCE2C8;
        HeaderFont.Color := clBlack;
        FooterFont.Color := clBlack;
        ButtonFont.Color := clBlack;
      end;
      tsOffice2013LightGray:
      begin
        ButtonColor := clWhite;
        ButtonHoverColor := $FCF0E4;
        ButtonDownColor := $FCE2C8;
        HeaderFont.Color := clBlack;
        FooterFont.Color := clBlack;
        ButtonFont.Color := clBlack;
      end;

      tsOffice2013Gray:
      begin
        ButtonColor := $E5E5E5;
        ButtonHoverColor := $FCF0E4;
        ButtonDownColor := $FCE2C8;
        HeaderFont.Color := clBlack;
        FooterFont.Color := clBlack;
        ButtonFont.Color := clBlack;
      end;
      tsOffice2016White:
      begin
        ButtonColor := clWhite;
        ButtonHoverColor := $F2E1D5;
        ButtonDownColor := $E3BDA3;
        HeaderFont.Color := clBlack;
        FooterFont.Color := clBlack;
        ButtonFont.Color := clBlack;
      end;
      tsOffice2016Gray:
      begin
        ButtonColor := $B2B2B2;
        ButtonHoverColor := $F2E1D5;
        ButtonDownColor := $E3BDA3;
        HeaderFont.Color := clBlack;
        FooterFont.Color := clBlack;
        ButtonFont.Color := clBlack;
      end;

      tsOffice2016Black:
      begin
        ButtonColor := $363636;
        ButtonHoverColor := $6A6A6A;
        ButtonDownColor := $444444;
        HeaderFont.Color := $A6A6A6;
        FooterFont.Color := $A6A6A6;
        ButtonFont.Color := $A6A6A6;
      end;
    else
    begin
      ButtonColor := Darker(Color, 40);
      ButtonHoverColor := Lighter(ButtonColor, 20);
      ButtonDisabledColor := clSilver;
      ButtonDownColor := Darker(ButtonColor, 20);
      HeaderFont.Color := Darker(ButtonColor, 40);
      FooterFont.Color := Darker(ButtonColor, 40);
      ButtonFont.Color := Darker(ButtonColor, 40);
    end;
  end;

  ShadowColor := Color;
end;

procedure TAdvSmoothPopup.SetControl(const Value: TControl);
begin
  if FControl <> Value then
  begin
    if Assigned(FControl) and Assigned(FPrevParent) then
    begin
      FControl.Visible := True;
      FControl.Parent := FPrevParent;
      FControl.Align := FPrevAlign;
      FControl.SetBounds(FPrevLeft, FPrevTop, FPrevWidth, FPrevHeight);
    end;
    FControl := Value;
    if Assigned(FControl) then
    begin
      FPrevParent := FControl.Parent;
      FPrevAlign := FControl.Align;
      FPrevLeft := FControl.Left;
      FPrevTop := FControl.Top;
      FPrevWidth := FControl.Width;
      FPrevHeight := FControl.Height;
      FControl.Visible := False;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetDefaultStyle;
begin
  Opacity := 175;
  Color := $2D1606;
  BorderColor := $2D1606;
  ShadowColor := $2D1606;
  ShadowIntensity := 0.9;
  FooterHeight := 35;
  HeaderHeight := 35;
  ShadowSize := 20;
  ArrowSize := 17;
  ButtonColor := $2D1606;
  ButtonHoverColor := $00AB5216;
  ButtonDownColor := $00783910;
  ButtonDisabledColor := clSilver;
  ButtonFont := TFont.Create;
  ButtonFont.Name := 'Tahoma';
  ButtonFont.Color := clWhite;
  HeaderFont.Name := 'Tahoma';
  HeaderFont.Color := clWhite;
  HeaderFont.Size := 12;
  HeaderFont.Style := [fsBold];
  FooterFont.Name := 'Tahoma';
  FooterFont.Color := clWhite;
  FooterFont.Size := 12;
  FooterFont.Style := [fsBold];
end;

procedure TAdvSmoothPopup.SetFooterCaption(const Value: String);
begin
  if FFooterCaption <> Value then
  begin
    FFooterCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetFooterFont(const Value: TFont);
begin
  if FFooterFont <> Value then
  begin
    FFooterFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetFooterHeight(const Value: Integer);
begin
  if FFooterHeight <> Value then
  begin
    FFooterHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetFormStyle(const Value: TFormStyle);
begin
  FFormStyle := Value;
end;

procedure TAdvSmoothPopup.SetGlobalColor(AColor: TColor);
begin
  Color := AColor;
  ButtonColor := Color;
  ButtonHoverColor := Lighter(ButtonColor, 20);
  ButtonDisabledColor := clSilver;
  ButtonDownColor := Darker(ButtonColor, 20);

  ShadowColor := Color;

  ButtonFont.Color := Darker(Color, 40);
  HeaderFont.Color := Darker(Color, 40);
  FooterFont.Color := Darker(Color, 40);
end;

procedure TAdvSmoothPopup.SetHeaderCaption(const Value: String);
begin
  if FHeaderCaption <> Value then
  begin
    FHeaderCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetHeaderFont(const Value: TFont);
begin
  if FHeaderFont <> Value then
  begin
    FHeaderFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetHeaderHeight(const Value: Integer);
begin
  if FHeaderHeight <> Value then
  begin
    FHeaderHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetInnerBorder(const Value: Boolean);
begin
  if FInnerBorder <> Value then
  begin
    FInnerBorder := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetInnerBorderColor(const Value: TColor);
begin
  if FInnerBorderColor <> Value then
  begin
    FInnerBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetInnerBorderWidth(const Value: Integer);
begin
  if FInnerBorderWidth <> Value then
  begin
    FInnerBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetOpacity(const Value: Byte);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetShadowIntensity(const Value: Double);
begin
  if FShadowIntensity <> Value then
  begin
    FShadowIntensity := min(1, max(0, Value));
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetShadowSize(const Value: Integer);
begin
  if FShadowSize <> Value then
  begin
    FShadowSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothPopup.ShowWithAnimation;
begin
  while Assigned(frmC) and Assigned(frm) and (frmC.AlphaBlendValue < 255) do
  begin
    frmC.AlphaBlendValue := min(255, frmC.AlphaBlendValue + (255 - AnimationFactor));
    frm.UpdateMainWindow;
    Sleep(1);
    Application.ProcessMessages;
  end;
end;

{ TAdvSmoothPopupForm }

procedure TAdvSmoothPopupForm.ClearBuffer(Graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := Graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(Graphics) then
    g.Free;
end;

procedure TAdvSmoothPopupForm.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(Popup) and not (csDestroying in ComponentState) then
  begin
    Popup.FHoveredElement := nil;
    Popup.FDownElement := nil;
    UpdateWindow;
  end;
end;

function TAdvSmoothPopupForm.CreateGraphics: TGPGraphics;
begin
  result := nil;
  if Assigned(FMainBuffer) then
    result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothPopupForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothPopupForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
end;

procedure TAdvSmoothPopupForm.CreateWnd;
begin
  inherited;
end;

procedure TAdvSmoothPopupForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothPopupForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
end;

procedure TAdvSmoothPopupForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
end;

procedure TAdvSmoothPopupForm.Draw(Graphics: TGPGraphics);
var
  g: TGPGraphics;
  R: TGPRectF;
  mxs: Integer;
begin
  g := Graphics;
  if not Assigned(g) then
    g := CreateGraphics;

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);

  if Assigned(Popup) then
  begin
    mxs := Max(0, Popup.ArrowSize - Popup.ShadowSize);
    R := MakeRect(mxs, mxs, Width - mxs * 2 - 1, Height - mxs * 2 - 1);
    Popup.DrawPopup(g, R);
    Popup.DrawButtons(g, R);
    Popup.DrawHeaderAndFooter(g);
  end;

  if not Assigned(Graphics) then
    g.Free;
end;

procedure TAdvSmoothPopupForm.Init;
begin
  DoubleBuffered := true;
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := False;
  Color := clWhite;

  CreateMainBuffer;
  SetLayeredWindow;
  UpdateLayered;
end;

procedure TAdvSmoothPopupForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  prev: TPopupButton;
begin
  inherited;
  if Assigned(Popup) then
  begin
    prev := Popup.FDownElement;
    Popup.FDownElement := Popup.ButtonAtXY(X, Y);
    if prev <> Popup.FDownElement then
    begin
      UpdateWindow;
    end;
  end;
end;

procedure TAdvSmoothPopupForm.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  prev: TPopupButton;
begin
  inherited;
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(Popup) then
  begin
    prev := Popup.FHoveredElement;
    Popup.FHoveredElement := Popup.ButtonAtXY(X, Y);
    if prev <> Popup.FHoveredElement then
    begin
      UpdateWindow;
    end;
  end;
end;

procedure TAdvSmoothPopupForm.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (csDestroying in ComponentState) then
    Exit;

  if Assigned(Popup) then
  begin
    if Assigned(Popup.FDownElement) then
    begin
      Popup.DoButtonClick(Popup.FDownElement, Popup.FDownElement.Index);
      if not Popup.FClosing then
      begin
        Popup.FDownElement := nil;
        UpdateWindow;
      end;
    end
    else
    begin
      if PtInGPRect(Popup.FHeaderR, Point(X, Y)) then
      begin
        if Assigned(Popup.OnHeaderClick) then
          Popup.OnHeaderClick(Self);
      end
      else if PtInGPRect(Popup.FFooterR, Point(X, Y)) then
      begin
        if Assigned(Popup.OnFooterClick) then
          Popup.OnFooterClick(Self);
      end

    end;
  end;
end;

procedure TAdvSmoothPopupForm.Paint;
begin
  inherited;
  UpdateWindow;
end;

procedure TAdvSmoothPopupForm.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE)
        or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothPopupForm.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOP, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow;
end;

procedure TAdvSmoothPopupForm.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  size: TSize;
  p, S: TPoint;
  chk: Boolean;
begin
  chk := Assigned(Popup);
  if chk then
    chk := Popup.Animation and Assigned(Popup.frmC);
  ScrDC := CreateCompatibleDC(0);
  MemDC := CreateCompatibleDC(ScrDC);

  FMainBuffer.GetHBITMAP(0, BitmapHandle);
  PrevBitmap := SelectObject(MemDC, BitmapHandle);
  size.cx := Width;
  size.cy := Height;
  p := Point(Left, Top);
  S := Point(0, 0);

  with BlendFunc do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    if chk then
      SourceConstantAlpha := Popup.frmC.AlphaBlendValue
    else
      SourceConstantAlpha := 255;
    AlphaFormat := AC_SRC_ALPHA;
  end;

  UpdateLayeredWindow(Handle, ScrDC, @p, @size, MemDC, @S, 0, @BlendFunc,
    ULW_ALPHA);

  SelectObject(MemDC, PrevBitmap);
  DeleteObject(BitmapHandle);

  DeleteDC(MemDC);
  DeleteDC(ScrDC);
end;

procedure TAdvSmoothPopupForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothPopupForm.WMActivate(var Message: TMessage);
begin
  inherited;
  Message.Result := 1;
end;

procedure TAdvSmoothPopupForm.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  inherited;
end;

procedure TAdvSmoothPopupForm.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TAdvSmoothPopupForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

{ TAdvSmoothPopupPlaceHolder }

procedure TAdvSmoothPopupFormPlaceHolder.Init;
begin
  AlphaBlend := true;
  DoubleBuffered := true;
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := False;
  Color := clWhite;
  KeyPreview := True;
end;

{ TPopupButton }

procedure TPopupButton.Assign(Source: TPersistent);
begin
  if (Source is TPopupButton) then
  begin
    FCaption := (Source as TPopupButton).Caption;
    FEnabled := (Source as TPopupButton).Enabled;
    FImage.Assign((Source as TPopupButton).Image);
    FImageIndex := (Source as TPopupButton).ImageIndex;
    FImageName := (Source as TPopupButton).ImageName;
    Changed;
  end;
end;

procedure TPopupButton.Changed;
begin
  FOwner.Changed;
end;

constructor TPopupButton.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TPopupButtons).FOwner;
  FEnabled := true;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageIndex := -1;
  FPosition := bpTopLeft;
  FVisible := true;
  FTag := 0;
  FOwner.Changed;
end;

destructor TPopupButton.Destroy;
begin
  FImage.Free;
  inherited;
  FOwner.Changed;
end;

procedure TPopupButton.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TPopupButton.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TPopupButton.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TPopupButton.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> Value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TPopupButton.SetImageIndex(const Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TPopupButton.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

procedure TPopupButton.SetPosition(const Value: TPopupButtonPosition);
begin
  if FPosition <> Value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TPopupButton.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TPopupButtons }

function TPopupButtons.Add: TPopupButton;
begin
  result := TPopupButton( inherited Add);
end;

procedure TPopupButtons.Clear;
begin
  if Count > 0 then
  begin
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  end;
end;

constructor TPopupButtons.Create(AOwner: TAdvSmoothPopup);
begin
  inherited Create(TPopupButton);
  FOwner := AOwner;
end;

procedure TPopupButtons.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TPopupButtons.GetItem(Index: Integer): TPopupButton;
begin
  result := TPopupButton( inherited Items[Index]);
end;

function TPopupButtons.GetOwner: TPersistent;
begin
  result := FOwner;
end;

function TPopupButtons.Insert(Index: Integer): TPopupButton;
begin
  result := TPopupButton( inherited Insert(Index));
end;

procedure TPopupButtons.SetItem(Index: Integer; const Value: TPopupButton);
begin
  inherited Items[Index] := Value;
end;


procedure TAdvSmoothPopupFormPlaceHolder.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited;

end;

procedure TAdvSmoothPopupFormPlaceHolder.KeyUp(var Key: Word;
  Shift: TShiftState);
var
  close: Boolean;
begin
  inherited;
  if Assigned(FOwner) then
  begin
    close := (Key = VK_ESCAPE);
    if close then
      Fowner.ClosePopup;
  end;
end;

end.
