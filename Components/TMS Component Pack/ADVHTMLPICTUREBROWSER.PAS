{***************************************************************************}
{ TAdvHTMLPictureBrowser component                                          }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}
unit AdvHTMLPictureBrowser;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Types, Controls, Graphics, ExtCtrls,
  Menus, Math, PictureContainer, ImgList, GDIPicture
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TAdvHTMLPictureBrowser = class;

  TDescPlacement = (dpAbovePicture, dpBelowPicture);
  TNavigationType = (navText, navImages, navBullets);
  TAnchorClick = procedure(Sender: TObject; Text: string) of object;
  TNavImgType = (first, last, next, previous, bullet, selected);
  TPictureChangedEvent = procedure(Sender: TObject; AItemIndex: integer) of object;

  THTMLPictureItem = class(TCollectionItem)
  private
    { Private declarations }
    FHTMLDescription: TStringList;
    FPicture: TGDIPPicture;
    FOwner: TAdvHTMLPictureBrowser;
    procedure SetGDIPPicture(const Value: TGDIPPicture);
    procedure SetHTMLDescription(const Value: TStringList);
  protected
    { Protected declarations }
    procedure HTMLChanged(Sender: TObject);
    function GetDisplayName: string; override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Picture: TGDIPPicture read FPicture write SeTGDIPPicture;
    property HTMLDescription: TStringList read FHTMLDescription write SetHTMLDescription;
  end;

  THTMLPictureList = class(TOwnedCollection)
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    { Protected declarations }
    function GetItem(Index: Integer): THTMLPictureItem;
    procedure SetItem(Index: Integer; Value: THTMLPictureItem);
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(AOwner: TAdvHTMLPictureBrowser);
    destructor Destroy; override;
    function Add: THTMLPictureItem;
    function Insert(Index: Integer): THTMLPictureItem;
    property Items[Index: Integer]: THTMLPictureItem read GetItem write SetItem; default;
  published
    { Published declarations }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TNavigationBarSettings = class(TPersistent)
  private
    { Private declarations }
    FContainer: TPictureContainer;
    FImageLastIndex: TImageIndex;
    FImages: TCustomImageList;
    FImageSelectedBulletIndex: TImageIndex;
    FImagePreviousIndex: TImageIndex;
    FImageFirstIndex: TImageIndex;
    FImageBulletIndex: TImageIndex;
    FImageNextIndex: TImageIndex;
    FMaxBullets: Integer;
    FNameImageLast: string;
    FNameImageSelectedBullet: string;
    FNameImagePrevious: string;
    FNameImageFirst: string;
    FNameImageBullet: string;
    FNameImageNext: string;
    FNavBarHTML: string;
    FNavType: TNavigationType;
    FOwner: TAdvHTMLPictureBrowser;
    FPictureContainer: TPictureContainer;
    FShowFirst: Boolean;
    FShowLast: Boolean;
    FShowNext: Boolean;
    FShowPrevious: Boolean;
    FOnUpdate: TNotifyEvent;
    FBorderColor: TColor;
    FColor: TColor;
    FColorTo: TColor;
    procedure AddIndexLinkToHTML(Index: Integer);
    procedure AddImageToHTML(imgType: TNavImgType);
    procedure AddImageLinkToHTML(Href: string; imgType: TNavImgType);
    procedure AddResourceToContainer(Container: TPictureContainer; resName, itemName: string);
    procedure AddLinkToHTML(Href, Text: string);
    procedure AddToHTML(Text: string);
    function GetImageSource(imgType: TNavImgType): string;
    procedure SetImageBulletIndex(const Value: TImageIndex);
    procedure SetImageFirstIndex(const Value: TImageIndex);
    procedure SetImageLastIndex(const Value: TImageIndex);
    procedure SetImageNextIndex(const Value: TImageIndex);
    procedure SetImagePreviousIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageSelectedBulletIndex(const Value: TImageIndex);
    procedure SetMaxBullets(const Value: Integer);
    procedure SetNameImageLast(const Value: string);
    procedure SetNameImageSelectedBullet(const Value: string);
    procedure SetNameImagePrevious(const Value: string);
    procedure SetNameImageFirst(const Value: string);
    procedure SetNameImageBullet(const Value: string);
    procedure SetNameImageNext(const Value: string);
    procedure SetNavType(const Value: TNavigationType);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetShowFirst(const Value: Boolean);
    procedure SetShowLast(const Value: Boolean);
    procedure SetShowNext(const Value: Boolean);
    procedure SetShowPrevious(const Value: Boolean);
    procedure SetOnUpdate(const Value: TNotifyEvent);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
  protected
    { Protected declarations }
    function SetNavBarHTML(Index, MaxIndex: Integer): string;
    property NavBarHTML: string read FNavBarHTML;
    property OnUpdate: TNotifyEvent read FOnUpdate write SetOnUpdate;
  public
    { Public declarations }
    constructor Create(AOwner: TAdvHTMLPictureBrowser);
    destructor Destroy; override;
  published
    { Published declarations }
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clSilver;
    property ImageBulletIndex: TImageIndex read FImageBulletIndex write SetImageBulletIndex default 0;
    property ImageFirstIndex: TImageIndex read FImageFirstIndex write SetImageFirstIndex default 0;
    property ImageLastIndex: TImageIndex read FImageLastIndex write SetImageLastIndex default 0;
    property ImageNextIndex: TImageIndex read FImageNextIndex write SetImageNextIndex default 0;
    property ImagePreviousIndex: TImageIndex read FImagePreviousIndex write SetImagePreviousIndex default 0;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageSelectedBulletIndex: TImageIndex read FImageSelectedBulletIndex write SetImageSelectedBulletIndex default 0;
    property MaxBullets: Integer read FMaxBullets write SetMaxBullets default 5;
    property NameImageLast: string read FNameImageLast write SetNameImageLast;
    property NameImageSelectedBullet: string read FNameImageSelectedBullet write SetNameImageSelectedBullet;
    property NameImagePrevious: string read FNameImagePrevious write SetNameImagePrevious;
    property NameImageFirst: string read FNameImageFirst write SetNameImageFirst;
    property NameImageBullet: string read FNameImageBullet write SetNameImageBullet;
    property NameImageNext: string read FNameImageNext write SetNameImageNext;
    property NavigationType: TNavigationType read FNavType write SetNavType default navBullets;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
    property ShowFirst: Boolean read FShowFirst write SetShowFirst default True;
    property ShowLast: Boolean read FShowLast write SetShowLast default True;
    property ShowNext: Boolean read FShowNext write SetShowNext default True;
    property ShowPrevious: Boolean read FShowPrevious write SetShowPrevious default True;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvHTMLPictureBrowser = class(TCustomControl)
  private
    { Private declarations }
    FAspectRatio: Boolean;
    FBorderColor: TColor;
    FButtonNextImage: TGDIPPicture;
    FButtonNextRect: TRect;
    FButtonPreviousImage: TGDIPPicture;
    FButtonPreviousRect: TRect;
    FCenter: Boolean;
    FColor: TColor;
    FCropping: Boolean;
    FDescriptionBackgroundColor: TColor;
    FDescriptionBorderColor: TColor;
    FDescriptionPlacement: TDescPlacement;
    FDescriptionHover: Boolean;
    FDescriptionHoverDelay: Integer;
    FGraceTimer: TTimer;
    FHoveringOverPicture: Boolean;
    FHoverTimer: TTimer;
    FHTMLRect: TRect;
    FImageBackgroundColor: TColor;
    FImageBorderColor: TColor;
    FImageBoundsRect: TRect;
    FImageContentsRect: TRect;
    FImages: TCustomImageList;
    FInnerMargin: Integer;
    FItemIndex: Integer;
    FNavigation: TNavigationBarSettings;
    FNavigationRect: TRect;
    FNavTextRect: TRect;
    FOnAnchorclick: TAnchorClick;
    FOuterMargins: TMargins;
    FPageSize: Integer;
    FPictureContainer: TPictureContainer;
    FPictureMargins: TMargins;
    FPicturePopUp: TPopupMenu;
    FPictures: THTMLPictureList;
    FShowDesc: Boolean;
    FStretch: Boolean;
    FSwipeEnabled: Boolean;
    FSwipeStarted: Boolean;
    FSwipeXStart: Integer;
    FSwipeXStop: Integer;
    FTimedThisHover: Boolean;
    FWordWrap: Boolean;
    FImageBackgroundColorTo: TColor;
    FColorTo: TColor;
    FDescriptionBackgroundColorTo: TColor;
    FDescriptionHoverGraceTime: Integer;
    FDescriptionHeight: Integer;
    FOnPictureChanged: TPictureChangedEvent;
    // WM
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    //CM
    procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
    //normal procs/functions
    procedure AddResourceToPictures(resName, HTML:string);
    procedure Changed;
    function DesignTime: Boolean;
    function GetVersion: string;
    function GetVersionNr: Integer;
    procedure HoverTimerTick(Sender: TObject);
    procedure SetDescriptionPlacement(const Value: TDescPlacement);
    procedure SetItemIndex(const Value: Integer);
    procedure SeTNavigationBarSettings(const Value: TNavigationBarSettings);
    procedure SetPictures(const Value: THTMLPictureList);
    procedure SetVersion(const Value: string);
    procedure SetOuterMargins(const Value: TMargins);
    procedure SetAspectRatio(const Value: Boolean);
    procedure SetCenter(const Value: Boolean);
    procedure SetCropping(const Value: Boolean);
    procedure SetStretch(const Value: Boolean);
    procedure SetDescriptionHoverDelay(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetPictureMargins(const Value: TMargins);
    procedure SetInnerMargin(const Value: Integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetDescriptionBackgroundColor(const Value: TColor);
    procedure SetDescriptionBackgroundColorTo(const Value: TColor);
    procedure SetDescriptionBorderColor(const Value: TColor);
    procedure SetImageBackgroundColor(const Value: TColor);
    procedure SetImageBackgroundColorTo(const Value: TColor);
    procedure SetImageBorderColor(const Value: TColor);
    procedure SetSwipeEnabled(const Value: Boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetPictureContainer(const Value: TPictureContainer);
    procedure SetButtonNextImage(const Value: TGDIPPicture);
    procedure SetButtonPreviousImage(const Value: TGDIPPicture);
    procedure SetDescriptionHover(const Value: Boolean);
    procedure SetColorTo(const Value: TColor);
    procedure Swipe;
    procedure SetDescriptionHoverGraceTime(const Value: Integer);
    procedure SetDescriptionHeight(const Value: Integer);
  protected

    { Protected declarations }
    FInternalX, FInternalY: Integer;
    procedure PaintHTML;
    procedure PaintRect(BgColor, BgColorTo, LineColor: TColor; r: TRect);
    procedure Resize; override;
    procedure PicturesChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);
    procedure NavigationUpdated(Sender: TObject);
    procedure NavigateByLink(Text: string);
    procedure CalculateImageRect;
    procedure CalculateButtonRects;
    procedure CalculateHTMLRect;
    procedure CalculateNavRect;
    function GetSelectedImage: THTMLPictureItem;
    procedure Paint; override;
    function HoveringOverDescription(X, Y: Integer): Boolean;
    function HoveringOverPicture(X, Y: Integer): Boolean;
    function HoveringOverNext(X, Y: Integer): Boolean;
    function HoveringOverPrevious(X, Y: Integer): Boolean;
    procedure Loaded; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    class procedure GetAspectSize(var x, y, w, h: Integer; ow, oh, nw, nh: Integer; asr, str, cro: Boolean);
    function AnchorTextAt(X, Y: Integer): string;
    function NavAnchorTextAt(X, Y: Integer): string;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure First;
    procedure Last;
    procedure Next;
    procedure Previous;
    procedure MoveTo(Index: Integer);
    procedure Increase(Step: Integer);
    procedure Decrease(Step: Integer);
  published
    { Published declarations }
    property Align;
    property Anchors;
    property AspectRatio: Boolean read FAspectRatio write SetAspectRatio default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property ButtonNextImage: TGDIPPicture read FButtonNextImage write SetButtonNextImage;
    property ButtonPreviousImage: TGDIPPicture read FButtonPreviousImage write SetButtonPreviousImage;
    property Center: Boolean read FCenter write SetCenter default True;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property Constraints;
    property Cropping: Boolean read FCropping write SetCropping default False;
    property DescriptionBackgroundColor: TColor read FDescriptionBackgroundColor write SetDescriptionBackgroundColor default $00F3F3F3;
    property DescriptionBackgroundColorTo: TColor read FDescriptionBackgroundColorTo write SetDescriptionBackgroundColorTo default clNone;
    property DescriptionBorderColor: TColor read FDescriptionBorderColor write SetDescriptionBorderColor default clSilver;
    property DescriptionHeight: Integer read FDescriptionHeight write SetDescriptionHeight default 70;
    property DescriptionHover: Boolean read FDescriptionHover write SetDescriptionHover default false;
    property DescriptionHoverDelay: Integer read FDescriptionHoverDelay write SetDescriptionHoverDelay default 100;
    property DescriptionHoverGraceTime: Integer read FDescriptionHoverGraceTime write SetDescriptionHoverGraceTime default 500;
    property DescriptionPlacement: TDescPlacement read FDescriptionPlacement write SetDescriptionPlacement default dpBelowPicture;
    property DoubleBuffered default True;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height default 325;
    property ImageBackgroundColor: TColor read FImageBackgroundColor write SetImageBackgroundColor default $00FBEEE3;
    property ImageBackgroundColorTo: TColor read FImageBackgroundColorTo write SetImageBackgroundColorTo default clNone;
    property ImageBorderColor: TColor read FImageBorderColor write SetImageBorderColor default clSilver;
    property Images: TCustomImageList read FImages write SetImages;
    property InnerMargin: Integer read FInnerMargin write SetInnerMargin default 5;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default 0;
    property Navigation: TNavigationBarSettings read FNavigation write SetNavigationBarSettings;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property OuterMargins: TMargins read FOuterMargins write SetOuterMargins;
    property PageSize: Integer read FPageSize write FPageSize default 3;
    property PictureContainer: TPictureContainer read FPictureContainer write SetPictureContainer;
    property PicturePopUpMenu: TPopupMenu read FPicturePopUp write FPicturePopup;
    property Pictures: THTMLPictureList read FPictures write SetPictures;
    property PictureMargins: TMargins read FPictureMargins write SetPictureMargins;
    property PopupMenu;
    property ShowHint;
    property SwipeEnabled: Boolean read FSwipeEnabled write SetSwipeEnabled default True;
    property Tag;
    property TabOrder;
    property TabStop default True;
    property Top;
    property Touch;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property Width default 225;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;

    property onAnchorClick: TAnchorClick read FOnAnchorclick write FOnAnchorClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPictureChanged: TPictureChangedEvent read FOnPictureChanged write FOnPictureChanged;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  ComObj, Commctrl, ShellApi;

const
  NAVHEIGHT = 30;

{$R AdvHTMLPictureBrowser.res}
{$I HTMLEngo.pas}

{ TAdvHTMLPictureBrowser }

class procedure TAdvHTMLPictureBrowser.GetAspectSize(var x, y, w, h: Integer; ow, oh, nw, nh: Integer; asr, str, cro: Boolean);
var
  arc, ar: Single;
begin
  if asr then
  begin
    if (ow > 0) and (oh > 0) and (nw > 0) and (nh > 0) then
    begin
      if (ow < nw) and (oh < nh) and (not str) then
      begin
        w := ow;
        h := oh;
      end
      else
      begin
        if ow / oh < nw / nh then
        begin
          h := nh;
          w := Trunc(nh * ow / oh);
        end
        else
        begin
          w := nw;
          h := Trunc(nw * oh / ow);
        end;
      end;
    end
    else
    begin
      w := 0;
      h := 0;
    end;
  end
  else
  begin
    if str then
    begin
      w := nw;
      h := nh;
    end
    else
    begin
      w := ow;
      h := oh;

      if cro then
      begin
        if (w >= h) and (w > 0) then
        begin
          h := Trunc(nw / w * h);
          w := nw;
        end
        else
        if (h >= w) and (h > 0) then
        begin
          w := Trunc(nh / h * w);
          h := nh;
        end;

        if h = 0 then
          ar := 1
        else
          ar := w / h;

        if nh = 0 then
          arc := 1
        else
          arc := nw / nh;

        if (ar < 1) or (arc > ar) then
        begin
          h := Trunc(nw / ar);
          w := nw;
        end
        else
        begin
          w := Trunc(ar * nh);
          h := nh;
        end;
      end;
    end;
  end;
end;

function TAdvHTMLPictureBrowser.GetSelectedImage: THTMLPictureItem;
begin
  Result := nil;
  if (ItemIndex >= 0) and (ItemIndex <= FPictures.Count -1) then
    Result := FPictures[ItemIndex];
end;

procedure TAdvHTMLPictureBrowser.Increase(Step: Integer);
begin
  MoveTo(FItemIndex + Step);
end;

procedure TAdvHTMLPictureBrowser.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  case Key of
    vk_Left: Previous;
    vk_Up: Previous;
    vk_Right: Next;
    vk_Down: Next;
    vk_Prior: Decrease(FPageSize);
    vk_Next: Increase(FPageSize);
    vk_Home: First;
    vk_End: Last;
  end;
end;

procedure TAdvHTMLPictureBrowser.AddResourceToPictures(resName, HTML: string);
var
  item: THTMLPictureItem;
begin
  item := FPictures.Add;
  item.Picture.LoadFromResourceName(HInstance, resName);
  item.HTMLDescription.Add(HTML);
end;

function TAdvHTMLPictureBrowser.AnchorTextAt(X, Y: Integer): string;
var
  xs, ys, HyperLinks, MouseLink: Integer;
  a, s, str, focusanchor: string;
  res: boolean;
  img: THTMLPictureItem;
  r: TRect;
begin
  Result := '';
  res := false;
  a := '';
  img := GetSelectedImage;
  if (FHTMLRect.Top < FHTMLRect.Bottom) and (img <> nil) then
  begin
    str := img.FHTMLDescription.Text;

    res := HTMLDrawEx(Canvas, str, FHTMLRect, FImages, X, Y, -1, -1, 1, True, False, False, False, False, False, FWordWrap, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys, HyperLinks, MouseLink, r, nil, FPictureContainer, 0);
  end;

  if res then
    Result := a;
end;

procedure TAdvHTMLPictureBrowser.CalculateButtonRects;
var
  imgrect, maximgrect: TRect;
  x, y, w, h, iw, ih: Integer;
  pic: TGDIPPicture;
  pm: TMargins;
begin
  if Assigned(FButtonNextImage) and Assigned(FButtonPreviousImage) then
  begin
    pic := FButtonPreviousImage;
    pic.GetImageSizes;
    pm := FPictureMargins;
    imgrect := FImageContentsRect;
    maximgrect := FImageBoundsRect;

    imgrect.Left := Max(imgrect.Left, maximgrect.Left);
    imgrect.Right := Min(imgrect.Right, maximgrect.Right);
    imgrect.Top := Max(imgrect.Top, maximgrect.Top);
    imgrect.Bottom := Min(imgrect.Bottom, maximgrect.Bottom);

    iw := imgrect.Right - imgrect.Left - pm.Left - pm.Right;
    ih := imgrect.Bottom - imgrect.Top - pm.Top - pm.Bottom;


    x := imgrect.Left;
    y := imgrect.Top + Trunc((ih - pic.Height) / 2) + (pm.Top + pm.Bottom) div 2;
    w := Min(pic.Width, iw);
    h := Min(pic.Height, ih);
    FButtonPreviousRect := Bounds(x, y, w, h);

    pic := FButtonNextImage;
    pic.GetImageSizes;
    x := Min(maximgrect.Right, imgrect.Right - pic.Width);
    y := Max(maximgrect.Top, imgrect.Top + Trunc((ih - pic.Height) / 2) + (pm.Top + pm.Bottom) div 2);
    w := Min(pic.Width, iw);
    h := Min(pic.Height, ih);
    FButtonNextRect := Bounds(x, y, w, h);
  end;
end;

procedure TAdvHTMLPictureBrowser.CalculateHTMLRect;
var
  navr, r: TRect;
  om: TMargins;
begin
  om := OuterMargins;
  navr := FNavigationRect;
//  imgr := FImageContentsRect;
  r.Left := ClientRect.Left + om.Left;
  r.Right := ClientRect.Right - om.Right;

  case FDescriptionPlacement of
    dpAbovePicture:
    begin
      r.Top := ClientRect.Top + om.Top;
      r.Bottom := r.Top + FDescriptionHeight;
    end;
    dpBelowPicture:
    begin
      r.Bottom := navr.Top - FInnerMargin;
      r.Top := r.Bottom - FDescriptionHeight;
    end;
  end;

  FHTMLRect := r;
end;

procedure TAdvHTMLPictureBrowser.CalculateImageRect;
var
  r, rimg, navr, descr: TRect;
  om, pm: TMargins;
  img: THTMLPictureItem;
  x, y, w, h: Integer;
  pic: TGDIPPicture;
  rdest: TRect;
  imgw, imgh: Integer;
begin
  r := ClientRect;
  navr := FNavigationRect;
  descr := FHTMLRect;
  om := OuterMargins;
  rimg.Left := r.Left + om.Left;
  rimg.Right := r.Right - om.Right;
  if not FShowDesc then
  begin
    rimg.Top := r.Top + om.Top;
    rimg.Bottom := navr.Top - FInnerMargin;
  end
  else
  begin
    if FDescriptionPlacement = dpAbovePicture then
    begin
      rimg.Bottom := navr.Top -FInnerMargin;
      rimg.Top := descr.Bottom + FInnerMargin;
    end
    else
    begin
      rimg.Bottom := descr.Top - FInnerMargin;
      rimg.Top := r.Top + om.Top;
    end;
  end;

  FImageBoundsRect := rimg;
  FImageContentsRect := rimg;
  img := GetSelectedImage;
  if Assigned(img) and Assigned(img.Picture) then
  begin
    pm := FPictureMargins;
    imgw := rimg.Right - pm.Right - rimg.Left - pm.Left;
    imgh := rimg.Bottom - pm.Bottom - rimg.Top - pm.Top;
    pic := img.Picture;
    pic.GetImageSizes;
    x := 0;
    y := 0;
    w := 0;
    h := 0;
    GetAspectSize(x, y, w, h, pic.Width, pic.Height, imgw, imgh, AspectRatio, Stretch, Cropping);
//    rsrc := Bounds(0, 0, pic.Width, pic.Height);
    if Center or Cropping then
    begin
      x := (imgw - w) div 2;
      y := (imgh - h) div 2;
    end;

    x := x + rimg.Left + pm.Left;
    y := y + rimg.Top + pm.Top;

    rdest := Rect(x, y, x + w, y + h);
    FImageContentsRect := rdest;
    CalculateButtonRects;
  end;
end;

procedure TAdvHTMLPictureBrowser.CalculateNavRect;
var
  om: TMargins;
  left, top, right, bottom, xs, ys: Integer;
  HyperLinks, MouseLink: Integer;
  a, s, focusanchor, str: string;
  r: TRect;
  pc: TPictureContainer;
begin
  om := FOuterMargins;
  left := ClientRect.Left + om.Left;
  right := ClientRect.Right - om.Right;
  bottom := ClientRect.Bottom - om.Bottom;
  top:= bottom - NAVHEIGHT;
  FNavigationRect := Rect(left, top, right, bottom);
  str := FNavigation.NavBarHTML;
  if Assigned(FNavigation.PictureContainer) then
    pc := FNavigation.PictureContainer
  else
    pc:= FNavigation.FContainer; 
  HTMLDrawEx(Canvas, str, FNavigationRect, FNavigation.Images, -1, -1, -1, -1, -1, True, False, False, False, False, False,
               FWordWrap ,1.0, clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys , HyperLinks, MouseLink,
               r, nil, pc, 0);
  FNavTextRect.Left := left + Trunc((right - left - xs) / 2);
  FNavTextRect.Right := FNavTextRect.Left + xs;
  FNavTextRect.Top := top + Trunc((bottom - top - ys) /2);
  FNavTextRect.Bottom := FNavTextRect.Top + ys;
end;

procedure TAdvHTMLPictureBrowser.Changed;
begin
  CalculateNavRect;
  CalculateHTMLRect;
  CalculateImageRect;
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.CMMouseEnter(var msg: TMessage);
begin
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.CMMouseLeave(var msg: TMessage);
begin
  FHoveringOverPicture := False;
  if FDescriptionHover and (FDescriptionHoverDelay > 0) then
  begin
    FShowDesc := False;
    Changed;
    Invalidate;
  end;
end;

constructor TAdvHTMLPictureBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDescriptionPlacement := dpBelowPicture;
  DoubleBuffered := True;
  FDescriptionHeight := 70;
  FPictures := THTMLPictureList.Create(Self);
  FOuterMargins := TMargins.Create(Self);
  FPictureMargins := TMargins.Create(Self);
  FOuterMargins.OnChange := MarginsChanged;
  FPictureMargins.OnChange := MarginsChanged;
  FCenter := True;
  FStretch := False;
  FAspectRatio := True;
  FCropping := False;
  FHoveringOverPicture := False;
  FShowDesc := true;
  FDescriptionHover := False;
  FDescriptionHoverDelay := 100;
  FHoverTimer := TTimer.Create(Self);
  FHoverTimer.Interval := 0;
  FHoverTimer.OnTimer := HoverTimerTick;
  FHoverTimer.Enabled := false;
  FTimedThisHover := False;
  FDescriptionHoverGraceTime := 500;
  FGraceTimer := TTimer.Create(Self);
  FGraceTimer.Enabled := false;
  FGraceTimer.Interval := 500;
  FGraceTimer.OnTimer := HoverTimerTick;
  FInternalX := 0;
  FInternalY := 0;
  FInnerMargin := 5;
  FBorderColor := clSilver;
  FColor := clWhite;
  FColorTo := clNone;
  FImageBackgroundColor := $00FBEEE3;
  FImageBackgroundColorTo := clNone;
  FImageBorderColor := clSilver;
  FDescriptionBackgroundColor := $00F3F3F3;
  FDescriptionBackgroundColorTo := clNone;
  FDescriptionBorderColor := clSilver;
  FPageSize := 3;
  FWordWrap := True;
  FSwipeStarted := False;
  FSwipeXStart := 0;
  FSwipeXStop := 0;
  FSwipeEnabled := true;

  FNavigation := TNavigationBarSettings.Create(Self);
  FNavigation.OnUpdate := NavigationUpdated;
  FButtonNextImage := TGDIPPicture.Create;
  FButtonPreviousImage := TGDIPPicture.Create;
  begin
    AddResourceToPictures('TMSDEFAULTPIC1', 'Navigate by using the Arrows, clicking the buttons in the picture or using the navigation tab below.<br>Click <a href="http://www.tmssoftware.com">this</a> link to trigger OnAnchorClick');
    AddResourceToPictures('TMSDEFAULTPIC2', 'You don''t have to put hyperlinks in the href part, <a href="Yes, any text will go">any</a> text will do');
    AddResourceToPictures('TMSDEFAULTPIC3', 'Assign a TPictureContainer to PictureContainer or a TImagelist to Images to use images in the description');
    FItemIndex := 0;
    FNavigation.SetNavBarHTML(0, 2);
    FButtonNextImage.LoadFromResourceName(HInstance, 'TMSPICNEXT');
    FButtonPreviousImage.LoadFromResourceName(HInstance, 'TMSPICPREVIOUS');
  end;

  Height := 325;
  Width := 225;
  TabStop := True;

  FPictures.OnChange := PicturesChanged;
end;

procedure TAdvHTMLPictureBrowser.Decrease(Step: Integer);
begin
  MoveTo(FItemIndex - Step);
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionHeight(const Value: Integer);
begin
  if (FDescriptionHeight <> Value) and (Value > 0) then
  begin
    FDescriptionHeight := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionHover(const Value: Boolean);
begin
  if FDescriptionHover <> Value then
  begin
    FDescriptionHover := Value;
    FDescriptionHoverDelay := FDescriptionHoverDelay + 1;
    SetDescriptionHoverDelay(FDescriptionHoverDelay - 1);
  end;
end;

function TAdvHTMLPictureBrowser.DesignTime: Boolean;
begin
  Result := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

destructor TAdvHTMLPictureBrowser.Destroy;
begin
  FGraceTimer.Free;
  FHoverTimer.Free;
  FOuterMargins.Free;
  FPictureMargins.Free;
  FButtonNextImage.Free;
  FButtonPreviousImage.Free;
  FPictures.Free;
//  FNavigation.FContainer.Free;
  FNavigation.Free;
  inherited;
end;

function TAdvHTMLPictureBrowser.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvHTMLPictureBrowser.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvHTMLPictureBrowser.HoveringOverPicture(X, Y: Integer): Boolean;
var
  xmin, xmax, ymin, ymax: Integer;
begin
  xmin := Max(FImageBoundsRect.Left, FImageContentsRect.Left);
  xmax := Min(FImageBoundsRect.Right, FImageContentsRect.Right);
  ymin := Max(FImageBoundsRect.Top, FImageContentsRect.Top);
  ymax := Min(FImageBoundsRect.Bottom, FImageContentsRect.Bottom);
  Result := (X >= xmin) and (X <= xmax) and (Y >= ymin) and (Y <= ymax);
end;

function TAdvHTMLPictureBrowser.HoveringOverNext(X, Y: Integer): Boolean;
var
  xmin, xmax, ymin, ymax: Integer;
begin
  xmin := FButtonNextRect.Left;
  xmax := FButtonNextRect.Right;
  ymin := FButtonNextRect.Top;
  ymax := FButtonNextRect.Bottom;
  Result := (X >= xmin) and (X <= xmax) and (Y >= ymin) and (Y <= ymax);
end;

function TAdvHTMLPictureBrowser.HoveringOverPrevious(X, Y: Integer): Boolean;
var
  xmin, xmax, ymin, ymax: Integer;
begin
  xmin := FButtonPreviousRect.Left;
  xmax := FButtonPreviousRect.Right;
  ymin := FButtonPreviousRect.Top;
  ymax := FButtonPreviousRect.Bottom;
  Result := (X >= xmin) and (X <= xmax) and (Y >= ymin) and (Y <= ymax);
end;

function TAdvHTMLPictureBrowser.HoveringOverDescription(X, Y: Integer): Boolean;
var
  xmin, xmax, ymin, ymax: Integer;
begin
  xmin := FHTMLRect.Left;
  xmax := FHTMLRect.Right;
  ymin := FHTMLRect.Top;
  ymax := FHTMLRect.Bottom;

  Result := (X >= xmin) and (X <= xmax) and (Y >= ymin) and (Y <= ymax);
end;

procedure TAdvHTMLPictureBrowser.HoverTimerTick(Sender: TObject);
begin
  (Sender as TTimer).Enabled := False;
  if HoveringOverPicture(FInternalX, FInternalY) or HoveringOverDescription(FInternalX, FInternalY) then
  begin
    FShowDesc := True;
    FTimedThisHover := False;
    Changed;
  end
  else
  begin
    FShowDesc := false;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.Loaded;
begin
  inherited;
  FNavigation.SetNavBarHTML(FItemIndex, FPictures.Count - 1);
  Changed;
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.MarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvHTMLPictureBrowser.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Focused then
    SetFocus;
  FInternalX := X;
  FInternalY := Y;
  if HoveringOverPicture(X, Y) and FSwipeEnabled then
  begin
    if (ssLeft in Shift) or (ssTouch in Shift) then
    begin
      if not FSwipeStarted then
        FSwipeXStart := X;
      FSwipeStarted := True;
      FSwipeXStop := X;
    end;
  end;
  if AnchorTextAt(X, Y) <> '' then
  begin
    if Assigned(FOnAnchorclick) then
      FOnAnchorclick(Self, AnchorTextAt(X, Y));
  end
  else
  if NavAnchorTextAt(X, Y) <> '' then
    NavigateByLink(NavAnchorTextAt(X, Y))
  else
  if Assigned(FPicturePopUp) and HoveringOverPicture(X, Y) then
  begin
    if Button = mbRight then
      FPicturePopUp.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
  if HoveringOverNext(X, Y) then
    Next
  else
  begin
    if HoveringOverPrevious(X, Y) then
      Previous;
  end;
end;

procedure TAdvHTMLPictureBrowser.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not ((ssLeft in Shift) or (ssTouch in Shift)) and FSwipeStarted then
      Swipe;
  FInternalX := X;
  FInternalY := Y;
  if ((ssLeft in Shift) or (ssTouch in Shift)) and FSwipeEnabled then
    begin
      if (not FSwipeStarted) and HoveringOverPicture(X, Y) then
      begin
        FSwipeXStart := X;
        FSwipeStarted := True;
      end;
      FSwipeXStop := X;
    end;
  if FHoveringOverPicture <> HoveringOverPicture(X, Y) then
  begin
    FHoveringOverPicture := HoveringOverPicture(X, Y);
    Invalidate;
  end;
  if (FDescriptionHoverDelay > 0) and FDescriptionHover then
  begin
    if HoveringOverPicture(X, Y) then
    begin
      if not(FHoverTimer.Enabled) and not FTimedThisHover then
        FHoverTimer.Enabled := True;
    end
    else
    begin
      if FShowDesc then
        FGraceTimer.Enabled := True
      else
      begin
        FShowDesc := HoveringOverDescription(X, Y) or HoveringOverPicture(X, Y);
        Changed;
        Invalidate;
      end;
    end;
  end;

  if (AnchorTextAt(X, Y) <> '') or (NavAnchorTextAt(X, Y) <> '') then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

procedure TAdvHTMLPictureBrowser.MoveTo(Index: Integer);
begin
  if Assigned(FPictures) and (FPictures.Count > 0) then
  begin
    ItemIndex := Max(Index, 0);
    ItemIndex := Min(ItemIndex, FPictures.Count - 1);
    CalculateImageRect;
    Invalidate;
  end
  else
    ItemIndex := -1;
end;

procedure TAdvHTMLPictureBrowser.First;
begin
  MoveTo(0);
end;

procedure TAdvHTMLPictureBrowser.Last;
begin
  MoveTo(MaxInt);
end;

function TAdvHTMLPictureBrowser.NavAnchorTextAt(X, Y: Integer): string;
var
  str, a, s, focusanchor: string;
  res: Boolean;
  xs, ys, HyperLinks, MouseLink: Integer;
  r: TRect;
  pc: TPictureContainer;
begin
  Result := '';
  a := '';
  str := FNavigation.NavBarHTML;
  if Assigned(FNavigation.PictureContainer) then
    pc := FNavigation.PictureContainer
  else
    pc:= FNavigation.FContainer;
  res := HTMLDrawEx(Canvas,str,FNavTextRect, FNavigation.Images, X, Y, -1, -1, 1, True, False, False, False, False, False,
                       FWordWrap ,1.0, clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys , HyperLinks, MouseLink,
                       r, nil, pc, 0);
  if res then
    Result := a;
end;

procedure TAdvHTMLPictureBrowser.NavigateByLink(Text: string);
var
  MoveIndex: Integer;
  str: string;
begin
  str := UpperCase(Text);
  if str = 'FIRST' then
    First
  else
  begin
    if str = 'LAST' then
      Last
    else
    begin
      if str = 'PREVIOUS' then
        Previous
      else
      begin
        if str = 'NEXT' then
          Next
        else
        begin
          MoveIndex := StrToInt(str);
          MoveTo(MoveIndex);
        end;
      end;
    end;
  end;
end;

procedure TAdvHTMLPictureBrowser.NavigationUpdated(Sender: TObject);
begin
  FNavigation.SetNavBarHTML(FItemIndex, FPictures.Count - 1);
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.Next;
begin
  MoveTo(FItemIndex + 1);
end;

procedure TAdvHTMLPictureBrowser.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if (AComponent = FImages) then
      FImages := nil;

    if (AComponent = FPictureContainer) then
      FPictureContainer := nil;

    if (AComponent = FPicturePopUp) then
      FPicturePopup := nil;
  end;

  inherited;
end;

procedure TAdvHTMLPictureBrowser.Previous;
begin
  MoveTo(FItemIndex - 1);
end;

procedure TAdvHTMLPictureBrowser.Paint;
var
  clipRGN: HRGN;
  imgr: TRect;
  pic: THTMLPictureItem;
  imgrc: TRect;
  pm: TMargins;
begin
  inherited;
  PaintRect(Color, ColorTo, BorderColor, ClientRect);
  pm := FPictureMargins;
  imgr := FImageBoundsRect;
  PaintRect(ImageBackgroundColor, ImageBackgroundColorTo, ImageBorderColor, imgr);
  pic := GetSelectedImage;
  if Assigned(pic) then
  begin
    imgrc := FImageContentsRect;
    clipRGN := CreateRectRgn(imgr.Left + pm.Left, imgr.Top + pm.Top, imgr.Right - pm.Right, imgr.Bottom - pm.Bottom);
    if clipRGN <> 0 then
        SelectClipRgn(Canvas.Handle,clipRGN);

    Canvas.StretchDraw(imgrc, pic.Picture);



    if clipRGN <> 0 then
    begin
      SelectClipRgn(Canvas.Handle, 0);
      DeleteObject(clipRGN);
    end;
    if FHoveringOverPicture then
    begin
      Canvas.StretchDraw(FButtonPreviousRect, FButtonPreviousImage);
      Canvas.StretchDraw(FButtonNextRect, FButtonNextImage);
    end;
    PaintHTML;
  end;
end;

procedure TAdvHTMLPictureBrowser.PaintRect(BgColor, BGColorTo, LineColor: TColor; r: TRect);
var
  steps: Integer;
begin
  if (BgColor <> clNone)then
  begin
    if BgColorTo = clNone then
    begin
      if BgColor = clNone then
        Canvas.Brush.Style := bsClear
      else
      begin
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := BgColor;
      end;

      if LineColor = clNone then
        Canvas.Pen.Style := psClear
      else
      begin
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := LineColor;
      end;

      Canvas.Rectangle(r);
    end
    else
    begin
      steps := r.Right - r.Left;
      DrawHTMLGradient(Canvas, BgColor, BgColorTo, LineColor, steps, r, False);
    end;
  end;
end;

procedure TAdvHTMLPictureBrowser.PaintHTML;
var
  a, s, str, focusAnchor: string;
  xs, ys, HyperLinks, Mouselink: Integer;
  img: THTMLPictureItem;
  r, hr: TRect;
  pc: TPictureContainer;
  clipRGN: HRGN;
begin
  if (FHTMLRect.Top < FHTMLRect.Bottom) and FShowDesc then
  begin
    hr := FHTMLRect;
    PaintRect(DescriptionBackgroundColor, DescriptionBackgroundColorTo, DescriptionBorderColor, hr);
    img := GetSelectedImage;
    if Assigned(img) then
    begin
      clipRGN := CreateRectRgn(hr.Left, hr.Top, hr.Right, hr.Bottom);
      if clipRGN <> 0 then
          SelectClipRgn(Canvas.Handle,clipRGN);

      str := img.FHTMLDescription.Text;
      hr.Left := hr.Left + 5;
      HTMLDrawEx(Canvas, str, hr, FImages, -1, -1, -1, -1, 1, False, False, False, False, False, False, FWordWrap, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys , HyperLinks, MouseLink, r, nil, FPictureContainer, 0);
      hr.Left := hr.Left - 5;

      if clipRGN <> 0 then
      begin
        SelectClipRgn(Canvas.Handle, 0);
        DeleteObject(clipRGN);
      end;
    end;
  end;

  PaintRect(Navigation.Color, Navigation.ColorTo, Navigation.BorderColor, FNavigationRect);
  if Assigned(FNavigation.PictureContainer) then
    pc := FNavigation.PictureContainer
  else
    pc:= FNavigation.FContainer;

  str := FNavigation.NavBarHTML;
  HTMLDrawEx(Canvas, str, FNavTextRect, FNavigation.Images, -1, -1, -1, -1, 1, False, False, False, False, False, False, False, 1.0,
             clBlue, clNone, clNone, clGray, a, s, focusanchor, xs, ys , HyperLinks, MouseLink, r, nil, pc, 0);
end;

procedure TAdvHTMLPictureBrowser.SetPictureMargins(const Value: TMargins);
begin
  if FPictureMargins <> Value then
  begin
    FPictureMargins.Assign(Value);
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.PicturesChanged(Sender: TObject);
begin
  FNavigation.SetNavBarHTML(FItemIndex, Pictures.Count - 1);
  Changed;
end;

procedure TAdvHTMLPictureBrowser.Resize;
begin
  inherited;
  Changed;
end;

procedure TAdvHTMLPictureBrowser.SetAspectRatio(const Value: Boolean);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetButtonNextImage(const Value: TGDIPPicture);
begin
  FButtonNextImage.Assign(Value);
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.SetButtonPreviousImage(
  const Value: TGDIPPicture);
begin
  FButtonPreviousImage.Assign(Value);
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.SetCenter(const Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetCropping(const Value: Boolean);
begin
  if FCropping <> Value then
  begin
    FCropping := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionBackgroundColor(
  const Value: TColor);
begin
  if FDescriptionBackgroundColor <> Value then
  begin
    FDescriptionBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionBackgroundColorTo(
  const Value: TColor);
begin
  if FDescriptionBackgroundColorTo <> Value then
  begin
    FDescriptionBackgroundColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionBorderColor(const Value: TColor);
begin
  if FDescriptionBorderColor <> Value then
  begin
    FDescriptionBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionHoverDelay(const Value: Integer);
begin
  if (FDescriptionHoverDelay <> Value) then
  begin
    FDescriptionHoverDelay := Value;
    if value >= 0 then
    begin
      FHoverTimer.Interval := Value;
      FShowDesc := not FDescriptionHover;
    end
    else
      FShowDesc := true;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionHoverGraceTime(
  const Value: Integer);
begin
  if (FDescriptionHoverGraceTime <> Value) and (Value >= 0) then
  begin
    FDescriptionHoverGraceTime := Value;
    FGraceTimer.Interval := Value;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetDescriptionPlacement(
  const Value: TDescPlacement);
begin
  if Value <> FDescriptionPlacement then
  begin
    FDescriptionPlacement := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetImageBackgroundColor(const Value: TColor);
begin
  if FImageBackgroundColor <> Value then
  begin
    FImageBackgroundColor := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetImageBackgroundColorTo(const Value: TColor);
begin
  if FImageBackgroundColorTo <> Value then
  begin
    FImageBackgroundColorTo := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetImageBorderColor(const Value: TColor);
begin
  if FImageBorderColor <> Value then
  begin
    FImageBorderColor := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.SetInnerMargin(const Value: Integer);
begin
  if FInnerMargin <> Value then
  begin
    FInnerMargin := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetItemIndex(const Value: Integer);
begin
  if FItemIndex <> Value then
  begin
    if (Value < Pictures.Count) and (Value >= 0) or ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
      FItemIndex := Value
    else
    begin
      if (Pictures.Count = 0) then
        FItemIndex := -1
      else
        FItemIndex := FPictures.Count - 1;
    end;
    FNavigation.SetNavBarHTML(FItemIndex, Pictures.Count - 1);

    if Assigned(OnPictureChanged) then
      OnPictureChanged(Self, FItemIndex);

    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SeTNavigationBarSettings(const Value: TNavigationBarSettings);
begin
  FNavigation.Assign(Value);
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.SetPictureContainer(const Value: TPictureContainer);
begin
  FPictureContainer := Value;
  Invalidate;
end;

procedure TAdvHTMLPictureBrowser.SetOuterMargins(const Value: TMargins);
begin
  FOuterMargins.Assign(Value);
  Changed;
end;

procedure TAdvHTMLPictureBrowser.SetPictures(const Value: THTMLPictureList);
begin
  if Value <> FPictures then
  begin
    FPictures.Assign(Value);
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    Changed;
  end;
end;

procedure TAdvHTMLPictureBrowser.SetSwipeEnabled(const Value: Boolean);
begin
  if Value <> FSwipeEnabled then
    FSwipeEnabled := Value;
end;

procedure TAdvHTMLPictureBrowser.SetVersion(const Value: string);
begin

end;

procedure TAdvHTMLPictureBrowser.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

procedure TAdvHTMLPictureBrowser.Swipe;
begin
  if (FSwipeXStop - FSwipeXStart) > 10 then
    Previous
  else
  begin
    if (FSwipeXStart - FSwipeXStop) > 10 then
      Next;
  end;
  FSwipeStarted := False;
end;

procedure TAdvHTMLPictureBrowser.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS;
end;

{ THTMLPictureItem }

constructor THTMLPictureItem.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TGDIPPicture.Create;
  FPicture.LoadFromResourceName(HInstance, 'TMSDEFAULTPIC1');
  FOwner := Collection.Owner as TAdvHTMLPictureBrowser;
  FHTMLDescription := TStringList.Create;
  FHTMLDescription.OnChange := HTMLChanged;
end;

destructor THTMLPictureItem.Destroy;
begin
  FHTMLDescription.Free;
  FPicture.Free;
  inherited;
end;

function THTMLPictureItem.GetDisplayName: string;
begin
  Result := 'Picture ' + IntToStr(Index);
end;

procedure THTMLPictureItem.HTMLChanged(Sender: TObject);
begin
  FOwner.Invalidate;
end;

procedure THTMLPictureItem.SetHTMLDescription(const Value: TStringList);
begin
  if Assigned(Value) then
  begin
    FHTMLDescription.Text := CRLFStrip(Value.Text,False);
    FOwner.Invalidate;
  end;
end;

procedure THTMLPictureItem.SetGDIPPicture(const Value: TGDIPPicture);
begin
  if FPicture <> Value then
  begin
    FPicture.Assign(Value);
    FOwner.Invalidate;
  end;
end;

{ THTMLPictureList }

function THTMLPictureList.Add: THTMLPictureItem;
begin
  Result := THTMLPictureItem(inherited Add);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor THTMLPictureList.Create(AOwner: TAdvHTMLPictureBrowser);
begin
  inherited Create(AOwner, THTMLPictureItem);
end;

destructor THTMLPictureList.Destroy;
begin

  inherited;
end;

function THTMLPictureList.GetItem(Index: Integer): THTMLPictureItem;
begin
  Result := THTMLPictureItem(inherited GetItem(Index));
end;

function THTMLPictureList.Insert(Index: Integer): THTMLPictureItem;
begin
  Result := THTMLPictureItem(inherited Insert(Index));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THTMLPictureList.SetItem(Index: Integer; Value: THTMLPictureItem);
begin
  inherited SetItem(Index, Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THTMLPictureList.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure THTMLPictureList.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TNavigationBarSettings }

procedure TNavigationBarSettings.AddIndexLinkToHTML(Index: Integer);
begin
  FNavBarHTML := FNavBarHTML + '<a href="' + IntToStr(Index) + '">' + IntToStr(Index + 1) +'</a>&nbsp;';
end;

procedure TNavigationBarSettings.AddLinkToHTML(Href, Text: string);
begin
  FNavBarHTML := FNavBarHTML + '<a href="' + Href + '">' + Text +'</a>&nbsp;';
end;

procedure TNavigationBarSettings.AddResourceToContainer(Container: TPictureContainer; resName, itemName: string);
var
  pic: THTMLPicture;
  item: TPictureItem;
begin
  pic := THTMLPicture.Create;
  item := Container.Items.Add;
  try
    pic.LoadFromResourceName(HInstance, resName);
    item.Picture.Assign(pic);
    item.Name := itemName;
  finally
    pic.Free;
  end;
end;

procedure TNavigationBarSettings.AddImageToHTML(imgType: TNavImgType);
begin
  FNavBarHTML := FNavBarHTML + '<img src="' + GetImageSource(imgType) + '" width="24" height="24">&nbsp;';
end;

procedure TNavigationBarSettings.AddImageLinkToHTML(Href: string; imgType: TNavImgType);
begin

  FNavBarHTML := FNavBarHTML + '<a href="' + Href + '"><img align="middle" src="' + GetImageSource(imgType) + '" width="24" height="24"></a>&nbsp;';
end;

procedure TNavigationBarSettings.AddToHTML(Text: string);
begin
  FNavBarHTML := FNavBarHTML + Text + '&nbsp;';
end;

constructor TNavigationBarSettings.Create(AOwner: TAdvHTMLPictureBrowser);
begin
  inherited Create;
  FMaxBullets := 5;
  FOwner := AOwner;
  FNavType := navBullets;
  FImageFirstIndex := 0;
  FImageLastIndex := 0;
  FImagePreviousIndex := 0;
  FImageNextIndex := 0;
  FImageBulletIndex := 0;
  FImageSelectedBulletIndex := 0;
  FShowFirst := True;
  FShowLast := True;
  FShowNext := True;
  FBorderColor := clSilver;
  FColor := clWhite;
  FColorTo := clSilver;
  FShowPrevious := True;
  FContainer := TPictureContainer.Create(Aowner);
  AddResourceToContainer(FContainer, 'TMSBULLET', 'bullet');
  AddResourceToContainer(FContainer, 'TMSSELECTED', 'selected');
  AddResourceToContainer(FContainer, 'TMSFIRST', 'first');
  AddResourceToContainer(FContainer, 'TMSLAST', 'last');
  AddResourceToContainer(FContainer, 'TMSNEXT', 'next');
  AddResourceToContainer(FContainer, 'TMSPREVIOUS', 'previous');
end;

destructor TNavigationBarSettings.Destroy;
begin
  FContainer.Free;
  inherited;
end;

function TNavigationBarSettings.GetImageSource(imgType: TNavImgType): string;
begin
  Result := '';
  if Assigned(FPictureContainer) then
  begin
    case imgType of
      first: Result := FNameImageFirst;
      last: Result := FNameImageLast;
      next: Result := FNameImageNext;
      previous: Result := FNameImagePrevious;
      bullet: Result := FNameImageBullet;
      selected: Result := FNameImageSelectedBullet;
    end;
  end
  else
  begin
    if Assigned(FImages) then
    begin
      case imgType of
        first: Result := 'idx:' + IntToStr(FImageFirstIndex) ;
        last: Result := 'idx:' + IntToStr(FImageLastIndex);
        next: Result := 'idx:' + IntToStr(FImageNextIndex);
        previous: Result := 'idx:' + IntToStr(FImagePreviousIndex);
        bullet: Result := 'idx:' + IntToStr(FImageBulletIndex);
        selected: Result := 'idx:' + IntToStr(FImageSelectedBulletIndex);
      end;
    end
    else
    begin
      case imgType of
        first: Result := 'first';
        last: Result := 'last';
        next: Result := 'next';
        previous: Result := 'previous';
        bullet: Result := 'bullet';
        selected: Result := 'selected';
      end;
    end;
  end;
end;

procedure TNavigationBarSettings.SetNameImageBullet(const Value: string);
begin
  if FNameImageBullet <> Value then
  begin
    FNameImageBullet := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetNameImageFirst(const Value: string);
begin
  if FNameImageFirst <> Value then
  begin
    FNameImageFirst := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetNameImageLast(const Value: string);
begin
  if FNameImageLast <> Value then
  begin
    FNameImageLast := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetNameImageNext(const Value: string);
begin
  if FNameImageNext <> Value then
  begin
    FNameImageNext := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetNameImagePrevious(const Value: string);
begin
  if FNameImagePrevious <> Value then
  begin
    FNameImagePrevious := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetNameImageSelectedBullet(
  const Value: string);
begin
  if FNameImageSelectedBullet <> Value then
  begin
    FNameImageSelectedBullet := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

function TNavigationBarSettings.SetNavBarHTML(Index, MaxIndex: Integer): string;
var
  I: Integer;
begin
  FNavBarHTML := '<p align="center">';
  case NavigationType of
    navText:
    begin
      if ShowFirst then
      begin
        if Index = 0 then
          AddToHTML('&lt;&lt;')
        else
          AddLinkToHTML('first', '&lt;&lt;');
      end;
      if ShowPrevious then
      begin
        if Index = 0 then
          AddToHTML('&lt;&nbsp;')
        else
          AddLinkToHTML('previous', '&lt;');
      end;
      if Index - FMaxBullets div 2 > 0 then
      begin
        for I := Min(MaxIndex - FMaxBullets, Index - FMaxBullets div 2) to Index - 1 do
        begin
          if I >= 0 then
            AddIndexLinkToHTML(I);
        end;
        AddToHTML(IntToStr(Index + 1));
        for I := Index + 1 to Index + (FMaxBullets - FMaxBullets div 2 - 1) do
        begin
          if I <= MaxIndex then
            AddIndexLinkToHTML(I);
        end;
      end
      else
      begin
        if Index <> 0 then
        begin
          for I := 0 to Index - 1 do
            AddIndexLinkToHTML(I);
        end;
        AddToHTML(IntToStr(Index + 1));
        for I := Index + 1 to FMaxBullets - 1 do
        begin
          if I <= MaxIndex then
            AddIndexLinkToHTML(I);
        end;
      end;
      if ShowNext then
      begin
        if Index < MaxIndex then
          AddLinkToHTML('next', '&gt;')
        else
          AddToHTML('&gt;');
      end;
      if ShowLast then
      begin
        if Index < MaxIndex then
          AddLinkToHTML('last', '&gt;&gt;')
        else
          AddToHTML('&gt;&gt;')
      end;
    end;

    navImages:
    begin
      if ShowFirst then
      begin
        if Index = 0 then
          AddImageToHTML(first)
        else
          AddImageLinkToHTML('first', first);
      end;
      if ShowPrevious then
      begin
        if Index = 0 then
          AddImageToHTML(previous)
        else
          AddImageLinkToHTML('previous', previous);
      end;
      if Index - FMaxBullets div 2 > 0 then
      begin
        for I := Min(MaxIndex - FMaxBullets, Index - FMaxBullets div 2) to Index - 1 do
        begin
          if I >= 0 then
            AddIndexLinkToHTML(I);
        end;
        AddToHTML(IntToStr(Index + 1));
        for I := Index + 1 to Index + (FMaxBullets - FMaxBullets div 2 - 1) do
        begin
          if I <= MaxIndex then
            AddIndexLinkToHTML(I);
        end;
      end
      else
      begin
        if Index <> 0 then
        begin
          for I := 0 to Index - 1 do
            AddIndexLinkToHTML(I);
        end;
        AddToHTML(IntToStr(Index + 1));
        for I := Index + 1 to FMaxBullets  - 1 do
        begin
          if I <= MaxIndex then
            AddIndexLinkToHTML(I);
        end;
      end;
      if ShowNext then
      begin
        if Index < MaxIndex then
          AddImageLinkToHTML('next', next)
        else
          AddImageToHTML(next);
      end;
      if ShowLast then
      begin
        if Index < MaxIndex then
          AddImageLinkToHTML('Last', last)
        else
          AddImageToHTML(last);
      end;
    end;

    navBullets:
    begin
      if ShowFirst then
      begin
        if Index = 0 then
          AddImageToHTML(first)
        else
          AddImageLinkToHTML('first', first);
      end;
      if ShowPrevious then
      begin
        if Index = 0 then
          AddImageToHTML(previous)
        else
          AddImageLinkToHTML('previous', previous);
      end;
      if Index - FMaxBullets div 2 > 0 then
      begin
        for I := Min(MaxIndex - FMaxBullets, Index - FMaxBullets div 2) to Index - 1 do
        begin
          if I >= 0 then
            AddImageLinkToHTML(IntToStr(I), bullet);
        end;
        AddImageToHTML(selected);
        for I := Index + 1 to Index + (FMaxBullets - FMaxBullets div 2 - 1) do
        begin
          if I <= MaxIndex then
            AddImageLinkToHTML(IntToStr(I), bullet);
        end;
      end
      else
      begin
        if Index <> 0 then
        begin
          for I := 0 to Index - 1 do
            AddImageLinkToHTML(IntToStr(I), bullet);
        end;
        AddImageToHTML(selected);
        for I := Index + 1 to FMaxBullets - 1 do
        begin
          if I <= MaxIndex then
            AddImageLinkToHTML(IntToStr(I), bullet);
        end;
      end;
      if ShowNext then
      begin
        if Index < MaxIndex then
          AddImageLinkToHTML('next', next)
        else
          AddImageToHTML(next);
      end;
      if ShowLast then
      begin
        if Index < MaxIndex then
          AddImageLinkToHTML('Last', last)
        else
          AddImageToHTML(last);
      end;
    end;
  end;

  FNavBarHTML := FNavBarHTML + '</p>';
  if not FOwner.DesignTime then
    FOwner.CalculateNavRect;
end;

procedure TNavigationBarSettings.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  FOwner.Invalidate;
end;

procedure TNavigationBarSettings.SetColor(const Value: TColor);
begin
  FColor := Value;
  FOwner.Invalidate;
end;

procedure TNavigationBarSettings.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  FOwner.Invalidate;
end;

procedure TNavigationBarSettings.SetImageBulletIndex(const Value: TImageIndex);
begin
  if FImageBulletIndex <> Value then
  begin
    FImageBulletIndex := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetImageFirstIndex(const Value: TImageIndex);
begin
  if FImageFirstIndex <> Value then
  begin
    FImageFirstIndex := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetImageLastIndex(const Value: TImageIndex);
begin
  if FImageLastIndex <> Value then
  begin
    FImageLastIndex := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetImageNextIndex(const Value: TImageIndex);
begin
  if FImageNextIndex <> Value then
  begin
    FImageNextIndex := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetImagePreviousIndex(
  const Value: TImageIndex);
begin
  if FImagePreviousIndex <> Value then
  begin
    FImagePreviousIndex := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    FImages := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetImageSelectedBulletIndex(
  const Value: TImageIndex);
begin
  if FImageSelectedBulletIndex <> Value then
  begin
    FImageSelectedBulletIndex := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetMaxBullets(const Value: Integer);
begin
  if (FMaxBullets <> Value) and (Value > 2) then
  begin
    FMaxBullets := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetNavType(const Value: TNavigationType);
begin
  if FNavType <> Value then
  begin
    FNavType := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetOnUpdate(const Value: TNotifyEvent);
begin
  FOnUpdate := Value;
end;

procedure TNavigationBarSettings.SetPictureContainer(const Value: TPictureContainer);
begin
  if FPictureContainer <> Value then
  begin
    FPictureContainer := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetShowFirst(const Value: Boolean);
begin
  if FShowFirst <> Value then
  begin
    FShowFirst := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
  end;
end;

procedure TNavigationBarSettings.SetShowLast(const Value: Boolean);
begin
  if FShowLast <> Value then
  begin
    FShowLast := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
    FOwner.Invalidate;
  end;
end;

procedure TNavigationBarSettings.SetShowNext(const Value: Boolean);
begin
  if FShowNext <> Value then
  begin
    FShowNext := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
    FOwner.Invalidate;
  end;
end;

procedure TNavigationBarSettings.SetShowPrevious(const Value: Boolean);
begin
  if FShowPrevious <> Value then
  begin
    FShowPrevious := Value;
    if Assigned(FOnUpdate) then
      FOnUpdate(Self);
    FOwner.Invalidate;
  end;
end;

end.

