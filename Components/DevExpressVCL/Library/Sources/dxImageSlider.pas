{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxImageSlider;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, Classes, Controls, Graphics, Forms, ImgList, Messages, StdCtrls, dxCore, dxCoreClasses,
  cxControls, cxClasses, cxGraphics, dxTouch, dxGDIPlusClasses, dxGDIPlusApi, dxAnimation, cxLookAndFeels,
  cxGeometry, cxScrollBar, Generics.Defaults, Generics.Collections, Math;

const
  dxImageSliderThumbnailIndent = 20;
  dxImageSliderThumbnailDefaultHeight = 90;
  dxImageSliderThumbnailDefaultWidth = 60;
  dxImageSliderThumbnailFrameWidth = 1;
  dxImageSliderThumbnailMinSize = 16;
  dxImageSliderThumbnailSelectionFrameWidth = 2;
  dxImageSliderPreviewScrollStep = dxImageSliderThumbnailDefaultWidth div 3;
  dxImageSliderPreviewSplitterDefaultSize = 3;

  dxImageSliderDefaultAnimationTime = 800;


type
  TdxCustomImageSlider = class;

  TdxImageSliderTransitionEffect = (isteNone, isteSlide, isteFade, isteSegmentedFade, isteRandomSegmentedFade);

  TdxImageSliderScrollMode = (issmButtons, issmScrollBar);

  TdxImageSliderGetThumbnailEvent = procedure(Sender: TdxCustomImageSlider; AFrameIndex: Integer;
                                    var AThumbnail: TGraphic) of object;

  { TdxImageSliderPreviewOptions }

  TdxImageSliderPreviewOptions = class(TcxOwnedPersistent)
  strict private
    FIndent: Integer;
    FPosition: TcxPosition;
    FResizable: Boolean;
    FSize: Integer;
    FThumbnailCaptionAlignment: TAlignment;
    FThumbnailSize: TcxSize;

    function GetImageSlider: TdxCustomImageSlider;
    procedure SetIndent(AValue: Integer);
    procedure SetPosition(AValue: TcxPosition);
    procedure SetResizable(AValue: Boolean);
    procedure SetSize(AValue: Integer);
    procedure SetThumbnailCaptionAlignment(AValue: TAlignment);
    procedure SetThumbnailSize(AValue: TcxSize);
    procedure SizeChanged(Sender: TObject);
  protected
    function ActuallySize: Integer;
    procedure Changed; virtual;
    procedure ChangeScale(M, D: Integer);
    function DefaultSize: Integer;
    function IndentsHeight: Integer;
    function IndentsWidth: Integer;
    function MinHeight: Integer;
    function MinWidth: Integer;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    property ImageSlider: TdxCustomImageSlider read GetImageSlider;
  published
    property Indent: Integer read FIndent write SetIndent default dxImageSliderThumbnailIndent;
    property Position: TcxPosition read FPosition write SetPosition default posNone;
    property Resizable: Boolean read FResizable write SetResizable default True;
    property Size: Integer read FSize write SetSize default 0;
    property ThumbnailCaptionAlignment: TAlignment read FThumbnailCaptionAlignment write SetThumbnailCaptionAlignment default taCenter;
    property ThumbnailSize: TcxSize read FThumbnailSize write SetThumbnailSize;
  end;

  { TdxCustomImageSlider }

  TdxCustomImageSlider = class(TcxControl,
    IcxImageCollectionListener,
    IcxMouseTrackingCaller,
    IdxSkinSupport)
  strict private type
  {$REGION 'private types'}
    TViewInfoItem = class;

    TDrawItemHandler = procedure(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem) of object;

    TViewInfoItem = class
    public
      Bounds, VisibleBounds: TRect;
      ObjectID: Integer;
      OnClick: TNotifyEvent;
      OnDrawItem: TDrawItemHandler;
      procedure Click;
      function ClipRect: TRect;
      procedure Draw(ACanvas: TcxCanvas);
    end;

    TPreviewTouchScrollUIHelper = class(TcxIUnknownObject, IdxTouchScrollUIOwner, IdxHybridScrollbarOwner)
    private
      FHybridScrollbarsManager: TdxHybridScrollbarsManager;
      FSlider: TdxCustomImageSlider;
    public
      constructor Create(AOwner: TdxCustomImageSlider);
      destructor Destroy; override;
      // IdxTouchScrollUIOwner
      procedure CheckUIPosition;
      function GetOwnerControl: TcxControl;
      function HasVisibleUI: Boolean;
      procedure HideUI;
      // IdxHybridScrollbarOwner
      function GetBaseColor: TColor;
      function GetManager: TdxHybridScrollbarsManager;
      procedure Invalidate;
    end;

    TPreviewGestureHelper = class(TcxIUnknownObject, IdxGestureClient)
    private
      FSlider: TdxCustomImageSlider;
    public
      constructor Create(AOwner: TdxCustomImageSlider);
      // IdxGestureClient
      function AllowGesture(AGestureId: Integer): Boolean;
      function AllowPan(AScrollKind: TScrollBarKind): Boolean;
      procedure BeginGestureScroll(APos: TPoint);
      procedure EndGestureScroll;
      procedure GestureScroll(ADeltaX, ADeltaY: Integer);
      function GetPanOptions: Integer;
      function IsPanArea(const APoint: TPoint): Boolean;
      function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
    end;

  {$ENDREGION}
  private
    FAnimation: TdxImageAnimationTransition;
    FAnimationTime: Integer;
    FButtonNext: TViewInfoItem;
    FButtonPrev: TViewInfoItem;
    FHotTrackItem: TViewInfoItem;
    FImageFitMode: TcxImageFitMode;
    FImages: TcxImageCollection;
    FItemIndex: Integer;
    FMousePressed: Boolean;
    FMultiFrameImage: TdxSmartGlyph;
    FPreviewGestureHelper: TPreviewGestureHelper;
    FPreviewTouchScrollUIHelper: TPreviewTouchScrollUIHelper;
    FPreviewOptions: TdxImageSliderPreviewOptions;
    FPreviewScrollBar: TdxScrollBarWrapper;
    FPreviewScrollPosition: Integer;
    FRefreshLocked: Boolean;
    FScrollBarKind: TScrollBarKind;
    FScrollMode: TdxImageSliderScrollMode;
    FScrollNeeded: Boolean;
    FSplitter: TViewInfoItem;
    FThumbnails: TcxObjectList;
    FTransitionEffect: TdxImageSliderTransitionEffect;
    FViewInfoItems: TObjectList<TViewInfoItem>;

    FOnChange: TNotifyEvent;
    FOnGetThumbnail: TdxImageSliderGetThumbnailEvent;

    function AddViewInfo(const ABounds: TRect; AObjectID: Integer; ADrawItem: TDrawItemHandler; AClickHandler: TNotifyEvent): TViewInfoItem; overload;
    function AddViewInfo(const ABounds, AVisibleBounds: TRect; AObjectID: Integer; ADrawItem: TDrawItemHandler; AClickHandler: TNotifyEvent): TViewInfoItem; overload;
    function CheckSplitterHitTest(X, Y: Integer): Boolean;
    function GetCount: Integer;
    function GetHotTrackItem: TViewInfoItem;
    function GetPartBounds(const ABounds: TRect; APosition: TcxPosition; ASize: Integer): TRect; inline;
    function GetPreviewRowCount: Integer;
    function GetPreviewScrollBarSize: Integer; inline;
    procedure GetPreviewScrollBarInfo(var APageSize, AMax: Integer);
    function GetPreviewSize: Integer;
    function GetPreviewSplitterSize: Integer; inline;
    function GetThumbnail(AIndex: Integer): TdxSmartImage;
    function IsValidItemIndex(AIndex: Integer): Boolean;
    function IsVerticalScrollMode: Boolean;
    function ItemFromPoint(X, Y: Integer): TViewInfoItem;
    procedure SetAnimationTime(AValue: Integer);
    procedure SetHotTrackItem(AValue: TViewInfoItem);
    procedure SetImageFitMode(AValue: TcxImageFitMode);
    procedure SetImages(AValue: TcxImageCollection);
    procedure SetItemIndex(AValue: Integer);
    procedure SetMultiFrameImage(AValue: TdxSmartGlyph);
    procedure SetPreviewOptions(AValue: TdxImageSliderPreviewOptions);
    procedure SetPreviewScrollPosition(AValue: Integer);
    procedure SetScrollBarKind(AValue: TScrollBarKind);
    procedure SetScrollMode(AValue: TdxImageSliderScrollMode);

    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = TrackingCallerMouseLeave;
    procedure TrackingCallerMouseLeave;

    // event handlers
    procedure ImageChanged(Sender: TObject);
    procedure NavigationButtonClick(Sender: TObject);
    procedure PagePreviewClick(Sender: TObject);
    procedure SplitterClick(Sender: TObject);

    property HotTrackItem: TViewInfoItem read GetHotTrackItem write SetHotTrackItem;
    property MousePressed: Boolean read FMousePressed write FMousePressed;
    property PreviewRowCount: Integer read GetPreviewRowCount;
    property RefreshLocked: Boolean read FRefreshLocked write FRefreshLocked;
    property ViewInfoItems: TObjectList<TViewInfoItem> read FViewInfoItems;
  protected

    // scroll
    function AllowPan(AScrollKind: TScrollBarKind): Boolean; override;
    function AllowTouchScrollUIMode: Boolean; override;
    procedure BeginGestureScroll(APos: TPoint); override;
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; override;
    function HasVisibleTouchScrollUI: Boolean; override;
    procedure HideScrollBars; override;
    function IsSliderButtonsAvailable: Boolean;
    function IsSliderButtonsVisible: Boolean;
    procedure InitializePreviewScrollBar;
    procedure InitScrollBars; override;
    procedure InitScrollBarsParameters; override;
    procedure InvalidateSliderButtons;
    function IsPreviewVisible: Boolean;
    function IsPreviewScrollBarVisible: Boolean;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer); override;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;

    procedure BoundsChanged; override;
    procedure CalculatePreviewLayout;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    function GetClientBounds: TRect; override;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    function GetDragAndDropObjectClass: TcxDragAndDropObjectClass; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetPreviewBounds: TRect; virtual;
    function GetPreviewPosition: TcxPosition;
    function GetPreviewScrollBarBounds: TRect;
    function GetPreviewSplitterBounds: TRect;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;
    procedure ScaleFactorChanged; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure ScrollPreview(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; override;
    procedure ValidateThumbnail(AIndex: Integer); virtual;

    //mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); override;
    procedure MouseLeave(AControl: TControl); override;

    //paint
    procedure DrawButton(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem); virtual;
    procedure DrawPreviewPage(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem); virtual;
    procedure DrawSplitter(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem); virtual;
    procedure EraseBackground(ACanvas: TcxCanvas; const ARect: TRect); override;
    procedure Paint; override;

    function NeedRedrawOnResize: Boolean; override;

    procedure Animate(AStartIndex, AFinishIndex: Integer); overload;
    procedure Animate(AStartIndex, AFinishIndex: Integer; AAnimationMode: TdxDrawAnimationMode; ATransition: TdxAnimationTransitionEffect); overload;
    procedure AnimationHandler(Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
    procedure DrawFrame(ACanvas: TcxCanvas; const ABounds: TRect; AFrameIndex: Integer; AFitMode: TcxImageFitMode); virtual;
    procedure StopAnimation;
    procedure SetPaintRegion; override;

    //IcxImageCollectionListener
    procedure ImageCollectionChanged;
    procedure ImageCollectionDestroyed;

    property Animation: TdxImageAnimationTransition read FAnimation;
    property AnimationTime: Integer read FAnimationTime write SetAnimationTime default dxImageSliderDefaultAnimationTime;
    property Count: Integer read GetCount;
    property ImageFitMode: TcxImageFitMode read FImageFitMode write SetImageFitMode;
    property Images: TcxImageCollection read FImages write SetImages;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property MultiFrameImage: TdxSmartGlyph read FMultiFrameImage write SetMultiFrameImage;
    property ScrollBarKind: TScrollBarKind read FScrollBarKind write SetScrollBarKind default sbHorizontal;
    property ScrollMode: TdxImageSliderScrollMode read FScrollMode write SetScrollMode default issmButtons;
    property Thumbnails[Index: Integer]: TdxSmartImage read GetThumbnail;
    property PreviewScrollPosition: Integer read FPreviewScrollPosition write SetPreviewScrollPosition;
    property PreviewScrollBar: TdxScrollBarWrapper read FPreviewScrollBar;

    property PreviewOptions: TdxImageSliderPreviewOptions read FPreviewOptions write SetPreviewOptions;

    property TransitionEffect: TdxImageSliderTransitionEffect read FTransitionEffect write FTransitionEffect;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetThumbnail: TdxImageSliderGetThumbnailEvent read FOnGetThumbnail write FOnGetThumbnail;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CanGoToNextImage: Boolean; virtual;
    function CanGoToPrevImage: Boolean; virtual;
    procedure GoToImage(ATargetItemIndex: Integer); virtual;
    procedure GoToNextImage; virtual;
    procedure GoToPrevImage; virtual;
  end;

  { TdxImageSlider }

  TdxImageSlider = class(TdxCustomImageSlider)
  published
    property Align;
    property Anchors;
    property AnimationTime;
    property BiDiMode;
    property BorderStyle default cxcbsDefault;
    property Color;
    property Enabled;
    property Font;
    property ImageFitMode default ifmProportionalStretch;
    property Images;
    property ItemIndex default -1;
    property LookAndFeel;
    property MultiFrameImage;
    property ParentBiDiMode;
    property ParentColor;
    property PreviewOptions;
    property ScrollBarKind;
    property ScrollMode;
    property TransitionEffect default isteSlide;
    property Transparent default True;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnChange;
    property OnDblClick;
    property OnGetThumbnail;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

uses
  Types, cxLookAndFeelPainters, cxSplitter, dxDPIAwareUtils;

const
  Sign: array[Boolean] of Integer = (-1, 1);
  SplitterCursors: array[Boolean] of TCursor = (crVSplit, crHSplit);

type
  TdxSplitterDragImageAccess = class(TdxSplitterDragImage);

  { TdxImageSliderResizePreviewDragAndDropObject }

  TdxImageSliderResizePreviewDragAndDropObject = class(TcxDragAndDropObject)
  protected
    FDragImage: TdxSplitterDragImage;
    FStartMousePos: TPoint;
    function IsHorzSizing: Boolean;
    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDragDropArea: TRect;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetDragImageBounds: TRect;
    function GetImageSlider: TdxCustomImageSlider;
    function GetImmediateStart: Boolean; override;
    function GetOptions: TdxImageSliderPreviewOptions;

    property ImageSlider: TdxCustomImageSlider read GetImageSlider;
    property Options: TdxImageSliderPreviewOptions read GetOptions;
  public
    destructor Destroy; override;
  end;

{ TdxImageSliderResizePreviewDragAndDropObject }

destructor TdxImageSliderResizePreviewDragAndDropObject.Destroy;
begin
  FreeAndNil(FDragImage);
  inherited Destroy;
end;

function TdxImageSliderResizePreviewDragAndDropObject.IsHorzSizing: Boolean;
begin
  Result := ImageSlider.GetPreviewPosition in [posRight, posLeft];
end;

procedure TdxImageSliderResizePreviewDragAndDropObject.BeginDragAndDrop;
begin
  FStartMousePos := GetMouseCursorPos;
  FDragImage := TdxSplitterDragImage.Create;
  TdxSplitterDragImageAccess(FDragImage).Canvas.Brush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
  FDragImage.BoundsRect := GetDragImageBounds;
  FDragImage.Show;
end;

procedure TdxImageSliderResizePreviewDragAndDropObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
begin
  Accepted := IsHorzSizing and InRange(P.X, GetDragDropArea.Left, GetDragDropArea.Right) or
    not IsHorzSizing and InRange(P.X, GetDragDropArea.Left, GetDragDropArea.Right);
  FDragImage.BoundsRect := GetDragImageBounds;
  inherited;
end;

procedure TdxImageSliderResizePreviewDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
var
  R: TRect;
begin
  if Accepted then
  begin
    R := cxRectOffset(GetDragImageBounds, Control.ClientToScreen(cxNullPoint), False);
    if IsHorzSizing then
      Options.Size := Options.ActuallySize + (R.Left - ImageSlider.GetPreviewSplitterBounds.Left) * Sign[ImageSlider.GetPreviewPosition = posLeft]
    else
      Options.Size := Options.ActuallySize + (R.Top - ImageSlider.GetPreviewSplitterBounds.Top) * Sign[ImageSlider.GetPreviewPosition = posTop];
  end;
  inherited EndDragAndDrop(Accepted);
end;

function TdxImageSliderResizePreviewDragAndDropObject.GetDragDropArea: TRect;
var
  ACenter: TPoint;
begin
   Result := ImageSlider.ClientBounds;
   ACenter := cxRectCenter(ImageSlider.Bounds);
   case ImageSlider.GetPreviewPosition of
     posLeft:
     begin
       Result.Left := Options.MinWidth;
       Result.Right := Max(ACenter.X, Result.Left);
     end;
     posRight:
     begin
       Result.Right := ImageSlider.Bounds.Right - Options.MinWidth;
       Result.Left := Min(ACenter.X, Result.Right);
     end;
     posTop:
     begin
       Result.Top := Options.MinHeight;
       Result.Bottom := ACenter.Y;
     end;
     posBottom:
     begin
       Result.Bottom := ImageSlider.Bounds.Bottom - Options.MinHeight;
       Result.Top := Min(ACenter.Y, Result.Bottom);
     end;
   end;
end;

function TdxImageSliderResizePreviewDragAndDropObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := SplitterCursors[IsHorzSizing]
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxImageSliderResizePreviewDragAndDropObject.GetDragImageBounds: TRect;
var
  R: TRect;
  AOffset: TPoint;
begin
  AOffset := cxPointOffset(GetMouseCursorPos, FStartMousePos, False);
  Result := cxRectOffset(ImageSlider.GetPreviewSplitterBounds, Control.ClientToScreen(cxNullPoint));
  R := cxRectOffset(GetDragDropArea, Control.ClientToScreen(cxNullPoint));
  if IsHorzSizing then
  begin
    Result := cxRectOffset(Result, AOffset.X, 0);
    if Result.Left < R.Left then
      Result := cxRectSetLeft(Result, R.Left)
    else
      if Result.Right > R.Right then
        Result := cxRectSetRight(Result, R.Right);
  end
  else
  begin
    Result := cxRectOffset(Result, 0, AOffset.Y);
    if Result.Top < R.Top then
      Result := cxRectSetTop(Result, R.Top)
    else
      if Result.Bottom > R.Bottom then
        Result := cxRectSetBottom(Result, R.Bottom);
  end;
end;

function TdxImageSliderResizePreviewDragAndDropObject.GetImageSlider: TdxCustomImageSlider;
begin
  Result := TdxCustomImageSlider(Control);
end;

function TdxImageSliderResizePreviewDragAndDropObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TdxImageSliderResizePreviewDragAndDropObject.GetOptions: TdxImageSliderPreviewOptions;
begin
  Result := ImageSlider.PreviewOptions;
end;

{ TdxImageSliderPreviewOptions }

constructor TdxImageSliderPreviewOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FThumbnailSize := TcxSize.Create(Self, dxImageSliderThumbnailDefaultWidth, dxImageSliderThumbnailDefaultHeight);
  FThumbnailSize.OnChange := SizeChanged;
  FThumbnailCaptionAlignment := taCenter;
  FIndent := dxImageSliderThumbnailIndent;
  FResizable := True;
end;

destructor TdxImageSliderPreviewOptions.Destroy;
begin
  FreeAndNil(FThumbnailSize);
  inherited Destroy;
end;

function TdxImageSliderPreviewOptions.ActuallySize: Integer;
begin
  Result := FSize;
  if Result = 0 then
    Result := DefaultSize;
end;

procedure TdxImageSliderPreviewOptions.Changed;
begin
  ImageSlider.LayoutChanged;
end;

procedure TdxImageSliderPreviewOptions.ChangeScale(M, D: Integer);
begin
  Size := MulDiv(Size, M, D);
  Indent := MulDiv(Indent, M, D);
  ThumbnailSize.ChangeScale(M, D);
end;

function TdxImageSliderPreviewOptions.DefaultSize: Integer;
begin
  if Position in [posTop, posBottom] then
    Result := ThumbnailSize.Height + IndentsHeight
  else
    Result := ThumbnailSize.Width + IndentsWidth;
end;

function TdxImageSliderPreviewOptions.IndentsHeight: Integer;
begin
  Result := IndentsWidth + cxTextHeight(ImageSlider.Font) + ImageSlider.ScaleFactor.Apply(cxTextOffset);
end;

function TdxImageSliderPreviewOptions.IndentsWidth: Integer;
begin
  Result := (Indent + dxImageSliderThumbnailFrameWidth + dxImageSliderThumbnailSelectionFrameWidth) * 2;
end;

function TdxImageSliderPreviewOptions.MinHeight: Integer;
begin
  Result := IndentsHeight + ImageSlider.ScaleFactor.Apply(dxImageSliderThumbnailMinSize);
end;

function TdxImageSliderPreviewOptions.MinWidth: Integer;
begin
  Result := IndentsWidth + ImageSlider.ScaleFactor.Apply(dxImageSliderThumbnailMinSize);
end;

function TdxImageSliderPreviewOptions.GetImageSlider: TdxCustomImageSlider;
begin
  Result := TdxCustomImageSlider(GetOwner);
end;

procedure TdxImageSliderPreviewOptions.SetIndent(AValue: Integer);
begin
  AValue := Max(5, AValue);
  if Indent <> AValue then
  begin
    FIndent := AValue;
    Changed;
  end;
end;

procedure TdxImageSliderPreviewOptions.SetPosition(AValue: TcxPosition);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    ImageSlider.PreviewScrollPosition := 0;
    Changed;
  end;
end;

procedure TdxImageSliderPreviewOptions.SetResizable(AValue: Boolean);
begin
  if AValue <> Resizable then
  begin
    FResizable := AValue;
    Changed;
  end;
end;

procedure TdxImageSliderPreviewOptions.SetSize(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FSize <> AValue then
  begin
    FSize := AValue;
    Changed;
  end;
end;

procedure TdxImageSliderPreviewOptions.SetThumbnailCaptionAlignment(AValue: TAlignment);
begin
  if FThumbnailCaptionAlignment <> AValue then
  begin
    FThumbnailCaptionAlignment := AValue;
    Changed;
  end;
end;

procedure TdxImageSliderPreviewOptions.SetThumbnailSize(AValue: TcxSize);
begin
  ThumbnailSize.Assign(AValue);
end;

procedure TdxImageSliderPreviewOptions.SizeChanged(Sender: TObject);
var
  ASize: TSize;
begin
  ASize := cxSize(
    Max(ImageSlider.ScaleFactor.Apply(dxImageSliderThumbnailMinSize), ThumbnailSize.Width),
    Max(ImageSlider.ScaleFactor.Apply(dxImageSliderThumbnailMinSize), ThumbnailSize.Height));

  if not cxSizeIsEqual(ThumbnailSize.Size, ASize) then
    ThumbnailSize.Size := ASize
  else
    Changed;
end;

{ TdxCustomImageSlider }

constructor TdxCustomImageSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAnimationTime := dxImageSliderDefaultAnimationTime;
  TransitionEffect := isteSlide;
  FPreviewTouchScrollUIHelper := TPreviewTouchScrollUIHelper.Create(Self);
  FPreviewGestureHelper := TPreviewGestureHelper.Create(Self);
  FPreviewOptions := TdxImageSliderPreviewOptions.Create(Self);
  FViewInfoItems := TObjectList<TViewInfoItem>.Create();
  BorderStyle := cxcbsDefault;
  Width := 150;
  Height := 100;
  FThumbnails := TcxObjectList.Create;
  FPreviewScrollBar := TdxScrollBarWrapper.Create(FPreviewTouchScrollUIHelper);
  FPreviewScrollBar.OnScroll := ScrollPreview;
  FMultiFrameImage := TdxSmartGlyph.Create;
  FMultiFrameImage.OnChange := ImageChanged;
  FImageFitMode := ifmProportionalStretch;
  FItemIndex := -1;
  Transparent := True;
  DoubleBuffered := True;
end;

destructor TdxCustomImageSlider.Destroy;
begin
  EndMouseTracking(Self);
  StopAnimation;
  Images := nil;
  FreeAndNil(FPreviewOptions);
  FreeAndNil(FPreviewScrollBar);
  FreeAndNil(FPreviewGestureHelper);
  FreeAndNil(FPreviewTouchScrollUIHelper);
  FreeAndNil(FThumbnails);
  FreeAndNil(FMultiFrameImage);
  FreeAndNil(FViewInfoItems);
  inherited Destroy;
end;


procedure TdxCustomImageSlider.Animate(AStartIndex, AFinishIndex: Integer);
const
  DrawAnimationModeMap: array[TdxImageSliderTransitionEffect] of TdxDrawAnimationMode = (
    amFade, amFade, amFade, amSegmentedFade, amRandomSegmentedFade);

  AnimationScrollMode: array[Boolean, Boolean] of TdxDrawAnimationMode =
    ((amScrollLeft, amScrollRight), (amScrollUp, amScrollDown));

  TransitionMap: array[TdxDrawAnimationMode] of TdxAnimationTransitionEffect = (
    ateAccelerateDecelerate, ateAccelerateDecelerate, ateAccelerateDecelerate, ateAccelerateDecelerate,
    ateLinear, ateLinear, ateLinear,
    ateAccelerateDecelerate, ateAccelerateDecelerate, ateAccelerateDecelerate, ateAccelerateDecelerate);

var
  ADrawAnimationMode: TdxDrawAnimationMode;
begin
  if (TransitionEffect <> isteNone) and IsValidItemIndex(AStartIndex) and IsValidItemIndex(AFinishIndex) then
  begin
    if TransitionEffect <> isteSlide then
      ADrawAnimationMode := DrawAnimationModeMap[TransitionEffect]
    else
      ADrawAnimationMode := AnimationScrollMode[IsVerticalScrollMode, AStartIndex > AFinishIndex];
    if UseRightToLeftAlignment then
      case ADrawAnimationMode of
        amScrollLeft:
          ADrawAnimationMode := amScrollRight;
        amScrollRight:
          ADrawAnimationMode := amScrollLeft;
      end;
    Animate(AStartIndex, AFinishIndex, ADrawAnimationMode, TransitionMap[ADrawAnimationMode]);
  end
  else
    Invalidate;
end;

procedure TdxCustomImageSlider.Animate(AStartIndex, AFinishIndex: Integer;
  AAnimationMode: TdxDrawAnimationMode; ATransition: TdxAnimationTransitionEffect);
var
  ARect: TRect;
  AStartBitmap, AFinishBitmap: TcxBitmap;
begin
  StopAnimation;
  ARect := ClientBounds;
  AStartBitmap := TcxBitmap.CreateSize(ARect);
  AFinishBitmap := TcxBitmap.CreateSize(ARect);
  EraseBackground(AStartBitmap.cxCanvas, AStartBitmap.ClientRect);
  cxCopyImage(AStartBitmap, AFinishBitmap, cxNullPoint, cxNullPoint, AStartBitmap.ClientRect);
  DrawFrame(AStartBitmap.cxCanvas, AStartBitmap.ClientRect, AStartIndex, ImageFitMode);
  DrawFrame(AFinishBitmap.cxCanvas, AFinishBitmap.ClientRect, AFinishIndex, ImageFitMode);
  FAnimation := TdxImageAnimationTransition.Create(AStartBitmap, AFinishBitmap, AnimationTime,
    AAnimationMode, ATransition, -1, True);
  FAnimation.OnAnimate := AnimationHandler;
  FAnimation.Resume;
end;

procedure TdxCustomImageSlider.AnimationHandler(
  Sender: TdxAnimationTransition; var APosition: Integer; var AFinished: Boolean);
begin
  if AFinished then
    FAnimation := nil;
  if not RefreshLocked then
    Refresh;
end;

procedure TdxCustomImageSlider.DrawFrame(ACanvas: TcxCanvas; const ABounds: TRect; AFrameIndex: Integer; AFitMode: TcxImageFitMode);
var
  AActiveFrame: Integer;
  AOnChange: TNotifyEvent;
begin
  if not MultiFrameImage.Empty then
  begin
    AOnChange := MultiFrameImage.OnChange;
    try
      MultiFrameImage.OnChange := nil;
      AActiveFrame := MultiFrameImage.ActiveFrame;
      MultiFrameImage.ActiveFrame := AFrameIndex;
      cxDrawImage(ACanvas, ABounds, MultiFrameImage, nil, -1, AFitMode, idmNormal, True, nil, ScaleFactor,
        cxSmoothDrawingNeeded(ABounds, MultiFrameImage, ScaleFactor));
      MultiFrameImage.ActiveFrame := AActiveFrame;
    finally
      MultiFrameImage.OnChange := AOnChange;
    end;
  end
  else
    if Images <> nil then
      cxDrawPicture(ACanvas, ABounds, Images.Items[AFrameIndex].Picture, AFitMode, ScaleFactor);
end;

function TdxCustomImageSlider.CanGoToNextImage: Boolean;
begin
  Result := IsValidItemIndex(ItemIndex + 1);
end;

function TdxCustomImageSlider.CanGoToPrevImage: Boolean;
begin
  Result := IsValidItemIndex(ItemIndex - 1);
end;

procedure TdxCustomImageSlider.DrawButton(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem);
const
  AStates: array[Boolean] of TcxButtonState = (cxbsHot, cxbsPressed);
  ADirection: array[0..3] of TcxArrowDirection = (adLeft, adRight, adUp, adDown);
var
  AState: TcxButtonState;
  AButtonDrawDirection: TcxArrowDirection;
begin
  if not IsSliderButtonsVisible or ((AViewInfo.ObjectID in [0, 2]) and not CanGoToPrevImage) or
  ((AViewInfo.ObjectID in [1, 3]) and not CanGoToNextImage) then
    Exit;
  AState := cxbsNormal;
  if HotTrackItem = AViewInfo then
    AState := AStates[MousePressed];
  AButtonDrawDirection := ADirection[AViewInfo.ObjectID];
  LookAndFeelPainter.DrawScaledSliderButton(Canvas, AViewInfo.Bounds, AButtonDrawDirection, AState, ScaleFactor)
end;

procedure TdxCustomImageSlider.DrawPreviewPage(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem);
var
  R: TRect;
begin
  R := cxRectSetBottom(AViewInfo.Bounds, AViewInfo.Bounds.Bottom, cxTextHeight(Font) + ScaleFactor.Apply(cxTextOffset));
  ACanvas.Brush.Style := bsClear;
  ACanvas.Font := Font;
  if AViewInfo.ObjectID = ItemIndex then
    ACanvas.Font.Color := LookAndFeelPainter.DefaultSchedulerViewSelectedTextColor
  else
    ACanvas.Font.Color := LookAndFeelPainter.PrintPreviewBackgroundTextColor;

  ACanvas.DrawTexT(IntToStr(AViewInfo.ObjectID), cxRectInflate(R, -(dxImageSliderThumbnailFrameWidth +
    dxImageSliderThumbnailSelectionFrameWidth), 0), PreviewOptions.ThumbnailCaptionAlignment, vaTop, False, False);
  R := cxRectSetYPos(AViewInfo.Bounds, AViewInfo.Bounds.Top, R.Top);
  if AViewInfo.ObjectID = ItemIndex then
    ACanvas.FrameRect(R, LookAndFeelPainter.DefaultHyperlinkTextColor, 2);
  R := cxRectInflate(R, -dxImageSliderThumbnailSelectionFrameWidth);
  ACanvas.FrameRect(R, LookAndFeelPainter.DefaultContentTextColor, dxImageSliderThumbnailFrameWidth);
  R := cxRectInflate(R, -dxImageSliderThumbnailFrameWidth);
  ACanvas.StretchDraw(R, Thumbnails[AViewInfo.ObjectID]);
end;

procedure TdxCustomImageSlider.DrawSplitter(ACanvas: TcxCanvas; AViewInfo: TViewInfoItem);
begin
  EraseBackground(ACanvas, AViewInfo.Bounds);
  LookAndFeelPainter.DrawScaledSplitter(ACanvas, AViewInfo.Bounds, AViewInfo = HotTrackItem,
    (AViewInfo = HotTrackItem) and MousePressed, GetPreviewPosition in [posTop, posBottom], ScaleFactor);
  ACanvas.ExcludeClipRect(AViewInfo.Bounds);
end;

procedure TdxCustomImageSlider.EraseBackground(ACanvas: TcxCanvas; const ARect: TRect);
begin
  if Transparent then
    cxDrawTransparentControlBackground(Self, ACanvas, ARect, False)
  else
    ACanvas.FillRect(ARect, Color);
end;

procedure TdxCustomImageSlider.ImageCollectionChanged;
begin
  ImageChanged(nil);
end;

procedure TdxCustomImageSlider.ImageCollectionDestroyed;
begin
  Images := nil;
end;

procedure TdxCustomImageSlider.GoToNextImage;
begin
  if CanGoToNextImage then
    GoToImage(ItemIndex + 1);
end;

procedure TdxCustomImageSlider.GoToPrevImage;
begin
  if CanGoToPrevImage then
    GoToImage(ItemIndex - 1);
end;

procedure TdxCustomImageSlider.GoToImage(ATargetItemIndex: Integer);
var
  APrevItemIndex: Integer;
begin
  APrevItemIndex := ItemIndex;
  RefreshLocked := True;
  try
    if Animation <> nil then
      StopAnimation;
    ItemIndex := ATargetItemIndex;
    if ItemIndex <> APrevItemIndex then
    begin
      dxCallNotify(OnChange, Self);
      Animate(APrevItemIndex, ItemIndex);
    end;
  finally
    RefreshLocked := False;
  end;
end;

function TdxCustomImageSlider.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  if IsVerticalScrollMode then
    Result := AScrollKind = sbVertical
  else
    Result := AScrollKind = sbHorizontal;
end;

function TdxCustomImageSlider.IsSliderButtonsAvailable: Boolean;
begin
  Result := not IsDesigning and (ScrollMode = issmButtons) and (Count > 0);
end;

function TdxCustomImageSlider.IsSliderButtonsVisible: Boolean;
begin
  Result := IsSliderButtonsAvailable and (Animation = nil) and (
    PtInRect(ClientBounds, GetMouseCursorClientPos) and IsMouseTracking(Self));
end;

procedure TdxCustomImageSlider.BeginGestureScroll(APos: TPoint);
begin
  FScrollNeeded := True;
end;

procedure TdxCustomImageSlider.DoCreateScrollBars;
begin
  FPreviewScrollBar.CreateInnerScrollBar;
  inherited DoCreateScrollBars;
end;

procedure TdxCustomImageSlider.DoDestroyScrollBars;
begin
  FPreviewScrollBar.DestroyInnerScrollBar;
  inherited DoDestroyScrollBars;
end;

function TdxCustomImageSlider.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  if PtInRect(GetPreviewBounds, APoint) then
    Result := FPreviewGestureHelper
  else
    Result := inherited GetGestureClient(APoint);
end;

function TdxCustomImageSlider.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := nil;
  if not PtInRect(GetPreviewSplitterBounds, APoint) then
    if PtInRect(GetPreviewBounds, APoint) then
      Result := FPreviewTouchScrollUIHelper
    else
      if PtInRect(ClientBounds, APoint) then
        Result := Self;
end;

function TdxCustomImageSlider.HasVisibleTouchScrollUI: Boolean;
begin
  Result := inherited HasVisibleTouchScrollUI or PreviewScrollBar.Visible;
end;

procedure TdxCustomImageSlider.HideScrollBars;
begin
  inherited HideScrollBars;
  if not IsPopupScrollBars then
    PreviewScrollBar.Visible := False;
end;

procedure TdxCustomImageSlider.InitializePreviewScrollBar;
var
  APageSize, AMax: Integer;
const
  AKind: array[Boolean] of TScrollBarKind = (sbHorizontal, sbVertical);
begin
  if FPreviewScrollBar = nil then
    Exit;
  PreviewScrollBar.Kind := AKind[GetPreviewPosition in [posLeft, posRight]];
  GetPreviewScrollBarInfo(APageSize, AMax);
  if PreviewScrollPosition + APageSize > AMax then
    PreviewScrollPosition := AMax - APageSize;
  PreviewScrollBar.SetScrollParams(0, AMax, PreviewScrollPosition, APageSize, False);
  PreviewScrollBar.BoundsRect := GetPreviewScrollBarBounds;
  PreviewScrollBar.Visible := IsPreviewScrollBarVisible;
end;

procedure TdxCustomImageSlider.InitScrollBarsParameters;
var
  AVisible: Boolean;
  AKind: TScrollBarKind;
begin
  for AKind := sbHorizontal to sbVertical do
  begin
    AVisible := (ScrollMode = issmScrollBar) and (ScrollBarKind = AKind);
    SetScrollBarInfo(AKind, 0, IfThen(Visible, Count - 1, 0), 1, 1,
      IfThen(Visible, ItemIndex, 0), AVisible, True);
  end;
  InitializePreviewScrollBar;
end;

procedure TdxCustomImageSlider.InvalidateSliderButtons;
begin
  if IsSliderButtonsAvailable then
  begin
    if FButtonPrev <> nil then
      InvalidateRect(FButtonPrev.VisibleBounds, False);
    if FButtonNext <> nil then
      InvalidateRect(FButtonNext.VisibleBounds, False);
  end;
end;

function TdxCustomImageSlider.IsPreviewVisible: Boolean;
begin
  Result := (GetPreviewPosition <> posNone) and (Count >= 1);
end;

function TdxCustomImageSlider.IsPreviewScrollBarVisible: Boolean;
var
  APage, AMax: Integer;
begin
  GetPreviewScrollBarInfo(APage, AMax);
  Result := IsPreviewVisible and (APage < AMax);
end;

procedure TdxCustomImageSlider.GestureScroll(ADeltaX, ADeltaY: Integer);
begin
  if FScrollNeeded then
  begin
    if IsVerticalScrollMode and (ADeltaY > 0) or
      not IsVerticalScrollMode and (ADeltaX > 0) then
        GoToPrevImage
      else
        GoToNextImage;
    FScrollNeeded := False;
  end;
end;

function TdxCustomImageSlider.GetMainScrollBarsClass: TcxControlCustomScrollBarsClass;
begin
  if IsPopupScrollbars then
    Result := inherited GetMainScrollBarsClass
  else
    Result := TcxControlScrollBars;
end;


procedure TdxCustomImageSlider.BoundsChanged;
begin
  InitializePreviewScrollBar;
  inherited BoundsChanged;
  LayoutChanged;
end;

procedure TdxCustomImageSlider.CalculatePreviewLayout;
var
  ACol, ARow: Longint;
  AIndex, DX, DY, AOffset: Integer;
  APreviewBounds, APageBounds, R: TRect;
const
  AContentPosition: array[Boolean, TcxPosition] of TcxPosition = ((posNone, posLeft, posLeft, posTop, posTop),
    (posNone, posRight, posRight, posTop, posTop));
  ASplitterPosition: array[TcxPosition] of TcxPosition = (posNone, posLeft, posRight, posTop, posBottom);
begin
  if not IsPreviewVisible then
    Exit;
  FSplitter := AddViewInfo(GetPreviewSplitterBounds, -1, DrawSplitter, SplitterClick);
  APreviewBounds := GetPartBounds(GetPreviewBounds, AContentPosition[UseRightToLeftScrollBar, GetPreviewPosition],
    GetPreviewSize - GetPreviewSplitterSize - IfThen(not IsPopupScrollBars, GetPreviewScrollBarSize));
  if UseRightToLeftScrollBar then
  begin
    if GetPreviewPosition = posLeft then
      OffsetRect(APreviewBounds, -GetPreviewSplitterSize, 0);
  end
  else
    if GetPreviewPosition = posRight then
      OffsetRect(APreviewBounds, GetPreviewSplitterSize, 0)
    else
      if GetPreviewPosition = posBottom then
        OffsetRect(APreviewBounds, 0, GetPreviewSplitterSize);

  AOffset := PreviewOptions.IndentsWidth div 2;
  APageBounds := cxRectSetOrigin(cxRect(PreviewOptions.ThumbnailSize.Size), cxRectOffset(APreviewBounds,  AOffset, AOffset).TopLeft);
  Inc(APageBounds.Bottom, cxTextHeight(Font) + ScaleFactor.Apply(cxTextOffset));

  DX := cxRectWidth(APageBounds) + AOffset;
  DY := cxRectHeight(APageBounds) + AOffset;
  if GetPreviewPosition in [posTop, posBottom] then
  begin
    AOffset := Max(0, cxRectHeight(APreviewBounds) - PreviewOptions.Indent -
      (PreviewOptions.ThumbnailSize.Height + PreviewOptions.IndentsHeight - PreviewOptions.Indent) * PreviewRowCount);
    OffsetRect(APageBounds, -PreviewScrollPosition, AOffset div 2)
  end
  else
  begin
    AOffset := Max(0, cxRectWidth(APreviewBounds) - PreviewOptions.Indent -
      (PreviewOptions.ThumbnailSize.Width + PreviewOptions.IndentsWidth - PreviewOptions.Indent) * PreviewRowCount);
    OffsetRect(APageBounds, AOffset div 2, -PreviewScrollPosition);
  end;

  for AIndex := 0 to Count -1 do
  begin
    ACol := AIndex mod PreviewRowCount;
    ARow := AIndex div PreviewRowCount;
    if GetPreviewPosition in [posTop, posBottom] then
      ExchangeLongWords(ACol, ARow);
    R := cxRectOffset(APageBounds, DX * ACol , DY * ARow);
    if UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, APreviewBounds);
    AddViewInfo(R, APreviewBounds, AIndex, DrawPreviewPage, PagePreviewClick);
    if cxRectIntersect(R, APreviewBounds) then
      ValidateThumbnail(AIndex);
  end;
end;

procedure TdxCustomImageSlider.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  PreviewOptions.ChangeScale(M, D);
end;

function TdxCustomImageSlider.GetClientBounds: TRect;
begin
  if GetPreviewPosition = posNone then
    Result := inherited GetClientBounds
  else
  begin
    Result := cxRectInflate(Bounds, -BorderSize);
    case GetPreviewPosition of
      posLeft:
        Result.Left := GetPreviewBounds.Right;
      posRight:
        Result.Right := GetPreviewBounds.Left;
      posTop:
        Result.Top := GetPreviewBounds.Bottom;
      posBottom:
        Result.Bottom := GetPreviewBounds.Top;
    end;
    if not IsPopupScrollBars then
    begin
      if HScrollBarVisible then
        Dec(Result.Bottom, GetHScrollBarAreaHeight);
      if VScrollBarVisible then
        if UseRightToLeftScrollBar then
          Inc(Result.Left, GetVScrollBarAreaWidth)
        else
          Dec(Result.Right, GetVScrollBarAreaWidth);
    end;
  end;
end;

function TdxCustomImageSlider.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  Result := inherited GetCurrentCursor(X, Y);
  if CheckSplitterHitTest(X, Y) then
    Result := SplitterCursors[GetPreviewPosition in [posRight, posLeft]]
end;

function TdxCustomImageSlider.GetDragAndDropObjectClass: TcxDragAndDropObjectClass;
begin
  Result := TdxImageSliderResizePreviewDragAndDropObject;
end;

function TdxCustomImageSlider.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
const
  AWheelScrollKind: array[TcxPosition] of TcxMouseWheelScrollingKind =
    (mwskNone, mwskHorizontal, mwskHorizontal, mwskVertical, mwskVertical);
begin
  Result := AWheelScrollKind[GetPreviewPosition];
end;

function TdxCustomImageSlider.GetPreviewBounds: TRect;
begin
  if GetPreviewPosition = posNone then
  begin
    Result := cxNullRect;
    Exit;
  end;
  Result := GetPartBounds(cxRectInflate(Bounds, -BorderSize), GetPreviewPosition, GetPreviewSize);
end;

function TdxCustomImageSlider.GetPreviewPosition: TcxPosition;
begin
  Result := PreviewOptions.Position;
  if (Result in [posLeft, posRight]) and UseRightToLeftAlignment then
    if Result = posLeft then
      Result := posRight
    else
      Result := posLeft;
end;

function TdxCustomImageSlider.GetPreviewScrollBarBounds: TRect;
const
  AScrollBarPosition: array[TcxPosition] of TcxPosition = (posNone, posRight, posRight, posBottom, posBottom);
var
  R: TRect;
begin
  R := GetPreviewBounds;
  case GetPreviewPosition of
    posLeft:
      Dec(R.Right, GetPreviewSplitterSize);
    posRight:
      Inc(R.Left, GetPreviewSplitterSize);
    posTop:
      Dec(R.Bottom, GetPreviewSplitterSize);
    posBottom:
      Inc(R.Top, GetPreviewSplitterSize);
  end;
  Result := GetPartBounds(R, AScrollBarPosition[GetPreviewPosition], GetPreviewScrollBarSize);
  if UseRightToLeftScrollBar then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, R);
end;

function TdxCustomImageSlider.GetPreviewSplitterBounds: TRect;
const
  ASplitterPosition: array[TcxPosition] of TcxPosition = (posNone, posRight, posLeft, posBottom, posTop);
begin
  Result := GetPartBounds(GetPreviewBounds, ASplitterPosition[GetPreviewPosition], GetPreviewSplitterSize);
end;

function TdxCustomImageSlider.InternalMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := IsPreviewVisible and PtInRect(GetPreviewBounds, ScreenToClient(MousePos)) and
    (MouseWheelScrollingKind <> mwskNone);
  if Result then
    PreviewScrollPosition := PreviewScrollPosition - Math.Sign(WheelDelta) * dxImageSliderPreviewScrollStep;
end;

procedure TdxCustomImageSlider.LayoutChanged;
var
  R, AButtonRect: TRect;
  AButtonSize, AOffset: TSize;
begin
  if IsLoading or IsDestroying then
    Exit;
  R := ClientBounds;
  AButtonSize := LookAndFeelPainter.ScaledSliderButtonSize(adLeft, ScaleFactor);
  AOffset := cxSize(AButtonSize.cx div 2, AButtonSize.cy div 2);

  ViewInfoItems.Clear;
  FButtonPrev := nil;
  FButtonNext := nil;

  if IsSliderButtonsAvailable then
  begin
{   todo: // for top bottom buttons
    if PreviewOptions.Position in [posTop, posBottom] then
    begin
      FButtonPrev := AddViewInfo(cxRectBounds((R.Left + R.Right - AButtonSize.cy) div 2, R.Top + AOffset.cy,
        AButtonSize.cx, AButtonSize.cy), 2, DrawButton, NavigationButtonClick);
      FButtonNext := AddViewInfo(cxRectBounds((R.Left + R.Right - AButtonSize.cy) div 2, R.Bottom - AOffset.cy - AButtonSize.cy,
        AButtonSize.cx, AButtonSize.cy), 3, DrawButton, NavigationButtonClick);
    end
    else  }
    begin
      AButtonRect := cxRectBounds(R.Left + AOffset.cx, (R.Top + R.Bottom - AButtonSize.cy) div 2,
        AButtonSize.cx, AButtonSize.cy);
      if UseRightToLeftAlignment then
        AButtonRect := TdxRightToLeftLayoutConverter.ConvertRect(AButtonRect, R);
      FButtonPrev := AddViewInfo(AButtonRect, 0, DrawButton, NavigationButtonClick);
      AButtonRect := cxRectBounds(R.Right - AButtonSize.cx - AOffset.cx, (R.Top + R.Bottom - AButtonSize.cy) div 2,
        AButtonSize.cx, AButtonSize.cy);
      if UseRightToLeftAlignment then
        AButtonRect := TdxRightToLeftLayoutConverter.ConvertRect(AButtonRect, R);
      FButtonNext := AddViewInfo(AButtonRect, 1, DrawButton, NavigationButtonClick);
    end;
  end;

  if IsPreviewVisible then
    CalculatePreviewLayout;

  GoToImage(ItemIndex);
  UpdateScrollBars;
  Invalidate;
end;

procedure TdxCustomImageSlider.Loaded;
begin
  inherited Loaded;
  ItemIndex := ItemIndex;
end;

procedure TdxCustomImageSlider.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  LayoutChanged;
end;

function TdxCustomImageSlider.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TdxCustomImageSlider.ScaleFactorChanged;
begin
  FThumbnails.Clear;
  inherited;
end;

procedure TdxCustomImageSlider.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollCode = scEndScroll then
    Exit;
  Caption := '';
  case AScrollCode of
    scLineUp, scPageUp:
      GoToImage(ItemIndex - 1);
    scLineDown, scPageDown:
      GoToImage(ItemIndex + 1);
    scTrack:
      begin
        // todo show thumbanil
      end;
    scPosition:
      GoToImage(AScrollPos);
    scTop:
      GoToImage(0);
    scBottom:
      GoToImage(Count - 1);
  end;
  AScrollPos := ItemIndex;
end;

procedure TdxCustomImageSlider.ScrollPreview(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  APageSize, AMax: Integer;
begin
  if ScrollCode = scEndScroll then
    Exit;
  GetPreviewScrollBarInfo(APageSize, AMax);
  case ScrollCode of
    scLineUp, scLineDown:
      PreviewScrollPosition := PreviewScrollPosition + Sign[ScrollCode = scLineDown] * dxImageSliderPreviewScrollStep;
    scPageUp, scPageDown:
      PreviewScrollPosition := PreviewScrollPosition + Sign[ScrollCode = scPageDown] * APageSize;
    scTrack, scPosition:
      PreviewScrollPosition := ScrollPos;
    scTop:
      PreviewScrollPosition := 0;
    scBottom:
      PreviewScrollPosition := AMax;
  end;
  ScrollPos := PreviewScrollPosition;
end;

function TdxCustomImageSlider.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := CheckSplitterHitTest(P.X, P.Y);
end;

procedure TdxCustomImageSlider.ValidateThumbnail(AIndex: Integer);
var
  ABitmap: TcxBitmap32;
  ACustomThumbnail: TGraphic;
  ASize: TSize;
  AThumbnail: TdxSmartGlyph;
begin
  if (AIndex < FThumbnails.Count) and (FThumbnails[AIndex] <> nil) then
    Exit;

  FThumbnails.Count := Max(Count, FThumbnails.Count);
  if not MultiFrameImage.Empty then
    ASize := dxGetImageSize(MultiFrameImage, ScaleFactor)
  else
    if Images <> nil then
      ASize := dxGetImageSize(Images.Items[AIndex].Picture, ScaleFactor)
    else
      ASize := cxNullSize;

  if ASize.cx = 0 then
    Exit;
  ABitmap := TcxBitmap32.CreateSize(ASize.cx, ASize.cy);
  try
    ACustomThumbnail := nil;
    if Assigned(OnGetThumbnail) then
      OnGetThumbnail(Self, AIndex, ACustomThumbnail);
    if ACustomThumbnail <> nil then
      ABitmap.cxCanvas.StretchDraw(ABitmap.ClientRect, ACustomThumbnail)
    else
      DrawFrame(ABitmap.cxCanvas, ABitmap.ClientRect, AIndex, ifmProportionalStretch);

    AThumbnail := TdxSmartGlyph.CreateFromBitmap(ABitmap);
    AThumbnail.Resize(PreviewOptions.ThumbnailSize.Size);
    AThumbnail.SourceDPI := ScaleFactor.Apply(dxDefaultDPI);
    FThumbnails.List[AIndex] := AThumbnail;
  finally
    ABitmap.Free;
  end;
end;

procedure TdxCustomImageSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  MousePressed := Button = mbLeft;
  HotTrackItem := ItemFromPoint(X, Y);
end;

procedure TdxCustomImageSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  HotTrackItem := ItemFromPoint(X, Y);
  InvalidateSliderButtons;
end;

procedure TdxCustomImageSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItem: TViewInfoItem;
begin
  inherited MouseUp(Button, Shift, X, Y);
  AItem := HotTrackItem;
  if not IsDesigning and (AItem <> nil) and (AItem = ItemFromPoint(X, Y)) and
    (Button = mbLeft) and MousePressed {and (Animation = nil)} then
    AItem.OnClick(HotTrackItem);
  MousePressed := MousePressed and (Button <> mbLeft);
  HotTrackItem := ItemFromPoint(X, Y);
  InvalidateSliderButtons;
end;

procedure TdxCustomImageSlider.MouseEnter(AControl: TControl);
begin
  BeginMouseTracking(Self, Bounds, Self);
  InvalidateSliderButtons;
end;

procedure TdxCustomImageSlider.MouseLeave(AControl: TControl);
begin
  EndMouseTracking(Self);
  InvalidateSliderButtons;
  HotTrackItem := nil;
end;

function TdxCustomImageSlider.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

procedure TdxCustomImageSlider.Paint;
var
  AIndex: Integer;
begin
  inherited Paint;
  if IsPreviewVisible then
    LookAndFeelPainter.DrawPrintPreviewBackground(Canvas, GetPreviewBounds);
  if IsValidItemIndex(ItemIndex) and Canvas.RectVisible(ClientBounds) then
  begin
    Canvas.SaveClipRegion;
    try
      Canvas.IntersectClipRect(ClientBounds);
      if Animation <> nil then
        Animation.Draw(Canvas.Canvas, ClientBounds)
      else
        DrawFrame(Canvas, ClientBounds, ItemIndex,  ImageFitMode);
    finally
      Canvas.RestoreClipRegion;
    end;
  end;
  for AIndex := 0 to ViewInfoItems.Count - 1 do
    ViewInfoItems[AIndex].Draw(Canvas);
end;

procedure TdxCustomImageSlider.StopAnimation;
begin
  if FAnimation <> nil then
  begin
    FAnimation.Terminate;
    FAnimation := nil;
  end;
end;

procedure TdxCustomImageSlider.SetPaintRegion;
begin
  Canvas.IntersectClipRect(cxRectInflate(Bounds, -BorderSize));
end;

function TdxCustomImageSlider.AddViewInfo(const ABounds: TRect; AObjectID: Integer;
  ADrawItem: TDrawItemHandler; AClickHandler: TNotifyEvent): TViewInfoItem;
begin
  Result := AddViewInfo(ABounds, ABounds, AObjectID, ADrawItem, AClickHandler);
end;

function TdxCustomImageSlider.AddViewInfo(const ABounds, AVisibleBounds: TRect; AObjectID: Integer;
  ADrawItem: TDrawItemHandler; AClickHandler: TNotifyEvent): TViewInfoItem;
begin
  Result := TViewInfoItem.Create;
  Result.Bounds := ABounds;
  Result.VisibleBounds := AVisibleBounds;
  Result.ObjectID := AObjectID;
  Result.OnClick := AClickHandler;
  Result.OnDrawItem := ADrawItem;
  ViewInfoItems.Add(Result);
end;

function TdxCustomImageSlider.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

function TdxCustomImageSlider.CheckSplitterHitTest(X, Y: Integer): Boolean;
begin
  Result := (FSplitter <> nil) and (ItemFromPoint(X, Y) = FSplitter);
end;

function TdxCustomImageSlider.GetCount: Integer;
begin
  if (MultiFrameImage <> nil) and not MultiFrameImage.Empty then
    Result := MultiFrameImage.AnimationFrameCount
  else
    if Images <> nil then
      Result := Images.Count
    else
      Result := 0;
end;

function TdxCustomImageSlider.GetHotTrackItem: TViewInfoItem;
var
  AMousePos: TPoint;
begin
  Result := FHotTrackItem;
  if MousePressed then
  begin
    AMousePos := GetMouseCursorClientPos;
    if ItemFromPoint(AMousePos.X, AMousePos.Y) <> Result then
      Result := nil;
  end;
end;

function TdxCustomImageSlider.GetPartBounds(const ABounds: TRect; APosition: TcxPosition; ASize: Integer): TRect;
begin
  if APosition = posNone then
  begin
    Result := cxNullRect;
    Exit;
  end;
  Result := ABounds;
  case APosition of
    posLeft:
      Result.Right := Result.Left + ASize;
    posRight:
      Result.Left := Result.Right - ASize;
    posTop:
      Result.Bottom := Result.Top + ASize;
    posBottom:
      Result.Top := Result.Bottom - ASize;
  end;
end;

function TdxCustomImageSlider.GetPreviewRowCount: Integer;
begin
  Result := (PreviewOptions.ActuallySize - PreviewOptions.Indent);
  if GetPreviewPosition in [posLeft, posRight] then
    Result := Result div (PreviewOptions.ThumbnailSize.Width + PreviewOptions.IndentsWidth - PreviewOptions.Indent)
  else
    Result := Result div (PreviewOptions.ThumbnailSize.Height + PreviewOptions.IndentsHeight - PreviewOptions.Indent);
  Result := Max(1, Result);
end;

function TdxCustomImageSlider.GetPreviewScrollBarSize: Integer;
begin
  Result := 0;
  if IsPreviewScrollBarVisible then
  begin
    if GetPreviewPosition in [posTop, posBottom] then
      Result := GetScrollBarSize.cy
    else
      Result := GetScrollBarSize.cx;
  end;
end;

procedure TdxCustomImageSlider.GetPreviewScrollBarInfo(var APageSize, AMax: Integer);
var
  R: TRect;
  AIndent: Integer;
begin
  R := cxRectInflate(Bounds, -BorderSize);
  if not IsPopupScrollBars then
  begin
    if HScrollBarVisible then
      Dec(R.Bottom, HScrollBar.Height);
    if VScrollBarVisible then
      Dec(R.Right, VScrollBar.Width);
  end;
  AIndent := PreviewOptions.Indent + dxImageSliderThumbnailFrameWidth + dxImageSliderThumbnailSelectionFrameWidth;
  if GetPreviewPosition in [posLeft, posRight] then
  begin
    APageSize := cxRectHeight(R);
    AMax := PreviewOptions.ThumbnailSize.Height + cxTextHeight(Font) + ScaleFactor.Apply(cxTextOffset);
  end
  else
  begin
    APageSize := cxRectWidth(R);
    AMax := PreviewOptions.ThumbnailSize.Width;
  end;
  AMax := Max((AMax + AIndent) * Ceil(Count / PreviewRowCount) + PreviewOptions.Indent,  APageSize);
end;

function TdxCustomImageSlider.GetPreviewSize: Integer;
begin
  Result := PreviewOptions.ActuallySize + IfThen(not IsPopupScrollBars, GetPreviewScrollBarSize) + GetPreviewSplitterSize;
end;

function TdxCustomImageSlider.GetPreviewSplitterSize: Integer;
var
  ASize: TSize;
begin
  if (GetPreviewPosition = posNone) or not PreviewOptions.Resizable then
    Result := 0
  else
  begin
    ASize := LookAndFeelPainter.GetScaledSplitterSize(GetPreviewPosition in [posTop, posBottom], ScaleFactor);
    if GetPreviewPosition in [posTop, posBottom] then
      Result := Max(ASize.cy, ScaleFactor.Apply(dxImageSliderPreviewSplitterDefaultSize))
    else
      Result := Max(ASize.cx, ScaleFactor.Apply(dxImageSliderPreviewSplitterDefaultSize));
  end;
end;

function TdxCustomImageSlider.GetThumbnail(AIndex: Integer): TdxSmartImage;
begin
  ValidateThumbnail(AIndex);
  Result := FThumbnails[AIndex] as TdxSmartImage;
end;

function TdxCustomImageSlider.IsValidItemIndex(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
end;

function TdxCustomImageSlider.IsVerticalScrollMode: Boolean;
begin
  Result := (ScrollMode = issmScrollBar) and (ScrollBarKind = sbVertical);
end;

function TdxCustomImageSlider.ItemFromPoint(X, Y: Integer): TViewInfoItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ViewInfoItems.Count - 1 do
    if PtInRect(ViewInfoItems[I].ClipRect, Point(X, Y)) then
    begin
      Result := ViewInfoItems[I];
      Break;
    end;
end;

procedure TdxCustomImageSlider.SetAnimationTime(AValue: Integer);
begin
  FAnimationTime := Max(0, AValue);
end;

procedure TdxCustomImageSlider.SetHotTrackItem(AValue: TViewInfoItem);
begin
  if FHotTrackItem <> nil then
    InvalidateRect(FHotTrackItem.VisibleBounds, False);
  if (FHotTrackItem = AValue) or MousePressed and (FHotTrackItem <> nil) then
    Exit;
  FHotTrackItem := AValue;
  if FHotTrackItem <> nil then
    InvalidateRect(FHotTrackItem.VisibleBounds, False);
end;

procedure TdxCustomImageSlider.SetImageFitMode(AValue: TcxImageFitMode);
begin
  if ImageFitMode <> AValue then
  begin
    FImageFitMode := AValue;
    LayoutChanged;
  end;
end;

procedure TdxCustomImageSlider.SetImages(AValue: TcxImageCollection);
begin
  if Images <> AValue then
  begin
    if Images <> nil then
      Images.RemoveListener(Self);
    FImages := AValue;
    if Images <> nil then
      Images.AddListener(Self);
    ImageChanged(nil);
  end;
end;

procedure TdxCustomImageSlider.SetItemIndex(AValue: Integer);
begin
  if csLoading in ComponentState then
    FItemIndex := AValue
  else
  begin
    AValue := Min(Max(AValue, 0), Count - 1);
    if ItemIndex <> AValue then
    begin
      FItemIndex := AValue;
      LayoutChanged;
    end;
  end;
end;

procedure TdxCustomImageSlider.SetMultiFrameImage(AValue: TdxSmartGlyph);
begin
  FMultiFrameImage.Assign(AValue);
  ImageChanged(nil);
end;

procedure TdxCustomImageSlider.SetPreviewOptions(AValue: TdxImageSliderPreviewOptions);
begin
  FPreviewOptions.Assign(AValue);
end;

procedure TdxCustomImageSlider.SetPreviewScrollPosition(AValue: Integer);
var
  I, ADelta, AMax, APageSize: Integer;
begin
  GetPreviewScrollBarInfo(APageSize, AMax);
  AValue := Max(0, Min(AValue, AMax - APageSize + 1));

  if AValue = FPreviewScrollPosition then
    Exit;
  ADelta := FPreviewScrollPosition - AValue;
  FPreviewScrollPosition := AValue;
  InitializePreviewScrollBar;
  for I := ViewInfoItems.Count - Count to ViewInfoItems.Count - 1 do
    if GetPreviewPosition in [posTop, posBottom] then
      if UseRightToLeftAlignment then
        OffsetRect(ViewInfoItems[I].Bounds, -ADelta, 0)
      else
        OffsetRect(ViewInfoItems[I].Bounds, ADelta, 0)
    else
      OffsetRect(ViewInfoItems[I].Bounds, 0, ADelta);
  InvalidateRect(GetPreviewBounds, False);
end;

procedure TdxCustomImageSlider.SetScrollBarKind(AValue: TScrollBarKind);
begin
  if ScrollBarKind <> AValue then
  begin
    FScrollBarKind := AValue;
    UpdateScrollBars;
  end;
end;

procedure TdxCustomImageSlider.SetScrollMode(AValue: TdxImageSliderScrollMode);
begin
  if ScrollMode <> AValue then
  begin
    FScrollMode := AValue;
    UpdateScrollBars;
  end;
end;

procedure TdxCustomImageSlider.TrackingCallerMouseLeave;
begin
  MouseLeave(nil);
end;

procedure TdxCustomImageSlider.ImageChanged(Sender: TObject);
begin
  FPreviewScrollPosition := 0;
  FThumbnails.Clear;
  LayoutChanged;
end;

procedure TdxCustomImageSlider.InitScrollBars;
begin
  inherited InitScrollBars;
  FPreviewScrollBar.InitControl;
end;

procedure TdxCustomImageSlider.NavigationButtonClick(Sender: TObject);
begin
  if TViewInfoItem(Sender).ObjectID in [0, 2] then
    GotoPrevImage
  else
    GotoNextImage;
end;

procedure TdxCustomImageSlider.PagePreviewClick(Sender: TObject);
var
  R, R1: TRect;
begin
  R := TViewInfoItem(Sender).Bounds;
  R1 := GetPreviewBounds;
  if GetPreviewPosition in [posTop, posBottom] then
  begin
    R := cxRectRotate(R);
    R1 := cxRectRotate(R1);
  end;
  if R.Top < R1.Top then
    PreviewScrollPosition := PreviewScrollPosition - (Abs(R.Top) + PreviewOptions.Indent)
  else
    if R.Bottom > R1.Bottom then
      PreviewScrollPosition := PreviewScrollPosition + (R.Bottom - R1.Bottom) + PreviewOptions.Indent;
  GoToImage(TViewInfoItem(Sender).ObjectID);
end;

procedure TdxCustomImageSlider.SplitterClick(Sender: TObject);
begin
end;

procedure TdxCustomImageSlider.TViewInfoItem.Click;
begin
  if Assigned(OnClick) then
    OnClick(Self);
end;

function TdxCustomImageSlider.TViewInfoItem.ClipRect: TRect;
begin
  cxRectIntersect(Result, Bounds, VisibleBounds);
end;

procedure TdxCustomImageSlider.TViewInfoItem.Draw(ACanvas: TcxCanvas);
begin
  if Assigned(OnDrawItem) and ACanvas.RectVisible(VisibleBounds) then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.IntersectClipRect(VisibleBounds);
      if ACanvas.RectVisible(cxRectInflate(Bounds, dxImageSliderThumbnailIndent)) then
        OnDrawItem(ACanvas, Self);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;


{ TdxCustomImageSlider.TPreviewTouchScrollUIHelper }

constructor TdxCustomImageSlider.TPreviewTouchScrollUIHelper.Create(AOwner: TdxCustomImageSlider);
begin
  inherited Create;
  FHybridScrollbarsManager := TdxHybridScrollbarsManager.Create(Self);
  FSlider := AOwner;
end;

destructor TdxCustomImageSlider.TPreviewTouchScrollUIHelper.Destroy;
begin
  FreeAndNil(FHybridScrollbarsManager);
  inherited Destroy;
end;

//IdxTouchScrollUIOwner

procedure TdxCustomImageSlider.TPreviewTouchScrollUIHelper.CheckUIPosition;
begin
  (FSlider as IdxTouchScrollUIOwner).CheckUIPosition;
end;

function TdxCustomImageSlider.TPreviewTouchScrollUIHelper.GetOwnerControl: TcxControl;
begin
  Result := (FSlider as IdxTouchScrollUIOwner).GetOwnerControl;
end;

function TdxCustomImageSlider.TPreviewTouchScrollUIHelper.HasVisibleUI: Boolean;
begin
  Result := FSlider.PreviewScrollBar.Visible;
end;

procedure TdxCustomImageSlider.TPreviewTouchScrollUIHelper.HideUI;
begin
  FSlider.PreviewScrollBar.Visible := False;
end;

//IdxHybridScrollbarOwner

function TdxCustomImageSlider.TPreviewTouchScrollUIHelper.GetBaseColor: TColor;
begin
  Result := FSlider.GetHybridScrollbarBaseColor;
end;

function TdxCustomImageSlider.TPreviewTouchScrollUIHelper.GetManager: TdxHybridScrollbarsManager;
begin
  Result := FHybridScrollbarsManager;
end;

procedure TdxCustomImageSlider.TPreviewTouchScrollUIHelper.Invalidate;
begin
  FSlider.PreviewScrollBar.Invalidate;
end;

{ TdxCustomImageSlider.TPreviewGestureHelper }

constructor TdxCustomImageSlider.TPreviewGestureHelper.Create(AOwner: TdxCustomImageSlider);
begin
  inherited Create;
  FSlider := AOwner;
end;

function TdxCustomImageSlider.TPreviewGestureHelper.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := FSlider.AllowGesture(AGestureId);
end;

function TdxCustomImageSlider.TPreviewGestureHelper.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := False;
  if FSlider.GetPreviewPosition in [posLeft, posRight] then
    Result := AScrollKind = sbVertical
  else
    if FSlider.GetPreviewPosition in [posTop, posBottom] then
      Result := AScrollKind = sbHorizontal;
end;

procedure TdxCustomImageSlider.TPreviewGestureHelper.BeginGestureScroll(APos: TPoint);
begin
end;

procedure TdxCustomImageSlider.TPreviewGestureHelper.EndGestureScroll;
begin
end;

procedure TdxCustomImageSlider.TPreviewGestureHelper.GestureScroll(ADeltaX, ADeltaY: Integer);
var
  ANewPos: Integer;
  ADelta: Integer;
begin
  if FSlider.IsPopupScrollBars or FSlider.PreviewScrollBar.Visible then
  begin
    if FSlider.PreviewScrollBar.Kind = sbHorizontal then
      ADelta := ADeltaX
    else
      ADelta := ADeltaY;

    ANewPos := FSlider.PreviewScrollPosition - ADelta;
    if ADelta <> 0 then
      FSlider.PreviewScrollPosition := ANewPos;
  end;

end;
function TdxCustomImageSlider.TPreviewGestureHelper.GetPanOptions: Integer;
begin
  Result := FSlider.GetPanOptions;
end;

function TdxCustomImageSlider.TPreviewGestureHelper.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(FSlider.GetPreviewBounds, APoint);
end;

function TdxCustomImageSlider.TPreviewGestureHelper.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := FSlider.NeedPanningFeedback(AScrollKind);
end;

end.




