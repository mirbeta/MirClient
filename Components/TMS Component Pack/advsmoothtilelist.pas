{**************************************************************************}
{ TAdvSmoothTileList Component                                             }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2011 - 2015                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothTileList;

{$I TMSDEFS.inc}

interface

uses
  Forms, Classes, SysUtils, Controls, GDIPFill, Windows, AdvHintInfo, AdvSmoothTheme,
  AdvStyleIF, Math, ExtCtrls, Messages, Graphics, AdvGDIP, ImgList,
  GDIPPictureContainer, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  CLICKMARGIN = 5;

  //version history
  // v1.0.0.0 : First Release
  // v1.0.1.0 : New : Transparent property
  // v1.0.2.0 : New : HTMLAlign textposition to position text with html align tag.
  //          : Fixed : Issue with changing visible state from OnDblClick
  // v1.1.0.0 : New : Metro Style Support
  //          : Fixed : Issue with double-clicking tile and moving afterwards
  // v1.1.1.0 : New : OnTileFont event to customize text font per tile
  //          : Fixed : Issue with text and regions
  //          : Fixed : Issue with clearing tiles
  //          : Fixed : Issue with bullets and arrows in navigation
  // v1.1.1.1 : Fixed : Issue with AllowMove and timer
  // v1.1.1.2 : Improved : Click Margin for touch based screens
  // v1.1.1.3 : Fixed : Issue with version number not displaying
  // v1.1.1.4 : Fixed : Issue with invalid floating point error in older delphi versions
  // v1.1.2.0 : New : Property AllowScroll added
  // v1.1.2.1 : Fixed : Issue with drawing image when setting text position to custom
  // v1.2.0.0 : New : checkbox support
  // v1.2.0.1 : Improved : Clear method to perform initial loading when using subtiles
  // v1.3.0.0 : Fixed : Issue with header and footer caption
  //          : Fixed : Issue with tile delete
  //          : New : Header and Footer Caption location and Caption top and left properties
  // v1.4.0.0 : New : Windows 8, Office 2013 styles added
  // v1.4.1.0 : New : NavigateToTile and SelectTile functionality
  // v1.4.1.1 : Improved : ToggleMaximized called after OnTileDblClick event
  // v1.4.2.0 : New : Anchor hints
  // v1.4.3.0 : New : GoHome method added
  //          : New : public property Tiles.Tag property added
  //          : New : public property Tiles.ParentTiles added
  //          : New : public property WorkTiles added
  // v1.4.3.1 : New : OnLevelUp and OnLevelDown events
  // v1.5.0.0 : New : Windows 10, Office 2016 styles added
  // v1.5.0.1 : Improved: NavigateAndSelectSubTile and SelectSubTile methods on for each tile in TAdvSmoothTileList
  //          : Improved: IsSubTileMode to determine when the tile list has reached subtiles after a drill-down animation in TAdvSmoothTileList


type
  {$IFDEF DELPHIXE3_LVL}
  TImageIndex = System.UITypes.TImageIndex;
  {$ENDIF}

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;


  TAdvSmoothTileList = class;
  TAdvSmoothTile = class;
  TAdvSmoothTiles = class;

  {$IFNDEF DELPHI2006_LVL}
  TMargins = class(TPersistent)
  private
    FOwner: TAdvSmoothTileList;
    FRight: integer;
    FBottom: integer;
    FTop: integer;
    FLeft: integer;
    FOnChange: TNotifyEvent;
    procedure SetBottom(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TAdvSmoothTileList);
    procedure Assign(Source: TPersistent); override;
  published
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Right: integer read FRight write SetRight;
    property Bottom: integer read FBottom write SetBottom;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  {$ENDIF}

  TTileListToggleMode = (tmAuto, tmExit, tmEnter);
  TTileListOption = (toAllowMove, toAllowDelete, toAllowMaximize);
  TTileListOptions = set of TTileListOption;  
  TAnimationRectangleMode = (rmNormal, rmDown, rmBack, rmDownReverse, rmBackReverse);
  TTileState = (tsNormal, tsHovered, tsSelected, tsDisabled, tsMaximized);
  TTileListCheckBoxPosition = (cpTopLeft, cpTopCenter, cpTopRight, cpCenterLeft, cpCenterCenter, cpCenterRight, cpBottomLeft, cpBottomCenter, cpBottomRight, cpCustom);
  TTileListTextPosition = (tpTopLeft, tpTopCenter, tpTopRight, tpCenterLeft, tpCenterCenter, tpCenterRight, tpBottomLeft, tpBottomCenter, tpBottomRight, tpCustom, tpHTMLAlign);
  TTileListMode = (tmView, tmDelete, tmEdit, tmContent, tmContentNavigation);
  TTileListFontEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; AFont: TFont) of object;
  TTileListFillEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; Fill: TGDIPFill) of object;
  TTileListTextEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; var Text: String) of object;
  TTileListEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState) of object;
  TTileListPageChangedEvent = procedure(Sender: TObject; PageIndex: Integer) of object;
  TTileListDrawBulletEvent = procedure(Sender: TObject; g: TGPGraphics; BulletIndex: Integer; BulletRect, NavigationRect: TGPRectF; var DefaultDraw: Boolean) of object;
  TTileListBeforeDraw = procedure(Sender: TObject; g: TGPGraphics; Tile: TAdvSmoothTile; TileRect: TGPRectF; var DefaultDraw: Boolean) of object;
  TTileListAfterDraw = procedure(Sender: TObject; g: TGPGraphics; Tile: TAdvSmoothTile; TileRect: TGPRectF) of object;
  TTileListBackGroundBeforeDraw = procedure(Sender: TObject; g: TGPGraphics; Rect: TGPRectF; var DefaultDraw: Boolean) of object;
  TTileListBackGroundAfterDraw = procedure(Sender: TObject; g: TGPGraphics; Rect: TGPRectF) of object;
  TTileListHintEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; var Hint: String) of object;
  TTileListIndicatorEvent = procedure(Sender: TObject; Tile: TAdvsmoothTile; State: TTileState; var Indicator: String) of object;
  TTileListIndicatorClickEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; Indicator: String; DeleteMode: Boolean) of object;
  TTileListDeleteEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; var AllowDelete: Boolean) of object;
  TTileListAnchorEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState; Anchor: String) of object;
  TTileListStateEvent = procedure(Sender: TObject; Tile: TAdvSmoothTile; State: TTileState) of object;
  TTileListModeChangedEvent = procedure(Sender: TObject; PreviousMode, Mode: TTileListMode) of object;
  TTileListMovedEvent = procedure(Sender: TObject; FromIndex, ToIndex: Integer; var Allow: Boolean) of object;
  TTileListLevelChanged = procedure(Sender: TObject; Level: Integer) of object;

  TAdvSmoothTileContent = class;

  TAdvSmoothTileListVisualizer = class(TComponent)
  private
    FIsWinXP: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function XYToAnchor(Tile: TAdvSmoothTile; pX, pY: Integer; Focus: Boolean = False): string; virtual;
    function DoMouseDown(Tile: TAdvSmoothTile; Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function DoMouseMove(Tile: TAdvSmoothTile; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function DoMouseUp(Tile: TAdvSmoothTile; Button: TMouseButton; Shift: TShiftState; X, Y: Integer): Boolean; virtual;
    function DrawText(g: TGPGraphics; R: TGPRectF; Tile: TAdvSmoothTile; Text: String): TGPRectF; virtual;
    function DrawCheckBox(g: TGPGraphics; R: TGPRectF; Tile: TAdvSmoothTile; TileContent: TAdvSmoothTileContent): TGPRectF; virtual;
    function DrawTile(g: TGPGraphics; R: TGPRectF; Tile: TAdvSmoothTile): TGPRectF; virtual;
  end;

  TAdvSmoothTileContent = class(TPersistent)
  private
    FOwner: TAdvSmoothTile;
    FImage: TAdvGDIPPicture;
    FImageName: String;
    FText: String;
    FImageIndex: TImageIndex;
    FTextPosition: TTileListTextPosition;
    FTextTop: Integer;
    FTextLeft: Integer;
    FImageStretch: Boolean;
    FImageAspectRatio: Boolean;
    FOfficeHint: TAdvHintInfo;
    FHint: string;
    FShowCheckBox: Boolean;
    FChecked: Boolean;
    FCheckBoxEnabled: Boolean;
    FCheckBoxTop: Integer;
    FCheckBoxLeft: Integer;
    FCheckBoxPosition: TTileListCheckBoxPosition;
    procedure SetImage(const Value: TAdvGDIPPicture);
    procedure SetImageName(const Value: String);
    procedure SetText(const Value: String);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetTextPosition(const Value: TTileListTextPosition);
    procedure SetTextLeft(const Value: Integer);
    procedure SetTextTop(const Value: Integer);
    procedure SetImageAspectRatio(const Value: Boolean);
    procedure SetImageStretch(const Value: Boolean);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetCheckBoxEnabled(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetShowCheckBox(const Value: Boolean);
    procedure SetCheckBoxLeft(const Value: Integer);
    procedure SetCheckBoxPosition(const Value: TTileListCheckBoxPosition);
    procedure SetCheckBoxTop(const Value: Integer);
  protected
    procedure Changed;
    procedure ImageChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothTile);
    destructor Destroy; override;
    function TileList: TAdvSmoothTileList;
    function XYToCheckBox(X, Y: Integer): Boolean;
  published
    property Hint: string read FHint write FHint;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageName: String read FImageName write SetImageName;
    property Image: TAdvGDIPPicture read FImage write SetImage;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property Text: String read FText write SetText;
    property TextPosition: TTileListTextPosition read FTextPosition write SetTextPosition default tpCenterCenter;
    property TextLeft: Integer read FTextLeft write SetTextLeft default 0;
    property TextTop: Integer read FTextTop write SetTextTop default 0;
    property ImageAspectRatio: Boolean read FImageAspectRatio write SetImageAspectRatio default True;
    property ImageStretch: Boolean read FImageStretch write SetImageStretch default False;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default False;
    property CheckBoxPosition: TTileListCheckBoxPosition read FCheckBoxPosition write SetCheckBoxPosition default cpCenterLeft;
    property CheckBoxLeft: Integer read FCheckBoxLeft write SetCheckBoxLeft default 0;
    property CheckBoxTop: Integer read FCheckBoxTop write SetCheckBoxTop default 0;
    property Checked: Boolean read FChecked write SetChecked default False;
    property CheckBoxEnabled: Boolean read FCheckBoxEnabled write SetCheckBoxEnabled default True;
  end;

  TAdvSmoothTile = class(TCollectionItem)
  private
    FVisualizerMaximized: TAdvSmoothTileListVisualizer;
    DoAnimationX, DoAnimationY: Boolean;
    FOwner: TAdvSmoothTileList;
    FEnabled: Boolean;
    FEmpty: Boolean;
    FTileRectangle: TGPRectF;
    FOrigTileRectangle: TGPRectF;
    FContent: TAdvSmoothTileContent;
    FContentMaximized: TAdvSmoothTileContent;
    FVisualizer: TAdvSmoothTileListVisualizer;
    FCanDelete: Boolean;
    FStatusIndicator: String;
    FSubTiles: TAdvSmoothTiles;
    FBackTile: Boolean;
    FDeleteIndicator: String;
    FStatusIndicatorTop: Integer;
    FStatusIndicatorLeft: Integer;
    FDeleteIndicatorTop: Integer;
    FDeleteIndicatorLeft: Integer;
    FItemObject: TObject;
    FTag: Integer;
    FData: String;
    procedure SetEnabled(const Value: Boolean);
    procedure SetEmpty(const Value: Boolean);
    procedure SetContent(const Value: TAdvSmoothTileContent);
    procedure SetContentMaximized(const Value: TAdvSmoothTileContent);
    procedure SetVisualizer(const Value: TAdvSmoothTileListVisualizer);
    function GetVisualizer: TAdvSmoothTileListVisualizer;
    function GetTileState: TTileState;
    function GetTileFill: TGDIPFill;
    procedure SetCanDelete(const Value: Boolean);
    procedure SetStatusIndicator(const Value: String);
    procedure SetSubTiles(const Value: TAdvSmoothTiles);
    procedure SetBackTile(const Value: Boolean);
    function GetVisualizerMaximized: TAdvSmoothTileListVisualizer;
    procedure SetVisualizerMaximized(const Value: TAdvSmoothTileListVisualizer);
    procedure SetDeleteIndicator(const Value: String);
    procedure SetDeleteIndicatorLeft(const Value: Integer);
    procedure SetDeleteIndicatorTop(const Value: Integer);
    procedure SetStatusIndicatorLeft(const Value: Integer);
    procedure SetStatusIndicatorTop(const Value: Integer);
  protected
    procedure Changed;
    procedure SubTilesChanged(Sender: TObject);
  public
    property TileList: TAdvSmoothTileList read FOwner;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CreateContent: TAdvSmoothTileContent; virtual;
    function CreateContentMaximized: TAdvSmoothTileContent; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Draw(g: TGPGraphics);
    procedure DrawStatus(g: TGPGraphics);
    property TileRectangle: TGPRectF read FTileRectangle write FTileRectangle;
    property OrigTileRectangle: TGPRectF read FOrigTileRectangle write FOrigTileRectangle;
    property TileState: TTileState read GetTileState;
    property TileFill: TGDIPFill read GetTileFill;
    function CheckContentTile: Boolean;
    property ItemOject: TObject read FItemObject write FItemObject;
    procedure NavigateAndSelectSubTile(ATileIndex: Integer);
    procedure SelectSubTile(ATileIndex: Integer);
  published
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Empty: Boolean read FEmpty write SetEmpty default False;
    property BackTile: Boolean read FBackTile write SetBackTile default False;
    property Content: TAdvSmoothTileContent read FContent write SetContent;
    property ContentMaximized: TAdvSmoothTileContent read FContentMaximized write SetContentMaximized;
    property Visualizer: TAdvSmoothTileListVisualizer read GetVisualizer write SetVisualizer;
    property VisualizerMaximized: TAdvSmoothTileListVisualizer read GetVisualizerMaximized write SetVisualizerMaximized;
    property StatusIndicator: String read FStatusIndicator write SetStatusIndicator;
    property DeleteIndicator: String read FDeleteIndicator write SetDeleteIndicator;
    property StatusIndicatorLeft: Integer read FStatusIndicatorLeft write SetStatusIndicatorLeft default 0;
    property StatusIndicatorTop: Integer read FStatusIndicatorTop write SetStatusIndicatorTop default 0;
    property DeleteIndicatorLeft: Integer read FDeleteIndicatorLeft write SetDeleteIndicatorLeft default 0;
    property DeleteIndicatorTop: Integer read FDeleteIndicatorTop write SetDeleteIndicatorTop default 0;
    property CanDelete: Boolean read FCanDelete write SetCanDelete default True;
    property SubTiles: TAdvSmoothTiles read FSubTiles write SetSubTiles;
    property Tag: Integer read FTag write FTag;
    property Data: String read FData write FData;
  end;

  TAdvSmoothTiles = class(TOwnedCollection)
  private
    FParentTiles: TAdvSmoothTiles;
    FOwner: TAdvSmoothTileList;
    FOnChange: TNotifyEvent;
    FTag: integer;
    function GetItem(Index: Integer): TAdvSmoothTile;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothTile);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    function GetPageCount: Integer;
    function CreateItemClass: TCollectionItemClass; virtual;
    constructor Create(AOwner: TAdvSmoothTileList);
    property Items[Index: Integer]: TAdvSmoothTile read GetItem write SetItem; default;
    function Add: TAdvSmoothTile;
    function Insert(Index: Integer): TAdvSmoothTile;
    procedure Delete(Index: Integer);
    property ParentTiles: TAdvSmoothTiles read FParentTiles;
    property Tag: integer read FTag write FTag;
  end;

  TAdvSmoothTileAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothTileList;
    FSmallViewFill: TGDIPFill;
    FLargeViewFill: TGDIPFill;
    FVerticalSpacing: Integer;
    FHorizontalSpacing: Integer;
    FBackGround: Boolean;
    FTargetTileColor: TColor;
    FMovingTileColor: TColor;
    FSmallViewFillDisabled: TGDIPFill;
    FSmallViewFillSelected: TGDIPFill;
    FSmallViewFillHover: TGDIPFill;
    FSmallViewFontDisabled: TFont;
    FSmallViewFontSelected: TFont;
    FSmallViewFont: TFont;
    FSmallViewFontHover: TFont;
    FLargeViewFont: TFont;
    FStatusIndicatorAppearance: TGDIPStatus;
    FDeleteIndicatorAppearance: TGDIPStatus;
    procedure SetSmallViewFill(const Value: TGDIPFill);
    procedure SetHorizontalSpacing(const Value: Integer);
    procedure SetVerticalSpacing(const Value: Integer);
    procedure SetBackGround(const Value: Boolean);
    procedure SetTargetTileColor(const Value: TColor);
    procedure SetMovingTileColor(const Value: TColor);
    procedure SetLargeViewFill(const Value: TGDIPFill);
    procedure SetSmallViewFillDisabled(const Value: TGDIPFill);
    procedure SetSmallViewFillSelected(const Value: TGDIPFill);
    procedure SetSmallViewFillHover(const Value: TGDIPFill);
    procedure SetLargeViewFont(const Value: TFont);
    procedure SetSmallViewFont(const Value: TFont);
    procedure SetSmallViewFontDisabled(const Value: TFont);
    procedure SetSmallViewFontHover(const Value: TFont);
    procedure SetSmallViewFontSelected(const Value: TFont);
    procedure SetStatusIndicatorAppearance(const Value: TGDIPStatus);
    procedure SetDeleteIndicatorAppearance(const Value: TGDIPStatus);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure IndicatorChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothTileList);
    destructor Destroy; override;
  published
    property LargeViewFill: TGDIPFill read FLargeViewFill write SetLargeViewFill;
    property SmallViewFill: TGDIPFill read FSmallViewFill write SetSmallViewFill;
    property SmallViewFillSelected: TGDIPFill read FSmallViewFillSelected write SetSmallViewFillSelected;
    property SmallViewFillDisabled: TGDIPFill read FSmallViewFillDisabled write SetSmallViewFillDisabled;
    property SmallViewFillHover: TGDIPFill read FSmallViewFillHover write SetSmallViewFillHover;

    property LargeViewFont: TFont read FLargeViewFont write SetLargeViewFont;
    property SmallViewFont: TFont read FSmallViewFont write SetSmallViewFont;
    property SmallViewFontSelected: TFont read FSmallViewFontSelected write SetSmallViewFontSelected;
    property SmallViewFontDisabled: TFont read FSmallViewFontDisabled write SetSmallViewFontDisabled;
    property SmallViewFontHover: TFont read FSmallViewFontHover write SetSmallViewFontHover;

    property BackGround: Boolean read FBackGround write SetBackGround default True;
    property VerticalSpacing: Integer read FVerticalSpacing write SetVerticalSpacing default 15;
    property HorizontalSpacing: Integer read FHorizontalSpacing write SetHorizontalSpacing default 15;
    property TargetTileColor: TColor read FTargetTileColor write SetTargetTileColor default clGreen;
    property MovingTileColor: TColor read FMovingTileColor write SetMovingTileColor default clRed;
    property StatusIndicatorAppearance: TGDIPStatus read FStatusIndicatorAppearance write SetStatusIndicatorAppearance;
    property DeleteIndicatorAppearance: TGDIPStatus read FDeleteIndicatorAppearance write SetDeleteIndicatorAppearance;    
  end;

  TAdvSmoothTileListNavigation = class(TPersistent)
  private
    FOwner: TAdvSmoothTileList;
    FNavigation: Boolean;
    FVisible: Boolean;
    FFill: TGDIPFill;
    FHeight: Integer;
    FBulletColor: TColor;
    FBulletSelectedColor: TColor;
    FBulletSize: Integer;
    FArrowColor: TColor;
    FArrowRectangleSize: Integer;
    FArrowSize: Integer;
    FShowPages: Boolean;
    FArrowNavigation: Boolean;
    FCaption: String;
    FFont: TFont;
    FNextHint: String;
    FPreviousHint: String;
    FFloat: Boolean;
    FCaptionPosition: TTileListTextPosition;
    FCaptionTop: Integer;
    FCaptionLeft: Integer;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetHeight(const Value: Integer);
    procedure SetNavigation(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetBulletColor(const Value: TColor);
    procedure SetBulletSelectedColor(const Value: TColor);
    procedure SetBulletSize(const Value: Integer);
    procedure SetArrowColor(const Value: TColor);
    procedure SetArrowRectangleSize(const Value: Integer);
    procedure SetArrowSize(const Value: Integer);
    function GetHeight: Integer;
    procedure SetArrowNavigation(const Value: Boolean);
    procedure SetShowPages(const Value: Boolean);
    procedure SetCaption(const Value: String);
    procedure SetFont(const Value: TFont);
    procedure SetNextHint(const Value: String);
    procedure SetPreviousHint(const Value: String);
    procedure SetFloat(const Value: Boolean);
    procedure SetCaptionPosition(const Value: TTileListTextPosition);
    procedure SetCaptionLeft(const Value: Integer);
    procedure SetCaptionTop(const Value: Integer);
  protected
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    function XYToBullet(X, Y: Integer; Header: Boolean): integer;
    function XYToNavigation(X, Y: Integer; Header: Boolean): Integer;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothTileList);
    destructor Destroy; override;
    procedure Draw(g: TGPGraphics; r: TGPRectF);
  published
    property Float: Boolean read FFloat write SetFloat default False;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Fill: TGDIPFill read FFill write SetFill;
    property Height: Integer read FHeight write SetHeight default 30;
    property Navigation: Boolean read FNavigation write SetNavigation default True;
    property BulletColor: TColor read FBulletColor write SetBulletColor default clWhite;
    property BulletSelectedColor: TColor read FBulletSelectedColor write SetBulletSelectedColor default $E1A732;
    property BulletSize: Integer read FBulletSize write SetBulletSize default 9;
    property ArrowColor: TColor read FArrowColor write SetArrowColor default clWhite;
    property ArrowSize: Integer read FArrowSize write SetArrowSize default 12;
    property ArrowRectangleSize: Integer read FArrowRectangleSize write SetArrowRectangleSize default 50;
    property ArrowNavigation: Boolean read FArrowNavigation write SetArrowNavigation default True;
    property ShowPages: Boolean read FShowPages write SetShowPages default True;
    property Caption: String read FCaption write SetCaption;
    property CaptionLeft: Integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: Integer read FCaptionTop write SetCaptionTop default 0;
    property CaptionPosition: TTileListTextPosition read FCaptionPosition write SetCaptionPosition default tpCenterCenter;
    property Font: TFont read FFont write SetFont;
    property NextHint: String read FNextHint write SetNextHint;
    property PreviousHint: String read FPreviousHint write SetPreviousHint;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothTileList = class(TCustomControl, ITMSStyle, ITMSOfficeHint, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FMetroStyle: Boolean;
    FAnimateTilesParent, FAnimateTilesSub: TAdvSmoothTiles;
    FSubTile: TAdvSmoothTile;
    FOfficeHint: TAdvHintInfo;
    FSubListMode, FSubListDown: Boolean;
    FSubListIndex: Integer;
    FMaximized: Boolean;
    FGotoPage: Boolean;
    PrevIndicatorTile, DeleteIndicatorTile, IndicatorTile: TAdvSmoothTile;
    FBulletIdx, FPrevBulletIdx, FNavidx, FPrevNavIdx: Integer;
    FNavHeader: Boolean;
    FDefaultVisualizer: TAdvSmoothTileListVisualizer;
    FStartX, FStartY: Integer;
    FStartRectX, FStartRectY: Double;
    FMode: TTileListMode;
    FPrevX, FDownX, FDownY: Integer;
    FMoveCount: Integer;
    FAnimate, FMove: TTimer;
    FMouseDown, FDoubleClicked: Boolean;
    FTilePos: Double;
    FUpdateCount: Integer;
    FTiles, FWorkTiles: TAdvSmoothTiles;
    FRows: Integer;
    FColumns: Integer;
    FTileAppearance: TAdvSmoothTileAppearance;
    FFill: TGDIPFill;
    FPageIndex: Integer;
    FTileMargins: TMargins;
    FAnimationFactor: Double;
    FOnTileClick: TTileListEvent;
    FHeader: TAdvSmoothTileListNavigation;
    FFooter: TAdvSmoothTileListNavigation;
    FVisualizer, FVisualizerMaximized: TAdvSmoothTileListVisualizer;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TCustomImageList;
    FOnPageChanged: TTileListPageChangedEvent;
    FOnDrawBullet: TTileListDrawBulletEvent;
    FOnTileDblClick: TTileListEvent;
    FOnTileEnter: TTileListEvent;
    FOnTileLeave: TTileListEvent;
    FOnTileText: TTileListTextEvent;
    FOnTileFill: TTileListFillEvent;
    FOnTileBeforeDraw: TTileListBeforeDraw;
    FOnTileAfterDraw: TTileListAfterDraw;
    FOnBeforeDrawBackGround: TTileListBackGroundBeforeDraw;
    FOnAfterDrawBackGround: TTileListBackGroundAfterDraw;
    FContentTile: TAdvSmoothTile;
    FOnTileHint: TTileListHintEvent;
    FTextRendering: TTextRenderingHint;
    FOnTileDelete: TTileListDeleteEvent;
    FOnTileAnchorClick: TTileListAnchorEvent;
    FOptions: TTileListOptions;
    FOnTileStatusIndicatorClick: TTileListIndicatorClickEvent;
    FOnTileDeleteIndicatorClick: TTileListIndicatorClickEvent;
    FOnTileStatusIndicator: TTileListIndicatorEvent;
    FOnTileDeleteIndicator: TTileListIndicatorEvent;
    FOnTileMaximized: TTileListStateEvent;
    FOnTileMinimized: TTileListStateEvent;
    FOnTileListModeChanged: TTileListModeChangedEvent;
    FOnTileMoved: TTileListMovedEvent;
    FPageAnimationFactor: Double;
    FTransparent: Boolean;
    FOnTileFont: TTileListFontEvent;
    FAllowScroll: Boolean;
    FOnLevelDown: TTileListLevelChanged;
    FOnLevelUp: TTileListLevelChanged;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetTiles(const Value: TAdvSmoothTiles);
    procedure SetColumns(const Value: Integer);
    procedure SetRows(const Value: Integer);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetTileAppearance(const Value: TAdvSmoothTileAppearance);
    procedure SetPageIndex(const Value: Integer);
    function GetPageCount: Integer;
    procedure SetTileMargins(const Value: TMargins);
    procedure SetAnimationFactor(const Value: Double);
    procedure SetFooter(const Value: TAdvSmoothTileListNavigation);
    procedure SetHeader(const Value: TAdvSmoothTileListNavigation);
    procedure SetVisualizer(const Value: TAdvSmoothTileListVisualizer);
    function GetVisualizer: TAdvSmoothTileListVisualizer;
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetImageList(const Value: TCustomImageList);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetContentTile(const Value: TAdvSmoothTile);
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure SetTextRendering(const Value: TTextRenderingHint);
    function GetVisualizerMaximized: TAdvSmoothTileListVisualizer;
    procedure SetVisualizerMaximized(const Value: TAdvSmoothTileListVisualizer);
    procedure SetOptions(const Value: TTileListOptions);
    procedure SetMode(const Value: TTileListMode);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetPageAnimationFactor(const Value: Double);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetTransparent(const Value: Boolean);
    function GetVersion: String;
    function GetVersionNr: Integer;
    procedure SetSelectedTile(const Value: TAdvSmoothTile);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    function IsDesignTime: Boolean;
    function GetNextContentIndex: integer;
    function GetLastContentIndex: integer;
    function GetPreviousContentIndex: integer;
    function GetFirstContentIndex: integer;
    function GetNextIndex(ACount: Integer): integer;
    function GetLastIndex: integer;
    function GetPreviousIndex(ACount: Integer): integer;
    function GetFirstIndex: integer;
    procedure UpdateContentTiles;
    procedure Changed;
    procedure TilesChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ProcessKeyDown(Key: Word; Shift: TShiftState);
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Animate(Sender: TObject);
    procedure Move(Sender: TObject);
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property ContentTile: TAdvSmoothTile read FContentTile write SetContentTile;
    procedure SetAnimationMode(AMode: TTileListMode);
    procedure GetOfficeHint(PT: TPoint; var HintInfo: TAdvHintInfo); virtual;
  public
    PrevContentTile, NextContentTile, FSelectedTile, HoverTile: TAdvSmoothTile;
    MoveTile, ToTile, IndicationTile: TAdvSmoothTile;
    function PageIndexForTileIndex(Index: Integer): Integer;
    function TilesPerPage: Integer;
    procedure NavigateAndSelectTile(ATileIndex: Integer);
    procedure SelectTile(ATileIndex: Integer);
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure UseDefaultStyle;
    function IsSubTileMode: Boolean;
    property Mode: TTileListMode read FMode write SetMode;
    property TilePos: Double read FTilePos write FTilePos;
    procedure GoBack;
    procedure GoHome;
    procedure ToggleMaximized(Tile: TAdvSmoothTile; AMode: TTileListToggleMode = tmAuto);
    procedure GoDown(Tile: TAdvSmoothTile);
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitSample;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure Draw(g: TGPGraphics);
    function DrawBackGround(g: TGPGraphics): TGPRectF;
    procedure DrawTiles(g: TGPGraphics; pgidx: Integer);
    procedure DrawTilesStatus(g: TGPGraphics; pgidx: Integer);
    procedure BuildAllTiles(AnimationRectangle: Boolean; ATiles: TAdvSmoothTiles; AnimationRectangleMode: TAnimationRectangleMode);
    procedure BuildTiles(pgidx: Integer; AnimationRectangle: Boolean; ATiles: TAdvSmoothTiles; AnimationRectangleMode: TAnimationRectangleMode);
    function GetTileRectangle: TGPRectF;
    function GetBoundsRectangle: TGPRectF;
    function GetHeaderRectangle: TGPRectF;
    function GetFooterRectangle: TGPRectF;
    function XYToTile(X, Y: Integer; CountEmpty: Boolean = False; IncludeDisabled: Boolean = False): TAdvSmoothTile;
    function XYToStatusIndicatorTile(X, Y: Integer): TAdvSmoothTile;
    function XYToDeleteIndicatorTile(X, Y: Integer): TAdvSmoothTile;    
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NextPage;
    procedure GoToPage(APageIndex: Integer);
    procedure PreviousPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Resize; override;
    procedure DblClick; override;
    function CreateTiles: TAdvSmoothTiles; virtual;
    property PageAnimationFactor: Double read FPageAnimationFactor write SetPageAnimationFactor;
    procedure Clear;
    property SelectedTile: TAdvSmoothTile read FSelectedTile write SetSelectedTile;
    property WorkTiles: TAdvSmoothTiles read FWorkTiles;
  published

    property Version: String read GetVersion;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property TextRendering: TTextRenderingHint read FTextRendering write SetTextRendering default TextRenderingHintClearTypeGridFit;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write SetPictureContainer;
    property ImageList: TCustomImageList read FImageList write SetImageList;
    property AllowScroll: boolean read FAllowScroll write FAllowScroll default true;
    property AnimationFactor: Double read FAnimationFactor write SetAnimationFactor;
    property Fill: TGDIPFill read FFill write SetFill;
    property Tiles: TAdvSmoothTiles read FTiles write SetTiles;
    property Columns: Integer read FColumns write SetColumns default 3;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property Rows: Integer read FRows write SetRows default 3;
    property TileAppearance: TAdvSmoothTileAppearance read FTileAppearance write SetTileAppearance;
    property PageIndex: Integer read FPageIndex write SetPageIndex
     default 0;
    property PageCount: Integer read GetPageCount;
    property TileMargins: TMargins read FTileMargins write SetTileMargins;
    property Header: TAdvSmoothTileListNavigation read FHeader write SetHeader;
    property Footer: TAdvSmoothTileListNavigation read FFooter write SetFooter;
    property Visualizer: TAdvSmoothTileListVisualizer read GetVisualizer write SetVisualizer;
    property VisualizerMaximized: TAdvSmoothTileListVisualizer read GetVisualizerMaximized write SetVisualizerMaximized;


    property Options: TTileListOptions read FOptions write SetOptions default [toAllowMove, toAllowDelete, toAllowMaximize];
    property OnBeforeDrawBackGround: TTileListBackGroundBeforeDraw read FOnBeforeDrawBackGround write FOnBeforeDrawBackGround;
    property OnAfterDrawBackGround: TTileListBackGroundAfterDraw read FOnAfterDrawBackGround write FOnAfterDrawBackGround;
    property OnLevelUp: TTileListLevelChanged read FOnLevelUp write FOnLevelUp;
    property OnLevelDown: TTileListLevelChanged read FOnLevelDown write FOnLevelDown;
    property OnTileFill: TTileListFillEvent read FOnTileFill write FOnTileFill;
    property OnTileFont: TTileListFontEvent read FOnTileFont write FOnTileFont;
    property OnTileText: TTileListTextEvent read FOnTileText write FOnTileText;
    property OnTileClick: TTileListEvent read FOnTileClick write FOnTileClick;
    property OnTileDblClick: TTileListEvent read FOnTileDblClick write FOnTileDblClick;
    property OnTileEnter: TTileListEvent read FOnTileEnter write FOnTileEnter;
    property OnTileLeave: TTileListEvent read FOnTileLeave write FOnTileLeave;
    property OnPageChanged: TTileListPageChangedEvent read FOnPageChanged write FOnPageChanged;
    property OnBulletDraw: TTileListDrawBulletEvent read FOnDrawBullet write FOnDrawBullet;
    property OnTileBeforeDraw: TTileListBeforeDraw read FOnTileBeforeDraw write FOnTileBeforeDraw;
    property OnTileAfterDraw: TTileListAfterDraw read FOnTileAfterDraw write FOnTileAfterDraw;
    property OnTileHint: TTileListHintEvent read FOnTileHint write FOnTileHint;
    property OnTileStatusIndicator: TTileListIndicatorEvent read FOnTileStatusIndicator write FOnTileStatusIndicator;
    property OnTileStatusIndicatorClick: TTileListIndicatorClickEvent read FOnTileStatusIndicatorClick write FOnTileStatusIndicatorClick;
    property OnTileDeleteIndicator: TTileListIndicatorEvent read FOnTileDeleteIndicator write FOnTileDeleteIndicator;
    property OnTileDeleteIndicatorClick: TTileListIndicatorClickEvent read FOnTileDeleteIndicatorClick write FOnTileDeleteIndicatorClick;
    property OnTileDelete: TTileListDeleteEvent read FOnTileDelete write FOnTileDelete;
    property OnTileAnchorClick: TTileListAnchorEvent read FOnTileAnchorClick write FOnTileAnchorClick;
    property OnTileMaximized: TTileListStateEvent read FOnTileMaximized write FOnTileMaximized;
    property OnTileMinimized: TTileListStateEvent read FOnTileMinimized write FOnTileMinimized;
    property OnTileListModeChanged: TTileListModeChangedEvent read FOnTileListModeChanged write FOnTileListModeChanged;
    property OnTileMoved: TTileListMovedEvent read FOnTileMoved write FOnTileMoved;

    property Align;
    property Anchors;
    property Ctl3D;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property DragKind;
    property DragMode;
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnEndDrag;
    property OnDragDrop;
    property OnDragOver;
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property Enabled;
    property Color nodefault;
    {$IFDEF DELPHI2006_LVL}
    property Padding;
    property ParentBackground default False;
    {$ENDIF}
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnUnDock;
  end;


procedure GetTextPosition(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; position: TTileListTextPosition);
function RectanglesInterSect(r1, r2: TGPRectF): Boolean;
  
implementation

{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer; var
  FileHandle:dword;
  l: Integer;
  pvs: PVSFixedFileInfo;
  lptr: uint;
  querybuf: array[0..255] of char;
  buf: PChar;
begin
  Result := -1;

  StrPCopy(querybuf,FileName);
  l := GetFileVersionInfoSize(querybuf,FileHandle);
  if (l>0) then
  begin
    GetMem(buf,l);
    GetFileVersionInfo(querybuf,FileHandle,l,buf);
    if VerQueryValue(buf,'\',Pointer(pvs),lptr) then
    begin
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}

procedure GetTextPosition(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; position: TTileListTextPosition);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case position of
    tpTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    tpTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    tpBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    tpBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    tpTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    tpBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    tpCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    tpCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    tpCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;
end;

procedure GetCheckBoxPosition(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; position: TTileListCheckBoxPosition);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case position of
    cpTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    cpTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    cpBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    cpBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    cpTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    cpBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    cpCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    cpCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    cpCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;
end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
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

function SaveRound(Val: Double): Integer;
begin
  if Val < 0 then
    Result := Round(Max(Val, -MaxInt))
  else
    Result := Round(Min(Val, MaxInt))
end;

function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := SaveRound(Start + Delta)
    else
      Start := SaveRound(Start - Delta);
  end;
end;

{ TAdvSmoothTiles }

function TAdvSmoothTiles.Add: TAdvSmoothTile;
begin
  Result := TAdvSmoothTile(inherited Add);
end;

constructor TAdvSmoothTiles.Create(AOwner: TAdvSmoothTileList);
begin
  inherited Create(AOwner, CreateItemClass);
  FOwner := AOwner;
end;

function TAdvSmoothTiles.CreateItemClass: TCollectionItemClass;
begin
  Result := TAdvSmoothTile;
end;

procedure TAdvSmoothTiles.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothTiles.GetItem(Index: Integer): TAdvSmoothTile;
begin
  Result := TAdvSmoothTile(inherited Items[Index]);
end;

function TAdvSmoothTiles.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothTiles.GetPageCount: Integer;
var
  I: Integer;
  idx: integer;
  total: Integer;
begin
  Result := 0;
  idx := 0;
  total := FOwner.TilesPerPage;
  if (FOwner.Columns > 0) or (FOwner.Rows > 0) then
  begin
    for I := 0 to Count - 1 do
    begin
      Inc(idx);
      if Idx = total then
      begin
        idx := 0;
        Inc(Result);
      end
    end;
  end;

  if idx > 0 then
    Inc(Result);
end;

function TAdvSmoothTiles.Insert(Index: Integer): TAdvSmoothTile;
begin
  Result := TAdvSmoothTile(inherited Insert(Index));
end;

procedure TAdvSmoothTiles.SetItem(Index: Integer;
  const Value: TAdvSmoothTile);
begin
  inherited Items[Index] := Value;
end;

{ TAdvSmoothTileList }

procedure TAdvSmoothTileList.Animate(Sender: TObject);
var
  d: Double;
  R: TGPRectF;
  DoAnimate: Boolean;
  FTilePosTo: Double;
  pgidx: integer;
  pos: Single;
  anim: Double;
  nxtidx, previdx: integer;
  cnt, cntTotal: Integer;
  TileIndex: Integer;
  I, Row, Column: integer;
  dx, dy, dw, dh, dtox, dtoy, dwto, dhto: Single;
  DoAnimationX, DoAnimationY, DoAnimationW, DoAnimationH: Boolean;
  rFrom, rTo: TGPRectF;
begin
  if FSubListMode and Assigned(FAnimateTilesParent) and Assigned(FAnimateTilesSub) then
  begin
    cntTotal := 0;
    for I := PageIndex - 1 to PageIndex + 1 do
    begin
      TileIndex := I * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesSub.Count - 1) then
            Inc(cntTotal);

          Inc(TileIndex);
        end;
      end;
    end;

    for I := PageIndex - 1 to PageIndex + 1 do
    begin
      TileIndex := I * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesParent.Count - 1) then
            Inc(cntTotal);

          Inc(TileIndex);
        end;
      end;
    end;

    cnt := 0;
    for I := PageIndex - 1 to PageIndex + 1 do
    begin
      TileIndex := I * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesParent.Count - 1) then
          begin
            rTo := FAnimateTilesParent[TileIndex].OrigTileRectangle;
            rFrom := FAnimateTilesParent[TileIndex].TileRectangle;

            dx := Abs(rTo.X - rFrom.X) / AnimationFactor;
            dy := Abs(rTo.Y - rFrom.Y) / AnimationFactor;
            dw := Abs(rTo.Width - rFrom.Width) / AnimationFactor;
            dh := Abs(rTo.Height - rFrom.Height) / AnimationFactor;
            dtox := rFrom.X;
            dtoy := rFrom.Y;
            dwto := rFrom.Width;
            dhto := rFrom.Height;
            DoAnimationX := AnimateDouble(dtox, rTo.X, dx, 1);
            DoAnimationY := AnimateDouble(dtoy, rTo.Y, dy, 1);
            DoAnimationW := AnimateDouble(dwto, rTo.Width, dw, 1);
            DoAnimationH := AnimateDouble(dhto, rTo.Height, dh, 1);

            if DoAnimationX or DoAnimationY or DoAnimationW or DoAnimationH then
            begin
              FAnimateTilesParent[TileIndex].TileRectangle := MakeRect(dtox, dtoy, dwto, dhto);
              Invalidate;
            end
            else
              Inc(cnt);
          end;
          Inc(TileIndex);
        end;
      end;
    end;

    for I := PageIndex - 1 to PageIndex + 1 do
    begin
      TileIndex := I * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesSub.Count - 1) then
          begin
            rTo := FAnimateTilesSub[TileIndex].OrigTileRectangle;
            rFrom := FAnimateTilesSub[TileIndex].TileRectangle;

            dx := Abs(rTo.X - rFrom.X) / AnimationFactor;
            dy := Abs(rTo.Y - rFrom.Y) / AnimationFactor;
            dw := Abs(rTo.Width - rFrom.Width) / AnimationFactor;
            dh := Abs(rTo.Height - rFrom.Height) / AnimationFactor;
            dtox := rFrom.X;
            dtoy := rFrom.Y;
            dwto := rFrom.Width;
            dhto := rFrom.Height;
            DoAnimationX := AnimateDouble(dtox, rTo.X, dx, 1);
            DoAnimationY := AnimateDouble(dtoy, rTo.Y, dy, 1);
            DoAnimationW := AnimateDouble(dwto, rTo.Width, dw, 1);
            DoAnimationH := AnimateDouble(dhto, rTo.Height, dh, 1);

            if DoAnimationX or DoAnimationY or DoAnimationW or DoAnimationH then
            begin
              FAnimateTilesSub[TileIndex].TileRectangle := MakeRect(dtox, dtoy, dwto, dhto);
              Invalidate;
            end
            else
              Inc(cnt);
          end;
          Inc(TileIndex);
        end;
      end;
    end;

    if cnt = cntTotal then
    begin
      if FSubListDown then
      begin
        FWorkTiles := FAnimateTilesSub;
        FWorkTiles.FParentTiles := FAnimateTilesParent;
        Inc(FSubListIndex);
        if Assigned(OnLevelDown) then
          OnLevelDown(Self, FSubListIndex);
      end
      else
      begin
        FWorkTiles := FAnimateTilesParent;
        Dec(FSubListIndex);
        if Assigned(OnLevelUp) then
          OnLevelUp(Self, FSubListIndex);
      end;

      FAnimate.Enabled := False;
      FSubListMode := False;
      Changed;
    end;
  end
  else
  begin
    pgidx := PageIndex;
    R := GetTileRectangle;

    if not ((FMaximized and (FMode = tmContentNavigation) and Assigned(ContentTile))) then
    begin
      if FTilePos < 0 then
      begin
        if (FTilePos <= -r.Width / 2) and (PageIndex < PageCount - 1) then
        begin
          FTilePosTo := -r.Width;
          pgidx := PageIndex + 1;
        end
        else
          FTilePosTo := 0;
      end
      else
      begin
        if (FTilePos >= r.Width / 2) and (PageIndex > 0) then
        begin
          FTilePosTo := r.Width;
          pgidx := PageIndex - 1;
        end
        else
          FTilePosTo := 0;
      end;
    end
    else
    begin
      if FTilePos < 0 then
      begin
        if (FTilePos <= -r.Width / 2) and (ContentTile.index < GetLastContentIndex) then
        begin
          FTilePosTo := -r.Width;
          pgidx := GetNextContentIndex;
        end
        else
        begin
          pgidx := ContentTile.Index;
          FTilePosTo := 0;
        end;
      end
      else
      begin
        if (FTilePos >= r.Width / 2) and (ContentTile.index > GetFirstContentIndex) then
        begin
          FTilePosTo := r.Width;
          pgidx := GetPreviousContentIndex;
        end
        else
        begin
          pgidx := ContentTile.Index;
          FTilePosTo := 0;
        end;
      end;
    end;

    if FGotoPage then
      anim := PageAnimationFactor
    else
      anim := AnimationFactor;

    d := Abs(FTilePos - FTilePosTo) / anim;
    pos := FTilePos;
    DoAnimate := AnimateDouble(pos, FTilePosTo, d, 1);
    if DoAnimate then
    begin
      FTilePos := pos;
      Invalidate;
    end
    else
    begin
      FAnimate.Enabled := False;
      if not ((FMaximized and (FMode = tmContentNavigation) and Assigned(ContentTile))) then
      begin
        FTilePos := 0;
        PageIndex := pgidx
      end
      else
      begin
        if Assigned(ContentTile) then
        begin
          ContentTile := FWorkTiles[pgidx];
          nxtidx := GetNextContentIndex;
          previdx := GetPreviousContentIndex;

          if (nxtidx >= 0) and (nxtidx <= FWorkTiles.Count - 1) then
            NextContentTile := FWorkTiles[nxtidx]
          else
            NextContentTile := nil;

          if (previdx >= 0) and (previdx <= FWorkTiles.Count - 1) then
            PrevContentTile := FWorkTiles[previdx]
          else
            PrevContentTile := nil;

          UpdateContentTiles;
          FMove.Enabled := True;

          FTilePos := 0;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothTileList.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothTileList then
  begin
    FTiles.Assign((Source as TAdvSmoothTileList).Tiles);
    FAnimationFactor := (Source as TAdvSmoothTileList).AnimationFactor;
    FPageAnimationFactor := (Source as TAdvSmoothTileList).PageAnimationFactor;
    FFill.Assign((Source as TAdvSmoothTileList).Fill);
    FColumns := (Source as TAdvSmoothTileList).Columns;
    FRows := (Source as TAdvSmoothTileList).Rows;
    FTileAppearance.Assign((Source as TAdvSmoothTileList).TileAppearance);
    FPageIndex := (Source as TAdvSmoothTileList).PageIndex;
    FTileMargins.Assign((Source as TAdvSmoothTileList).TileMargins);
    FHeader.Assign((Source as TAdvSmoothTileList).Header);
    FFooter.Assign((Source as TAdvSmoothTileList).Footer);
  end;
end;

procedure TAdvSmoothTileList.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothTileList.BuildAllTiles(AnimationRectangle: Boolean; ATiles: TAdvSmoothTiles; AnimationRectangleMode: TAnimationRectangleMode);
begin
  BuildTiles(PageIndex - 1, AnimationRectangle, ATiles, AnimationRectangleMode);
  BuildTiles(PageIndex, AnimationRectangle, ATiles, AnimationRectangleMode);
  BuildTiles(PageIndex + 1, AnimationRectangle, ATiles, AnimationRectangleMode);
end;

procedure TAdvSmoothTileList.BuildTiles(pgidx: Integer; AnimationRectangle: Boolean; ATiles: TAdvSmoothTiles; AnimationRectangleMode: TAnimationRectangleMode);
var
  Row, Column: Integer;
  TileIndex: Integer;
  TileX, TileY, TileW, TileH: Double;
  TileRectangle: TGPRectF;
  w, h: Double;
  chk: Boolean;
  rTo, rFrom: TGPRectF;
begin
  if ATiles.Count = 0 then
    Exit;

  if (pgidx >= 0) and (pgidx < ATiles.GetPageCount) then
  begin
    TileIndex := pgidx * TilesPerPage;
    TileRectangle := GetTileRectangle;
    w := TileRectangle.Width - TileAppearance.HorizontalSpacing;
    TileW := w;
    if Columns > 0 then
     TileW := (w - (Columns * TileAppearance.HorizontalSpacing)) / Columns;

    h := TileRectangle.Height - TileAppearance.VerticalSpacing;
    TileH := h;
    if Rows > 0 then
     TileH := (h - (Rows * TileAppearance.VerticalSpacing)) / Rows;

    TileX := ((PageIndexForTileIndex(TileIndex) - PageIndex) * TileRectangle.Width) + TileRectangle.X + TileAppearance.HorizontalSpacing;
    TileY := TileRectangle.Y + TileAppearance.VerticalSpacing;
    for Row := 0 to Rows - 1 do
    begin
      for Column := 0 to Columns - 1 do
      begin
        if (TileIndex >= 0) and (TileIndex <= ATiles.Count - 1) then
        begin
          chk := ((ATiles[TileIndex] <> MoveTile) and (FMode = tmedit)) or (FMode = tmView) or (FMode = tmDelete);
          ATiles[TileIndex].OrigTileRectangle := MakeRect(Round(TileX), Round(TileY), Round(TileW), Round(TileH));
          if chk and AnimationRectangle then
          begin
            case AnimationRectangleMode of
              rmNormal: ATiles[TileIndex].TileRectangle :=  ATiles[TileIndex].OrigTileRectangle;
              rmDownReverse: ATiles[TileIndex].OrigTileRectangle :=  MakeRect(w / 2, h / 2, 0, 0);
              rmBackReverse:
              begin
               ATiles[TileIndex].TileRectangle :=  ATiles[TileIndex].OrigTileRectangle;
               rFrom := ATiles[TileIndex].TileRectangle;
               rTo := GetTileRectangle;
                if (rFrom.X < rTo.X + rTo.Width / 2) and (rFrom.Y < rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X - rFrom.Width * 2, rTo.Y - rFrom.Height * 2, rFrom.Width, rFrom.Height)
                else if (rFrom.X < rTo.X + rTo.Width / 2) and (rFrom.Y >= rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X - rFrom.Width * 2, rTo.Y + rTo.Height + rFrom.Height * 2, rFrom.Width, rFrom.Height)
                else if (rFrom.X >= rTo.X + rTo.Width / 2) and (rFrom.Y < rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X + rTo.Width + rFrom.Width * 2, rTo.Y - rFrom.Height * 2, rFrom.Width, rFrom.Height)
                else if (rFrom.X >= rTo.X + rTo.Width / 2) and (rFrom.Y >= rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X + rTo.Width + rFrom.Width * 2, rTo.Y + rTo.Height + rFrom.Height * 2, rFrom.Width, rFrom.Height);

                ATiles[TileIndex].TileRectangle := rTo;
              end;
              rmDown: ATiles[TileIndex].TileRectangle :=  MakeRect(w / 2, h / 2, 0, 0);
              rmBack:
              begin
               ATiles[TileIndex].TileRectangle :=  ATiles[TileIndex].OrigTileRectangle;
               rFrom := ATiles[TileIndex].OrigTileRectangle;
               rTo := GetTileRectangle;
                if (rFrom.X < rTo.X + rTo.Width / 2) and (rFrom.Y < rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X - rFrom.Width * 2, rTo.Y - rFrom.Height * 2, rFrom.Width, rFrom.Height)
                else if (rFrom.X < rTo.X + rTo.Width / 2) and (rFrom.Y >= rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X - rFrom.Width * 2, rTo.Y + rTo.Height + rFrom.Height * 2, rFrom.Width, rFrom.Height)
                else if (rFrom.X >= rTo.X + rTo.Width / 2) and (rFrom.Y < rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X + rTo.Width + rFrom.Width * 2, rTo.Y - rFrom.Height * 2, rFrom.Width, rFrom.Height)
                else if (rFrom.X >= rTo.X + rTo.Width / 2) and (rFrom.Y >= rTo.Y + rTo.Height / 2) then
                  rTo := MakeRect(rTo.X + rTo.Width + rFrom.Width * 2, rTo.Y + rTo.Height + rFrom.Height * 2, rFrom.Width, rFrom.Height);

                ATiles[TileIndex].OrigTileRectangle := rTo;
              end;
            end;
          end;
        end;

        TileX := TileX + TileAppearance.HorizontalSpacing + TileW;
        Inc(TileIndex);
      end;
      TileX := ((PageIndexForTileIndex(TileIndex) - PageIndex) * TileRectangle.Width) + TileRectangle.X + TileAppearance.HorizontalSpacing;
      TileY := TileY + TileAppearance.VerticalSpacing + TileH;
    end;
  end;
end;

procedure TAdvSmoothTileList.Changed;
begin
  if FUpdateCount > 0 then
    Exit;

  BuildAllTiles(True, FWorkTiles, rmNormal);
  Invalidate;
end;

procedure TAdvSmoothTileList.Clear;
begin
  ContentTile := nil;
  FSelectedTile := nil;
  NextContentTile := nil;
  PrevContentTile := nil;
  FSubListMode := False;
  FSubListDown := False;
  FSubListIndex := 0;
  FPageIndex := 0;
  FAnimateTilesSub    := nil;

  if Assigned(FTiles) then
  begin
    FTiles.Clear;
    FWorkTiles := FTiles;
    FAnimateTilesParent := FWorkTiles.FParentTiles;
  end;

  Invalidate;
end;

procedure TAdvSmoothTileList.CMHintShow(var Message: TMessage);
var
  hint, aHint: String;
  n: TAdvSmoothTileListNavigation;
begin
  if Assigned(HoverTile) and ((FMode = tmView) or (FMode = tmDelete)) then
  begin
    with TCMHintShow(Message).HintInfo^ do
    begin
      hint := '';
      if (HoverTile.TileState = tsMaximized) and (HoverTile.ContentMaximized.Hint <> '') then
        hint := HoverTile.ContentMaximized.Hint
      else
      begin
        if HoverTile.Content.Hint <> '' then
          hint := HoverTile.Content.Hint
      end;

      if Assigned(HoverTile.VisualizerMaximized) and (HoverTile.TileState = tsMaximized) then
        aHint := HoverTile.VisualizerMaximized.XYToAnchor(HoverTile, CursorPos.X, CursorPos.Y, True)
      else if Assigned(HoverTile.Visualizer) then
        aHint := HoverTile.Visualizer.XYToAnchor(HoverTile, CursorPos.X, CursorPos.Y, True);

      if AHint <> '' then
        hint := aHint;

      if Assigned(OnTileHint) then
        OnTileHint(Self, HoverTile, HoverTile.TileState, hint);
      HintStr := hint;
      ReshowTimeout := 0;
    end;
  end
  else if (FNavIdx <> -1) then
  begin
    with TCMHintShow(Message).HintInfo^ do
    begin
      if FNavHeader then
        n := Header
      else
        n := Footer;

      if FNavidx = 0 then
        HintStr := n.PreviousHint
      else if FNavIdx = 1 then
        HintStr := n.NextHint;

      ReshowTimeout := 0;
    end;
  end
  else if (FBulletIdx <> -1) then
  begin
    with TCMHintShow(Message).HintInfo^ do
    begin
      HintStr := 'Page ' + inttostr(FbulletIdx + 1);
      ReshowTimeout := 0;
    end;
  end;
end;

procedure TAdvSmoothTileList.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  HoverTile := nil;
  FPrevBulletIdx := -1;
  FBulletIdx := -1;
  FNavidx := -1;
  FPrevNavIdx := -1;
  FMouseDown := False;
  Screen.Cursor := crDefault;
  Invalidate;
end;

function TAdvSmoothTileList.CreateTiles: TAdvSmoothTiles;
begin
  Result := TAdvSmoothTiles.Create(Self);
end;

constructor TAdvSmoothTileList.Create(AOwner: TComponent);
begin
  inherited;
  TabStop := True;
  DoubleBuffered := true;
  Width := 450;
  Height := 450;
  FAnimationFactor := 4;
  FPageAnimationFactor := 1.2;
  FRows := 3;
  FColumns := 3;
  FTiles := CreateTiles;
  FTiles.OnChange := TilesChanged;
  FWorkTiles := FTiles;
  FOfficeHint := TAdvHintInfo.Create;
  FTransparent := False;
  FAllowScroll := True;

  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;

  FTileAppearance := TAdvSmoothTileAppearance.Create(Self);

  FOptions := [toAllowMove, toAllowDelete, toAllowMaximize];

  FTileMargins := TMargins.Create(nil);
  FTileMargins.OnChange := MarginsChanged;

  FFooter := TAdvSmoothTileListNavigation.Create(Self);
  FHeader := TAdvSmoothTileListNavigation.Create(Self);

  FAnimate := TTimer.Create(Self);
  FAnimate.OnTimer := Animate;
  FAnimate.Interval := 10;
  FAnimate.Enabled := False;

  FMove := TTimer.Create(Self);
  FMove.OnTimer := Move;
  FMove.Interval := 10;
  FMove.Enabled := False;

  FTextRendering := TextRenderingHintClearTypeGridFit;

  FDefaultVisualizer := TAdvSmoothTileListVisualizer.Create(Self);

  if(csDesigning in ComponentState) and not ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState)) then
    InitSample;
end;

procedure TAdvSmoothTileList.DblClick;
var
  x, y: integer;
  pt: TPoint;
  Tile: TAdvSmoothTile;
begin
  inherited;
  x := Mouse.CursorPos.X;
  y := Mouse.CursorPos.Y;
  pt := ScreenToClient(Point(x, y));

  if PtInGPRect(GetTileRectangle, pt) then
  begin
    Tile := nil;
    if (FTilePos = 0) then
      Tile := XYToTile(pt.X, pt.Y);

    if Assigned(Tile) then
    begin
      FDoubleClicked := True;
      if Assigned(OnTileDblClick) then
        OnTileDblClick(Self, tile, tile.TileState);
      ToggleMaximized(Tile);
    end
    else
      FMove.Enabled := False;
  end;
end;

destructor TAdvSmoothTileList.Destroy;
begin
  FDefaultVisualizer.Free;
  FMove.Free;
  FAnimate.Free;
  FFooter.Free;
  FHeader.Free;
  FTileMargins.Free;
  FTileAppearance.Free;
  FFill.Free;
  FTiles.Free;
  FOfficeHint.Free;
  inherited;
end;

procedure TAdvSmoothTileList.Draw(g: TGPGraphics);
begin
  DrawBackGround(g);

  DrawTiles(g, PageIndex - 1);
  DrawTiles(g, PageIndex);
  DrawTiles(g, PageIndex + 1);

  DrawTilesStatus(g, PageIndex - 1);
  DrawTilesStatus(g, PageIndex);
  DrawTilesStatus(g, PageIndex + 1);  

  if (FMode = tmEdit)  then
  begin
    if Assigned(MoveTile) then
    begin
      MoveTile.Draw(g);
      MoveTile.DrawStatus(g);
    end;
  end;
  
  if Assigned(PrevContentTile) then
    PrevContentTile.Draw(g);
  if Assigned(ContentTile) then
    ContentTile.Draw(g);
  if Assigned(NextContentTile) then
    NextContentTile.Draw(g);

  Header.Draw(g, GetHeaderRectangle);
  Footer.Draw(g, GetFooterRectangle);
end;

function TAdvSmoothTileList.DrawBackGround(g: TGPGraphics): TGPRectF;
var
  DefaultDraw: Boolean;
  r: TGPRectF;
begin
  r := GetBoundsRectangle;

  DefaultDraw := True;
  if Assigned(OnBeforeDrawBackGround) then
    OnBeforeDrawBackGround(Self, g, r, DefaultDraw);

  if DefaultDraw then
    Fill.Fill(g, r);

  if Assigned(OnAfterDrawBackGround) then
    OnAfterDrawBackGround(Self, g, r);
end;

procedure TAdvSmoothTileList.DrawTiles(g: TGPGraphics; pgidx: Integer);
var
  Row, Column: Integer;
  TileIndex: Integer;
  chk: Boolean;
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(GetTileRectangle);
  g.SetClip(rgn);



  if FSubListMode and Assigned(FAnimateTilesParent) and Assigned(FAnimateTilesSub) then
  begin
    if (pgidx >= 0) and (pgidx < FAnimateTilesParent.GetPageCount) then
    begin
      TileIndex := pgidx * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesParent.Count - 1) then
          begin
            chk := ((FAnimateTilesParent[TileIndex] <> MoveTile) and (FMode = tmedit)) or ((FAnimateTilesParent[TileIndex] <> ContentTile)
               and (FAnimateTilesParent[TileIndex] <> NextContentTile) and (FAnimateTilesParent[TileIndex] <> PrevContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) or (FMode = tmView) or (FMode = tmDelete));
            if chk then
              FAnimateTilesParent[TileIndex].Draw(g);
          end;
          Inc(TileIndex);
        end;
      end;
    end;

    if (pgidx >= 0) and (pgidx < FAnimateTilesSub.GetPageCount) then
    begin
      TileIndex := pgidx * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesSub.Count - 1) then
          begin
            chk := ((FAnimateTilesSub[TileIndex] <> MoveTile) and (FMode = tmedit)) or ((FAnimateTilesSub[TileIndex] <> ContentTile)
              and (FAnimateTilesSub[TileIndex] <> NextContentTile) and (FAnimateTilesSub[TileIndex] <> PrevContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) or (FMode = tmView) or (FMode = tmDelete));
            if chk then
              FAnimateTilesSub[TileIndex].Draw(g);
          end;
          Inc(TileIndex);
        end;
      end;
    end;
  end
  else
  begin
    if FWorkTiles.Count = 0 then
    begin
      g.ResetClip;
      rgn.Free;
      Exit;
    end;

    if (pgidx >= 0) and (pgidx < PageCount) then
    begin
      TileIndex := pgidx * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
          begin
            chk := ((FWorkTiles[TileIndex] <> MoveTile) and (FMode = tmedit)) or ((FWorkTiles[TileIndex] <> ContentTile)
              and (FWorkTiles[TileIndex] <> NextContentTile) and (FWorkTiles[TileIndex] <> PrevContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) or (FMode = tmView) or (FMode = tmDelete));
            if chk then
              FWorkTiles[TileIndex].Draw(g);
          end;
          Inc(TileIndex);
        end;
      end;
    end;
  end;

  g.ResetClip;
  rgn.Free;
end;

procedure TAdvSmoothTileList.DrawTilesStatus(g: TGPGraphics; pgidx: Integer);
var
  Row, Column: Integer;
  TileIndex: Integer;
  chk: Boolean;
begin
  if FSubListMode and Assigned(FAnimateTilesParent) and Assigned(FAnimateTilesSub) then
  begin
    if (pgidx >= 0) and (pgidx < FAnimateTilesParent.GetPageCount) then
    begin
      TileIndex := pgidx * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesParent.Count - 1) then
          begin
            chk := ((FAnimateTilesParent[TileIndex] <> MoveTile) and (FMode = tmedit)) or ((FAnimateTilesParent[TileIndex] <> ContentTile)
               and (FAnimateTilesParent[TileIndex] <> NextContentTile) and (FAnimateTilesParent[TileIndex] <> PrevContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) or (FMode = tmView) or (FMode = tmDelete));
            if chk then
              FAnimateTilesParent[TileIndex].DrawStatus(g);
          end;
          Inc(TileIndex);
        end;
      end;
    end;

    if (pgidx >= 0) and (pgidx < FAnimateTilesSub.GetPageCount) then
    begin
      TileIndex := pgidx * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FAnimateTilesSub.Count - 1) then
          begin
            chk := ((FAnimateTilesSub[TileIndex] <> MoveTile) and (FMode = tmedit)) or ((FAnimateTilesSub[TileIndex] <> ContentTile)
              and (FAnimateTilesSub[TileIndex] <> NextContentTile) and (FAnimateTilesSub[TileIndex] <> PrevContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) or (FMode = tmView) or (FMode = tmDelete));
            if chk then
              FAnimateTilesSub[TileIndex].DrawStatus(g);
          end;
          Inc(TileIndex);
        end;
      end;
    end;
  end
  else
  begin
    if FWorkTiles.Count = 0 then
      Exit;

    if (pgidx >= 0) and (pgidx < PageCount) then
    begin
      TileIndex := pgidx * TilesPerPage;
      for Row := 0 to Rows - 1 do
      begin
        for Column := 0 to Columns - 1 do
        begin
          if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
          begin
            chk := ((FWorkTiles[TileIndex] <> MoveTile) and (FMode = tmedit)) or ((FWorkTiles[TileIndex] <> ContentTile)
              and (FWorkTiles[TileIndex] <> NextContentTile) and (FWorkTiles[TileIndex] <> PrevContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) or (FMode = tmView) or (FMode = tmDelete));
            if chk then
              FWorkTiles[TileIndex].DrawStatus(g);
          end;
          Inc(TileIndex);
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothTileList.EndUpdate;
begin
  Dec(FUpdateCount);
  Changed;
end;

procedure TAdvSmoothTileList.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileList.FirstPage;
begin
  PageIndex := 0;
end;

function TAdvSmoothTileList.GetBoundsRectangle: TGPRectF;
var
  hh, fh: Double;
begin
  hh := 0;
  fh := 0;
  if not Header.Float then
    hh := Header.GetHeight;

  if not Footer.Float then
    fh := Footer.GetHeight;

  Result := MakeRect(0, hh, Width - 1, Height - fh - hh);
end;

function TAdvSmoothTileList.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothTileList.GetFirstContentIndex: integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FWorkTiles.Count - 1 do
  begin
    if not FWorkTiles[i].Empty then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothTileList.GetFirstIndex: integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FWorkTiles.Count - 1 do
  begin
    if not FWorkTiles[i].Empty and FWorkTiles[i].Enabled then
    begin
      Result := I;
      Break;
    end;
  end;    
end;

function TAdvSmoothTileList.GetFooterRectangle: TGPRectF;
var
  r: TGPRectF;
begin
  r := MakeRect(0, 0, Width - 1, Height - 1);
  if Footer.Visible then
    Result := MakeRect(r.X, r.Y + r.Height - Footer.GetHeight, r.Width, Footer.GetHeight)
  else
    Result := MakeRect(0, 0, 0, 0);
end;

function TAdvSmoothTileList.GetHeaderRectangle: TGPRectF;
var
  r: TGPRectF;
begin
  r := MakeRect(0, 0, Width - 1, Height - 1);
  if Header.Visible then
    Result := MakeRect(r.X, r.Y, r.Width, Header.GetHeight)
  else
    Result := MakeRect(0, 0, 0, 0);
end;

function TAdvSmoothTileList.GetLastContentIndex: integer;
var
  I: Integer;
begin
  Result := FWorkTiles.Count - 1;
  for I := FWorkTiles.Count - 1 downto 0 do
  begin
    if not FWorkTiles[i].Empty then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothTileList.GetLastIndex: integer;
var
  I: Integer;
begin
  Result := FWorkTiles.Count - 1;
  for I := FWorkTiles.Count - 1 downto 0 do
  begin
    if not FWorkTiles[i].Empty and FWorkTiles[i].Enabled then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TAdvSmoothTileList.GetNextContentIndex: integer;
var
  i: integer;
begin
  Result := -1;
  if Assigned(ContentTile) then
  begin
    for I := ContentTile.Index + 1 to FWorkTiles.Count - 1 do
    begin
      if not FWorkTiles[I].Empty then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

function TAdvSmoothTileList.GetNextIndex(ACount: Integer): integer;
var
  i: integer;
begin
  Result := -1;
  if Assigned(FSelectedTile) then
  begin
    for I := FSelectedTile.Index + ACount to FWorkTiles.Count - 1 do
    begin
      if not FWorkTiles[I].Empty and FWorkTiles[i].Enabled then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

procedure TAdvSmoothTileList.GetOfficeHint(PT: TPoint; var HintInfo: TAdvHintInfo);
begin
  if Assigned(HoverTile) then
  begin
    if (HoverTile.TileState = tsMaximized) and not HoverTile.ContentMaximized.OfficeHint.IsEmpty then
      HintInfo := HoverTile.ContentMaximized.OfficeHint
    else
    begin
      if not HoverTile.Content.OfficeHint.IsEmpty then
        HintInfo := HoverTile.Content.OfficeHint
      else
        HintInfo := OfficeHint;
    end;
  end
  else
    HintInfo := OfficeHint;
end;

function TAdvSmoothTileList.GetPageCount: Integer;
var
  I: Integer;
  idx: integer;
begin
  Result := 0;
  if FWorkTiles.Count = 0 then
    Exit;
      
  idx := 0;
  if (Columns > 0) or (Rows > 0) then
  begin
    for I := 0 to FWorkTiles.Count - 1 do
    begin
      Inc(idx);
      if Idx = Rows * Columns then
      begin
        idx := 0;
        Inc(Result);
      end
    end;
  end;

  if idx > 0 then
    Inc(Result);
end;

function TAdvSmoothTileList.GetPreviousContentIndex: integer;
var
  i: integer;
begin
  Result := -1;
  if Assigned(ContentTile) then
  begin
    for I := ContentTile.Index - 1 downto 0 do
    begin
      if not FWorkTiles[I].Empty then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

function TAdvSmoothTileList.GetPreviousIndex(ACount: Integer): integer;
var
  i: integer;
begin
  Result := -1;
  if Assigned(FSelectedTile) then
  begin
    for I := FSelectedTile.Index - ACount downto 0 do
    begin
      if not FWorkTiles[I].Empty and FWorkTiles[I].Enabled then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

function TAdvSmoothTileList.GetTileRectangle: TGPRectF;
var
  r: TGPRectF;
begin
  r := GetBoundsRectangle;
  Result := MakeRect(r.X +  TileMargins.Left, r.Y + TileMargins.Top, r.Width - TileMargins.Right - TileMargins.Left, r.Height - TileMargins.Bottom - TileMargins.Top)
end;

function TAdvSmoothTileList.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothTileList.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothTileList.GetVisualizer: TAdvSmoothTileListVisualizer;
begin
  if Assigned(FVisualizer) then
    Result := FVisualizer
  else
    Result := FDefaultVisualizer;
end;

function TAdvSmoothTileList.GetVisualizerMaximized: TAdvSmoothTileListVisualizer;
begin
  if Assigned(FVisualizerMaximized) then
    Result := FVisualizerMaximized
  else
    Result := FDefaultVisualizer;
end;

procedure TAdvSmoothTileList.GoHome;
begin
  ContentTile := nil;
  NextContentTile := nil;
  PrevContentTile := nil;
  FSubListMode := True;
  FSubListDown := False;
  FPageIndex := 0;
  FAnimateTilesSub := FWorkTiles;

  if Assigned(FAnimateTilesSub) then
  begin
    BuildAllTiles(True, FAnimateTilesSub, rmDownReverse);
    FSubListMode := false;
    FWorktiles := Tiles;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.GoBack;
begin
  ContentTile := nil;
  NextContentTile := nil;
  PrevContentTile := nil;
  FSubListMode := True;
  FSubListDown := False;
  FPageIndex := 0;
  if Assigned(FWorkTiles) then
    FAnimateTilesParent := FWorkTiles.FParentTiles;

  FAnimateTilesSub := FWorkTiles;

  if Assigned(FAnimateTilesParent) and Assigned(FAnimateTilesSub) then
  begin
    BuildAllTiles(True, FAnimateTilesSub, rmDownReverse);
    BuildAllTiles(True, FAnimateTilesParent, rmBackReverse);
    FAnimate.Enabled := True;
  end;
end;

procedure TAdvSmoothTileList.GoDown(Tile: TAdvSmoothTile);
begin
  if Assigned(Tile) then
  begin
    ContentTile := nil;
    NextContentTile := nil;
    PrevContentTile := nil;
    FAnimate.Enabled := True;
    FSubListMode := True;
    FSubListDown := True;
    FPageIndex := 0;
    FAnimateTilesParent := FWorkTiles;
    FAnimateTilesSub := Tile.SubTiles;

    if Assigned(FAnimateTilesParent) and Assigned(FAnimateTilesSub) then
    begin
      BuildAllTiles(True, FAnimateTilesParent, rmBack);
      BuildAllTiles(True, FAnimateTilesSub, rmDown);
      FAnimate.Enabled := True;
    end;
  end;
end;

procedure TAdvSmoothTileList.GoToPage(APageIndex: Integer);
var
  R: TGPRectF;
  I, B: Integer;
begin
  FGotoPage := True;
  R := GetTileRectangle;
  I := PageIndex;
  b := APageIndex;
  if I < b then
  begin
    while (I < b) do
    begin
      nextpage;
      while FAnimate.Enabled do
        Application.ProcessMessages;

      Inc(I);
    end;
  end
  else if I > b then
  begin
    while I > b do
    begin
      PreviousPage;
      while FAnimate.Enabled do
        Application.ProcessMessages;

      Dec(I);
    end;
  end;
  FGotoPage := false;
end;

procedure TAdvSmoothTileList.InitSample;
var
  I: Integer;
begin
  UseDefaultStyle;
  for I := 0 to 3 do
  begin
    with FWorkTiles.Add do
    begin
      Content.Text := 'Tile ' + inttostr(I + 1);
      ContentMaximized.Text := 'Description for ' + Content.Text;
    end;
  end;
end;

procedure TAdvSmoothTileList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothTileList.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothTileList.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothTileList.IsDesignTime: Boolean;
begin
  Result := (csDesigning in ComponentState) and not ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));
end;

function TAdvSmoothTileList.IsSubTileMode: Boolean;
begin
  Result := FSubListMode;
end;

procedure TAdvSmoothTileList.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  ProcessKeyDown(Key, Shift);
end;

procedure TAdvSmoothTileList.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if FAnimate.Enabled or FMove.Enabled then
    Exit;
    
  if FWorkTiles.Count > 0 then
  begin  
    case Key of
      VK_SPACE: ToggleMaximized(FSelectedTile, tmEnter);
      VK_ESCAPE: ToggleMaximized(FSelectedTile, tmExit);
    end;
  end;
end;

procedure TAdvSmoothTileList.LastPage;
begin
  PageIndex := PageCount;
end;

procedure TAdvSmoothTileList.MarginsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileList.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  chktile, VisualizerTile: TAdvSmoothTile;
  proceed, proceedmax: Boolean;  
  ct: TAdvSmoothTileContent;
begin
  inherited;

  if not Visible then
    Exit;

  FDownX := X;
  FDownY := Y;

  SetFocus;

  chktile := XYToTile(X, Y);
  if Assigned(chktile) then
  begin
    if chktile.CheckContentTile then
      ct := chktile.ContentMaximized
    else
      ct := chktile.Content;

    if Assigned(ct) then
    begin
      if ct.XYToCheckBox(X, Y) then
        Exit;
    end;         
  end;       
    
  proceed := true;
  proceedmax := true;  
  VisualizerTile := XYToTile(X, Y, False, True);
  if Assigned(VisualizerTile) then
  begin
    if Assigned(VisualizerTile.Visualizer) and not (VisualizerTile.CheckContentTile)  then
      proceed := VisualizerTile.Visualizer.DoMouseDown(VisualizerTile, Button, Shift, X, Y);

    if Assigned(VisualizerTile.VisualizerMaximized)and (VisualizerTile.CheckContentTile) then
      proceedmax := VisualizerTile.VisualizerMaximized.DoMouseDown(VisualizerTile,Button, Shift, X, Y);      
  end;

  if proceed and proceedmax then
  begin
    FMouseDown := not FDoubleClicked;
    FDoubleClicked := False;
    FPrevX := X;
    if (FMode = tmView) then
    begin
      MoveTile := XYToTile(X, Y);
      if Assigned(MoveTile) then
      begin
        if FMouseDown then
        begin
          FMoveCount := 0;
          FMove.Enabled := true;
        end;
        FStartX := X;
        FStartY := Y;
        FStartRectX := MoveTile.TileRectangle.X;
        FStartRectY := MoveTile.TileRectangle.Y;

        if not (toAllowMove in Options) then
          MoveTile := nil;
      end;
    end;
  end;
end;

procedure TAdvSmoothTileList.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r, tr: TGPRectF;
  hTile: TAdvSmoothTile;
var
  VisualizerTile: TAdvSmoothTile;
  proceed, proceedmax: Boolean;
begin
  inherited;

  proceed := true;
  proceedmax := true;

  if not FMouseDown then
  begin
    VisualizerTile := XYToTile(X, Y, False, True);
    if Assigned(VisualizerTile) then
    begin
      if Assigned(VisualizerTile.Visualizer) and not (VisualizerTile.CheckContentTile)  then
        proceed := VisualizerTile.Visualizer.DoMouseMove(VisualizerTile, Shift, X, Y);

      if Assigned(VisualizerTile.VisualizerMaximized) and (VisualizerTile.CheckContentTile) then
        proceedmax := VisualizerTile.VisualizerMaximized.DoMouseMove(VisualizerTile,Shift, X, Y);
    end;
  end;

  if proceedmax and proceed then
  begin
    if FMouseDown then
    begin
      if (FMode = tmEdit) And Assigned(MoveTile) then
      begin
        r := MoveTile.TileRectangle;
        r.x := (X + FStartRectX - FStartX);
        r.y := (Y + FStartRectY - FStartY);
        MoveTile.TileRectangle := r;
        TR := GetTileRectangle;
        if (MoveTile.TileRectangle.X + MoveTile.TileRectangle.Width > tr.X + tr.Width) and (X - FPrevX > 0) then
        begin
          NextPage;
        end
        else if (MoveTile.TileRectangle.X < tr.X) and (X - FPrevX < 0) then
          PreviousPage;

        IndicationTile := XYToTile(X, Y, True, True);

        Invalidate;
      end
      else if (FMode <> tmEdit) and AllowScroll then
      begin
        if PtInGPRect(GetTileRectangle, Point(X, Y)) and not ((X > FDownX - CLICKMARGIN) and (X < FDownX + CLICKMARGIN) and (Y > FDownY - CLICKMARGIN) and (Y < FDownY + CLICKMARGIN)) then
        begin
          FMoveCount := 0;
          FTilePos := FTilePos - (FPrevX - X);
          Invalidate;
        end;
      end;
      FPrevX := X;
    end;

    if (FTilePos = 0) and (FMode = tmView) then
    begin
      hTile := XYToTile(X, Y, False);
      if (hTile <> HoverTile) then
      begin
        Application.CancelHint;
        if Assigned(HoverTile) then
        begin
          if Assigned(OnTileLeave) then
            OnTileLeave(Self, HoverTile, HoverTile.TileState);
        end;

        HoverTile := hTile;

        if Assigned(OnTileEnter) and Assigned(HoverTile) then
          OnTileEnter(Self, HoverTile, HoverTile.TileState);

        Invalidate;
      end;
    end;

    if not Assigned(HoverTile) then
    begin
      FBulletIdx := Header.XYToBullet(X, Y, True);
      if FBulletIdx = -1 then
        FBulletIdx := Footer.XYToBullet(X, Y, False);

      FNavidx := Header.XYToNavigation(X, Y, True);
      if FNavIdx = -1 then
      begin
        FNavIdx := Footer.XYToNavigation(X, Y, False);
        if FNavIdx <> -1 then
          FNavHeader := False;
      end
      else
        FNavHeader := True;


      if (FPrevNavIdx <> FNavIdx) then
      begin
        Application.CancelHint;
        FPrevNavIdx := FNavIdx;

        if FNavIdx <> -1 then
          Screen.Cursor := crHandPoint
        else
          Screen.Cursor := crDefault;
      end;

      if FNavIdx = -1 then
      begin
        if (FPrevBulletIdx <> FBulletIdx) then
        begin
          Application.CancelHint;
          FPrevBulletIdx := FBulletIdx;

          if FBulletIdx <> -1 then
            Screen.Cursor := crHandPoint
          else
            Screen.Cursor := crDefault;
        end;

        if FBulletIdx = -1 then
        begin
          IndicatorTile := XYToStatusIndicatorTile(X, Y);
          if not Assigned(IndicatorTile) then
            IndicatorTile := XYToDeleteIndicatorTile(X, Y);
            
          if IndicatorTile <> PrevIndicatorTile then
          begin
            PrevIndicatorTile := IndicatorTile;
            if Assigned(IndicatorTile) then
              Screen.Cursor := crHandPoint
            else
              Screen.Cursor := crDefault;
          end;
        end;
      end;
    end;

  end;
end;

procedure TAdvSmoothTileList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  sTile: TAdvSmoothTile;
  AllowDelete: Boolean;
  AllowMove: Boolean;
  chktile, VisualizerTile: TAdvSmoothTile;
  ct: TAdvSmoothTileContent;
  proceed, proceedmax: Boolean;
begin
  inherited;

  chktile := XYToTile(X, Y);
  if Assigned(chktile) then
  begin
    if chktile.CheckContentTile then
      ct := chktile.ContentMaximized
    else
      ct := chktile.Content;

    if Assigned(ct) then
    begin
      if ct.XYToCheckBox(X, Y) then
      begin
        ct.Checked := not ct.Checked;
        Exit;
      end;
    end;         
  end;       
  
  proceed := true;
  proceedmax := true;
  VisualizerTile := XYToTile(X, Y, False, True);
  if Assigned(VisualizerTile) then
  begin
    if Assigned(VisualizerTile.Visualizer) and not (VisualizerTile.CheckContentTile) then
      proceed := VisualizerTile.Visualizer.DoMouseUp(VisualizerTile, Button, Shift, X, Y);

    if Assigned(VisualizerTile.VisualizerMaximized) and (VisualizerTile.CheckContentTile) then
      proceedmax := VisualizerTile.VisualizerMaximized.DoMouseUp(VisualizerTile,Button, Shift, X, Y);
  end;

  if proceedmax and proceed then
  begin
    FBulletIdx := Header.XYToBullet(X, Y, True);
    if FBulletIdx = -1 then
      FBulletIdx := Footer.XYToBullet(X, Y, False);

    if (FBulletIdx <> -1) then
    begin
      GoToPage(FBulletIdx);
    end;

    FNavidx := Header.XYToNavigation(X, Y, True);
    if FNavidx = -1 then
      FNavidx := Footer.XYToNavigation(X, Y, False);

    if (FNavidx <> -1) then
    begin
      case FNavidx of
        0: PreviousPage;
        1: NextPage;
      end;
    end;

    if (FMode = tmEdit) and (FTilePos = 0) then
    begin
      ToTile := XYToTile(X, Y, True, True);
      if Assigned(ToTile) and Assigned(MoveTile) then
      begin
        AllowMove := True;
        if Assigned(OnTileMoved) then
          OnTileMoved(Self, MoveTile.Index, ToTile.Index, AllowMove);

        if AllowMove then
        begin
          MoveTile.Index := ToTile.Index;
          BuildAllTiles(False, FWorkTiles, rmNormal);
        end;
        FMove.Enabled := True;
      end;      
    end
    else if (FMode = tmView) or (FMode = tmDelete) then
    begin
      FMoveCount := 0;
      FMove.Enabled := False;
      IndicatorTile := XYToStatusIndicatorTile(X, Y);
      if Assigned(IndicatorTile) then
      begin
        if Assigned(OnTileStatusIndicatorClick) then
          OnTileStatusIndicatorClick(Self, IndicatorTile, IndicatorTile.TileState, IndicatorTile.StatusIndicator, FMode = tmDelete);
      end;

      DeleteIndicatorTile := XYToDeleteIndicatorTile(X, Y);
      if Assigned(DeleteIndicatorTile) then
      begin
        if Assigned(OnTileDeleteIndicatorClick) then
          OnTileDeleteIndicatorClick(Self, DeleteIndicatorTile, DeleteIndicatorTile.TileState, DeleteIndicatorTile.DeleteIndicator, FMode = tmDelete);
      end;      

      if (FMode = tmDelete) and (FTilePos = 0) then
      begin
        if Assigned(DeleteIndicatorTile) then
        begin
          AllowDelete := True;
          if Assigned(OnTileDelete) then
            OnTileDelete(Self, DeleteIndicatorTile, DeleteIndicatorTile.TileState, AllowDelete);

          if AllowDelete and DeleteIndicatorTile.CanDelete then
          begin
            FUpdateCount := 1;
            DeleteIndicatorTile.Free;
            FMode := tmDelete;
            BuildAllTiles(False, FWorkTiles, rmNormal);
            FUpdateCount := 0;             
            FMove.Enabled := True;
          end;
        end
        else if FMouseDown then
        begin
          FMode := tmView;
        end;
      end
    end;

    if (FTilePos = 0) and (FMode = tmView) then
    begin
      sTile := XYToTile(X, Y, True);
      if (sTile <> FSelectedTile) then
      begin
        FSelectedTile := sTile;
        Invalidate;
      end;

      if Assigned(OnTileClick) and Assigned(FSelectedTile) then
        OnTileClick(Self, FSelectedTile, FSelectedTile.TileState);
    end;

    FMouseDown := False;
    FMoveCount := 0;
    if (FTilePos <> 0) and ((FMode = tmView) or (FMode = tmDelete) or (Fmode = tmContentNavigation)) then
      FAnimate.Enabled := True
    else if FMode = tmEdit then
      FMove.Enabled := True;

    Invalidate;
  end;
end;

procedure TAdvSmoothTileList.MouseWheelHandler(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    WM_MOUSEWHEEL:
    begin
      if integer(Message.WParam) < 0 then
        ProcessKeyDown(VK_RIGHT, [])
      else
        ProcessKeyDown(VK_LEFT, []);
    end;
  end;
end;

procedure TAdvSmoothTileList.Move(Sender: TObject);
var
  dx, dy, dw, dh: Double;
  dtox, dtoy, dwto, dhto: Single;
  DoAnimationX, DoAnimationY, DoAnimationW, DoAnimationH: Boolean;
  I, TileIndex, Row, Column: Integer;
  cnt, TotalCount: Integer;
  rFrom, rTo: TGPRectF;
  cntTot: Integer;
begin
  Inc(FMoveCount);
  if (Assigned(MoveTile) and (FMode = tmEdit)) or (FMode = tmDelete) then
  begin
    TotalCount := 0;
    if Assigned(ToTile) or (FMode = tmDelete) then
    begin
      for I := PageIndex - 1 to PageIndex + 1 do
      begin
        TileIndex := I * TilesPerPage;
        for Row := 0 to Rows - 1 do
        begin
          for Column := 0 to Columns - 1 do
          begin
            if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
              Inc(TotalCount);

            Inc(TileIndex);
          end;
        end;
      end;

      cnt := 0;
      for I := PageIndex - 1 to PageIndex + 1 do
      begin
        TileIndex := I * TilesPerPage;
        for Row := 0 to Rows - 1 do
        begin
          for Column := 0 to Columns - 1 do
          begin
            if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
            begin
              dx := Abs(FWorkTiles[TileIndex].TileRectangle.X - FWorkTiles[TileIndex].OrigTileRectangle.X) / AnimationFactor;
              dy := Abs(FWorkTiles[TileIndex].TileRectangle.Y - FWorkTiles[TileIndex].OrigTileRectangle.Y) / AnimationFactor;
              dtox := FWorkTiles[TileIndex].TileRectangle.X;
              dtoy := FWorkTiles[TileIndex].TileRectangle.Y;
              FWorkTiles[TileIndex].DoAnimationX := AnimateDouble(dtox, FWorkTiles[TileIndex].OrigTileRectangle.X, dx, 1);
              FWorkTiles[TileIndex].DoAnimationY := AnimateDouble(dtoy, FWorkTiles[TileIndex].OrigTileRectangle.Y, dy, 1);

              if FWorkTiles[TileIndex].DoAnimationX or FWorkTiles[TileIndex].DoAnimationY then
              begin
                FWorkTiles[TileIndex].TileRectangle := MakeRect(dtox, dtoy, FWorkTiles[TileIndex].TileRectangle.Width, FWorkTiles[TileIndex].TileRectangle.Height);
                Invalidate;
              end
              else
              begin
                Inc(cnt);
                FWorkTiles[TileIndex].TileRectangle := FWorkTiles[TileIndex].OrigTileRectangle;
              end;
            end;

            Inc(TileIndex);
          end;
        end;
      end;

      if (cnt = TotalCount) then
      begin
        ToTile := nil;
        MoveTile := nil;
        IndicationTile := nil;
        if FMode <> tmDelete then        
          FMode := tmView;
        FMove.Enabled := False;
        Invalidate;
      end;
    end
    else
    begin
      dx := Abs(MoveTile.TileRectangle.X - MoveTile.OrigTileRectangle.X) / AnimationFactor;
      dy := Abs(MoveTile.TileRectangle.Y - MoveTile.OrigTileRectangle.Y) / AnimationFactor;
      dtox := MoveTile.TileRectangle.X;
      dtoy := MoveTile.TileRectangle.Y;
      DoAnimationX := AnimateDouble(dtox, MoveTile.OrigTileRectangle.X, dx, 1);
      DoAnimationY := AnimateDouble(dtoy, MoveTile.OrigTileRectangle.Y, dy, 1);

      if DoAnimationX or DoAnimationY then
      begin
        MoveTile.TileRectangle := MakeRect(dtox, dtoy, Movetile.TileRectangle.Width, Movetile.TileRectangle.Height);
        Invalidate;
      end
      else
      begin
        IndicationTile := nil;
        MoveTile := nil;
        FMode := tmView;
        FMove.Enabled := False;
        Invalidate;
      end;
    end;
  end
  else if Assigned(ContentTile) and ((FMode = tmContent) or (FMode = tmContentNavigation)) then
  begin
    cnttot := 0;
    for I := 0 to FWorkTiles.Count - 1 do
    begin
      if FMaximized then
      begin
        if (FWorkTiles[I] = ContentTile) or (FWorkTiles[i] = PrevContentTile) or (FWorkTiles[i] = NextContentTile) then
        begin
          rFrom := FWorkTiles[i].TileRectangle;
          rTo := GetTileRectangle;
          if FWorkTiles[I].Index < ContentTile.Index then
            rTo := MakeRect(rto.X + rto.Width * -1 , rto.Y, rTo.Width, rTo.Height)
          else if FWorkTiles[I].Index = ContentTile.Index then
            rTo := MakeRect(rto.X , rto.Y, rTo.Width, rTo.Height)
          else if FWorkTiles[I].Index > ContentTile.Index then
            rTo := MakeRect(rto.X + rto.Width , rto.Y, rTo.Width, rTo.Height);
        end
        else
        begin
          rFrom := FWorkTiles[i].TileRectangle;
          rTo := GetTileRectangle;
          if (rFrom.X < rTo.X + rTo.Width / 2) and (rFrom.Y < rTo.Y + rTo.Height / 2) then
            rTo := MakeRect(rTo.X - rFrom.Width * 2, rTo.Y - rFrom.Height * 2, rFrom.Width, rFrom.Height)
          else if (rFrom.X < rTo.X + rTo.Width / 2) and (rFrom.Y >= rTo.Y + rTo.Height / 2) then
            rTo := MakeRect(rTo.X - rFrom.Width * 2, rTo.Y + rTo.Height + rFrom.Height * 2, rFrom.Width, rFrom.Height)
          else if (rFrom.X >= rTo.X + rTo.Width / 2) and (rFrom.Y < rTo.Y + rTo.Height / 2) then
            rTo := MakeRect(rTo.X + rTo.Width + rFrom.Width * 2, rTo.Y - rFrom.Height * 2, rFrom.Width, rFrom.Height)
          else if (rFrom.X >= rTo.X + rTo.Width / 2) and (rFrom.Y >= rTo.Y + rTo.Height / 2) then
            rTo := MakeRect(rTo.X + rTo.Width + rFrom.Width * 2, rTo.Y + rTo.Height + rFrom.Height * 2, rFrom.Width, rFrom.Height)
        end;
      end
      else
      begin
        rFrom := FWorkTiles[i].TileRectangle;
        rTo := FWorkTiles[i].OrigTileRectangle;
      end;
    
      dx := Abs(rTo.X - rFrom.X) / AnimationFactor;
      dy := Abs(rTo.Y - rFrom.Y) / AnimationFactor;
      dw := Abs(rTo.Width - rFrom.Width) / AnimationFactor;
      dh := Abs(rTo.Height - rFrom.Height) / AnimationFactor;
      dtox := rFrom.X;
      dtoy := rFrom.Y;
      dwto := rFrom.Width;
      dhto := rFrom.Height;
      DoAnimationX := AnimateDouble(dtox, rTo.X, dx, 1);
      DoAnimationY := AnimateDouble(dtoy, rTo.Y, dy, 1);
      DoAnimationW := AnimateDouble(dwto, rTo.Width, dw, 1);
      DoAnimationH := AnimateDouble(dhto, rTo.Height, dh, 1);

      if DoAnimationX or DoAnimationY or DoAnimationW or DoAnimationH then
      begin
        if (FWorkTiles[i] = PrevContentTile) or (FWorkTiles[i] = NextContentTile) then
          FWorkTiles[i].TileRectangle := rTo
        else
          FWorkTiles[I].TileRectangle := MakeRect(dtox, dtoy, dwto, dhto);

         if not FMaximized and (FWorkTiles[i] <> ContentTile) then
           FWorkTiles[i].TileRectangle := FWorkTiles[i].OrigTileRectangle;

        Invalidate;
      end
      else
        Inc(cnttot);
    end;

    if cntTot = FWorkTiles.Count then
    begin
      if not FMaximized then
      begin
        FMode := tmView;
        if Assigned(ContentTile) and Assigned(OnTileMinimized) then
          OnTileMinimized(Self, ContentTile, ContentTile.TileState);              
        ContentTile := nil;
        NextContentTile := nil;
        PrevContentTile := nil;
      end
      else
      begin
        FMode := tmContentNavigation;
        if Assigned(ContentTile) and Assigned(OnTileMaximized) then
          OnTileMaximized(Self, ContentTile, ContentTile.TileState);
      end;

      FMove.Enabled := False;
      Invalidate;
    end;
  end
  else
  begin
    if (FMoveCount >= 50) and (FTilePos = 0) then
    begin
      FMove.Enabled := False;
      if (MoveTile = FSelectedTile) and (toAllowMove in Options) then
        FMode := tmEdit
      else
      begin
        if toAllowDelete in Options then
          FMode := tmDelete;
        FMouseDown := False;
      end;

      Invalidate;
    end;
  end;
end;

procedure TAdvSmoothTileList.NavigateAndSelectTile(ATileIndex: Integer);
begin
  if (ATileIndex >= 0) and (ATileIndex <= Tiles.Count - 1) then
  begin
    if Tiles[ATileIndex].Enabled then
    begin
      SelectedTile := Tiles[ATileIndex];
      PageIndex := PageIndexForTileIndex(ATileIndex)
    end;
  end;
end;

procedure TAdvSmoothTileList.NextPage;
var
  R: TGPRectF;
begin
  R := GetTileRectangle;
  if FMaximized and Assigned(ContentTile) and (FMode = tmContentNavigation) then
  begin
    if ContentTile.Index < GetLastContentIndex then
    begin
      FTilePos := -R.Width / 2;
      FAnimate.Enabled := True;
    end;
  end
  else
  begin
    if PageIndex < PageCount - 1 then
    begin
      FTilePos := -R.Width / 2;
      FAnimate.Enabled := True;
    end;
  end;
end;

procedure TAdvSmoothTileList.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
  if csDestroying in ComponentState then
    Exit;

  if (AOperation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;

  if (AOperation = opRemove) and (AComponent = FVisualizer) then
    FVisualizer := nil;

  if (AOperation = opRemove) and (AComponent = FVisualizerMaximized) then
    FVisualizerMaximized := nil;

  if (AOperation = opRemove) then
  begin
    if Assigned(FWorkTiles) then
    begin
      for I := 0 to FWorkTiles.Count - 1 do
      begin
        if (AComponent = FWorkTiles[I].Visualizer) then
          FWorkTiles[I].Visualizer := nil;

        if (AComponent = FWorkTiles[I].VisualizerMaximized) then
          FWorkTiles[I].VisualizerMaximized := nil;
      end;
    end;
  end;

  inherited;
end;

function TAdvSmoothTileList.PageIndexForTileIndex(Index: Integer): Integer;
begin
  Result := 0;
  if TilesPerPage > 0 then
    Result := Index div TilesPerPage;
end;

procedure TAdvSmoothTileList.Paint;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRendering);
  Draw(g);
  g.Free;
end;

procedure TAdvSmoothTileList.PreviousPage;
var
  R: TGPRectF;
begin
  R := GetTileRectangle;
  if FMaximized and Assigned(ContentTile) and (FMode = tmContentNavigation) then
  begin
    if ContentTile.Index > GetFirstContentIndex then
    begin
      FTilePos := R.Width / 2;
      FAnimate.Enabled := True;
    end;
  end
  else
  begin
    if PageIndex > 0 then
    begin
      R := GetTileRectangle;
      FTilePos := R.Width / 2;
      FAnimate.Enabled := True;
    end;
  end;
end;

procedure TAdvSmoothTileList.ProcessKeyDown(Key: Word; Shift: TShiftState);
var
  idx, pgidx: integer;
begin
  if FAnimate.Enabled or FMove.Enabled then
    Exit;

  if FWorkTiles.Count > 0 then
  begin
    if not FMaximized then
    begin
      idx := -1;
      if Assigned(FSelectedTile) then
        idx := FSelectedTile.Index;

      case Key of
        VK_LEFT, VK_UP:
        begin
          if Assigned(FSelectedTile) then
          begin
            case Key of
              VK_LEFT: idx := GetPreviousIndex(1);
              VK_UP: idx := GetPreviousIndex(Columns);
            end;
          end
          else
            idx := GetFirstIndex;
        end;
        VK_RIGHT, VK_DOWN:
        begin
          if Assigned(FSelectedTile) then
          begin
            case Key of
              VK_RIGHT: idx := GetNextIndex(1);
              VK_DOWN: idx := GetNextIndex(Columns);
            end;
          end
          else
            idx := GetFirstIndex;
        end;
      end;

      if (idx >= 0) and (idx <= FWorkTiles.Count - 1) then
      begin
        case Key of
          VK_LEFT,VK_RIGHT, VK_UP, VK_DOWN:
          begin
            FSelectedTile := FWorkTiles[idx];
            pgidx := Self.PageIndexForTileIndex(FSelectedTile.Index);
            if pgidx <> PageIndex then
              PageIndex := pgidx;
          end;
        end;
      end
      else
        FSelectedTile := nil;

      case Key of
        VK_NEXT: NextPage;
        VK_PRIOR: PreviousPage;
        VK_HOME: FirstPage;
        VK_END: LastPage;
      end;
    end
    else
    begin
      case Key of
        VK_LEFT: PreviousPage;
        VK_RIGHT: NextPage;
      end;
    end;

    Invalidate;
  end;
end;

procedure TAdvSmoothTileList.Resize;
begin
  inherited;
  FMaximized := False;
  Changed;
end;

procedure TAdvSmoothTileList.SelectTile(ATileIndex: Integer);
begin
  if (ATileIndex >= 0) and (ATileIndex <= Tiles.Count - 1) then
  begin
    if Tiles[ATileIndex].Enabled then
      SelectedTile := Tiles[ATileIndex];
  end;
end;

procedure TAdvSmoothTileList.SetAnimationFactor(const Value: Double);
begin
  if FAnimationFactor <> Value then
  begin
    FAnimationFactor := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetAnimationMode(AMode: TTileListMode);
begin
  if Assigned(OnTileListModeChanged) then
    OnTileListModeChanged(Self, FMode, AMode);
  FMode := AMode;
end;

procedure TAdvSmoothTileList.SetColorTones(ATones: TColorTones);
begin
  FMetroStyle := True;
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;

  Header.Fill.Color := ATones.Selected.BrushColor;
  Header.Fill.ColorTo := ATones.Selected.BrushColor;
  Header.Fill.GradientMirrorType := gtNone;
  Header.Fill.ColorMirror := clNone;
  Header.Font.Color := ATones.Selected.TextColor;
  Header.Fill.BorderColor := ATones.Selected.BorderColor;
  Header.ArrowColor := ATones.Selected.TextColor;
  Header.Font.Name := GetMetroFont;
  Header.Font.Color := ATones.Selected.TextColor;

  Footer.Fill.Color := ATones.Selected.BrushColor;
  Footer.Fill.ColorTo := ATones.Selected.BrushColor;
  Footer.Fill.GradientMirrorType := gtNone;
  Footer.Fill.ColorMirror := clNone;
  Footer.Font.Color := ATones.Selected.TextColor;
  Footer.Fill.BorderColor := ATones.Selected.BorderColor;
  Footer.ArrowColor := ATones.Selected.TextColor;
  Footer.Font.Name := GetMetroFont;
  Footer.Font.Color := ATones.Selected.TextColor;

  Header.BulletColor := ATones.Foreground.BrushColor;
  Header.BulletSelectedColor := ATones.Selected.BrushColor;
  Footer.BulletColor := ATones.Foreground.BrushColor;
  Footer.BulletSelectedColor := ATones.Selected.BrushColor;

  TileAppearance.SmallViewFill.Color := ATones.Background.BrushColor;
  TileAppearance.SmallViewFill.ColorTo := ATones.Background.BrushColor;
  TileAppearance.SmallViewFill.ColorMirror := ATones.Background.BrushColor;
  TileAppearance.SmallViewFill.ColorMirrorTo := ATones.Background.BrushColor;
  TileAppearance.SmallViewFill.BorderColor := ATones.Background.BorderColor;
  TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

  TileAppearance.SmallViewFillDisabled.Color := ATones.Disabled.BrushColor;
  TileAppearance.SmallViewFillDisabled.ColorTo := ATones.Disabled.BrushColor;
  TileAppearance.SmallViewFillDisabled.ColorMirror := ATones.Disabled.BrushColor;
  TileAppearance.SmallViewFillDisabled.ColorMirrorTo := ATones.Disabled.BrushColor;
  TileAppearance.SmallViewFillDisabled.BorderColor := ATones.Disabled.BorderColor;
  TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

  TileAppearance.SmallViewFillHover.Color := ATones.Hover.BrushColor;
  TileAppearance.SmallViewFillHover.ColorTo := ATones.Hover.BrushColor;
  TileAppearance.SmallViewFillHover.BorderColor :=  ATones.Hover.BorderColor;
  TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

  TileAppearance.SmallViewFillSelected.Color := ATones.Selected.BrushColor;
  TileAppearance.SmallViewFillSelected.ColorTo := ATones.Selected.BrushColor;
  TileAppearance.SmallViewFillSelected.ColorMirror := ATones.Selected.BrushColor;
  TileAppearance.SmallViewFillSelected.ColorMirrorTo := ATones.Selected.BrushColor;
  TileAppearance.SmallViewFillSelected.BorderColor := ATones.Selected.BorderColor;
  TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

  TileAppearance.SmallViewFont.Color := ATones.Background.TextColor;
  TileAppearance.SmallViewFont.Name := GetMetroFont;
  TileAppearance.SmallViewFontSelected.Color := ATones.Selected.TextColor;
  TileAppearance.SmallViewFontSelected.Name := GetMetroFont;
  TileAppearance.SmallViewFontDisabled.Color := ATones.Disabled.TextColor;
  TileAppearance.SmallViewFontDisabled.Name := GetMetroFont;
  TileAppearance.SmallViewFontHover.Color := ATones.Hover.TextColor;
  TileAppearance.SmallViewFontHover.Name := GetMetroFont;

  TileAppearance.LargeViewFill.Color := ATones.Background.BrushColor;
  TileAppearance.LargeViewFill.ColorTo := ATones.Background.BrushColor;
  TileAppearance.LargeViewFill.ColorMirror := ATones.Background.BrushColor;
  TileAppearance.LargeViewFill.ColorMirrorTo := ATones.Background.BrushColor;
  TileAppearance.LargeViewFill.BorderColor := ATones.Background.BorderColor;
  TileAppearance.LargeViewFill.GradientMirrorType := gtVertical;

  TileAppearance.LargeViewFont.Color := ATones.Background.TextColor;
  TileAppearance.LargeViewFont.Name := GetMetroFont;
end;

procedure TAdvSmoothTileList.SetColumns(const Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  FMetroStyle := False;
  TileAppearance.SmallViewFill.Glow := gmNone;
  TileAppearance.SmallViewFillDisabled.Glow := gmNone;
  TileAppearance.SmallViewFillHover.Glow := gmNone;
  TileAppearance.SmallViewFillSelected.Glow := gmNone;
  Header.Fill.GradientType := gtVertical;
  Header.Fill.GradientMirrorType := gtNone;
  Footer.Fill.GradientType := gtVertical;
  Footer.Fill.GradientMirrorType := gtNone;
  Header.Fill.ColorMirror := clNone;
  Header.Fill.ColorMirrorTo := clNone;
  Footer.Fill.ColorMirror := clNone;
  Footer.Fill.ColorMirrorTo := clNone;

  TileAppearance.SmallViewFont.Color:= clBlack;
  TileAppearance.SmallViewFontSelected.Color:= clBlack;
  TileAppearance.SmallViewFontDisabled.Color:= clBlack;
  TileAppearance.SmallViewFontHover.Color:= clBlack;

  case AStyle of
  tsOffice2003Blue:
    begin
      Fill.Color := $00FFD2AF;
      Fill.ColorTo := $00FFD2AF;

      Header.Fill.Color := $D68759;
      Header.Fill.ColorTo := $933803;
      Header.Font.Color := clWhite;
      Header.Fill.BorderColor := $962D00;

      Footer.Fill.Color := $D68759;
      Footer.Fill.ColorTo := $933803;
      Footer.Font.Color := clWhite;
      Footer.Fill.BorderColor := $962D00;

      TileAppearance.SmallViewFill.Color := $FCE1CB;
      TileAppearance.SmallViewFill.ColorTo := $E0A57D;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $962D00;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $962D00;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $EBFDFF;
      TileAppearance.SmallViewFillHover.ColorTo := $ACECFF;
      TileAppearance.SmallViewFillHover.BorderColor :=  $962D00;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $94E6FB;
      TileAppearance.SmallViewFillSelected.ColorTo := $1595EE;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $962D00;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
    end;
  tsOffice2003Silver:
    begin
      Fill.Color := $00E6D8D8;
      Fill.ColorTo := $00E6D8D8;

      Header.Fill.Color := $BDA4A5;
      Header.Fill.ColorTo := $957475;
      Header.Font.Color := clWhite;
      Header.Fill.BorderColor := $947C7C;

      Footer.Fill.Color := $BDA4A5;
      Footer.Fill.ColorTo := $957475;
      Footer.Font.Color := clWhite;
      Footer.Fill.BorderColor := $947C7C;

      TileAppearance.SmallViewFill.Color := $ECE2E1;
      TileAppearance.SmallViewFill.ColorTo := $B39698;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $947C7C;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $947C7C;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $EBFDFF;
      TileAppearance.SmallViewFillHover.ColorTo := $ACECFF;
      TileAppearance.SmallViewFillHover.BorderColor :=  $947C7C;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $94E6FB;
      TileAppearance.SmallViewFillSelected.ColorTo := $1595EE;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $947C7C;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

    end;
  tsOffice2003Olive:
    begin
      Fill.Color := $CFF0EA;
      Fill.ColorTo := $CFF0EA;

      Header.Fill.Color := $82C0AF;
      Header.Fill.ColorTo := $447A63;
      Header.Font.Color := clWhite;
      Header.Fill.BorderColor := $588060;

      Footer.Fill.Color := $82C0AF;
      Footer.Fill.ColorTo := $447A63;
      Footer.Font.Color := clWhite;
      Footer.Fill.BorderColor := $588060;

      TileAppearance.SmallViewFill.Color := $CFF0EA;
      TileAppearance.SmallViewFill.ColorTo := $8CC0B1;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $588060;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $588060;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $EBFDFF;
      TileAppearance.SmallViewFillHover.ColorTo := $ACECFF;
      TileAppearance.SmallViewFillHover.BorderColor :=  $947C7C;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $94E6FB;
      TileAppearance.SmallViewFillSelected.ColorTo := $1595EE;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $588060;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
    end;
    tsOffice2003Classic:
    begin
      Fill.Color := $00F2F2F2;
      Fill.ColorTo := $00F2F2F2;

      Header.Fill.Color := $808080;
      Header.Fill.ColorTo := $808080;
      Header.Font.Color := clWhite;
      Header.Fill.BorderColor := $808080;

      Footer.Fill.Color := $808080;
      Footer.Fill.ColorTo := $808080;
      Footer.Font.Color := clWhite;
      Footer.Fill.BorderColor := $808080;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := $C9D1D5;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $808080;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $D8D5D4;
      TileAppearance.SmallViewFillDisabled.ColorTo := $D8D5D4;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $808080;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $B59285;
      TileAppearance.SmallViewFillSelected.ColorTo := $B59285;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $962D00;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $D2BDB6;
      TileAppearance.SmallViewFillHover.ColorTo := $D2BDB6;
      TileAppearance.SmallViewFillHover.BorderColor :=  $808080;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

    end;
  tsOffice2007Luna:
    begin
      Fill.Color := $DCB698;
      Fill.ColorTo := $DCB698;

      Header.Fill.Color := $FFEFE3;
      Header.Fill.ColorTo := $FFD2AF;
      Header.Font.Color := $723708;
      Header.Fill.BorderColor := $00FFD2AF;

      Footer.Fill.Color := $FFEFE3;
      Footer.Fill.ColorTo := $FFD2AF;
      Footer.Font.Color := $723708;
      Footer.Fill.BorderColor := $00FFD2AF;

      TileAppearance.SmallViewFill.Color := $FFEFE3;
      TileAppearance.SmallViewFill.ColorTo := $FFDDC4;
      TileAppearance.SmallViewFill.ColorMirror := $FFD1AD;
      TileAppearance.SmallViewFill.ColorMirrorTo := $FFDBC0;
      TileAppearance.SmallViewFill.BorderColor := $FFD1AD;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.BorderColor := $FFD1AD;//$00B6B6B6;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $AAD9FF;
      TileAppearance.SmallViewFillSelected.ColorTo := $6EBBFF;
      TileAppearance.SmallViewFillSelected.ColorMirror := $42AEFE;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := $7AE1FE;
      TileAppearance.SmallViewFillSelected.BorderColor := $FFD1AD;//$42AEFE;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $EBFDFF;
      TileAppearance.SmallViewFillHover.ColorTo := $ACECFF;
      TileAppearance.SmallViewFillHover.ColorMirror := $59DAFF;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := $A4E9FF;
      TileAppearance.SmallViewFillHover.BorderColor :=  $99CEDB;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

    end;
  tsOffice2007Obsidian:
    begin
      Fill.Color := $5C534C;
      Fill.ColorTo := $5C534C;

      Header.Fill.Color := $F2F1F0;
      Header.Fill.ColorTo := $C9C2BD;
      Header.Font.Color := $433C37;
      Header.Fill.BorderColor := $5C534C;

      Footer.Fill.Color := $F2F1F0;
      Footer.Fill.ColorTo := $C9C2BD;
      Footer.Font.Color := $433C37;
      Footer.Fill.BorderColor := $5C534C;

      TileAppearance.SmallViewFill.Color := $F9F8F8;
      TileAppearance.SmallViewFill.ColorTo := $E4E2DF;
      TileAppearance.SmallViewFill.ColorMirror := $D1CBC7;
      TileAppearance.SmallViewFill.ColorMirrorTo := $E2DEDB;
      TileAppearance.SmallViewFill.BorderColor := clBlack;//$D1CBC7;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $AAD9FF;
      TileAppearance.SmallViewFillSelected.ColorTo := $6EBBFF;
      TileAppearance.SmallViewFillSelected.ColorMirror := $42AEFE;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := $7AE1FE;
      TileAppearance.SmallViewFillSelected.BorderColor := clBlack;//$42AEFE;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.BorderColor := clBlack;//$00B6B6B6;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $EBFDFF;
      TileAppearance.SmallViewFillHover.ColorTo := $ACECFF;
      TileAppearance.SmallViewFillHover.ColorMirror := $59DAFF;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := $A4E9FF;
      TileAppearance.SmallViewFillHover.BorderColor :=  $99CEDB;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

    end;
  tsWindowsXP:
    begin
      Fill.Color := $00B6B6B6;
      Fill.ColorTo := $00B6B6B6;

      Header.Fill.Color := clBtnFace;
      Header.Fill.ColorTo := clBtnFace;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := clBlack;

      Footer.Fill.Color := clBtnFace;
      Footer.Fill.ColorTo := clBtnFace;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := clBlack;

      TileAppearance.SmallViewFillSelected.Color := clInactiveCaption;
      TileAppearance.SmallViewFillSelected.ColorTo := clInactiveCaption;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := clBlack;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := clBtnFace;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := clBlack;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := clInactiveCaptionText;
      TileAppearance.SmallViewFillHover.ColorTo := clInactiveCaptionText;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor := clBlack;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := clBlack;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

    end;
  tsWhidbey:
    begin
      Fill.Color := $F5F9FA;
      Fill.ColorTo := $F5F9FA;

      Header.Fill.Color := $EBEEEF;
      Header.Fill.ColorTo := $7E9898;
      Header.Font.Color := clWhite;
      Header.Fill.BorderColor := $962D00;

      Footer.Fill.Color := $EBEEEF;
      Footer.Fill.ColorTo := $7E9898;
      Footer.Font.Color := clWhite;
      Footer.Fill.BorderColor := $962D00;

      TileAppearance.SmallViewFill.Color := $F5F9FA;
      TileAppearance.SmallViewFill.ColorTo := $A8C0C0;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $962D00;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $962D00;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $94E6FB;
      TileAppearance.SmallViewFillHover.ColorTo := $1595EE;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor := clBlack;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $94E6FB;
      TileAppearance.SmallViewFillSelected.ColorTo := $1595EE;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $962D00;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

    end;
  tsCustom: ;
  tsOffice2007Silver:
    begin
      Fill.Color := $00CAC1BA;
      Fill.ColorTo := $00CAC1BA;

      Header.Fill.Color := $F8F7F6;
      Header.Fill.ColorTo := $E8E0DB;
      Header.Font.Color := $8B4215;
      Header.Fill.BorderColor := $74706F;

      Footer.Fill.Color := $F8F7F6;
      Footer.Fill.ColorTo := $E8E0DB;
      Footer.Font.Color := $8B4215;
      Footer.Fill.BorderColor := $74706F;

      TileAppearance.SmallViewFill.Color := $FAEEEB;
      TileAppearance.SmallViewFill.ColorTo := $E5DBD7;
      TileAppearance.SmallViewFill.ColorMirror := $E2D8D4;
      TileAppearance.SmallViewFill.ColorMirrorTo := $D1C7C5;
      TileAppearance.SmallViewFill.BorderColor := clBlack;//$E2D8D4;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $AAD9FF;
      TileAppearance.SmallViewFillSelected.ColorTo := $6EBBFF;
      TileAppearance.SmallViewFillSelected.ColorMirror := $42AEFE;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := $7AE1FE;
      TileAppearance.SmallViewFillSelected.BorderColor := clBlack;//$42AEFE;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $EBFDFF;
      TileAppearance.SmallViewFillHover.ColorTo := $ACECFF;
      TileAppearance.SmallViewFillHover.ColorMirror := $59DAFF;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := $A4E9FF;
      TileAppearance.SmallViewFillHover.BorderColor := clBlack;//$42AEFE;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.BorderColor := clBlack;//$00B6B6B6;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

    end;
  tsWindowsVista:
    begin
      Fill.Color := $F7EED9;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $FCF9F2;
      Header.Fill.ColorTo := $F7EED9;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $F9D996;

      Footer.Fill.Color := $FCF9F2;
      Footer.Fill.ColorTo := $F7EED9;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $F9D996;

      TileAppearance.SmallViewFill.Color := $FFFDF9;
      TileAppearance.SmallViewFill.ColorTo := $FFFAF0;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $FCF2DA;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $FBFAFA;
      TileAppearance.SmallViewFillDisabled.ColorTo := $E6E6E6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $D9D9D9;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $FEF9F0;
      TileAppearance.SmallViewFillHover.ColorTo := $FDF0D7;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $FEDF9A;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $FEF9F0;
      TileAppearance.SmallViewFillSelected.ColorTo := $FDF0D7;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $FEDF9A;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;

    end;
    tsWindows7:
    begin
      Fill.Color := $F7EED9;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $FCF9F2;
      Header.Fill.ColorTo := $F7EED9;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $F9D996;

      Footer.Fill.Color := $FCF9F2;
      Footer.Fill.ColorTo := $F7EED9;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $F9D996;

      TileAppearance.SmallViewFill.Color := $FFFDF9;
      TileAppearance.SmallViewFill.ColorTo := $FFFAF0;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $FCF2DA;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $FBFAFA;
      TileAppearance.SmallViewFillDisabled.ColorTo := $E6E6E6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $D9D9D9;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillHover.Color := $FDFBFA;
      TileAppearance.SmallViewFillHover.ColorTo := $FDF3EB;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $FBD6B8;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $FCEBDC;
      TileAppearance.SmallViewFillSelected.ColorTo := $FCDBC1;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $CEA27D;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
    end;
    tsTerminal:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clWhite;

      Header.Fill.Color := clBtnFace;
      Header.Fill.ColorTo := clBtnFace;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := clGray;

      Footer.Fill.Color := clBtnFace;
      Footer.Fill.ColorTo := clBtnFace;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := clGray;

      TileAppearance.SmallViewFill.Color := clBtnFace;
      TileAppearance.SmallViewFill.ColorTo := clBtnFace;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := clGray;


      TileAppearance.SmallViewFillDisabled.Color := clWhite;
      TileAppearance.SmallViewFillDisabled.ColorTo := clWhite;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := clNone;


      TileAppearance.SmallViewFillHover.Color := clSilver;
      TileAppearance.SmallViewFillHover.ColorTo := clSilver;
      TileAppearance.SmallViewFillHover.ColorMirror:= clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo:= clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  clGray;


      TileAppearance.SmallViewFillSelected.Color := clHighLight;
      TileAppearance.SmallViewFillSelected.ColorTo := clHighLight;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := clGray;


    end;
    tsOffice2010Blue:
    begin
      Fill.Color := $EAD3BF;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $FDF6EF;
      Header.Fill.ColorTo := $F0DAC7;
      Header.Font.Color := $5B391E;
      Header.Fill.BorderColor := $C7B29F;

      Footer.Fill.Color := $FDF6EF;
      Footer.Fill.ColorTo := $F0DAC7;
      Footer.Font.Color := $5B391E;
      Footer.Fill.BorderColor := $C7B29F;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := RGB(237, 239, 241);
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := RGB(236, 237, 237);
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFill.Rounding:= 2;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $962D00;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillDisabled.Rounding:= 2;

      TileAppearance.SmallViewFillHover.Color := $8AE3FD;
      TileAppearance.SmallViewFillHover.ColorTo :=  clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $58CAF1;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.GradientType := gtVertical;
      TileAppearance.SmallViewFillHover.Rounding:= 2;
      TileAppearance.SmallViewFillHover.Glow:= gmGradient;
      TileAppearance.SmallViewFillHover.GlowGradientColor:= $D9F9FD;

      TileAppearance.SmallViewFillSelected.Color := $6CD0FF;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $308AC2;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Rounding:= 2;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $7BEEFF;

    end;
    tsOffice2010Silver:
    begin
      Fill.Color := $D4CFCB;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $FFFFFF;
      Header.Fill.ColorTo := $EDE5E0;
      Header.Font.Color := $5B391E;
      Header.Fill.BorderColor := $D2CDC8;

      Footer.Fill.Color := $FFFFFF;
      Footer.Fill.ColorTo := $EDE5E0;
      Footer.Font.Color := $5B391E;
      Footer.Fill.BorderColor := $D2CDC8;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := RGB(237, 239, 241);
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := RGB(236, 237, 237);
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFill.Rounding:= 2;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $962D00;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillDisabled.Rounding:= 2;

      TileAppearance.SmallViewFillHover.Color := $8AE3FD;
      TileAppearance.SmallViewFillHover.ColorTo :=  clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $58CAF1;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.GradientType := gtVertical;
      TileAppearance.SmallViewFillHover.Rounding:= 2;
      TileAppearance.SmallViewFillHover.Glow:= gmGradient;
      TileAppearance.SmallViewFillHover.GlowGradientColor:= $D9F9FD;

      TileAppearance.SmallViewFillSelected.Color := $6CD0FF;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $308AC2;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Rounding:= 2;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $7BEEFF;

    end;
    tsOffice2010Black:
    begin
      Fill.Color := $656565;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $BFBFBF;
      Header.Fill.ColorTo := $919191;
      Header.Font.Color := $D7D7D6;
      Header.Fill.BorderColor := $6D6D6D;

      Footer.Fill.Color := $BFBFBF;
      Footer.Fill.ColorTo := $919191;
      Footer.Font.Color := $D7D7D6;
      Footer.Fill.BorderColor := $6D6D6D;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := RGB(237, 239, 241);
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := RGB(236, 237, 237);
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFill.Rounding:= 2;

      TileAppearance.SmallViewFillDisabled.Color := $00F2F2F2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $00B6B6B6;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $962D00;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillDisabled.Rounding:= 2;

      TileAppearance.SmallViewFillHover.Color := $8AE3FD;
      TileAppearance.SmallViewFillHover.ColorTo :=  clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $58CAF1;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.GradientType := gtVertical;
      TileAppearance.SmallViewFillHover.Rounding:= 2;
      TileAppearance.SmallViewFillHover.Glow:= gmGradient;
      TileAppearance.SmallViewFillHover.GlowGradientColor:= $D9F9FD;

      TileAppearance.SmallViewFillSelected.Color := $6CD0FF;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $308AC2;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Rounding:= 2;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $7BEEFF;

    end;
  tsWindows8, tsWindows10:
    begin

      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $F7F6F5;
      Header.Fill.ColorTo := $F7F6F5;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $E4E3E2;

      Footer.Fill.Color := $F7F6F5;
      Footer.Fill.ColorTo := $F7F6F5;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $E4E3E2;


      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := clWhite;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $E4E3E2;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $F7F7F7;
      TileAppearance.SmallViewFillDisabled.ColorTo := $F7F7F7;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $DEDEDE;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $F7E0C9;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $E4A262;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $F7E0C9;

      TileAppearance.SmallViewFillHover.Color := $F7EFE8;
      TileAppearance.SmallViewFillHover.ColorTo := $F7EFE8;
      TileAppearance.SmallViewFillHover.ColorMirror := $F7EFE8;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := $F7EFE8;
      TileAppearance.SmallViewFillHover.BorderColor :=  $F9CEA4;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;

    end;
  tsOffice2013White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      Header.Fill.Color := clWhite;
      Header.Fill.ColorTo := clNone;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $D4D4D4;

      Footer.Fill.Color := clWhite;
      Footer.Fill.ColorTo := clNone;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $D4D4D4;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := clNone;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $D4D4D4;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $EEEEEE;
      TileAppearance.SmallViewFillDisabled.ColorTo := $EEEEEE;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $ACACAC;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $FCE2C8;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $E59D56;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $FCE2C8;


      TileAppearance.SmallViewFillHover.Color := $FCF0E4;
      TileAppearance.SmallViewFillHover.ColorTo := $FCF0E4;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $EAB47E;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;


    end;
  tsOffice2013LightGray:
    begin

      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $F6F6F6;
      Header.Fill.ColorTo := clNone;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $C6C6C6;

      Footer.Fill.Color := $F6F6F6;
      Footer.Fill.ColorTo := clNone;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $C6C6C6;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := clNone;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $D4D4D4;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $EEEEEE;
      TileAppearance.SmallViewFillDisabled.ColorTo := $EEEEEE;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $ACACAC;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $FCE2C8;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $E59D56;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $FCE2C8;


      TileAppearance.SmallViewFillHover.Color := $FCF0E4;
      TileAppearance.SmallViewFillHover.ColorTo := $FCF0E4;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $EAB47E;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;


    end;
  tsOffice2013Gray:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $E5E5E5;
      Header.Fill.ColorTo := clNone;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $ABABAB;

      Footer.Fill.Color := $E5E5E5;
      Footer.Fill.ColorTo := clNone;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $ABABAB;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := clNone;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $D4D4D4;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $EEEEEE;
      TileAppearance.SmallViewFillDisabled.ColorTo := $EEEEEE;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $ACACAC;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $FCE2C8;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $E59D56;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmGradient;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= $FCE2C8;


      TileAppearance.SmallViewFillHover.Color := $FCF0E4;
      TileAppearance.SmallViewFillHover.ColorTo := $FCF0E4;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $EAB47E;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;


      end;

  tsOffice2016White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clNone;

      Header.Fill.Color := clWhite;
      Header.Fill.ColorTo := clNone;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $D4D4D4;

      Footer.Fill.Color := clWhite;
      Footer.Fill.ColorTo := clNone;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $D4D4D4;

      TileAppearance.SmallViewFill.Color := clWhite;
      TileAppearance.SmallViewFill.ColorTo := clNone;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $D4D4D4;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := clWhite;
      TileAppearance.SmallViewFillDisabled.ColorTo := clWhite;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $D4D4D4;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $E3BDA3;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $E3BDA3;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmNone;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= clNone;


      TileAppearance.SmallViewFillHover.Color := $F2E1D5;
      TileAppearance.SmallViewFillHover.ColorTo := $F2E1D5;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor := $F2E1D5;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;


    end;
  tsOffice2016Gray:
    begin

      Fill.Color := $D4D4D4;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $444444;
      Header.Fill.ColorTo := clNone;
      Header.Font.Color := clBlack;
      Header.Fill.BorderColor := $444444;

      Footer.Fill.Color := $444444;
      Footer.Fill.ColorTo := clNone;
      Footer.Font.Color := clBlack;
      Footer.Fill.BorderColor := $444444;

      TileAppearance.SmallViewFill.Color := $B2B2B2;
      TileAppearance.SmallViewFill.ColorTo := clNone;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $444444;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $B2B2B2;
      TileAppearance.SmallViewFillDisabled.ColorTo := $B2B2B2;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $444444;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $E3BDA3;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $E3BDA3;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmNone;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= clNone;


      TileAppearance.SmallViewFillHover.Color := $F2E1D5;
      TileAppearance.SmallViewFillHover.ColorTo := $F2E1D5;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $F2E1D5;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;


    end;
  tsOffice2016Black:
    begin
      Fill.Color := $363636;
      Fill.ColorTo := clNone;

      Header.Fill.Color := $444444;
      Header.Fill.ColorTo := clNone;
      Header.Font.Color := $FFFFFF;
      Header.Fill.BorderColor := $444444;

      Footer.Fill.Color := $444444;
      Footer.Fill.ColorTo := clNone;
      Footer.Font.Color := $FFFFFF;
      Footer.Fill.BorderColor := $444444;

      TileAppearance.SmallViewFont.Color:= $A6A6A6;
      TileAppearance.SmallViewFontSelected.Color:= $A6A6A6;
      TileAppearance.SmallViewFontDisabled.Color:= $A6A6A6;
      TileAppearance.SmallViewFontHover.Color:= $A6A6A6;

      TileAppearance.SmallViewFill.Color := $363636;
      TileAppearance.SmallViewFill.ColorTo := clNone;
      TileAppearance.SmallViewFill.ColorMirror := clNone;
      TileAppearance.SmallViewFill.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFill.BorderColor := $444444;
      TileAppearance.SmallViewFill.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillDisabled.Color := $363636;
      TileAppearance.SmallViewFillDisabled.ColorTo := $363636;
      TileAppearance.SmallViewFillDisabled.ColorMirror := clNone;
      TileAppearance.SmallViewFillDisabled.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillDisabled.BorderColor := $444444;
      TileAppearance.SmallViewFillDisabled.GradientMirrorType := gtVertical;

      TileAppearance.SmallViewFillSelected.Color := $444444;
      TileAppearance.SmallViewFillSelected.ColorTo := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirror := clNone;
      TileAppearance.SmallViewFillSelected.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillSelected.BorderColor := $444444;
      TileAppearance.SmallViewFillSelected.GradientMirrorType := gtVertical;
      TileAppearance.SmallViewFillSelected.Glow:= gmNone;
      TileAppearance.SmallViewFillSelected.GlowGradientColor:= clNone;


      TileAppearance.SmallViewFillHover.Color := $6A6A6A;
      TileAppearance.SmallViewFillHover.ColorTo := $6A6A6A;
      TileAppearance.SmallViewFillHover.ColorMirror := clNone;
      TileAppearance.SmallViewFillHover.ColorMirrorTo := clNone;
      TileAppearance.SmallViewFillHover.BorderColor :=  $6A6A6A;
      TileAppearance.SmallViewFillHover.GradientMirrorType := gtVertical;


      end;


  end;

  TileAppearance.LargeViewFill.Assign(TileAppearance.SmallViewFill);
end;

procedure TAdvSmoothTileList.SetContentTile(const Value: TAdvSmoothTile);
begin
  FContentTile := Value;
  if Assigned(FContentTile) then
    PageIndex := PageIndexForTileIndex(FContentTile.Index);
end;

procedure TAdvSmoothTileList.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetFooter(
  const Value: TAdvSmoothTileListNavigation);
begin
  if FFooter <> Value then
  begin
    FFooter.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetHeader(
  const Value: TAdvSmoothTileListNavigation);
begin
  if FHeader <> Value then
  begin
    FHeader.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetImageList(const Value: TCustomImageList);
begin
  if FImageList <> Value then
  begin
    FImageList := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetMode(const Value: TTileListMode);
var
  nxtidx, previdx: integer;
begin
  if FMode <> Value then
  begin
    FMode := Value;
    MoveTile := nil;
    IndicatorTile := nil;
    IndicationTile := nil;
    HoverTile := nil;
    FSelectedTile := nil;
    PrevIndicatorTile := nil;
    ContentTile := nil;
    NextContentTile := nil;
    PrevContentTile := nil;
    case FMode of
      tmEdit:
      begin
        if Tiles.Count > 0 then
          MoveTile := Tiles[0];
      end;
      tmContent, tmContentNavigation:
      begin
        if Tiles.Count > 0 then
        begin
          ContentTile := Tiles[0];
          nxtidx := GetNextContentIndex;
          previdx := GetPreviousContentIndex;

          if (nxtidx >= 0) and (nxtidx <= FWorkTiles.Count - 1) then
            NextContentTile := FWorkTiles[nxtidx]
          else
            NextContentTile := nil;

          if (previdx >= 0) and (previdx <= FWorkTiles.Count - 1) then
            PrevContentTile := FWorkTiles[previdx]
          else
            PrevContentTile := nil;

          FMaximized := not FMaximized;
          FMode := tmContent;
          FMove.Enabled := True;
          HoverTile := nil;
          IndicationTile := nil;
          IndicatorTile := nil;
          MoveTile := nil;
        end;
      end;
    end;
    Invalidate;
  end;
end;

procedure TAdvSmoothTileList.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

procedure TAdvSmoothTileList.SetOptions(const Value: TTileListOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetTileMargins(const Value: TMargins);
begin
  if FTileMargins <> Value then
  begin
    FTileMargins.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetPageAnimationFactor(const Value:Double);
begin
  if FPageAnimationFactor <> Value then
  begin
    FPageAnimationFactor := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetPageIndex(const Value: Integer);
var
  val: Integer;
begin
  val := Min(PageCount - 1, Max(0, Value));
  if FPageIndex <> val then
  begin
    FPageIndex := val;
    if Assigned(OnPageChanged) then
      OnPageChanged(Self, FPageIndex);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetPictureContainer(
  const Value: TGDIPPictureContainer);
begin
  if FPictureContainer <> Value then
  begin
    FPictureContainer := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    FRows := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetSelectedTile(const Value: TAdvSmoothTile);
begin
  FSelectedTile := Value;
  Invalidate;
end;

procedure TAdvSmoothTileList.SetTextRendering(const Value: TTextRenderingHint);
begin
  if FTextRendering <> Value then
  begin
    FTextRendering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetTileAppearance(
  const Value: TAdvSmoothTileAppearance);
begin
  if FTileAppearance <> Value then
  begin
    FTileAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetTiles(const Value: TAdvSmoothTiles);
begin
  if FTiles <> Value then
  begin
    FTiles.Assign(Value);
    FWorkTiles := FTiles;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetVisualizer(
  const Value: TAdvSmoothTileListVisualizer);
begin
  if FVisualizer <> Value then
  begin
    FVisualizer := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.SetVisualizerMaximized(
  const Value: TAdvSmoothTileListVisualizer);
begin
  if FVisualizerMaximized <> Value then
  begin
    FVisualizerMaximized := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileList.TilesChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothTileList.TilesPerPage: Integer;
begin
  Result := Columns * Rows;
end;

procedure TAdvSmoothTileList.ToggleMaximized(Tile: TAdvSmoothTile; AMode: TTileListToggleMode = tmAuto);
var
  nxtidx, previdx: integer;
  chkExit, chkEnter: Boolean;
begin
  if Assigned(Tile) then
    chkExit := (Tile.BackTile and (AMode = tmAuto)) or (AMode = tmExit) and not FMaximized
  else
    chkExit := (AMode = tmAuto) or (AMode = tmExit) and not FMaximized;

  if Assigned(Tile) then  
    chkEnter := ((Tile.SubTiles.Count > 0) and not FMaximized)
  else
    chkEnter := False;
    
    
  FSubTile := Tile;
  if chkExit then
  begin
    GoBack;
  end
  else if chkEnter then
  begin
    GoDown(Tile);
  end
  else if Assigned(Tile) and (toAllowMaximize in Options) then
  begin
    ContentTile := tile;
    nxtidx := GetNextContentIndex;
    previdx := GetPreviousContentIndex;

    if (nxtidx >= 0) and (nxtidx <= FWorkTiles.Count - 1) then
      NextContentTile := FWorkTiles[nxtidx]
    else
      NextContentTile := nil;

    if (previdx >= 0) and (previdx <= FWorkTiles.Count - 1) then
      PrevContentTile := FWorkTiles[previdx]
    else
      PrevContentTile := nil;

    FMaximized := not FMaximized;
    FMode := tmContent;
    FMove.Enabled := True;
    HoverTile := nil;
    IndicationTile := nil;
    IndicatorTile := nil;
    MoveTile := nil;
  end;
end;

procedure TAdvSmoothTileList.UpdateContentTiles;
var
  R: TGPRectF;
begin
  if Assigned(PrevContentTile) then
  begin
    r := PrevContentTile.TileRectangle;
    r.X := r.X + FTilePos;
    PrevContentTile.TileRectangle := r;
  end;
  if Assigned(ContentTile) then
  begin
    r := ContentTile.TileRectangle;
    r.X := r.X + FTilePos;
    ContentTile.TileRectangle := r;
  end;
  if Assigned(NextContentTile) then
  begin
    r := NextContentTile.TileRectangle;
    r.X := r.X + FTilePos;
    NextContentTile.TileRectangle := r;
  end;

  Invalidate;
end;

procedure TAdvSmoothTileList.UseDefaultStyle;
begin
  Header.Fill.Color := RGB(148,152,162);
  Header.Fill.ColorTo := RGB(123,128,133);
  Header.Fill.ColorMirror := RGB(113,118,124);
  Header.Fill.ColorMirrorTo := RGB(91,97,103);
  Header.Fill.BorderColor := RGB(102,104,106);
  Header.Fill.GradientType := gtVertical;
  Header.Fill.GradientMirrorType := gtVertical;
  Header.ArrowColor := clWhite;
  Footer.ArrowColor := clWhite;
  Footer.Fill.Assign(Header.Fill);
  Fill.Color := clWhite;
  Fill.GradientType := gtSolid;
  Fill.BorderColor := Header.Fill.BorderColor;
  TileAppearance.SmallViewFill.Opacity := 180;
  TileAppearance.SmallViewFill.Color := RGB(188, 188, 188);
  TileAppearance.SmallViewFill.GradientType := gtSolid;
  TileAppearance.SmallViewFill.BorderColor := RGB(167,167,167);
  TileAppearance.SmallViewFill.GradientMirrorType := gtNone;
  TileAppearance.SmallViewFill.ColorMirror := clNone;

  TileAppearance.SmallViewFillHover.Assign(TileAppearance.SmallViewFill);
  TileAppearance.SmallViewFillHover.Opacity := 220;
  TileAppearance.SmallViewFillHover.ColorMirror := clNone;

  TileAppearance.SmallViewFillDisabled.Color := clGray;
  TileAppearance.SmallViewFillDisabled.GradientType := gtSolid;
  TileAppearance.SmallViewFillDisabled.BorderColor := RGB(167,167,167);

  TileAppearance.SmallViewFillSelected.Assign(Header.Fill);

  TileAppearance.LargeViewFill.Assign(TileAppearance.SmallViewFill);

  TileAppearance.StatusIndicatorAppearance.Fill.Color := clRed;
  TileAppearance.StatusIndicatorAppearance.Fill.GradientType := gtSolid;
  TileAppearance.StatusIndicatorAppearance.Fill.BorderColor := clGray;
  TileAppearance.StatusIndicatorAppearance.Font.Color := clWhite;

  TileAppearance.DeleteIndicatorAppearance.Fill.Color := clBlack;
  TileAppearance.DeleteIndicatorAppearance.Fill.GradientType := gtSolid;
  TileAppearance.DeleteIndicatorAppearance.Fill.BorderColor := clWhite;
  TileAppearance.DeleteIndicatorAppearance.Font.Color := clWhite;  
end;

procedure TAdvSmoothTileList.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if not Transparent then
  begin
    inherited;
    Exit;
  end;


  {$IFDEF DELPHI2006_LVL}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  message.Result := 1;
  {$ENDIF}
end;

procedure TAdvSmoothTileList.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothTileList.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
begin
  if not Transparent then
  begin
    inherited;
    Exit;
  end;

  if Assigned(Parent) {and (Fill.ShadowOffset > 0) ?} then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
      {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
      {$ENDIF}
      i := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(DC, p.x, p.y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then
        (Parent as TWinCtrl).PaintCtrls(DC, nil);
      RestoreDC(DC, i);
      {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
      {$ENDIF}
    end;
  end;

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

function TAdvSmoothTileList.XYToDeleteIndicatorTile(X, Y: Integer): TAdvSmoothTile;
var
  Row, Column: Integer;
  TileIndex: Integer;
  chk: Boolean;
  cap: String;
  bmp: TBitmap;
  g: TGPGraphics;
  xs, ys: Double;
  r: TGPRectF;
begin

  Result := nil;
  if FMode = tmDelete then
  begin
    bmp := TBitmap.Create;
    g := TGPGraphics.Create(bmp.Canvas.Handle);  
    TileIndex := PageIndex * TilesPerPage;
    for Row := 0 to Rows - 1 do
    begin
      for Column := 0 to Columns - 1 do
      begin
        if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
        begin
          chk := not FWorkTiles[TileIndex].CheckContentTile and not FWorkTiles[TileIndex].Empty and ((FWorkTiles[TileIndex] <> MoveTile) and (FMode = tmedit)) or (FMode <> tmEdit);
          if chk then
          begin
            r := FWorkTiles[TileIndex].TileRectangle;
            cap := FWorkTiles[TileIndex].DeleteIndicator;

            if Assigned(FWorkTiles[TileIndex].FOwner.OnTileDeleteIndicator) then
              FWorkTiles[TileIndex].FOwner.OnTileDeleteIndicator(Self, FWorkTiles[TileIndex], FWorkTiles[TileIndex].TileState, cap);

            if cap <> '' then
            begin
              with FWorkTiles[TileIndex].FOwner.TileAppearance.DeleteIndicatorAppearance do
              begin
                CalculateSize(g, cap);
                xs := r.X  - GetWidth / 2;
                ys := r.Y - GetHeight / 2;
                xs := xs + FWorkTiles[TileIndex].DeleteIndicatorLeft;
                ys := ys + FWorkTiles[TileIndex].DeleteIndicatorTop;

                if PtInGPRect(MakeRect(xs, ys, GetWidth, GetHeight), Point(X, Y)) then
                begin
                  Result := FWorkTiles[TileIndex];
                  break;
                end;
              end;
            end;
          end;
          Inc(TileIndex);
        end;
      end;
    end;
    g.free;
    bmp.free;
  end;
end;

function TAdvSmoothTileList.XYToStatusIndicatorTile(X, Y: Integer): TAdvSmoothTile;
var
  Row, Column: Integer;
  TileIndex: Integer;
  chk: Boolean;
  cap: String;
  bmp: TBitmap;
  g: TGPGraphics;
  xs, ys: Double;
  r: TGPRectF;
begin
  bmp := TBitmap.Create;
  g := TGPGraphics.Create(bmp.Canvas.Handle);
  Result := nil;
  TileIndex := PageIndex * TilesPerPage;
  for Row := 0 to Rows - 1 do
  begin
    for Column := 0 to Columns - 1 do
    begin
      if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
      begin
        chk := not FWorkTiles[TileIndex].CheckContentTile and not FWorkTiles[TileIndex].Empty and ((FWorkTiles[TileIndex] <> MoveTile) and (FMode = tmedit)) or (FMode <> tmEdit);
        if chk then
        begin
          r := FWorkTiles[TileIndex].TileRectangle;
          cap := FWorkTiles[TileIndex].StatusIndicator;

          if Assigned(FWorkTiles[TileIndex].FOwner.OnTileStatusIndicator) then
            FWorkTiles[TileIndex].FOwner.OnTileStatusIndicator(Self, FWorkTiles[TileIndex], FWorkTiles[TileIndex].TileState, cap);

          if cap <> '' then
          begin
            with FWorkTiles[TileIndex].FOwner.TileAppearance.StatusIndicatorAppearance do
            begin
              CalculateSize(g, cap);
              xs := r.X + R.Width - GetWidth / 2;
              ys := r.Y - GetHeight / 2;
              xs := xs + FWorkTiles[TileIndex].StatusIndicatorLeft;
              ys := ys + FWorkTiles[TileIndex].StatusIndicatorTop;

              if PtInGPRect(MakeRect(xs, ys, GetWidth, GetHeight), Point(X, Y)) then
              begin
                Result := FWorkTiles[TileIndex];
                break;
              end;
            end;
          end;
        end;
        Inc(TileIndex);
      end;
    end;
  end;
  g.free;
  bmp.free;
end;

function TAdvSmoothTileList.XYToTile(X, Y: Integer; CountEmpty: Boolean = False; IncludeDisabled: Boolean = False): TAdvSmoothTile;
var
  Row, Column: Integer;
  TileIndex: Integer;
  chk: Boolean;
begin
  Result := nil;
  if FAnimate.Enabled or FMove.Enabled then
    Exit;

  TileIndex := PageIndex * TilesPerPage;
  for Row := 0 to Rows - 1 do
  begin
    for Column := 0 to Columns - 1 do
    begin
      if (TileIndex >= 0) and (TileIndex <= FWorkTiles.Count - 1) then
      begin
        chk := ((FWorkTiles[TileIndex] <> MoveTile) and (FMode = tmedit)) or (FMode <> tmEdit);
        if chk then
        begin
          if (CountEmpty or (not CountEmpty and not FWorkTiles[TileIndex].Empty)) and PtInGPRect(FWorkTiles[TileIndex].TileRectangle, Point(X, Y)) then
          begin
            if IncludeDisabled or (not IncludeDisabled and FWorkTiles[TileIndex].Enabled) then
            begin
              Result := FWorkTiles[TileIndex];
              break;
            end;
          end;
        end;
        Inc(TileIndex);
      end;
    end;
  end;
end;

{ TAdvSmoothTile }

procedure TAdvSmoothTile.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTile) then
  begin
    FSubTiles.Assign((Source as TAdvSmoothTile).SubTiles);
    FEnabled := (Source as TAdvSmoothTile).Enabled;
    FEmpty := (Source as TAdvSmoothTile).Empty;
    FContent.Assign((Source as TAdvSmoothTile).Content);
    FContentMaximized.Assign((Source as TAdvSmoothTile).ContentMaximized);
    FStatusIndicator := (Source as TAdvSmoothTile).StatusIndicator;
    FDeleteIndicator := (Source as TAdvSmoothTile).DeleteIndicator;
    FCanDelete := (Source as TAdvSmoothTile).CanDelete;
    FBackTile := (Source as TAdvSmoothTile).BackTile;
    FStatusIndicatorTop := (Source as TAdvSmoothTile).StatusIndicatorTop;
    FStatusIndicatorLeft := (Source as TAdvSmoothTile).StatusIndicatorLeft;
    FDeleteIndicatorLeft := (Source as TAdvSmoothTile).DeleteIndicatorLeft;
    FDeleteIndicatorTop := (Source as TAdvSmoothTile).DeleteIndicatorTop;
  end;
end;

procedure TAdvSmoothTile.Changed;
begin
  FOwner.Changed;
end;

function TAdvSmoothTile.CheckContentTile: Boolean;
begin
  Result := ((FOwner.fmode = tmContent) or (FOwner.FMode = tmContentNavigation)) and ((Self = FOwner.ContentTile) or (Self = FOwner.PrevContentTile) or (Self = Fowner.NextContentTile)) and (FOwner.FMaximized);
end;

function TAdvSmoothTile.CreateContent: TAdvSmoothTileContent;
begin
  Result := TAdvSmoothTileContent.Create(Self);
end;

function TAdvSmoothTile.CreateContentMaximized: TAdvSmoothTileContent;
begin
  Result := TAdvSmoothTileContent.Create(Self);
end;


constructor TAdvSmoothTile.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothTiles).FOwner;
  FEnabled := True;
  FEmpty := False;
  FContent := CreateContent;
  FContentMaximized := CreateContentMaximized;
  FCanDelete := True;
  FSubTiles := (Collection as TAdvSmoothTiles).FOwner.CreateTiles;
  FSubTiles.OnChange := SubTilesChanged;
  FBackTile := False;
  FDeleteIndicatorTop := 0;
  FDeleteIndicatorLeft := 0;
  FStatusIndicatorTop := 0;
  FStatusIndicatorLeft := 0;

  if FOwner.IsDesignTime then
    FDeleteIndicator := 'X';
  FOwner.Changed;            
end;

destructor TAdvSmoothTile.Destroy;
begin
  FSubTiles.Free;
  FContent.Free;
  FContentMaximized.Free;
  inherited;

  FOwner.NextContentTile := nil;
  FOwner.PrevContentTile := nil;
  FOwner.ContentTile := nil;

  FOwner.FMaximized := False;
  FOwner.FMode := tmView;
  FOwner.FMove.Enabled := False;
  FOwner.HoverTile := nil;
  FOwner.IndicationTile := nil;
  FOwner.IndicatorTile := nil;
  FOwner.MoveTile := nil;

  if not (csDestroying in FOwner.ComponentState) then
    FOwner.Changed;
end;

procedure TAdvSmoothTile.Draw(g: TGPGraphics);
var
  DefaultDraw: Boolean;
  r: TGPRectF;
  chkmove: Boolean;
begin
  if not Assigned(FOwner) then
    Exit;

  DefaultDraw := true;
  R := TileRectangle;
  chkmove := (FOwner.FMode = tmEdit) and (self = FOwner.MoveTile);

  if not chkmove then
    R.X := R.X + FOwner.FTilePos;

  if RectanglesInterSect(FOwner.GetTileRectangle, r) then
  begin
    if Assigned(FOwner.OnTileBeforeDraw) then
      FOwner.OnTileBeforeDraw(Self, g, Self, R, DefaultDraw);   

    if DefaultDraw then
    begin
      if CheckContentTile then
        VisualizerMaximized.DrawTile(g, R, Self)
      else
        Visualizer.DrawTile(g, R, Self);
    end;

    if Assigned(FOwner.OnTileAfterDraw) then
      FOwner.OnTileAfterDraw(Self, g, Self, R);
  end;
end;

procedure TAdvSmoothTile.DrawStatus(g: TGPGraphics);
var
  cap: String;
  xs, ys: Double;
  R: TGPRectF; 
  chkmove: Boolean;
begin 
  if not CheckContentTile then
  begin
    R := TileRectangle;
    chkmove := (FOwner.FMode = tmEdit) and (self = FOwner.MoveTile);

    if not chkmove then
      R.X := R.X + FOwner.FTilePos;

    if RectanglesInterSect(FOwner.GetTileRectangle, R) then      
    begin      
      if (FOwner.FMode = tmDelete) then
      begin
        cap := DeleteIndicator;
        if Assigned(FOwner.OnTileDeleteIndicator) then
          FOwner.OnTileDeleteIndicator(Self, Self, TileState, cap);
          
        if cap <> '' then
        begin
          with FOwner.TileAppearance.DeleteIndicatorAppearance do
          begin
            CalculateSize(g, cap);
            xs := r.X - GetWidth / 2;
            ys := r.Y - GetHeight / 2;
            xs := xs + DeleteIndicatorLeft;
            ys := ys + DeleteIndicatorTop;
            Draw(g, SaveRound(xs), SaveRound(ys), 0, 0, true,cap);
          end;
        end;
      end;

      cap := StatusIndicator;
      if Assigned(FOwner.OnTileStatusIndicator) then
        FOwner.OnTileStatusIndicator(Self, Self, TileState, cap);

      if cap <> '' then
      begin
        with FOwner.TileAppearance.StatusIndicatorAppearance do
        begin
          CalculateSize(g, cap);
          xs := r.X + R.Width - GetWidth / 2;
          ys := r.Y - GetHeight / 2;
          xs := xs + StatusIndicatorLeft;
          ys := ys + StatusIndicatorTop;
          Draw(g, SaveRound(xs), SaveRound(ys), 0, 0, true,cap);
        end;
      end;
    end;  
  end;
end;

function TAdvSmoothTile.GetTileFill: TGDIPFill;
begin
  if Enabled then
  begin
    if CheckContentTile then
      Result := FOwner.FTileAppearance.LargeViewFill
    else if (FOwner.FSelectedTile = Self) then
      Result := FOwner.FTileAppearance.SmallViewFillSelected
    else if (FOwner.HoverTile = Self) then
      Result := FOwner.FTileAppearance.SmallViewFillHover
    else
      Result := FOwner.FTileAppearance.SmallViewFill
  end
  else
    Result := FOwner.FTileAppearance.SmallViewFillDisabled
end;

function TAdvSmoothTile.GetTileState: TTileState;
begin
  if Enabled then
  begin
    if CheckContentTile then
      Result := tsMaximized
    else if (FOwner.FSelectedTile = Self) then
      Result := tsSelected
    else if (FOwner.HoverTile = Self) then
      Result := tsHovered
    else
      Result := tsNormal
  end
  else
    Result := tsDisabled;
end;

function TAdvSmoothTile.GetVisualizer: TAdvSmoothTileListVisualizer;
begin
  if Assigned(FVisualizer) then
    Result := FVisualizer
  else
    Result := FOwner.Visualizer;
end;

function TAdvSmoothTile.GetVisualizerMaximized: TAdvSmoothTileListVisualizer;
begin
  if Assigned(FVisualizerMaximized) then
    Result := FVisualizerMaximized
  else
    Result := FOwner.VisualizerMaximized;
end;

procedure TAdvSmoothTile.NavigateAndSelectSubTile(ATileIndex: Integer);
begin
  if (ATileIndex >= 0) and (ATileIndex <= SubTiles.Count - 1) then
  begin
    if SubTiles[ATileIndex].Enabled then
    begin
      FOwner.SelectedTile := SubTiles[ATileIndex];
      FOwner.PageIndex := FOwner.PageIndexForTileIndex(ATileIndex)
    end;
  end;
end;

procedure TAdvSmoothTile.SelectSubTile(ATileIndex: Integer);
begin
  if (ATileIndex >= 0) and (ATileIndex <= SubTiles.Count - 1) then
  begin
    if SubTiles[ATileIndex].Enabled then
      FOwner.SelectedTile := SubTiles[ATileIndex];
  end;
end;

procedure TAdvSmoothTile.SetBackTile(const Value: Boolean);
begin
  if FBackTile <> Value then
  begin
    FBackTile := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetCanDelete(const Value: Boolean);
begin
  if FCanDelete <> Value then
  begin
    FCanDelete := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetContent(const Value: TAdvSmoothTileContent);
begin
  if FContent <> Value then
  begin
    FContent.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetContentMaximized(const Value: TAdvSmoothTileContent);
begin
  if FContentMaximized <> Value then
  begin
    FContentMaximized.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetDeleteIndicator(const Value: String);
begin
  if FDeleteIndicator <> Value then
  begin
    FDeleteIndicator := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetDeleteIndicatorLeft(const Value: Integer);
begin
  if FDeleteIndicatorLeft <> Value then
  begin
    FDeleteIndicatorLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetDeleteIndicatorTop(const Value: Integer);
begin
  if FDeleteIndicatorTop <> Value then
  begin
    FDeleteIndicatorTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetEmpty(const Value: Boolean);
begin
  if FEmpty <> Value then
  begin
    FEmpty := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetStatusIndicator(const Value: String);
begin
  if FStatusIndicator <> Value then
  begin
    FStatusIndicator := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetStatusIndicatorLeft(const Value: Integer);
begin
  if FStatusIndicatorLeft <> Value then
  begin
    FStatusIndicatorLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetStatusIndicatorTop(const Value: Integer);
begin
  if FStatusIndicatorTop <> Value then
  begin
    FStatusIndicatorTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetSubTiles(const Value: TAdvSmoothTiles);
begin
  if FSubTiles <> Value then
  begin
    FSubTiles.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetVisualizer(const Value: TAdvSmoothTileListVisualizer);
begin
  if FVisualizer <> Value then
  begin
    FVisualizer := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SetVisualizerMaximized(
  const Value: TAdvSmoothTileListVisualizer);
begin
  if FVisualizerMaximized <> Value then
  begin
    FVisualizerMaximized := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTile.SubTilesChanged(Sender: TObject);
begin
  Changed;
end;

{ TAdvSmoothTileAppearance }

procedure TAdvSmoothTileAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothTileAppearance then
  begin
    FStatusIndicatorAppearance.Assign((Source as TAdvSmoothTileAppearance).StatusIndicatorAppearance);
    FDeleteIndicatorAppearance.Assign((Source as TAdvSmoothTileAppearance).DeleteIndicatorAppearance);    
    FSmallViewFillDisabled.Assign((Source as TAdvSmoothTileAppearance).SmallViewFillDisabled);
    FSmallViewFillSelected.Assign((Source as TAdvSmoothTileAppearance).SmallViewFillSelected);
    FSmallViewFillHover.Assign((Source as TAdvSmoothTileAppearance).SmallViewFillHover);
    FSmallViewFill.Assign((Source as TAdvSmoothTileAppearance).SmallViewFill);
    FSmallViewFontDisabled.Assign((Source as TAdvSmoothTileAppearance).SmallViewFontDisabled);
    FSmallViewFontSelected.Assign((Source as TAdvSmoothTileAppearance).SmallViewFontSelected);
    FSmallViewFontHover.Assign((Source as TAdvSmoothTileAppearance).SmallViewFontHover);
    FSmallViewFont.Assign((Source as TAdvSmoothTileAppearance).SmallViewFont);
    FLargeViewFill.Assign((Source as TAdvSmoothTileAppearance).LargeViewFill);
    FLargeViewFont.Assign((Source as TAdvSmoothTileAppearance).LargeViewFont);
    FVerticalSpacing := (Source as TAdvSmoothTileAppearance).VerticalSpacing;
    FHorizontalSpacing := (Source as TAdvSmoothTileAppearance).HorizontalSpacing;
    FBackGround := (Source as TAdvSmoothTileAppearance).BackGround;
  end;
end;

procedure TAdvSmoothTileAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTileAppearance.Create(AOwner: TAdvSmoothTileList);
begin
  FOwner := AOwner;
  FSmallViewFill := TGDIPFill.Create;
  FSmallViewFill.OnChange := FillChanged;
  FSmallViewFillDisabled := TGDIPFill.Create;
  FSmallViewFillDisabled.OnChange := FillChanged;
  FSmallViewFillSelected := TGDIPFill.Create;
  FSmallViewFillSelected.OnChange := FillChanged;
  FSmallViewFillHover := TGDIPFill.Create;
  FSmallViewFillHover.OnChange := FillChanged;
  FLargeViewFill := TGDIPFill.Create;
  FLargeViewFill.OnChange := FillChanged;
  FVerticalSpacing := 15;
  FHorizontalSpacing := 15;
  FBackGround := true;
  FTargetTileColor := clGreen;
  FMovingTileColor := clRed;

  FSmallViewFontDisabled := TFont.Create;
  FSmallViewFontSelected := TFont.Create;
  FSmallViewFont := TFont.Create;
  FSmallViewFontHover := TFont.Create;
  FLargeViewFont := TFont.Create;


  {$IFNDEF DELPHI9_LVL}
  FSmallViewFontDisabled.Name := 'Tahoma';
  FSmallViewFontSelected.Name := 'Tahoma';
  FSmallViewFont.Name := 'Tahoma';
  FSmallViewFontHover.Name := 'Tahoma';
  FLargeViewFont.Name := 'Tahoma';
  {$ENDIF}

  FSmallViewFontDisabled.OnChange := FontChanged;
  FSmallViewFontSelected.OnChange := FontChanged;
  FSmallViewFont.OnChange := FontChanged;
  FSmallViewFontHover.OnChange := FontChanged;
  FLargeViewFont.OnChange := FontChanged;


  FStatusIndicatorAppearance := TGDIPStatus.Create;
  FStatusIndicatorAppearance.OnChange := IndicatorChanged;

  FDeleteIndicatorAppearance := TGDIPStatus.Create;
  FDeleteIndicatorAppearance.OnChange := IndicatorChanged;  
end;

destructor TAdvSmoothTileAppearance.Destroy;
begin
  FStatusIndicatorAppearance.Free;
  FDeleteIndicatorAppearance.Free;
  FLargeViewFont.Free;
  FSmallViewFont.Free;
  FSmallViewFontDisabled.Free;
  FSmallViewFontSelected.Free;
  FSmallViewFontHover.Free;

  FLargeViewFill.Free;
  FSmallViewFill.Free;
  FSmallViewFillDisabled.Free;
  FSmallViewFillSelected.Free;
  FSmallViewFillHover.Free;
  inherited;
end;

procedure TAdvSmoothTileAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileAppearance.IndicatorChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFill(const Value: TGDIPFill);
begin
  if FSmallViewFill <> Value then
  begin
    FSmallViewFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFillDisabled(
  const Value: TGDIPFill);
begin
  if FSmallViewFillDisabled <> Value then
  begin
    FSmallViewFillDisabled.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFillHover(
  const Value: TGDIPFill);
begin
  if FSmallViewFillHover <> Value then
  begin
    FSmallViewFillHover.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFillSelected(
  const Value: TGDIPFill);
begin
  if FSmallViewFillSelected <> Value then
  begin
    FSmallViewFillSelected.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFont(const Value: TFont);
begin
  if FSmallViewFont <> Value then
  begin
    FSmallViewFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFontDisabled(const Value: TFont);
begin
  if FSmallViewFontDisabled <> Value then
  begin
    FSmallViewFontDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFontHover(const Value: TFont);
begin
  if FSmallViewFontHover <> Value then
  begin
    FSmallViewFontHover.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetSmallViewFontSelected(const Value: TFont);
begin
  if FSmallViewFontSelected <> Value then
  begin
    FSmallViewFontSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetHorizontalSpacing(const Value: Integer);
begin
  if FHorizontalSpacing <> Value then
  begin
    FHorizontalSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetStatusIndicatorAppearance(
  const Value: TGDIPStatus);
begin
  if FStatusIndicatorAppearance <> Value then
  begin
    FStatusIndicatorAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetTargetTileColor(const Value: TColor);
begin
  if FTargetTileColor <> Value then
  begin
    FTargetTileColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetLargeViewFill(const Value: TGDIPFill);
begin
  if FLargeViewFill <> Value then
  begin
    FLargeViewFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTileAppearance.SetLargeViewFont(const Value: TFont);
begin
  if FLargeViewFont <> Value then
  begin
    FLargeViewFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothTileAppearance.SetMovingTileColor(const Value: TColor);
begin
  if FMovingTileColor <> Value then
  begin
    FMovingTileColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetBackGround(const Value: Boolean);
begin
  if FBackGround <> Value then
  begin
    FBackGround := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetDeleteIndicatorAppearance(
  const Value: TGDIPStatus);
begin
  if FDeleteIndicatorAppearance <> Value then
  begin
    FDeleteIndicatorAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileAppearance.SetVerticalSpacing(const Value: Integer);
begin
  if FVerticalSpacing <> Value then
  begin
    FVerticalSpacing := Value;
    Changed;
  end;
end;

{ TAdvSmoothTileListNavigation }

procedure TAdvSmoothTileListNavigation.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTileListNavigation) then
  begin
    FVisible := (Source as TAdvSmoothTileListNavigation).Visible;
    FFill.Assign((Source as TAdvSmoothTileListNavigation).Fill);
    FHeight := (Source as TAdvSmoothTileListNavigation).Height;
    FNavigation := (Source as TAdvSmoothTileListNavigation).Navigation;
    FBulletColor := (Source as TAdvSmoothTileListNavigation).BulletColor;
    FBulletSelectedColor := (Source as TAdvSmoothTileListNavigation).BulletSelectedColor;
    FBulletSize := (Source as TAdvSmoothTileListNavigation).BulletSize;
    FArrowColor := (Source as TAdvSmoothTileListNavigation).ArrowColor;
    FArrowRectangleSize := (Source as TAdvSmoothTileListNavigation).ArrowRectangleSize;
    FArrowSize := (Source as TAdvSmoothTileListNavigation).ArrowSize;
    FShowPages := (Source as TAdvSmoothTileListNavigation).ShowPages;
    FArrowNavigation := (Source as TAdvSmoothTileListNavigation).ArrowNavigation;
    FCaption := (Source as TAdvSmoothTileListNavigation).Caption;
    FFloat := (Source as TAdvSmoothTileListNavigation).Float;
    FFont.Assign((Source as TAdvSmoothTileListNavigation).Font);
  end;
end;

procedure TAdvSmoothTileListNavigation.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTileListNavigation.Create(AOwner: TAdvSmoothTileList);
begin
  FOwner := AOwner;
  FHeight := 30;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FCaptionPosition := tpCenterCenter;
  FNavigation := True;
  FVisible := True;
  FCaptionLeft := 0;
  FCaptionTop := 0;
  FBulletColor := clWhite;
  FBulletSelectedColor := $E1A732;
  FBulletSize := 9;
  FArrowColor := clWhite;
  FArrowRectangleSize := 50;
  FArrowSize := 12;
  FShowPages := True;
  FArrowNavigation := True;
  FFloat := False;

  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FFont.OnChange := FontChanged;
end;

destructor TAdvSmoothTileListNavigation.Destroy;
begin
  FFont.Free;
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothTileListNavigation.Draw(g: TGPGraphics; r: TGPRectF);
var
  I: Integer;
  b: TGPSolidBrush;
  lb: TGPLinearGradientBrush;
  er: TGPRectF;
  st, si: Integer;
  c: TColor;
  w: double;
  p: TGPPen;
  s: integer;
  sz: integer;
  pth: TGPGraphicsPath;
  o: Byte;
  DefaultBullet: Boolean;
  ft: TGPFont;
  sf: TGPStringFormat;
  sizer: TGPRectF;
  x, y: Double;
  rgn: TGPRegion;
  rs: TGPRectF;
begin
  if not Visible then
    Exit;

  DefaultBullet := True;

  Fill.Fill(g, r);
  if Navigation then
  begin
    if ShowPages and not FOwner.FMaximized then
    begin
      b := TGPSolidBrush.Create(MakeColor(50, clBlack));
      st := BulletSize;
      si := st - 3;
      if ArrowNavigation then
      begin
        sz := ArrowRectangleSize;
        rs := r;
        rs.X := rs.X + sz;
        rs.Width := rs.Width - sz * 2;
      end
      else
        rs := r;

      rgn := TGPRegion.Create(rs);
      g.SetClip(rgn);

      for I := 0 to FOwner.PageCount - 1 do
      begin
        if FOwner.PageIndex = I then
          c := BulletSelectedColor
        else
          c := BulletColor;

        w := FOwner.PageCount * (st + 5);

        er := MakeRect(r.X + (r.Width / 2) - (w / 2) + ((st + 5) * I) + 2.5, r.Y + 2 + (r.Height - st) / 2, st, st);

        if Assigned(FOwner.OnBulletDraw) then
          FOwner.OnBulletDraw(Self, g, I, er, r, DefaultBullet);

        if DefaultBullet then
        begin
          g.FillEllipse(b, er);

          er := MakeRect(r.X + (r.Width / 2) - (w / 2) + ((st + 5) * I)  + 2.5 + (st - si) / 2, r.Y + 2 + (r.Height - si) / 2, si, si);
          lb := TGPLinearGradientBrush.Create(er, MakeColor(150, c), Makecolor(230, c), LinearGradientModeVertical);
          g.FillEllipse(lb, er);
          lb.Free;
        end;
      end;

      rgn.Free;
      g.ResetClip;
      b.Free;
    end
    else if not ShowPages then
    begin
      ft := g.MakeFont(Font);
      sf := TGPStringFormat.Create;
      g.MeasureString(Caption, Length(Caption), ft, r, sf, sizer);
      GetTextPosition(x, y, r, sizer.Width, sizer.Height, CaptionPosition);
      b := TGPSolidBrush.Create(MakeColor(255, Font.Color));
      g.DrawString(Caption, Length(Caption), ft, MakePoint(x + r.X + CaptionLeft, y + r.Y + CaptionTop), b);
      b.Free;
      sf.Free;
      ft.Free;
    end;

    if ArrowNavigation then
    begin
      sz := ArrowRectangleSize;
      s := ArrowSize;

      o := 230;
      if (FOwner.FMaximized) and Assigned(FOwner.ContentTile) then
      begin
        if FOwner.ContentTile.Index = FOwner.GetFirstContentIndex then
          o := 100;
      end
      else
      begin
        if (FOwner.PageIndex = 0) or (FOwner.FWorkTiles.Count = 0) then
          o := 100;
      end;

      b := TGPSolidBrush.Create(MakeColor(o, ArrowColor));
      pth := TGPGraphicsPath.Create;
      pth.AddLine(r.X + sz / 2 - s / 2, r.Y + r.Height / 2, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 - s / 2);
      pth.AddLine(r.X + sz / 2 + s / 2, r.Y + r.Height / 2 - s / 2, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 + s / 2);
      pth.AddLine(r.X + sz / 2 - s / 2, r.Y + r.Height / 2, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 + s / 2);
      g.FillPath(b, pth);
      b.Free;

      pth.Reset;

      o := 230;
      if (FOwner.FMaximized) and Assigned(FOwner.ContentTile) then
      begin
        if FOwner.ContentTile.Index = FOwner.GetLastContentIndex then
          o := 100;
      end
      else
      begin
        if (FOwner.PageIndex = FOwner.PageCount - 1)  or (FOwner.FWorkTiles.Count = 0) then
          o := 100;
      end;

      b := TGPSolidBrush.Create(MakeColor(o, ArrowColor));
      pth.AddLine(r.X + r.Width - sz / 2 + s / 2, r.Y + r.Height / 2, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 - s / 2);
      pth.AddLine(r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 - s / 2, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 + s / 2);
      pth.AddLine(r.X + r.Width - sz / 2 + s / 2, r.Y + r.Height / 2, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 + s / 2);

      g.FillPath(b, pth);
      b.Free;


      if not FOwner.FMetroStyle then
      begin
        p := TGPPen.Create(MakeColor(Fill.BorderOpacity, Fill.BorderColor), Fill.BorderWidth);
        g.DrawLine(p, r.X + sz, r.Y, r.X + sz, r.Y + r.Height);
        g.DrawLine(p, r.X + r.Width - sz, r.Y, r.X + r.Width - sz, r.Y + r.Height);
        g.DrawLine(p, r.X + sz / 2 - s / 2, r.Y + r.Height / 2, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 - s / 2);
        g.DrawLine(p, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 - s / 2, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 + s / 2);
        g.DrawLine(p, r.X + r.Width - sz / 2 + s / 2, r.Y + r.Height / 2, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 - s / 2);
        g.DrawLine(p, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 - s / 2, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 + s / 2);
        p.Free;
      end;


      p := TGPPen.Create(MakeColor(100, clWhite), Fill.BorderWidth);
      g.DrawLine(p, r.X + sz + 1, r.Y, r.X + sz + 1, r.Y + r.Height);
      g.DrawLine(p, r.X + r.Width - sz + 1, r.Y, r.X + r.Width - sz + 1, r.Y + r.Height);
      g.DrawLine(p, r.X + sz / 2 - s / 2, r.Y + r.Height / 2, r.X + sz / 2 + s / 2, r.Y + r.Height / 2 + s / 2);
      g.DrawLine(p, r.X + r.Width - sz / 2 + s / 2, r.Y + r.Height / 2, r.X + r.Width - sz / 2 - s / 2, r.Y + r.Height / 2 + s / 2);
      p.Free;
    end;
  end;
end;

procedure TAdvSmoothTileListNavigation.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileListNavigation.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothTileListNavigation.GetHeight: Integer;
begin
  if Visible then
    Result := FHeight
  else
    Result := 0;
end;

procedure TAdvSmoothTileListNavigation.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetArrowNavigation(const Value: Boolean);
begin
  if FArrowNavigation <> Value then
  begin
    FArrowNavigation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetArrowRectangleSize(
  const Value: Integer);
begin
  if FArrowRectangleSize <> Value then
  begin
    FArrowRectangleSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetArrowSize(const Value: Integer);
begin
  if FArrowSize <> Value then
  begin
    FArrowSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetBulletColor(const Value: TColor);
begin
  if FBulletColor <> Value then
  begin
    FBulletColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetShowPages(
  const Value: Boolean);
begin
  if FShowPages <> Value then
  begin
    FShowPages := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetBulletSelectedColor(
  const Value: TColor);
begin
  if FBulletSelectedColor <> Value then
  begin
    FBulletSelectedColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetBulletSize(const Value: Integer);
begin
  if FBulletSize <> Value  then
  begin
    FBulletSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetCaptionLeft(const Value: Integer);
begin
  if FCaptionLeft <> Value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetCaptionPosition(
  const Value: TTileListTextPosition);
begin
  if FCaptionPosition <> Value then
  begin
    FCaptionPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetCaptionTop(const Value: Integer);
begin
  if FCaptionTop <> Value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTileListNavigation.SetFloat(const Value: Boolean);
begin
  if FFloat <> Value then
  begin
    FFloat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

procedure TAdvSmoothTileListNavigation.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetNavigation(const Value: Boolean);
begin
  if FNavigation <> Value then
  begin
    FNavigation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetNextHint(const Value: String);
begin
  if FNextHint <> Value then
  begin
    FNextHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetPreviousHint(const Value: String);
begin
  if FPreviousHint <> Value then
  begin
    FPreviousHint := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileListNavigation.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

function TAdvSmoothTileListNavigation.XYToBullet(X, Y: Integer; Header: Boolean): integer;
var
  r: TGPRectF;
  st: integer;
  i: integer;
  er: TGPRectF;
  w: integer;
  sz: Integer;
  rs: TGPRectF;
begin
  Result := -1;
  if not Visible or not ShowPages or not Navigation or FOwner.FMaximized then
     Exit;

  if Header then
    r := FOwner.GetHeaderRectangle
  else
    r := FOwner.GetFooterRectangle;

  if ArrowNavigation then
  begin
    sz := ArrowRectangleSize;
    rs := r;
    rs.X := rs.X + sz;
    rs.Width := rs.Width - sz * 2;
  end
  else
    rs := r;

  if PtInGPRect(rs, Point(X, Y)) then
  begin
    st := BulletSize;
    w := FOwner.PageCount * (st + 5);
    for I := 0 to FOwner.PageCount - 1 do
    begin
      er := MakeRect(r.X + (r.Width / 2) - (w / 2) + ((st + 5) * I) + 2.5, r.Y + 2 + (r.Height - st) / 2, st, st);
      if PtInGPRect(er, Point(X, Y)) then
      begin
        Result := I;
        break;
      end;
    end;
  end;
end;

function TAdvSmoothTileListNavigation.XYToNavigation(X, Y: Integer; Header: Boolean): Integer;
var
  r: TGPRectF;
  rleft, rright: TGPRectF;
  sz: integer;
begin
  Result := -1;
  if not Visible or not ArrowNavigation or not Navigation then
     Exit;

  if Header then
    r := FOwner.GetHeaderRectangle
  else
    r := FOwner.GetFooterRectangle;

  sz := ArrowRectangleSize;

  rleft := MakeRect(r.X, r.Y, sz, r.Height);
  rright := MakeRect(r.X + r.Width - sz, r.Y, sz, r.Height);

  if PtInGPRect(rleft, Point(X, Y)) then
    Result := 0
  else if PtInGPRect(rright, Point(X, Y)) then
    Result := 1
end;

{ TAdvSmoothTileListVisualizer }

constructor TAdvSmoothTileListVisualizer.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);
end;

function TAdvSmoothTileListVisualizer.DoMouseDown(Tile: TAdvSmoothTile; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := True;
end;

function TAdvSmoothTileListVisualizer.DoMouseMove(Tile: TAdvSmoothTile; Shift: TShiftState; X,
  Y: Integer): Boolean;
begin
  Result := True;
end;

function TAdvSmoothTileListVisualizer.DoMouseUp(Tile: TAdvSmoothTile; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := True;
end;

function TAdvSmoothTileListVisualizer.DrawCheckBox(g: TGPGraphics; R: TGPRectF;
  Tile: TAdvSmoothTile; TileContent: TAdvSmoothTileContent): TGPRectF;
var
  hc: HDC;
  ca: TCanvas;
  htheme: THandle;
  DChecked: Integer;
  o: TAdvSmoothTileList;
  ThemeStyle: dword;
  rt: TGPRectF;
  rtt: TRect;
  x, y: Double;
begin
  if not TileContent.ShowCheckBox then
    Exit;

  o := Tile.TileList;

  if not Assigned(o) then
    Exit;

  hc := g.GetHDC;

  ca := TCanvas.Create;
  ca.Handle := hc;

  if TileContent.Checked then
  begin
    DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
    if TileContent.CheckBoxEnabled then
      ThemeStyle := CBS_CHECKEDNORMAL
    else
      ThemeStyle := CBS_CHECKEDDISABLED;
  end
  else
  begin
    DChecked := DFCS_BUTTONCHECK;
    if TileContent.CheckBoxEnabled then
      ThemeStyle := CBS_UNCHECKEDNORMAL
    else
      ThemeStyle := CBS_UNCHECKEDDISABLED;
  end;

  rt := MakeRect(r.X + 3, r.Y + 3, r.Width - 3, r.Height - 3);

  x := 0;
  y := 0;

  if (TileContent.CheckBoxPosition <> cpCustom) then
    GetCheckBoxPosition(x, y, rt, 16, 16, TileContent.CheckBoxPosition)
  else
  begin
    x := TileContent.CheckBoxLeft;
    y := TileContent.CheckBoxTop;
  end;

  x := x + rt.X;
  y := y + rt.Y;  

  rtt := Bounds(Round(x), Round(y), 16, 16);

  if FIsWinXP and IsThemeActive then
  begin
    htheme := OpenThemeData(o.Handle,'button');
    DrawThemeBackground(HTheme,ca.Handle, BP_CHECKBOX,ThemeStyle,@rtt,nil);
    CloseThemeData(htheme);
  end
  else
    DrawFrameControl(ca.Handle,rtt,DFC_BUTTON, DChecked);

  ca.Free;

  g.ReleaseHDC(hc);
end;

function TAdvSmoothTileListVisualizer.DrawText(g: TGPGraphics;
  R:TGPRectF; Tile: TAdvSmoothTile; Text: String): TGPRectF;
var
  rt, sizer: TGPRectF;
  ct: TAdvSmoothTileContent;
  ft: TFont;
  sf: TGPStringFormat;
  f: TGPFont;
  x, y: Double;
  b: TGPSolidBrush;
  xsize, ysize: Double;
begin
  if Tile.CheckContentTile then
    ct := Tile.ContentMaximized
  else
    ct := Tile.Content;

  ft := TFont.Create;

  if Tile.Enabled then
  begin
    if Tile.CheckContentTile then
      ft.Assign(Tile.FOwner.FTileAppearance.LargeViewFont)
    else if (Tile.FOwner.FSelectedTile = Tile) then
      ft.Assign(Tile.FOwner.FTileAppearance.SmallViewFontSelected)
    else if (Tile.FOwner.HoverTile = Tile) then
      ft.Assign(Tile.FOwner.FTileAppearance.SmallViewFontHover)
    else
      ft.Assign(Tile.FOwner.FTileAppearance.SmallViewFont)
  end
  else
    ft.Assign(Tile.FOwner.FTileAppearance.SmallViewFontDisabled);

  if Assigned(Tile.FOwner.OnTileFont) then
    Tile.FOwner.OnTileFont(Self, Tile, Tile.TileState, ft);

  rt := MakeRect(r.X + 2, r.Y + 2, r.Width - 4, r.Height - 4);

  f := g.MakeFont(ft);
  sf := TGPStringFormat.Create;

  g.MeasureString(Text, Length(Text), f, rt, sf, sizer);

  if (ct.TextPosition <> tpCustom) then
    GetTextPosition(x, y, rt, sizer.Width, sizer.Height, ct.TextPosition)
  else
  begin
    x := ct.TextLeft;
    y := ct.TextTop;
  end;

  x := x + rt.X;
  y := y + rt.Y;

  b := TGPSolidBrush.Create(MakeColor(255, ft.Color));
  xsize := rt.Width - x + rt.X;
  ysize := rt.Height - y + rt.Y;

  g.DrawString(Text, length(Text), f, MakeRect(x, y, xsize, ysize), sf, b);
  b.Free;

  sf.Free;

  ft.Free;

  Result := MakeRect(x, y, sizer.Width, sizer.Height);
end;

function TAdvSmoothTileListVisualizer.DrawTile(g: TGPGraphics;
  R: TGPRectF; Tile: TAdvSmoothTile): TGPRectF;
var
  chkmove, chkind: Boolean;
  pen: TGPPen;
  c: TColor;
  ct: TAdvSmoothTileContent;
  str: string;
  fl: TGDIPFill;
  rgn: TGPRegion;
  rt: TGPRectF;
begin
  chkmove := (Tile.FOwner.FMode = tmEdit) and (Tile = Tile.FOwner.MoveTile);
  chkind := (Tile.FOwner.FMode = tmEdit) and (Tile = Tile.FOwner.IndicationTile);
  
  if RectanglesInterSect(R, Tile.FOwner.GetTileRectangle) or chkmove then
  begin
    c := Tile.FOwner.TileAppearance.MovingTileColor;
    if chkind then
      c := Tile.FOwner.TileAppearance.TargetTileColor;

    if not Tile.Empty then
    begin
      if Tile.FOwner.FTileAppearance.BackGround then
      begin
        fl := TGDIPFill.Create;
        fl.Assign(Tile.GetTileFill);
        if Assigned(Tile.FOwner.OnTileFill) then
          Tile.FOwner.OnTileFill(Self, Tile, Tile.TileState, fl);

        R := fl.Fill(g,R);
        fl.Free;
      end;

      if Tile.CheckContentTile then
        ct := Tile.ContentMaximized
      else
        ct := Tile.Content;


      str := ct.Text;
      if Assigned(Tile.FOwner.OnTileText) then
        Tile.FOwner.OnTileText(Self, Tile, Tile.TileState, str);

      rgn := TGPRegion.Create(MakeRect(r.X, r.Y, r.Width + 1, r.Height + 1));
      g.SetClip(rgn);

      rt := r;
      Result := DrawText(g, rt, Tile, str);
      DrawCheckBox(g, rt, Tile, ct);
      g.ResetClip;
      rgn.Free;
    end;

    if chkmove or chkind then
    begin
      pen := TGPPEN.Create(MakeColor(255, c), 5);
      g.DrawRectangle(pen, R);
      pen.Free;
    end;
  end;
end;

function TAdvSmoothTileListVisualizer.XYToAnchor(Tile: TAdvSmoothTile; pX,
  pY: Integer; Focus: Boolean = False): string;
begin
  Result := '';
end;

{ TAdvSmoothTileContent }

procedure TAdvSmoothTileContent.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTileContent) then
  begin
    FImageIndex := (Source as TAdvSmoothTileContent).ImageIndex;
    FImageName := (Source as TAdvSmoothTileContent).ImageName;
    FImage.Assign((Source as TAdvSmoothTileContent).Image);
    FText := (Source as TAdvSmoothTileContent).Text;
    FTextLeft := (Source as TAdvSmoothTileContent).TextLeft;
    FTextTop := (Source as TAdvSmoothTileContent).TextTop;
    FTextPosition := (Source as TAdvSmoothTileContent).TextPosition;
    FImageStretch := (Source as TAdvSmoothTileContent).ImageStretch;
    FImageAspectRatio := (Source as TAdvSmoothTileContent).ImageAspectRatio;
    FOfficeHint.Assign((Source as TAdvSmoothTileContent).OfficeHint);
    FShowCheckBox := (Source as TAdvSmoothTileContent).ShowCheckBox;
    FChecked := (Source as TAdvSmoothTileContent).Checked;
    FCheckBoxEnabled := (Source as TAdvSmoothTileContent).CheckBoxEnabled;
    FCheckBoxTop := (Source as TAdvSmoothTileContent).CheckBoxTop;
    FCheckBoxLeft := (Source as TAdvSmoothTileContent).CheckBoxLeft;
    FCheckBoxPosition := (Source as TAdvSmoothTileContent).CheckBoxPosition;
    FHint := (Source as TAdvSmoothTileContent).Hint;
  end;
end;

procedure TAdvSmoothTileContent.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothTileContent.Create(AOwner: TAdvSmoothTile);
begin
  FOwner := AOwner;
  FImage := TAdvGDIPPicture.Create;
  FImage.OnChange := ImageChanged;
  FImageIndex := -1;
  FTextPosition := tpCenterCenter;
  FTextLeft := 0;
  FTextTop := 0;
  FImageAspectRatio := True;
  FCheckBoxTop := 0;
  FCheckBoxLeft := 0;
  FCheckBoxPosition := cpCenterLeft;
  FImageStretch := False;
  FShowCheckBox := False;
  FChecked := False;
  FCheckBoxEnabled := True;
  FOfficeHint := TAdvHintInfo.Create;
end;

destructor TAdvSmoothTileContent.Destroy;
begin
  FImage.Free;
  FOfficeHint.Free;
  inherited;
end;

procedure TAdvSmoothTileContent.ImageChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTileContent.SetCheckBoxEnabled(const Value: Boolean);
begin
  if FCheckBoxEnabled <> Value then
  begin
    FCheckBoxEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetCheckBoxLeft(const Value: Integer);
begin
  if FCheckBoxLeft <> Value then
  begin
    FCheckBoxLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetCheckBoxPosition(
  const Value: TTileListCheckBoxPosition);
begin
  if FCheckBoxPosition <> Value then
  begin  
    FCheckBoxPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetCheckBoxTop(const Value: Integer);
begin
  if FCheckBoxTop <> Value then
  begin
    FCheckBoxTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetChecked(const Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetImage(const Value: TAdvGDIPPicture);
begin
  if FImage <> Value then
  begin
    FImage.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetImageAspectRatio(const Value: Boolean);
begin
  if FImageAspectRatio <> Value then
  begin
    FImageAspectRatio := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetImageIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetImageName(const Value: String);
begin
  if FImageName <> Value then
  begin
    FImageName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetImageStretch(const Value: Boolean);
begin
  if FImageStretch <> Value then
  begin
    FImageStretch := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

procedure TAdvSmoothTileContent.SetShowCheckBox(const Value: Boolean);
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetText(const Value: String);
begin
  if FText <> Value then
  begin
    FText := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetTextLeft(const Value: Integer);
begin
  if FTextLeft <> Value then
  begin
    FTextLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetTextPosition(const Value: TTileListTextPosition);
begin
  if FTextPosition <> Value then
  begin
    FTextPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTileContent.SetTextTop(const Value: Integer);
begin
  if FTextTop <> Value then
  begin
    FTextTop := Value;
    Changed;
  end;
end;

function TAdvSmoothTileContent.TileList: TAdvSmoothTileList;
begin
  Result := FOwner.TileList;
end;

function TAdvSmoothTileContent.XYToCheckBox(X, Y: Integer): Boolean;
var
  rt: TGPRectF;
  rtt: TRect;
  R: TGPRectF;
  chkmove: Boolean;
  xt, yt: Double;
begin
  Result := False;
  if not ShowCheckBox then
    Exit;

  R := FOwner.TileRectangle;
  chkmove := (FOwner.FOwner.FMode = tmEdit) and (FOwner = FOwner.FOwner.MoveTile);

  if not chkmove then
    R.X := R.X + FOwner.FOwner.FTilePos;    

  rt := MakeRect(r.X + 3, r.Y + 3, r.Width - 3, r.Height - 3);

  xt := 0;
  yt := 0;

  if (CheckBoxPosition <> cpCustom) then
    GetCheckBoxPosition(xt, yt, rt, 16, 16, CheckBoxPosition)
  else
  begin
    xt := CheckBoxLeft;
    yt := CheckBoxTop;
  end;

  xt := xt + rt.X;
  yt := yt + rt.Y;  

  rtt := Bounds(Round(xt), Round(yt), 16, 16);

  Result := PtInRect(rtt, Point(X, Y));

end;

{$IFNDEF DELPHI2006_LVL}

procedure TMargins.Assign(Source: TPersistent);
begin
  if Source is TMargins then
  begin
    FLeft := (Source as TMargins).Left;
    FTop := (Source as TMargins).Top;
    FRight := (Source as TMargins).Right;
    FBottom := (Source as TMargins).Bottom;
  end;
end;

procedure TMargins.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TMargins.Create(AOwner: TAdvSmoothTileList);
begin
  FOwner := AOwner;
  FLeft := 0;
  FBottom := 0;
  FRight := 0;
  FTop := 0;
end;

procedure TMargins.SetBottom(const Value: integer);
begin
  if FBottom <> value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TMargins.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TMargins.SetRight(const Value: integer);
begin
  if FRight <> value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TMargins.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

{$ENDIF}

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.
