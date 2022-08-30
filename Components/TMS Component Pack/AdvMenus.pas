{***************************************************************************}
{ TAdvMenu & TAdvPopupMenu component                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2015                                        }
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

{$I TMSDEFS.INC}
{$R AdvMenuResources.res}
{$DEFINE DESIGNTIMEPREVIEW}

unit AdvMenus;

interface

uses
  Forms, Windows, SysUtils, Classes, Graphics, Menus, Controls, AdvGDIP
  {$IFDEF DELPHI2006_LVL}
  , ActnMan
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF DELPHIXE2_LVL}
  , Themes
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 6; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.1.0.2 : Fixed issues with multimonitor setup
  // 1.2.0.0 : New Whidbey appearance
  //         : Improved styler setup & support to use stylers in datamodules
  // 1.2.0.1 : improved popup menu handling
  // 1.2.1.0 : check drawing for items with image
  // 1.2.2.0 : Improved drawing of checked items
  // 1.2.2.1 : Improvement in painting for Bidi RightToLeft style
  // 1.2.3.0 : Added public property DirectUpdate: boolean;
  // 1.2.3.1 : Fixed font style assign issue for popupmenu
  // 1.2.3.2 : Fixed issue with identical MDI child names
  // 2.0.0.0 : New Office 2007 styles
  // 2.0.5.0 : New support for DisabledImages in TAdvMainMenu, TAdvPopupMenu
  // 2.5.0.0 : New Office 2007 Silver style added
  // 2.5.0.1 : Fixed : issue with hidden root menu items
  // 2.5.1.0 : Fixed : issue with disabled menu items & notes
  //         : New : property ShowNotes added
  // 2.5.1.1 : Fixed : issue with disabled menu items
  // 2.5.2.0 : New : property IntlKeybShortCuts added for automatic internationalization of keyboard values
  // 2.5.2.1 : Fixed : issue with IsRightAligned function with menus with hidden items
  // 2.5.2.2 : Improved : issue with MDI window icon drawing
  // 2.5.2.3 : Improved : small or large icon selection for MDI window icon
  // 2.5.2.4 : Fixed : issue with destroying menu
  // 2.5.2.5 : Fixed : GDI object leak in drawing MDI icon
  // 2.5.2.6 : Fixed : issue with painting update after menu merging
  // 2.5.2.7 : Fixed : issue with use of menu bitmaps > 16x16
  // 2.5.2.8 : Fixed : full width rootitem drawing for menus on multiple rows
  // 2.5.2.9 : Fixed : checkbox drawing positions in iconbar
  //         : Fixed : painting issue with hidden root items
  // 2.5.2.10: Fixed : issue with IntlKeybShortCuts = true and F1..F12 shortcuts
  // 2.5.2.11: Fixed : issue with icon bar background color
  // 2.5.2.12: Fixed : issue with menu item height for imagelists with large images
  // 2.5.3.0 : New : support to use ActionManager.DisabledImages
  // 2.5.3.1 : Fixed : Flickering issue when used in BidiMode righttoleft
  // 2.5.3.2 : Fixed : Item height calculation with large images & notes text
  // 2.5.3.3 : Fixed : Width calculation issue with large notes
  // 2.5.3.4 : Fixed : Issue with using TAdvMainMenu with owner different from Form
  // 2.5.3.5 : Fixed : Issue with font on large font Windows config.
  // 2.5.4.0 : New : Event OnClose added to TAdvPopupMenu
  // 2.5.4.1 : Fixed : Issue with 64bit apps on Windows 8
  // 2.5.4.2 : Fixed : Issue with MDI windows & VCL styles
  // 2.5.4.3 : Improved : Handling of menu on 64bit systems
  // 2.5.5.0 : New : Support for Delphi XE6 & C++Builder XE6 Prof, Ent. Architect added
  // 2.5.5.1 : Improved : Use of DisabledImageList when assigned to menu and not to actionmanager
  // 2.5.6.0 : New : Support for Delphi XE7 & C++Builder XE7 Prof, Ent. Architect added
  // 2.5.7.0 : New : Support for Delphi XE8 & C++Builder XE8 Prof, Ent. Architect added
  // 2.6.0.0 : New : Windows 10, Office 2016 styles added
  // 2.6.1.0 : New : Support for Delphi 10 Seattle & C++Builder 10 Seattle Prof, Ent. Architect added
  // 2.6.1.1 : Improved : Office 2016 Black color style handling

type
  TDrawStyle = (dsNormal, dsTile, dsStretch);
  TGradientDirection = (gdVertical, gdHorizontal);
  TGradientType = (gtLeft, gtRight, gtBoth); // gradient starts from left/right/both sides
  TBarPart = (bpGradient, bpPicture, bpBackPicture, bpCaption, bpBorder,
    bpSplitter);
  TBarParts = set of TBarPart;
  TSideBarPosition = (spLeft, spRight);
  // sidebar is visible in first visible menu/first level menus/all menus
  TSideBarVisibleIn = (viFirstVisible, viFirstLevel, viAnyLevel);
  TDirection = (dDownToUp, dUpToDown);
  TSeparatorStyle = (ssNormal, ssShortLine, ssCaption);
  TBackgroundPosition = (bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight,
    bpTopCenter, bpBottomCenter, bpTiled, bpStretched, bpCenter, bpXY);
  TGlyphPosition = (gpLeft, gpRight);

resourcestring
  AdvResourceRadioItem = 'ADVRADIOITEM';
  AdvResourceCheckItem = 'ADVCHECKITEM';
  AdvResourceSubMenuItem = 'ADVSUBMENUITEM';
  AdvWinKeyString = 'Win+';

const
  MENUITEM_SPACING = 2;

  ItemTextOffset = 8; // text offset for non top level items
  GlyphOffset = 6; // offset for top level item glyphs
  MinItemHeight = 16; // minimal height of the menu item
  ItemHeightIncrement = 0; // increment used to adjust item height

  TriangleSize = 17; // size of the _default_ submenu triangle

  SystemImageWidth = 8;

  TriangleColor = clBlack;

  DefaultOpacity = 255; // non-opaque
  DefaultFlat = True;
  DefaultUseSystemFont = True;

  // menu border
  DefaultMenuBorderColor = $007C7C94;

  // gradient bar
  DefaultColor = $00D8BDAF;
  DefaultColorTo = clNone;
  DefaultMenuGradientDirection = gdVertical;

  // sidebar
  DefaultBarVisible = False;
  DefaultBarVisibleIn = viFirstVisible;
  DefaultBarSide = spLeft;
  DefaultBarSize = 24;
  DefaultBarBorder = $A19D9D;

  // sidebar splitter
  DefaultBarSplitterColor = clGray;
  DefaultBarSplitterColorTo = clNone;
  DefaultBarSplitterWidth = 1;
  DefaultBarSplitterMargin = 0;
  DefaultBarSplitterGradientWidth = 0;
  DefaultBarSplitterGradientType = gtBoth;

  // normal item
  DefaultItemColor = $00FFFAFD;
  DefaultItemColorTo = clNone;
  DefaultItemGradientDirection = gdVertical;
  DefaultTextColor = clWindowText;

  // root item
  DefaultRootItemColor = $00E5D7D7;
  DefaultRootItemColorTo = $00F7F2F2;
  DefaultRootItemGradientDirection = gdHorizontal;

  //  DefaultRootItemBorder = clNone;
  DefaultRootTextColor = clWindowText;
  // -//- selected
  DefaultRootSelectedColor = $00F2E8E8;
  DefaultRootSelectedColorTo = $00CDB8B9;

  DefaultRootSelectedGradientDirection = gdVertical;
  DefaultRootSelectedBorder = $00947C7C;
  DefaultRootSelectedTextColor = clWindowText;
  // -//- hovered
  DefaultHoverColor = $00CFF6FF;
  DefaultHoverColorTo = $009DD8FF;

  DefaultHoverGradientDirection = gdVertical;
  DefaultHoverBorder = $006F4B4B;
  DefaultHoverTextColor = clWindowText;

  // selected item
  DefaultSelectedColor = $00C2EEFF; //clHighlight;
  DefaultSelectedColorTo = clNone;
  DefaultSelectedGradientDirection = gdVertical;
  DefaultSelectedBorder = $006F4B4B;
  DefaultSelectedTextColor = clWindowText;
  // -//- check & radio box
  DefaultSelectedCheckColor = $003E80FE;
  DefaultSelectedCheckColorTo = clNone;
  DefaultSelectedCheckBorder = $006F4B4B; //clNone;
  DefaultSelectedCheckGradientDirection = gdVertical;
  DefaultSelectedRadioColor = $003E80FE;
  DefaultSelectedRadioColorTo = clNone;
  DefaultSelectedRadioBorder = $006F4B4B; //clNone;
  DefaultSelectedRadioGradientDirection = gdVertical;

  // iconbar
  DefaultIconBarSize = 24; //30
  MinIconBarSize = 16;
  DefaultIconBarColor = $00FFF9F9;
  DefaultIconBarColorTo = $00BBA0A1;
  DefaultIconBarGradientDirection = gdHorizontal;
  // -//- check & radio box
  DefaultCheckColor = $006FC0FF;
  DefaultCheckColorTo = clNone;
  DefaultCheckBorder = $006F4B4B; //clNone;
  DefaultCheckGradientDirection = gdVertical;
  DefaultRadioColor = $006FC0FF;
  DefaultRadioColorTo = clNone;
  DefaultRadioBorder = $006F4B4B; //clNone;
  DefaultRadioGradientDirection = gdVertical;

  DefaultBarCaptionOffsetY = 6;
  DefaultBarCaptionAlignment = taLeftJustify;
  DefaultBarCaptionDirection = dDownToUp;
  DefaultBarCaptionShadowColor = clNone;
  DefaultBarCaptionHighlightColor = clNone;
  DefaultBarCaptionDepth = 1;

  DefaultBarBitmapPosition = bpCenter;
  DefaultBarBitmapTransparent = True;

  DefaultBarBackBitmapDrawStyle = dsNormal;
  DefaultBarBackBitmapPosition = bpTopLeft;
  DefaultBarBackBitmapTransparent = False;

  DefaultSeparatorMargin = 2;
  DefaultSeparatorWidth = 1;
  DefaultSeparatorColor = $008F6D6E;
  DefaultSeparatorColorTo = clNone;
  DefaultSeparatorGradientDirection = gdHorizontal;
  DefaultSeparatorGradientType = gtBoth;
  DefaultSeparatorGradientWidth = 0;

type
  { Forward declarations }
  TAdvPopupMenu = class;
  TAdvMainMenu = class;
  TCustomAdvMenuStyler = class;
  TGradientBar = class;
  TSideBar = class;
  TSeparator = class;
  TBackground = class;
  TBarCaption = class;
  TIconBar = class;
  TMenuGlyphs = class;
  TRootItem = class;
  TSelectedItem = class;
  IntPtr = Pointer;

  { IAdvMenu:
    This Interface layer provides common interface to properties
    of TAdvPopupMenu and TAdvMainMenu. There is no
    interface reference counting. This because the interfaced
    objects are VCL components. It is not possible to put the
    properties to common anchestor as it would be TMenu. That is
    why we use interface. }

  IAdvMenu = interface
    { Methods }
    procedure UpdateItems;
    function GetSelf: TMenu; // access to the menu object, in a "Self".
    function GetMenuStyler: TCustomAdvMenuStyler;
    function GetCurrentMenuStyler: TCustomAdvMenuStyler;
    procedure SetMenuStyler(const Value: TCustomAdvMenuStyler);
    function GetOpacity: Byte;
    procedure SetOpacity(const Value: Byte);
    function GetShowNotes: Boolean;
    procedure SetShowNotes(const Value: Boolean);
    function GetIntlKeybShortCuts: Boolean;
    procedure SetIntlKeybShortCuts(const Value: Boolean);
    { Properties }
    { Protected }
    property Self: TMenu read GetSelf;
    property CurrentMenuStyler: TCustomAdvMenuStyler read GetCurrentMenuStyler;
    { Public }
    property MenuStyler: TCustomAdvMenuStyler read GetMenuStyler write SetMenuStyler;
    property Opacity: Byte read GetOpacity write SetOpacity;
    property ShowNotes: Boolean read GetShowNotes write SetShowNotes;
    property IntlKeybShortCuts: Boolean read GetIntlKeybShortCuts write SetIntlKeybShortCuts;
  end;

  TButtonAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FHoverColor: TColor;
    FColorTo: TColor;
    FColor: TColor;
    FHoverColorTo: TColor;
    FDownColorTo: TColor;
    FDownColor: TColor;
    FDownBorderColor: TColor;
    FBorderColor: TColor;
    FHoverBorderColor: TColor;
    FGradientDirection: TGradientDirection;
    FCaptionFont: TFont;
    FGlyphPosition: TGlyphPosition;
    FUseSystemFont: Boolean;
    procedure Change;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetDownColor(const Value: TColor);
    procedure SetDownColorTo(const Value: TColor);
    procedure SetHoverColor(const Value: TColor);
    procedure SetHoverColorTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradientDirection(const Value: TGradientDirection);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetGlyphPosition(const Value: TGlyphPosition);
    procedure SetUseSystemFont(const Value: Boolean);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property GlyphPosition: TGlyphPosition read FGlyphPosition write SetGlyphPosition default gpLeft;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clNone;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property DownColor: TColor read FDownColor write SetDownColor default $00B59285;
    property DownColorTo: TColor read FDownColorTo write SetDownColorTo default clNone;
    property HoverColor: TColor read FHoverColor write SetHoverColor default $00D2BDB6;
    property HoverColorTo: TColor read FHoverColorTo write SetHoverColorTo default clNone;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection default gdVertical;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property DownBorderColor: TColor read FDownBorderColor write FDownBorderColor default $006A240A;
    property HoverBorderColor: TColor read FHoverBorderColor write FHoverBorderColor default $006A240A;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default DefaultUseSystemFont;
  end;

  { TAdvMenuStyler }
  TCustomAdvMenuStyler = class(TComponent)
  private
    FUseSystemFont: Boolean;
    FSeparator: TSeparator;
    FBackground: TBackground;
    FFont: TFont;
    FIconBar: TIconBar;
    FGlyphs: TMenuGlyphs;
    FRootItem: TRootItem;
    FSelectedItem: TSelectedItem;
    FSideBar: TSideBar;
    FMenuBorderColor: TColor;
    FMenuBarUpdating: Boolean;
    FMenu: TMenu;
    FBackgroundBitmap: TBitmap;
    FNotesFont: TFont;
    FAntiAlias: TAntiAlias;
    FButtonAppearance: TButtonAppearance;
    procedure SetBackground(const Value: TBackground);
    procedure SetFont(const Value: TFont);
    procedure SetGlyphs(const Value: TMenuGlyphs);
    procedure SetIconBar(const Value: TIconBar);
    procedure SetRootItem(const Value: TRootItem);
    procedure SetSelectedItem(const Value: TSelectedItem);
    procedure SetSeparator(const Value: TSeparator);
    procedure SetSideBar(const Value: TSideBar);
    procedure SetUseSystemFont(const Value: Boolean);
    procedure SetMenu(const Value: TMenu);
    procedure SetNotesFont(const Value: TFont);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetTButtonAppearance(const Value: TButtonAppearance);
  protected
    property MenuBarUpdating: Boolean read FMenuBarUpdating write FMenuBarUpdating default False;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Menu: TMenu read FMenu write SetMenu;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias;
    property Background: TBackground read FBackground write SetBackground;
    property IconBar: TIconBar read FIconBar write SetIconBar;
    property SelectedItem: TSelectedItem read FSelectedItem write SetSelectedItem;
    property RootItem: TRootItem read FRootItem write SetRootItem;
    property Glyphs: TMenuGlyphs read FGlyphs write SetGlyphs;
    property SideBar: TSideBar read FSideBar write SetSideBar;
    property Separator: TSeparator read FSeparator write SetSeparator;
    property Font: TFont read FFont write SetFont;
    property NotesFont: TFont read FNotesFont write SetNotesFont;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default DefaultUseSystemFont;
    property MenuBorderColor: TColor read FMenuBorderColor write FMenuBorderColor default DefaultMenuBorderColor;
    property ButtonAppearance: TButtonAppearance read FButtonAppearance write SetTButtonAppearance;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMenuStyler = class(TCustomAdvMenuStyler)
  published
    property AntiAlias;
    property Background;
    property IconBar;
    property SelectedItem;
    property RootItem;
    property Glyphs;
    property SideBar;
    property Separator;
    property Font;
    property NotesFont;
    property UseSystemFont;
    property MenuBorderColor;
    property ButtonAppearance;
  end;

  { TGradientBar }

  TGradientBar = class(TPersistent)
  private
    FGradientDirection: TGradientDirection;
    FMenu: TMenu;
    function GetGradientColor(const Index: Integer): TColor;
    procedure SetGradientColor(const Index: Integer; const Value: TColor);
    procedure SetGradientDirection(const Value: TGradientDirection);
  protected
    { for C++Builder compatibility }
    FColor: TColor;
    FColorTo: TColor;
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Color: TColor index 0 read GetGradientColor write SetGradientColor default DefaultColor;
    property ColorTo: TColor index 1 read GetGradientColor write SetGradientColor default DefaultColorTo;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection default DefaultMenuGradientDirection;
    property Menu: TMenu read FMenu;
  published
  end;

  { TIconBar }

  TIconBar = class(TGradientBar)
  private
    FRadioBorder: TColor;
    FCheckBorder: TColor;
    FRadioColor: TColor;
    FRadioColorTo: TColor;
    FCheckColor: TColor;
    FCheckColorTo: TColor;
    FCheckGradientDirection: TGradientDirection;
    FRadioGradientDirection: TGradientDirection;
    FDrawn: Boolean;
    FSize: Integer;
    FSeparatorColor: TColor;
    function GetCheckColor(const Index: Integer): TColor;
    procedure SetCheckColor(const Index: Integer; const Value: TColor);
    function GetRadioColor(const Index: Integer): TColor;
    procedure SetRadioColor(const Index: Integer; const Value: TColor);
    procedure SetSize(const Value: Integer);
  protected
    property Drawn: Boolean read FDrawn write FDrawn;
  public
    constructor Create(AMenu: TMenu);
    procedure Assign(Source: TPersistent); override;
  published
    property Color default DefaultIconBarColor;
    property ColorTo default DefaultIconBarColorTo;
    property GradientDirection default DefaultIconBarGradientDirection;
    property CheckColor: TColor index 0 read GetCheckColor write SetCheckColor default DefaultCheckColor;
    property CheckColorTo: TColor index 1 read GetCheckColor write SetCheckColor default DefaultCheckColorTo;
    property CheckBorder: TColor read FCheckBorder write FCheckBorder default DefaultCheckBorder;
    property CheckGradientDirection: TGradientDirection read FCheckGradientDirection write FCheckGradientDirection default DefaultCheckGradientDirection;
    property RadioColor: TColor index 0 read GetRadioColor write SetRadioColor default DefaultRadioColor;
    property RadioColorTo: TColor index 1 read GetRadioColor write SetRadioColor default DefaultRadioColorTo;
    property RadioBorder: TColor read FRadioBorder write FRadioBorder default DefaultRadioBorder;
    property RadioGradientDirection: TGradientDirection read FRadioGradientDirection write FRadioGradientDirection default DefaultRadioGradientDirection;
    property SeparatorColor: TColor read FSeparatorColor write FSeparatorColor default clNone;
    property Size: Integer read FSize write SetSize default DefaultIconBarSize;
  end;

  { TSelectedItem }

  TSelectedItem = class(TGradientBar)
  private
    FRadioBorder: TColor;
    FCheckBorder: TColor;
    FRadioColor: TColor;
    FRadioColorTo: TColor;
    FCheckColor: TColor;
    FCheckColorTo: TColor;
    FCheckGradientDirection: TGradientDirection;
    FRadioGradientDirection: TGradientDirection;
    FBorderColor: TColor;
    FFont: TFont;
    FUseSystemFont: Boolean;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FNotesFont: TFont;
    function GetCheckColor(const Index: Integer): TColor;
    procedure SetCheckColor(const Index: Integer; const Value: TColor);
    function GetRadioColor(const Index: Integer): TColor;
    procedure SetRadioColor(const Index: Integer; const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetNotesFont(const Value: TFont);
    procedure SetUseSystemFont(const Value: Boolean);
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color default DefaultSelectedColor;
    property ColorTo default DefaultSelectedColorTo;
    property ColorMirror: TColor read FColorMirror write FColorMirror default clNone;
    property ColorMirrorTo: TColor read FColorMirrorTo write FColorMirrorTo default clNone;
    property GradientDirection default DefaultSelectedGradientDirection;
    property BorderColor: TColor read FBorderColor write FBorderColor default DefaultSelectedBorder;
    property Font: TFont read FFont write SetFont;
    property NotesFont: TFont read FNotesFont write SetNotesFont;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default DefaultUseSystemFont;
    property CheckColor: TColor index 0 read GetCheckColor write SetCheckColor default DefaultSelectedCheckColor;
    property CheckColorTo: TColor index 1 read GetCheckColor write SetCheckColor default DefaultSelectedCheckColorTo;
    property CheckBorder: TColor read FCheckBorder write FCheckBorder default DefaultSelectedCheckBorder;
    property CheckGradientDirection: TGradientDirection read FCheckGradientDirection write FCheckGradientDirection default DefaultSelectedCheckGradientDirection;
    property RadioColor: TColor index 0 read GetRadioColor write SetRadioColor default DefaultSelectedRadioColor;
    property RadioColorTo: TColor index 1 read GetRadioColor write SetRadioColor default DefaultSelectedRadioColorTo;
    property RadioBorder: TColor read FRadioBorder write FRadioBorder default DefaultSelectedRadioBorder;
    property RadioGradientDirection: TGradientDirection read FRadioGradientDirection write FRadioGradientDirection default DefaultSelectedRadioGradientDirection;
  end;

  { TRootItem }

  TRootItem = class(TGradientBar)
  private
    // FBorderColor: TColor;
    FFont: TFont;
    FSelectedBorderColor: TColor;
    FSelectedTextColor: TColor;
    FUseSystemFont: Boolean;
    FHoverTextColor: TColor;
    FHoverBorderColor: TColor;
    FSelectedColor: TColor;
    FSelectedColorTo: TColor;
    FSelectedColorMirror: TColor;
    FSelectedColorMirrorTo: TColor;
    FHoverColor: TColor;
    FHoverColorTo: TColor;
    FHoverColorMirror: TColor;
    FHoverColorMirrorTo: TColor;
    FHoverGradientDirection: TGradientDirection;
    FSelectedGradientDirection: TGradientDirection;
    procedure SetFont(const Value: TFont);
    procedure SetUseSystemFont(const Value: Boolean);
    function GetSelectedColor(const Index: Integer): TColor;
    procedure SetSelectedColor(const Index: Integer; const Value: TColor);
    function GetHoverColor(const Index: Integer): TColor;
    procedure SetHoverColor(const Index: Integer; const Value: TColor);
  protected
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure UpdateFont;
  published
    property Color default DefaultRootItemColor;
    property ColorTo default DefaultRootItemColorTo;
    property GradientDirection default DefaultRootItemGradientDirection;
    //    property BorderColor: TColor read FBorderColor write FBorderColor default DefaultRootItemBorder;
    property Font: TFont read FFont write SetFont;
    property UseSystemFont: Boolean read FUseSystemFont write SetUseSystemFont default DefaultUseSystemFont;
    property SelectedColor: TColor index 0 read GetSelectedColor write SetSelectedColor default DefaultRootSelectedColor;
    property SelectedColorTo: TColor index 1 read GetSelectedColor write SetSelectedColor default DefaultRootSelectedColorTo;
    property SelectedColorMirror: TColor index 2 read GetSelectedColor write SetSelectedColor default clNone;
    property SelectedColorMirrorTo: TColor index 3 read GetSelectedColor write SetSelectedColor default clNone;
    property SelectedGradientDirection: TGradientDirection read FSelectedGradientDirection write FSelectedGradientDirection default DefaultRootSelectedGradientDirection;
    property SelectedBorderColor: TColor read FSelectedBorderColor write FSelectedBorderColor default DefaultRootSelectedBorder;
    property SelectedTextColor: TColor read FSelectedTextColor write FSelectedTextColor default DefaultRootSelectedTextColor;
    property HoverColor: TColor index 0 read GetHoverColor write SetHoverColor default DefaultHoverColor;
    property HoverColorTo: TColor index 1 read GetHoverColor write SetHoverColor default DefaultHoverColorTo;
    property HoverColorMirror: TColor index 2 read GetHoverColor write SetHoverColor default clNone;
    property HoverColorMirrorTo: TColor index 3 read GetHoverColor write SetHoverColor default clNone;
    property HoverGradientDirection: TGradientDirection read FHoverGradientDirection write FHoverGradientDirection default DefaultMenuGradientDirection;
    property HoverBorderColor: TColor read FHoverBorderColor write FHoverBorderColor default DefaultHoverBorder;
    property HoverTextColor: TColor read FHoverTextColor write FHoverTextColor default DefaultHoverTextColor;
  end;

  { TMenuGlyphs }

  TMenuGlyphs = class(TPersistent)
  private
    FSubMenu: TBitmap;
    FCheck: TBitmap;
    FRadio: TBitmap;
    FMenu: TMenu;
    procedure SetSubMenu(const Value: TBitmap);
    procedure SetCheck(const Value: TBitmap);
    procedure SetRadio(const Value: TBitmap);
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Menu: TMenu read FMenu;
  published
    property SubMenu: TBitmap read FSubMenu write SetSubMenu;
    property Check: TBitmap read FCheck write SetCheck;
    property Radio: TBitmap read FRadio write SetRadio;
  end;

  { TBarCaption }

  TBarCaption = class(TPersistent)
  private
    FCaption: string;
    FFont: TFont;
    //  FOffsetX,
    FOffsetY: Integer;
    FDirection: TDirection;
    FShadowColor: TColor;
    FHighlightColor: TColor;
    FDepth: Integer;
    FAlignment: TAlignment;
    FMenu: TMenu;
  protected
    procedure SetFont(Value: TFont);
    //  property OffsetX: Integer read FOffsetX write FOffsetX default 0;
    property OffsetY: Integer read FOffsetY write FOffsetY default DefaultBarCaptionOffsetY;
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property HighlightColor: TColor read FHighlightColor write FHighlightColor default DefaultBarCaptionHighlightColor;
    property Menu: TMenu read FMenu;
  published
    property Alignment: TAlignment read FAlignment write FAlignment default DefaultBarCaptionAlignment;
    property Caption: string read FCaption write FCaption;
    property Font: TFont read FFont write SetFont;
    property ShadowColor: TColor read FShadowColor write FShadowColor default DefaultBarCaptionShadowColor;
    property Depth: Integer read FDepth write FDepth default DefaultBarCaptionDepth;
    property Direction: TDirection read FDirection write FDirection default DefaultBarCaptionDirection;
  end;

  { TSideBar }

  TSideBar = class(TBarCaption)
  private
    FVisible: Boolean;
    FBackground: TBackground;
    FImage: TBackground;
    FSplitterWidth: Integer;
    FPosition: TSideBarPosition;
    FSize: Integer;
    FSplitterMargin: Integer;
    FSplitterGradientWidth: Integer;
    FSplitterGradientType: TGradientType;
    FBorderColor: TColor;
    FSplitterColor: TColor;
    FSplitterColorTo: TColor;
    FVisibleIn: TSideBarVisibleIn;
    FPopupHeight: Integer;
    FMenu: TMenu;
    function GetImage: TBackground;
    function GetBackground: TBackground;
    procedure SetImage(const Value: TBackground);
    procedure SetBackground(const Value: TBackground);
    procedure SetVisible(Value: Boolean);
    function GetVisible: Boolean;
  protected
    property PopupHeight: Integer read FPopupHeight write FPopupHeight;
    property Position: TSideBarPosition read FPosition write FPosition default DefaultBarSide;
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Menu: TMenu read FMenu;
  published
    property Visible: Boolean read GetVisible write SetVisible default DefaultBarVisible;
    property VisibleIn: TSideBarVisibleIn read FVisibleIn write FVisibleIn default DefaultBarVisibleIn;
    property Image: TBackground read GetImage write SetImage;
    property Background: TBackground read GetBackground write SetBackground;
    property Size: Integer read FSize write FSize default DefaultBarSize;
    property SplitterColor: TColor read FSplitterColor write FSplitterColor default DefaultBarSplitterColor;
    property SplitterColorTo: TColor read FSplitterColorTo write FSplitterColorTo default DefaultBarSplitterColorTo;
    property SplitterWidth: Integer read FSplitterWidth write FSplitterWidth default DefaultBarSplitterWidth;
    property SplitterMargin: Integer read FSplitterMargin write FSplitterMargin default DefaultBarSplitterMargin;
    property SplitterGradientWidth: Integer read FSplitterGradientWidth write FSplitterGradientWidth default DefaultBarSplitterGradientWidth;
    property SplitterGradientType: TGradientType read FSplitterGradientType write FSplitterGradientType default DefaultBarSplitterGradientType;
    property BorderColor: TColor read FBorderColor write FBorderColor default DefaultBarBorder;
  end;

  { TBackground }

  TBackground = class(TGradientBar)
  private
    FImage: TPicture;
    FOffsetX, FOffsetY: Integer;
    FPosition: TBackgroundPosition;
    FMarginX: Integer;
    FMarginY: Integer;
    FBitmap: TBitmap;
    FMenu: TMenu;
    function GetImage: TPicture;
    function GetBitmap: TBitmap;
  protected
    procedure SetImage(Value: TPicture);
    property Bitmap: TBitmap read GetBitmap write FBitmap;
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CalcPicturePosition(ARect: TRect; var X, Y: Integer);
    property Menu: TMenu read FMenu;
  published
    property Image: TPicture read GetImage write SetImage;
    property Position: TBackgroundPosition read FPosition write FPosition;
    property OffsetX: Integer read FOffsetX write FOffsetX default 0;
    property OffsetY: Integer read FOffsetY write FOffsetY default 0;
    property MarginX: Integer read FMarginX write FMarginX default 0;
    property MarginY: Integer read FMarginY write FMarginY default 0;
    property Color default DefaultItemColor;
    property ColorTo default DefaultItemColorTo;
    property GradientDirection default DefaultItemGradientDirection;
  end;

  { TSeparator }

  TSeparator = class(TGradientBar)
  private
    FGradientType: TGradientType;
    FGradientWidth: Integer;
    FMargin: Integer;
    FWidth: Integer;
    FMenu: TMenu;
  protected
  public
    constructor Create(AMenu: TMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Menu: TMenu read FMenu;
  published
    property Color default DefaultSeparatorColor;
    property ColorTo default DefaultSeparatorColorTo;
    property Width: Integer read FWidth write FWidth default DefaultSeparatorWidth;
    property GradientType: TGradientType read FGradientType write FGradientType;
    property GradientWidth: Integer read FGradientWidth write FGradientWidth default DefaultSeparatorGradientWidth;
    property Margin: Integer read FMargin write FMargin default DefaultSeparatorMargin;
  end;

  { TAdvMainMenu }

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvMainMenu = class(TMainMenu, IAdvMenu , IUnknown)
  private
    { Private declarations }
    FMenuStyler: TCustomAdvMenuStyler;
    FInternalMenuStyler: TCustomAdvMenuStyler;
    FCurrentMenuStyler: TCustomAdvMenuStyler;
    FOpacity: Byte;
    FShowNotes: Boolean;
    FMenuUpdating: Boolean;
    FUpdateRef: Integer;
    FOldWndHandle: THandle;
    FOldWndProcPtr: IntPtr;
    FDirectUpdate: Boolean;
    FDisabledImages: TImageList;
    FIntlKeybShortCuts: Boolean;
    function GetMenuStyler: TCustomAdvMenuStyler;
    function GetCurrentMenuStyler: TCustomAdvMenuStyler;
    procedure SetMenuStyler(const Value: TCustomAdvMenuStyler);
    function GetOpacity: Byte;
    procedure SetOpacity(const Value: Byte);
    function GetShowNotes: Boolean;
    procedure SetShowNotes(const Value: Boolean);
    function GetIntlKeybShortCuts: Boolean;
    procedure SetIntlKeybShortCuts(const Value: Boolean);
    procedure DrawMDIFormIcon(MI: TMenuItem; MR: TRect);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Reference counting disable for TComponent }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { Protected declarations }
    function GetSelf: TMenu;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AllocMenuStyler;
    procedure MenuFontChanged(Sender: TObject);
    procedure ActivateMDIChild(Sender: TObject);
    { does not work in TMainMenu }
    property Opacity: Byte read GetOpacity write SetOpacity default DefaultOpacity;
    function GetVersionNr: Integer; virtual;
    property OldWndHandle: THandle read FOldWndHandle write FOldWndHandle;
    property OldWndProcPtr: IntPtr read FOldWndProcPtr write FOldWndProcPtr;
    property CurrentMenuStyler: TCustomAdvMenuStyler read GetCurrentMenuStyler;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Merge(Menu: TMainMenu);
    procedure ExpandItemWidth(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);
    property MenuUpdating: Boolean read FMenuUpdating;
    property DirectUpdate: Boolean read FDirectUpdate write FDirectUpdate;
    procedure Refresh;
  published
    property DisabledImages: TImageList read FDisabledImages write FDisabledImages;
    property MenuStyler: TCustomAdvMenuStyler read GetMenuStyler write SetMenuStyler;
    property OwnerDraw default True;
    property IntlKeybShortCuts: Boolean read GetIntlKeybShortCuts write SetIntlKeybShortCuts default False;
    property ShowNotes: Boolean read GetShowNotes write SetShowNotes default True;
    property Version: string read GetVersion write SetVersion;
  end;

  { TAdvPopupMenu }
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPopupMenu = class(TPopupMenu, IAdvMenu , IUnknown
  )
  private
    { Private declarations }
    FMenuStyler: TCustomAdvMenuStyler;
    FInternalMenuStyler: TCustomAdvMenuStyler;
    FCurrentMenuStyler: TCustomAdvMenuStyler;
    FOpacity: Byte;
    FShowNotes: Boolean;
    FMenuUpdating: Boolean;
    FUpdateRef: Integer;
    FDirectUpdate: Boolean;
    FDisabledImages: TImageList;
    FIntlKeybShortCuts: Boolean;
    FOnClose: TNotifyEvent;
    FItemObject: TObject;
    function GetMenuStyler: TCustomAdvMenuStyler;
    function GetCurrentMenuStyler: TCustomAdvMenuStyler;
    procedure SetMenuStyler(const Value: TCustomAdvMenuStyler);
    function GetOpacity: Byte;
    procedure SetOpacity(const Value: Byte);
    function GetShowNotes: Boolean;
    procedure SetShowNotes(const Value: Boolean);
    function GetIntlKeybShortCuts: Boolean;
    procedure SetIntlKeybShortCuts(const Value: Boolean);
    procedure AllocMenuStyler;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Protected declarations }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    function GetSelf: TMenu;
    procedure MenuChanged(Sender: TObject; Source: TMenuItem; Rebuild: Boolean); override;
    property Opacity: Byte read GetOpacity write SetOpacity default DefaultOpacity;
    function GetVersionNr: Integer; virtual;
    property CurrentMenuStyler: TCustomAdvMenuStyler read GetCurrentMenuStyler;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExpandItemWidth(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState);
    procedure Popup(X, Y: Integer); overload; override;
    procedure Popup(P: TPoint); reintroduce; overload; virtual;
    procedure PopupAtCursor;
    procedure BeginUpdate;
    procedure EndUpdate;
    property MenuUpdating: Boolean read FMenuUpdating;
    property DirectUpdate: Boolean read FDirectUpdate write FDirectUpdate;
    property ItemObject: TObject read FItemObject write FItemObject;
  published
    property DisabledImages: TImageList read FDisabledImages write FDisabledImages;
    property MenuStyler: TCustomAdvMenuStyler read GetMenuStyler write SetMenuStyler;
    property OwnerDraw default True;
    property IntlKeybShortCuts: Boolean read GetIntlKeybShortCuts write SetIntlKeybShortCuts default False;
    property ShowNotes: Boolean read GetShowNotes write SetShowNotes default True;
    property Version: string read GetVersion write SetVersion;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

  {
   TPopupListEx = class(TPopupList)
   private
   procedure WndProc(var Message: TMessage); override;
   public
   PopupMenu: TPopupMenu;
   MenuLoop: Boolean;
   end;
   }

  { intended to be used from events }

procedure DrawMenuItemBackground(MenuItem: TMenuItem; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState; SideBarVisible: Boolean = False);
procedure MeasureItem(MenuItem: TMenuItem; ACanvas: TCanvas;
  var Width, Height: Integer; SideBarVisible: Boolean = False);
procedure DrawMenuItemEx(AdvMenu: IAdvMenu; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
procedure DrawMenuItemText(MenuItem: TMenuItem; ACanvas: TCanvas;
  const ACaption: string; var Rect: TRect; State: TOwnerDrawState;
  Flags: Longint);
procedure DrawMenuItem(AdvMenu: IAdvMenu; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
procedure DrawLineItem(AdvMenu: IAdvMenu; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect;
  AColor: TColor; AGradientWidth: Integer; AClip: Boolean);
procedure DrawGradient(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect;
  ColorFrom, ColorTo: TColor; Direction: TGradientDirection);
procedure DrawGradientClipped(ABitmap: TBitmap; ACanvas: TCanvas;
  ARect, AClipRect: TRect; ColorFrom, ColorTo: TColor;
  Direction: TGradientDirection);
procedure DrawFramedGradient(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect;
  ColorFrom, ColorTo: TColor; Direction: TGradientDirection;
  BorderColor: TColor; Fill: Boolean = True);
procedure DrawFramedVistaGradient(ABitmap: TBitmap; ACanvas: TCanvas;
  ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: TGradientDirection; BorderColor: TColor; Fill: Boolean = True);
procedure DrawGradientEx(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect;
  ColorFrom, ColorTo: TColor; Direction: TGradientDirection;
  GradientType: TGradientType; GradientWidth, Margin: Integer);
procedure DrawBarCaption(AdvMenu: IAdvMenu; ACanvas: TCanvas; AClipRect: TRect);
procedure DrawBarBorder(AdvMenu: IAdvMenu; ACanvas: TCanvas; ARect: TRect);
procedure DrawBarSplitter(AdvMenu: IAdvMenu; ACanvas: TCanvas; ARect: TRect);
procedure DrawImage(ACanvas: TCanvas; ARect: TRect; AImage: TBackground;
  BackGradient: Boolean = True);
procedure DrawMenuWindowBorder(MenuItem: TMenuItem; Canvas: TCanvas); overload;
procedure RefreshMenu(AdvMenu: IAdvMenu; const BarChanged,
  WidthChanged: Boolean);
procedure SetMenuOpacity(AdvMenu: IAdvMenu; const WindowHandle: THandle;
  const Opacity: Byte); overload;
procedure SetMenuOpacity(MenuItem: TMenuItem; Canvas: TCanvas); overload;
function GetParentMenuEx(MenuItem: TMenuItem): TMenu;
procedure SetMenuItemEvents(Menu: TMenu; MenuItem: TMenuItem;
  ExpandItemWidth: TMenuMeasureItemEvent;
  AdvancedDrawItem: TAdvancedMenuDrawItemEvent; Recursive: Boolean;
  AllowOwnerDrawn: Boolean = True);
function FirstVisibleItem(MenuItem: TMenuItem): TMenuItem;
function LastVisibleItem(MenuItem: TMenuItem): TMenuItem;
function VisibleMenuIndex(MenuItem: TMenuItem): Integer;
function IsInFirstVisibleMenu(MenuItem: TMenuItem): Boolean;
function IsFirstSubMenuItem(MenuItem: TMenuItem): Boolean;
function IsInTopMainMenu(MenuItem: TMenuItem): Boolean;
function IsInTopPopupMenu(MenuItem: TMenuItem): Boolean;
function IsInTopMenu(MenuItem: TMenuItem): Boolean;
function IsInFirstLevelMenu(MenuItem: TMenuItem): Boolean;
function GetMenuItemLevel(MenuItem: TMenuItem): Integer;
function IsSideBarVisible(MenuItem: TMenuItem): Boolean;
function IsAfterMenuBreak(MenuItem: TMenuItem): Boolean;
function IsRightAligned(MenuItem: TMenuItem; MenuWnd: THandle): Boolean;
function IsRightmost(MenuItem: TMenuItem): Boolean;
function GetAdvMenuIntf(AMenu: TMenu): IAdvMenu;
function GetMenuItemIntf(AMenuItem: TMenuItem): IAdvMenu;
function AdvShortCutToText(ShortCut: TShortCut; Intl: Boolean): string;
function GetMenuRect(MenuItem: TMenuItem; MenuWnd: THandle): TRect;
function GetMenuWnd(Menu: TMenu): THandle;
function GetMDIClientWnd(AdvMenu: IAdvMenu): THandle;
function FindForm(Window: THandle): TForm;
procedure DisableMenuAnimation;
procedure RestoreMenuAnimation;

implementation

uses
  ActnList, AdvMenuUtil, Messages, Dialogs, ImgList;

const
  // Extension to TShortCut to allow Win key status
  scWinKey = $0200;

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}

var
  OldMenuAnimation: Boolean;

procedure MeasureItem(MenuItem: TMenuItem; ACanvas: TCanvas;
  var Width, Height: Integer; SideBarVisible: Boolean);
const
  Alignments: array [TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  ImageList: TCustomImageList;
  TopLevel: Boolean;
  Text, s: string;
  R: TRect;
  AdvMenu: IAdvMenu;
  th, tw, ch, mw: Integer;

  procedure GetMenuSize;
  var
    NonClientMetrics: TNonClientMetrics;
  begin
    NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0)
      then
    begin
      Width := NonClientMetrics.iMenuWidth;
      Height := NonClientMetrics.iMenuHeight;
    end;
  end;

begin
  if not MenuItem.Visible then
  begin
    Height := 0;
    Width := 0;
    Exit;
  end;

  AdvMenu := GetMenuItemIntf(MenuItem);

  with MenuItem, AdvMenu.CurrentMenuStyler do
  begin
    TopLevel := MenuItem.GetParentComponent is TMainMenu;

    if TopLevel then
      GetMenuSize;

    if MenuItem.IsLine then
    begin
      Height := Separator.Width + 2;
      Width := -2;
    end
    else
    begin
      //ParentMenu := GetParentMenu;
      ImageList := GetImageList;

      if not TopLevel then
        Width := IconBar.Size;

      if Assigned(ImageList) and ((ImageIndex > -1) or not TopLevel) then
      begin
        if TopLevel then
          Width := ImageList.Width
        else
        begin
          if Height < ImageList.Height then
            Height := ImageList.Height;
        end;
      end
      //todo: item may have different height when Bitmap is assigned, is it OK?
      else if Assigned(Bitmap) and not Bitmap.Empty then
      begin
        if TopLevel then
          Width := Bitmap.Width
        else
        begin
          if Height < Bitmap.Height then
            Height := Bitmap.Height;
        end;
      end
      else if TopLevel then
        Width := -7
      else
        Dec(Height, 3); //undo changes made by TMenuItem.MeasureItem

      if not TopLevel then
      begin
        Inc(Width, 15);
        Inc(Height, MENUITEM_SPACING);
      end;

      {
       if ParentMenu is TMenu then
       Alignment := paLeft
       else if ParentMenu is TPopupMenu then
       Alignment := TPopupMenu(ParentMenu).Alignment
       else
       Alignment := paLeft;
       }

      if (pos('\n', Caption) > 0) and AdvMenu.ShowNotes then
        Text := copy(Caption, 1, pos('\n', Caption) - 1)
      else
        Text := Caption;

      if (ShortCut <> 0) then
        Text := Text + AdvShortCutToText(ShortCut, AdvMenu.IntlKeybShortCuts);

      {
       DrawStyle := Alignments[Alignment] or DT_EXPANDTABS or DT_SINGLELINE or
       DT_NOCLIP or DT_CALCRECT;
       }

      // ***fix for size calculation
      if IsInTopMenu(MenuItem) then
      begin
        if not RootItem.UseSystemFont then
          ACanvas.Font.Assign(RootItem.Font)
      end
      else
      begin
        if not UseSystemFont then
          ACanvas.Font.Assign(Font)
        else if not SelectedItem.UseSystemFont then
          ACanvas.Font.Assign(SelectedItem.Font);
      end;

      R := NilRect;
      DrawText(ACanvas.Handle, PChar(Text), Length(Text), R, DT_CALCRECT or DT_SINGLELINE);
      (*if not TopLevel and not UseSystemFont then
       begin
       if not SelectedItem.UseSystemFont then
       ACanvas.Font.Assign(SelectedItem.Font)
       else if not UseSystemFont then
       ACanvas.Font.Assign(Font);
       R2 := NilRect;
       DrawText(ACanvas.Handle, PChar(Text), Length(Text), R2, DT_CALCRECT or DT_SINGLELINE);
       UnionRect(R, R, R2);
       end;*)

      Inc(Width, R.Right - R.Left + 7);

      //      if not UseSystemFont then

      ch := RectHeight(R) + 6;

      if not TopLevel then
      begin
        // If shortcut has winkey extension, increase the width of the menuitem.
        if (MenuItem.ShortCut and scWinKey) <> 0 then
          Inc(Width, ACanvas.TextWidth(AdvWinKeyString));

        if SideBarVisible and not IsAfterMenuBreak(MenuItem) then
          Inc(Width, SideBar.Size + SideBar.SplitterWidth);
        { make space for graphical bar }

        Inc(Height, ItemHeightIncrement);
        Height := ((Height + 1) div 2) * 2; // make Height even and round to up
        Inc(Width, TriangleSize);

        s := MenuItem.Caption;

        if (pos('\n', s) > 0) and AdvMenu.ShowNotes then
        begin
          ACanvas.Font.Assign(NotesFont);
          th := ACanvas.TextHeight('gh');
          mw := 0;

          while pos('\n', s) > 0 do
          begin
            tw := ACanvas.TextWidth(copy(s, 1, pos('\n', s) - 1));
            if tw > mw then
              mw := tw;

            Inc(ch, th);
            System.delete(s, 1, pos('\n', s) + 1);
          end;

          if (s <> '') then
          begin
            tw := ACanvas.TextWidth(s);
            if tw > mw then
              mw := tw;
          end;

          if mw > 0 then
          begin
            if mw + TriangleSize + IconBar.Size + 8 > Width then
              Width := mw + TriangleSize + IconBar.Size + 8;
          end;
        end;
      end;

      Height := Max(Height, ch);
    end;
  end;
end;

procedure DrawMenuItemBackground(MenuItem: TMenuItem; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState; SideBarVisible: Boolean);
var
  ItemRect, SmallItemRect, SidebarRect, IconBarRect, MenuRect,
    SmallMenuRect: TRect;
  AdvMenu: IAdvMenu;
begin
  AdvMenu := GetMenuItemIntf(MenuItem);
  if not Assigned(AdvMenu) then
    Exit;

  { set menu opacity }
  //SetMenuOpacity(AdvMenu, MenuItem.GetParentMenu.WindowHandle, Opacity);

  with AdvMenu.CurrentMenuStyler do
  begin
    { check bar visiblity in case of menu breaks }
    SideBarVisible := SideBarVisible and not IsAfterMenuBreak(MenuItem);
    { align rect where item is drawn to leave space for the bar }
    ItemRect := ARect;
    if SideBarVisible then
    begin
      ItemRect.Left := ItemRect.Left + SideBar.Size + SideBar.SplitterWidth;
      { remove bar width }
      if AdvMenu.CurrentMenuStyler.SideBar.Position = spRight then
        OffsetRect(ItemRect, -SideBar.Size - SideBar.SplitterWidth, 0);
    end;

    if not IsInTopMainMenu(MenuItem) then
    begin
      MenuRect := GetMenuRect(MenuItem, WindowFromDC(ACanvas.Handle));
      OffsetRect(MenuRect, -MenuRect.Left, -MenuRect.Top);
      Dec(MenuRect.Right, TriangleSize);

      // TODO: prevent sidebar flickering
      // Draw sidebar
      if SideBarVisible and IsFirstSubMenuItem(MenuItem) then
      begin
        SidebarRect := MenuRect;

        //    if (PopupHeight <> RectHeight(SideBarRect)) then
        //      begin
        SideBar.PopupHeight := RectHeight(SidebarRect);
        if AdvMenu.CurrentMenuStyler.SideBar.Position = spLeft then
          SidebarRect.Right := SidebarRect.Left + SideBar.Size +
            SideBar.SplitterWidth
        else
          SidebarRect.Left := SidebarRect.Right - SideBar.Size -
            SideBar.SplitterWidth;

        DrawImage(ACanvas, SidebarRect, SideBar.Background);

        if (SideBar.Image.Color <> clNone) and Assigned(SideBar.Image.Image)
          and Assigned(SideBar.Image.Image.Graphic) and not
          (SideBar.Image.Image.Graphic.Empty) then
          DrawImage(ACanvas, SidebarRect, SideBar.Image);

        DrawBarCaption(AdvMenu, ACanvas, SidebarRect);
        DrawBarBorder(AdvMenu, ACanvas, SidebarRect);
        DrawBarSplitter(AdvMenu, ACanvas, SidebarRect);
      end;
      //    end;

      // Draw iconbar
      IconBarRect := Rect(ItemRect.Left, MenuRect.Top,
        ItemRect.Left + IconBar.Size, MenuRect.Bottom);

      DrawGradientClipped(nil, ACanvas, IconBarRect, ItemRect, IconBar.Color,
        IconBar.ColorTo, IconBar.GradientDirection);

      if IconBar.SeparatorColor <> clNone then
      begin
        ACanvas.Pen.Color := IconBar.SeparatorColor;
        ACanvas.MoveTo(IconBarRect.Right - 1, IconBarRect.Top);
        ACanvas.LineTo(IconBarRect.Right - 1, IconBarRect.Bottom);
      end;

      // Draw menu background
      SmallItemRect := ItemRect;
      SmallItemRect.Left := IconBarRect.Right;
      SmallMenuRect := Rect(SmallItemRect.Left, MenuRect.Top,
        SmallItemRect.Right, MenuRect.Bottom);
      //todo: remove MarginX & MarginY properties
      if (Background.MarginX > 0) or (Background.MarginY > 0) then
      begin
        ACanvas.Brush.Color := Background.Color;
        ACanvas.FillRect(SmallItemRect);
        InflateRect(SmallItemRect, -Background.MarginX, -Background.MarginY);
      end;
      //todo: draw gradient using DrawImage
      DrawGradientClipped(nil, ACanvas, SmallMenuRect, SmallItemRect,
        Background.Color, Background.ColorTo,
        Background.GradientDirection);

      if Assigned(Background.Image.Graphic)
        and not Background.Image.Graphic.Empty then
      begin
        if (FBackgroundBitmap.Width <> RectWidth(MenuRect)) or
          (FBackgroundBitmap.Height <> RectHeight(MenuRect)) then
        begin
          FBackgroundBitmap.Width := RectWidth(MenuRect);
          FBackgroundBitmap.Height := RectHeight(MenuRect);
          DrawImage(FBackgroundBitmap.Canvas, MenuRect, Background, False);
        end;
        ACanvas.CopyMode := cmSrcCopy;
        ACanvas.CopyRect(SmallItemRect, FBackgroundBitmap.Canvas,
          SmallItemRect);
      end;

    end;
  end;

  // Draw menu item
  DrawMenuItemEx(AdvMenu, MenuItem, ACanvas, ItemRect, State);
  DrawMenuWindowBorder(MenuItem, ACanvas);
end;

procedure DrawMenuItemText(MenuItem: TMenuItem; ACanvas: TCanvas;
  const ACaption: string; var Rect: TRect; State: TOwnerDrawState;
  Flags: Longint);
var
  Text, s, dt: string;
  ParentMenu: TMenu;
  AdvMenu: IAdvMenu;
  dr: TRect;
  Graphics: TGPGraphics;
  gfont: TGPFont;
  gfontfamily: TGPFontFamily;
  rectf, measrect: TGPRectF;
  gstringformat: TGPStringFormat;
  gsolidBrush: TGPSolidBrush;
  th: Integer;
  fclr: TColor;

begin
  AdvMenu := GetMenuItemIntf(MenuItem);
  with MenuItem do
  begin
    ParentMenu := GetParentMenu;
    if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then
    begin
      if Flags and DT_LEFT = DT_LEFT then
        Flags := Flags and (not DT_LEFT) or DT_RIGHT
      else if Flags and DT_RIGHT = DT_RIGHT then
        Flags := Flags and (not DT_RIGHT) or DT_LEFT;
      Flags := Flags or DT_RTLREADING;
    end;
    Text := ACaption;
    if (Flags and DT_CALCRECT <> 0) and
      ((Text = '') or (Text[1] = cHotkeyPrefix) and (Text[2] = #0)
      ) then
      Text := Text + ' ';

    with ACanvas do
    begin
      Brush.Style := bsClear;

      if IsInTopMenu(MenuItem) then
      begin
        if AdvMenu.CurrentMenuStyler.RootItem.UseSystemFont then
        begin
          Font.Assign(Screen.MenuFont);
        end
        else
        begin
          Font.Assign(AdvMenu.CurrentMenuStyler.RootItem.Font);
        end;

        if (odSelected in State) and
          (AdvMenu.CurrentMenuStyler.RootItem.SelectedTextColor <> clNone) then
          Font.Color := AdvMenu.CurrentMenuStyler.RootItem.SelectedTextColor
        else if (odHotLight in State) and
          (AdvMenu.CurrentMenuStyler.RootItem.HoverTextColor <> clNone) then
          Font.Color := AdvMenu.CurrentMenuStyler.RootItem.HoverTextColor;
      end
      else if (odSelected in State) then
      begin
        if AdvMenu.CurrentMenuStyler.SelectedItem.UseSystemFont then
          Font.Assign(Screen.MenuFont)
        else // set SelectedFont instead of RootItemFont
          Font.Assign(AdvMenu.CurrentMenuStyler.SelectedItem.Font)
      end
      else if AdvMenu.CurrentMenuStyler.UseSystemFont then
        Font.Assign(Screen.MenuFont)
      else
        Font.Assign(AdvMenu.CurrentMenuStyler.Font);

      if Default then
        Font.Style := Font.Style + [fsBold];
      (*
       if not Enabled then
       begin
       if not (odSelected in State) then
       begin
       OffsetRect(Rect, 1, 1);
       Font.Color := clBtnHighlight;
       DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
       OffsetRect(Rect, -1, -1);
       end;

       if (odSelected in State) and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
       Font.Color := clBtnHighlight
       else
       Font.Color := clBtnShadow;
       end;
       *)
      //mmmm
      if (pos('\n', Text) > 0) and (AdvMenu.ShowNotes) then
      begin
        if AdvMenu.CurrentMenuStyler.AntiAlias = aaNone then
        begin

          if not Enabled then
          begin
            if (odSelected in State) and (ColorToRGB(clHighlight) = ColorToRGB
                (clBtnShadow)) then
              Font.Color := clBtnHighlight
            else
              Font.Color := clBtnShadow;
          end;

          Flags := Flags AND NOT DT_VCENTER AND DT_SINGLELINE;

          s := copy(Text, 1, pos('\n', Text) - 1);
          DrawText(Handle, PChar(s), Length(s), Rect, Flags);
          s := Text;
          dr := Rect;
          dr.Top := dr.Top + ACanvas.TextHeight('gh');

          if (odSelected in State) then
            Font.Assign(AdvMenu.CurrentMenuStyler.SelectedItem.NotesFont)
          else
            Font.Assign(AdvMenu.CurrentMenuStyler.NotesFont);

          if not Enabled then
          begin
            if (odSelected in State) and (ColorToRGB(clHighlight) = ColorToRGB
                (clBtnShadow)) then
              Font.Color := clBtnHighlight
            else
              Font.Color := clBtnShadow;
          end;

          th := ACanvas.TextHeight('gh');
          repeat
            System.delete(s, 1, pos('\n', s) + 1);

            if pos('\n', s) > 0 then
              dt := copy(s, 1, pos('\n', s) - 1)
            else
              dt := s;
            DrawText(Handle, PChar(dt), Length(dt), dr, Flags);
            dr.Top := dr.Top + th;
          until (pos('\n', s) = 0);
        end
        else
        begin
          Graphics := TGPGraphics.Create(Handle);
          gfontfamily := TGPFontFamily.Create(Font.Name);
          gfont := TGPFont.Create(gfontfamily, Font.Size, 0, UnitPoint);

          case AdvMenu.CurrentMenuStyler.AntiAlias of
            aaClearType:
              Graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
            aaAntiAlias:
              Graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
          end;

          rectf.X := Rect.Left;
          rectf.Y := Rect.Top;
          rectf.Width := Rect.Right - Rect.Left;
          rectf.Height := Rect.Bottom - Rect.Top;

          gstringformat := TGPStringFormat.Create(GDIP_NOWRAP);
          gstringformat.SetHotkeyPrefix(HotkeyPrefixShow);

          fclr := Font.Color;

          if not Enabled then
          begin
            if (odSelected in State) and (ColorToRGB(clHighlight) = ColorToRGB
                (clBtnShadow)) then
              fclr := clBtnHighlight
            else
              fclr := clBtnShadow;
          end;

          gsolidBrush := TGPSolidBrush.Create(ColorToARGB(fclr));

          s := copy(Text, 1, pos('\n', Text) - 1);

          Graphics.DrawString(s, Length(s), gfont, rectf, gstringformat,
            gsolidBrush);

          gfont.Free;
          gfontfamily.Free;
          gsolidBrush.Free;

          if (odSelected in State) then
          begin
            gfontfamily := TGPFontFamily.Create
              (AdvMenu.CurrentMenuStyler.SelectedItem.NotesFont.Name);
            gfont := TGPFont.Create(gfontfamily,
              AdvMenu.CurrentMenuStyler.SelectedItem.NotesFont.Size, 0,
              UnitPoint);
            fclr := AdvMenu.CurrentMenuStyler.SelectedItem.NotesFont.Color;
          end
          else
          begin
            gfontfamily := TGPFontFamily.Create
              (AdvMenu.CurrentMenuStyler.NotesFont.Name);
            gfont := TGPFont.Create(gfontfamily,
              AdvMenu.CurrentMenuStyler.NotesFont.Size, 0, UnitPoint);
            fclr := AdvMenu.CurrentMenuStyler.NotesFont.Color;
          end;

          if not Enabled then
          begin
            if (odSelected in State) and (ColorToRGB(clHighlight) = ColorToRGB
                (clBtnShadow)) then
              fclr := clBtnHighlight
            else
              fclr := clBtnShadow;

          end;

          gsolidBrush := TGPSolidBrush.Create(ColorToARGB(fclr));

          s := Text;
          rectf.Y := rectf.Y + 20;

          Graphics.MeasureString('gh', 2, gfont, rectf, gstringformat,
            measrect);
          th := round(measrect.Height);

          repeat
            System.delete(s, 1, pos('\n', s) + 1);

            if pos('\n', s) > 0 then
              dt := copy(s, 1, pos('\n', s) - 1)
            else
              dt := s;

            Graphics.DrawString(dt, Length(dt), gfont, rectf, gstringformat,
              gsolidBrush);
            rectf.Y := rectf.Y + th;

          until (pos('\n', s) = 0);

          gfont.Free;
          gfontfamily.Free;
          gstringformat.Free;
          gsolidBrush.Free;
          Graphics.Free;
        end;
      end
      else
      begin
        if not Enabled then
        begin
          if (odSelected in State) and (ColorToRGB(clHighlight) = ColorToRGB
              (clBtnShadow)) then
            Font.Color := clBtnHighlight
          else
            Font.Color := clBtnShadow;
        end;

        if AdvMenu.CurrentMenuStyler.AntiAlias = aaNone then
        begin
          DrawText(Handle, PChar(Text), Length(Text), Rect, Flags);
        end
        else
        begin
          Graphics := TGPGraphics.Create(Handle);
          gfontfamily := TGPFontFamily.Create(Font.Name);
          gfont := TGPFont.Create(gfontfamily, Font.Size, 0, UnitPoint);

          case AdvMenu.CurrentMenuStyler.AntiAlias of
            aaClearType:
              Graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
            aaAntiAlias:
              Graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
          end;

          rectf.X := Rect.Left;
          rectf.Y := Rect.Top;
          rectf.Width := Rect.Right - Rect.Left;
          rectf.Height := Rect.Bottom - Rect.Top;

          gstringformat := TGPStringFormat.Create(GDIP_NOWRAP);
          gstringformat.SetHotkeyPrefix(HotkeyPrefixShow);
          gstringformat.SetLineAlignment(StringAlignmentCenter);

          if IsInTopMenu(MenuItem) then
            gstringformat.SetAlignment(StringAlignmentCenter)
          else if Flags and DT_RIGHT = DT_RIGHT then
          begin
            gstringformat.SetAlignment(StringAlignmentFar);
          end;

          gsolidBrush := TGPSolidBrush.Create(ColorToARGB(Font.Color));
          Graphics.DrawString(Text, Length(Text), gfont, rectf, gstringformat,
            gsolidBrush);
          gfont.Free;
          gfontfamily.Free;
          gstringformat.Free;
          gsolidBrush.Free;
          Graphics.Free;
        end;
      end;
    end;
  end;
end;

procedure DrawMenuItem(AdvMenu: IAdvMenu; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
const
  Alignments: array [TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  ImageList, DisabledImageList: TCustomImageList;
  Glyph: TBitmap;
  ParentMenu: TMenu;
  Alignment: TPopupAlignment;
  HaveImage, HaveActionImage, HaveBitmap, HaveCheck, TopLevel,
    Maximized: Boolean;
  BorderRect, GlyphRect, SaveRect, FilledRect{, ItemRect}: TRect;
  BorderRectWidth: Integer;
  DrawStyle: Longint;
  OldFontColor: TColor;
  MenuWnd, ClientHandle: THandle;
  OwnerForm: TForm;
  NrOfIcons: Integer;
  ImgList: TCustomImageList;
  {
   WindowDC: HDC;
   WindowCanvas: TCanvas;
   }

  procedure DrawCheckMark;
  begin
    Glyph := TBitmap.Create;
    try
      if MenuItem.RadioItem then
      begin
        if odSelected in State then
          with AdvMenu.CurrentMenuStyler.SelectedItem do
            DrawFramedGradient(nil, ACanvas, BorderRect, RadioColor,
              RadioColorTo, RadioGradientDirection, RadioBorder,
              not(odDisabled in State))
          else
            with AdvMenu.CurrentMenuStyler.IconBar do
              DrawFramedGradient(nil, ACanvas, BorderRect, RadioColor,
                RadioColorTo, RadioGradientDirection, RadioBorder,
                not(odDisabled in State));
        Glyph.Assign(AdvMenu.CurrentMenuStyler.Glyphs.Radio);
      end
      else
      begin
        if odSelected in State then
          with AdvMenu.CurrentMenuStyler.SelectedItem do
            DrawFramedGradient(nil, ACanvas, BorderRect, CheckColor,
              CheckColorTo, CheckGradientDirection, CheckBorder,
              not(odDisabled in State))
          else
            with AdvMenu.CurrentMenuStyler.IconBar do
              DrawFramedGradient(nil, ACanvas, BorderRect, CheckColor,
                CheckColorTo, CheckGradientDirection, CheckBorder,
                not(odDisabled in State));
        Glyph.Assign(AdvMenu.CurrentMenuStyler.Glyphs.Check);
      end;

      // restore selection rectangle
      if odSelected in State then
      begin
        ACanvas.Brush.Color :=
          AdvMenu.CurrentMenuStyler.SelectedItem.BorderColor;
        ACanvas.FrameRect(ARect);
      end;

      OldFontColor := ACanvas.Font.Color;
      //todo: what does font color do here (copied from menus.pas)
      ACanvas.Font.Color := clBtnText;
      ACanvas.Draw(GlyphRect.Left, GlyphRect.Top, Glyph);
      ACanvas.Font.Color := OldFontColor;
    finally
      FreeAndNil(Glyph);
    end;
  end;

  procedure DrawSubmenuTriangle;
  var
    R: TRect;
    DY: Integer;
  begin
    if SysLocale.MiddleEast and (AdvMenu.Self.BiDiMode = bdRightToLeft) then
      Exit;

    DY := (ARect.Top + ARect.Bottom - 5) div 2;
    // Default submenu triangle has 4x7 size
    if Alignment <> paRight then
      R := Rect(ARect.Right - 8, DY, ARect.Right - 5, DY + 6)
    else
      R := Rect(ARect.Left + 5, DY, ARect.Left + 8, DY + 6);

    with AdvMenu.CurrentMenuStyler.Glyphs do
    begin
      CenterRect(R, SubMenu.Width, SubMenu.Height);
      OldFontColor := ACanvas.Font.Color;
      ACanvas.Font.Color := TriangleColor;
      ACanvas.Draw(R.Left, R.Top, AdvMenu.CurrentMenuStyler.Glyphs.SubMenu);
      ACanvas.Font.Color := OldFontColor;
    end;
  end;

  procedure NormalDraw;
  var
    drwenabled: Boolean;
    Rl, Rs, Rr: TRect;
    WindowHandle: THandle;
  begin
    with MenuItem, ACanvas do
    begin
      drwenabled := Enabled;
      // Draw item background
      if TopLevel then
        with AdvMenu.CurrentMenuStyler.RootItem do
        begin

          if (odSelected in State) then
          begin
            if SelectedColorMirror <> clNone then
              DrawFramedVistaGradient(nil, ACanvas, ARect, SelectedColor,
                SelectedColorTo, SelectedColorMirror, SelectedColorMirrorTo,
                SelectedGradientDirection, SelectedBorderColor)
            else
              DrawFramedGradient(nil, ACanvas, ARect, SelectedColor,
                SelectedColorTo, SelectedGradientDirection,
                SelectedBorderColor);

            if GetMenuItemCount(MenuItem.Handle) > 0 then
            begin
              ACanvas.Pen.Color := AdvMenu.CurrentMenuStyler.Background.Color;
              ACanvas.MoveTo(ARect.Left, ARect.Bottom - 1);
              ACanvas.LineTo(ARect.Right - 1, ARect.Bottom - 1);
            end;
          end
          else if (odHotLight in State) then
          begin
            if HoverColorMirror <> clNone then
              DrawFramedVistaGradient(nil, ACanvas, ARect, HoverColor,
                HoverColorTo, HoverColorMirror, HoverColorMirrorTo,
                HoverGradientDirection, HoverBorderColor)
            else
              DrawFramedGradient(nil, ACanvas, ARect, HoverColor, HoverColorTo,
                HoverGradientDirection, HoverBorderColor)
          end
          else if Color <> clNone then
          begin
            // get menu bar rect into FilledRect
            MenuWnd := GetMenuWnd(AdvMenu.Self);
            if not IsWindow(MenuWnd) then
              Exit;

            GetWindowRect(MenuWnd, SaveRect);
            GetClientRect(MenuWnd, FilledRect);
            ClientToScreen(MenuWnd, FilledRect.TopLeft);
            ClientToScreen(MenuWnd, FilledRect.BottomRight);
            OffsetRect(FilledRect, -SaveRect.Left, -SaveRect.Top);
            FilledRect.Top := ARect.Top;
            FilledRect.Bottom := ARect.Bottom;

            SaveRect := ARect;

            if IsRightmost(MenuItem) then
            begin
              SaveRect.Right := FilledRect.Right;

              if AdvMenu.Self.Owner is TForm then
              begin
                OwnerForm := TForm(AdvMenu.Self.Owner);
                if Assigned(OwnerForm) and (OwnerForm.FormStyle = fsMDIForm)
                  then
                  OwnerForm := OwnerForm.ActiveMDIChild;
                if Assigned(OwnerForm) and (OwnerForm.FormStyle = fsMDIChild)
                  then
                begin
                  ClientHandle := GetParent(OwnerForm.Handle);
                  if (ClientHandle <> 0) and
                  (SendMessage(ClientHandle, WM_MDIGETACTIVE, 0, LIntParam(@Maximized)) <> 0)
                  and Maximized then
                  begin
                    NrOfIcons := 0;

                    if (biMinimize in OwnerForm.BorderIcons) then
                      Inc(NrOfIcons);

                    if (biMaximize in OwnerForm.BorderIcons) then
                      Inc(NrOfIcons);

                    if (biSystemMenu in OwnerForm.BorderIcons) then
                      Inc(NrOfIcons);

                    Dec(SaveRect.Right,
                      NrOfIcons * GetSystemMetrics(SM_CXMENUSIZE));
                  end;
                end;
              end;
            end;

            // draw gradient clipped by current item
            if IsInTopMainMenu(MenuItem) and IsRightmost(MenuItem) and Assigned
              (AdvMenu) then
            begin
              Rl := FilledRect;
              Rs := SaveRect;
              OwnerForm := nil;

              if AdvMenu.Self.Owner is TForm then
                OwnerForm := TForm(AdvMenu.Self.Owner);

              WindowHandle := WindowFromDC(ACanvas.Handle);
              if (WindowHandle <> 0) and IsWindow(WindowHandle) and
                (not Assigned(OwnerForm) or
                  (OwnerForm.Handle <> WindowHandle)) then
              begin
                Windows.GetClientRect(WindowHandle, Rr);
                Rs.Right := Rs.Right + (Rr.Right - Rr.Left) -
                  (Rl.Right - Rl.Left);
                Rl.Right := Max(Rl.Right,
                  Rl.Right + (Rr.Right - Rr.Left) - (Rl.Right - Rl.Left) + 50);
              end
              else
                Rl.Right := Rl.Right + 100;
              DrawGradientClipped(nil, ACanvas, Rl, Rs, Color, ColorTo,
                GradientDirection);
            end
            else

            begin
              FilledRect.Right := FilledRect.Right + 100;
              // draw gradient clipped by current item
              DrawGradientClipped(nil, ACanvas, FilledRect, SaveRect, Color,
                ColorTo, GradientDirection);
            end;

            (*
             FilledRect.Top := FilledRect.Bottom;
             inc(FilledRect.Bottom);
             SaveRect.Top := SaveRect.Bottom;
             inc(SaveRect.Bottom);
             WindowDC := GetWindowDC(WindowFromDC(ACanvas.Handle));
             WindowCanvas := TCanvas.Create;
             try
             WindowCanvas.Handle := WindowDC;
             if GradientDirection = gdVertical then
             begin
             WindowCanvas.Pen.Color := AdvMenu.MenuStyler.RootItem.ColorTo;
             WindowCanvas.MoveTo(FilledRect.Left, FilledRect.Top);
             WindowCanvas.LineTo(FilledRect.Right - 1, FilledRect.Top);
             end
             else { gdHorizontal }
             DrawGradientClipped(nil, WindowCanvas, FilledRect, SaveRect, Color, ColorTo, gdHorizontal);

             finally
             WindowCanvas.Free;
             end;
             *)
          end
        end
        else
          with AdvMenu.CurrentMenuStyler.SelectedItem do
            if (odSelected in State) then
            begin
              //DrawFramedGradient(nil, ACanvas, ARect, Color, ColorTo, GradientDirection, BorderColor);

              DrawFramedVistaGradient(nil, ACanvas, ARect, Color, ColorTo,
                ColorMirror, ColorMirrorTo, GradientDirection, BorderColor);
            end;

      // Set menu alignment
      if TopLevel then
        Alignment := paCenter
      else if ParentMenu is TMenu then
        Alignment := paLeft
      else if ParentMenu is TPopupMenu then
        Alignment := TPopupMenu(ParentMenu).Alignment
      else
        Alignment := paLeft;

      BorderRect := ARect;
      BorderRect.Right :=
        BorderRect.Left + AdvMenu.CurrentMenuStyler.IconBar.Size;

      BorderRect.Top := BorderRect.Top + 1;
      BorderRect.Bottom := BorderRect.Bottom - 1;

      BorderRectWidth := Min(RectWidth(BorderRect), RectHeight(BorderRect));
      CenterRect(BorderRect, BorderRectWidth, BorderRectWidth);

      // HaveImage = True if Image or checkbox (not Bitmap) is visible
      HaveImage := (Assigned(ImageList) and (ImageIndex > -1) and
          (ImageIndex < ImageList.Count));

      HaveActionImage := Assigned(MenuItem.Action) and
        (MenuItem.Action is TCustomAction) and Assigned
        ((MenuItem.Action as TCustomAction).ActionList) and Assigned
        ((MenuItem.Action as TCustomAction).ActionList.Images) and
        (ImageIndex <> -1) and (ImageIndex < (MenuItem.Action as TCustomAction)
          .ActionList.Images.Count);

      HaveBitmap := (Assigned(Bitmap) and not Bitmap.Empty) and not HaveImage;
      HaveCheck := (Checked and not TopLevel)
        and not HaveImage and not HaveBitmap;

      if HaveImage or HaveActionImage or HaveBitmap or HaveCheck then
      begin
        GlyphRect := BorderRect;

        if HaveActionImage then
        begin
{$IFDEF DELPHI_UNICODE}
          if ((MenuItem.Action as TCustomAction).ActionList is TActionManager)
            then
          begin
            if not Enabled and Assigned(((MenuItem.Action as TCustomAction).ActionList as TActionManager).DisabledImages)
              then
            begin
              drwenabled := True;
              ImgList := ((MenuItem.Action as TCustomAction).ActionList as TActionManager).DisabledImages;
            end
            else
            begin
              if not Enabled and Assigned(DisabledImageList) then
                ImgList := DisabledImageList
              else
                ImgList := ((MenuItem.Action as TCustomAction).ActionList as TActionManager).Images;
            end;
          end
          else
{$ENDIF}
          begin
            if not Enabled and Assigned(DisabledImageList) then
              ImgList := DisabledImageList
            else
              ImgList := (MenuItem.Action as TCustomAction).ActionList.Images;
          end;
        end
        else
        begin
          if not Enabled and Assigned(DisabledImageList) then
          begin
            drwenabled := True;
            ImgList := DisabledImageList;
          end
          else
            ImgList := ImageList;
        end;

        { Draw check background for checked item with image }
        {
         if Checked then
         with AdvMenu.CurrentMenuStyler.SelectedItem do
         DrawFramedGradient(nil, ACanvas, BorderRect, CheckColor, CheckColorTo,
         CheckGradientDirection, CheckBorder, not (odDisabled in State));
         }

        if Checked then
        begin
          if odSelected in State then
          begin
            with AdvMenu.CurrentMenuStyler.SelectedItem do
              DrawFramedGradient(nil, ACanvas, BorderRect, CheckColor,
                CheckColorTo, CheckGradientDirection, CheckBorder,
                not(odDisabled in State));
          end
          else
          begin
            with AdvMenu.CurrentMenuStyler.IconBar do
              DrawFramedGradient(nil, ACanvas, BorderRect, CheckColor,
                CheckColorTo, CheckGradientDirection, CheckBorder,
                not(odDisabled in State));
          end;
        end;

        if HaveImage or HaveActionImage then
        begin
          CenterRect(GlyphRect, ImgList.Width, ImgList.Height);

          if TopLevel then
            OffsetRect(GlyphRect, ARect.Left - GlyphRect.Left + GlyphOffset, 0);

          if (ImageIndex > -1) and (ImageIndex < ImgList.Count) then
          begin
            if Assigned(MenuItem.Action) and (MenuItem.Action is TCustomAction)
              and Assigned((MenuItem.Action as TCustomAction).ActionList)
              and Assigned((MenuItem.Action as TCustomAction)
                .ActionList.Images) and
              (ImageIndex < (MenuItem.Action as TCustomAction)
                .ActionList.Images.Count) then
              ImgList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex,
                drwenabled)

              //(MenuItem.Action as TCustomAction).ActionList.Images.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, Enabled)
            else
            begin
              if Assigned(DisabledImageList) and not Enabled then
                DisabledImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top,
                  ImageIndex, True)
              else
                ImageList.Draw(ACanvas, GlyphRect.Left, GlyphRect.Top,
                  ImageIndex, Enabled)
            end
          end;

        end
        else
        begin
          { Need to add BitmapWidth/Height properties for TMenuItem if we're to
            support them. Right now let's hardcode them to 16x16. }

          if TopLevel then
            OffsetRect(GlyphRect, ARect.Left - GlyphRect.Left + GlyphOffset, 0);

          if HaveCheck then
          begin
            if Assigned(AdvMenu.CurrentMenuStyler) then
            begin
              if MenuItem.RadioItem then
                CenterRect(GlyphRect,
                  AdvMenu.CurrentMenuStyler.Glyphs.Radio.Width,
                  AdvMenu.CurrentMenuStyler.Glyphs.Radio.Height)
              else
                CenterRect(GlyphRect,
                  AdvMenu.CurrentMenuStyler.Glyphs.Check.Width,
                  AdvMenu.CurrentMenuStyler.Glyphs.Check.Height)
            end;
            DrawCheckMark;
          end
          else
          begin
            CenterRect(GlyphRect, Bitmap.Width, Bitmap.Height);
            Bitmap.Transparent := True;
            Bitmap.TransparentMode := tmAuto;
            Draw(GlyphRect.Left, GlyphRect.Top, Bitmap);
            //StretchDraw(GlyphRect, Bitmap);
          end;
        end;
      end;

      if not TopLevel and (MenuItem.Count > 0) then
        DrawSubmenuTriangle;

      if not TopLevel then
        Inc(ARect.Left, AdvMenu.CurrentMenuStyler.IconBar.Size + ItemTextOffset)
      else if HaveImage or HaveBitmap then
        Inc(ARect.Left, RectWidth(GlyphRect));

      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or Alignments[Alignment];

      //if odNoAccel in State then
      //  DrawStyle := DrawStyle or DT_HIDEPREFIX;

      if odDefault in State then
        Font.Style := [fsBold];

      { Calculate vertical layout }
      SaveRect := ARect;
      DrawMenuItemText(MenuItem, ACanvas, Caption, ARect, State,
        DrawStyle or DT_CALCRECT or DT_NOCLIP);
      OffsetRect(ARect, 0, (RectHeight(SaveRect) - RectHeight(ARect)) div 2);

      if TopLevel then
      begin
        ARect.Left := SaveRect.Left;
        ARect.Right := SaveRect.Right;
      end;

      DrawMenuItemText(MenuItem, ACanvas, Caption, ARect, State, DrawStyle);

      if (ShortCut <> 0) and not TopLevel then
      begin
        ARect.Left := ARect.Left;
        ARect.Right := SaveRect.Right - 10;
        DrawMenuItemText(MenuItem, ACanvas,
          AdvShortCutToText(ShortCut, AdvMenu.IntlKeybShortCuts), ARect, State, DT_RIGHT);
      end;
    end;
  end;

begin
  ParentMenu := MenuItem.GetParentMenu;
  ImageList := MenuItem.GetImageList;

  if MenuItem.GetParentMenu is TAdvMainMenu then
    DisabledImageList := TAdvMainMenu(MenuItem.GetParentMenu).DisabledImages
  else if MenuItem.GetParentMenu is TAdvPopupMenu then
    DisabledImageList := TAdvPopupMenu(MenuItem.GetParentMenu).DisabledImages
  else
    DisabledImageList := nil;

  TopLevel := IsInTopMainMenu(MenuItem);
  if (ParentMenu <> nil) then
    NormalDraw;
end;

procedure DrawMenuItemEx(AdvMenu: IAdvMenu; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
begin
  if MenuItem.IsLine then
    DrawLineItem(AdvMenu, MenuItem, ACanvas, ARect, State)
  else
    DrawMenuItem(AdvMenu, MenuItem, ACanvas, ARect, State);
end;

procedure DrawLineItem(AdvMenu: IAdvMenu; MenuItem: TMenuItem;
  ACanvas: TCanvas; ARect: TRect; State: TOwnerDrawState);
var
  Middle: Integer;
begin
  with AdvMenu.CurrentMenuStyler, Separator do
  begin
    Inc(ARect.Left, IconBar.Size);
    Middle := ARect.Top + RectHeight(ARect) div 2;

    DrawGradientEx(nil, ACanvas, Rect(ARect.Left, Middle - Width div 2,
        ARect.Right, Middle + (Width + 1) div 2), Color, ColorTo,
      GradientDirection, GradientType, GradientWidth, Margin);
  end;
end;

procedure DrawFadeLine(ACanvas: TCanvas; AClipRect, ALineRect: TRect;
  AColor: TColor; AGradientWidth: Integer; AClip: Boolean);
var
  I, AToDiv2, ATo, AFrom, ATop, R1, G1, B1, R2, G2, B2: Integer;
  C: TColor;
begin
  AToDiv2 := ALineRect.Left + RectWidth(ALineRect) div 2;
  ATop := Max(ALineRect.Top, AClipRect.Top);
  if AClip then
  begin
    ATo := Min(AToDiv2, AClipRect.Left) - 1;
    AFrom := Max(AToDiv2, AClipRect.Right);
  end
  else
  begin
    ATo := AToDiv2;
    AFrom := AToDiv2;
  end;

  ColorToRGBVal(AColor, R1, G1, B1);

  for I := ALineRect.Left to ATo do
  begin
    if I < (ALineRect.Left + AGradientWidth) then
    begin
      C := ACanvas.Pixels[I, ATop];
      ColorToRGBVal(C, R2, G2, B2);
      R2 := R2 + (((R1 - R2) * (I - ALineRect.Left)) div AGradientWidth);
      G2 := G2 + (((G1 - G2) * (I - ALineRect.Left)) div AGradientWidth);
      B2 := B2 + (((B1 - B2) * (I - ALineRect.Left)) div AGradientWidth);
      C := AdvMenuUtil.RGB(R2, G2, B2, 0);
      ACanvas.Pixels[I, ATop] := C;
    end
    else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
  //  if (AClipRect.Right > AToDiv2) then
  for I := AFrom to ALineRect.Right do
  begin
    if I > (ALineRect.Right - AGradientWidth) then
    begin
      C := ACanvas.Pixels[I, ATop];
      ColorToRGBVal(C, R2, G2, B2);
      R2 := R2 + (((R1 - R2) * (ALineRect.Right - I)) div AGradientWidth);
      G2 := G2 + (((G1 - G2) * (ALineRect.Right - I)) div AGradientWidth);
      B2 := B2 + (((B1 - B2) * (ALineRect.Right - I)) div AGradientWidth);
      C := AdvMenuUtil.RGB(R2, G2, B2, 0);
      ACanvas.Pixels[I, ATop] := C;
    end
    else
      ACanvas.Pixels[I, ATop] := AColor;
  end;
end;

procedure DrawBarCaption(AdvMenu: IAdvMenu; ACanvas: TCanvas; AClipRect: TRect);
var
  OldFontHandle, NewFontHandle: HFONT;
  ADepth, X, Y: Integer;
  TextSize: TSize;
  R: TRect;
  Angle: Integer;
  C: TColor;
begin
  with AdvMenu, ACanvas do
  begin
    { vertical text to gradient bar }
    Font.Assign(CurrentMenuStyler.SideBar.Font);

    if CurrentMenuStyler.SideBar.Direction = dUpToDown then
      Angle := -90
    else
      Angle := 90;

    { Resource leak fix }
    NewFontHandle := CreateRotatedFont(Font, Angle);
    OldFontHandle := SelectObject(Handle, NewFontHandle);

    TextSize := TextExtent(CurrentMenuStyler.SideBar.Caption);
    X := round((CurrentMenuStyler.SideBar.Size - TextSize.cy) / 2 - 0.5);

    SetBkMode(Handle, Transparent);

    R := AClipRect;
    Inc(R.Bottom, 1);

    Y := 0;

    with CurrentMenuStyler.SideBar do
    begin
      if Alignment = taCenter then
        Y := round((RectHeight(AClipRect) + TextSize.cx) / 2 - 0.5)
      else if ((Alignment = taRightJustify) and (Direction = dDownToUp)) or
        ((Alignment = taLeftJustify) and (Direction = dUpToDown)) then
        Y := OffsetY + TextSize.cx
      else if ((Alignment = taLeftJustify) and (Direction = dDownToUp)) or
        ((Alignment = taRightJustify) and (Direction = dUpToDown)) then
      begin
        Y := RectHeight(AClipRect) - OffsetY;
        if Assigned(Image.Image) and (Image.Position in [bpBottomLeft,
          bpBottomRight]) then
          Y := Y - Image.Image.Height;
      end;

      if Direction = dUpToDown then
      begin
        Dec(Y, TextSize.cx);
        Inc(X, TextSize.cy + 1);
      end;

      ADepth := Depth;
      { highlight }
      C := ColorToRGB(HighlightColor);
      if (C <> clNone) then
      begin
        SetTextColor(Handle, Cardinal(C));
        ExtTextOut(Handle, X - ADepth, Y + ADepth, ETO_CLIPPED, @R,
          PChar(Caption), Length(Caption), nil);
      end;

      { shadow }
      C := ColorToRGB(ShadowColor);
      if (C <> clNone) then
      begin
        SetTextColor(Handle, Cardinal(C));
        ExtTextOut(Handle, X + ADepth, Y - ADepth, ETO_CLIPPED, @R,
          PChar(Caption), Length(Caption), nil);
      end;

      { text itself }
      SetTextColor(Handle, Font.Color);
      ExtTextOut(Handle, X, Y, ETO_CLIPPED, @R, PChar(Caption),
        Length(Caption), nil);
      { restore old font and delete new font }
      SelectObject(Handle, OldFontHandle);
      DeleteObject(NewFontHandle);
      SetBkMode(Handle, OPAQUE);
    end;
  end;
end;

{ Draws gradient bar to the bitmap or canvas with given parameters. Does not support
  negative rects. Switch ColorFrom/ColorTo to achieve same effect. ARect
  should be inside valid ABitmap. Set ABitmap to nil if you want to use ACanvas.
  Using TBitmap is faster but not always possible. }

procedure DrawGradient(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect;
  ColorFrom, ColorTo: TColor; Direction: TGradientDirection);
var
  UseBitmap: Boolean;
  DrawCanvas: TCanvas;
  OldBrushHandle, NewBrushHandle: HBRUSH;

  GradientWidth, GradientHeight: Integer;
  DrawFrom, DrawTo, I, X, Y: Integer;
  ColorValue: TColor;
  ColorFromQuad, ColorToQuad: TLargeColorQuad;
  P, PF: P32bitQuadScanLine;
  procedure FillScanLine(AColor: TColor);
  var
    X: Integer;
  begin
    if UseBitmap then
    begin
      for X := ARect.Left to ARect.Right - 1 do
      begin
{$R-}
        P^[X].Red := TColorQuad(AColor).Red;
        P^[X].Green := TColorQuad(AColor).Green;
        P^[X].Blue := TColorQuad(AColor).Blue;
        P^[X].Alpha := $00;
      end;
    end
    else
    begin
      NewBrushHandle := CreateSolidBrush(AColor);
      OldBrushHandle := SelectObject(DrawCanvas.Handle, NewBrushHandle);
      try
        PatBlt(DrawCanvas.Handle, ARect.Left, Y, GradientWidth, 1, PATCOPY);
      finally
        SelectObject(DrawCanvas.Handle, OldBrushHandle);
        DeleteObject(NewBrushHandle);
      end;
    end;
  end;

begin
  if ColorTo = clNone then
    ColorTo := ColorFrom;

  UseBitmap := Assigned(ABitmap);
  if UseBitmap then
  begin
    DrawCanvas := ABitmap.Canvas;
    ABitmap.PixelFormat := pf32bit;
  end
  else
    DrawCanvas := ACanvas;

  with DrawCanvas do
  begin
    GradientWidth := RectWidth(ARect);
    GradientHeight := RectHeight(ARect);
    DrawFrom := ARect.Top;
    DrawTo := ARect.Bottom - 1;

    if (ColorFrom = ColorTo) or (ColorTo = clNone) then
    { same color, just one drawing phase required }
    begin
      if UseBitmap then
      begin
        for I := DrawFrom to DrawTo do
        begin
          P := ABitmap.ScanLine[I];
          FillScanLine(ColorFrom);
        end;
      end
      else
      begin
        DrawCanvas.Brush.Color := ColorFrom;
        FillRect(ARect);
      end;
    end
    else
    begin

      ColorToRGBVal(ColorFrom, ColorFromQuad.Red, ColorFromQuad.Green,
        ColorFromQuad.Blue);
      ColorToRGBVal(ColorTo, ColorToQuad.Red, ColorToQuad.Green,
        ColorToQuad.Blue);

      ColorToQuad.Red := ColorToQuad.Red - ColorFromQuad.Red;
      ColorToQuad.Green := ColorToQuad.Green - ColorFromQuad.Green;
      ColorToQuad.Blue := ColorToQuad.Blue - ColorFromQuad.Blue;

      if GradientHeight > 0 then
      begin
        case Direction of
          gdVertical:
            begin
              for Y := DrawFrom to DrawTo do
              begin
                I := Y - DrawFrom;
                ColorValue := AdvMenuUtil.RGB
                  ((ColorFromQuad.Red + ((ColorToQuad.Red * I)
                        div GradientHeight)),
                  (ColorFromQuad.Green + ((ColorToQuad.Green * I)
                        div GradientHeight)),
                  (ColorFromQuad.Blue + ((ColorToQuad.Blue * I)
                        div GradientHeight)));

                if UseBitmap then
                begin
                  P := ABitmap.ScanLine[Y];
                  try
                    FillScanLine(ColorValue);
                  except
                    Exit;
                  end;
                end
                else
                begin
                  DrawCanvas.Pen.Color := ColorValue;
                  DrawCanvas.Pen.Width := 1;
                  DrawCanvas.MoveTo(ARect.Left, Y);
                  DrawCanvas.LineTo(ARect.Left + GradientWidth, Y);
                end;

              end;
            end;
          gdHorizontal:
            begin
              PF := nil;
              if UseBitmap then
              begin
                PF := ABitmap.ScanLine[DrawFrom];
                if Assigned(PF) then
                  for X := ARect.Left to ARect.Right - 1 do
                  begin
                    I := X - ARect.Left;
                    ColorValue := AdvMenuUtil.RGB
                      ((ColorFromQuad.Red + ((ColorToQuad.Red * I)
                            div GradientWidth)),
                      (ColorFromQuad.Green + ((ColorToQuad.Green * I)
                            div GradientWidth)),
                      (ColorFromQuad.Blue + ((ColorToQuad.Blue * I)
                            div GradientWidth)));
                    try
                      PF^[X].Red := TColorQuad(ColorValue).Red;
                      PF^[X].Green := TColorQuad(ColorValue).Green;
                      PF^[X].Blue := TColorQuad(ColorValue).Blue;
                      PF^[X].Alpha := $00;
                    except
                      Exit;
                    end;
                  end;
              end
              else
                for X := ARect.Left to ARect.Right - 1 do
                begin
                  I := X - ARect.Left;
                  Pen.Color := AdvMenuUtil.RGB
                    ((ColorFromQuad.Red + ((ColorToQuad.Red * I)
                          div GradientWidth)),
                    (ColorFromQuad.Green + ((ColorToQuad.Green * I)
                          div GradientWidth)),
                    (ColorFromQuad.Blue + ((ColorToQuad.Blue * I)
                          div GradientWidth)));
                  MoveTo(X, DrawFrom);
                  LineTo(X, DrawTo + 1);
                end;

              // copy the first line till end
              if UseBitmap then
              begin
                for Y := DrawFrom + 1 to DrawTo do
                begin
                  for X := ARect.Left to ARect.Right - 1 do
                  begin
                    P := ABitmap.ScanLine[Y];
                    P^[X].Red := PF^[X].Red;
                    P^[X].Green := PF^[X].Green;
                    P^[X].Blue := PF^[X].Blue;
                    P^[X].Alpha := PF^[X].Alpha;
                   end;
                end;
              end;
            end;
        end;
      end;
    end;
  end;
end;

procedure DrawGradientClipped(ABitmap: TBitmap; ACanvas: TCanvas;
  ARect, AClipRect: TRect; ColorFrom, ColorTo: TColor;
  Direction: TGradientDirection);
var
  R1, G1, B1, R2, G2, B2, GradientWidth: Integer;
  R, g, b: Integer;
begin
  Windows.IntersectRect(AClipRect, ARect, AClipRect);
  if (RectWidth(AClipRect) = 0) or (RectHeight(AClipRect) = 0) then
    Exit;

  if ColorTo = clNone then
    ColorTo := ColorFrom;

  ColorToRGBVal(ColorFrom, R1, G1, B1);
  ColorToRGBVal(ColorTo, R2, G2, B2);

  if Direction = gdHorizontal then
  begin
    GradientWidth := RectWidth(ARect);
    R := R1 + ((R2 - R1) * (AClipRect.Left - ARect.Left)) div GradientWidth;
    g := G1 + ((G2 - G1) * (AClipRect.Left - ARect.Left)) div GradientWidth;
    b := B1 + ((B2 - B1) * (AClipRect.Left - ARect.Left)) div GradientWidth;
    ColorFrom := RGB(R, g, b);
    R := R1 + ((R2 - R1) * (AClipRect.Right - ARect.Left)) div GradientWidth;
    g := G1 + ((G2 - G1) * (AClipRect.Right - ARect.Left)) div GradientWidth;
    b := B1 + ((B2 - B1) * (AClipRect.Right - ARect.Left)) div GradientWidth;
    ColorTo := RGB(R, g, b);
  end
  else {if Direction = gdVertical then}
  begin
    GradientWidth := RectHeight(ARect);
    R := R1 + ((R2 - R1) * (AClipRect.Top - ARect.Top)) div GradientWidth;
    g := G1 + ((G2 - G1) * (AClipRect.Top - ARect.Top)) div GradientWidth;
    b := B1 + ((B2 - B1) * (AClipRect.Top - ARect.Top)) div GradientWidth;
    ColorFrom := RGB(R, g, b);
    R := R1 + ((R2 - R1) * (AClipRect.Bottom - ARect.Top)) div GradientWidth;
    g := G1 + ((G2 - G1) * (AClipRect.Bottom - ARect.Top)) div GradientWidth;
    b := B1 + ((B2 - B1) * (AClipRect.Bottom - ARect.Top)) div GradientWidth;
    ColorTo := RGB(R, g, b);
  end;
  DrawGradient(ABitmap, ACanvas, AClipRect, ColorFrom, ColorTo, Direction);
end;

// Draw gradient in the specified rectangle (if Fill = True and ColorFrom <> clNone),
// frame it with BorderColor color.
procedure DrawFramedGradient(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect;
  ColorFrom, ColorTo: TColor; Direction: TGradientDirection;
  BorderColor: TColor; Fill: Boolean = True);
begin
  if Fill and (ColorFrom <> clNone) then
    DrawGradient(ABitmap, ACanvas, ARect, ColorFrom, ColorTo, Direction);
  if BorderColor <> clNone then
  begin
    ACanvas.Brush.Color := BorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;

// Draw gradient in the specified rectangle (if Fill = True and ColorFrom <> clNone),
// frame it with BorderColor color.
procedure DrawFramedVistaGradient(ABitmap: TBitmap; ACanvas: TCanvas;
  ARect: TRect; ColorFrom, ColorTo, ColorMirrorFrom, ColorMirrorTo: TColor;
  Direction: TGradientDirection; BorderColor: TColor; Fill: Boolean = True);
var
  R: TRect;

begin
  if Fill and (ColorFrom <> clNone) then
  begin
    if ColorMirrorFrom <> clNone then
    begin
      R := ARect;
      R.Bottom := R.Top + ((R.Bottom - R.Top) div 2);
      DrawGradient(ABitmap, ACanvas, R, ColorFrom, ColorTo, Direction);
      R := ARect;
      R.Top := R.Top + ((R.Bottom - R.Top) div 2);
      DrawGradient(ABitmap, ACanvas, R, ColorMirrorFrom, ColorMirrorTo,
        Direction);
    end
    else
      DrawGradient(ABitmap, ACanvas, ARect, ColorFrom, ColorTo, Direction);
  end;

  if BorderColor <> clNone then
  begin
    ACanvas.Brush.Color := BorderColor;
    ACanvas.FrameRect(ARect);
  end;
end;

procedure DrawGradientEx(ABitmap: TBitmap; ACanvas: TCanvas; ARect: TRect;
  ColorFrom, ColorTo: TColor; Direction: TGradientDirection;
  GradientType: TGradientType; GradientWidth, Margin: Integer);
begin
  if ColorTo = clNone then
    ColorTo := ColorFrom;
  with ACanvas, ARect do
    if Direction = gdVertical then
    begin
      Inc(Top, Margin);
      Dec(Bottom, Margin);

      if GradientType = gtBoth then
        GradientWidth := GradientWidth * 2;

      if GradientWidth >= RectHeight(ARect) then
        GradientWidth := RectHeight(ARect)
      else
      begin
        Brush.Color := ColorTo;
        case GradientType of
          gtLeft:
            FillRect(Rect(Left, Top, Right, Bottom - GradientWidth));
          gtRight:
            FillRect(Rect(Left, Top + GradientWidth, Right, Bottom));
          gtBoth:
            begin
              Brush.Color := ColorFrom;
              FillRect(Rect(Left, Top + GradientWidth div 2, Right,
                  Bottom - GradientWidth div 2));
            end;
        end;
      end;

      if GradientWidth > 0 then
        case GradientType of
          gtLeft:
            DrawGradient(ABitmap, ACanvas, Rect(Left, Bottom - GradientWidth,
                Right, Bottom), ColorTo, ColorFrom, Direction);
          gtRight:
            DrawGradient(ABitmap, ACanvas, Rect(Left, Top, Right,
                Top + GradientWidth), ColorFrom, ColorTo, Direction);
          gtBoth:
            begin
              DrawGradient(ABitmap, ACanvas, Rect(Left, Top, Right,
                  Top + GradientWidth div 2), ColorTo, ColorFrom, Direction);
              DrawGradient(ABitmap, ACanvas,
                Rect(Left, Bottom - GradientWidth div 2, Right,
                  Bottom), ColorFrom, ColorTo, Direction);
            end;
        end;
    end
    else
    begin
      Inc(Left, Margin);
      Dec(Right, Margin);

      if GradientWidth >= RectWidth(ARect) then
        GradientWidth := RectWidth(ARect)
      else
      begin
        Brush.Color := ColorTo;
        case GradientType of
          gtLeft:
            FillRect(Rect(Left + GradientWidth, Top, Right, Bottom));
          gtRight:
            FillRect(Rect(Left, Top, Right - GradientWidth, Bottom));
          gtBoth:
            begin
              Brush.Color := ColorFrom;
              FillRect(Rect(Left + GradientWidth div 2, Top,
                  Right - GradientWidth div 2, Bottom));
            end;
        end;
      end;

      if GradientWidth > 0 then
        case GradientType of
          gtLeft:
            DrawGradient(ABitmap, ACanvas,
              Rect(Left, Top, Left + GradientWidth, Bottom), ColorFrom,
              ColorTo, Direction);
          gtRight:
            DrawGradient(ABitmap, ACanvas, Rect(Right - GradientWidth, Top,
                Right, Bottom), ColorTo, ColorFrom, Direction);
          gtBoth:
            begin
              DrawGradient(ABitmap, ACanvas,
                Rect(Left, Top, Left + GradientWidth div 2, Bottom), ColorTo,
                ColorFrom, Direction);
              DrawGradient(ABitmap, ACanvas, Rect(Right - GradientWidth div 2,
                  Top, Right, Bottom), ColorFrom, ColorTo, Direction);
            end;
        end;
    end;
end;

procedure DrawBarBorder(AdvMenu: IAdvMenu; ACanvas: TCanvas; ARect: TRect);
begin
  with AdvMenu.CurrentMenuStyler.SideBar, ACanvas, ARect do
    if FBorderColor <> clNone then
    begin
      case Position of
        spLeft:
          Dec(Right, SplitterWidth);
        spRight:
          Inc(Left, SplitterWidth);
      end;
      Brush.Color := FBorderColor;
      FrameRect(ARect);
    end;
end;

procedure DrawBarSplitter(AdvMenu: IAdvMenu; ACanvas: TCanvas; ARect: TRect);
var
  GradientRect: TRect;
begin
  with AdvMenu.CurrentMenuStyler.SideBar, AdvMenu do
    if SplitterColor <> clNone then
      case Position of
        spLeft, spRight:
          begin
            GradientRect := Rect(ARect.Left, ARect.Top,
              ARect.Left + SplitterWidth, ARect.Bottom);
            if Position = spLeft then
              OffsetRect(GradientRect, RectWidth(ARect) - SplitterWidth, 0);

            DrawGradientEx(nil, ACanvas, GradientRect, SplitterColor,
              SplitterColorTo, gdVertical, SplitterGradientType,
              SplitterGradientWidth, SplitterMargin);
          end;
      end;
end;

{ Draw image and background gradient. If ABitmap is not assigned,
  uses ACanvas for drawing }
procedure DrawImage(ACanvas: TCanvas; ARect: TRect; AImage: TBackground;
  BackGradient: Boolean = True);
var
  X, Y: Integer;
  HaveImage: Boolean;
begin
  with AImage do
  begin
    InflateRect(ARect, -MarginX, -MarginY);
    Bitmap.Height := RectHeight(ARect);
    Bitmap.Width := RectWidth(ARect);

    HaveImage := Assigned(Image) and Assigned(Image.Graphic)
      and not Image.Graphic.Empty;

    // roughly determine if gradient is completely covered by image
    if BackGradient and (not HaveImage or (not(Position in [bpTiled,
          bpStretched]) or Image.Bitmap.Transparent)) then
      DrawGradient(nil, Bitmap.Canvas, BitmapRect(Bitmap), Color, ColorTo,
        GradientDirection);

    if HaveImage then
    begin
      case Position of
        bpXY:
          Bitmap.Canvas.Draw(FOffsetX, FOffsetY, Image.Graphic);
        bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight, bpTopCenter,
          bpBottomCenter, bpCenter:
          begin
            CalcPicturePosition(BitmapRect(Bitmap), X, Y);
            Bitmap.Canvas.Draw(X, Y, Image.Graphic);
          end;
        bpTiled:
          if (Image.Width > 0) and (Image.Height > 0) then
          begin
            Y := 0;
            while Y < BitmapRect(Bitmap).Bottom do
            begin
              X := 0;
              while X < BitmapRect(Bitmap).Right do
              begin
                Bitmap.Canvas.Draw(X, Y, Image.Graphic);
                Inc(X, Image.Width);
              end;
              Inc(Y, Image.Height);
            end;
          end;
        bpStretched:
          Bitmap.Canvas.StretchDraw(BitmapRect(Bitmap), Image.Graphic);
      end;
    end;
    ACanvas.Draw(ARect.Left, ARect.Top, Bitmap);
  end;
end;

function GetParentMenuEx(MenuItem: TMenuItem): TMenu;
begin
  Result := nil;
  if not Assigned(MenuItem) then
    Exit;
  if not Assigned(MenuItem.Parent) then
    Result := TMenu(MenuItem.Owner)
  else
    Result := MenuItem.GetParentMenu;
end;

procedure SetMenuItemEvents(Menu: TMenu; MenuItem: TMenuItem;
  ExpandItemWidth: TMenuMeasureItemEvent;
  AdvancedDrawItem: TAdvancedMenuDrawItemEvent; Recursive: Boolean;
  AllowOwnerDrawn: Boolean);

  procedure SetBarEvents(AMenuItem: TMenuItem);
  begin
    if not(csLoading in Menu.ComponentState) then
      with AMenuItem do
      begin
        if (Addr(OnAdvancedDrawItem) <> Addr(AdvancedDrawItem)) then
        begin
          if not Assigned(OnMeasureItem) or not AllowOwnerDrawn then
            OnMeasureItem := ExpandItemWidth;
          if (not Assigned(OnAdvancedDrawItem) and not Assigned(OnDrawItem))
            or not AllowOwnerDrawn then
            OnAdvancedDrawItem := AdvancedDrawItem;
        end;
      end;
  end;

  procedure SetBarEventsForItems(AMenuItem: TMenuItem; ARecursive: Boolean);
  var
    I: Integer;
  begin
    for I := 0 to AMenuItem.Count - 1 do
    begin
      SetBarEvents(AMenuItem.Items[I]);
      if ARecursive and (AMenuItem.Items[I].Count > 0) then
        SetBarEventsForItems(AMenuItem.Items[I], ARecursive);
    end;
  end;

begin
  if Assigned(MenuItem) then
    SetBarEvents(MenuItem)
  else
    SetBarEventsForItems(Menu.Items, Recursive);
end;

procedure SetMenuOpacity(MenuItem: TMenuItem; Canvas: TCanvas);
var
  AdvMenu: IAdvMenu;
begin
  AdvMenu := GetMenuItemIntf(MenuItem);
  SetMenuOpacity(AdvMenu, WindowFromDC(Canvas.Handle), AdvMenu.Opacity);
end;

procedure SetMenuOpacity(AdvMenu: IAdvMenu; const WindowHandle: THandle;
  const Opacity: Byte);
var
  IsLayered: Boolean;
  Flags: Longint;
begin
  if WindowHandle = 0 then
    Exit;
  if SupportsLayeredWindows then // has layered support
      with AdvMenu do
      begin
        { TMainMenu is problematic because the handles are system wide, so
          setting layered, will affect other applications too!! Solution ->
          todo own menutoolbar which uses popupmenus. For now, MainMenu does
          not have opacity. }
        Flags := GetWindowLong(WindowHandle, GWL_EXSTYLE);
        IsLayered := ((Flags and WS_EX_LAYERED) <> 0);
        if (Self is TPopupMenu) then
        begin
          if (not IsLayered) and (Opacity < 255) then
          begin
            SetWindowLong(WindowHandle, GWL_EXSTYLE, Flags or WS_EX_LAYERED);
            SetLayeredWindowAttributes(WindowHandle, 0, Opacity, LWA_ALPHA);
          end;
        end
{$IFOPT D+}
        { Debugging code which disables the layered flags in TMainMenu }
        else if (Self is TMainMenu) then
        begin
          if IsLayered then
          begin
            SetLayeredWindowAttributes(WindowHandle, 0, 255, LWA_ALPHA);
            SetWindowLong(WindowHandle, GWL_EXSTYLE,
              Flags and not WS_EX_LAYERED);
          end;
        end;
{$ENDIF}
      end;
end;

function MenuWindowProc(hWnd: hWnd; uMsg: Integer; WParam: WParam;
  lParam: lParam): LRESULT; stdcall;
var
  OldWndProc: LInteger;
begin
  if (uMsg = WM_ERASEBKGND) then
  begin
    Result := 1;
    Exit;
  end;

  {$IFDEF DELPHI_UNICODE}
  OldWndProc := GetWindowLongPtr(hWnd, GWL_USERDATA);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  OldWndProc := GetWindowLong(hWnd, GWL_USERDATA);
  {$ENDIF}
  if ((uMsg = WM_SHOWWINDOW) and (not Boolean(WParam))) or (uMsg = WM_CLOSE) or
    (uMsg = WM_DESTROY) then
  begin
    SetWindowRgn(hWnd, 0, True);
//    if (OldWndProc <> 0) then
//    begin
//      SetWindowLong(hWnd, GWL_WNDPROC, OldWndProc);
//      SetWindowLong(hWnd, GWL_USERDATA, 0);
//    end;
  end;
  Result := CallWindowProc(IntPtr(OldWndProc), hWnd, uMsg, WParam, lParam);
end;

function MDIClientWndProc(hWnd: hWnd; uMsg: Integer; WParam: WParam;
  lParam: lParam): LRESULT; stdcall;
const
  SeparatorShortcut = $ABCD;
  WindowItemGroupIndex = $CD;
var
  AdvMenu: TAdvMainMenu;
  MenuForm: TForm;
  Separator, NewItem: TMenuItem;
  I: Integer;
  ChildList: TStringList;
begin
  if uMsg = WM_MDIREFRESHMENU then
  begin
    Result := 0;
    MenuForm := FindForm(GetParent(hWnd));

    if (MenuForm <> nil) and (MenuForm.FormStyle = fsMDIForm) and
      (MenuForm.Menu <> nil) and (MenuForm.WindowMenu <> nil) and
      (MenuForm.Menu is TAdvMainMenu) then
      with MenuForm do
      begin
        AdvMenu := TAdvMainMenu(Menu);
        AdvMenu.BeginUpdate;
        Separator := AdvMenu.FindItem(SeparatorShortcut, fkShortcut);
        if Assigned(Separator) and (Separator.Caption = cLineCaption) then
          while Separator.MenuIndex + 1 < WindowMenu.Count do
            WindowMenu.delete(Separator.MenuIndex + 1)
          else
          begin
            Separator := TMenuItem.Create(MenuForm.WindowMenu);
            Separator.Caption := cLineCaption;
            Separator.ShortCut := SeparatorShortcut;
            Separator.AutoLineReduction := maAutomatic;
            Separator.Visible := MenuForm.MDIChildCount > 0;
            WindowMenu.Add(Separator);
          end;

        ChildList := TStringList.Create;
        ChildList.Sorted := True;
        ChildList.Duplicates := dupAccept;

        for I := 0 to MenuForm.MDIChildCount - 1 do
        begin
          if not(csDestroying in MenuForm.MDIChildren[I].ComponentState) then
          begin
            ChildList.AddObject(MenuForm.MDIChildren[I].Caption,
              TObject(MenuForm.MDIChildren[I].Handle));
          end;
        end;

        for I := 0 to ChildList.Count - 1 do
        begin
          NewItem := TMenuItem.Create(WindowMenu);

          with NewItem do
          begin
            Caption := IntToStr(I + 1) + ' ' + ChildList.Strings[I];
            Tag := THandle((ChildList.Objects[I]));
            OnClick := AdvMenu.ActivateMDIChild;
            RadioItem := True;
            GroupIndex := WindowItemGroupIndex;
            Checked := Assigned(MenuForm.ActiveMDIChild) and
              (Tag = Integer(MenuForm.ActiveMDIChild.Handle));
          end;
          WindowMenu.Add(NewItem);
        end;
        ChildList.Free;
        AdvMenu.EndUpdate;
        Result := AdvMenu.Handle;
      end;
  end
  else
  {$IFDEF DELPHI_UNICODE}
    Result := CallWindowProc(IntPtr(GetWindowLongPtr(hWnd, GWL_USERDATA)), hWnd,
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
    Result := CallWindowProc(IntPtr(GetWindowLong(hWnd, GWL_USERDATA)), hWnd,
  {$ENDIF}
      uMsg, WParam, lParam);
end;

procedure DrawMenuWindowBorder(MenuItem: TMenuItem; Canvas: TCanvas);
var
  R, WindowRect: TRect;
  WindowCanvas: TCanvas;
  WindowRgnMenu: HRGN;
  AdvMenu: IAdvMenu;
  WindowHandle: THandle;
  Level: Integer;
  TopRect: TRect;
  idx, I: Integer;
begin
  {TODO: find workaround in case when WindowFromDC returns zero due to menu animation}
  WindowHandle := WindowFromDC(Canvas.Handle);
  AdvMenu := GetMenuItemIntf(MenuItem);

  if not IsWindow(WindowHandle) or (WindowHandle = AdvMenu.Self.WindowHandle)
    or IsInTopMainMenu(MenuItem) then
    Exit;

  GetWindowRect(WindowHandle, WindowRect);

  Level := GetMenuItemLevel(MenuItem);

  if AdvMenu.Self is TPopupMenu then
    Inc(Level);


  {$IFDEF DELPHI_UNICODE}
  if (GetWindowLongPtr(WindowHandle, GWL_WNDPROC) <> LInteger(@MenuWindowProc)) then
  begin
    SetWindowLongPtr(WindowHandle, GWL_USERDATA, GetWindowLongPtr(WindowHandle, GWL_WNDPROC));
    SetWindowLongPtr(WindowHandle, GWL_WNDPROC, LInteger(@MenuWindowProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  if (GetWindowLong(WindowHandle, GWL_WNDPROC) <> LInteger(@MenuWindowProc)) then
  begin
    SetWindowLong(WindowHandle, GWL_USERDATA, GetWindowLong(WindowHandle, GWL_WNDPROC));
    SetWindowLong(WindowHandle, GWL_WNDPROC, LInteger(@MenuWindowProc));
  {$ENDIF}

    with AdvMenu do
    begin
      // Modify window region
      R := Rect(0, 0, RectWidth(WindowRect), RectHeight(WindowRect));
      InflateRect(R, -1, -1);
      // Hide default submenu triangle
      Dec(R.Right, TriangleSize);
      WindowRgnMenu := CreateRectRgnIndirect(R);
      SetWindowRgn(WindowHandle, WindowRgnMenu, True);
      if Level = 1 then
        MoveWindow(WindowHandle, WindowRect.Left - 1, WindowRect.Top - 2,
          RectWidth(WindowRect), RectHeight(WindowRect), True);

      // Move menu window
      if (Level >= 2) then
      begin
        if IsRightAligned(MenuItem, WindowHandle) then
        begin
          Dec(WindowRect.Left, TriangleSize);
        end
        else
        begin
          Inc(WindowRect.Left, TriangleSize);
        end;

        MoveWindow(WindowHandle, WindowRect.Left + 3, WindowRect.Top,
          RectWidth(WindowRect), RectHeight(WindowRect), True);
      end;
    end;
  end;


  // Draw window border
  with AdvMenu.CurrentMenuStyler do
  begin
    WindowCanvas := nil;
    try
      WindowCanvas := TCanvas.Create;
      with WindowCanvas do
      begin
        R := Rect(0, 0, RectWidth(WindowRect), RectHeight(WindowRect));
        InflateRect(R, -1, -1);

        if ((Level < 2) or IsRightAligned(MenuItem, WindowHandle)) then
        begin
          Dec(R.Right, TriangleSize);

          if Level >= 2 then
            Dec(R.Right, TriangleSize);
        end;

        Handle := GetWindowDC(WindowHandle);
        Brush.Color := MenuBorderColor;
        FrameRect(R);

        if (Level = 1) then
        begin
          idx := MenuItem.Parent.MenuIndex;

          for I := MenuItem.Parent.MenuIndex downto 0 do
          begin
            if not GetParentMenuEx(MenuItem).Items[I].Visible then
              Dec(idx);
          end;

          GetMenuItemRect(GetParent(WindowHandle),
            GetParentMenuEx(MenuItem).Items.Handle, idx, TopRect);

          Pen.Color := Background.Color;
          OffsetRect(TopRect, -WindowRect.Left, -WindowRect.Top);
          MoveTo(TopRect.Left + 1, 1);
          LineTo(TopRect.Right - 1, 1);
        end;

        InflateRect(R, -1, -1);
        Brush.Color := Background.Color;
        FrameRect(R);
      end;
    finally
      ReleaseDC(WindowHandle, WindowCanvas.Handle);
      WindowCanvas.Handle := 0;
      FreeAndNil(WindowCanvas);
    end;
  end;
end;

procedure RefreshMenu(AdvMenu: IAdvMenu; const BarChanged,
  WidthChanged: Boolean);
var
  MDIClientWnd: THandle;
  CurWndProcPtr: Pointer;
  vclstyles: boolean;
  {$IFDEF DELPHIXE2_LVL}
  LStyle: TCustomStyleServices;
  {$ENDIF}

begin
  if not Assigned(AdvMenu) or not Assigned(AdvMenu.Self) then
    Exit;

  with AdvMenu do
  begin
    { Windows.DrawMenuBar does not execute MeasureItem events unless menu needs
      rebuild. UpdateItems forces to rebuild and redraw. For PopupMenus the
      MeasureItem events are executed. }
    if (BarChanged or WidthChanged) then
      UpdateItems; { todo : check if this is required only in designtime }
    //    else
    { if menu is TMainMenu, then the WindowHandle points to the form which
      uses the menu. If no form uses it, WindowHandle = 0. }
    //      if (Self is TMainMenu) and (Self.WindowHandle <> 0) then
    //        Windows.DrawMenuBar(Self.WindowHandle);

    //Self.MenuChanged(Self, FirstVisibleMenuItem(Self), (BarChanged or WidthChanged));

  {$IFNDEF DELPHIXE2_LVL}
  vclstyles := false;
  {$ENDIF}

  {$IFDEF DELPHIXE2_LVL}
  LStyle := StyleServices;
  vclstyles :=  LStyle.Enabled and (LStyle.Name <> 'Windows');
  {$ENDIF}

    // set window procedure if new window handle is assigned
    if (Self is TMainMenu) and not vclstyles then
    begin
      MDIClientWnd := GetMDIClientWnd(AdvMenu);
      if MDIClientWnd <> 0 then
      begin
        with TAdvMainMenu(Self) do
        begin
          {$IFDEF DELPHI_UNICODE}
          CurWndProcPtr := Pointer(GetWindowLongPtr(MDIClientWnd, GWL_WNDPROC));
          if (CurWndProcPtr <> @MDIClientWndProc) then
          begin
            if (OldWndHandle <> 0) then
            begin
              SetWindowLongPtr(OldWndHandle, GWL_WNDPROC, LInteger(OldWndProcPtr));
              SetWindowLongPtr(OldWndHandle, GWL_USERDATA, 0);
            end;
            OldWndHandle := MDIClientWnd;
            OldWndProcPtr := CurWndProcPtr;
            SetWindowLongPtr(OldWndHandle, GWL_WNDPROC, LInteger(@MDIClientWndProc));
            SetWindowLongPtr(OldWndHandle, GWL_USERDATA, LInteger(OldWndProcPtr));
          end;
          {$ENDIF}

          {$IFNDEF DELPHI_UNICODE}
          CurWndProcPtr := Pointer(GetWindowLong(MDIClientWnd, GWL_WNDPROC));
          if (CurWndProcPtr <> @MDIClientWndProc) then
          begin
            if (OldWndHandle <> 0) then
            begin
              SetWindowLong(OldWndHandle, GWL_WNDPROC, LInteger(OldWndProcPtr));
              SetWindowLong(OldWndHandle, GWL_USERDATA, 0);
            end;
            OldWndHandle := MDIClientWnd;
            OldWndProcPtr := CurWndProcPtr;
            SetWindowLong(OldWndHandle, GWL_WNDPROC, LInteger(@MDIClientWndProc));
            SetWindowLong(OldWndHandle, GWL_USERDATA, LInteger(OldWndProcPtr));
          end;
          {$ENDIF}

        end;
      end;
    end;
  end;
end;

function FirstVisibleItem(MenuItem: TMenuItem): TMenuItem;
var
  I: Integer;
begin
  { Returns first visible subitem of the specified Item }
  Result := nil;
  for I := 0 to MenuItem.Count - 1 do
  begin
    if MenuItem[I].Visible then // find first visible item
    begin
      Result := MenuItem[I];
      Break;
    end;
  end;
end;

function LastVisibleItem(MenuItem: TMenuItem): TMenuItem;
var
  I: Integer;
begin
  { Returns first visible subitem of the specified Item }
  Result := nil;
  for I := MenuItem.Count - 1 downto 0 do
  begin
    if MenuItem[I].Visible then // find first visible item
    begin
      Result := MenuItem[I];
      Break;
    end;
  end;
end;

function VisibleMenuIndex(MenuItem: TMenuItem): Integer;
var
  I: Integer;
  ParentItem: TMenuItem;
begin
  { Returns index of the menu item relative to its parent menu }
  Result := -1;
  if not MenuItem.Visible then
    Exit;

  ParentItem := MenuItem.Parent;
  for I := 0 to ParentItem.Count - 1 do
  begin
    if ParentItem[I].Visible then // find first visible item
      Inc(Result);
    if MenuItem = ParentItem[I] then
      Exit;
  end;
end;

function IsInFirstVisibleMenu(MenuItem: TMenuItem): Boolean;
var
  FirstMI: TMenuItem;
  Menu: TMenu;
begin
  Result := False;
  if not Assigned(MenuItem) then
    Exit;
  Menu := MenuItem.GetParentMenu;
  if Menu is TMainMenu then
  begin
    FirstMI := FirstVisibleItem(Menu.Items);
    if Assigned(FirstMI) then
      Result := (MenuItem.Parent = FirstMI);
  end
  else {if Menu is TPopupMenu then}
    Result := IsInTopMenu(MenuItem);
end;

function IsInTopMenu(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is in top menu of mainmenu or popupmenu }
  Result := (MenuItem.GetParentComponent is TMenu) and not
    (MenuItem.GetParentComponent is TAdvPopupMenu);
end;

function IsInFirstLevelMenu(MenuItem: TMenuItem): Boolean;
var
  ParentComponent: TComponent;
begin
  { tells if the item is in first level menu of mainmenu or popupmenu }
  ParentComponent := MenuItem.GetParentComponent;
  Result := Assigned(ParentComponent) and ((ParentComponent is TPopupMenu) or
      (ParentComponent.GetParentComponent is TMainMenu));
end;

function GetMenuItemLevel(MenuItem: TMenuItem): Integer;
begin
  { get level of the menu containing menu item (0 for top item) }
  Result := -1;

  while Assigned(MenuItem.Parent) do
  begin
    Inc(Result);
    MenuItem := MenuItem.Parent;
  end;
end;

function IsSideBarVisible(MenuItem: TMenuItem): Boolean;
var
  AdvMenu: IAdvMenu;
begin
  AdvMenu := GetMenuItemIntf(MenuItem);
  with AdvMenu.CurrentMenuStyler.SideBar do
    Result := Visible and (((VisibleIn = viFirstVisible)
          and IsInFirstVisibleMenu(MenuItem)) or ((VisibleIn = viFirstLevel)
          and IsInFirstLevelMenu(MenuItem)) or ((VisibleIn = viAnyLevel)
          and not IsInTopMainMenu(MenuItem)));
end;

function IsInTopPopupMenu(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is in top menu of popupmenu }
  Result := MenuItem.GetParentComponent is TPopupMenu;
end;

function IsFirstSubMenuItem(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is first in the submenu }
  Result := False;
  if not Assigned(MenuItem) or not Assigned(MenuItem.Parent) then
    Exit;
  Result := (MenuItem = FirstVisibleItem(MenuItem.Parent));
end;

function IsInTopMainMenu(MenuItem: TMenuItem): Boolean;
begin
  { tells if the item is in top menu of mainmenu }
  Result := MenuItem.GetParentComponent is TMainMenu;
end;

{ look back for visible menu items and for break items }

function IsAfterMenuBreak(MenuItem: TMenuItem): Boolean;
var
  I: Integer;
  PMI: TMenuItem;
begin
  Result := False;
  if not Assigned(MenuItem.Parent) then
    Exit;
  with MenuItem do
  begin
    for I := MenuIndex { - 1} downto 0 do
    begin
      PMI := Parent.Items[I];
      if not PMI.Visible then
        Continue;
      if PMI.Break <> mbNone then
      begin
        Result := True;
        System.Break;
      end;
    end;
  end;
end;

function IsRightAligned(MenuItem: TMenuItem; MenuWnd: THandle): Boolean;
var
  R1, R2: TRect;
  I: Integer;
  RectIsEmpty: Boolean;
begin
  Result := False;

  // Check if menu item is aligned right relative to it's parent
  if IsInTopMenu(MenuItem) then
  begin
    // Don't use this fucntion with top level items
    Result := True
  end
  else
  begin
    RectIsEmpty := False;
    // workaround for NT: use MenuWnd = 0 for correct results
    if Win32Platform = VER_PLATFORM_WIN32_NT then
      MenuWnd := 0;

    I := MenuItem.MenuIndex;
    repeat
      if MenuItem.Visible then
      begin
        GetMenuItemRect(MenuWnd, MenuItem.Parent.Handle, 0, R1);
        RectIsEmpty := (RectHeight(R1) = 0) and (RectWidth(R1) = 0);
      end;
      Dec(I);
    until not RectIsEmpty or (I < 0);

    if not RectIsEmpty then
    begin
      with MenuItem.Parent do
      begin
        if Visible then
        begin
          GetMenuItemRect(GetParent(MenuWnd), Parent.Handle,
            VisibleMenuIndex(MenuItem.Parent), R2);
          Result := (R1.Left > R2.Left) or
            ((R1.Left = R2.Left) and (R1.Right > R2.Right));
        end;
        //Result := (R1.Left > R2.Right));
      end
    end
    else
    begin
      Result := True;
    end;
  end;
end;

(*
 function IsRightAligned(MenuItem: TMenuItem; MenuWnd: THandle): Boolean;
 var
 R1, R2: TRect;
 begin
 // Check if menu item is aligned right relative to it's parent
 if IsInTopMenu(MenuItem) then
 // Don't use this fucntion with top level items
 Result := True
 else
 begin
 // workaround for NT: use MenuWnd = 0 for correct results
 if Win32Platform = VER_PLATFORM_WIN32_NT then
 MenuWnd := 0;
 GetMenuItemRect(MenuWnd, MenuItem.Parent.Handle, MenuItem.MenuIndex, R1);
 MenuItem := MenuItem.Parent;
 GetMenuItemRect(GetParent(MenuWnd), MenuItem.Parent.Handle, MenuItem.MenuIndex, R2);
 Result := (R1.Left > R2.Left) or ((R1.Left = R2.Left) and (R1.Right > R2.Right));
 end;
 end;
*)

function IsRightmost(MenuItem: TMenuItem): Boolean;
var
  AdvMenu: IAdvMenu;
  MenuWnd: THandle;
  ParentMenu: HMENU;
  ItemInfo: TMenuItemInfo;
  I: Integer;
  ir1, ir2: TRect;

begin
  // Check if top level menu item is rightmost in its line
  Result := False;
  if not IsInTopMainMenu(MenuItem) then
    Exit;

  AdvMenu := GetMenuItemIntf(MenuItem);
  MenuWnd := GetMenuWnd(AdvMenu.Self);
  ParentMenu := GetMenu(MenuWnd);

  if MenuItem.MenuIndex < AdvMenu.Self.Items.Count - 1 then
  begin
    if Assigned(AdvMenu.Self) and Assigned(AdvMenu.Self.Owner) and
      (AdvMenu.Self.Owner is TWinControl) then
    begin
      GetMenuItemRect((AdvMenu.Self.Owner as TWinControl).Handle,
        MenuItem.GetParentMenu.Handle, MenuItem.MenuIndex, ir1);
      GetMenuItemRect((AdvMenu.Self.Owner as TWinControl).Handle,
        MenuItem.GetParentMenu.Handle, MenuItem.MenuIndex + 1, ir2);
      // ir2.Left = ir2.Right when item is not visible
      if (ir1.Top <> ir2.Top) and (ir2.Left <> ir2.Right) then
      begin

        Result := True;
        Exit;
      end;
    end;
  end;

  ItemInfo.cbSize := SizeOf(TMenuItemInfo);
  ItemInfo.fMask := MIIM_TYPE;
  I := GetMenuItemCount(ParentMenu);
  repeat
    Dec(I);
    GetMenuItemInfo(ParentMenu, I, True, ItemInfo);
  until (I < 0) or (ItemInfo.fType = MF_OWNERDRAW);

  Result := (I >= 0) and ((MenuItem.Command = GetMenuItemID(ParentMenu, I)) or
      (MenuItem.Handle = GetSubMenu(ParentMenu, I)));

  // fix for incorrect root item position in Win98
  if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    UpdateWindow(MenuWnd);
end;

function GetAdvMenuIntf(AMenu: TMenu): IAdvMenu;
begin
  Result := nil;
  if AMenu is TAdvPopupMenu then
    Result := TAdvPopupMenu(AMenu)
  else if AMenu is TAdvMainMenu then
    Result := TAdvMainMenu(AMenu);
end;

function GetMenuItemIntf(AMenuItem: TMenuItem): IAdvMenu;
begin
  Result := GetAdvMenuIntf(GetParentMenuEx(AMenuItem));
end;

function GetKeyText(nkeyCode: Integer): string;
var
  nScanCode: Integer;
  sText: array [0 .. 255] of char;
begin
  nScanCode := MapVirtualKey(nkeyCode, 0) shl 16;

  if not(nkeyCode in [VK_SHIFT, VK_MENU, VK_CONTROL, VK_RETURN, VK_BACK]) then
    nScanCode := nScanCode or $1000000;

  GetKeyNameText(nScanCode, sText, 255);
  Result := strpas(sText);
end;

function MakeUpperLower(s: string): string;
var
  Len: Integer;
begin
  Len := Length(s);
  Result := copy(s, 1, 1) + copy(LowerCase(s), 2, Len - 1);
  if (pos('(', Result) > 0) then
    Result := copy(Result, 1, pos('(', Result)) + UpperCase
      (copy(Result, pos('(', Result) + 1, 1)) + copy(Result,
      pos('(', Result) + 2, Len - pos('(', Result) - 1);
end;

function AdvShortCutToText(ShortCut: TShortCut; Intl: Boolean): string;
var
  MaskedKey: TShortCut;
begin
  if Intl then
  begin
    MaskedKey := ShortCut and $FF;

    if (MaskedKey >= VK_F1) and (MaskedKey <= VK_F24) then
      Result := 'F' + IntToStr(MaskedKey - (VK_F1 - 1))
    else
      Result := MakeUpperLower(GetKeyText(MaskedKey));

    if ShortCut and scShift <> 0 then
      Result := MakeUpperLower(GetKeyText(VK_SHIFT)) + '+' + Result;

    if ShortCut and scCtrl <> 0 then
      Result := MakeUpperLower(GetKeyText(VK_CONTROL)) + '+' + Result;

    if ShortCut and scAlt <> 0 then
      Result := MakeUpperLower(GetKeyText(VK_MENU)) + '+' + Result;
  end
  else
  begin
    Result := ShortCutToText(ShortCut);
    if (ShortCut and scWinKey) <> 0 then
      Result := AdvWinKeyString + Result;
  end;
end;

function GetMenuRect(MenuItem: TMenuItem; MenuWnd: THandle): TRect;
var
  I: Integer;
  ItemRect: TRect;
begin
  with MenuItem.Parent do
  begin
    GetMenuItemRect(MenuWnd, Handle, 0, Result);

    for I := 1 to Count do
    begin
      GetMenuItemRect(MenuWnd, Handle, I, ItemRect);
      UnionRect(Result, Result, ItemRect);
    end;
  end;
end;

function GetMenuWnd(Menu: TMenu): THandle;
begin
  Result := Menu.WindowHandle;
  if Menu.Owner is TForm then
    with TForm(Menu.Owner) do
      if FormStyle = fsMDIChild then
      begin
        Result := Handle;
        if Result <> 0 then
          Result := GetParent(Result);
        if Result <> 0 then
          Result := GetParent(Result);
      end;
end;

function GetMDIClientWnd(AdvMenu: IAdvMenu): THandle;
begin
  Result := 0;

  if not(AdvMenu.Self is TMainMenu) then
    Exit;

  if (csDestroying in AdvMenu.Self.ComponentState) then
    Exit;

  if AdvMenu.Self.Owner is TForm then
    with AdvMenu.Self.Owner as TForm do
      case FormStyle of
        fsMDIChild:
          Result := GetParent(Handle);
        fsMDIForm:
          Result := ClientHandle;
      end;
end;

function FindForm(Window: THandle): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
    if Window = Screen.Forms[I].Handle then
    begin
      Result := Screen.Forms[I];
      System.Break;
    end;
end;

procedure DisableMenuAnimation;
begin
  // todo: disable animation only for period of menu appearing
  SystemParametersInfo(SPI_GETMENUANIMATION, 0, @OldMenuAnimation, 0);
  SystemParametersInfo(SPI_SETMENUANIMATION, 0, TObject(0), SPIF_SENDCHANGE);
end;

procedure RestoreMenuAnimation;
begin
  SystemParametersInfo(SPI_SETMENUANIMATION, 0, TObject(OldMenuAnimation),
    SPIF_SENDCHANGE);
end;

{ TGradientBar }

{ ============================================================================
  TGradientBar.Create
  TGradientBar.Destroy
  ---------------------------------------------------------------------------- }

constructor TGradientBar.Create(AMenu: TMenu);
begin
  inherited Create;
  FColor := DefaultColor;
  FColorTo := DefaultColorTo;
  FGradientDirection := DefaultMenuGradientDirection;
  FMenu := AMenu;
end;

destructor TGradientBar.Destroy;
begin
  inherited Destroy;
end;

function TGradientBar.GetGradientColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FColor;
    1:
      Result := FColorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TGradientBar.SetGradientColor(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0:
      FColor := Value;
    1:
      FColorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TGradientBar.SetGradientDirection(const Value: TGradientDirection);
begin
  FGradientDirection := Value;
end;

{ ============================================================================
  TAdvBarGradient.Assign
  ---------------------------------------------------------------------------- }

procedure TGradientBar.Assign(Source: TPersistent);
begin
  if Source is TGradientBar then
    with TGradientBar(Source) do
    begin
      Self.Color := Color;
      Self.ColorTo := ColorTo;
      Self.GradientDirection := GradientDirection;
    end
    else
      inherited Assign(Source); // raises an exception if cannot assign
end;

{ TSeparator }

{ ============================================================================
  TSeparator.Create
  TSeparator.Destroy
  ---------------------------------------------------------------------------- }

constructor TSeparator.Create(AMenu: TMenu);
begin
  inherited;
  FColor := DefaultSeparatorColor;
  FColorTo := DefaultSeparatorColorTo;
  FWidth := DefaultSeparatorWidth;
  FGradientDirection := DefaultSeparatorGradientDirection;
  FMargin := DefaultSeparatorMargin;
  FGradientType := DefaultSeparatorGradientType;
  FGradientWidth := DefaultSeparatorGradientWidth;
  GradientDirection := gdHorizontal;
  FMenu := AMenu;
end;

destructor TSeparator.Destroy;
begin
  inherited Destroy;
end;

{ ============================================================================
  TSeparator.Assign
  ---------------------------------------------------------------------------- }

procedure TSeparator.Assign(Source: TPersistent);
begin
  if Source is TSeparator then
    with TSeparator(Source) do
    begin
      Self.Width := Width;
      Self.GradientType := GradientType;
      Self.GradientWidth := GradientWidth;
      Self.Margin := Margin;
    end;
  inherited Assign(Source); // raises an exception if cannot assign
end;

{ TSideBar }

{ ============================================================================
  TSideBar.Create
  TSideBar.Destroy
  ---------------------------------------------------------------------------- }

constructor TSideBar.Create(AMenu: TMenu);
begin
  inherited;
  FVisible := DefaultBarVisible;
  FPosition := DefaultBarSide;
  FSize := DefaultBarSize;
  FSplitterColor := DefaultBarSplitterColor;
  FSplitterWidth := DefaultBarSplitterWidth;
  FSplitterMargin := DefaultBarSplitterMargin;
  FSplitterGradientWidth := DefaultBarSplitterGradientWidth;
  FSplitterGradientType := DefaultBarSplitterGradientType;
  FImage := TBackground.Create(FMenu);
  FBackground := TBackground.Create(FMenu);
  FBorderColor := DefaultBarBorder;
  FPopupHeight := 0;
  FVisibleIn := DefaultBarVisibleIn;
  FMenu := AMenu;
end;

destructor TSideBar.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FBackground);
  inherited Destroy;
end;

{ ============================================================================
  TSideBar
  Set methods
  ---------------------------------------------------------------------------- }

procedure TSideBar.SetVisible(Value: Boolean);
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if not(csDesigning in FMenu.ComponentState) then
{$ENDIF}
    FVisible := Value;
{$IFDEF DESIGNTIMEPREVIEW}
  if (FMenu is TAdvMainMenu) and (FMenu.WindowHandle <> 0) then
    RefreshMenu(TAdvMainMenu(FMenu), False, True);
{$ENDIF}
end;

procedure TSideBar.SetImage(const Value: TBackground);
begin
  FImage.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), True, False);
end;

procedure TSideBar.SetBackground(const Value: TBackground);
begin
  FBackground.Assign(Value);
  if Assigned(FBackground.Image) and Assigned(FBackground.Image.Graphic) and
    (FBackground.Image.Graphic is TBitmap) then
    FBackground.Bitmap.Transparent := True;
  RefreshMenu(GetAdvMenuIntf(Menu), True, False);
end;

{ ============================================================================
  TSideBar
  Get methods
  ---------------------------------------------------------------------------- }

function TSideBar.GetImage: TBackground;
begin
  Result := FImage;
end;

function TSideBar.GetBackground: TBackground;
begin
  Result := FBackground;
end;

function TSideBar.GetVisible: Boolean;
begin
  Result := FVisible;
end;

{ ============================================================================
  TSideBar.Assign
  ---------------------------------------------------------------------------- }

procedure TSideBar.Assign(Source: TPersistent);
begin
  if Source is TSideBar then
    with TSideBar(Source) do
    begin
      Self.Visible := Visible;
      Self.Image.Assign(Image);
      Self.Background.Assign(Background);
      Self.Position := Position;
      Self.Size := Size;
      Self.SplitterWidth := SplitterWidth;
      Self.SplitterMargin := SplitterMargin;
      Self.SplitterGradientWidth := SplitterGradientWidth;
      Self.SplitterGradientType := SplitterGradientType;
      Self.BorderColor := BorderColor;
    end;
  inherited Assign(Source);
end;

{ TBackground }

{ ============================================================================
  TBackground.Create
  TBackground.Destroy
  ---------------------------------------------------------------------------- }

constructor TBackground.Create(AMenu: TMenu);
begin
  inherited;
  if Assigned(Image.Graphic) and (Image.Graphic is TBitmap) then
    Image.Bitmap.Transparent := DefaultBarBitmapTransparent;

  FPosition := DefaultBarBitmapPosition;
  FOffsetX := 0;
  FOffsetY := 0;
  FMarginX := 0;
  FMarginY := 0;
  FColor := DefaultItemColor;
  FColorTo := DefaultItemColorTo;
  GradientDirection := DefaultItemGradientDirection;
  FMenu := AMenu;
end;

destructor TBackground.Destroy;
begin
  if Assigned(FImage) then
    FreeAndNil(FImage);

  if Assigned(FBitmap) then
    FreeAndNil(FBitmap);

  inherited Destroy;
end;

{ ============================================================================
  TBackground.Assign
  ---------------------------------------------------------------------------- }

procedure TBackground.Assign(Source: TPersistent);
begin
  if Source is TBackground then
    with TBackground(Source) do
    begin
      Self.Image := Image; // this will use the SetPicture method
      Self.OffsetX := OffsetX;
      Self.OffsetY := OffsetY;
      Self.Position := Position;
      Self.Color := Color;
      Self.ColorTo := ColorTo;
    end
    else
      inherited Assign(Source); // raises exception
end;

{ ============================================================================
  TBackground.CalcPicturePosition
  Description: Calculates the position of the picture where is should be drawn.
  Uses the owner menus PopupHeight property.
  ---------------------------------------------------------------------------- }

procedure TBackground.CalcPicturePosition(ARect: TRect; var X, Y: Integer);
begin
  X := ARect.Left;
  Y := ARect.Top;
  if not Assigned(Image) or not Assigned(Image.Graphic)
    or Image.Graphic.Empty then
    Exit;

  case Position of
    bpTopLeft, bpBottomLeft:
      Inc(X, OffsetX);
    bpTopRight, bpBottomRight:
      Inc(X, RectWidth(ARect) + OffsetX - Image.Width);
    bpCenter, bpTopCenter, bpBottomCenter:
      Inc(X, ((RectWidth(ARect) - Image.Width) div 2) + OffsetX);
  end;
  case Position of
    bpTopLeft, bpTopRight, bpTopCenter:
      Inc(Y, OffsetY);
    bpBottomLeft, bpBottomRight, bpBottomCenter:
      Inc(Y, RectHeight(ARect) + OffsetY - Image.Height);
    bpCenter:
      Inc(Y, ((RectHeight(ARect) - Image.Height) div 2) + OffsetY);
  end;
end;

{ ============================================================================
  TBackground
  Set methods
  ---------------------------------------------------------------------------- }

procedure TBackground.SetImage(Value: TPicture);
begin
  { If the new value is nil or empty then free the internal instance of
    TPicture to save resources. }
  if Assigned(Value) then
    Image.Assign(Value)
  else
  begin
    if Assigned(FImage) and Assigned(FImage.Graphic) then
      FImage.Graphic := nil;
    FreeAndNil(FImage);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

{ ============================================================================
  TBackground
  Get methods
  ---------------------------------------------------------------------------- }

function TBackground.GetImage: TPicture;
begin
  if not Assigned(FImage) then
    FImage := TPicture.Create;

  if FImage.Graphic is TBitmap then
  begin
    FImage.Bitmap.Transparent := DefaultBarBitmapTransparent;
    FImage.Bitmap.TransparentMode := tmAuto;
  end;
  Result := FImage;
end;

function TBackground.GetBitmap: TBitmap;
begin
  if not Assigned(FBitmap) then
    FBitmap := TBitmap.Create;
  Result := FBitmap;
end;

{ TBarCaption }

{ ============================================================================
  TBarCaption.Create
  TBarCaption.Destroy
  ---------------------------------------------------------------------------- }

constructor TBarCaption.Create(AMenu: TMenu);
begin
  inherited Create;
  FAlignment := DefaultBarCaptionAlignment;
  FShadowColor := DefaultBarCaptionShadowColor;
  FHighlightColor := DefaultBarCaptionHighlightColor;
  FDepth := DefaultBarCaptionDepth;
  FDirection := DefaultBarCaptionDirection;
  FOffsetY := DefaultBarCaptionOffsetY;
  FCaption := '';

  FFont := TFont.Create;
  with FFont do
  begin
    Name := 'Tahoma';
    Size := 14;
    Color := clWhite;
    Style := [fsBold, fsItalic];
  end;
  FMenu := AMenu;
end;

destructor TBarCaption.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TBarCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), True, False);
end;

{ ============================================================================
  TBarCaption.Assign
  ---------------------------------------------------------------------------- }

procedure TBarCaption.Assign(Source: TPersistent);
begin
  if Source is TBarCaption then
    with TBarCaption(Source) do
    begin
      Self.OffsetY := OffsetY;
      Self.Alignment := Alignment;
      Self.Caption := Caption;
      Self.Font.Assign(Font);
      Self.ShadowColor := ShadowColor;
      Self.HighlightColor := HighlightColor;
      Self.Depth := Depth;
      Self.Direction := Direction;
    end
    else
      inherited Assign(Source); // raises exception
end;

{ TAdvPopupMenu }

{ ============================================================================
  TAdvPopupMenu.Create
  TAdvPopupMenu.Destroy
  ---------------------------------------------------------------------------- }

constructor TAdvPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerDraw := True; // required!!
  FOpacity := DefaultOpacity;
  FUpdateRef := 0;
  FMenuUpdating := False;
  AllocMenuStyler;
  FShowNotes := True;
end;

destructor TAdvPopupMenu.Destroy;
begin
  FMenuStyler := nil;
  FreeAndNil(FInternalMenuStyler);
  inherited Destroy;
end;

function TAdvPopupMenu._AddRef: Integer;
begin
  Result := 1;
end;

function TAdvPopupMenu._Release: Integer;
begin
  Result := 1;
end;

function TAdvPopupMenu.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn)))
    + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvPopupMenu.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvPopupMenu.SetVersion(const Value: string);
begin
end;

{ ============================================================================
  TAdvPopupMenu
  Set methods
  ---------------------------------------------------------------------------- }

procedure TAdvPopupMenu.SetOpacity(const Value: Byte);
begin
  FOpacity := Value;
end;

procedure TAdvPopupMenu.SetShowNotes(const Value: Boolean);
begin
  FShowNotes := Value;
end;

procedure TAdvPopupMenu.SetIntlKeybShortCuts(const Value: Boolean);
begin
  FIntlKeybShortCuts := Value;
end;

procedure TAdvPopupMenu.SetMenuStyler(const Value: TCustomAdvMenuStyler);
begin
  { Assigning existing value or clearing internal MenuStyler is a NOP }
  if (Value = FMenuStyler)
  {or ((Value = nil) and Assigned(FInternalMenuStyler))} then
    Exit;
  { Remove FreeNotification from existing external reference }
  if Assigned(FMenuStyler) and (FMenuStyler <> FInternalMenuStyler) then
    FMenuStyler.RemoveFreeNotification(Self);
  { Reference to external MenuStyler was cleared, recreate internal }
  if (Value = nil) then
    AllocMenuStyler
  else
  begin
    { Free the internal MenuStyler when assigning an external MenuStyler }
    if Assigned(FInternalMenuStyler) and
    { but not if we are streaming in, then wait until loaded is called }
    not(csLoading in FInternalMenuStyler.ComponentState) then
      FreeAndNil(FInternalMenuStyler);
    FMenuStyler := Value;
    FCurrentMenuStyler := FMenuStyler;
    FMenuStyler.FreeNotification(Self);
  end;
  FCurrentMenuStyler.Menu := Self;
end;

{ ============================================================================
  TAdvPopupMenu
  Get methods
  ---------------------------------------------------------------------------- }

function TAdvPopupMenu.GetSelf: TMenu;
begin
  Result := Self;
end;

function TAdvPopupMenu.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

function TAdvPopupMenu.GetShowNotes: Boolean;
begin
  Result := FShowNotes;
end;

function TAdvPopupMenu.GetIntlKeybShortCuts: Boolean;
begin
  Result := FIntlKeybShortCuts;
end;

function TAdvPopupMenu.GetMenuStyler: TCustomAdvMenuStyler;
begin
  Result := FMenuStyler;
end;

{ ============================================================================
  TAdvPopupMenu.Popup
  TAdvPopupMenu.PopupAtCursor
  Description: Overloaded method for P: TPoint parameter. Good for e.g.
  MyPopup.Popup(Mouse.CursorPos); or use the PopupAtCursor.
  ---------------------------------------------------------------------------- }
procedure TAdvPopupMenu.Popup(X, Y: Integer);
var
  LeftSide, RightSide: Boolean;
  MenuWidth: Integer;
  MeasureItemStruct: TMeasureItemStruct;
begin
  if Items.Count <= 0 then
  begin
    inherited Popup(X, Y);
    Exit;
  end;

  // correct popup point coordinates due to custom submenu triangles support

  with MeasureItemStruct do
  begin
    CtlType := ODT_MENU;
    itemID := Items[0].Command;
    itemWidth := 10;
    itemHeight := 10;
  end;

  SendMessage(PopupList.Window, WM_MEASUREITEM, 0, lParam(@MeasureItemStruct));

  MenuWidth := Integer(MeasureItemStruct.itemWidth) + TriangleSize;

  LeftSide := (X - TriangleSize - MenuWidth < 0);
  RightSide := (X + TriangleSize * 2 + MenuWidth >= Screen.DesktopWidth);

  if (Alignment = paRight) and not LeftSide then
    Inc(X, TriangleSize)
  else if (Alignment = paLeft) and RightSide then
    Inc(X, TriangleSize)
  else if Alignment = paCenter then
    Inc(X, TriangleSize div 2);

  if X >= Screen.DesktopWidth then
    X := Screen.DesktopWidth - 1;

  inherited Popup(X, Y);

  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TAdvPopupMenu.Popup(P: TPoint);
begin
  Popup(P.X, P.Y);
end;

procedure TAdvPopupMenu.PopupAtCursor;
begin
  Popup(Mouse.CursorPos);
end;

{ ============================================================================
  TAdvPopupMenu.ExpandItemWidth
  TAdvPopupMenu.AdvancedDrawWidth
  Description: Events for menuitems
  ---------------------------------------------------------------------------- }

procedure TAdvPopupMenu.ExpandItemWidth(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  MI: TMenuItem;
begin
  MI := TMenuItem(Sender);
  MeasureItem(MI, ACanvas, Width, Height, IsSideBarVisible(MI));
end;

procedure TAdvPopupMenu.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState);
var
  MI: TMenuItem;
begin
  MI := TMenuItem(Sender);
  //if not (csDesigning in ComponentState) then
  Dec(ARect.Right, TriangleSize); // cut off space reserved for triangle
  DrawMenuItemBackground(MI, ACanvas, ARect, State, IsSideBarVisible(MI));
end;

{ ============================================================================
  TAdvPopupMenu.MenuChanged
  Description: Monitors changes in the menu and makes any post changes.
  ---------------------------------------------------------------------------- }

procedure TAdvPopupMenu.MenuChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if not(csDesigning in ComponentState) then
{$ENDIF}
  begin
    if FDirectUpdate then
    begin
      SetMenuItemEvents(Self, Source, ExpandItemWidth, AdvancedDrawItem, True);
      if not FMenuUpdating then
        RefreshMenu(Self, False, False);
    end
    else
    begin
      if not MenuUpdating then
        SetMenuItemEvents(Self, Source, ExpandItemWidth, AdvancedDrawItem, True)
      else
        RefreshMenu(Self, False, False);
    end;
  end;
  inherited MenuChanged(Sender, Source, Rebuild);
end;

procedure TAdvPopupMenu.Loaded;
begin
  inherited Loaded;

  OwnerDraw := True;
  SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);

  { Internal menu styler can now be safely deleted if needed }
  if FInternalMenuStyler <> FCurrentMenuStyler then
    FreeAndNil(FInternalMenuStyler);
end;

{ ============================================================================
  TAdvMainMenu.Notification
  TAdvMainMenu.AllocMenuStyler
  ---------------------------------------------------------------------------- }

procedure TAdvPopupMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if not(csDestroying in ComponentState) and (Operation = opRemove) and
    (AComponent = FMenuStyler) and (AComponent.Owner <> Self) then
    AllocMenuStyler;

  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;

  if (Operation = opRemove) and (AComponent = DisabledImages) then
    FDisabledImages := nil;
end;

procedure TAdvPopupMenu.AllocMenuStyler;
begin
  FMenuStyler := nil;
  FCurrentMenuStyler := TCustomAdvMenuStyler.Create(Self);
  FInternalMenuStyler := FCurrentMenuStyler;
  FCurrentMenuStyler.Name := 'InternalMenuStyler';
{$IFDEF DELPHI6_LVL}
  FCurrentMenuStyler.SetSubComponent(True);
{$ENDIF}
end;

procedure TAdvPopupMenu.BeginUpdate;
begin
  FMenuUpdating := True;
  Inc(FUpdateRef);
end;

procedure TAdvPopupMenu.EndUpdate;
begin
  if FUpdateRef > 0 then
    Dec(FUpdateRef);

  if FUpdateRef = 0 then
  begin
    SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);
    FMenuUpdating := False;
  end;
end;

{ TAdvMainMenu }

{ ============================================================================
  TAdvMainMenu.Create
  TAdvMainMenu.Destroy
  ---------------------------------------------------------------------------- }

constructor TAdvMainMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OwnerDraw := True; // required!!
  FOpacity := DefaultOpacity;
  FUpdateRef := 0;
  FMenuUpdating := False;
  AllocMenuStyler;
  FShowNotes := True;
end;

destructor TAdvMainMenu.Destroy;
begin
  {$IFDEF DELPHI_UNICODE}
  SetWindowLongPtr(OldWndHandle, GWL_WNDPROC, LInteger(OldWndProcPtr));
  SetWindowLongPtr(OldWndHandle, GWL_USERDATA, 0);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  SetWindowLong(OldWndHandle, GWL_WNDPROC, LInteger(OldWndProcPtr));
  SetWindowLong(OldWndHandle, GWL_USERDATA, 0);
  {$ENDIF}
  OldWndHandle := 0;
  OldWndProcPtr := nil;

  FMenuStyler := nil;
  FreeAndNil(FInternalMenuStyler);
  inherited Destroy;
end;

function TAdvMainMenu._AddRef: Integer;
begin
  Result := 1;
end;

function TAdvMainMenu._Release: Integer;
begin
  Result := 1;
end;

function TAdvMainMenu.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn)))
    + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvMainMenu.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvMainMenu.SetVersion(const Value: string);
begin
end;

{ ============================================================================
  TAdvMainMenu.Notification
  TAdvMainMenu.AllocMenuStyler
  ---------------------------------------------------------------------------- }

procedure TAdvMainMenu.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if not(csDestroying in ComponentState) and (Operation = opRemove) and
    (AComponent = FMenuStyler) and (AComponent.Owner <> Self) then
    AllocMenuStyler;

  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;

  if (Operation = opRemove) and (AComponent = DisabledImages) then
    FDisabledImages := nil;
end;

procedure TAdvMainMenu.Refresh;
begin
  RefreshMenu(Self, True, True);
end;

procedure TAdvMainMenu.AllocMenuStyler;
begin
  FMenuStyler := nil;
  FCurrentMenuStyler := TAdvMenuStyler.Create(Self);
  FInternalMenuStyler := FCurrentMenuStyler;
  FCurrentMenuStyler.Name := 'InternalMenuStyler';
{$IFDEF DELPHI6_LVL}
  FCurrentMenuStyler.SetSubComponent(True);
{$ENDIF}
end;

{ ============================================================================
  TAdvMainMenu
  Set methods
  ---------------------------------------------------------------------------- }

procedure TAdvMainMenu.SetOpacity(const Value: Byte);
begin
  FOpacity := Value;
end;

procedure TAdvMainMenu.SetShowNotes(const Value: Boolean);
begin
  FShowNotes := Value;
end;

procedure TAdvMainMenu.SetIntlKeybShortCuts(const Value: Boolean);
begin
  FIntlKeybShortCuts := Value;
end;

procedure TAdvMainMenu.SetMenuStyler(const Value: TCustomAdvMenuStyler);
begin
  { Assigning existing value or clearing internal MenuStyler is a NOP }
  if (Value = FMenuStyler)
  {or ((Value = nil) and Assigned(FInternalMenuStyler))} then
    Exit;
  { Remove FreeNotification from existing external reference }
  if Assigned(FMenuStyler) and (FMenuStyler <> FInternalMenuStyler) then
    FMenuStyler.RemoveFreeNotification(Self);
  { Reference to external MenuStyler was cleared, recreate internal }
  if (Value = nil) then
    AllocMenuStyler
  else
  begin
    { Free the internal MenuStyler when assigning an external MenuStyler }
    if Assigned(FInternalMenuStyler) and
    { but not if we are streaming in, then wait until loaded is called }
    not(csLoading in FInternalMenuStyler.ComponentState) then
      FreeAndNil(FInternalMenuStyler);
    FMenuStyler := Value;
    FCurrentMenuStyler := FMenuStyler;
    FMenuStyler.FreeNotification(Self);
  end;
  FCurrentMenuStyler.Menu := Self;
  FCurrentMenuStyler.Font.OnChange := MenuFontChanged;
end;

{ ============================================================================
  TAdvMainMenu
  Get methods
  ---------------------------------------------------------------------------- }

function TAdvMainMenu.GetSelf: TMenu;
begin
  Result := Self;
end;

function TAdvMainMenu.GetMenuStyler: TCustomAdvMenuStyler;
begin
  Result := FMenuStyler;
end;

function TAdvMainMenu.GetOpacity: Byte;
begin
  Result := FOpacity;
end;

function TAdvMainMenu.GetShowNotes: Boolean;
begin
  Result := FShowNotes;
end;

function TAdvMainMenu.GetIntlKeybShortCuts: Boolean;
begin
  Result := FIntlKeybShortCuts;
end;

{ ============================================================================
  TAdvMainMenu.ExpandItemWidth
  TAdvMainMenu.AdvancedDrawWidth
  Description: Events for menuitems
  ---------------------------------------------------------------------------- }

procedure TAdvMainMenu.ExpandItemWidth(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  MI: TMenuItem;
begin
  MI := TMenuItem(Sender);
  MeasureItem(MI, ACanvas, Width, Height, IsSideBarVisible(MI));
end;

function GetWindowIcon(Wnd: hWnd; LargeIcon: Boolean; var Size: TSize): HICON;
var
  Width, Height: Integer;
  TempIcon: HICON;
  IconType: DWORD;
begin
  if LargeIcon then
  begin
    Width := GetSystemMetrics(SM_CXICON);
    Height := GetSystemMetrics(SM_CYICON);
    IconType := ICON_BIG;
    TempIcon := GetClassLong(Wnd, GCL_HICON);
    if TempIcon = 0 then
      TempIcon := SendMessage(Wnd, WM_GETICON, IconType, 0);
  end
  else
  begin
    Width := GetSystemMetrics(SM_CXSMICON);
    Height := GetSystemMetrics(SM_CYSMICON);
    IconType := ICON_SMALL;
    TempIcon := GetClassLong(Wnd, GCL_HICONSM);

    if TempIcon = 0 then
      TempIcon := SendMessage(Wnd, WM_GETICON, IconType, 0);
{$IFDEF DELPHI7_LVL}
    if TempIcon = 0 then
      TempIcon := SendMessage(Wnd, WM_GETICON, ICON_SMALL2, 0);
{$ENDIF}
  end;
  if (TempIcon = 0) and not LargeIcon then
    TempIcon := SendMessage(Wnd, WM_GETICON, ICON_BIG, 0);
  Result := CopyImage(TempIcon, IMAGE_ICON, Width, Height, 0);
  Size.cx := Width;
  Size.cy := Height;
end;

procedure TAdvMainMenu.DrawMDIFormIcon(MI: TMenuItem; MR: TRect);
var
  PF: TForm;
  //ic: TIcon;
  R: TRect;
  ACanvas: TCanvas;
  Hi: HICON;
  sz: TSize;
begin
  if not IsInTopMainMenu(MI) or (Self.Items.IndexOf(MI) > 0) or (MR.Left < 5)
    then
    Exit;

  if (Self.Owner is TForm) then
  begin
    PF := TForm(Self.Owner);
    if {not IsIconic(pf.Handle) or} (PF.FormStyle <> fsMDIForm) or not Assigned
      (PF.ActiveMDIChild) or not(PF.ActiveMDIChild.HandleAllocated)
    {or (pf.ActiveMDIChild.WindowState <> wsMaximized)} then
      Exit;
    {ic := Application.Icon;
     if not pf.ActiveMDIChild.Icon.Empty then
     ic := pf.ActiveMDIChild.Icon;
     }

    sz.cx := -1;
    sz.cy := -1;
    Hi := GetWindowIcon(PF.ActiveMDIChild.Handle, False, sz);
    if (Hi = 0){ic.Empty} then
      Exit;

    //R := Rect(4, 30, 4 + XTtlBit - 3, 28 + YTtlBit - 4);
    R := Rect(4, MR.Top, MR.Left, MR.Bottom);

    if (sz.cx > R.Right) then
    begin
      DestroyIcon(Hi);
      Exit;
    end;

    ACanvas := TCanvas.Create;
    ACanvas.Handle := GetWindowDC(PF.Handle); {Get Device context for drawing}

    try
      //-- Draw background
      with CurrentMenuStyler.RootItem do
      begin
        if (GradientDirection = gdVertical) then
          DrawGradientClipped(nil, ACanvas, R, R, Color, ColorTo,
            GradientDirection)
        else
          DrawGradientClipped(nil, ACanvas, R, R, Color, Color,
            GradientDirection);
      end;
      //--

      //-- Draw Icon
      //DrawIconEx(ACanvas.Handle, R.Left + 3, R.top + 2, ic.Handle, 16, 16, 0, 0, DI_NORMAL);
      DrawIconEx(ACanvas.Handle, R.Right - 16, R.Top + 2, Hi, 16, 16, 0, 0,
        DI_NORMAL);
      //--
    finally
      ReleaseDC(PF.Handle, ACanvas.Handle);
      ACanvas.Handle := 0;
      ACanvas.Free;
      DestroyIcon(Hi);
    end;
  end;
end;

procedure TAdvMainMenu.AdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState);
var
  MI: TMenuItem;
begin
  if FUpdateRef > 0 then
    Exit;

  MI := TMenuItem(Sender);
  if not IsInTopMainMenu(MI) then
    Dec(ARect.Right, TriangleSize); // cut off space reserved for triangle

  DrawMDIFormIcon(MI, ARect);
  DrawMenuItemBackground(MI, ACanvas, ARect, State, IsSideBarVisible(MI));
end;

{ ============================================================================
  TAdvMainMenu.MenuChanged
  TAdvMainMenu.MenuFontChanged
  Description: Monitors changes in the menu and makes any post changes.
  ---------------------------------------------------------------------------- }

procedure TAdvMainMenu.MenuChanged(Sender: TObject; Source: TMenuItem;
  Rebuild: Boolean);
begin
{$IFNDEF DESIGNTIMEPREVIEW}
  if not(csDesigning in ComponentState) then
{$ENDIF}
    if FDirectUpdate then
    begin
      SetMenuItemEvents(Self, Source, ExpandItemWidth, AdvancedDrawItem, True);
      if not FMenuUpdating then
        RefreshMenu(Self, False, False);
    end
    else
    begin
      if FMenuUpdating then
        SetMenuItemEvents(Self, Source, ExpandItemWidth, AdvancedDrawItem, True)
      else
        RefreshMenu(Self, False, False);
    end;

  inherited MenuChanged(Sender, Source, True);
end;

procedure TAdvMainMenu.MenuFontChanged(Sender: TObject);
begin
  RefreshMenu(Self, False, False);
end;

{ ============================================================================
  TAdvMainMenu.Loaded
  ---------------------------------------------------------------------------- }

procedure TAdvMainMenu.Loaded;
begin
  inherited Loaded;
  OwnerDraw := True;
  SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);

  { Internal menu styler can now be safely deleted if needed }
  if FInternalMenuStyler <> FCurrentMenuStyler then
    FreeAndNil(FInternalMenuStyler);

  { todo : why the measure events are not called without this "workaround" ?? }
  if Items.Count > 0 then
  begin
    Items[0].Visible := False;
    Items[0].Visible := True;
  end;
{$IFDEF DESIGNTIMEPREVIEW}
  //  MenuChanged(Self, nil, False);
{$ENDIF}
  RefreshMenu(Self, False, False);
end;

procedure TAdvMainMenu.BeginUpdate;
begin
  FMenuUpdating := True;
  Inc(FUpdateRef);
end;

procedure TAdvMainMenu.EndUpdate;
begin
  if FUpdateRef > 0 then
    Dec(FUpdateRef);

  if FUpdateRef = 0 then
  begin
    SetMenuItemEvents(Self, nil, ExpandItemWidth, AdvancedDrawItem, True);
    DrawMenuBar(WindowHandle);
    FMenuUpdating := False;
  end;
end;

procedure TAdvMainMenu.ActivateMDIChild(Sender: TObject);
var
  MenuItem: TMenuItem;
begin
  if (Sender is TMenuItem) then
  begin
    MenuItem := TMenuItem(Sender);
    if (MenuItem.Tag <> 0) and
      (SendMessage(GetMDIClientWnd(GetMenuItemIntf(MenuItem)), WM_MDIACTIVATE,
        MenuItem.Tag, 0) = 0) then
      MenuItem.Checked := True;
  end;
end;

{ TIconBar }

{ ============================================================================
  TIconBar.Create
  TIconBar.Assign
  ---------------------------------------------------------------------------- }

constructor TIconBar.Create(AMenu: TMenu);
begin
  inherited;
  FCheckColor := DefaultCheckColor;
  FCheckColorTo := DefaultCheckColorTo;
  FCheckBorder := DefaultCheckBorder;
  FCheckGradientDirection := DefaultCheckGradientDirection;
  FRadioColor := DefaultRadioColor;
  FRadioColorTo := DefaultRadioColorTo;
  FRadioBorder := DefaultRadioBorder;
  FRadioGradientDirection := DefaultRadioGradientDirection;
  FDrawn := False;
  FColor := DefaultIconBarColor;
  FColorTo := DefaultIconBarColorTo;
  FGradientDirection := DefaultIconBarGradientDirection;
  FSize := DefaultIconBarSize;
  FMenu := AMenu;
  FSeparatorColor := clNone;
end;

procedure TIconBar.Assign(Source: TPersistent);
begin
  if Source is TIconBar then
    with TIconBar(Source) do
    begin
      Self.CheckColor := CheckColor;
      Self.CheckColorTo := CheckColorTo;
      Self.CheckBorder := CheckBorder;
      Self.CheckGradientDirection := CheckGradientDirection;
      Self.RadioColor := RadioColor;
      Self.RadioColorTo := RadioColorTo;
      Self.RadioBorder := RadioBorder;
      Self.RadioGradientDirection := RadioGradientDirection;
      Self.Size := Size;
      Self.SeparatorColor := SeparatorColor;
    end
    else
      inherited Assign(Source); // raises exception
end;

{ ============================================================================
  TIconBar
  Get methods
  ---------------------------------------------------------------------------- }

function TIconBar.GetCheckColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FCheckColor;
    1:
      Result := FCheckColorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

function TIconBar.GetRadioColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FRadioColor;
    1:
      Result := FRadioColorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

{ ============================================================================
  TIconBar
  Set methods
  ---------------------------------------------------------------------------- }

procedure TIconBar.SetCheckColor(const Index: Integer; const Value: TColor);
begin
  case Index of
    0:
      FCheckColor := Value;
    1:
      FCheckColorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TIconBar.SetRadioColor(const Index: Integer; const Value: TColor);
begin
  case Index of
    0:
      FRadioColor := Value;
    1:
      FRadioColorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TIconBar.SetSize(const Value: Integer);
begin
  FSize := Max(Value, MinIconBarSize);
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

{ TMenuGlyphs }

{ ============================================================================
  TMenuGlyphs.Create
  TMenuGlyphs.Destroy
  TMenuGlyphs.Assign
  ---------------------------------------------------------------------------- }

constructor TMenuGlyphs.Create(AMenu: TMenu);
begin
  inherited Create;
  FSubMenu := TBitmap.Create;
  FSubMenu.Handle := LoadBitmap(HInstance, PChar(AdvResourceSubMenuItem));
  FSubMenu.Transparent := True;

  FCheck := TBitmap.Create;
  FCheck.Handle := LoadBitmap(HInstance, PChar(AdvResourceCheckItem));
  FCheck.Transparent := True;

  FRadio := TBitmap.Create;
  FRadio.Handle := LoadBitmap(HInstance, PChar(AdvResourceRadioItem));
  FRadio.Transparent := True;
  FMenu := AMenu;
end;

destructor TMenuGlyphs.Destroy;
begin
  FreeAndNil(FSubMenu);
  FreeAndNil(FCheck);
  FreeAndNil(FRadio);
  inherited;
end;

procedure TMenuGlyphs.Assign(Source: TPersistent);
begin
  if (Source is TMenuGlyphs) then
  begin
    FSubMenu.Assign((Source as TMenuGlyphs).SubMenu);
    FCheck.Assign((Source as TMenuGlyphs).Check);
    FRadio.Assign((Source as TMenuGlyphs).Radio);
  end
end;

{ ============================================================================
  TMenuGlyphs
  Set methods
  ---------------------------------------------------------------------------- }

procedure TMenuGlyphs.SetCheck(const Value: TBitmap);
begin
  FCheck.Assign(Value);
  FCheck.Transparent := True;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TMenuGlyphs.SetRadio(const Value: TBitmap);
begin
  FRadio.Assign(Value);
  FRadio.Transparent := True;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TMenuGlyphs.SetSubMenu(const Value: TBitmap);
begin
  FSubMenu.Assign(Value);
  FSubMenu.Transparent := True;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

{ TSelectedItem }

constructor TSelectedItem.Create(AMenu: TMenu);
begin
  inherited;
  FBorderColor := DefaultSelectedBorder;
  FUseSystemFont := DefaultUseSystemFont;
  FCheckColor := DefaultSelectedCheckColor;
  FCheckColorTo := DefaultSelectedCheckColorTo;
  FCheckBorder := DefaultSelectedCheckBorder;
  FCheckGradientDirection := DefaultSelectedCheckGradientDirection;
  FRadioColor := DefaultSelectedRadioColor;
  FRadioColorTo := DefaultSelectedRadioColorTo;
  FRadioBorder := DefaultSelectedRadioBorder;
  FRadioGradientDirection := DefaultSelectedRadioGradientDirection;
  FFont := TFont.Create;
  FFont.Assign(Screen.MenuFont);
  FFont.Color := DefaultSelectedTextColor;
  Color := DefaultSelectedColor;
  ColorTo := DefaultSelectedColorTo;
  GradientDirection := DefaultSelectedGradientDirection;
  FMenu := AMenu;
  FColorMirror := clNone;
  FColorMirrorTo := clNone;
  FNotesFont := TFont.Create;
  FNotesFont.Assign(Screen.MenuFont);
  FNotesFont.Size := FNotesFont.Size - 2;
  FNotesFont.Color := DefaultSelectedTextColor;
end;

destructor TSelectedItem.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FNotesFont);
  inherited;
end;

procedure TSelectedItem.Assign(Source: TPersistent);
begin
  if Source is TBackground then
    with TBackground(Source) do
    begin
      Self.ColorMirror := ColorMirror;
      Self.ColorMirrorTo := ColorMirrorTo;
      Self.BorderColor := BorderColor;
      Self.Font.Assign(Font);
      Self.UseSystemFont := UseSystemFont;
      Self.CheckColor := CheckColor;
      Self.CheckColorTo := CheckColorTo;
      Self.CheckGradientDirection := CheckGradientDirection;
      Self.RadioColor := RadioColor;
      Self.RadioColorTo := RadioColorTo;
      Self.RadioBorder := RadioBorder;
      Self.RadioGradientDirection := RadioGradientDirection;
    end
    else
      inherited Assign(Source); // raises exception
end;

function TSelectedItem.GetCheckColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FCheckColor;
    1:
      Result := FCheckColorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

function TSelectedItem.GetRadioColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FRadioColor;
    1:
      Result := FRadioColorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TSelectedItem.SetCheckColor(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0:
      FCheckColor := Value;
    1:
      FCheckColorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TSelectedItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TSelectedItem.SetNotesFont(const Value: TFont);
begin
  FNotesFont.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TSelectedItem.SetRadioColor(const Index: Integer;
  const Value: TColor);
begin
  case Index of
    0:
      FRadioColor := Value;
    1:
      FRadioColorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TSelectedItem.SetUseSystemFont(const Value: Boolean);
begin
  FUseSystemFont := Value;
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

{ TRootItem }

constructor TRootItem.Create(AMenu: TMenu);
begin
  inherited;
  Color := DefaultRootItemColor;
  ColorTo := DefaultRootItemColorTo;
  GradientDirection := DefaultRootItemGradientDirection;
  //  FBorderColor := DefaultRootItemBorder;
  FUseSystemFont := DefaultUseSystemFont;
  FFont := TFont.Create;
  FFont.Assign(Screen.MenuFont);
  FSelectedColor := DefaultRootSelectedColor;
  FSelectedColorTo := DefaultRootSelectedColorTo;
  FSelectedGradientDirection := DefaultRootSelectedGradientDirection;
  FSelectedBorderColor := DefaultRootSelectedBorder;
  FSelectedTextColor := DefaultRootSelectedTextColor;
  FHoverColor := DefaultHoverColor;
  FHoverColorTo := DefaultHoverColorTo;
  FHoverGradientDirection := DefaultHoverGradientDirection;
  FHoverBorderColor := DefaultHoverBorder;
  FHoverTextColor := DefaultHoverTextColor;
  FSelectedColorMirror := clNone;
  FSelectedColorMirrorTo := clNone;
  FMenu := AMenu;
  FSelectedColorMirror := clNone;
  FSelectedColorMirrorTo := clNone;
  FHoverColorMirror := clNone;
  FHoverColorMirrorTo := clNone;

end;

destructor TRootItem.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TRootItem.Assign(Source: TPersistent);
begin

  if Source is TRootItem then
    with TRootItem(Source) do
    begin
      //      Self.BorderColor := BorderColor;
      Self.Font.Assign(Font);
      Self.UseSystemFont := UseSystemFont;
      Self.SelectedColor := SelectedColor;
      Self.SelectedColorTo := SelectedColorTo;
      Self.SelectedGradientDirection := SelectedGradientDirection;
      Self.SelectedBorderColor := SelectedBorderColor;
      Self.SelectedTextColor := SelectedTextColor;
      Self.HoverColor := HoverColor;
      Self.HoverColorTo := HoverColorTo;
      Self.HoverGradientDirection := HoverGradientDirection;
      Self.HoverBorderColor := HoverBorderColor;
      Self.HoverTextColor := HoverTextColor;
      Self.SelectedColorMirror := SelectedColorMirror;
      Self.SelectedColorMirrorTo := SelectedColorMirrorTo;
    end
    else
      inherited Assign(Source); // raises exception
end;

function TRootItem.GetHoverColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FHoverColor;
    1:
      Result := FHoverColorTo;
    2:
      Result := FHoverColorMirror;
    3:
      Result := FHoverColorMirrorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

function TRootItem.GetSelectedColor(const Index: Integer): TColor;
begin
  case Index of
    0:
      Result := FSelectedColor;
    1:
      Result := FSelectedColorTo;
    2:
      Result := FSelectedColorMirror;
    3:
      Result := FSelectedColorMirrorTo;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
end;

procedure TRootItem.UpdateFont;
begin
  if UseSystemFont then
  begin
    FFont.Assign(Screen.MenuFont);
  end;
end;

procedure TRootItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TRootItem.SetHoverColor(const Index: Integer; const Value: TColor);
begin
  case Index of
    0:
      FHoverColor := Value;
    1:
      FHoverColorTo := Value;
    2:
      FHoverColorMirror := Value;
    3:
      FHoverColorMirrorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TRootItem.SetSelectedColor(const Index: Integer; const Value: TColor);
begin
  case Index of
    0:
      FSelectedColor := Value;
    1:
      FSelectedColorTo := Value;
    2:
      FSelectedColorMirror := Value;
    3:
      FSelectedColorMirrorTo := Value;
  else
    raise EInvalidPropertyIndex.CreateFmt(SInvalidPropertyIndex, [Index]);
  end;
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TRootItem.SetUseSystemFont(const Value: Boolean);
begin
  FUseSystemFont := Value;
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

{ TAdvMenuStyler }

procedure TCustomAdvMenuStyler.Assign(Source: TPersistent);
begin
  if (Source is TCustomAdvMenuStyler) then
  begin
    FBackground.Assign((Source as TCustomAdvMenuStyler).Background);
    FIconBar.Assign((Source as TCustomAdvMenuStyler).IconBar);
    FSelectedItem.Assign((Source as TCustomAdvMenuStyler).SelectedItem);
    FRootItem.Assign((Source as TCustomAdvMenuStyler).RootItem);
    FGlyphs.Assign((Source as TCustomAdvMenuStyler).Glyphs);
    FSideBar.Assign((Source as TCustomAdvMenuStyler).SideBar);
    FSeparator.Assign((Source as TCustomAdvMenuStyler).Separator);
    FFont.Assign((Source as TCustomAdvMenuStyler).Font);
    FNotesFont.Assign((Source as TCustomAdvMenuStyler).NotesFont);
    FUseSystemFont := (Source as TCustomAdvMenuStyler).UseSystemFont;

  end;
end;

constructor TCustomAdvMenuStyler.Create(AOwner: TComponent);
begin
  inherited;
  FSideBar := TSideBar.Create(FMenu);
  FBackground := TBackground.Create(FMenu);
  FIconBar := TIconBar.Create(FMenu);
  FSelectedItem := TSelectedItem.Create(FMenu);
  FRootItem := TRootItem.Create(FMenu);
  FGlyphs := TMenuGlyphs.Create(FMenu);
  FSeparator := TSeparator.Create(FMenu);
  FFont := TFont.Create;
  FFont.Assign(Screen.MenuFont);
  FUseSystemFont := DefaultUseSystemFont;
  FMenuBarUpdating := False;
  FMenuBorderColor := DefaultMenuBorderColor;
  FMenu := nil;
  FBackgroundBitmap := TBitmap.Create;
  FNotesFont := TFont.Create;
  FNotesFont.Assign(Screen.MenuFont);
  FNotesFont.Size := FNotesFont.Size - 2;
  FNotesFont.Color := clGray;
  FButtonAppearance := TButtonAppearance.Create;
end;

destructor TCustomAdvMenuStyler.Destroy;
begin
  FreeAndNil(FBackground);
  FreeAndNil(FIconBar);
  FreeAndNil(FSelectedItem);
  FreeAndNil(FRootItem);
  FreeAndNil(FGlyphs);
  FreeAndNil(FSideBar);
  FreeAndNil(FSeparator);
  FreeAndNil(FFont);
  FreeAndNil(FBackgroundBitmap);
  FreeAndNil(FNotesFont);
  FreeAndNil(FButtonAppearance);
  inherited;
end;

procedure TCustomAdvMenuStyler.Loaded;
begin
  inherited;
end;

procedure TCustomAdvMenuStyler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FMenu) then
    Menu := nil;
end;

procedure TCustomAdvMenuStyler.SetBackground(const Value: TBackground);
begin
  FBackground.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TCustomAdvMenuStyler.SetNotesFont(const Value: TFont);
begin
  FNotesFont.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TCustomAdvMenuStyler.SetAntiAlias(const Value: TAntiAlias);
begin
  FAntiAlias := Value;
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TCustomAdvMenuStyler.SetGlyphs(const Value: TMenuGlyphs);
begin
  FGlyphs.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetIconBar(const Value: TIconBar);
begin
  FIconBar.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetMenu(const Value: TMenu);
begin
  FMenu := Value;
  if Assigned(FMenu) then
    FMenu.FreeNotification(Self);
  Background.FMenu := Value;
  IconBar.FMenu := Value;
  SelectedItem.FMenu := Value;
  RootItem.FMenu := Value;
  Glyphs.FMenu := Value;
  SideBar.FMenu := Value;
  Separator.FMenu := Value;
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetRootItem(const Value: TRootItem);
begin
  FRootItem.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetSelectedItem(const Value: TSelectedItem);
begin
  FSelectedItem.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetSeparator(const Value: TSeparator);
begin
  FSeparator.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), False, False);
end;

procedure TCustomAdvMenuStyler.SetSideBar(const Value: TSideBar);
begin
  FSideBar.Assign(Value);
  RefreshMenu(GetAdvMenuIntf(Menu), True, False);
end;

procedure TCustomAdvMenuStyler.SetUseSystemFont(const Value: Boolean);
begin
  FUseSystemFont := Value;
  RefreshMenu(GetAdvMenuIntf(Menu), False, True);
end;

procedure TCustomAdvMenuStyler.SetTButtonAppearance
  (const Value: TButtonAppearance);
begin
  FButtonAppearance.Assign(Value);
end;

function IsVista: Boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := False;
end;

{ TButtonAppearance }

procedure TButtonAppearance.Assign(Source: TPersistent);
begin
  if Source is TButtonAppearance then
  begin
    FColor := TButtonAppearance(Source).Color;
    FColorTo := TButtonAppearance(Source).ColorTo;
    FHoverColor := TButtonAppearance(Source).HoverColor;
    FHoverColorTo := TButtonAppearance(Source).HoverColorTo;
    FDownColor := TButtonAppearance(Source).DownColor;
    FDownColorTo := TButtonAppearance(Source).DownColorTo;
    FGradientDirection := TButtonAppearance(Source).GradientDirection;
    FBorderColor := TButtonAppearance(Source).BorderColor;
    FHoverBorderColor := TButtonAppearance(Source).HoverBorderColor;
    FDownBorderColor := TButtonAppearance(Source).DownBorderColor;
    inherited Assign(Source);
  end;
end;

procedure TButtonAppearance.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TButtonAppearance.Create;
begin
  inherited;

  FColor := clNone;
  FColorTo := clNone;
  FHoverColor := $00D2BDB6;
  FHoverColorTo := clNone;
  FDownColor := $00B59285;
  FDownColorTo := clNone;
  FGradientDirection := gdHorizontal;

  FBorderColor := clNone;
  FDownBorderColor := RGB(10, 36, 106);
  FHoverBorderColor := RGB(10, 36, 106);

  FCaptionFont := TFont.Create;
  FGlyphPosition := gpLeft;
  FGradientDirection := gdVertical;

  if IsVista then
    CaptionFont.Name := 'Segoe UI'
  else
    CaptionFont.Name := 'Tahoma';

  FUseSystemFont := DefaultUseSystemFont;
end;

destructor TButtonAppearance.Destroy;
begin
  FCaptionFont.Free;
  inherited;
end;

procedure TButtonAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Change;
end;

procedure TButtonAppearance.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetDownColor(const Value: TColor);
begin
  if FDownColor <> Value then
  begin
    FDownColor := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetDownColorTo(const Value: TColor);
begin
  if FDownColorTo <> Value then
  begin
    FDownColorTo := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetHoverColor(const Value: TColor);
begin
  if FHoverColor <> Value then
  begin
    FHoverColor := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetHoverColorTo(const Value: TColor);
begin
  if FHoverColorTo <> Value then
  begin
    FHoverColorTo := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetGradientDirection
  (const Value: TGradientDirection);
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetGlyphPosition(const Value: TGlyphPosition);
begin
  if FGlyphPosition <> Value then
  begin
    FGlyphPosition := Value;
    Change;
  end;
end;

procedure TButtonAppearance.SetUseSystemFont(const Value: Boolean);
begin
  if (FUseSystemFont <> Value) then
  begin
    FUseSystemFont := Value;

    if Value then
    begin
      if IsVista then
        FCaptionFont.Name := 'Segoe UI'
      else
        FCaptionFont.Name := 'Tahoma';
    end;
  end;
end;

function TAdvMainMenu.GetCurrentMenuStyler: TCustomAdvMenuStyler;
begin
  Result := FCurrentMenuStyler;
end;

function TAdvPopupMenu.GetCurrentMenuStyler: TCustomAdvMenuStyler;
begin
  Result := FCurrentMenuStyler;
end;

{
 procedure TPopupListEx.WndProc(var Message: TMessage);
 begin
 case Message.Msg of
 WM_MENUSELECT:
 begin
 Message.Result := 0;
 Exit;
 end;
 WM_MENUCOMMAND:
 begin
 Message.Result := 0;
 Exit;
 end;
 WM_ENTERMENULOOP:
 MenuLoop := true;
 WM_EXITMENULOOP:
 begin
 MenuLoop := false;
 outputdebugstring('exit');
 Message.Result := 0;
 end;
 end;

 inherited WndProc(Message);
 end;
}

procedure TAdvMainMenu.Merge(Menu: TMainMenu);
begin
  inherited Merge(Menu);
  //if Assigned(Menu) then
  //  Menu.WindowHandle := Self.WindowHandle;
end;

initialization

{
 PopupList.Free;
 PopupList := TPopupListEx.Create;
}
  DisableMenuAnimation;

finalization
  RestoreMenuAnimation;

end.
