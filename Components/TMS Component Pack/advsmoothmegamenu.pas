{**************************************************************************}
{ TAdvSmoothMegaMenu component                                             }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2015                                                       }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothMegaMenu;

interface

{$I TMSDEFS.INC}

uses
  Forms, Messages, Windows, Dialogs, SysUtils, Classes, Graphics, Controls, StdCtrls,
  Comobj, AdvStyleIF, ImgList,
  GDIPPictureContainer, ExtCtrls, Math, GDIPFill, AdvSmoothTheme,
  GDIPMenu, INIFiles, Menus, AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.1.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : Fixed : Issue with Mega menu editor OnMouseLeave of PaintBox
  //          : Fixed : Issue with Mega menu gallery OnMouseLeave of PaintBox
  // v1.0.1.1 : Fixed : Issue with changing DefaultTopLayerItem with AutoSize = asCustom
  //          : Fixed : Issue with DropDownLocation of root menu item
  //          : Fixed : Hints in Mega menu editor
  //          : Fixed : Access violation when hovering empty section
  // v1.0.1.2 : Improved : CaptionLocation of MenuItem
  // v1.0.1.3 : Improved : Focusing of Empty Menu Items
  // v1.0.2.0 : New : Delphi 2010 Touch Support
  //          : Fixed : Issue with OnMenuItemClick and showing sub menu
  // v1.0.2.1 : Fixed : Issue with Saving changes in editor
  //          : Fixed : Issue with floating menus and mousemove
  // v1.0.2.2 : Fixed : Issue with <BR> tag in HTML
  //          : Fixed : Issue with hovering
  // v1.0.2.3 : Improved : Text drawing
  //          : Fixed : Issues with accessing menuitems and submenuitems in different ways
  //          : Fixed : Issue with text drawing
  //          : Fixed : Issue accessing the default section items
  //          : Fixed : Issue with OnMouseEnter and OnMouseLeave not firing
  // v1.0.3.0 : New : Property TextRendering to change to cleartype or antialiasing
  //          : Improved : Root menu caption in editor Form caption
  // v1.0.3.1 : Improved : ImageList support on root menu items
  // v1.0.3.2 : Fixed : Issue with overlapping shortcut keys
  //          : Fixed : Issue with GroupIndex and RadioButtons
  // v1.0.3.3 : Fixed : Issue with CaptionLocation
  // v1.0.4.0 : New : Property OpenMenusOnClick
  // v1.0.5.0 : New : Reorder Section items inside the mega menu editor through drag / drop
  //          : Fixed : Issue with Stay on top when using controls
  //          : Improved : Behavior when OpenMenusOnClick and AutoOpenMenus is true
  // v1.0.6.0 : New : Events OnMenuItemDropDown and OnMenuItemCloseUp
  //          : Improved : Calculation of item width with different font styles
  //          : Fixed : Issue with Adding non – wincontrol controls to a section item
  // v1.0.6.1 : Fixed : Issue with calling OnMouseMove event
  // v1.0.7.0 : New : Built-in support for Office 2010 colors
  //          : Fixed : Recalculation issue when modifying properties when changing to float mode
  // v1.0.7.1 : Fixed : Access violation when opening menu when destroying items
  // v1.0.8.0 : New : Visible property for menu items and section items
  // v1.0.8.1 : New : public SelectedItem property
  // v1.0.8.2 : Fixed : Visible and enabled items not selectable.
  //          : Fixed : Issue with focusing and keyboard
  //          : Fixed : Issue with ALT+F4
  // v1.0.8.3 : Fixed : Issue with closing menu second time
  //          : Fixed : Issue with positioning controls at designtime
  //          : Fixed : Issue with TabStop on submenu
  //          : Improved : Default AutoOpenMenus is True
  // v1.0.8.4 : Fixed : Issue with AutoSize based on caption.
  // v1.0.8.5 : Fixed : Issue with setting width of defaultsection access violation
  // v1.0.8.6 : Fixed : Issue with destroying forms in OnItemClick
  // v1.0.9.0 : New : Added Tag property for TAdvSmoothMegaMenuItem
  //          : New : CloseAllMenus procedure
  // v1.0.9.1 : Improved : Flickering when hovering from item to form
  //          : Fixed : Issue with hovering and floating menus
  //          : Fixed : Issue with closing menu in Delphi 7
  // v1.0.9.2 : Fixed : Issue with TearOff menu
  // v1.1.0.0 : New : Metro Style Support
  //          : Fixed : Issue with hovering over closed menu by clicking on a section item
  // v1.1.0.1 : Fixed : Issue with DoItemClick called when hovering
  // v1.2.0.0 : New : Windows 8, Office 2013 styles added
  // v1.2.0.1 : Fixed : Issue with initializing designtime editor
  // v1.3.5.0 : New : Editor Copy, Move and Reorder items and sections
  //          : New : Hiding Sections
  // v1.3.5.1 : Fixed : Issue with hiding items
  // v1.3.5.2 : Fixed : Issue with autosizing
  // v1.3.5.3 : Fixed : floating point error when hiding menu items
  // v1.3.5.4 : Fixed : Issue with auto-closing menu
  // v1.3.5.5 : Fixed : Issue with closing menu form and message handling
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added

const
  WM_RECALLFLOATINGFORM = WM_USER + 111;

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothMegaMenu = class;

  TAdvSmoothMegaMenuItem = class;

  TAdvSmoothMegaMenuTextRenderingHint = (tAntiAlias, tAntiAliasGridFit, tClearType);

  TAdvSmoothMegaMenuItemForm = class(TForm)
  private
    FMouseLeft: Boolean;
    FFloatingEvent: Boolean;
    FPosX, FPosY: integer;
    FParentLeft, FParentTop: integer;
    FMainBuffer: TGPBitmap;
    FMenu: TAdvSmoothMegaMenu;
    FMenuItem: TAdvSmoothMegaMenuItem;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure DoCMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var MSG: TWMSetCursor); message WM_SETCURSOR;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure Paint; override;
    procedure FormShow(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function IsFormFloating: Boolean;

    // ---- Paint proc
    procedure Draw(graphics: TGPGraphics);

    // ---- Paint buffer
    procedure CreateMainBuffer;
    procedure DestroyMainBuffer;
    procedure ClearBuffer(graphics: TGPGraphics);
    function CreateGraphics: TGPGraphics;

    //---- Layered window
    procedure SetLayeredWindow;
    procedure UpdateForm;
    procedure ClearControls;
    procedure AssignControls;
    procedure UpdateLayered;
    procedure UpdateMainWindow(Alpha: Byte);
    procedure UpdateWindow;
    procedure SetPosition(PosX: integer = -1; PosY: integer = -1);
    procedure DoItemSelection(Key: Word; SelectedItem: TCurrentItem; var sec, secit: integer);
  public
    procedure Init;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    property Menu: TAdvSmoothMegaMenu read FMenu write FMenu;
    property MenuItem: TAdvSmoothMegaMenuItem read FMenuItem write FMenuItem;
  end;

  TAdvSmoothMegaMenuMargin = class(TPersistent)
  private
    FOwner: TAdvSmoothMegaMenu;
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
    constructor Create(AOwner: TAdvSmoothMegaMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: integer read FLeft write SetLeft default 3;
    property Top: integer read FTop write SetTop default 3;
    property Bottom: integer read FBottom write SetBottom default 3;
    property Right: integer read FRight write SetRight default 3;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothMegaMenuItem = class(TCollectionItem)
  private
    FFormDestroying: Boolean;
    FFloating, FWillFloat: Boolean;
    frm: TAdvSmoothMegaMenuItemForm;
    FItemRect, FCaptionRect: TGPRectF;
    FOwner: TAdvSmoothMegaMenu;
    FEnabled: Boolean;
    FWidth: integer;
    FHeight: integer;
    FMenu: TGDIPMenu;
    FSeparator: Boolean;
    FGraphicLeftDisabledName: String;
    FGraphicLeftSelectedName: String;
    FGraphicLeftName: String;
    FGraphicRightSelectedName: String;
    FGraphicLeftHoverName: String;
    FGraphicRightName: String;
    FGraphicRightHoverName: String;
    FGraphicRightDisabledName: String;
    FCaptionFont: TFont;
    FCaptionTop: integer;
    FCaptionLeft: integer;
    FCaption: String;
    FCaptionLocation: TGDIPMenuLocation;
    FMenuHeight: integer;
    FMenuAutoSize: Boolean;
    FMenuWidth: integer;
    FPersistSelection: Boolean;
    FGraphicLeftHoverIndex: Integer;
    FGraphicRightIndex: Integer;
    FGraphicRightHoverIndex: Integer;
    FGraphicLeftDisabledIndex: Integer;
    FGraphicLeftSelectedIndex: Integer;
    FGraphicRightDisabledIndex: Integer;
    FGraphicLeftIndex: Integer;
    FGraphicRightSelectedIndex: Integer;
    FVisible: Boolean;
    FTag: Integer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetCaption(const Value: String);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetMenu(const Value: TGDIPMenu);
    procedure SetSeparator(const Value: Boolean);
    procedure SetGraphicLeftDisabledName(const Value: String);
    procedure SetGraphicLeftHoverName(const Value: String);
    procedure SetGraphicLeftName(const Value: String);
    procedure SetGraphicLeftSelectedName(const Value: String);
    procedure SetGraphicRightHoverName(const Value: String);
    procedure SetGraphicRightName(const Value: String);
    procedure SetGraphicRightSelectedName(const Value: String);
    procedure SetGraphicRightDisabledName(const Value: String);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCaptionLeft(const Value: integer);
    procedure SetCaptionLocation(const Value: TGDIPMenuLocation);
    procedure SetCaptionTop(const Value: integer);
    procedure SetMenuAutoSize(const Value: Boolean);
    procedure SetMenuHeight(const Value: integer);
    procedure SetMenuWidth(const Value: integer);
    function GetSections(Index: integer): TGDIPMenuSection;
    procedure SetSections(Index: integer; const Value: TGDIPMenuSection);
    procedure SetPersistSelection(const Value: Boolean);
    function GetSectionItems(SectionIndex,
      SectionItemIndex: integer): TGDIPMenuSectionItem;
    procedure SetSectionItems(SectionIndex, SectionItemIndex: integer;
      const Value: TGDIPMenuSectionItem);
    procedure SetGraphicLeftDisabledIndex(const Value: Integer);
    procedure SetGraphicLeftHoverIndex(const Value: Integer);
    procedure SetGraphicLeftIndex(const Value: Integer);
    procedure SetGraphicLeftSelectedIndex(const Value: Integer);
    procedure SetGraphicRightDisabledIndex(const Value: Integer);
    procedure SetGraphicRightHoverIndex(const Value: Integer);
    procedure SetGraphicRightIndex(const Value: Integer);
    procedure SetGraphicRightSelectedIndex(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure MenuChanged(Sender: TObject);
    procedure CaptionChanged(Sender: TObject);
    procedure Draw(g: TGPGraphics);
    function DrawMenuItemHTMLText(g: TGPGraphics; f: TFont; HTML: TGDIPMenuHTMLText; Location: TGDIPMenuLocation; r: TGPRectF; var htmlr: TGPRectF; str: String;
      DoCalculate: Boolean = false; DoAnchor: Boolean = false; fX: integer = -1; fY: integer = -1): String;
    function IsFormMoving: Boolean;
    procedure UpdateFormSize;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function IsFloating: Boolean;
    function GetMenu: TAdvSmoothMegaMenu;
    procedure OpenMenu;
    procedure CloseMenu(CloseFloating: Boolean = false);
    procedure CloseForm(Sender: TObject; var Action: TCloseAction);
    procedure FloatMenu(X: integer = -1; Y: integer = -1);
    procedure SetMenuPosition(X, Y: integer);
    property Sections[Index: integer]: TGDIPMenuSection read GetSections write SetSections;
    property SectionItems[SectionIndex, SectionItemIndex: integer]: TGDIPMenuSectionItem read GetSectionItems write SetSectionItems;
  published
    property Menu: TGDIPMenu read FMenu write SetMenu;
    property MenuAutoSize: Boolean read FMenuAutoSize write SetMenuAutoSize;
    property MenuWidth: integer read FMenuWidth write SetMenuWidth default 250;
    property MenuHeight: integer read FMenuHeight write SetMenuHeight default 150;
    property Caption: String read FCaption write SetCaption;
    property CaptionLocation: TGDIPMenuLocation read FCaptionLocation write SetCaptionLocation default mlCenterCenter;
    property CaptionLeft: integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: integer read FCaptionTop write SetCaptionTop default 0;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Width: integer read FWidth write SetWidth default 75;
    property Height: integer read FHeight write SetHeight default 30;
    property Separator: Boolean read FSeparator write SetSeparator default false;
    property GraphicLeftName: String read FGraphicLeftName write SetGraphicLeftName;
    property GraphicRightName: String read FGraphicRightName write SetGraphicRightName;
    property GraphicLeftSelectedName: String read FGraphicLeftSelectedName write SetGraphicLeftSelectedName;
    property GraphicRightSelectedName: String read FGraphicRightSelectedName write SetGraphicRightSelectedName;
    property GraphicLeftHoverName: String read FGraphicLeftHoverName write SetGraphicLeftHoverName;
    property GraphicRightHoverName: String read FGraphicRightHoverName write SetGraphicRightHoverName;
    property GraphicLeftDisabledName: String read FGraphicLeftDisabledName write SetGraphicLeftDisabledName;
    property GraphicRightDisabledName: String read FGraphicRightDisabledName write SetGraphicRightDisabledName;
    property GraphicLeftIndex: Integer read FGraphicLeftIndex write SetGraphicLeftIndex default -1;
    property GraphicRightIndex: Integer read FGraphicRightIndex write SetGraphicRightIndex default -1;
    property GraphicLeftSelectedIndex: Integer read FGraphicLeftSelectedIndex write SetGraphicLeftSelectedIndex default -1;
    property GraphicRightSelectedIndex: Integer read FGraphicRightSelectedIndex write SetGraphicRightSelectedIndex default -1;
    property GraphicLeftHoverIndex: Integer read FGraphicLeftHoverIndex write SetGraphicLeftHoverIndex default -1;
    property GraphicRightHoverIndex: Integer read FGraphicRightHoverIndex write SetGraphicRightHoverIndex default -1;
    property GraphicLeftDisabledIndex: Integer read FGraphicLeftDisabledIndex write SetGraphicLeftDisabledIndex default -1;
    property GraphicRightDisabledIndex: Integer read FGraphicRightDisabledIndex write SetGraphicRightDisabledIndex default -1;
    property PersistSelection: Boolean read FPersistSelection write SetPersistSelection default false;
    property Tag: Integer read FTag write FTag;
  end;

  TAdvSmoothMegaMenuItems = class(TOwnedCollection)
  private
    FOwner: TAdvSmoothMegaMenu;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TAdvSmoothMegaMenuItem;
    procedure SetItem(Index: Integer; const Value: TAdvSmoothMegaMenuItem);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
  public
    function CreateItemClass: TCollectionItemClass; virtual;
    constructor Create(AOwner: TAdvSmoothMegaMenu);
    property Items[Index: Integer]: TAdvSmoothMegaMenuItem read GetItem write SetItem; default;
    function Add: TAdvSmoothMegaMenuItem;
    function Insert(Index: Integer): TAdvSmoothMegaMenuItem;
    procedure Delete(Index: Integer);
  end;

  TAdvSmoothMegaMenuAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothMegaMenu;
    FOnChange: TNotifyEvent;
    FMenuItemSeparatorFill: TGDIPFill;
    FMenuItemFillHover: TGDIPFill;
    FMenuItemFontDisabled: TFont;
    FMenuItemFontSelected: TFont;
    FMenuItemFont: TFont;
    FMenuItemFillDisabled: TGDIPFill;
    FMenuItemFillSelected: TGDIPFill;
    FMenuItemSpacing: integer;
    FMenuItemFontHover: TFont;
    FMenuItemFill: TGDIPFill;
    FMargin: TAdvSmoothMegaMenuMargin;
    FMenuItemSeparatorHeight: integer;
    FMenuItemSeparatorWidth: integer;
    FMenuItemShadowColor: TColor;
    FMenuItemURLColor: TColor;
    FMenuItemShadowOffset: integer;
    procedure SetMenuItemFill(const Value: TGDIPFill);
    procedure SetMenuItemFillDisabled(const Value: TGDIPFill);
    procedure SetMenuItemFillHover(const Value: TGDIPFill);
    procedure SetMenuItemFillSelected(const Value: TGDIPFill);
    procedure SetMenuItemFont(const Value: TFont);
    procedure SetMenuItemFontDisabled(const Value: TFont);
    procedure SetMenuItemFontHover(const Value: TFont);
    procedure SetMenuItemFontSelected(const Value: TFont);
    procedure SetMenuItemSeparatorFill(const Value: TGDIPFill);
    procedure SetMenuItemSpacing(const Value: integer);
    procedure SetMargin(const Value: TAdvSmoothMegaMenuMargin);
    procedure SetMenuItemSeparatorHeight(const Value: integer);
    procedure SetMenuItemSeparatorWidth(const Value: integer);
    procedure SetMenuItemShadowColor(const Value: TColor);
    procedure SetMenuItemShadowOffset(const Value: integer);
    procedure SetMenuItemURLColor(const Value: TColor);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure MarginChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothMegaMenu);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SaveToFile(ini: TIniFile; Section: String);
    procedure LoadFromFile(ini: TIniFile; Section: String);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property MenuItemURLColor: TColor read FMenuItemURLColor write SetMenuItemURLColor default clBlue;
    property MenuItemShadowColor: TColor read FMenuItemShadowColor write SetMenuItemShadowColor default clGray;
    property MenuItemShadowOffset: integer read FMenuItemShadowOffset write SetMenuItemShadowOffset default 5;
    property MenuItemFill: TGDIPFill read FMenuItemFill write SetMenuItemFill;
    property MenuItemFillSelected: TGDIPFill read FMenuItemFillSelected write SetMenuItemFillSelected;
    property MenuItemFillHover: TGDIPFill read FMenuItemFillHover write SetMenuItemFillHover;
    property MenuItemFillDisabled: TGDIPFill read FMenuItemFillDisabled write SetMenuItemFillDisabled;
    property MenuItemFont: TFont read FMenuItemFont write SetMenuItemFont;
    property MenuItemFontSelected: TFont read FMenuItemFontSelected write SetMenuItemFontSelected;
    property MenuItemFontHover: TFont read FMenuItemFontHover write SetMenuItemFontHover;
    property MenuItemFontDisabled: TFont read FMenuItemFontDisabled write SetMenuItemFontDisabled;
    property MenuItemSeparatorFill: TGDIPFill read FMenuItemSeparatorFill write SetMenuItemSeparatorFill;
    property MenuItemSeparatorWidth: integer read FMenuItemSeparatorWidth write SetMenuItemSeparatorWidth default 3;
    property MenuItemSeparatorHeight: integer read FMenuItemSeparatorHeight write SetMenuItemSeparatorHeight default 50;
    property MenuItemSpacing: integer read FMenuItemSpacing write SetMenuItemSpacing default 0;
    property Margin: TAdvSmoothMegaMenuMargin read FMargin write SetMargin;
  end;

  TAdvSmoothMegaMenuDirection = (mdVertical, mdHorizontal);

  TAdvSmoothMegaMenuItemClickEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemMouseDownEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemMouseUpEvent = procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer; ItemIndex: Integer) of object;

  TAdvSmoothMegaMenuItemEnterEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemLeaveEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemChangeEvent = procedure(Sender: TObject; OldItemIndex, NewItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemFloatChangeEvent = procedure(Sender: TObject; Floating: Boolean; Menu: TAdvSmoothMegaMenuItem) of object;

  TAdvSmoothMegaMenuAutoSize = (asControlSize, asCaptionSize, asCustom);

  TAdvSmoothMegaMenuSubItemCheckChangedEvent = procedure(Sender: TObject; Menu: TAdvSmoothMegaMenu; MenuItem: TAdvSmoothMegaMenuItem; item: TGDIPMenuSectionItem) of object;

  TAdvSmoothMegaMenuSubItemEditChanged = procedure(Sender: TObject;  Menu: TAdvSmoothMegaMenu; MenuItem: TAdvSmoothMegaMenuItem; Text: String; item: TGDIPMenuSectionItem) of object;

  TAdvSmoothMegaMenuSubItemEvent = procedure(Sender: TObject;  Menu: TAdvSmoothMegaMenu; MenuItem: TAdvSmoothMegaMenuItem; Item: TGDIPMenuSectionItem; Text: String) of object;

  TAdvSmoothMegaMenuItemDropDownEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuItemCloseUpEvent = procedure(Sender: TObject; ItemIndex: integer) of object;

  TAdvSmoothMegaMenuSubItemAnchorEvent = procedure(Sender: TObject;  Menu: TAdvSmoothMegaMenu; MenuItem: TAdvSmoothMegaMenuItem; Item: TGDIPMenuSectionItem; Anchor: String) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothMegaMenu = class(TCustomControl, ITMSStyle, ITMSMegaMenu, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FPrevX, FPrevY: Integer;
    FMenuItemRecall: TAdvSmoothMegaMenuItem;
    SelectOnly: Boolean;
    FMenuAutoOpen, FMenuIsOpen: Boolean;
    FFocused: Boolean;
    Fstyle: TTMSStyle;
    FDesignTime: Boolean;
    FConstructed: Boolean;
    FMouseLeave: Boolean;
    MouseLeave, MouseEnter: Boolean;
    FFocusedItem, FLastHoveredItem, FHoveredItem, FSelectedItem: Integer;
    FAppearance: TAdvSmoothMegaMenuAppearance;
    FMenuItems: TAdvSmoothMegaMenuItems;
    FDirection: TAdvSmoothMegaMenuDirection;
    FTransparent: Boolean;
    FOnItemLeave: TAdvSmoothMegaMenuItemLeaveEvent;
    FOnItemEnter: TAdvSmoothMegaMenuItemEnterEvent;
    FOnItemClick: TAdvSmoothMegaMenuItemClickEvent;
    FBackGroundFill: TGDIPFill;
    FAutoSize: TAdvSmoothMegaMenuAutoSize;
    FPictureContainer: TGDIPPictureContainer;
    FImageList: TImageList;
    FOnSubItemClick: TAdvSmoothMegaMenuSubItemEvent;
    FOnSubItemHover: TAdvSmoothMegaMenuSubItemEvent;
    FAllowSelection: Boolean;
    FOnSubItemEditChanged: TAdvSmoothMegaMenuSubItemEditChanged;
    FOnSubItemCheckChanged: TAdvSmoothMegaMenuSubItemCheckChangedEvent;
    FOnFloatChanged: TAdvSmoothMegaMenuItemFloatChangeEvent;
    FDefaultSectionItemAppearance: TGDIPMenuSectionItemAppearance;
    FDefaultTopLayerItems: TGDIPMenuTopLayerItems;
    FDefaultSections: TGDIPMenuSections;
    FDefaultTopLayerItem: TGDIPMenuTopLayerItem;
    FDefaultSection: TGDIPMenuSection;
    FDefaultMenuTearOffFill: TGDIPFill;
    FDefaultMenuContentFill: TGDIPFill;
    FOnSubItemAnchorClick: TAdvSmoothMegaMenuSubItemAnchorEvent;
    FAutoOpenMenus: Boolean;
    FPersistSelection: Boolean;
    FTextRendering: TAdvSmoothMegaMenuTextRenderingHint;
    FOpenMenusOnClick: Boolean;
    FOnMenuItemDropDown: TAdvSmoothMegaMenuItemDropDownEvent;
    FOnMenuItemCloseUp: TAdvSmoothMegaMenuItemCloseUpEvent;
    FOnMenuItemMouseDown: TAdvSmoothMegaMenuItemMouseDownEvent;
    FOnMenuItemMouseMove: TAdvSmoothMegaMenuItemMouseMoveEvent;
    FOnMenuItemMouseUp: TAdvSmoothMegaMenuItemMouseUpEvent;
    procedure SetAppearance(const Value: TAdvSmoothMegaMenuAppearance);
    procedure SetMenuItems(const Value: TAdvSmoothMegaMenuItems);
    procedure SetDirection(const Value: TAdvSmoothMegaMenuDirection);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetTransparent(const Value: Boolean);
    procedure SetBackGroundFill(const Value: TGDIPFill);
    procedure SetAS(const Value: TAdvSmoothMegaMenuAutoSize);
    procedure SetPictureContainer(const Value: TGDIPPictureContainer);
    procedure SetDefaultSectionItemAppearance(
      const Value: TGDIPMenuSectionItemAppearance);
    procedure SetDefaultSection(const Value: TGDIPMenuSection);
    procedure SetDefaultTopLayerItem(const Value: TGDIPMenuTopLayerItem);
    procedure SetDefaultMenuContentFill(const Value: TGDIPFill);
    procedure SetDefaultMenuTearOffFill(const Value: TGDIPFill);
    procedure SetAllowSelection(const Value: Boolean);
    procedure SetAutoOpenMenus(const Value: Boolean);
    procedure SetPersistSelection(const Value: Boolean);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    procedure SetTextRendering(
      const Value: TAdvSmoothMegaMenuTextRenderingHint);
    procedure SetOpenMenusOnClick(const Value: Boolean);
    procedure SetSelectedItem(const Value: Integer);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    procedure MenuItemsChanged(Sender: TObject);
    procedure InitItemRects;
    function InsideRect: TRect;
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawMenuItems(g: TGPGraphics);
    function XYToItem(X, Y: integer): integer;
    procedure DoItemClick(Sender: TObject; ItemIndex: integer);
    procedure DoItemEnter(Sender: TObject; ItemIndex: integer);
    procedure DoItemLeave(Sender: TObject; ItemIndex: integer);
    procedure DoDropDown(Sender: TObject; ItemIndex: integer);
    procedure DoCloseUp(Sender: TObject; ItemIndex: integer);
    procedure DoMenuSubItemClick(Sender: TObject; Item: TGDIPMenuSectionItem; Text: String);
    procedure DoMenuSubItemHover(Sender: TObject; Item: TGDIPMenuSectionItem; Text: String);
    procedure DoMenuSubItemEditChanged(Sender: TObject; Text: String; item: TGDIPMenuSectionItem);
    procedure DoMenuSubItemCheckChanged(Sender: TObject; item: TGDIPMenuSectionItem);
    procedure DoMenuSubItemAnchor(Sender: TObject; Item: TGDIPMenuSectionItem; Anchor: String);
    procedure ShowItemForm(item: integer);
    procedure GetLocation(var x, y: Double; rectangle: TGPRectF;
      objectwidth, objectheight: Double; location: TGDIPMenuLocation);
    procedure InitPreview;
    function GetMenuItemFromSubMenu(Item: TGDIPMenuSectionItem): TAdvSmoothMegaMenuItem;
    function GetVersionNr: integer;
    procedure WndProc(var Message: TMessage); override;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure CloseAllMenus;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure Resize; override;
    procedure Paint; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure CreateWnd; override;
    procedure ApplyMenuItemDefault(MenuItem, Section, TopLayerItem: integer);
    procedure ApplyDFstyle;
    procedure SaveToFile(Filename: String);
    procedure LoadFromFile(Filename: String; ApplyDefaultStyles: Boolean = true);
    procedure LoadFromTheme(FileName: String);
    procedure SaveToTheme(FileName: String);
    function GetThemeId: String;
    function GetPictureContainer: TGDIPPictureContainer;
    function GetImageList: TCustomImageList;
     function GetDefaultItemAppearance: TGDIPMenuSectionItemAppearance;
    function GetDefaultSection: TGDIPMenuSection;
    function GetDefaultTopLayerItem: TGDIPMenuTopLayerItem;
    function GetFirstMenu: TGDIPMenu;
    function GetNextMenu(Menu: TGDIPMenu): TGDIPMenu;
    function GetPreviousMenu(Menu: TGDIPMenu): TGDIPMenu;
    function HasMultipleMenus: Boolean;
    property SelectedItem: Integer read FSelectedItem write SetSelectedItem;
  published
    property OpenMenusOnClick: Boolean read FOpenMenusOnClick write SetOpenMenusOnClick default false;
    property AutoOpenMenus: Boolean read FAutoOpenMenus write SetAutoOpenMenus default True;
    property DefaultMenuContentFill: TGDIPFill read FDefaultMenuContentFill write SetDefaultMenuContentFill;
    property DefaultMenuTearOffFill: TGDIPFill read FDefaultMenuTearOffFill write SetDefaultMenuTearOffFill;
    property DefaultSectionItemAppearance: TGDIPMenuSectionItemAppearance read FDefaultSectionItemAppearance write SetDefaultSectionItemAppearance;
    property DefaultSection: TGDIPMenuSection read FDefaultSection write SetDefaultSection;
    property DefaultTopLayerItem: TGDIPMenuTopLayerItem read FDefaultTopLayerItem write SetDefaultTopLayerItem;
    property AllowSelection: Boolean read FAllowSelection write SetAllowSelection default true;
    property PersistSelection: Boolean read FPersistSelection write SetPersistSelection default true;
    property AutoSize: TAdvSmoothMegaMenuAutoSize read FAutoSize write SetAS default asControlSize;
    property ItemAppearance: TAdvSmoothMegaMenuAppearance read FAppearance write SetAppearance;
     property MenuItems: TAdvSmoothMegaMenuItems read FMenuItems write SetMenuItems;
    property BackGroundFill: TGDIPFill read FBackGroundFill write SetBackGroundFill;
    property Direction: TAdvSmoothMegaMenuDirection read FDirection write SetDirection default mdHorizontal;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property ImageList: TImageList read FImageList write FImageList;
    property PictureContainer: TGDIPPictureContainer read FPictureContainer write SetPictureContainer;
    property OnMenuSubItemClick: TAdvSmoothMegaMenuSubItemEvent read FOnSubItemClick write FOnSubItemClick;
    property OnMenuSubItemHover: TAdvSmoothMegaMenuSubItemEvent read FOnSubItemHover write FOnSubItemHover;
    property OnMenuSubItemEditChanged: TAdvSmoothMegaMenuSubItemEditChanged read FOnSubItemEditChanged write FOnSubItemEditChanged;
    property OnMenuSubItemCheckChanged: TAdvSmoothMegaMenuSubItemCheckChangedEvent read FOnSubItemCheckChanged write FOnSubItemCheckChanged;
    property OnMenuSubItemAnchorClick: TAdvSmoothMegaMenuSubItemAnchorEvent read FOnSubItemAnchorClick write FOnSubItemAnchorClick;
    property OnMenuItemClick: TAdvSmoothMegaMenuItemClickEvent read FOnItemClick write FOnItemClick;
    property OnMenuItemMouseDown: TAdvSmoothMegaMenuItemMouseDownEvent read FOnMenuItemMouseDown write FOnMenuItemMouseDown;
    property OnMenuItemMouseMove: TAdvSmoothMegaMenuItemMouseMoveEvent read FOnMenuItemMouseMove write FOnMenuItemMouseMove;
    property OnMenuItemMouseUp: TAdvSmoothMegaMenuItemMouseUpEvent read FOnMenuItemMouseUp write FOnMenuItemMouseUp;
    property OnMenuItemEnter: TAdvSmoothMegaMenuItemEnterEvent read FOnItemEnter write FOnItemEnter;
    property OnMenuItemLeave: TAdvSmoothMegaMenuItemLeaveEvent read FOnItemLeave write FOnItemLeave;
    property OnMenuItemFloatChanged: TAdvSmoothMegaMenuItemFloatChangeEvent read FOnFloatChanged write FOnFloatChanged;
    property OnMenuItemDropDown: TAdvSmoothMegaMenuItemDropDownEvent read FOnMenuItemDropDown write FOnMenuItemDropDown;
    property OnMenuItemCloseUp: TAdvSmoothMegaMenuItemCloseUpEvent read FOnMenuItemCloseUp write FOnMenuItemCloseUp;

    property Version: String read GetVersion write SetVersion;
    property TextRendering: TAdvSmoothMegaMenuTextRenderingHint read FTextRendering write SetTextRendering default tClearType;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property Align;
    property ShowHint;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseActivate;
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseDown;
    property OnStartDock;
    property OnStartDrag;
    property PopupMenu;
    property Visible;
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

uses
  CommCtrl, ShellApi;

{$I GDIPHTMLEngine.pas}

function GetFontStyles(Style: TFontStyles): String;
var
  str: String;
begin
  str := '';
  if (fsBold in Style) then
    str := str + ':0';
  if (fsItalic in Style) then
    str := str + ':1';
  if (fsUnderline in Style) then
    str := str + ':2';
  if (fsStrikeOut in Style) then
    str := str + ':3';

  Result := str;
end;

procedure Split
   (const Delimiter: Char;
    Input: string;
    const Strings: TStrings) ;
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

procedure SaveFont(ini: TIniFile; Section: String; f: TFont);
begin
  ini.WriteInteger(Section, 'FontSize', f.Size);
  ini.WriteInteger(Section, 'FontColor', f.Color);
  ini.WriteString(Section, 'FontName', f.Name);
  ini.WriteString(Section, 'FontStyle', GetFontStyles(f.Style));
end;

procedure LoadFont(ini: TIniFile; Section: String; f: TFont);
var
  str: String;
  a: TStringList;
  i: integer;
begin
  f.Size := ini.ReadInteger(Section, 'FontSize', f.Size);
  f.Color := ini.ReadInteger(Section, 'FontColor', f.Color);
  f.Name := ini.ReadString(Section, 'FontName', f.Name);
  str := ini.ReadString(Section, 'FontStyle', '');
  A := TStringList.Create;
  Split(':',str, A);
  for I := 1 to A.Count - 1 do
  begin
    f.Style := f.Style + [Graphics.TFontStyle(strtoint(A[I]))];
  end;
  A.Free;
end;

{ TAdvSmoothMegaMenu }

procedure TAdvSmoothMegaMenu.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenu.ApplyDFstyle;
var
  i, c: integer;
begin
  for I := 0 to MenuItems.Count - 1 do
  begin
    MenuItems[I].Menu.ItemAppearance.Assign(DefaultSectionItemAppearance);
    MenuItems[I].Menu.ContentFill.Assign(DefaultMenuContentFill);
    MenuItems[I].Menu.TearOffFill.Assign(DefaultMenuTearOffFill);
    for C := 0 to MenuItems[I].Menu.Sections.Count - 1 do
    begin
      MenuItems[I].Menu.Sections[C].CaptionFont.Assign(DefaultSection.CaptionFont);
      MenuItems[I].Menu.Sections[C].CaptionFill.Assign(DefaultSection.CaptionFill);
      MenuItems[I].Menu.Sections[C].BackGroundFill.Assign(DefaultSection.BackGroundFill);
    end;

    for C := 0 to MenuItems[I].Menu.TopLayerItems.Count - 1 do
    begin
      MenuItems[I].Menu.TopLayerItems[C].Fill.Assign(DefaultTopLayerItem.Fill);
    end;
  end;
end;

procedure TAdvSmoothMegaMenu.ApplyMenuItemDefault(MenuItem, Section, TopLayerItem: integer);
begin
  if (MenuItem >= 0) and (MenuItem <= MenuItems.Count - 1) then
  begin
    DefaultMenuContentFill.Assign(MenuItems[0].Menu.ContentFill);
    DefaultMenuTearOffFill.Assign(MenuItems[0].Menu.TearOffFill);
    DefaultSectionItemAppearance.Assign(MenuItems[0].Menu.ItemAppearance);
    if (Section >= 0) and (Section <= MenuItems[0].Menu.Sections.Count - 1) then
      DefaultSection.Assign(MenuItems[0].Menu.Sections[0]);
    if (TopLayerItem >= 0) and (TopLayerItem <= MenuItems[0].Menu.TopLayerItems.Count - 1) then
      DefaultTopLayerItem.Assign(MenuItems[0].Menu.TopLayerItems[0]);
  end;
end;

procedure TAdvSmoothMegaMenu.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMegaMenu) then
  begin
    FAppearance.Assign((Source as TAdvSmoothMegaMenu).ItemAppearance);
    FMenuItems.Assign((Source as TAdvSmoothMegaMenu).MenuItems);
    FBackGroundFill.Assign((Source as TAdvSmoothMegaMenu).BackGroundFill);
    FDirection := (Source as TAdvSmoothMegaMenu).Direction;
    FTransparent := (Source as TAdvSmoothMegaMenu).Transparent;
    FAutoSize := (Source as TAdvSmoothMegaMenu).AutoSize;
    FAllowSelection := (Source as TAdvSmoothMegaMenu).AllowSelection;
    FDefaultSectionItemAppearance.Assign((source as TAdvSmoothMegaMenu).DefaultSectionItemAppearance);
    FDefaultSection.Assign((Source as TAdvSmoothMegaMenu).DefaultSection);
    FDefaultTopLayerItem.Assign((Source as TAdvSmoothMegaMenu).DefaultTopLayerItem);
    FAutoOpenMenus := (Source as TAdvSmoothMegaMenu).AutoOpenMenus;
    FOpenMenusOnClick := (Source as  TAdvSmoothMegaMenu).OpenMenusOnClick;
    FPersistSelection := (Source as TAdvSmoothMegaMenu).PersistSelection;
    FTextRendering := (Source as TAdvSmoothMegaMenu).TextRendering;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.Changed;
begin
  InitItemRects;
  Invalidate;
end;

procedure TAdvSmoothMegaMenu.CloseAllMenus;
var
  I: integer;
begin
  for I := 0 to MenuItems.Count - 1 do
    MenuItems[I].CloseMenu(True);
end;

procedure TAdvSmoothMegaMenu.CMDialogChar(var Message: TCMDialogChar);
var
  i, k: integer;
begin
  with Message do
  begin
    for I := 0 to MenuItems.Count - 1 do
    begin
      if IsAccel(CharCode, MenuItems[I].Caption) and CanFocus and not Assigned(MenuItems[I].frm) then
      begin
        for K := 0 to MenuItems.Count - 1 do
        begin
          if Assigned(MenuItems[K].frm) and not(MenuItems[K].IsFloating) then
            MenuItems[K].CloseMenu;
        end;

        if Assigned(OnMenuItemClick) then
          OnMenuItemClick(Self, I);

        ShowItemForm(I);

        FHoveredItem := I;
        FFocusedItem := I;

        Result := 1;

        Exit;
      end
      else
      begin
        if Assigned(MenuItems[i].frm) then
        begin
          MenuItems[I].frm.DoCMDialogChar(Message);
          result := 1;
          Exit;
        end;
      end;
    end;
  end;
  inherited;
end;

procedure TAdvSmoothMegaMenu.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHoveredItem := FLastHoveredItem;
  FMenuAutoOpen := true;
  Changed;
end;

procedure TAdvSmoothMegaMenu.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FLastHoveredItem := FHoveredItem;
  FHoveredItem := -1;
  FMouseLeave := true;
  Changed;
end;

procedure TAdvSmoothMegaMenu.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

constructor TAdvSmoothMegaMenu.Create(AOwner: TComponent);
begin
  FConstructed := false;
  inherited;
  DoubleBuffered := true;
  FBackGroundFill := TGDIPFill.Create;
  FBackGroundFill.OnChange := FillChanged;

  FDefaultSections := TGDIPMenuSections.Create(Self, nil);
  FDefaultSection := FDefaultSections.Add;

  FDefaultTopLayerItems := TGDIPMenuTopLayerItems.Create(Self, nil);
  FDefaultTopLayerItem := FDefaultTopLayerItems.Add;

  FMenuItems := TAdvSmoothMegaMenuItems.Create(Self);
  FMenuItems.OnChange := MenuItemsChanged;
  FAppearance := TAdvSmoothMegaMenuAppearance.Create(Self);
  FAppearance.OnChange := AppearanceChanged;
  FDirection := mdHorizontal;
  Width := 500;
  Height := 35;
  FTransparent := false;
  FHoveredItem := -1;
  FSelectedItem := -1;
  FLastHoveredItem := -1;
  FFocusedItem := 0;
  FAutoOpenMenus := True;
  FOpenMenusOnClick := false;
  FAutoSize := asControlSize;
  FAllowSelection := true;
  FDefaultSectionItemAppearance := TGDIPMenuSectionItemAppearance.Create(nil);
  FDefaultMenuTearOffFill := TGDIPFill.Create;
  FDefaultMenuContentFill := TGDIPFill.Create;

  FPersistSelection := true;
  FTextRendering := tClearType;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

end;

procedure TAdvSmoothMegaMenu.CreateWnd;
begin
  inherited;
  if FConstructed then
    Exit;

  if FDesignTime then
    InitPreview;

  FConstructed := true;
end;

destructor TAdvSmoothMegaMenu.Destroy;
begin
  FDefaultSectionItemAppearance.Free;
  FDefaultMenuTearOffFill.Free;
  FDefaultMenuContentFill.Free;
  FAppearance.Free;
  FMenuItems.Free;
  FDefaultSection.Free;
  FDefaultSection := nil;
  FDefaultSections.Free;
  FDefaultTopLayerItem.Free;
  FDefaultTopLayerItem := nil;
  FDefaultTopLayerItems.Free;
  FBackGroundFill.Free;
  inherited;
end;

procedure TAdvSmoothMegaMenu.DoCloseUp(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(OnMenuItemCloseUp) then
    OnMenuItemCloseUp(Sender, ItemIndex);
end;

procedure TAdvSmoothMegaMenu.DoDropDown(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(OnMenuItemDropDown) then
    OnMenuItemDropDown(Sender, ItemIndex);
end;

procedure TAdvSmoothMegaMenu.DoEnter;
begin
  inherited;
  SetFocus;
  FFocused := true;
  FFocusedItem := 0;
  Changed;
end;

procedure TAdvSmoothMegaMenu.DoExit;
var
  i: integer;
begin
  inherited;
  FLastHoveredItem := -1;
  FHoveredItem := -1;
  FMouseLeave := true;
  FFocused := false;
  FFocusedItem := -1;
  for I := 0 to MenuItems.Count - 1 do
    MenuItems[I].CloseMenu;
  Changed;
end;

procedure TAdvSmoothMegaMenu.DoItemClick(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Sender, ItemIndex);
end;

procedure TAdvSmoothMegaMenu.DoItemEnter(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(FOnItemEnter) then
    FOnItemEnter(Sender, ItemIndex);
end;

procedure TAdvSmoothMegaMenu.DoItemLeave(Sender: TObject; ItemIndex: integer);
begin
  if Assigned(FOnItemLeave) then
    FOnItemLeave(Sender, ItemIndex);
end;

procedure TAdvSmoothMegaMenu.DoMenuSubItemAnchor(Sender: TObject;
  Item: TGDIPMenuSectionItem; Anchor: String);
begin
  if Assigned(OnMenuSubItemAnchorClick) then
    OnMenuSubItemAnchorClick(Sender, Self, GetMenuItemFromSubMenu(Item), Item, Anchor);
end;

procedure TAdvSmoothMegaMenu.DoMenuSubItemCheckChanged(Sender: TObject;
  item: TGDIPMenuSectionItem);
begin
  if Assigned(OnMenuSubItemCheckChanged) then
    OnMenuSubItemCheckChanged(Sender, Self, GetMenuItemFromSubMenu(Item), item);
end;

procedure TAdvSmoothMegaMenu.DoMenuSubItemClick(Sender: TObject;
  Item: TGDIPMenuSectionItem; Text: String);
var
  i: integer;
begin
  if Assigned(Item) then
  begin
    for I := 0 to MenuItems.Count - 1 do
    begin
      if item.HideOnSelection then
      begin
        if Item.Menu = MenuItems[I].Menu then
        begin
          MenuItems[I].CloseMenu;
          SetFocus;
          FLastHoveredItem := FHoveredItem;
          FHoveredItem := -1;
          FMenuAutoOpen := false;
          FMenuIsOpen := false;
          break;
        end;
      end;
    end;
  end;

  if Assigned(OnMenuSubItemClick) then
    OnMenuSubItemClick(Sender, Self, GetMenuItemFromSubMenu(Item), item, text);

  if Assigned(Item.OnClick) then
    Item.OnClick(Item);
end;

procedure TAdvSmoothMegaMenu.DoMenuSubItemEditChanged(Sender: TObject;
  Text: String; item: TGDIPMenuSectionItem);
begin
  if Assigned(OnMenuSubItemEditChanged) then
    OnMenuSubItemEditChanged(Sender, Self, GetMenuItemFromSubMenu(Item), Text, item);
end;

procedure TAdvSmoothMegaMenu.DoMenuSubItemHover(Sender: TObject;
  Item: TGDIPMenuSectionItem; Text: String);
begin
  if Assigned(OnMenuSubItemHover) then
    OnMenuSubItemHover(Sender, Self, GetMenuItemFromSubMenu(Item), item, text);
end;

procedure TAdvSmoothMegaMenu.DrawBackGround(g: TGPGraphics);
begin
  FBackGroundFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1))
end;

procedure TAdvSmoothMegaMenu.DrawMenuItems(g: TGPGraphics);
var
  i: integer;
begin
  for I := 0 to MenuItems.Count - 1 do
    MenuItems[I].Draw(g);
end;

procedure TAdvSmoothMegaMenu.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothMegaMenu.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothMegaMenu.GetDefaultItemAppearance: TGDIPMenuSectionItemAppearance;
begin
  Result := DefaultSectionItemAppearance;
end;

function TAdvSmoothMegaMenu.GetDefaultSection: TGDIPMenuSection;
begin
  Result := DefaultSection;
end;

function TAdvSmoothMegaMenu.GetDefaultTopLayerItem: TGDIPMenuTopLayerItem;
begin
  Result := DefaultTopLayerItem;
end;

function TAdvSmoothMegaMenu.GetFirstMenu: TGDIPMenu;
begin
  if MenuItems.Count >= 1 then
    Result := MenuItems[0].Menu
  else
    Result := nil;
end;

function TAdvSmoothMegaMenu.GetImageList: TCustomImageList;
begin
  Result := ImageList;
end;

procedure TAdvSmoothMegaMenu.GetLocation(var x, y: Double; rectangle: TGPRectF;
  objectwidth, objectheight: Double; location: TGDIPMenuLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    mlTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    mlTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    mlBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    mlBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    mlTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    mlBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    mlCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    mlCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    mlCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
  end;

  x := x + rectangle.X;
  y := y + rectangle.Y;
end;

function TAdvSmoothMegaMenu.GetMenuItemFromSubMenu(
  Item: TGDIPMenuSectionItem): TAdvSmoothMegaMenuItem;
var
  i: integer;
begin
  result := nil;
  for I := 0 to MenuItems.Count - 1 do
  begin
    if Item.Menu = MenuItems[I].Menu then
    begin
      result := MenuItems[I];
      break;
    end;
  end;
end;

function TAdvSmoothMegaMenu.GetNextMenu(Menu: TGDIPMenu): TGDIPMenu;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to MenuItems.Count - 1 do
  begin
    if (MenuItems[I].Menu = Menu) and (i < MenuItems.Count - 1) then
    begin
      Result := MenuItems[I + 1].Menu;
      break;
    end;
  end;
end;

function TAdvSmoothMegaMenu.GetPictureContainer: TGDIPPictureContainer;
begin
  Result := PictureContainer;
end;

function TAdvSmoothMegaMenu.GetPreviousMenu(Menu: TGDIPMenu): TGDIPMenu;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to MenuItems.Count - 1 do
  begin
    if (MenuItems[I].Menu = Menu) and (i > 0) then
    begin
      Result := MenuItems[I - 1].Menu;
      break;
    end;
  end;
end;

function TAdvSmoothMegaMenu.GetThemeId: String;
begin
  Result := ClassName;
end;

function TAdvSmoothMegaMenu.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothMegaMenu.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothMegaMenu.HasMultipleMenus: Boolean;
begin
  Result := true;
end;

procedure TAdvSmoothMegaMenu.InitItemRects;
var
  I: Integer;
  g: TGPGraphics;
  sizeRect: TGPRectF;
  itemleft, itemtop: Double;
  its: Double;
  cf: TFont;
  r: TRect;
  w, h: Double;
  cnt: integer;
  itl, itr: String;
  itlidx, itridx: integer;
  picl, picr: TAdvGDIPPicture;
  wil, wir: integer;
  s: Double;
  htmlr: TGPRectF;
  fcap: TGDIPMenuHTMLText;
  bmpl, bmpr: TBitmap;
  viscnt: integer;
begin
  if (MenuItems.Count = 0) or (csDestroying in ComponentState) then
    Exit;

  fcap := TGDIPMenuHTMLText.Create(nil);
  fcap.URLColor := ItemAppearance.MenuItemURLColor;
  fcap.ShadowColor := ItemAppearance.MenuItemShadowColor;
  fcap.ShadowOffset := ItemAppearance.MenuItemShadowOffset;


  cf := TFont.Create;
  its := 0;
  r := InsideRect;
  w := r.Right - r.Left - ItemAppearance.Margin.Right - ItemAppearance.Margin.Left;
  h := r.Bottom - r.Top - ItemAppearance.Margin.Bottom - ItemAppearance.Margin.Top;
  itemleft := ItemAppearance.Margin.Left + r.Left;
  itemtop := ItemAppearance.Margin.Top + r.Top;

  cnt := 0;
  for I := 0 to MenuItems.Count - 1 do
  begin
    if MenuItems[I].Separator and MenuItems[I].Visible then
      Inc(cnt);
  end;

  viscnt := 0;
  for I := 0 to MenuItems.Count - 1 do
  begin
    if MenuItems[I].Visible then
      Inc(viscnt);
  end;

  if viscnt = 0 then
    Exit;

  case Direction of
    mdVertical:
    begin
      its := h - (cnt * ItemAppearance.MenuItemSpacing) - (cnt * ItemAppearance.MenuItemSeparatorHeight);
      if (cnt < viscnt + 1) and (viscnt - cnt > 0) then
        its := (its - ((viscnt - 1 - cnt) * ItemAppearance.MenuItemSpacing)) / (viscnt - cnt);

      h := its;
    end;
    mdHorizontal:
    begin
      its := w - (cnt * ItemAppearance.MenuItemSpacing) - (cnt * ItemAppearance.MenuItemSeparatorWidth);
      if (cnt < viscnt + 1) and (viscnt - cnt > 0) then
        its := (its - ((viscnt - 1 - cnt) * ItemAppearance.MenuItemSpacing)) / (viscnt - cnt);

      w := its;
    end;
  end;

  bmpl := TBitmap.Create;
  bmpr := TBitmap.Create;
  for I := 0 to MenuItems.Count - 1 do
  begin
    with MenuItems[I] do
    begin
      if Visible then
      begin
        if Enabled then
        begin
          if (FHoveredItem = Index) and not (FselectedItem = Index) then
          begin
            cf.Assign(ItemAppearance.MenuItemFontHover);
            itl := GraphicLeftHoverName;
            itr := GraphicRightHoverName;
            itlidx := GraphicLeftHoverIndex;
            itridx := GraphicRightHoverIndex;
          end
          else if (FSelectedItem = Index) then
          begin
            cf.Assign(ItemAppearance.MenuItemFontSelected);
            itl := GraphicLeftSelectedName;
            itr := GraphicRightSelectedName;
            itlidx := GraphicLeftSelectedIndex;
            itridx := GraphicRightSelectedIndex;
          end
          else
          begin
            cf.Assign(ItemAppearance.MenuItemFont);
            itl := GraphicLeftName;
            itr := GraphicRightName;
            itlidx := GraphicLeftIndex;
            itridx := GraphicRightIndex;
          end;
        end
        else
        begin
          cf.Assign(ItemAppearance.MenuItemFontDisabled);
          itl := GraphicLeftDisabledName;
          itr := GraphicRightDisabledName;
          itlidx := GraphicLeftDisabledIndex;
          itridx := GraphicRightDisabledIndex;
        end;

        if (GraphicLeftName <> '') and (itl = '') then
          itl := GraphicLeftName;

        if (GraphicRightName <> '') and (itr = '') then
          itr := GraphicRightName;

        if (GraphicLeftIndex > -1) and (itlidx = -1) then
          itlidx := GraphicLeftIndex;

        if (GraphicRightIndex > -1) and (itridx = -1) then
          itridx := GraphicRightIndex;

        if AutoSize = asCaptionSize then
          sizeRect := MakeRect(0, 0, 10000, 10000)
        else
          sizeRect := MakeRect(0, 0, w, h);
        htmlr := MakeRect(0, 0, 0, 0);
        if Fcaption <> '' then
        begin
          g := TGPGraphics.Create(Canvas.Handle);
          case TextRendering of
            tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
            tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
            tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
          end;
          fcap.Text := FCaption;
          DrawMenuItemHTMLText(g, cf, fcap, CaptionLocation, sizeRect, htmlr, FCaption, true);
          g.Free;
        end;

        wir := 0;
        wil := 0;
        sizeRect.Width := htmlr.width;
        sizeRect.Height := htmlr.Width;
        if Assigned(PictureContainer) then
        begin
          picl := PictureContainer.FindPicture(itl);
          if Assigned(picl) and not picl.Empty then
          begin
            picl.GetImageSizes;
            wil := picl.Width + 2;
          end;
          picr := PictureContainer.FindPicture(itr);
          if Assigned(picr) and not picr.Empty then
          begin
            picr.GetImageSizes;
            wir := picr.Width + 2;
          end;
        end;

        if Assigned(ImageList) then
        begin
          if (itlidx >= 0) and (itlidx <= ImageList.Count - 1) then
          begin
            ImageList.GetBitmap(itlidx, bmpl);
            if Assigned(bmpl) and not bmpl.Empty then
            begin
              if bmpl.Width > wil then
                wil := bmpl.Width + 2;
            end;
          end;

          if (itridx >= 0) and (itridx <= ImageList.Count - 1) then
          begin
            ImageList.GetBitmap(itridx, bmpr);
            if Assigned(bmpr) and not bmpr.Empty then
            begin
              if bmpr.Width > wir then
                wir := bmpr.Width + 2;
            end;
          end;
        end;

        case Direction of
          mdVertical:
          begin
            if not Separator then
            begin
              case AutoSize of
                asControlSize:
                begin
                  FItemRect.Y := itemtop;
                  itemtop := itemtop + its + ItemAppearance.MenuItemSpacing;
                  FItemRect.Height := its;
                  FItemRect.X := InsideRect.Left + ItemAppearance.Margin.Left;
                  FItemRect.Width := w;
                end;
                asCaptionSize:
                begin
                  FItemRect.Y := itemtop;
                  itemtop := itemtop + sizeRect.Height + 10 + ItemAppearance.MenuItemSpacing;
                  FItemRect.Height := sizeRect.Height + 10;
                  FItemRect.X := InsideRect.Left + ItemAppearance.Margin.Left;
                  FItemRect.Width := w + wir + wil;
                end;
                asCustom:
                begin
                  FItemRect.Y := itemtop;
                  itemtop := itemtop + FHeight + ItemAppearance.MenuItemSpacing;
                  FItemRect.Height := FHeight;
                  FItemRect.X := InsideRect.Left + ItemAppearance.Margin.Left;
                  FItemRect.Width := w;
                end;
              end;
            end
            else
            begin
              FItemRect.Y := itemtop;
              itemtop := itemtop + ItemAppearance.MenuItemSeparatorHeight  + ItemAppearance.MenuItemSpacing;
              FItemrect.Height := ItemAppearance.MenuItemSeparatorHeight;
              FItemRect.X := InsideRect.Left +  ItemAppearance.Margin.Left +  (w - ItemAppearance.MenuItemSeparatorWidth) / 2;
              FItemRect.Width := ItemAppearance.MenuItemSeparatorWidth;
            end;
          end;
          mdHorizontal:
          begin
            if not Separator then
            begin
              case AutoSize of
                asControlSize:
                begin
                  FItemRect.X := itemleft;
                  itemleft := itemleft + its + ItemAppearance.MenuItemSpacing;
                  FItemRect.Width := its;
                  FItemRect.Y := InsideRect.Top + ItemAppearance.Margin.Top;
                  FItemRect.Height := h;
                end;
                asCaptionSize:
                begin
                  FItemRect.X := itemleft;
                  itemleft := itemleft + sizeRect.Width + 10 + ItemAppearance.MenuItemSpacing + wir + wil;
                  FItemRect.Width := sizeRect.Width + 10 + wir + wil;
                  FItemRect.Y := InsideRect.Top + ItemAppearance.Margin.Top;
                  FItemRect.Height := h;
                end;
                asCustom:
                begin
                  FItemRect.X := itemleft;
                  itemleft := itemleft + FWidth + ItemAppearance.MenuItemSpacing;
                  FItemRect.Width := FWidth;
                  FItemRect.Y := InsideRect.Top + ItemAppearance.Margin.Top;
                  FItemRect.Height := h;
                end;
              end;
            end
            else
            begin
              FItemRect.X := itemleft;
              itemleft := itemleft + ItemAppearance.MenuItemSeparatorWidth + ItemAppearance.MenuItemSpacing;
              FItemrect.Width := ItemAppearance.MenuItemSeparatorWidth;
              FItemRect.Y := InsideRect.Top + ItemAppearance.Margin.Top + (h - ItemAppearance.MenuItemSeparatorHeight) / 2;
              FItemRect.Height := ItemAppearance.MenuItemSeparatorHeight;
            end;
          end;
        end;

        //preset caption rect
        s := sizeRect.Width + 4 + wir;
        s := (FItemRect.Width - s) / 2;
        if Caption <> '' then
          FCaptionRect := MakeRect(FItemRect.X + s + (wil / 2) + 2, FItemRect.Y + 2, sizerect.Width, FItemRect.Height - 4)
        else
          FCaptionRect := MakeRect(FItemRect.X + s + (wil / 2), FItemRect.Y + 2, sizerect.Width, FItemRect.Height - 4)

      end;
    end;
  end;

  bmpl.Free;
  bmpr.Free;

  fcap.Free;
  cf.Free;
end;

procedure TAdvSmoothMegaMenu.InitPreview;
var
  i, c: integer;
begin
  MenuItems.Clear;

  for I := 0 to 5 do
  begin
    with MenuItems.Add do
    begin
      with MenuItems[I].Menu.Sections.Add do
      begin
        Caption := 'Section on MenuItem ' + inttostr(I);
        for C := 0 to 4 do
        begin
          Items.Add.Text := 'Item ' + inttostr(C);
        end;
      end;
    end;
  end;

  SetComponentStyle(FStyle);
end;

function TAdvSmoothMegaMenu.InsideRect: TRect;
var
  sh, bw: integer;
begin
  sh := 0;
  if (BackGroundFill.ShadowColor <> clNone) {and not Transparent} then
    sh := BackGroundFill.ShadowOffset;

  Result := Rect(0, 0, Width, Height);
  // adapt width & height for GDI+ drawing rect

  Result.Right := Result.Right - 1 - sh;
  Result.Bottom := Result.Bottom - 1 - sh;

  if (BackGroundFill.BorderColor <> clNone) {and not Transparent} then
  begin
    if BackGroundFill.BorderWidth = 1 then
      bw := 1
    else
      bw := (BackGroundFill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;
end;

procedure TAdvSmoothMegaMenu.KeyDown(var Key: Word; Shift: TShiftState);
var
  k, i, c: integer;
  item: TCurrentItem;
begin
  inherited;
  case Direction of
    mdVertical:
    begin
      case Key of
        VK_DOWN:
        begin
          if FFocusedItem < MenuItems.Count - 1 then
            inc(FFocusedItem)
          else
            FFocusedItem := 0;

          for I := 0 to MenuItems.Count - 1 do
            MenuItems[I].CloseMenu;

          if (AutoOpenMenus and FMenuAutoOpen) then
          begin
            ShowItemForm(FFocusedItem);
            FHoveredItem := FFocusedItem;
            Paint;
          end;
        end;
        VK_UP:
        begin
          if FHoveredItem > 0 then
            dec(FFocusedItem)
          else
            FFocusedItem  := MenuItems.Count - 1;

          for I := 0 to MenuItems.Count - 1 do
            MenuItems[I].CloseMenu;

          if (AutoOpenMenus and FMenuAutoOpen) then
          begin
            ShowItemForm(FFocusedItem);
            FHoveredItem := FFocusedItem;
            Paint;
          end;
        end;
        VK_F4, VK_LEFT, VK_RIGHT, VK_RETURN:
        begin
          if (FFocusedItem >= 0) and (FFocusedItem <= MenuItems.Count - 1) then
          begin
            if not Assigned(MenuItems[FFocusedItem].frm) then
            begin
              ShowItemForm(FFocusedItem);
              FHoveredItem := FFocusedItem;
              FMenuAutoOpen := true;
            end;
          end;
        end;
      end;
    end;
    mdHorizontal:
    begin
      case Key of
        VK_RIGHT:
        begin
          if FFocusedItem < MenuItems.Count - 1 then
            inc(FFocusedItem)
          else
            FFocusedItem := 0;

          for I := 0 to MenuItems.Count - 1 do
            MenuItems[I].CloseMenu;

          if (AutoOpenMenus and FMenuAutoOpen) then
          begin
            ShowItemForm(FFocusedItem);
            FHoveredItem := FFocusedItem;
            Paint;
          end;
        end;
        VK_LEFT:
        begin
          if FFocusedItem > 0 then
            dec(FFocusedItem)
          else
            FFocusedItem  := MenuItems.Count - 1;

          for I := 0 to MenuItems.Count - 1 do
            MenuItems[I].CloseMenu;

          if (AutoOpenMenus and FMenuAutoOpen) then
          begin
            ShowItemForm(FFocusedItem);
            FHoveredItem := FFocusedItem;
            Paint;
          end;
        end;
        VK_F4, VK_UP, VK_DOWN, VK_RETURN:
        begin
          if ((Key = VK_F4) and not (ssAlt in Shift)) or (Key <> VK_F4) then
          begin
            if (FFocusedItem >= 0) and (FFocusedItem <= MenuItems.Count - 1) then
            begin
              if not Assigned(MenuItems[FFocusedItem].frm) then
              begin
                ShowItemForm(FFocusedItem);
                FHoveredItem := FFocusedItem;
                FMenuAutoOpen := true;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  case Key of
  VK_ESCAPE, VK_F4:
  begin
    if ((Key = VK_F4) and (ssAlt in Shift)) or (Key <> VK_F4) then
    begin
      if (FFocusedItem >= 0) and (FFocusedItem <= MenuItems.Count - 1) then
      begin
        MenuItems[FFocusedItem].CloseMenu;
        FHoveredItem := -1;
        FMenuAutoOpen := false;
      end;
    end;
  end;
  VK_SPACE: //ESC
    begin
      if FFocusedItem <> -1 then
      begin
        FSelectedItem := FFocusedItem;
        if Assigned(MenuItems[FSelectedItem].frm) and MenuItems[FSelectedItem].IsFloating then
        begin
          MenuItems[FSelectedItem].FFloating := false;
          MenuItems[FSelectedItem].frm.SetPosition;
          if Assigned(OnMenuItemFloatChanged) then
            OnMenuItemFloatChanged(Self, false, MenuItems[FSelectedItem]);
          MenuItems[FSelectedItem].frm.FFloatingEvent := false;
        end;

        if (FSelectedItem <> -1) and Assigned(FOnItemClick) then
          FOnItemClick(Self, FSelectedItem);
      end;
    end;
  end;


  for C := 0 to MenuItems.Count - 1 do
  begin
    for I := 0 to MenuItems[C].Menu.Sections.Count - 1 do
    begin
      if MenuItems[C].Menu.Sections[I].Visible then
      begin
        with MenuItems[C].Menu.Sections[I] do
        begin
          for K := 0 to Items.Count - 1 do
          begin
            if ShortCut(Key, Shift) = Items[K].ShortCut then
            begin
              item.item := Items[K];
              item.section := MenuItems[C].Menu.Sections[I];
              MenuItems[C].Menu.SelectedItem := item;
              if Assigned(OnMenuSubItemClick) then
                OnMenuSubItemClick(Self, Self, GetMenuItemFromSubMenu(Items[K]), Items[K], Items[K].Text);

              if Assigned(Items[K].OnClick) then
                Items[K].OnClick(Items[K]);

              Changed;
              break;
            end;
          end;
        end;
      end;
    end;
  end;

  Changed;
end;

procedure TAdvSmoothMegaMenu.LoadFromFile(Filename: String; ApplyDefaultStyles: Boolean = true);
var
  ini: TIniFile;
begin
  ini := TInifile.Create(Filename);

  ItemAppearance.LoadFromFile(ini, GetThemeID +'.ItemAppearance');
  BackGroundfill.LoadFromFile(ini, GetThemeID +'.BackGroundFill');
  DefaultSectionItemAppearance.LoadFromFile(ini, GetThemeID +'.DefaultSectionItemAppearance');
  DefaultSection.LoadFromFile(ini, GetThemeID +'.DefaultSection');
  DefaultTopLayerItem.LoadFromFile(ini, GetThemeID +'.DefaultTopLayerItem');
  DefaultMenuContentFill.LoadFromFile(ini, GetThemeID +'.DefaultMenuContentFill');
  DefaultMenuTearOffFill.LoadFromFile(ini, GetThemeID +'.DefaultMenuTearOffFill');

  if ApplyDefaultStyles then
    ApplyDFstyle;

  ini.Free;
  Changed;
end;

procedure TAdvSmoothMegaMenu.LoadFromTheme(FileName: String);
begin
  LoadFromFile(FileName, true);
end;

procedure TAdvSmoothMegaMenu.MenuItemsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenu.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  it: integer;
begin
  inherited;
  it := XYToItem(X, Y);
  if (it <> -1) then
  begin
    FSelectedItem := it;
    if Assigned(OnMenuItemMouseDown) then
      OnMenuItemMouseDown(Self, Button, Shift, X, Y, FSelectedItem);
    if not SelectOnly then
      DoItemClick(self, FSelectedItem);

    if SelectOnly then
    begin
      Changed;
      Exit;
    end;

    if (it >= 0) and (it <= MenuItems.Count - 1) then
    begin
      if Assigned(MenuItems[it].frm) and MenuItems[it].IsFloating then
      begin
        MenuItems[it].FFloating := false;
        MenuItems[it].frm.SetPosition;
        if Assigned(OnMenuItemFloatChanged) then
          OnMenuItemFloatChanged(Self, false, MenuItems[it]);
        MenuItems[it].frm.FFloatingEvent := false;
      end
      else if Assigned(MenuItems[it].frm) then
      begin
        MenuItems[it].CloseMenu;
        FMenuAutoOpen := false;
        FMenuIsOpen := false;
      end
      else
      begin
        FMenuAutoOpen := true;
        FMenuIsOpen := true;
        ShowItemForm(it);
      end;
    end;
  end;
  Changed;
end;

procedure TAdvSmoothMegaMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  it: integer;
  item: TAdvSmoothMegaMenuItem;
  chk: Boolean;
  I: Integer;
begin
  inherited;
  if (FPrevX <> X) or (FPrevY <> Y) then
  begin
    FPrevX := X;
    FPrevY := Y;
    chk := Application.Active;
    if not chk then
      Exit;

    it := XYToItem(X, Y);
    if (it <> -1) then
    begin
      if Assigned(OnMenuItemMouseMove) then
        OnMenuItemMouseMove(Self, Shift, X, Y, it);
      item := MenuItems[it];
      if FMouseLeave then
      begin
        chk := chk and (FHoveredItem <> it) and (it <> FLastHoveredItem);
        FMouseLeave := false;
      end
      else
        chk := chk and (FHoveredItem <> it);

      if OpenMenusOnClick then
        chk := chk and AutoOpenMenus and FMenuAutoOpen and FMenuIsOpen
      else
        chk := chk and AutoOpenMenus and FMenuAutoOpen;

      FHoveredItem := item.Index;
      FFocusedItem := FHoveredItem;

      MouseLeave := true;
      if MouseEnter then
      begin
        DoItemEnter(Self, FHoveredItem);
        MouseEnter := false;
      end;

      Cursor := crHandPoint;
      Changed;

      if chk then
      begin
        for I := 0 to MenuItems.Count - 1 do
          MenuItems[I].CloseMenu;

        ShowItemForm(it);
      end;
    end
    else
    begin
      MouseEnter := true;
      if MouseLeave then
      begin
        DoItemLeave(Self, FHoveredItem);
        MouseLeave := false;
      end;

      Changed;
      Cursor := crArrow;
    end;
  end;
end;

procedure TAdvSmoothMegaMenu.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
    if Assigned(OnMenuItemMouseUp) and (FSelectedItem <> -1) then
      OnMenuItemMouseUp(Self, Button, Shift, X, Y, FSelectedItem);
  if not PersistSelection then
  begin
    FSelectedItem := -1;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i, k, c: integer;
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FPictureContainer) then
    FPictureContainer := nil;

  if (AOperation = opRemove) and (AComponent = FImageList) then
    FImageList := nil;

  inherited;
  if (csDestroying in ComponentState) then
    Exit;

  if (AOperation = opRemove) then
  begin
    for I := 0 to MenuItems.Count - 1 do
    begin
      with MenuItems[I] do
      begin
        for K := 0 to Menu.Sections.Count - 1 do
        begin
          for C := 0 to Menu.Sections[K].Items.Count - 1 do
          begin
            if (AComponent = Menu.Sections[K].Items[C].Control) then
              Menu.Sections[K].Items[C].Control := nil;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenu.Paint;
var
  g: TGPGraphics;
  bmp: TGPBitmap;
begin
  bmp := TGPBitmap.Create(Width, Height);
  g := TGPGraphics.Create(bmp);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  case TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;
  if not Transparent then
    DrawBackGround(g);

  DrawMenuItems(g);

  g.DrawImage(bmp, 0, 0);
  g.Free;

  g := TGPGraphics.Create(Canvas.Handle);
  g.DrawImage(bmp, 0, 0);
  bmp.Free;
  g.Free;
end;

procedure TAdvSmoothMegaMenu.Resize;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothMegaMenu.SaveToFile(Filename: String);
var
  ini: TIniFile;
begin
  ini := TInifile.Create(Filename);

  ItemAppearance.SaveToFile(ini, GetThemeID +'.ItemAppearance');
  BackGroundfill.SaveToFile(ini, GetThemeID +'.BackGroundFill');
  DefaultSectionItemAppearance.SaveToFile(ini, GetThemeID +'.DefaultSectionItemAppearance');
  DefaultSection.SaveToFile(ini, GetThemeID +'.DefaultSection');
  DefaultTopLayerItem.SaveToFile(ini, GetThemeID +'.DefaultTopLayerItem');
  DefaultMenuContentFill.SaveToFile(ini, GetThemeID +'.DefaultMenuContentFill');
  DefaultMenuTearOffFill.SaveToFile(ini, GetThemeID +'.DefaultMenuTearOffFill');
  ini.Free;
end;

procedure TAdvSmoothMegaMenu.SaveToTheme(FileName: String);
begin
  SaveToFile(FileName);
end;

procedure TAdvSmoothMegaMenu.SetAllowSelection(const Value: Boolean);
begin
  if FAllowSelection <> value then
  begin
    FAllowSelection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetAppearance(
  const Value: TAdvSmoothMegaMenuAppearance);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetAS(
  const Value: TAdvSmoothMegaMenuAutoSize);
begin
  if FAutoSize <> value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetAutoOpenMenus(const Value: Boolean);
begin
  if FAutoOpenMenus <> Value then
  begin
    FAutoOpenMenus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetBackGroundFill(const Value: TGDIPFill);
begin
  if FBackGroundFill <> value then
  begin
    FBackGroundFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  BackGroundFill.Color := ATones.Background.BrushColor;
  BackGroundFill.ColorTo := ATones.Background.BrushColor;
  BackGroundFill.BorderColor := ATones.Background.BorderColor;

  ItemAppearance.MenuItemFill.Color := ATones.Background.BrushColor;
  ItemAppearance.MenuItemFill.ColorTo := ATones.Background.BrushColor;
  ItemAppearance.MenuItemFill.ColorMirror := ATones.Background.BrushColor;
  ItemAppearance.MenuItemFill.ColorMirrorTo := ATones.Background.BrushColor;
  ItemAppearance.MenuItemFill.BorderColor := ATones.Background.BorderColor;
  ItemAppearance.MenuItemFill.GlowGradientColor:= clNone;

  ItemAppearance.MenuItemFillHover.Color := ATones.Hover.BrushColor;
  ItemAppearance.MenuItemFillHover.ColorTo :=  ATones.Hover.BrushColor;
  ItemAppearance.MenuItemFillHover.BorderColor :=  ATones.Hover.BorderColor;
  ItemAppearance.MenuItemFillHover.ColorMirror := ATones.Hover.BrushColor;
  ItemAppearance.MenuItemFillHover.ColorMirrorTo := ATones.Hover.BrushColor;
  ItemAppearance.MenuItemFillHover.GlowGradientColor:= clNone;

  ItemAppearance.MenuItemFillSelected.Color := ATones.Selected.BrushColor;
  ItemAppearance.MenuItemFillSelected.ColorTo := ATones.Selected.BrushColor;
  ItemAppearance.MenuItemFillSelected.ColorMirror := ATones.Selected.BrushColor;
  ItemAppearance.MenuItemFillSelected.ColorMirrorTo := ATones.Selected.BrushColor;
  ItemAppearance.MenuItemFillSelected.BorderColor := ATones.Selected.BorderColor;
  ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;


  ItemAppearance.MenuItemFillDisabled.Color := ATones.Disabled.BrushColor;
  ItemAppearance.MenuItemFillDisabled.ColorTo := ATones.Disabled.BrushColor;
  ItemAppearance.MenuItemFillDisabled.ColorMirror := ATones.Disabled.BrushColor;
  ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := ATones.Disabled.BrushColor;
    ItemAppearance.MenuItemFillDisabled.BorderColor := ATones.Disabled.BorderColor;

  ItemAppearance.MenuItemFont.Color := ATones.Background.TextColor ;
  ItemAppearance.MenuItemFontHover.Color := ATones.Hover.TextColor;
  ItemAppearance.MenuItemFontSelected.Color := ATones.Selected.TextColor;
  ItemAppearance.MenuItemFontDisabled.Color := ATones.Disabled.TextColor;



  DefaultMenuContentFill.Color := ATones.Background.BrushColor;
  DefaultMenuContentFill.ColorTo := ATones.Background.BrushColor;
  DefaultMenuContentFill.BorderColor := ATones.Background.BorderColor;

  DefaultSectionItemAppearance.FillHover.Color := ATones.Hover.BrushColor;
  DefaultSectionItemAppearance.FillHover.ColorTo := ATones.Hover.BrushColor;
  DefaultSectionItemAppearance.FillHover.ColorMirror := ATones.Hover.BrushColor;
  DefaultSectionItemAppearance.FillHover.ColorMirrorTo := ATones.Hover.BrushColor;
  DefaultSectionItemAppearance.FillHover.BorderColor :=  ATones.Hover.BorderColor;

  DefaultSectionItemAppearance.FillSelected.Color := ATones.Selected.BrushColor;
  DefaultSectionItemAppearance.FillSelected.ColorTo := ATones.Selected.BrushColor;
  DefaultSectionItemAppearance.FillSelected.ColorMirror := ATones.Selected.BrushColor;
  DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := ATones.Selected.BrushColor;
  DefaultSectionItemAppearance.FillSelected.BorderColor := ATones.Selected.BorderColor;


  DefaultSectionItemAppearance.FillDisabled.Color := ATones.Disabled.BrushColor;
  DefaultSectionItemAppearance.FillDisabled.ColorTo := ATones.Disabled.BrushColor;
  DefaultSectionItemAppearance.FillDisabled.ColorMirror := ATones.Disabled.BrushColor;
  DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := ATones.Disabled.BrushColor;
  DefaultSectionItemAppearance.FillDisabled.BorderColor := ATones.Disabled.BorderColor;

  DefaultSectionItemAppearance.FontDisabled.Color := ATones.Disabled.TextColor;
  DefaultSectionItemAppearance.FontSelected.Color := ATones.Selected.TextColor;
  DefaultSectionItemAppearance.FontHover.Color := ATones.Hover.TextColor;
  DefaultSectionItemAppearance.Font.Color := ATones.Background.TextColor;


  DefaultMenuTearOffFill.Color := ATones.Foreground.BrushColor;
  DefaultMenuTearOffFill.ColorTo := ATones.Foreground.BrushColor;
  DefaultMenuTearOffFill.BorderColor := ATones.Foreground.BorderColor;

  DefaultSection.SetColorTones(ATones);

  for I := 0 to MenuItems.Count - 1 do
    MenuItems[I].Menu.SetColorTones(ATones);
end;

procedure TAdvSmoothMegaMenu.SetComponentStyle(AStyle: TTMSStyle);
var
  I: Integer;
begin
  FTMSStyle := AStyle;
    ItemAppearance.MenuItemFont.Color:= clBlack;
    ItemAppearance.MenuItemFontSelected.Color := clBlack;
    ItemAppearance.MenuItemFontHover.color := clBlack;

    ItemAppearance.MenuItemFill.Glow := gmNone;
    ItemAppearance.MenuItemFillSelected.Glow := gmNone;
    ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clWhite;
    ItemAppearance.MenuItemFillHover.Glow := gmNone;
    ItemAppearance.MenuItemFillHover.GlowGradientColor:= clWhite;

  Fstyle := AStyle;
  // TODO : do color settings here
 case AStyle of

    tsOffice2003Blue:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $EEDBC8;
        ItemAppearance.MenuItemFill.ColorTo := $F6DDC9;
        ItemAppearance.MenuItemFill.ColorMirror := $EDD4C0;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $F7E1D0;
        ItemAppearance.MenuItemFill.BorderColor := $E0B99B;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $962D00;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver:
      begin
        BackGroundFill.Color := $00E6D8D8;
        BackGroundFill.ColorTo := $00E6D8D8;
        BackGroundFill.BorderColor := clNone;

//        Header.Fill.Color := $BDA4A5;
//        Header.Fill.ColorTo := $957475;
//        Header.Font.Color := clWhite;
//        Header.Fill.BorderColor := $947C7C;
//
//        Footer.Fill.Color := $BDA4A5;
//        Footer.Fill.ColorTo := $957475;
//        Footer.Font.Color := clWhite;
//        Footer.Fill.BorderColor := $947C7C;

        ItemAppearance.MenuItemFill.Color := $E6E9E2;
        ItemAppearance.MenuItemFill.ColorTo := $00E6D8D8;
        ItemAppearance.MenuItemFill.ColorMirror := $C8B2B3;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $E6E9E2;
        ItemAppearance.MenuItemFill.BorderColor := $927476;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $947C7C;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        BackGroundFill.Color := RGB(225, 234, 185);
        BackGroundFill.ColorTo := RGB(225, 234, 185);
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $CFF0EA;
        ItemAppearance.MenuItemFill.ColorTo := $CFF0EA;
        ItemAppearance.MenuItemFill.ColorMirror := $8CC0B1;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $CFF0EA;
        ItemAppearance.MenuItemFill.BorderColor := $8CC0B1;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $588060;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        BackGroundFill.Color := $00F2F2F2;
        BackGroundFill.ColorTo := $00F2F2F2;
        BackGroundFill.BorderColor := clNone;

//        Header.Fill.Color := $808080;
//        Header.Fill.ColorTo := $808080;
//        Header.Font.Color := clWhite;
//        Header.Fill.BorderColor := $808080;
//
//        Footer.Fill.Color := $808080;
//        Footer.Fill.ColorTo := $808080;
//        Footer.Font.Color := clWhite;
//        Footer.Fill.BorderColor := $808080;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := $C9D1D5;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $8CC0B1;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $D2BDB6;
        ItemAppearance.MenuItemFillHover.ColorTo := $D2BDB6;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor := $808080;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $D8D5D4;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $D8D5D4;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $808080;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $B59285;
        ItemAppearance.MenuItemFillSelected.ColorTo := $B59285;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $808080;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Luna:
      begin
        BackGroundFill.Color := $00F3E5DA;
        BackGroundFill.ColorTo := $00F0DED0;
        BackGroundFill.BorderColor := clNone;

//        Header.Fill.Color := $FFEFE3;
//        Header.Fill.ColorTo := $FFD2AF;
//        Header.Font.Color := $723708;
//        Header.Fill.BorderColor := $00FFD2AF;
//
//        Footer.Fill.Color := $FFEFE3;
//        Footer.Fill.ColorTo := $FFD2AF;
//        Footer.Font.Color := $723708;
//        Footer.Fill.BorderColor := $00FFD2AF;

        ItemAppearance.MenuItemFill.Color := $FFEFE3;
        ItemAppearance.MenuItemFill.ColorTo := $FFDDC4;
        ItemAppearance.MenuItemFill.ColorMirror := $FFD1AD;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $FFDBC0;
        ItemAppearance.MenuItemFill.BorderColor := $FFD1AD;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $FFD1AD;//$00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        BackGroundFill.Color := $5C534C;
        BackGroundFill.ColorTo := $5C534C;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $F9F8F8;
        ItemAppearance.MenuItemFill.ColorTo := $E4E2DF;
        ItemAppearance.MenuItemFill.ColorMirror := $D1CBC7;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $E2DEDB;
        ItemAppearance.MenuItemFill.BorderColor := $D1CBC7;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := clBlack;//$00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        BackGroundFill.Color := $00B6B6B6;
        BackGroundFill.ColorTo := $00B6B6B6;

//        Header.Fill.Color := clBtnFace;
//        Header.Fill.ColorTo := clBtnFace;
//        Header.Font.Color := clBlack;
//        Header.Fill.BorderColor := clBlack;
//
//        Footer.Fill.Color := clBtnFace;
//        Footer.Fill.ColorTo := clBtnFace;
//        Footer.Font.Color := clBlack;
//        Footer.Fill.BorderColor := clBlack;

        ItemAppearance.MenuItemFill.Color := clBtnFace;//clWhite;
        ItemAppearance.MenuItemFill.ColorTo := clBtnFace;//$B9D8DC;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $B9D8DC;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := clBlack;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EFD3C6;
        ItemAppearance.MenuItemFillHover.ColorTo := $EFD3C6;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  clHighlight;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := clInactiveCaption;
        ItemAppearance.MenuItemFillSelected.ColorTo := clInactiveCaption;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := clHighLight;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        BackGroundFill.Color := $F5F9FA;
        BackGroundFill.ColorTo := $F5F9FA;
        BackGroundFill.BorderColor := clNone;

//        Header.Fill.Color := $EBEEEF;
//        Header.Fill.ColorTo := $7E9898;
//        Header.Font.Color := clWhite;
//        Header.Fill.BorderColor := $962D00;
//
//        Footer.Fill.Color := $EBEEEF;
//        Footer.Fill.ColorTo := $7E9898;
//        Footer.Font.Color := clWhite;
//        Footer.Fill.BorderColor := $962D00;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := $DFEDF0;
        ItemAppearance.MenuItemFill.ColorMirror := $DFEDF0;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $DFEDF0;
        ItemAppearance.MenuItemFill.BorderColor := $99A8AC;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $962D00;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        BackGroundFill.Color := RGB(241, 244, 248);
        BackGroundFill.ColorTo := RGB(227, 232, 240);
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $F9F8F8;
        ItemAppearance.MenuItemFill.ColorTo := $E4E2DF;
        ItemAppearance.MenuItemFill.ColorMirror := $D1CBC7;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $E2DEDB;
        ItemAppearance.MenuItemFill.BorderColor := $D1CBC7;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $EBFDFF;
        ItemAppearance.MenuItemFillHover.ColorTo := $ACECFF;
        ItemAppearance.MenuItemFillHover.ColorMirror := $59DAFF;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $A4E9FF;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $99CEDB;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $AAD9FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := $6EBBFF;
        ItemAppearance.MenuItemFillSelected.ColorMirror := $42AEFE;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := $7AE1FE;
        ItemAppearance.MenuItemFillSelected.BorderColor := $42AEFE;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := $00F2F2F2;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := clBlack;//$00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $FFFFFF;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorTo := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorMirror := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $FFFFFF;
        ItemAppearance.MenuItemFill.BorderColor := $FCF2DA;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $588060;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $FFFDF9;
        ItemAppearance.MenuItemFillHover.ColorTo := $FFFAF0;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $FCF2DA;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $FEF9F0;
        ItemAppearance.MenuItemFillSelected.ColorTo := $FDF0D7;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $FEDF9A;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsWindows7:
      begin
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $FFFFFF;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorTo := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorMirror := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorMirrorTo := $FFFFFF;
        ItemAppearance.MenuItemFill.BorderColor := $FCF2DA;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
//        ItemAppearance.MenuItemFillDisabled.BorderColor := $588060;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $FDFBFA;
        ItemAppearance.MenuItemFillHover.ColorTo := $FDF3EB;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $FBD6B8;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $FCEBDC;
        ItemAppearance.MenuItemFillSelected.ColorTo := $FCDBC1;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $CEA27D;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        BackGroundFill.Color := clBtnFace;
        BackGroundFill.ColorTo := clBtnFace;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := clBtnFace;
        ItemAppearance.MenuItemFill.ColorTo := clBtnFace;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := clGray;
        ItemAppearance.MenuItemFont.Color := clBlack;


        ItemAppearance.MenuItemFillDisabled.Color := clBtnFace;
        ItemAppearance.MenuItemFillDisabled.ColorTo := clBtnFace;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clWhite;


        ItemAppearance.MenuItemFillHover.Color := clSilver;
        ItemAppearance.MenuItemFillHover.ColorTo := clSilver;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  clGray;


        ItemAppearance.MenuItemFillSelected.Color := clHighLight;
        ItemAppearance.MenuItemFillSelected.ColorTo := clHighLight;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := clGray;
        ItemAppearance.MenuItemFontSelected.Color:= clWhite;

      end;
         tsOffice2010Blue:
      begin
        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;
        BackGroundFill.Color := $FDF6EF;
        BackGroundFill.ColorTo := $F0DAC7;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $FDF6EF;
        ItemAppearance.MenuItemFill.ColorTo := $F0DAC7;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $C7B29F;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $8AE3FD;
        ItemAppearance.MenuItemFillHover.ColorTo :=  clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $58CAF1;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.GradientType := gtVertical;
        ItemAppearance.MenuItemFillHover.Glow:= gmGradient;
        ItemAppearance.MenuItemFillHover.GlowGradientColor:= $D9F9FD;

        ItemAppearance.MenuItemFillSelected.Color := $6CD0FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $308AC2;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $7BEEFF;


        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

      end;
         tsOffice2010Silver:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $EDE5E0;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $FFFFFF;
        ItemAppearance.MenuItemFill.ColorTo := $EDE5E0;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $D2CDC8;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $8AE3FD;
        ItemAppearance.MenuItemFillHover.ColorTo :=  clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $58CAF1;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.GradientType := gtVertical;
        ItemAppearance.MenuItemFillHover.Glow:= gmGradient;
        ItemAppearance.MenuItemFillHover.GlowGradientColor:= $D9F9FD;

        ItemAppearance.MenuItemFillSelected.Color := $6CD0FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $308AC2;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $7BEEFF;


        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;
      end;
         tsOffice2010Black:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $67BCF6;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;

        BackGroundFill.Color := $BFBFBF;
        BackGroundFill.ColorTo := $919191;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $BFBFBF;
        ItemAppearance.MenuItemFill.ColorTo := $919191;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $6D6D6D;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillHover.Color := $8AE3FD;
        ItemAppearance.MenuItemFillHover.ColorTo :=  clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $58CAF1;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.GradientType := gtVertical;
        ItemAppearance.MenuItemFillHover.Glow:= gmGradient;
        ItemAppearance.MenuItemFillHover.GlowGradientColor:= $D9F9FD;

        ItemAppearance.MenuItemFillSelected.Color := $6CD0FF;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $308AC2;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $7BEEFF;


        ItemAppearance.MenuItemFillDisabled.Color := $00F2F2F2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $00B6B6B6;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFontDisabled.Color := clGray;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;
      end;
    tsWindows8, tsWindows10:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := clWhite;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $E4E3E2;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $F7F7F7;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $F7F7F7;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $DEDEDE;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $F7E0C9;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $E4A262;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $F7E0C9;

        ItemAppearance.MenuItemFillHover.Color := $F7EFE8;
        ItemAppearance.MenuItemFillHover.ColorTo := $F7EFE8;
        ItemAppearance.MenuItemFillHover.ColorMirror := $F7EFE8;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := $F7EFE8;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $F9CEA4;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

      end;
    tsOffice2013White:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := clNone;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $D4D4D4;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $EEEEEE;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $ACACAC;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $FCE2C8;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $E59D56;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $FCE2C8;

        ItemAppearance.MenuItemFillHover.Color := $FCF0E4;
        ItemAppearance.MenuItemFillHover.ColorTo := $FCF0E4;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $EAB47E;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;
      end;

    tsOffice2013LightGray:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := clNone;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $D4D4D4;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $EEEEEE;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $ACACAC;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $FCE2C8;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $E59D56;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $FCE2C8;


        ItemAppearance.MenuItemFillHover.Color := $FCF0E4;
        ItemAppearance.MenuItemFillHover.ColorTo := $FCF0E4;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $EAB47E;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

      end;

    tsOffice2013Gray:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmGradient;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := clNone;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $D4D4D4;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $EEEEEE;
        ItemAppearance.MenuItemFillDisabled.ColorTo := $EEEEEE;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $ACACAC;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $FCE2C8;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $E59D56;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmGradient;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= $FCE2C8;

        ItemAppearance.MenuItemFillHover.Color := $FCF0E4;
        ItemAppearance.MenuItemFillHover.ColorTo := $FCF0E4;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $EAB47E;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;
      end;

  tsOffice2016White:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmNone;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmNone;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := clWhite;
        ItemAppearance.MenuItemFill.ColorTo := clNone;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $D4D4D4;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := clWhite;
        ItemAppearance.MenuItemFillDisabled.ColorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $D4D4D4;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $E3BDA3;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $E3BDA3;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmNone;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;

        ItemAppearance.MenuItemFillHover.Color := $F2E1D5;
        ItemAppearance.MenuItemFillHover.ColorTo := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $F2E1D5;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Gray:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmNone;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmNone;

        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $B2B2B2;
        ItemAppearance.MenuItemFill.ColorTo := clNone;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $444444;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillDisabled.Color := $B2B2B2;
        ItemAppearance.MenuItemFillDisabled.ColorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $444444;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $E3BDA3;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $E3BDA3;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmNone;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;

        ItemAppearance.MenuItemFillHover.Color := $F2E1D5;
        ItemAppearance.MenuItemFillHover.ColorTo := $F2E1D5;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $F2E1D5;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;

      end;

    tsOffice2016Black:
      begin

        ItemAppearance.MenuItemFillSelected.Glow := gmNone;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;
        ItemAppearance.MenuItemFillHover.Glow := gmNone;

        ItemAppearance.MenuItemFont.Color:= $DADADA;
        ItemAppearance.MenuItemFontSelected.Color := $FFFFFF;
        ItemAppearance.MenuItemFontHover.color := $FFFFFF;

        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := clNone;
        BackGroundFill.BorderColor := clNone;

        ItemAppearance.MenuItemFill.Color := $363636;
        ItemAppearance.MenuItemFill.ColorTo := clNone;
        ItemAppearance.MenuItemFill.ColorMirror := clNone;
        ItemAppearance.MenuItemFill.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFill.BorderColor := $444444;;
        ItemAppearance.MenuItemFill.GradientMirrorType := gtVertical;


        ItemAppearance.MenuItemFillDisabled.Color := $363636;
        ItemAppearance.MenuItemFillDisabled.ColorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirror := clNone;
        ItemAppearance.MenuItemFillDisabled.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillDisabled.BorderColor := $444444;
        ItemAppearance.MenuItemFillDisabled.GradientMirrorType := gtVertical;

        ItemAppearance.MenuItemFillSelected.Color := $444444;
        ItemAppearance.MenuItemFillSelected.ColorTo := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirror := clNone;
        ItemAppearance.MenuItemFillSelected.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillSelected.BorderColor := $444444;
        ItemAppearance.MenuItemFillSelected.GradientMirrorType := gtVertical;
        ItemAppearance.MenuItemFillSelected.Glow:= gmNone;
        ItemAppearance.MenuItemFillSelected.GlowGradientColor:= clNone;

        ItemAppearance.MenuItemFillHover.Color := $6A6A6A;
        ItemAppearance.MenuItemFillHover.ColorTo := $6A6A6A;
        ItemAppearance.MenuItemFillHover.ColorMirror := clNone;
        ItemAppearance.MenuItemFillHover.ColorMirrorTo := clNone;
        ItemAppearance.MenuItemFillHover.BorderColor :=  $6A6A6A;
        ItemAppearance.MenuItemFillHover.GradientMirrorType := gtVertical;
      end;

  end;

  DefaultSectionItemAppearance.Font.Color := clBlack;
  DefaultSectionItemAppearance.FontHover.Color := clBlack;
  DefaultSectionItemAppearance.FontSelected.Color := clBlack;

  DefaultSectionItemAppearance.Fill.Glow := gmNone;
  DefaultSectionItemAppearance.FillSelected.Glow := gmNone;
  DefaultSectionItemAppearance.FillHover.Glow := gmNone;
  DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clWhite;
  // TODO : do color settings here
 case AStyle of
    tsOffice2003Blue:
      begin
        DefaultMenuContentFill.Color := $00FFD2AF;
        DefaultMenuContentFill.ColorTo := $00FFD2AF;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver:
      begin
        DefaultMenuContentFill.Color := $00E6D8D8;
        DefaultMenuContentFill.ColorTo := $00E6D8D8;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.fontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        DefaultMenuContentFill.Color := RGB(225, 234, 185);
        DefaultMenuContentFill.ColorTo := RGB(225, 234, 185);
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        DefaultMenuContentFill.Color := $00F2F2F2;
        DefaultMenuContentFill.ColorTo := $00F2F2F2;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $D2BDB6;
        DefaultSectionItemAppearance.FillHover.ColorTo := $D2BDB6;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor := $808080;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $D8D5D4;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $D8D5D4;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $B59285;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $B59285;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $808080;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Luna:
      begin
        DefaultMenuContentFill.Color := $00F3E5DA;
        DefaultMenuContentFill.ColorTo := $00F0DED0;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

      end;
    tsOffice2007Obsidian:
      begin
        DefaultMenuContentFill.Color := $5C534C;
        DefaultMenuContentFill.ColorTo := $5C534C;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.Font.Color := clWhite;
        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;
        DefaultSectionItemAppearance.FontHover.Color := clWhite;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FontSelected.Color := clWhite;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        DefaultMenuContentFill.Color := $00B6B6B6;
        DefaultMenuContentFill.ColorTo := $00B6B6B6;

        DefaultSectionItemAppearance.FillDisabled.Color := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillHover.Color := $EFD3C6;
        DefaultSectionItemAppearance.FillHover.ColorTo := $EFD3C6;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  clHighlight;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := clInactiveCaption;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clInactiveCaption;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := clHighLight;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        DefaultMenuContentFill.Color := $F5F9FA;
        DefaultMenuContentFill.ColorTo := $F5F9FA;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        DefaultMenuContentFill.Color := RGB(241, 244, 248);
        DefaultMenuContentFill.ColorTo := RGB(227, 232, 240);
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $EBFDFF;
        DefaultSectionItemAppearance.FillHover.ColorTo := $ACECFF;
        DefaultSectionItemAppearance.FillHover.ColorMirror := $59DAFF;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := $A4E9FF;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $99CEDB;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $AAD9FF;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $6EBBFF;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := $7AE1FE;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $42AEFE;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := $00F2F2F2;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;
      end;
    tsWindowsVista:
      begin
        DefaultMenuContentFill.Color := $FFFDF9;
        DefaultMenuContentFill.ColorTo := $FFFDF9;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $FFFDF9;
        DefaultSectionItemAppearance.FillHover.ColorTo := $FFFAF0;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $FCF2DA;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $FEF9F0;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $FDF0D7;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $FEDF9A;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;

    tsWindows7:
      begin
        DefaultMenuContentFill.Color := $FFFDF9;
        DefaultMenuContentFill.ColorTo := $FFFDF9;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $FDFBFA;
        DefaultSectionItemAppearance.FillHover.ColorTo := $FDF3EB;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $FBD6B8;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $FCEBDC;
        DefaultSectionItemAppearance.FillSelected.ColorTo := $FCDBC1;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $CEA27D;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
    tsTerminal:
      begin
        DefaultMenuContentFill.Color := clBtnFace;
        DefaultMenuContentFill.ColorTo := clBtnFace;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := clSilver;
        DefaultSectionItemAppearance.FillHover.ColorTo := clSilver;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  clGray;


        DefaultSectionItemAppearance.FillSelected.Color := clHighLight;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clHighLight;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := clGray;
        DefaultSectionItemAppearance.FontSelected.Color:= clWhite;


        DefaultSectionItemAppearance.FillDisabled.Color := clBtnFace;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clBtnFace;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;

      end;
      tsOffice2010Blue:
      begin

        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := $00FFD2AF;
        DefaultMenuContentFill.ColorTo := $00FFD2AF;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := RGB(253, 227, 138);
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := RGB(254, 225, 69);
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := RGB(206, 160, 79);
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
      tsOffice2010Silver:
      begin

        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := $00FFD2AF;
        DefaultMenuContentFill.ColorTo := $00FFD2AF;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := RGB(253, 227, 138);
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := RGB(254, 225, 69);
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := RGB(206, 160, 79);
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
      tsOffice2010Black:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= $67BCF6; //$67C8F6;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := $00FFD2AF;
        DefaultMenuContentFill.ColorTo := $00FFD2AF;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := RGB(253, 227, 138);
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  RGB(242, 205, 96);;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := RGB(254, 225, 69);
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := RGB(206, 160, 79);
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $00F2F2F2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := $00B6B6B6;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
    tsWindows8, tsWindows10:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := clWhite;
        DefaultMenuContentFill.ColorTo := clWhite;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $F7EFE8;
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $F9CEA4;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $F7E0C9;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $E4A262;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $F7F7F7;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;

    tsOffice2013White:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := clWhite;
        DefaultMenuContentFill.ColorTo := clWhite;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $FCF0E4;
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $EAB47E;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $FCE2C8;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $E59D56;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $EEEEEE;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;

    tsOffice2013LightGray:
      begin
       DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := clWhite;
        DefaultMenuContentFill.ColorTo := clWhite;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $FCF0E4;
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $EAB47E;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $FCE2C8;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $E59D56;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $EEEEEE;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;

    tsOffice2013Gray:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := $00FFD2AF;     //todo
        DefaultMenuContentFill.ColorTo := $00FFD2AF;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillHover.Color := $FCF0E4;
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $EAB47E;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $FCE2C8;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $E59D56;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillDisabled.Color := $EEEEEE;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FontDisabled.Color := clGray;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

      end;
  tsOffice2016White:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmNone;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmNone;

        DefaultMenuContentFill.Color := clWhite;
        DefaultMenuContentFill.ColorTo := clNone;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillDisabled.Color := clWhite;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.BorderColor := $D4D4D4;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $E3BDA3;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $E3BDA3;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        DefaultSectionItemAppearance.FillSelected.Glow:= gmNone;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;

        DefaultSectionItemAppearance.FillHover.Color := $F2E1D5;
        DefaultSectionItemAppearance.FillHover.ColorTo := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $F2E1D5;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

      end;

    tsOffice2016Gray:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmNone;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmNone;

        DefaultMenuContentFill.Color := $B2B2B2;
        DefaultMenuContentFill.ColorTo := clNone;
        DefaultMenuContentFill.BorderColor := clNone;


        DefaultSectionItemAppearance.FillDisabled.Color := $B2B2B2;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.BorderColor := $444444;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $E3BDA3;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $E3BDA3;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        DefaultSectionItemAppearance.FillSelected.Glow:= gmNone;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;

        DefaultSectionItemAppearance.FillHover.Color := $F2E1D5;
        DefaultSectionItemAppearance.FillHover.ColorTo := $F2E1D5;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $F2E1D5;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

      end;

    tsOffice2016Black:
      begin
        DefaultSectionItemAppearance.FillSelected.Glow := gmGradient;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;
        DefaultSectionItemAppearance.FillHover.Glow := gmGradient;

        DefaultMenuContentFill.Color := $B2B2B2;
        DefaultMenuContentFill.ColorTo := clNone;
        DefaultMenuContentFill.BorderColor := clNone;

        DefaultSectionItemAppearance.FillDisabled.Color := $363636;
        DefaultSectionItemAppearance.FillDisabled.ColorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillDisabled.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillDisabled.BorderColor := $444444;
        DefaultSectionItemAppearance.FillDisabled.GradientMirrorType := gtVertical;

        DefaultSectionItemAppearance.FillSelected.Color := $444444;
        DefaultSectionItemAppearance.FillSelected.ColorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillSelected.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillSelected.BorderColor := $444444;
        DefaultSectionItemAppearance.FillSelected.GradientMirrorType := gtVertical;
        DefaultSectionItemAppearance.FillSelected.Glow:= gmNone;
        DefaultSectionItemAppearance.FillSelected.GlowGradientColor:= clNone;

        DefaultSectionItemAppearance.FillHover.Color := $6A6A6A;
        DefaultSectionItemAppearance.FillHover.ColorTo := $6A6A6A;
        DefaultSectionItemAppearance.FillHover.ColorMirror := clNone;
        DefaultSectionItemAppearance.FillHover.ColorMirrorTo := clNone;
        DefaultSectionItemAppearance.FillHover.BorderColor :=  $6A6A6A;
        DefaultSectionItemAppearance.FillHover.GradientMirrorType := gtVertical;

      end;


  end;

  DefaultSectionItemAppearance.Fill.Color := clNone;
  DefaultSectionItemAppearance.Fill.BorderColor := clNone;
  DefaultMenuContentFill.BorderColor := clBlack;
  DefaultMenuContentFill.BorderOpacity := 50;

  case AStyle of
    tsOffice2003Blue:
      begin
        DefaultMenuTearOffFill.Color := $D68759;
        DefaultMenuTearOffFill.ColorTo := $933803;
        DefaultMenuTearOffFill.BorderColor := $962D00;
      end;
    tsOffice2003Silver:
      begin
        DefaultMenuTearOffFill.Color := $BDA4A5;
        DefaultMenuTearOffFill.ColorTo := $957475;
        DefaultMenuTearOffFill.BorderColor := $947C7C;
      end;
    tsOffice2003Olive:
      begin
        DefaultMenuTearOffFill.Color := $82C0AF;
        DefaultMenuTearOffFill.ColorTo := $447A63;
        DefaultMenuTearOffFill.BorderColor := $588060;
      end;
    tsOffice2003Classic:
      begin
        DefaultMenuTearOffFill.Color := $808080;
        DefaultMenuTearOffFill.ColorTo := $808080;
        DefaultMenuTearOffFill.BorderColor := $808080;
      end;
    tsOffice2007Luna:
      begin
        DefaultMenuTearOffFill.Color := $FFEFE3;
        DefaultMenuTearOffFill.ColorTo := $FFD2AF;
        DefaultMenuTearOffFill.BorderColor := $00FFD2AF;
      end;
    tsOffice2007Obsidian:
      begin
        DefaultMenuTearOffFill.Color := $F2F1F0;
        DefaultMenuTearOffFill.ColorTo := $C9C2BD;
        DefaultMenuTearOffFill.BorderColor := $5C534C;
      end;
    tsWindowsXP:
      begin
        DefaultMenuTearOffFill.Color := clBtnFace;
        DefaultMenuTearOffFill.ColorTo := clBtnFace;
        DefaultMenuTearOffFill.BorderColor := clBlack;
      end;
    tsWhidbey:
      begin
        DefaultMenuTearOffFill.Color := $EBEEEF;
        DefaultMenuTearOffFill.ColorTo := $7E9898;
        DefaultMenuTearOffFill.BorderColor := $962D00;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        DefaultMenuTearOffFill.Color := $F8F7F6;
        DefaultMenuTearOffFill.ColorTo := $E8E0DB;
        DefaultMenuTearOffFill.BorderColor := $74706F;
      end;
    tsWindowsVista:
      begin
        DefaultMenuTearOffFill.Color := $F8F7F6;  //Test
        DefaultMenuTearOffFill.ColorTo := $E8E0DB;
        DefaultMenuTearOffFill.BorderColor := $74706F;
      end;
    tsWindows7:
      begin
        DefaultMenuTearOffFill.Color := $FCEBDC;  //Test
        DefaultMenuTearOffFill.ColorTo := $FCDBC1;
        DefaultMenuTearOffFill.BorderColor := $CEA27D;
      end;
    tsTerminal:
      begin
        DefaultMenuTearOffFill.Color := clBtnFace;
        DefaultMenuTearOffFill.ColorTo := clBtnFace;
        DefaultMenuTearOffFill.BorderColor := clGray;
      end;
      tsOffice2010Blue:
      begin

        DefaultMenuTearOffFill.Color := $9C8B7B;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;
      tsOffice2010Silver:
      begin
        DefaultMenuTearOffFill.Color := $96908A;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;
      tsOffice2010Black:
      begin
        DefaultMenuTearOffFill.Color := $444444;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;
    tsWindows8, tsWindows10:
      begin
        DefaultMenuTearOffFill.Color := $DAA026;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;

    tsOffice2013White:
      begin
        DefaultMenuTearOffFill.Color := $FF9933;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;

    tsOffice2013LightGray:
      begin
        DefaultMenuTearOffFill.Color := $FF9933;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;

    tsOffice2013Gray:
      begin
        DefaultMenuTearOffFill.Color := $FF9933;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;
    tsOffice2016White:
      begin
        DefaultMenuTearOffFill.Color := $F2D5C2;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;

    tsOffice2016Gray:
      begin
        DefaultMenuTearOffFill.Color := $F2D5C2;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;

    tsOffice2016Black:
      begin
        DefaultMenuTearOffFill.Color := $575757;
        DefaultMenuTearOffFill.ColorTo := clNone;
        DefaultMenuTearOffFill.BorderColor := clNone;
      end;

  end;

  DefaultSection.SetComponentStyle(AStyle);

  for I := 0 to MenuItems.Count - 1 do
    MenuItems[I].Menu.SetComponentStyle(AStyle);
end;

procedure TAdvSmoothMegaMenu.SetDefaultMenuContentFill(const Value: TGDIPFill);
begin
  if FDefaultMenuContentFill <> value then
  begin
    FDefaultMenuContentFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetDefaultMenuTearOffFill(const Value: TGDIPFill);
begin
  if FDefaultMenuTearOffFill <> value then
  begin
    FDefaultMenuTearOffFill := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetDefaultSection(const Value: TGDIPMenuSection);
begin
  if FDefaultSection <> Value then
  begin
    FDefaultSection.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetDefaultSectionItemAppearance(
  const Value: TGDIPMenuSectionItemAppearance);
begin
  if FDefaultSectionItemAppearance <> value then
  begin
    FDefaultSectionItemAppearance.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetDefaultTopLayerItem(
  const Value: TGDIPMenuTopLayerItem);
begin
  if FDefaultTopLayerItem <> value then
  begin
    FDefaultTopLayerItem := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetDirection(
  const Value: TAdvSmoothMegaMenuDirection);
begin
  if FDirection <> value then
  begin
    FDirection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetMenuItems(const Value: TAdvSmoothMegaMenuItems);
begin
  if FMenuItems <> value then
  begin
    FMenuItems.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetOpenMenusOnClick(const Value: Boolean);
begin
  if FOpenMenusOnClick <> Value then
  begin
    FOpenMenusOnClick := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetPersistSelection(const Value: Boolean);
begin
  if FPersistSelection <> value then
  begin
    FPersistSelection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetPictureContainer(
  const Value: TGDIPPictureContainer);
begin
  FPictureContainer := Value;
  Changed;
end;

procedure TAdvSmoothMegaMenu.SetSelectedItem(const Value: Integer);
begin
  if FSelectedItem <> Value then
  begin
    FSelectedItem := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetTextRendering(
  const Value: TAdvSmoothMegaMenuTextRenderingHint);
begin
  if FTextRendering <> value then
  begin
    FTextRendering := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenu.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothMegaMenu.ShowItemForm(item: integer);
begin
  if (item >= 0) and (item <= MenuItems.Count - 1) then
  begin
    if not Assigned(MenuItems[item].frm) and MenuItems[item].Enabled and MenuItems[item].Visible and (MenuItems[item].Menu.Sections.Count > 0) then
    begin
      MenuItems[item].frm := TAdvSmoothMegaMenuItemForm.CreateNew(Self);
      MenuItems[item].frm.OnClose := MenuItems[item].CloseForm;
      MenuItems[item].frm.Menu := Self;
      MenuItems[item].frm.MenuItem := MenuItems[item];
      MenuItems[item].UpdateFormSize;
      MenuItems[item].frm.Init;
      MenuItems[item].frm.SetPosition;
      if (MenuItems[item].frm.Width <> 0) and (MenuItems[item].frm.Height <> 0) then
      begin
        if MenuItems[item].Menu.UsesControls then
        begin
          MenuItems[item].frm.Visible := true
        end
        else
        begin
          SetWindowPos(MenuItems[item].frm.Handle, 0, MenuItems[item].frm.Left, MenuItems[item].frm.Top,
            MenuItems[item].frm.Width, MenuItems[item].frm.Height, SWP_SHOWWINDOW);
          if Assigned(MenuItems[item].frm) then
            MenuItems[item].frm.FormShow(Self);
        end;
      end;

      DoDropDown(Self, item);
    end;
  end;
end;

procedure TAdvSmoothMegaMenu.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothMegaMenu.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS;
end;

procedure TAdvSmoothMegaMenu.WMPaint(var Message: TWMPaint);
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

procedure TAdvSmoothMegaMenu.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothMegaMenu.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothMegaMenu.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothMegaMenu.WndProc(var Message: TMessage);
var
  l, t: Integer;
begin
  if Message.Msg = WM_RECALLFLOATINGFORM then
  begin
    if Assigned(FMenuItemRecall) and Assigned(FMenuItemRecall.Frm) then
    begin
      l := FMenuItemRecall.frm.Left;
      t := FMenuItemRecall.frm.Top;
//      FMenuItemRecall.CloseMenu(true);
//      FMenuItemRecall.OpenMenu;
      FMenuItemRecall.frm.Left := l;
      FMenuItemRecall.frm.Top := t;
      FMenuItemRecall := nil;
    end;
  end;
  inherited;
end;

function TAdvSmoothMegaMenu.XYToItem(X, Y: integer): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to MenuItems.Count - 1 do
  begin
    with MenuItems[I] do
    begin
      if PtInRect(Bounds(Round(FItemRect.X), Round(FItemRect.Y), Round(FItemRect.Width), Round(FItemRect.Height)), Point(X, Y))
        and Enabled and Visible and not Separator then
      begin
        Result := i;
        break;
      end;
    end;
  end;
end;

{ TAdvSmoothMegaMenuAppearance }

procedure TAdvSmoothMegaMenuAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMegaMenuAppearance) then
  begin
    FMenuItemSeparatorFill.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemSeparatorFill);
    FMenuItemFillHover.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFillHover);
    FMenuItemFillDisabled.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFillDisabled);
    FMenuItemFillSelected.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFillSelected);
    FMenuItemSpacing := (Source as TAdvSmoothMegaMenuAppearance).MenuItemSpacing;
    FMenuItemFill.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFill);
    FMenuItemFontDisabled.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFontDisabled);
    FMenuItemFontSelected.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFontSelected);
    FMenuItemFont.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFont);
    FMenuItemFontHover.Assign((Source as TAdvSmoothMegaMenuAppearance).MenuItemFontHover);
    FMenuItemSeparatorHeight := (Source as TAdvSmoothMegaMenuAppearance).MenuItemSeparatorHeight;
    FMenuItemSeparatorWidth := (Source as TAdvSmoothMegaMenuAppearance).MenuItemSeparatorWidth;
    FMenuItemURLColor := (Source as TAdvSmoothMegaMenuAppearance).MenuItemURLColor;
    FMenuItemShadowColor := (Source as TAdvSmoothMegaMenuAppearance).MenuItemShadowColor;
    FMenuItemShadowOffset := (Source as TAdvSmoothMegaMenuAppearance).MenuItemShadowOffset;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMegaMenuAppearance.Create(AOwner: TAdvSmoothMegaMenu);
begin
  FOwner := AOwner;
  FMenuItemSeparatorFill := TGDIPFill.Create;
  FMenuItemSeparatorFill.OnChange := FillChanged;
  FMenuItemFillHover := TGDIPFill.Create;
  FMenuItemFillHover.OnChange := FillChanged;
  FMenuItemFontDisabled := TFont.Create;
  FMenuItemFontDisabled.OnChange := FontChanged;
  FMenuItemFontSelected := TFont.Create;
  FMenuItemFontSelected.OnChange := FontChanged;
  FMenuItemFont := TFont.Create;
  FMenuItemFont.OnChange := FontChanged;
  FMenuItemFillDisabled := TGDIPFill.Create;
  FMenuItemFillDisabled.OnChange := FillChanged;
  FMenuItemFillSelected := TGDIPFill.Create;
  FMenuItemFillSelected.OnChange := FillChanged;
  FMenuItemSpacing := 0;
  FMargin := TAdvSmoothMegaMenuMargin.Create(fOwner);
  FMargin.OnChange := MarginChanged;
  FMargin.Left := 0;
  FMargin.Top := 0;
  FMargin.Right := 0;
  FMargin.Bottom := 0;
  FMenuItemFontHover := TFont.Create;
  FMenuItemFontHover.OnChange := FontChanged;
  FMenuItemFill := TGDIPFill.Create;
  FMenuItemFill.OnChange := FillChanged;
  FMenuItemSeparatorHeight := 50;
  FMenuItemSeparatorWidth := 3;
  FMenuItemURLColor := clBlue;
  FMenuItemShadowColor := clGray;
  FMenuItemShadowOffset := 5;

  {$IFNDEF DELPHI9_LVL}
  FMenuItemFont.Name := 'Tahoma';
  FMenuItemFontSelected.Name := 'Tahoma';
  FMenuItemFontHover.Name := 'Tahoma';
  FMenuItemFontDisabled.Name := 'Tahoma';
  {$ENDIF}
end;

destructor TAdvSmoothMegaMenuAppearance.Destroy;
begin
  FMenuItemSeparatorFill.Free;
  FMenuItemFillHover.Free;
  FMenuItemFontDisabled.Free;
  FMenuItemFontSelected.Free;
  FMenuItemFont.Free;
  FMenuItemFillDisabled.Free;
  FMenuItemFillSelected.Free;
  FMenuItemFontHover.Free;
  FMenuItemFill.Free;
  FMargin.Free;
  inherited;
end;

procedure TAdvSmoothMegaMenuAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenuAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenuAppearance.LoadFromFile(ini: TIniFile;
  Section: String);
begin
  MenuItemFill.LoadFromFile(ini, Section + '.MenuItemFill');
  MenuItemFillSelected.LoadFromFile(ini, Section + '.MenuItemFillSelected');
  MenuItemFillDisabled.LoadFromFile(ini, Section + '.MenuItemFillDisabled');
  MenuItemFillHover.LoadFromFile(ini, Section + '.MenuItemFillHover');
  MenuItemSeparatorFill.LoadFromFile(ini, Section + '.MenuItemSeparatorFill');
  LoadFont(ini, Section + '.MenuItemFont', MenuItemFont);
  LoadFont(ini, Section + '.MenuItemFontSelected', MenuItemFontSelected);
  LoadFont(ini, Section + '.MenuItemFontHover', MenuItemFontHover);
  LoadFont(ini, Section + '.MenuItemFontDisabled', MenuItemFontDisabled);
end;

procedure TAdvSmoothMegaMenuAppearance.MarginChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenuAppearance.SaveToFile(ini: TIniFile;
  Section: String);
begin
  MenuItemFill.SaveToFile(ini, Section + '.MenuItemFill');
  MenuItemFillSelected.SaveToFile(ini, Section + '.MenuItemFillSelected');
  MenuItemFillDisabled.SaveToFile(ini, Section + '.MenuItemFillDisabled');
  MenuItemFillHover.SaveToFile(ini, Section + '.MenuItemFillHover');
  MenuItemSeparatorFill.SaveToFile(ini, Section + '.MenuItemSeparatorFill');
  SaveFont(ini, Section + '.MenuItemFont', MenuItemFont);
  SaveFont(ini, Section + '.MenuItemFontSelected', MenuItemFontSelected);
  SaveFont(ini, Section + '.MenuItemFontHover', MenuItemFontHover);
  SaveFont(ini, Section + '.MenuItemFontDisabled', MenuItemFontDisabled);
end;

procedure TAdvSmoothMegaMenuAppearance.SetMargin(
  const Value: TAdvSmoothMegaMenuMargin);
begin
  if FMargin <> value then
  begin
    FMargin.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFill(const Value: TGDIPFill);
begin
  if FMenuItemFill <> value then
  begin
    FMenuItemFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFillDisabled(
  const Value: TGDIPFill);
begin
  if FMenuItemFillDisabled <> value then
  begin
    FMenuItemFillDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFillHover(
  const Value: TGDIPFill);
begin
  if FMenuItemFillHover <> value then
  begin
    FMenuItemFillHover.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFillSelected(
  const Value: TGDIPFill);
begin
  if FMenuItemFillSelected <> value then
  begin
    FMenuItemFillSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFont(const Value: TFont);
begin
  if FMenuItemFont <> value then
  begin
    FMenuItemFont.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFontDisabled(
  const Value: TFont);
begin
  if FMenuItemFontDisabled <> value then
  begin
    FMenuItemFontDisabled.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFontHover(const Value: TFont);
begin
  if FMenuItemFontHover <> value then
  begin
    FMenuItemFontHover.Assign(value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemFontSelected(
  const Value: TFont);
begin
  if FMenuItemFontSelected <> value then
  begin
    FMenuItemFontSelected.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemSeparatorFill(
  const Value: TGDIPFill);
begin
  if FMenuItemSeparatorFill <> value then
  begin
    FMenuItemSeparatorFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemSeparatorHeight(
  const Value: integer);
begin
  if FMenuItemSeparatorHeight <> value then
  begin
    FMenuItemSeparatorHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemSeparatorWidth(
  const Value: integer);
begin
  if FMenuItemSeparatorWidth <> value then
  begin
    FMenuItemSeparatorWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemShadowColor(
  const Value: TColor);
begin
  if FMenuItemShadowColor <> value then
  begin
    FMenuItemShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemShadowOffset(
  const Value: integer);
begin
  if FMenuItemShadowOffset <> value then
  begin
    FMenuItemShadowOffset := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemSpacing(const Value: integer);
begin
  if FMenuItemSpacing <> value then
  begin
    FMenuItemSpacing := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuAppearance.SetMenuItemURLColor(const Value: TColor);
begin
  if FMenuItemURLColor <> value then
  begin
    FMenuItemURLColor := Value;
    Changed;
  end;
end;

{ TAdvSmoothMegaMenuItems }

function TAdvSmoothMegaMenuItems.Add: TAdvSmoothMegaMenuItem;
begin
  Result := TAdvSmoothMegaMenuItem(inherited Add);
end;

constructor TAdvSmoothMegaMenuItems.Create(AOwner: TAdvSmoothMegaMenu);
begin
  inherited Create(AOwner, CreateItemClass);
  FOwner := AOwner;
end;

function TAdvSmoothMegaMenuItems.CreateItemClass: TCollectionItemClass;
begin
  Result := TAdvSmoothMegaMenuItem;
end;

procedure TAdvSmoothMegaMenuItems.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TAdvSmoothMegaMenuItems.GetItem(
  Index: Integer): TAdvSmoothMegaMenuItem;
begin
  Result := TAdvSmoothMegaMenuItem(inherited Items[Index]);
end;

function TAdvSmoothMegaMenuItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TAdvSmoothMegaMenuItems.Insert(Index: Integer): TAdvSmoothMegaMenuItem;
begin
  Result := TAdvSmoothMegaMenuItem(inherited Insert(Index));
end;

procedure TAdvSmoothMegaMenuItems.SetItem(Index: Integer;
  const Value: TAdvSmoothMegaMenuItem);
begin
  inherited Items[Index] := Value;
end;

{ TAdvSmoothMegaMenuItem }

procedure TAdvSmoothMegaMenuItem.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMegaMenuItem) then
  begin
    FCaption := (Source as TAdvSmoothMegaMenuItem).Caption;
    FEnabled := (Source as TAdvSmoothMegaMenuItem).Enabled;
    FVisible := (Source as TAdvSmoothMegaMenuItem).Visible;
    FWidth := (Source as TAdvSmoothMegaMenuItem).Width;
    FHeight := (Source as TAdvSmoothMegaMenuItem).Height;
    FMenu.Assign((Source as TAdvSmoothMegaMenuItem).Menu);
    FSeparator := (Source as TAdvSmoothMegaMenuItem).Separator;
    FGraphicLeftDisabledName := (Source as TAdvSmoothMegaMenuItem).GraphicLeftDisabledName;
    FGraphicLeftSelectedName := (Source as TAdvSmoothMegaMenuItem).GraphicLeftSelectedName;
    FGraphicLeftName := (Source as TAdvSmoothMegaMenuItem).GraphicLeftName;
    FGraphicLeftHoverName := (Source as TAdvSmoothMegaMenuItem).GraphicLeftHoverName;
    FGraphicRightDisabledName := (Source as TAdvSmoothMegaMenuItem).GraphicRightDisabledName;
    FGraphicRightSelectedName := (Source as TAdvSmoothMegaMenuItem).GraphicRightSelectedName;
    FGraphicRightName := (Source as TAdvSmoothMegaMenuItem).GraphicRightName;
    FGraphicRightHoverName := (Source as TAdvSmoothMegaMenuItem).GraphicRightHoverName;
    FGraphicLeftDisabledIndex := (Source as TAdvSmoothMegaMenuItem).GraphicLeftDisabledIndex;
    FGraphicLeftSelectedIndex := (Source as TAdvSmoothMegaMenuItem).GraphicLeftSelectedIndex;
    FGraphicLeftIndex := (Source as TAdvSmoothMegaMenuItem).GraphicLeftIndex;
    FGraphicLeftHoverIndex := (Source as TAdvSmoothMegaMenuItem).GraphicLeftHoverIndex;
    FGraphicRightDisabledIndex := (Source as TAdvSmoothMegaMenuItem).GraphicRightDisabledIndex;
    FGraphicRightSelectedIndex := (Source as TAdvSmoothMegaMenuItem).GraphicRightSelectedIndex;
    FGraphicRightIndex := (Source as TAdvSmoothMegaMenuItem).GraphicRightIndex;
    FGraphicRightHoverIndex := (Source as TAdvSmoothMegaMenuItem).GraphicRightHoverIndex;

    FMenuHeight := (Source as TAdvSmoothMegaMenuItem).MenuHeight;
    FMenuWidth := (Source as TAdvSmoothMegaMenuItem).MenuWidth;
    FMenuAutoSize := (Source as TAdvSmoothMegaMenuItem).MenuAutoSize;
    FPersistSelection := (Source as TAdvSmoothMegaMenuItem).PersistSelection;
    FTag := (Source as TAdvSmoothMegaMenuItem).Tag;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.CaptionChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenuItem.Changed;
begin
  FOwner.Changed;
end;

procedure TAdvSmoothMegaMenuItem.CloseForm(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TAdvSmoothMegaMenuItem.CloseMenu(CloseFloating: Boolean);
var
  Msg: TMessage;
  cu: TCurrentItem;
  I: Integer;
begin
  if Assigned(frm) then
  begin
    if (IsFloating and CloseFloating) or (not IsFloating) then
    begin
      FWillFloat := False;
      FFormDestroying := true;
      FMenu.DoCMMouseLeave(msg);
      frm.Close;
      frm := nil;
      FFormDestroying := false;
      FOwner.DoCloseUp(FOwner, Self.Index);
      for I := 0 to Menu.Sections.Count - 1 do
      begin
        Menu.Sections[I].AutoHeight := 0;
        Menu.Sections[I].AutoWidth := 0;
      end;

      if not PersistSelection then
      begin
        cu.item := nil;
        cu.section := nil;
        Menu.SelectedItem := cu;
      end;
    end;
  end;
end;

constructor TAdvSmoothMegaMenuItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvSmoothMegaMenuItems).FOwner;
  FEnabled := true;
  FVisible := true;
  FWidth := 75;
  FHeight := 30;
  FMenu := TGDIPMenu.Create(FOwner);
  FMenu.BeginUpdate;
  FMenu.RootCaption := Self.Caption;
  FMenu.OnChange := MenuChanged;
  FMenu.ImageList := FOwner.ImageList;
  FMenu.PictureContainer := FOwner.PictureContainer;
  //assign events
  FMenu.OnItemClick := FOwner.DoMenuSubItemClick;
  FMenu.OnItemHover := FOwner.DoMenuSubItemHover;
  FMenu.OnItemEditChanged := FOwner.DoMenuSubItemEditChanged;
  FMenu.OnItemCheckChanged := FOwner.DoMenuSubItemCheckChanged;
  FMenu.OnItemAnchorClick := FOwner.DoMenuSubItemAnchor;
  FMenuAutoSize := true;
  FMenuHeight := 150;
  FMenuWidth := 250;
  FGraphicLeftIndex := -1;
  FGraphicLeftHoverIndex := -1;
  FGraphicLeftSelectedIndex := -1;
  FGraphicLeftDisabledIndex := -1;
  FGraphicRightIndex := -1;
  FGraphicRightHoverIndex := -1;
  FGraphicRightSelectedIndex := -1;
  FGraphicRightDisabledIndex := -1;
  //
  FSeparator := false;
  FCaptionLocation := mlCenterCenter;
  FCaptionFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FCaptionFont.Name := 'Tahoma';
  {$ENDIF}
  FCaptionLeft := 0;
  FCaptionTop := 0;
  FPersistSelection := false;

  with FMenu do
  begin
    ItemAppearance.Assign(FOwner.DefaultSectionItemAppearance);
    ContentFill.Assign(FOwner.DefaultMenuContentFill);
    TearOffFill.Assign(FOwner.DefaultMenuTearOffFill);
  end;

  FMenu.EndUpdate;

  with FOwner do
  begin
    if (csDestroying in ComponentState) then
      Exit;

    if FDesignTime then
    begin
      FCaption := 'MenuItem ' + IntToStr(Index);
    end;

    Changed;
  end;
end;

destructor TAdvSmoothMegaMenuItem.Destroy;
begin
  CloseMenu(True);
  FCaptionFont.Free;
  FMenu.Free;
  inherited;
  with FOwner do
  begin
    if not (csDestroying in ComponentState) then
      Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.Draw(g: TGPGraphics);
var
  cf: TFont;
  fl: TGDIPFill;
  itl, itr: String;
  itlidx, itridx: integer;
  picl, picr: TAdvGDIPPicture;
  bmpl, bmpr: TBitmap;
  w, h: integer;
  htmlr: TGPrectF;
  FCap: TGDIPMenuHTMLText;
  ca: TCanvas;
begin
  if Visible then
  begin
    with FOwner do
    begin
      FCap := TGDIPMenuHTMLText.Create(nil);
      FCap.URLColor := ItemAppearance.MenuItemURLColor;
      FCap.ShadowColor := ItemAppearance.MenuItemShadowColor;
      FCap.ShadowOffset := ItemAppearance.MenuItemShadowOffset;
      if Separator then
      begin
        ItemAppearance.MenuItemSeparatorFill.Fill(g, FItemRect);
      end
      else
      begin
        cf := TFont.Create;
        fl := TGDIPFill.Create;
        if Self.Enabled then
        begin
          //Preset
          if (FSelectedItem = Index) and AllowSelection then
          begin
            cf.Assign(ItemAppearance.MenuItemFontSelected);
            fl.Assign(ItemAppearance.MenuItemFillSelected);
            itl := GraphicLeftSelectedName;
            itr := GraphicRightSelectedName;
            itlidx := GraphicLeftSelectedIndex;
            itridx := GraphicRightSelectedIndex;
          end
          else if (FHoveredItem = Index) then
          begin
            cf.Assign(ItemAppearance.MenuItemFontHover);
            fl.Assign(ItemAppearance.MenuItemFillHover);
            itl := GraphicLeftHoverName;
            itr := GraphicRightHoverName;
            itlidx := GraphicLeftHoverIndex;
            itridx := GraphicRightHoverIndex;
          end
          else
          begin
            cf.Assign(ItemAppearance.MenuItemFont);
            fl.Assign(ItemAppearance.MenuItemFill);
            itl := GraphicLeftName;
            itr := GraphicRightName;
            itlidx := GraphicLeftIndex;
            itridx := GraphicRightIndex;
          end;
          /////
        end
        else
        begin
          cf.Assign(ItemAppearance.MenuItemFontDisabled);
          fl.Assign(ItemAppearance.MenuItemFillDisabled);
          itl := GraphicLeftDisabledName;
          itr := GraphicRightDisabledName;
          itlidx := GraphicLeftDisabledIndex;
          itridx := GraphicRightDisabledIndex;
        end;

        if (GraphicLeftName <> '') and (itl = '') then
          itl := GraphicLeftName;

        if (GraphicRightName <> '') and (itr = '') then
          itr := GraphicRightName;

        if (GraphicLeftIndex <> -1) and (itlidx = -1) then
          itlidx := GraphicLeftIndex;

        if (GraphicRightIndex <> -1) and (itridx = -1) then
          itridx := GraphicRightIndex;

        fl.BeginUpdate;
        fl.Focus := FFocused and TabStop and (FFocusedItem = Index);
        fl.Fill(g, FItemRect);
        fl.EndUpdate;

        picl := nil;
        picr := nil;
        bmpl := TBitmap.Create;
        bmpr := TBitmap.Create;
        //graphic left and right
        if Assigned(PictureContainer) then
        begin
          picl := PictureContainer.FindPicture(itl);
          picr := PictureContainer.FindPicture(itr);

          if Assigned(picl) and not picl.Empty then
          begin
            picl.GetImageSizes;
            w := picl.Width;
            h := picl.Height;
            if Self.Caption <> '' then
              picl.GDIPDraw(g, Bounds(Round(FCaptionRect.x - w) - 4, Round(FItemRect.Y + (FItemRect.Height - h)/ 2), w, h))
            else
            begin
              if Assigned(picr) then
                picl.GDIPDraw(g, Bounds(Round(FItemRect.x + (FItemRect.Width / 2) - w - 2), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), w, h))
              else
                picl.GDIPDraw(g, Bounds(Round(FItemRect.x + ((FItemRect.Width - w) / 2)), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), w, h))
            end;
          end;

          if Assigned(picr) and not picr.Empty then
          begin
            picr.GetImageSizes;
            w := picr.Width;
            h := picr.Height;
            if Self.Caption <> '' then
              picr.GDIPDraw(g, Bounds(Round(FCaptionRect.X + FCaptionRect.Width) + 4, Round(FItemRect.Y + (FItemRect.Height - h)/ 2), w, h))
            else
            begin
              if Assigned(picl) then
                picr.GDIPDraw(g, Bounds(Round(FItemRect.x + (FItemRect.Width / 2) + 2), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), w, h))
              else
                picr.GDIPDraw(g, Bounds(Round(FItemRect.x + ((FItemRect.Width - w) / 2)), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), w, h))
            end;
          end;
        end;

        if Assigned(ImageList) then
        begin
          if (itlidx >= 0) and (itlidx <= ImageList.Count - 1) then
            ImageList.GetBitmap(itlidx, bmpl);

          if (itridx >= 0) and (itridx <= ImageList.Count - 1) then
            ImageList.GetBitmap(itridx, bmpr);

          ca := TCanvas.Create;
          ca.handle := g.GetHDC;
          if Assigned(bmpl) and not bmpl.Empty then
          begin
            w := bmpl.Width;
            h := bmpl.Height;
            if Self.Caption <> '' then
              ca.Draw(Round(FCaptionRect.x - w - 4), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), bmpl)
            else
            begin
              if Assigned(bmpr) then
                ca.Draw(Round(FItemRect.x + (FItemRect.Width / 2) - w - 2), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), bmpl)
              else
                ca.Draw(Round(FItemRect.x + ((FItemRect.Width - w) / 2)), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), bmpl);
            end;
          end;

          if Assigned(bmpr) and not bmpr.Empty then
          begin
            w := bmpr.Width;
            h := bmpr.Height;
            if Self.Caption <> '' then
              ca.Draw(Round(FCaptionRect.X + FCaptionRect.Width + 4), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), bmpr)
            else
            begin
              if Assigned(bmpl) then
                ca.Draw(Round(FItemRect.x + (FItemRect.Width / 2) + 2), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), bmpr)
              else
                ca.Draw(Round(FItemRect.x + ((FItemRect.Width - w) / 2)), Round(FItemRect.Y + (FItemRect.Height - h)/ 2), bmpr);
            end;
          end;

          g.ReleaseHDC(ca.Handle);
          ca.Free;
        end;

        if Fcaption <> '' then
        begin
          FCap.Text := FCaption;
          if (Assigned(Picr) and not picr.Empty) or (Assigned(picl) and not picl.Empty) or (Assigned(bmpl) and not bmpl.Empty) or (Assigned(bmpr) and not bmpr.Empty) then
            DrawMenuItemHTMLText(g, cf, Fcap, CaptionLocation, FCaptionRect, htmlr, FCaption)
          else
            DrawMenuItemHTMLText(g, cf, Fcap, CaptionLocation, FItemRect, htmlr, FCaption);
        end;

        cf.Free;
        fl.Free;
        bmpl.Free;
        bmpr.Free;
      end;
    end;
    FCap.Free;
  end;
end;

function TAdvSmoothMegaMenuItem.DrawMenuItemHTMLText(g: TGPGraphics; f: TFont;
  HTML: TGDIPMenuHTMLText; Location: TGDIPMenuLocation; r: TGPRectF; var htmlr: TGPRectF;
  str: String; DoCalculate, DoAnchor: Boolean; fX, fY: integer): String;
var
  a, s, k: String;
  l, m, XSize, YSize: integer;
  xs, ys: Double;
  x, y: Double;
  hr, htmlrect: TRect;
  UseHTML: Boolean;
  cf: TGPFont;
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  b: TGPSolidBrush;
  sizerect: TGPRectF;
  rtest: TRect;
begin
  with HTML do
  begin
    if str <> '' then
    begin
      UseHTML := (Pos('</', str) > 0) or (Pos('/>', str) > 0) or (Pos('<BR>',uppercase(str)) > 0);

      cf := nil;
      ff := nil;
      sf := nil;

      htmlrect := Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height));

      if UseHTML then
      begin
        HTMLDrawGDIP(g, f, str,htmlrect, FOwner.ImageList, 0,0,-1,-1, Html.ShadowOffset,False,true,false,false,
          False,False,true,1.0, Html.URLColor,clNone,clNone,html.ShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,Fowner.PictureContainer,2);

        xs := XSize;
        ys := YSize;
      end
      else
      begin
        ff := TGPFontFamily.Create(f.Name);
        if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
        begin
          ff.Free;
          ff := TGPFontFamily.Create('Arial');
        end;

        fs := 0;
        if (fsBold in f.Style) then
          fs := fs + 1;
        if (fsItalic in f.Style) then
          fs := fs + 2;
        if (fsUnderline in f.Style) then
          fs := fs + 4;

        sf := TGPStringFormat.Create(0);
        sf.SetHotkeyPrefix(HotkeyPrefixShow);
        cf := TGPFont.Create(ff, f.Size , fs, UnitPoint);
        g.MeasureString(str, Length(str), cf, r, sf, sizerect);

        xs := sizerect.Width;
        ys := sizerect.Height;
      end;

      if DoCalculate then
      begin
        htmlr := MakeRect(r.X, r.Y, xs, ys);
        if not UseHTML then
        begin
          cf.Free;
          ff.Free;
          sf.Free;
        end;
        Exit;
      end;

      if Location <> mlCustom then
        FOwner.GetLocation(x, y, r, xs, ys, Location)
      else
      begin
        x := 0;
        y := 0;
      end;

      if Location <> mlCustom then
        htmlr := MakeRect(x + FCaptionLeft, y + FCaptionTop, xs, ys)
      else
        htmlr := MakeRect(r.x + FCaptionLeft, r.y + FCaptionTop, xs, ys);

      htmlrect := Bounds(Round(htmlr.X), Round(htmlr.Y), Round(htmlr.Width), Round(htmlr.Height));

      if UseHTML then
      begin
        HTMLDrawGDIP(g, f, str,htmlrect,FOwner.ImageList, fx,fy,-1,-1,Html.ShadowOffset,DoAnchor,false,false,false,
          False,False,true,1.0,html.URLColor,clNone,clNone,html.ShadowColor,a,s,k,XSize,YSize,l,m,hr,nil,FOwner.PictureContainer,2);
      end
      else
      begin
        b := TGPSolidBrush.Create(MakeColor(255, f.Color));
        rtest := Bounds(Round(htmlr.X), Round(htmlr.Y), Round(htmlr.Width), Round(htmlr.Height));
        g.DrawString(str, Length(str), cf, htmlr, sf, b);
        b.Free;
        cf.Free;
        ff.Free;
        sf.Free;
      end;
      result := a;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItem.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothMegaMenuItem.FloatMenu(X, Y: integer);
begin
  FFloating := true;
  if not Assigned(frm) then
    OpenMenu;

  if Assigned(frm) then
  begin
    frm.FPosX := -1;
    frm.FPosY := -1;
  end;

  if (X <> -1) and (Y <> -1) then
    frm.SetPosition(X, Y);
end;

function TAdvSmoothMegaMenuItem.GetDisplayName: string;
begin
  if Caption <> '' then
    Result := Caption
  else
    Result := inherited GetDisplayName;
end;

function TAdvSmoothMegaMenuItem.GetMenu: TAdvSmoothMegaMenu;
begin
  Result := FOwner;
end;

function TAdvSmoothMegaMenuItem.GetSectionItems(SectionIndex,
  SectionItemIndex: integer): TGDIPMenuSectionItem;
begin
  Result := Menu.Sections[SectionIndex].Items[SectionItemIndex];
end;

function TAdvSmoothMegaMenuItem.GetSections(Index: integer): TGDIPMenuSection;
begin
  Result := Menu.Sections[Index];
end;

function TAdvSmoothMegaMenuItem.IsFloating: Boolean;
begin
  if Assigned(frm) then
    Result := FFloating and frm.IsFormFloating
  else
    Result := false;
end;

function TAdvSmoothMegaMenuItem.IsFormMoving: Boolean;
begin
  Result := frm.IsFormFloating;
end;

procedure TAdvSmoothMegaMenuItem.MenuChanged(Sender: TObject);
begin
  Changed;
  if Assigned(frm) then
    frm.UpdateForm;
end;

procedure TAdvSmoothMegaMenuItem.OpenMenu;
begin
  FOwner.ShowItemForm(self.Index);
  if Assigned(frm) then
  begin
    frm.FPosX := -1;
    frm.FPosY := -1;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    if Assigned(FMenu) then
      FMenu.RootCaption := FCaption;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetCaptionFont(const Value: TFont);
begin
  if FCaptionFont <> value then
  begin
    FCaptionFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetCaptionLeft(const Value: integer);
begin
  if FCaptionLeft <> value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetCaptionLocation(
  const Value: TGDIPMenuLocation);
begin
  if FCaptionLocation <> value then
  begin
    FCaptionLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetCaptionTop(const Value: integer);
begin
  if FCaptionTop <> value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftDisabledIndex(
  const Value: Integer);
begin
  if FGraphicLeftDisabledIndex <> value then
  begin
    FGraphicLeftDisabledIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftDisabledName(
  const Value: String);
begin
  if FGraphicLeftDisabledName <> value then
  begin
    FGraphicLeftDisabledName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftHoverIndex(const Value: Integer);
begin
  if FGraphicLeftHoverIndex <> Value then
  begin
    FGraphicLeftHoverIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftHoverName(const Value: String);
begin
  if FGraphicLeftHoverName <> value then
  begin
    FGraphicLeftHoverName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftIndex(const Value: Integer);
begin
  if FGraphicLeftIndex <> value then
  begin
    FGraphicLeftIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftName(const Value: String);
begin
  if FGraphicLeftName <> Value then
  begin
    FGraphicLeftName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftSelectedIndex(
  const Value: Integer);
begin
  if FGraphicLeftSelectedIndex <> Value then
  begin
    FGraphicLeftSelectedIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicLeftSelectedName(
  const Value: String);
begin
  if FGraphicLeftSelectedName <> value then
  begin
    FGraphicLeftSelectedName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightDisabledIndex(
  const Value: Integer);
begin
  if FGraphicRightDisabledIndex <> Value then
  begin
    FGraphicRightDisabledIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightDisabledName(
  const Value: String);
begin
  if FGraphicRightDisabledName <> value then
  begin
    FGraphicRightDisabledName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightHoverIndex(
  const Value: Integer);
begin
  if FGraphicRightHoverIndex <> value then
  begin
    FGraphicRightHoverIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightHoverName(const Value: String);
begin
  if FGraphicRightHoverName <> value then
  begin
    FGraphicRightHoverName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightIndex(const Value: Integer);
begin
  if FGraphicRightIndex <> value then
  begin
    FGraphicRightIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightName(const Value: String);
begin
  if FGraphicRightName <> value then
  begin
    FGraphicRightName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightSelectedIndex(
  const Value: Integer);
begin
  if FGraphicRightSelectedIndex <> value then
  begin
    FGraphicRightSelectedIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetGraphicRightSelectedName(
  const Value: String);
begin
  if FGraphicRightSelectedName <> value then
  begin
    FGraphicRightSelectedName := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetMenu(const Value: TGDIPMenu);
begin
  if FMenu <> Value then
  begin
    FMenu.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetMenuAutoSize(const Value: Boolean);
begin
  if FMenuAutoSize <> value then
  begin
    FMenuAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetMenuHeight(const Value: integer);
begin
  if FMenuHeight <> value then
  begin
    FMenuHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetMenuPosition(X, Y: integer);
begin
  if Assigned(frm) then
    frm.SetPosition(X, Y);
end;

procedure TAdvSmoothMegaMenuItem.SetMenuWidth(const Value: integer);
begin
  if FMenuWidth <> value then
  begin
    FMenuWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetPersistSelection(const Value: Boolean);
begin
  if FPersistSelection <> value then
  begin
    FPersistSelection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetSectionItems(SectionIndex,
  SectionItemIndex: integer; const Value: TGDIPMenuSectionItem);
begin
  Menu.Sections[SectionIndex].Items[SectionItemIndex].Assign(Value);
end;

procedure TAdvSmoothMegaMenuItem.SetSections(Index: integer;
  const Value: TGDIPMenuSection);
begin
  Menu.Sections[Index].Assign(Value);
end;

procedure TAdvSmoothMegaMenuItem.SetSeparator(const Value: Boolean);
begin
  if FSeparator <> value then
  begin
    FSeparator := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuItem.UpdateFormSize;
var
  i: integer;
  maxh, maxw: integer;
begin
  maxh := 0;
  maxw := 0;
  if MenuAutoSize then
  begin
    if Menu.AutoSectionSize then
      Menu.DoAutoSize;

    for I := 0 to Menu.Sections.Count - 1 do
    begin
      if Menu.Sections[I].Visible then
      begin
        case Menu.SectionLayout of
          slHorizontal:
          begin
            if Menu.Sections[I].GetHeight > maxh then
              maxh := Round(Menu.Sections[I].GetHeight);

             maxw := maxw + Round(Menu.Sections[I].GetWidth);
          end;
          slVertical:
          begin
            if Menu.Sections[I].GetWidth > maxw then
              maxw := Round(Menu.Sections[I].GetWidth);

            maxh := maxh + Round(Menu.Sections[I].GetHeight);
          end;
        end;
      end;
    end;

    frm.Height := maxh + Menu.SectionMargin.Top + Menu.SectionMargin.Bottom;
    frm.Width := maxw + Menu.SectionMargin.Left + Menu.SectionMargin.Right;
    if Menu.ContentFill.BorderColor <> clNone then
    begin
      frm.Height := frm.Height + (Menu.ContentFill.BorderWidth * 2);
      frm.Width := frm.Width + (Menu.ContentFill.BorderWidth * 2);
    end;

    if Menu.ContentFill.ShadowColor <> clNone then
    begin
      frm.Height := frm.Height + Menu.ContentFill.ShadowOffset;
      frm.Width := frm.Width + Menu.ContentFill.ShadowOffset;
    end;

    if (Menu.TearOffSize > 0) and Menu.TearOff then
    begin
      frm.Height := frm.Height + Menu.TearOffSize;
    end;

    frm.Height := frm.Height + 1;
    frm.Width := frm.Width + 1;
  end
  else
  begin
    frm.Width := MenuWidth;
    frm.Height := MenuHeight;
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TAdvSmoothMegaMenuMargin }

procedure TAdvSmoothMegaMenuMargin.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothMegaMenuMargin) then
  begin
    FLeft := (Source as TAdvSmoothMegaMenuMargin).Left;
    FTop := (Source as TAdvSmoothMegaMenuMargin).Top;
    FBottom := (Source as TAdvSmoothMegaMenuMargin).Bottom;
    FRight := (Source as TAdvSmoothMegaMenuMargin).Right;
  end;
end;

procedure TAdvSmoothMegaMenuMargin.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothMegaMenuMargin.Create(AOwner: TAdvSmoothMegaMenu);
begin
  FOwner := AOwner;
  FLeft := 3;
  FTop := 3;
  FRight := 3;
  FBottom := 3;
end;

destructor TAdvSmoothMegaMenuMargin.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothMegaMenuMargin.SetBottom(const Value: integer);
begin
  if FBottom <> value then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuMargin.SetLeft(const Value: integer);
begin
  if FLeft <> value then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuMargin.SetRight(const Value: integer);
begin
  if FRight <> value then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothMegaMenuMargin.SetTop(const Value: integer);
begin
  if FTop <> value then
  begin
    FTop := Value;
    Changed;
  end;
end;

{ TAdvSmoothMegaMenuItemForm }

procedure TAdvSmoothMegaMenuItemForm.AssignControls;
var
  K, I: integer;
begin
  if Assigned(Menu) and Assigned(MenuItem) then
  begin
    with MenuItem do
    begin
      for I := 0 to Menu.Sections.Count - 1 do
      begin
        if Menu.Sections[I].Visible then
        begin
          with Menu.Sections[I] do
          begin
            for K := 0 to Items.Count - 1 do
            begin
              if Assigned(Items[K].Control) then
              begin
                Items[K].control.Parent := Self;
                Items[K].Control.Visible := true;
              end
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.ClearBuffer(graphics: TGPGraphics);
var
  g: TGPGraphics;
begin
  g := graphics;
  if not Assigned(g) then
    g := CreateGraphics;
  g.Clear($00000000);
  if not Assigned(graphics) then
    g.Free;
end;

procedure TAdvSmoothMegaMenuItemForm.ClearControls;
var
  i, k: integer;
begin
  if Assigned(Menu) and Assigned(MenuItem) then
  begin
    with MenuItem do
    begin
      for I := 0 to Menu.Sections.Count - 1 do
      begin
        if Menu.Sections[I].Visible then
        begin
          with Menu.Sections[I] do
          begin
            for K := 0 to Items.Count - 1 do
            begin
              if Assigned(Items[K].Control) then
              begin
                Items[K].Control.Visible := false;
                Items[K].Control.Parent := Self.Menu.Parent;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.DoCMDialogChar(var Message: TCMDialogChar);
var
  k, i: integer;
  item: TCurrentItem;
begin
  if Assigned(Menu) and Assigned(MenuItem) then
  begin
    with Message do
    begin
      for I := 0 to MenuItem.Menu.Sections.Count - 1 do
      begin
        if MenuItem.Menu.Sections[I].Visible then
        begin
          with MenuItem.Menu.Sections[I] do
          begin
            for K := 0 to Items.Count - 1 do
            begin
              if IsAccel(CharCode, Items[K].Text) then
              begin
                item.item := Items[K];
                item.section := MenuItem.Menu.Sections[I];
                MenuItem.Menu.SelectedItem := item;
                if Assigned(Menu.OnMenuSubItemClick) then
                  Menu.OnMenuSubItemClick(Self, Menu, Menu.GetMenuItemFromSubMenu(Items[K]), Items[K], Items[K].Text);

                if Assigned(Items[K].OnClick) then
                  Items[K].OnClick(Items[K]);

                MenuItem.MenuChanged(Self);
                Result := 1;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseLeft := False;
end;

procedure TAdvSmoothMegaMenuItemForm.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseLeft := True;
  if Assigned(Menu) then
  begin
    if Assigned(MenuItem) then
    begin
      Menuitem.Menu.DoCMMouseLeave(Message);
    end;
  end;
end;

function TAdvSmoothMegaMenuItemForm.CreateGraphics: TGPGraphics;
begin
  Result := nil;
  if Assigned(FMainBuffer) then
    Result := TGPGraphics.Create(FMainBuffer);
end;

procedure TAdvSmoothMegaMenuItemForm.CreateMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;

  FMainBuffer := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
end;

constructor TAdvSmoothMegaMenuItemForm.CreateNew(AOwner: TComponent;
  Dummy: Integer);
begin
  inherited;
end;

procedure TAdvSmoothMegaMenuItemForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

procedure TAdvSmoothMegaMenuItemForm.CreateWnd;
begin
  inherited;
  invalidate;
end;

procedure TAdvSmoothMegaMenuItemForm.DestroyMainBuffer;
begin
  if Assigned(FMainBuffer) then
    FMainBuffer.Free;
end;

procedure TAdvSmoothMegaMenuItemForm.DoCreate;
begin
  inherited;
  FMainBuffer := nil;
  OnShow := FormShow;
end;

procedure TAdvSmoothMegaMenuItemForm.DoDestroy;
begin
  inherited;
  DestroyMainBuffer;
  ClearControls;
end;

procedure TAdvSmoothMegaMenuItemForm.DoItemSelection(Key: Word; SelectedItem: TCurrentItem;
  var sec, secit: integer);
var
  dosection: Boolean;
  I: integer;
begin
  if Assigned(MenuItem) then
  begin
    with MenuItem.Menu do
    begin
      dosection := false;
      for I := 0 to Sections.Count - 1 do
      begin
        if Sections[I].Visible then
        begin
          if Sections[I].Items.Count > 0 then
            dosection := true;
        end;
      end;

      sec := SelectedItem.section.Index;
      secit := SelectedItem.item.Index;

      case Key of
        VK_UP, VK_LEFT: Dec(secit);
        VK_DOWN, VK_RIGHT: Inc(secit);
      end;

      if (secit > Sections[sec].Items.Count - 1) then
      begin
        secit := 0;
        if (sec < Sections.Count - 1) then
          Inc(sec)
        else
          sec := 0;

        while ((Sections[sec].Items.Count = 0) or not Sections[sec].Visible) and dosection do
        begin
          if sec = Sections.Count - 1 then
            sec := 0;

          if (Sections[sec].Items.Count <> 0) and Sections[sec].Visible then
            break;

          Inc(sec);
        end;
      end
      else if (secit < 0) then
      begin
        if (sec > 0) then
          Dec(sec)
        else
          sec := Sections.Count - 1;

        while ((Sections[sec].Items.Count = 0) or not Sections[sec].Visible) and dosection do
        begin
          if sec = 0 then
            sec := Sections.Count - 1;

          if (Sections[sec].Items.Count <> 0) and Sections[sec].Visible then
            break;

          Dec(sec);
        end;

        secit := Sections[sec].Items.Count - 1;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.Draw(graphics: TGPGraphics);
var
  g: TGPGraphics;
  p: TGPPen;
  clr: TColor;
  opc: Byte;
  mr: TGPRectF;
begin
  if not Assigned(MenuItem) or not Assigned(Menu) or (MenuItem.Menu.Sections.Count = 0) then
    Exit;

  if not MenuItem.Menu.UsesControls then
  begin
    g := graphics;
    if not Assigned(g) then
      g := CreateGraphics;
  end
  else
    g := TGPGraphics.Create(Canvas.Handle);

  g.SetSmoothingMode(SmoothingModeAntiAlias);
  case Menu.TextRendering of
    tAntiAlias: g.SetTextRenderingHint(TextRenderingHintAntiAlias);
    tAntiAliasGridFit: g.SetTextRenderingHint(TextRenderingHintAntiAliasGridFit);
    tClearType: g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  end;

  MenuItem.Menu.Draw(g, Bounds(0, 0, Width, Height));

  //transition line
  if not MenuItem.IsFloating then
  begin
    opc := 0;
    clr := clNone;
    with Menu.ItemAppearance do
    begin
      case MenuItemFillHover.GradientType of
        gtVertical:
        begin
          if MenuItemFillHover.ColorMirror = clnone then
          begin
            if MenuItemFillHover.ColorTo = clNone then
            begin
              clr := MenuItemFillHover.Color;
              opc := MenuItemFillHover.Opacity;
            end
            else
            begin
              case MenuItem.Menu.DropDownLocation of
                ddTopLeft, ddTopRight, ddTopCenter:
                begin
                  if MenuItemFillHover.Color <> clNone then
                  begin
                    clr := MenuItemFillHover.Color;
                    opc := MenuItemFillHover.Opacity;
                  end;
                end;
                ddBottomLeft, ddBottomRight, ddBottomCenter:
                begin
                  clr := MenuItemFillHover.ColorTo;
                  opc := MenuItemFillHover.OpacityTo;
                end;
              end;
            end;
          end
          else
          begin
            case MenuItem.Menu.DropDownLocation of
              ddTopLeft, ddTopRight, ddTopCenter:
              begin
                if MenuItemFillHover.Color <> clNone then
                begin
                  clr := MenuItemFillHover.Color;
                  opc := MenuItemFillHover.Opacity;
                end
                else if MenuItemFillHover.ColorMirror <> clNone then
                begin
                  clr := MenuItemFillHover.ColorMirror;
                  opc := MenuItemFillHover.OpacityMirror;
                end
              end;
              ddBottomLeft, ddBottomRight, ddBottomCenter:
              begin
                if MenuItemFillHover.ColorMirrorTo = clNone then
                begin
                  clr := MenuItemFillHover.ColorMirror;
                  opc := MenuItemFillHover.OpacityMirror;
                end
                else
                begin
                  clr := MenuItemFillHover.ColorMirrorTo;
                  opc := MenuItemFillHover.OpacityMirrorTo;
                end;
              end;
            end;
          end;
        end;
        gtSolid:
        begin
          clr := MenuItemFillHover.Color;
          opc := MenuItemFillHover.Opacity;
        end;
      end;
    end;

    if (clr <> clNone) and (opc > 0) and (clr = MenuItem.Menu.ContentFill.Color) then
    begin
      mr := MenuItem.FItemRect;
      p := TGPPen.Create(MakeColor(opc, clr), 5);
      case MenuItem.Menu.DropDownLocation of
        ddTopLeft: g.DrawLine(p, Width - menuitem.Menu.ContentFill.BorderWidth, Height, Width - mr.Width - (menuitem.Menu.ContentFill.BorderWidth * 2), Height);
        ddTopCenter: g.DrawLine(p, (Width - mr.Width) / 2, Height, (Width + mr.Width) / 2, Height);
        ddTopRight: g.DrawLine(p, menuitem.Menu.ContentFill.BorderWidth, Height, mr.Width + menuitem.Menu.ContentFill.BorderWidth, Height);
        ddBottomLeft: g.DrawLine(p, Width - menuitem.Menu.ContentFill.BorderWidth, 0, Width - mr.Width - (menuitem.Menu.ContentFill.BorderWidth * 2), 0);
        ddBottomCenter: g.DrawLine(p, (Width - mr.Width) / 2, 0, (Width + mr.Width) / 2, 0);
        ddBottomRight: g.DrawLine(p,  menuitem.Menu.ContentFill.BorderWidth, 0, mr.Width, 0);
      end;
      p.Free;
    end;
  end;

  if not MenuItem.Menu.UsesControls then
  begin
    if not Assigned(graphics) then
      g.Free;
  end
  else
    g.Free;
end;

procedure TAdvSmoothMegaMenuItemForm.FormShow(Sender: TObject);
begin
  //save parent left and top
  if Assigned(FMenu) and Assigned(FMenuItem) then
  begin
    FParentLeft := FMenu.Parent.Left;
    FParentTop := FMenu.Parent.Top;
    MenuItem.Menu.Init(ClientRect, true, true);
    AssignControls;
    UpdateWindow;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.Init;
begin
  DoubleBuffered := true;
  Visible := False;
  BorderIcons := [];
  BorderStyle := bsNone;
  Ctl3D := false;
  Color := clWhite;
  TabStop := Menu.TabStop;


  CreateMainBuffer;
  if Assigned(Menu) and Assigned(MenuItem) and not MenuItem.Menu.UsesControls then
    SetLayeredWindow;
  UpdateLayered;
end;

function TAdvSmoothMegaMenuItemForm.IsFormFloating: Boolean;
begin
  Result := (FPosX <> Left) or (FPosY <> Top);
end;

procedure TAdvSmoothMegaMenuItemForm.KeyDown(var Key: Word; Shift: TShiftState);
var
  sec, secit: integer;
  newitem: TCurrentItem;
  I, c: integer;
  ItemNormal: Boolean;
  cnt: Integer;
begin
  inherited;
  if not TabStop then
    Exit;

  if Assigned(FMenu) and Assigned(FMenuItem) then
  begin
    case Key of
      VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_RETURN:;
      else
      begin
        Menu.KeyDown(Key, Shift);
        Exit;
      end;
    end;

    case Menu.Direction of
      mdVertical:
      begin
        case Key of
          VK_UP, VK_DOWN:
          begin
            Menu.KeyDown(Key, Shift);
            Exit;
          end;
        end;
      end;
      mdHorizontal:
      begin
        case Key of
          VK_LEFT, VK_RIGHT:
          begin
            Menu.KeyDown(Key, Shift);
            Exit;
          end;
        end;
      end;
    end;

    case key  of
      VK_RETURN:
      begin
        if Assigned(MenuItem.Menu.SelectedItem.item) and Assigned(MenuItem.Menu.SelectedItem.section) then
          Menu.DoMenuSubItemClick(Self, MenuItem.Menu.SelectedItem.item, MenuItem.Menu.SelectedItem.item.Text)
        else
        begin
          MenuItem.CloseMenu;
          Menu.FHoveredItem := -1;
          Menu.FMenuAutoOpen := false;
        end;
      end;
    end;

    ItemNormal := false;
    for I := 0 to MenuItem.Menu.Sections.Count - 1 do
    begin
      if MenuItem.Menu.Sections[I].Visible  then
      begin
        for C := 0 to MenuItem.Menu.Sections[I].Items.Count - 1 do
        begin
          if MenuItem.Menu.Sections[I].Items[C].ItemType = itNormal then
          begin
            ItemNormal := true;
            break;
          end;
        end;
      end;
    end;

    if ItemNormal then
    begin
      with FMenuItem.Menu do
      begin
        if Assigned(SelectedItem.section) and Assigned(SelectedItem.item) then
        begin
          DoItemSelection(Key, SelectedItem, sec, secit);
          if (secit >= 0) and (secit <= Sections[sec].Items.Count -1) then
          begin
            newitem.section := Sections[sec];
            newitem.item := Sections[sec].Items[secit];
            SelectedItem := newitem;
          end;
        end
        else if (Sections.Count > 0) and (Key <> VK_RETURN) then
        begin
          if Sections[0].Items.Count > 0 then
          begin
            newitem.section := Sections[0];
            newitem.item := Sections[0].Items[0];
            SelectedItem := newitem;
          end;
        end;

        if Assigned(SelectedItem.item) and Assigned(SelectedItem.section) then
        begin
          cnt := 0;
          for I := 0 to SelectedItem.section.Items.Count - 1 do
          begin
            if (SelectedItem.item.ItemType = itNormal) and SelectedItem.section.Items[I].Enabled and SelectedItem.section.Items[I].Visible then
            begin
              Inc(cnt);
            end;
          end;


          if cnt > 0 then
          begin
            while (SelectedItem.item.ItemType <> itNormal) or (not SelectedItem.item.Enabled) or (not  SelectedItem.item.Visible) do
            begin
              DoItemSelection(Key, SelectedItem, Sec, Secit);
              newitem.section := Sections[sec];
              newitem.item := Sections[sec].Items[secit];
              SelectedItem := newitem;
            end;
          end;
          MenuItem.MenuChanged(Self);
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TAdvSmoothMegaMenuItemForm.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Assigned(MenuItem) then
    MenuItem.Menu.DoMouseMove(Shift, X, Y);
end;

procedure TAdvSmoothMegaMenuItemForm.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned(MenuItem) then
    MenuItem.Menu.DoMouseUp(Button, Shift, X, Y);
end;

procedure TAdvSmoothMegaMenuItemForm.Paint;
begin
  inherited;
  if Assigned(FMenuItem) and FMenuItem.Menu.UsesControls then
    UpdateWindow;
end;

procedure TAdvSmoothMegaMenuItemForm.SetLayeredWindow;
begin
  if GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  UpdateLayered;
end;

procedure TAdvSmoothMegaMenuItemForm.SetPosition(PosX: integer = -1; PosY: integer = -1);
var
  x, y, w, h: integer;
begin
  if Assigned(Menu) and Assigned(MenuItem) then
  begin
    if (PosX = -1) and (PosY = -1) then
    begin
      x := Round(MenuItem.FItemRect.X);
      y := Round(MenuItem.FItemRect.Y);
      w := Round(MenuItem.FItemRect.Width);
      h := Round(MenuItem.FItemRect.Height);

      case MenuItem.Menu.DropDownLocation of
        ddCustom :
        begin
          x := x + MenuItem.Menu.DropDownLeft;
          y := y + MenuItem.Menu.DropDownTop;
        end;
        ddTopLeft:
        begin
          x := x - Width + w + 1;
          y := y - Height + 1;
        end;
        ddTopCenter:
        begin
          x := x + (w div 2) - (Width div 2);
          y := y - Height + 1;
        end;
        ddTopRight:
        begin
          y := y - Height + 1;
          x := x - 1;
        end;
        ddLeftCenterTop:
        begin
          x := x - Width;
          y := y - Height + h + 1;
        end;
        ddLeftCenterCenter:
        begin
          x := x - Width;
          y := y + (h div 2) - (Height div 2);
        end;
        ddLeftCenterBottom:
        begin
          x := x - Width;
          Y := y - 1;
        end;
        ddRightCenterTop:
        begin
          x := x + w ;
          y := y - Height + h + 1;
        end;
        ddRightCenterCenter:
        begin
          x := x + w ;
          y := y + (h div 2) - (Height div 2);
        end;
        ddRightCenterBottom:
        begin
          x := x + w ;
          y := y - 1;
        end;
        ddBottomLeft:
        begin
          x := x - Width + w + 1;
          y := y + h - 1;
        end;
        ddBottomCenter:
        begin
          x := x + (w div 2) - (Width div 2);
          y := y + h - 1;
        end;
        ddBottomRight:
        begin
          y := y + h - 1;
          x := x - 1;
        end;
      end;
      SetBounds(FMenu.Parent.ClientOrigin.X + FMenu.Left + x ,
        FMenu.Parent.ClientOrigin.Y + FMenu.Top + y, Width, Height);
    FPosX := Self.Left;
    FPosY := Self.Top;
    end
    else
    begin
      SetBounds(PosX ,
        PosY, Width, Height);
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.UpdateForm;
begin
  if assigned(FMenuItem) then
  begin
    if FMenuItem.Menu.NeedsUpdate then
    begin
      FMenuItem.UpdateFormSize;
      FMenuItem.Menu.Init(ClientRect, false, true);
      FMenuItem.Menu.NeedsUpdate := false;
    end;
    AssignControls;
    if not FMenuItem.Menu.UsesControls then
      UpdateLayered
    else
      Invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.UpdateLayered;
begin
  ClearBuffer(nil);

  SetWindowPos(Self.Handle, HWND_TOP, 0, 0, 0, 0,
    SWP_NOMOVE or SWP_NOSIZE or SWP_FRAMECHANGED or SWP_NOACTIVATE);

  Draw(nil);

  UpdateMainWindow(255);
end;

procedure TAdvSmoothMegaMenuItemForm.UpdateMainWindow;
var
  ScrDC, MemDC: HDC;
  BitmapHandle, PrevBitmap: HBITMAP;
  BlendFunc: _BLENDFUNCTION;
  Size: TSize;
  P, S: TPoint;
begin
  ScrDC := CreateCompatibleDC(0);
  MemDC := CreateCompatibleDC(ScrDC);

  FMainBuffer.GetHBITMAP(0, BitmapHandle);
  PrevBitmap := SelectObject(MemDC, BitmapHandle);
  Size.cx := Width;
  Size.cy := Height;
  P := Point(Left, Top);
  S := Point(0, 0);

  with BlendFunc do
  begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := 255;
    AlphaFormat := AC_SRC_ALPHA;
  end;

  UpdateLayeredWindow(Handle, ScrDC, @P, @Size, MemDC, @S, 0, @BlendFunc, ULW_ALPHA);

  SelectObject(MemDC, PrevBitmap);
  DeleteObject(BitmapHandle);

  DeleteDC(MemDC);
  DeleteDC(ScrDC);
end;

procedure TAdvSmoothMegaMenuItemForm.UpdateWindow;
begin
  CreateMainBuffer;
  UpdateLayered;
end;

procedure TAdvSmoothMegaMenuItemForm.WMActivate(var Msg: TWMActivate);
var
  Last, P: HWnd;
  pt: TPoint;
begin
  if not ContainsControl(ActiveControl) then
  begin
    if Assigned(Menu) and Assigned(MenuItem) then
    begin
      P := {GetParent}((Owner as TCustomControl).Handle);
      Last := P;
      while P <> 0 do
      begin
        Last := P;
        P := GetParent(P);
      end;
      SendMessage(Last, WM_NCACTIVATE, 1, 0);

      if not MenuItem.FFormDestroying then
      begin
        if Msg.Active = 0 then
        begin
          if Assigned(Menuitem.frm) then
          begin
            //simulate mousedown when clicked on menu
            pt := MenuItem.FOwner.ScreenToClient(Mouse.CursorPos);
            MenuItem.FOwner.SelectOnly := true;
            MenuItem.FOwner.MouseDown(mbLeft, [], pt.X, pt.Y);
            MenuItem.FOwner.SelectOnly := false;
            FMenu.FLastHoveredItem := FMenu.FHoveredItem;
            FMenu.FHoveredItem := -1;
            FMenu.FMenuAutoOpen := false;
            FMenu.FMenuIsOpen := false;
            menuitem.CloseMenu;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  inherited;
end;

procedure TAdvSmoothMegaMenuItemForm.WMMouseActivate(var Msg: TWMMouseActivate);
begin
  msg.result := MA_NOACTIVATE;
end;

procedure TAdvSmoothMegaMenuItemForm.WMMove(var Msg: TWMMove);
begin
  inherited;
  if Assigned(Menu) and Assigned(MenuItem) then
  begin
    if (not FFloatingEvent and MenuItem.IsFloating) or (not FFloatingEvent and MenuItem.FWillFloat and not MenuItem.IsFloating) then
    begin
      MenuItem.FFloating := True;
      MenuItem.FWillFloat := False;
      FFloatingEvent := true;
      menu.FMenuAutoOpen := false;

      if Assigned(Menu.OnMenuItemFloatChanged) then
        Menu.OnMenuItemFloatChanged(Menu, true, MenuItem);

      MenuItem.FOwner.FMenuItemRecall := MenuItem;
      PostMessage(Menuitem.FOwner.Handle, WM_RECALLFLOATINGFORM, 0, 0);

    end;

    if not Menuitem.Menu.UsesControls then
      UpdateLayered
    else
      invalidate;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
begin
  inherited;

  if Assigned(FMenuItem) and Assigned(FMenu) then
  begin
    if not FMenuItem.Menu.TearOff then
      Exit;

    pt := ScreenToClient(Point(msg.XPos, msg.YPos));
    if (pt.Y < MenuItem.Menu.TearOffSize)  then
    begin
      Msg.Result := HTCAPTION;
      FMenuItem.FWillFloat := True;
    end
    else
      FMenuItem.FWillFloat := False;
  end;
end;

procedure TAdvSmoothMegaMenuItemForm.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TAdvSmoothMegaMenuItemForm.WMSetCursor(var MSG: TWMSetCursor);
var
  pt: TPoint;
begin
  inherited;
  if Assigned(FMenuItem) then
  begin
    if not FMenuItem.Menu.TearOff then
      Exit;

    GetCursorPos(Pt);
    Pt := ScreenToClient(Pt);
    if (pt.Y < FMenuItem.Menu.TearOffSize) then
      SetCursor(Screen.Cursors[crSize]);
  end;
end;

end.
