{***************************************************************************}
{ TAdvOfficePager component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2015                                        }
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

unit AdvOfficePager;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Math, Menus,
  Dialogs, Forms, ImgList, CommCtrl, ExtCtrls, ComCtrls, AdvGDIP, GDIPicture,
  AdvHintInfo, AdvGlowButton, ACXPVS, AdvDWM, AdvStyleIf, Types, AxCtrls
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

{$R ADVOFFICEPAGER.RES}

{$DEFINE TMS_DWM}

const
  GLOWSPEED = 50;
  GLOWSTEP = 20;
  IMG_SPACE = 2;
  DropDownSectWidth = 13;
  DEFAULT_TABHEIGHT = 26;
  ADVPAGE_OFFSET = 1;
  SCROLLER_SIZE = 32;
  PAGEBUTTON_SIZE = 18;

  MAJ_VER = 3; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.1.0.0 : fixed issue with TAdvGlowButton painting on pager
  //         : fixed issue with anchored controls
  //         : fixed issue with page reorder in object treeview
  //         : added OnResize event
  //         : added FreeOnClose property
  // 1.1.1.0 : added PopupMenu property on TAdvOfficePage
  // 1.1.1.1 : fixed issue with tabs with TabEnabled = false
  // 1.1.2.0 : added new property PageIndex
  // 1.2.0.0 : New: keybooard support added
  //         : New: shortcuthints added
  //         : New: page OnShow event added
  // 1.2.0.1 : Fixed issue with form windows hook proc
  // 1.2.0.2 : Fixed issue with return value of AddAdvPage
  // 1.2.0.3 : Fixed issue with use of OnChanging event during component load
  //         : Fixed issue with Tab key handling
  // 1.2.0.4 : Fixed issue with hidden tabs & scrolling
  // 1.2.0.5 : Fixed issue with resizing control to very small sizes
  // 1.2.1.0 : New : OnClosedPage event added
  //         : New : drag & drop events added for TAdvOfficePager
  // 1.2.1.1 : Fixed issue with TabVisible and close button on tab
  // 1.2.1.2 : Improved : minor border drawing cosmetic issue
  // 1.2.1.3 : Improved : prevented reentrance in page close
  // 1.2.1.4 : Fixed : issue with styler change and button appearance update
  // 1.2.1.5 : Fixed : issue with FindNextPage
  // 1.2.1.6 : Fixed : issue with destroy during timer activity
  // 1.3.0.0 : New : ImagePosition can be set in TabSettings as left, top, bottom, right from tab caption
  //           New : Wordwrapped tab caption support
  //           New : autoscrolling pages while mouse is down on scroll buttons
  //           New : ScrollButtonsAlways property to show scroll buttons always irrespective of nr. of tabs
  //           New : OnDrawTab event for custom tab drawing
  //           New : fixed tab width setting with ellipsis text for non fitting tab caption
  // 1.3.0.1 : Fixed : issue with destroy of TAdvOfficePagerOfficeStyler at design time
  // 1.3.1.0 : New : added event OnHide to TAdvOfficePage
  // 1.3.1.1 : Fixed : issue with sequence on event OnClosedPage
  // 1.3.1.2 : Fixed : issue with OnChange event during loading
  // 1.3.2.0 : New : rotated prev/next buttons in left/right tab mode
  //         : Improved : image/text alignment in rotated tabs left/right
  // 1.3.2.1 : Fixed : issue with tabscroller update from setting TabVisible
  // 1.3.2.2 : Improved : position of text in tab with AntiAlias = aaNone
  // 1.3.2.3 : Fixed : issue with painting text on tab with fixed with & close button
  // 1.3.4.0 : New : Supports invisible tabs (with TabHeight = 0)
  // 1.3.5.0 : Improved : painting performance
  // 1.3.5.1 : Fixed : issue with OnShow event sequence
  // 1.3.5.2 : Fixed : issue with drawing imagelist images in tabs
  // 1.4.0.0 : New : Optional TabAppearance & PageAppearance control per page
  //         : New : Optional control on close button per page
  //         : New : left, right or left/right rounded tabs
  // 1.4.0.1 : Fixed : issue with copy & paste at design time
  // 1.4.1.0 : New : function IndexOfTabAt added
  // 1.4.1.1 : Fixed : small issue with OnChange/OnChanging event after removing all pages
  // 1.4.2.0 : New : AdvOfficePager.TabScroller public property exposed
  // 1.4.2.1 : Fixed : issue with OnPictureChanged handling
  // 1.5.0.0 : New : support for unicode captions
  // 1.5.1.0 : Improved : gradient, highlight & shadow painting improvements
  // 1.5.1.1 : Improved : GDI+ drawing
  // 1.5.2.0 : New : public property BufferedPages added
  // 1.5.3.0 : New : property Glow added
  // 1.5.3.1 : Fixed : issue with PageListMenu when WideCaptions are used
  // 1.6.0.0 : New : DockSite support
  // 1.6.0.1 : Fixed : issue with font styles on right positioned tabs
  // 1.6.0.2 : Fixed : issue with possible double auto creation of pages
  // 1.6.1.0 : Improved : undocking through tab
  // 1.6.2.0 : New : TabSettings.Alignment property for alignment control in fixed width tabs
  // 1.6.2.1 : Fixed : issue with font on disabled tabs
  // 1.6.2.2 : Fixed : issue with wordwrapped tabs
  // 1.6.2.3 : Fixed : issue with docking TTntForms on TAdvOfficePager
  // 1.6.2.4 : Fixed : issue with destroying pages in Delphi 7 or older
  // 1.6.2.5 : Fixed : issue with center alignment on left/right horiz. tabs
  // 1.6.2.6 : Fixed : issue with tab width calculation for wordwrapped tabs
  // 1.6.2.7 : Fixed : issue with tab width calculation for WideCaption
  // 1.6.2.8 : Improved : accelerator keys for wide captions also handled
  // 1.6.3.0 : New : exposed public property HotPageIndex
  // 1.6.4.0 : New : exposed OnClick / OnDblClick event on TAdvOfficePage
  // 1.6.4.1 : Fixed : update of tabs when TabAppearance.Font & tab picture changes at runtime
  // 1.6.4.2 : Fixed : issue with control aligning in Delphi 2009
  // 1.6.5.0 : New : method CloseActivePage added
  // 1.6.5.1 : Fixed : tab width calculation for large font display setting
  // 1.7.0.0 : New : capability to have a checkbox per tab
  //         : New : capability to have a close button on each tab instead of active tab only
  //         : New : capability to have First/Last button in scroll area
  // 1.7.0.1 : Fixed : issue with ShowClose = false and ShowCloseOnNonSelectedTabs
  // 1.7.1.0 : New : Events OnFirstClick, OnLastClick, OnPrevClick, OnNextClick added
  // 1.7.1.1 : Fixed : issue with Free of page
  // 1.7.1.2 : Fixed : small tab rectangle calculation issue
  // 1.7.1.3 : Fixed : issue with closing page when FreeOnClose = true
  //         : Fixed : issue with updating tabs after tab styler font change
  // 1.7.1.4 : Fixed : issue with closing tabs by code
  // 1.7.2.0 : Improved : when page BorderColor is set to clNone, no border is drawn
  // 1.8.0.0 : New : Windows Vista, Windows 7 & Terminal styles
  // 1.8.1.0 : New : property Transparent added
  // 1.8.1.1 : Improved : drawing of top & bottom tabs with ImagePosition set to ipTop/ipBottom
  // 1.8.2.0 : New : added support to switch active tab with Ctrl-Tab
  // 1.8.2.1 : Fixed : issue with setting focus from OnShow event
  // 1.8.2.2 : Fixed : issue with changing active page for pages without controls
  // 1.8.2.3 : Fixed : issue with scrolling tabs with images
  // 1.8.2.4 : Fixed : issue with focusing active control on page
  // 1.8.2.5 : Fixed : issue with changing focused control from the OnChange event
  // 1.8.2.6 : Fixed : issue with use with inherited forms
  // 1.8.2.7 : Fixed : issue with TabReorder and regular drag & drop
  // 1.8.2.8 : Fixed : issue with tab hints
  // 1.8.2.9 : Fixed : issue with selecting tab after deleting tab
  // 1.8.2.10: Fixed : issue with setting Visible
  // 1.8.2.11: Fixed : issue with transparent and some specific styles for non selected tab text color
  // 1.8.2.12: Fixed : issue with invisible tabs during design time
  // 1.9.0.0 : New : Office 2010 color styles added
  //         : New : Tab icons also used in tab picker dropdown menu
  // 2.0.0.0 : New : Close page list button

  // 2.1.0.0 : New : TabSettings.Glass added
  //           New : TabSettings.EndMargin added
  //           New : FixedTabs property added to have always fixed / non scrolling tabs
  //           New : Progress indication on tab added
  //           New : OnTabMouseLeave / OnTabMouseEnter events added
  //           New : Indicator during tab reorder added to indicate new position
  //           New : Glow / GlowColor property added per page as indicator for a page
  //           Improved : ImageList image positioning in bottom layout on left/right tab

  // 2.1.0.1 : Fixed : Issue with EndMargin when adding pages at runtime
  // 2.1.0.2 : Fixed : Issue with SelectFirstControl call wrt OnShow event
  // 2.1.0.3 : Fixed : Issue with using Page.OnShow event to set focused control
  // 2.1.1.0 : New : Exposed DrawTab() as a public function
  // 2.1.2.0 : New : Exposed DrawTabContent() as public function

  // 2.5.0.0 : New : Chrome style close button
  //         : New : Support for undocking tabs
  //         : New : Optional Insert tab button
  //         : New : Automatic theme adaption with Office 2007 / 2010
  //         : New : ButtonSize to control size of buttons and capability to make it bigger for touchscreens
  //         : New : Capability to persist tab order & visibility to file
  //         : New : ClearAdvPages method added to remove all pages
  //         : Improved : Support to be used on glass frame

  // 2.5.0.1 : Improved : Set AllowTabUndock = false by default, Set ShowClose on TAdvOfficePage false by default
  // 2.5.1.0 : New : FloatingBorderIcons property added
  // 2.5.2.0 : Improved : Support for auto dock back in AddAdvPage()
  //         : New : Exposed Touch & OnGesture
  // 2.5.2.1 : Fixed : Issue with progress bar on tab when Max = 0
  // 2.5.2.2 : Fixed : Issue with AllowTabUndock and dock back
  // 2.5.3.0 : New : OnInsertPage event added
  //         : New : DoInsertButtonClick method made virtual
  //         : New : AdvPageVisibleCount property added
  //         : Fixed : Issue with checked tabs when RotateTabLeftRight = false
  // 2.5.4.0 : New : OnResize event for floating pageform
  // 2.5.5.0 : New : AdvOfficePage.SetFloating() method added
  // 2.5.6.0 : New : Exposed OnShow,OnHide,OnActivate,OnDeactivate for the floating pageform
  // 3.0.0.0 : New : Metro style
  //         : New : Settings persistence
  //         : New : TabIndex property added on page level
  //         : New : ResetPages method added
  //         : New : PageByName, PageByCaption, PageByTabIndex functions added
  //         : Improved : SaveToFile/LoadFromFile also loads page floating state. Breaking change for file format!
  // 3.0.0.1 : Fixed : Issue with setting ActivePageIndex & scrolling when FixedTabs > 0
  // 3.0.0.2 : Fixed : Issue with painting tabs in area with scrollers in rare circumstances
  // 3.0.0.3 : Fixed : Issue with close button position on tabs with centering
  // 3.0.0.4 : Fixed : Issue with OnShow event handling when closing pages
  // 3.1.0.0 : New : Multiline tab capability added
  // 3.2.0.0 : New : Windows 8, Office 2013 styles added
  // 3.2.0.1 : Fixed : Issue with multiline tabs & tab undock
  //         : Fixed : Issue with resize & multiline tab when there is only one page
  // 3.3.0.0 : New : Badges on page tabs
  // 3.3.0.1 : Fixed : Issue with tab font on glass for Whidbey style
  // 3.3.0.2 : Fixed : Issue with close button position not updating when tab resizes
  // 3.3.0.3 : Fixed : Rare issue with undocking & hidden pages
  // 3.3.0.4 : Fixed : Rare issue with undocking & active tab
  // 3.3.1.0 : New : TabSettings.ButtonBorder property added
  // 3.3.2.0 : New : Public property RowCount exposed for multiline tab pagers
  // 3.3.2.1 : Fixed : Issue with fixed tabs & reorder
  // 3.3.2.2 : Fixed : Issue with SelectNext() and hidden tabs
  // 3.3.2.3 : Fixed : Issue with closing last tab
  //         : Fixed : Issue with centered wordwrapped tab text drawing
  //         : Fixed : Issue with arrow left key handling
  // 3.3.3.0 : New : Public PageObject property added on page
  //         : Fixed : Issue with centered caption drawing on fixed width tabs
  // 3.3.3.1 : Fixed : Issue with badge painting in pager position Left,Right,Bottom
  // 3.4.0.0 : New : Windows 10, Office 2016 styles added
  // 3.4.1.0 : New : OnTabUndocking event added
  // 3.4.1.1 : Improved : Painting behavior when resizing pages
  // 3.4.1.2 : Fixed : Issue with transparent controls on page
  // 3.5.0.0 : New : Support for HighDPI
  //         : Fixed : Issue with closing tabs in particular orders

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TAdvOfficePager = class;
  TAdvOfficePage = class;

  TGradientDirection = (gdHorizontal, gdVertical);
  TGlowState = (gsHover, gsPush, gsNone);
  TButtonLayout = (blGlyphLeft, blGlyphTop, blGlyphRight, blGlyphBottom);
  TImagePosition = (ipLeft, ipTop, ipRight, ipBottom);
  TDropDownPosition = (dpRight, dpBottom);
  //TGDIPGradient = (ggRadial, ggVertical, ggDiagonalForward, ggDiagonalBackward);
  TCloseOnTabPos = (cpRight, cpLeft);
  TAdvTabShape =  (tsRectangle, tsLeftRamp, tsRightRamp, tsLeftRightRamp);
  TCloseButtonLook = (cblOffice, cblChrome);

  TTabRounding = 0..8;

  TPagerTabSettings = class(TPersistent)
  private
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FOnChange: TNotifyEvent;
    FHeight: Integer;
    FStartMargin: Integer;
    FEndMargin: Integer;
    FSpacing: Integer;
    FWidth: Integer;
    FWordWrap: Boolean;
    FImagePosition: TImagePosition;
    FRounding: TTabRounding;
    FShape: TAdvTabShape;
    FAlignment: TAlignment;
    FGlass: Boolean;
    FPager: TAdvOfficePager;
    FButtonBorder: boolean;
    procedure SetLeftMargin(const Value: Integer);
    procedure SetRightMargin(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetStartMargin(const Value: Integer);
    procedure SetEndMargin(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetImagePosition(const Value: TImagePosition);
    procedure SetRounding(const Value: TTabRounding);
    procedure SetShape(const Value: TAdvTabShape);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetGlass(const Value: Boolean);
    procedure SetButtonBorder(const Value: boolean);
  protected
    procedure Changed;
    property Pager: TAdvOfficePager read FPager;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonBorder: boolean read FButtonBorder write SetButtonBorder default true;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 4;
    property RightMargin: Integer read FRightMargin write SetRightMargin default 4;
    property StartMargin: Integer read FStartMargin write SetStartMargin default 4;
    property EndMargin: Integer read FEndMargin write SetEndMargin default 0;
    property Height: Integer read FHeight write SetHeight default 26;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Width: Integer read FWidth write SetWidth default 0;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property ImagePosition: TImagePosition read FImagePosition write SetImagePosition default ipLeft;
    property Shape: TAdvTabShape read FShape write SetShape default tsRectangle;
    property Rounding: TTabRounding read FRounding write SetRounding default 1;
    property Glass: Boolean read FGlass Write SetGlass default False;
  end;

  TGradientBackground = class(TPersistent)
  private
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FDirection: TGradientDirection;
    FOnChange: TNotifyEvent;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetDirection(const Value: TGradientDirection);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
  protected
  public
    constructor Create; 
    procedure Assign(Source: TPersistent); override;  
  published
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property Direction: TGradientDirection read FDirection write SetDirection;
    property Steps: Integer read FSteps write SetSteps default 64;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TVistaBackground = class(TPersistent)
  private
    FSteps: Integer;
    FColor: TColor;
    FColorTo: TColor;
    FOnChange: TNotifyEvent;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FBorderColor: TColor;
    FGradientMirror: TGDIPGradient;
    FGradient: TGDIPGradient;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetSteps(const Value: Integer);
    procedure Changed;
    procedure SetBorderColor(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetGradient(const Value: TGDIPGradient);
    procedure SetGradientMirror(const Value: TGDIPGradient);
  protected
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorMirror: TColor read FColorMirror write SetColorMirror;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo;
    property Gradient: TGDIPGradient read FGradient write SetGradient;
    property GradientMirror: TGDIPGradient read FGradientMirror write SetGradientMirror;
    property Steps: Integer read FSteps write SetSteps default 64;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomTabAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderColorHot: TColor;
    FColor: TColor;
    FColorTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorSelectedTo: TColor;
    FBorderColorDisabled: TColor;
    FBorderColorSelected: TColor;
    FColorDisabled: TColor;
    FColorDisabledTo: TColor;
    FColorSelected: TColor;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FColorMirrorHot: TColor;
    FColorMirrorHotTo: TColor;
    FGradientMirror: TGDIPGradient;
    FGradientMirrorHot: TGDIPGradient;
    FGradient: TGDIPGradient;
    FGradientHot: TGDIPGradient;
    FColorMirrorDisabledTo: TColor;
    FColorMirrorDisabled: TColor;
    FColorMirrorSelectedTo: TColor;
    FColorMirrorSelected: TColor;
    FGradientSelected: TGDIPGradient;
    FGradientDisabled: TGDIPGradient;
    FGradientMirrorSelected: TGDIPGradient;
    FGradientMirrorDisabled: TGDIPGradient;
    FTextColorDisabled: TColor;
    FTextColorSelected: TColor;
    FTextColor: TColor;
    FTextColorHot: TColor;
    FBackGround: TGradientBackground;
    FBorderColorSelectedHot: TColor;
    FBorderColorDown: TColor;
    FFont: TFont;
    FHighLightColorHot: TColor;
    FShadowColor: TColor;
    FHighLightColorDown: TColor;
    FHighLightColorSelected: TColor;
    FHighLightColorSelectedHot: TColor;
    FHighLightColor: TColor;
    FOnFontChange: TNotifyEvent;
    procedure OnBackGroundChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    procedure SetBackGround(const Value: TGradientBackground);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorDisabled(const Value: TColor);
    procedure SetBorderColorSelected(const Value: TColor);
    procedure SetBorderColorSelectedHot(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorDisabledTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorDisabled(const Value: TColor);
    procedure SetColorMirrorDisabledTo(const Value: TColor);
    procedure SetColorMirrorHot(const Value: TColor);
    procedure SetColorMirrorHotTo(const Value: TColor);
    procedure SetColorMirrorSelected(const Value: TColor);
    procedure SetColorMirrorSelectedTo(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetColorSelected(const Value: TColor);
    procedure SetColorSelectedTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradient(const Value: TGDIPGradient);
    procedure SetGradientDisabled(const Value: TGDIPGradient);
    procedure SetGradientHot(const Value: TGDIPGradient);
    procedure SetGradientMirror(const Value: TGDIPGradient);
    procedure SetGradientMirrorDisabled(const Value: TGDIPGradient);
    procedure SetGradientMirrorHot(const Value: TGDIPGradient);
    procedure SetGradientMirrorSelected(const Value: TGDIPGradient);
    procedure SetGradientSelected(const Value: TGDIPGradient);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextColorDisabled(const Value: TColor);
    procedure SetTextColorHot(const Value: TColor);
    procedure SetTextColorSelected(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetFont(const Value: TFont);
  protected
    procedure Changed;
    procedure ClearValues;
    property BackGround: TGradientBackground read FBackGround write SetBackGround;
    property OnFontChange: TNotifyEvent read FOnFontChange write FOnFontChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderColorHot: TColor read FBorderColorHot write FBorderColorHot;
    property BorderColorSelected: TColor read FBorderColorSelected write SetBorderColorSelected;
    property BorderColorSelectedHot: TColor read FBorderColorSelectedHot write SetBorderColorSelectedHot;
    property BorderColorDisabled: TColor read FBorderColorDisabled write SetBorderColorDisabled;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown;
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorSelected: TColor read FColorSelected write SetColorSelected;
    property ColorSelectedTo: TColor read FColorSelectedTo write SetColorSelectedTo;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled;
    property ColorDisabledTo: TColor read FColorDisabledTo write SetColorDisabledTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property ColorMirror: TColor read FColorMirror write SetColorMirror;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo;
    property ColorMirrorHot: TColor read FColorMirrorHot write SetColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write SetColorMirrorHotTo;
    property ColorMirrorSelected: TColor read FColorMirrorSelected write SetColorMirrorSelected;
    property ColorMirrorSelectedTo: TColor read FColorMirrorSelectedTo write SetColorMirrorSelectedTo;
    property ColorMirrorDisabled: TColor read FColorMirrorDisabled write SetColorMirrorDisabled;
    property ColorMirrorDisabledTo: TColor read FColorMirrorDisabledTo write SetColorMirrorDisabledTo;
    property Font: TFont read FFont write SetFont;
    property Gradient: TGDIPGradient read FGradient write SetGradient;
    property GradientMirror: TGDIPGradient read FGradientMirror write SetGradientMirror;
    property GradientHot: TGDIPGradient read FGradientHot write SetGradientHot;
    property GradientMirrorHot: TGDIPGradient read FGradientMirrorHot write SetGradientMirrorHot;
    property GradientSelected: TGDIPGradient read FGradientSelected write SetGradientSelected;
    property GradientMirrorSelected: TGDIPGradient read FGradientMirrorSelected write SetGradientMirrorSelected;
    property GradientDisabled: TGDIPGradient read FGradientDisabled write SetGradientDisabled;
    property GradientMirrorDisabled: TGDIPGradient read FGradientMirrorDisabled write SetGradientMirrorDisabled;
    property TextColor: TColor read FTextColor write SetTextColor;
    property TextColorHot: TColor read FTextColorHot write SetTextColorHot;
    property TextColorSelected: TColor read FTextColorSelected write SetTextColorSelected;
    property TextColorDisabled: TColor read FTextColorDisabled write SetTextColorDisabled;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
    property HighLightColor: TColor read FHighLightColor write FHighLightColor;
    property HighLightColorHot: TColor read FHighLightColorHot write FHighLightColorHot;
    property HighLightColorSelected: TColor read FHighLightColorSelected write FHighLightColorSelected;
    property HighLightColorSelectedHot: TColor read FHighLightColorSelectedHot write FHighLightColorSelectedHot;
    property HighLightColorDown: TColor read FHighLightColorDown write FHighLightColorDown;
  end;

  TTabAppearance = class(TCustomTabAppearance)
  private
  protected
  public
  published
    property BackGround;
  end;

  TPagerTabScroller = class(TObject)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FVisible: Boolean;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
  public
    constructor Create;
    function CanGoForward: Boolean;
    function CanGoBack: Boolean;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read FPosition write SetPosition;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx; default;
  end;

  TPageButtonSettings = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FPageListButton: boolean;
    FCloseButton: Boolean;
    FScrollButtonNextPicture: TGDIPPicture;
    FScrollButtonPrevPicture: TGDIPPicture;
    FPageListButtonPicture: TGDIPPicture;
    FCloseButtonPicture: TGDIPPicture;
    FScrollButtonPrevHint: String;
    FScrollButtonNextHint: String;
    FCloseButtonHint: String;
    FPageListButtonHint: String;
    FScrollButtonsAlways: Boolean;
    FScrollButtonFirstPicture: TGDIPPicture;
    FScrollButtonLastPicture: TGDIPPicture;
    FScrollButtonFirstHint: String;
    FScrollButtonLastHint: String;
    FLastButton: Boolean;
    FFirstButton: Boolean;
    FClosedListButtonHint: String;
    FClosedListButtonPicture: TGDIPPicture;
    FClosedListButton: Boolean;
    FCloseButtonLook: TCloseButtonLook;
    FButtonSize: Integer;
    FShowInsertButton: Boolean;
    FInsertButtonHint: String;
    procedure Changed;
    procedure OnPictureChanged(Sender: TObject);
    procedure SetCloseButton(const Value: Boolean);
    procedure SetCloseButtonPicture(const Value: TGDIPPicture);
    procedure SetPageListButton(const Value: boolean);
    procedure SetPageListButtonPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonNextPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonPrevPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonsAlways(const Value: Boolean);
    procedure SetScrollButtonFirstPicture(const Value: TGDIPPicture);
    procedure SetScrollButtonLastPicture(const Value: TGDIPPicture);
    procedure SetFirstButton(const Value: Boolean);
    procedure SetLastButton(const Value: Boolean);
    procedure SetClosedListButtonPicture(const Value: TGDIPPicture);
    procedure SetClosedListButton(const Value: Boolean);
    procedure SetCloseButtonLook(const Value: TCloseButtonLook);
    procedure SetButtonSize(const Value: Integer);
    procedure SetShowInsertButton(const Value: Boolean);
  protected
    FScrollButtonDownPicture, FScrollButtonDownLastPicture: TGDIPPicture;
    FScrollButtonUpPicture, FScrollButtonUpFirstPicture: TGDIPPicture;
    FPrevPictureChanged: Boolean;
    FNextPictureChanged: Boolean;
    FFirstPictureChanged: Boolean;
    FLastPictureChanged: Boolean;
    //property NextPictureChanged: Boolean read FNextPictureChanged write SetNextPictureChanged;
    //property PrevPictureChanged: Boolean read FPrevPictureChanged write SetPrevPictureChanged;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CloseButton: Boolean read FCloseButton write SetCloseButton default false;
    property PageListButton: Boolean read FPageListButton write SetPageListButton default false;
    property ClosedListButton: Boolean read FClosedListButton write SetClosedListButton default false;
    property CloseButtonPicture: TGDIPPicture read FCloseButtonPicture write SetCloseButtonPicture;
    property PageListButtonPicture: TGDIPPicture read FPageListButtonPicture write SetPageListButtonPicture;
    property ScrollButtonPrevPicture: TGDIPPicture read FScrollButtonPrevPicture write SetScrollButtonPrevPicture;
    property ScrollButtonNextPicture: TGDIPPicture read FScrollButtonNextPicture write SetScrollButtonNextPicture;
    property ScrollButtonFirstPicture: TGDIPPicture read FScrollButtonFirstPicture write SetScrollButtonFirstPicture;
    property ScrollButtonLastPicture: TGDIPPicture read FScrollButtonLastPicture write SetScrollButtonLastPicture;
    property ClosedListButtonPicture: TGDIPPicture read FClosedListButtonPicture write SetClosedListButtonPicture;
    property CloseButtonHint: String read FCloseButtonHint write FCloseButtonHint;
    property InsertButtonHint: String read FInsertButtonHint write FInsertButtonHint;
    property PageListButtonHint: String read FPageListButtonHint write FPageListButtonHint;
    property ScrollButtonsAlways: Boolean read FScrollButtonsAlways write SetScrollButtonsAlways default False;
    property ScrollButtonNextHint: String read FScrollButtonNextHint write FScrollButtonNextHint;
    property ScrollButtonPrevHint: String read FScrollButtonPrevHint write FScrollButtonPrevHint;
    property ScrollButtonFirstHint: String read FScrollButtonFirstHint write FScrollButtonFirstHint;
    property ScrollButtonLastHint: String read FScrollButtonLastHint write FScrollButtonLastHint;
    property ClosedListButtonHint: String read FClosedListButtonHint write FClosedListButtonHint;
    property FirstButton: Boolean read FFirstButton write SetFirstButton default false;
    property LastButton: Boolean read FLastButton write SetLastButton default false;
    property CloseButtonLook: TCloseButtonLook read FCloseButtonLook write SetCloseButtonLook default cblOffice;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default PAGEBUTTON_SIZE;
    property ShowInsertButton: Boolean read FShowInsertButton write SetShowInsertButton default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCustomAdvOfficePagerStyler = class(TComponent)
  private
    FControlList: TDbgList;
    FTabAppearance: TTabAppearance;
    FPageAppearance: TVistaBackground;
    FRoundEdges: Boolean;
    FAutoThemeAdapt: boolean;
    FGlowButtonAppearance: TGlowButtonAppearance;
    FBlendFactor: Integer;
    FShowShadow: Boolean;
    FShow3D: Boolean;
    FTabRounding: TTabRounding;
    procedure OnTabAppearanceChanged(Sender: TObject);
    procedure OnTabAppearanceFontChanged(Sender: TObject);
    procedure OnPageAppearanceChanged(Sender: TObject);
    procedure OnGlowButtonAppearanceChanged(Sender: TObject);
    procedure SetRoundEdges(const Value: boolean);
    procedure SetTabAppearance(const Value: TTabAppearance);
    procedure SetPageAppearance(const Value: TVistaBackground);
    procedure SetGlowButtonAppearance(const Value: TGlowButtonAppearance);
    procedure SetShow3D(const Value: Boolean);
    procedure SetShowShadow(const Value: Boolean);
    procedure SetTabRounding(const Value: TTabRounding);
  protected
    procedure AddControl(AControl: TCustomControl);
    procedure RemoveControl(AControl: TCustomControl);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change(PropID: integer);
    procedure SetColorTones(ATones: TColorTones);
    procedure InitColorTones; virtual;
    property BlendFactor: Integer read FBlendFactor write FBlendFactor;
    procedure SetAutoThemeAdapt(const Value: boolean); virtual;

    property AutoThemeAdapt: boolean read FAutoThemeAdapt write SetAutoThemeAdapt default False;
    property TabAppearance: TTabAppearance read FTabAppearance write SetTabAppearance;  // 1
    property PageAppearance: TVistaBackground read FPageAppearance write SetPageAppearance; // 2
    property RoundEdges: Boolean read FRoundEdges write SetRoundEdges default True;   // 3,  page round edges
    property GlowButtonAppearance: TGlowButtonAppearance read FGlowButtonAppearance write SetGlowButtonAppearance; // 4
    property Show3D: Boolean read FShow3D write SetShow3D default True;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow default True;
    property TabRounding: TTabRounding read FTabRounding write SetTabRounding default 1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Assign(Source: TPersistent); override;
  end;

  TTabProgress = class(TPersistent)
  private
    FColor: TColor;
    FCompleteColor: TColor;
    FOnChange: TNotifyEvent;
    FMax: Integer;
    FMin: Integer;
    FVisible: Boolean;
    FPosition: Integer;
    FColorTo: TColor;
    FCompleteColorTo: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetCompleteColor(const Value: TColor);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetColorTo(const Value: TColor);
    procedure SetCompleteColorTo(const Value: TColor);
  protected
    procedure Changed;
    procedure Draw(g: TGPGraphics; R: TRect; RotateTabLeftRight: Boolean; Direction: TTabPosition);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clSilver;
    property CompleteColor: TColor read FCompleteColor write SetCompleteColor default clWhite;
    property CompleteColorTo: TColor read FCompleteColorTo write SetCompleteColorTo default clRed;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 50;
    property Visible: Boolean read FVisible write SetVisible default false;
  end;

  TArrowDirection = (arrUp, arrDown, arrLeft, arrRight);
  TArrowWindow = class(TPanel)
  private
    Dir: TArrowDirection;
    Arrow: array[0..8] of TPoint;
    FBrColor: TColor;
  public
    constructor Init(AOwner: TComponent;direction:TArrowDirection);
    procedure Loaded; override;
  protected
    procedure CreateWnd; override;
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TProWinControl = class(TWinControl);

  TAdvPagerButton = class(TGraphicControl)
  private
    FMouseEnter: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FTransparent: Boolean;
    FMouseDown: Boolean;
    FIPicture: TGDIPPicture;
    FIHotPicture: TGDIPPicture;
    FIDownPicture: TGDIPPicture;
    FIDisabledPicture: TGDIPPicture;
    FDrawClose: Boolean;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetTransparent(const Value: Boolean);
    procedure SetDownPicture(const Value: TGDIPPicture);
    procedure SetHotPicture(const Value: TGDIPPicture);
    procedure SetPicture(const Value: TGDIPPicture);
    procedure SetDisabledPicture(const Value: TGDIPPicture);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;

    // published
    property Enabled;
    property Font;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property HotPicture: TGDIPPicture read FIHotPicture write SetHotPicture;
    property Picture: TGDIPPicture read FIPicture write SetPicture;
    property DownPicture: TGDIPPicture read FIDownPicture write SetDownPicture;
    property DisabledPicture: TGDIPPicture read FIDisabledPicture write SetDisabledPicture;
    property DrawClose: Boolean read FDrawClose write FDrawClose default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  IFloatingPagerWindow = interface
  ['{26C994ED-F15A-4CBC-92FF-5DD24E3D24C6}']
    function AdvPager: TAdvOfficePager;
    procedure SetAdvPager(Pager: TAdvOfficePager);
  end;

  TFloatingPagerWindow = class(TForm, IFloatingPagerWindow)
  private
    FAdvPager: TAdvOfficePager;
  public
    function AdvPager: TAdvOfficePager;
    procedure SetAdvPager(Pager: TAdvOfficePager);
  published
    property Height;
    property Width;
    property OnActivate;
    property OnClose;
    property OnCloseQuery;
    property OnDeactivate;
    property OnShow;
    property OnHide;
    property OnResize;
  end;

  TFloatingPagerWindowClass = class of TForm;

  TAdvOfficePage = class(TCustomControl)
  private
    FTabVisible: Boolean;
    FAdvOfficePager: TAdvOfficePager;
    FCaption: TCaption;
    FTabEnabled: Boolean;
    FImageIndex: TImageIndex;
    FTimer: TTimer;
    FTimeInc: Integer;
    FStepHover: Integer;
    FStepPush: Integer;
    FGlowState: TGlowState;
    FTabHint: string;
    FOfficeHint: TAdvHintInfo;
    FIPicture: TGDIPPicture;
    FIDisabledPicture: TGDIPPicture;
    FUpdatingParent: Boolean;
    FShortCutHint: TShortCutHintWindow;
    FShortCutHintPos: TShortCutHintPos;
    FShortCutHintText: string;
    FOnShow: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FShowClose: Boolean;
    FUseTabAppearance: Boolean;
    FTabAppearance: TTabAppearance;
    FPageAppearance: TVistaBackground;
    FUsePageAppearance: Boolean;
    FBkgCache: TBitmap;
    FValidCache: boolean;
    FWideCaption: widestring;
    FShowCheckBox: Boolean;
    FChecked: Boolean;
    FCloseButton: TAdvGlowButton;
    FCloseButtonChrome: TAdvPagerButton;
    FProgress: TTabProgress;
    FGlow: Boolean;
    FGlowColor: TColor;
    FTxtColor: TColor;
    FTabIndex: integer;
    //--- Docking related
    FLocked: Boolean;
    FParentPager: TAdvOfficePager;
    FFloating: Boolean;   // True only when floating
    FDraggingAccept: Boolean;
    FBadgeColor: TColor;
    FBadgeTextColor: TColor;
    FBadge: string;
    FObject: TObject;
    //---
    procedure TimerProc(Sender: TObject);
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure PictureChanged(Sender: TObject);
    procedure OnTabAppearanceFontChanged(Sender: TObject);
    procedure OnTabAppearanceChanged(Sender: TObject);
    procedure OnPageAppearanceChanged(Sender: TObject);
    procedure OnProgressChanged(Sender: TObject);
    procedure SetAdvOfficePager(const Value: TAdvOfficePager);
    procedure SetTabVisible(const Value: Boolean);
    procedure SetCaption(const Value: TCaption);
    procedure SetTabEnabled(const Value: Boolean);
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetDisabledPicture(const Value: TGDIPPicture);
    procedure SetPicture(const Value: TGDIPPicture);
    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    procedure SetShowClose(const Value: Boolean);
    procedure SetTabAppearance(const Value: TTabAppearance);
    procedure SetUseTabAppearance(const Value: Boolean);
    procedure SetPageAppearance(const Value: TVistaBackground);
    procedure SetUsePageAppearance(const Value: Boolean);
    procedure SetWideCaption(const Value: widestring);
    procedure SetShowCheckBox(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetProgress(const Value: TTabProgress);
    procedure SetGlow(const Value: Boolean);
    procedure SetGlowColor(const Value: TColor);
    procedure SetLocked(const Value: Boolean);
    procedure SetBadge(const Value: string);
    procedure SetBadgeColor(const Value: TColor);
    procedure SetBadgeTextColor(const Value: TColor);
  protected
    FTabRect: TRect;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure ReadState(Reader: TReader); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure SetName(const Value: TComponentName); override;
    property TxtColor: TColor read FTxtColor write FTxtColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ShowShortCutHint;
    procedure HideShortCutHint;
    procedure SelectFirstControl;
    function FloatingWindow: IFloatingPagerWindow;
    function GetFloatingWindow: TForm;

    procedure SetFloating(X: integer = 0; Y: integer = 0); overload;
    procedure SetFloating(X, Y, W, H: integer); overload;
    property TabIndex: integer read FTabIndex write FTabIndex;
    property AdvOfficePager: TAdvOfficePager read FAdvOfficePager write SetAdvOfficePager;
    property PageObject: TObject read FObject write FObject;
  published
    property Badge: string read FBadge write SetBadge;
    property BadgeColor: TColor read FBadgeColor write SetBadgeColor default clRed;
    property BadgeTextColor: TColor read FBadgeTextColor write SetBadgeTextColor default clWhite;
    property Caption: TCaption read FCaption write SetCaption;
    property DoubleBuffered;
    property WideCaption: widestring read FWideCaption write SetWideCaption;
    property DisabledPicture: TGDIPPicture read FIDisabledPicture write SetDisabledPicture;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property PageAppearance: TVistaBackground read FPageAppearance write SetPageAppearance;
    property UsePageAppearance: Boolean read FUsePageAppearance write SetUsePageAppearance default false;
    property Picture: TGDIPPicture read FIPicture write SetPicture;
    property TabHint: string read FTabHint write FTabHint;
    property TabVisible: Boolean read FTabVisible write SetTabVisible default true;
    property TabEnabled: Boolean read FTabEnabled write SetTabEnabled default true;
    property ShowClose: Boolean read FShowClose write SetShowClose default true;
    property ShowHint;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored false;
    property ShortCutHint: string read FShortCutHintText write FShortCutHintText;
    property ShortCutHintPos: TShortCutHintPos read FShortCutHintPos write FShortCutHintPos default shpTop;
    property TabAppearance: TTabAppearance read FTabAppearance write SetTabAppearance;
    property UseTabAppearance: Boolean read FUseTabAppearance write SetUseTabAppearance default false;
    property ShowCheckBox: Boolean read FShowCheckBox write SetShowCheckBox default false;
    property Checked: Boolean read FChecked write SetChecked default false;
    property Progress: TTabProgress read FProgress write SetProgress;
    property Glow: Boolean read FGlow write SetGlow default false;
    property GlowColor: TColor read FGlowColor write SetGlowColor default clRed;
    property Locked: Boolean read FLocked write SetLocked default false;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClick;
    property OnDblClick;
    property PopupMenu;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDragOver;
    property OnDragDrop;
    property OnEndDrag;
    property OnStartDrag;
    property OnExit;
    property OnEnter;
  end;

  TTabChangingEvent = procedure(Sender: TObject; FromPage, ToPage: Integer; var AllowChange: Boolean) of object;
  TOnClosePage = procedure (Sender:TObject; PageIndex: integer; var Allow: boolean) of object;
  TOnClosedPage = procedure (Sender:TObject; PageIndex: integer) of object;

  TOnPageListClick = procedure (Sender: TObject; X, Y: integer) of object;
  TTabMovedEvent = procedure(Sender: TObject; FromPos: integer; ToPos: Integer)of object;
  TDrawTabEvent = procedure(Sender:TObject; TabIndex: integer; TabRect: TRect) of object;
  TTabClickEvent = procedure(Sender: TObject; PageIndex: integer) of object;
  TClosedListClick = procedure (Sender: TObject; X, Y: integer) of object;
  TTabEvent = procedure(Sender: TObject; PageIndex: integer) of object;
  TPageEvent = procedure(Sender: TObject; APage: TAdvOfficePage) of object;
  TTabUndockingEvent = procedure(Sender: TObject; APage: TAdvOfficePage; var Allow: boolean) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficePager = class(TCustomControl, ITMSTones)
  private
    FTones: TColorTones;
    FCloseButtonGlobal: TAdvGlowButton;
    FIsWinXP: Boolean;
    FInternalOfficePagerStyler: TCustomAdvOfficePagerStyler;
    FOfficePagerStyler: TCustomAdvOfficePagerStyler;
    FCurrentOfficePagerStyler: TCustomAdvOfficePagerStyler;
    FPageMargin: integer;
    FOffSetY: integer;
    FOffSetX: integer;
    FAdvPages: TDbgList;
    FPropertiesLoaded: Boolean;
    FShowNonSelectedTabs: Boolean;
    FTabSettings: TPagerTabSettings;
    FTabScroller: TPagerTabScroller;
    FActivePageIndex: Integer;
    FHotPageIndex: Integer;
    FDownPageIndex: Integer;
    FOldHotPageIndex: Integer;
    FHintPageIndex: Integer;
    FImages: TCustomImageList;
    FDisabledImages: TCustomImageList;
    FNewPage: TAdvOfficePage;
    FUndockPage: TAdvOfficePage;
    FShowTabHint: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TTabChangingEvent;
    FOldCapRightIndent: Integer;
    FOfficeHint: TAdvHintInfo;
    FTabPosition: TTabPosition;
    FAntiAlias: TAntiAlias;
    FButtonSettings: TPageButtonSettings;
    FPageListMenu: TPopupMenu;
    FOnClosePage: TOnClosePage;
    FOnClosedPage: TOnClosedPage;
    FOnPageListClick: TOnPageListClick;
    FRotateTabLeftRight: Boolean;
    FCloseOnTab: Boolean;
    FPageListButton: TAdvGlowButton;
    FScrollPrevButton: TAdvGlowButton;
    FScrollNextButton: TAdvGlowButton;
    FScrollFirstButton: TAdvGlowButton;
    FScrollLastButton: TAdvGlowButton;
    FTabOffSet: Integer;
    FUseMaxSpace: Boolean;
    FFreeOnClose: Boolean;
    FShortCutHintWinList: TDbgList;
    FFormWndProc: TWndMethod;
    FTabShortCutHintShowing: Boolean;
    FTabShortCutChars: String;
    FShowShortCutHints: Boolean;
    FTabReorder: Boolean;
    FOnTabMoved: TTabMovedEvent;
    FIsClosing: Boolean;
    FOnDrawTab: TDrawTabEvent;
    FButtonsBkg: TBitmap;
    FCloseOnTabPosition: TCloseOnTabPos;
    FDesignTime: boolean;
    FBufferedPages: boolean;
    FOnTabClick: TTabClickEvent;
    FOnTabDblClick: TTabClickEvent;
    FOnTabRightClick: TTabClickEvent;
    FOnTabCheckBoxClick: TTabClickEvent;
    FGlow: Boolean;
    FFormScaled: Boolean;
    FTransparent: Boolean;
    FShowCloseOnNonSelectedTabs: Boolean;
    FOnLastClick: TNotifyEvent;
    FOnFirstClick: TNotifyEvent;
    FOnPrevClick: TNotifyEvent;
    FOnNextClick: TNotifyEvent;
    FOnTabUndocking: TTabUndockingEvent;
    FClosedListButton: TAdvGlowButton;
    FClosedListMenu: TPopupMenu;
    FInternalClosedListMenu: TPopupMenu;
    FOnClosedListClick: TClosedListClick;
    FClosedPageList: TStringList;
    FOnTabMouseLeave: TTabEvent;
    FOnTabMouseEnter: TTabEvent;
    FBrGlowTimer: TTimer;
    FStepHover: Integer;
    FTimeInc: Integer;
    FArrow: TArrowWindow;
    FFixedTabs: Integer;
    FIsAeroVista: Boolean;
    FTabReorderIndicatorColor: TColor;
    FInsertButtonHot: Boolean;
    FInsertButtonDown: Boolean;
    FOldMouseX: Integer;
    FOldMouseY: Integer;
    FDraging: Boolean;
    FAllowTabUndock: Boolean;
    FDockList: TDbgList;
    FClonedOfPager: TAdvOfficePager; // Creator Pager, Assigned only for cloned pagers
    FFloatingPages: TDbgList;
    FDragingPage: TAdvOfficePage;
    FFloatingWindow: TForm;
    FFloatingBorderIcons: TBorderIcons;
    FFloatingBorderStyle: TFormBorderStyle;
    FDragTabX, FDragTabY: Integer;
    FOnTabDock: TPageEvent;
    FOnTabUnDock: TPageEvent;
    FOnInsertPage: TPageEvent;
    FTabRoundEdges: Boolean;
    FShadow: Boolean;
    FShow3D: Boolean;
    FItones: Boolean;
    FPageAppearance: TVistaBackground;
    FTabAppearance: TTabAppearance;
    FGlowButtonAppearance: TGlowButtonAppearance;
    FFloatingPagerWindowClass: TFloatingPagerWindowClass;
    //--- MulitiLine
    FMultiLine: Boolean;
    FTabRows: array of TPoint;  // 1: start index;  2: end index
    FMultiLineRowCount: Integer;
    FTabRowSize: Integer;  // Height in case of tpTop, tpBottom and width otherwise
    //---
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMControlListChange(var Message: TCMControlListChange); message CM_CONTROLLISTCHANGE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    procedure OnTabSettingsChanged(Sender: TObject);
    procedure OnButtonSettingChanged(Sender: TObject);
    procedure OnCloseButtonClick(Sender: TObject);
    procedure OnCloseButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnPageListButtonClick(Sender: TObject);
    procedure OnPageListMenuClick(Sender: TObject);
    procedure OnScrollPrevButtonClick(Sender: TObject);
    procedure OnScrollNextButtonClick(Sender: TObject);
    procedure OnScrollFirstButtonClick(Sender: TObject);
    procedure OnScrollLastButtonClick(Sender: TObject);
    procedure SubclassProc(var Msg: TMessage);
    procedure OnEnterTab(PageIndex: Integer);
    procedure OnExitTab(PageIndex: Integer);
    procedure SetPageValidCache(Value: Boolean);
    procedure SetPagePosition(AdvPage: TAdvOfficePage);
    procedure SetAllPagesPosition;
    procedure SetOfficePagerStyler(const Value: TCustomAdvOfficePagerStyler);
    function GetAdvOfficePageCount: integer;
    function GetAdvPages(index: integer): TAdvOfficePage;
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenuEx(const Value: TPopupMenu);
    procedure SetShowNonSelectedTabs(const Value: Boolean);
    function GetActivePage: TAdvOfficePage;
    function GetActivePageIndex: Integer;
    procedure SetActivePage(const Value: TAdvOfficePage);
    procedure SetActivePageIndex(const Value: Integer);
    procedure SetTabSettings(const Value: TPagerTabSettings);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetDisabledImages(const Value: TCustomImageList);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetTabPosition(const Value: TTabPosition);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetButtonSettings(const Value: TPageButtonSettings);
    procedure SetPageListMenu(const Value: TPopupMenu);
    procedure SetCloseOnTab(const Value: Boolean);
    procedure SetRotateTabLeftRight(const Value: Boolean);
    procedure SetPageMargin(const Value: integer);
    function GetNextPictureChanged: Boolean;
    function GetPrevPictureChanged: Boolean;
    procedure SetNextPictureChanged(const Value: Boolean);
    procedure SetPrevPictureChanged(const Value: Boolean);
    procedure ReadNextPicChanged(Reader: TReader);
    procedure ReadPrevPicChanged(Reader: TReader);
    procedure WriteNextPicChanged(Writer: TWriter);
    procedure WritePrevPicChanged(Writer: TWriter);
    procedure SetCloseOnTabPosition(const Value: TCloseOnTabPos);
    function GetDockClientFromMousePos(MousePos: TPoint): TControl;
    procedure SetShowCloseOnNonSelectedTabs(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure OnCloseListMenuClick(Sender: TObject);
    procedure ClosedListButtonClick(Sender: TObject);
    procedure CreateClosedListButton;
    procedure UpdateClosedListButton;
    procedure OnGlowTimerTime(Sender: TObject);
    procedure CreateGlowTimer;
    function IsAnyTabGlowing: Boolean;
    procedure CreateDropArrow;
    procedure SetFixedTabs(Value: Integer);
    procedure SetTabReorderIndicatorColor(const Value: TColor);
    procedure SetAllowTabUndock(const Value: Boolean);
    function GetAdvOfficePageVisibleCount: integer;
    procedure SetShadow(const Value: Boolean);
    procedure SetShow3D(const Value: Boolean);
    procedure SetTabRoundEdges(const Value: Boolean);
    procedure SetGlowButtonColorTones(AdvGlowButton: TAdvGlowButton);
    function GetFloatingPageCount: integer;
    function GetFloatingPages(index: integer): TAdvOfficePage;
    function GetSettings: string;
    procedure SetSettings(const Value: string);
    procedure SetMultiLine(const Value: Boolean);
  protected
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure WndProc(var Msg: TMessage); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure DragCanceled; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function GetPageFromDockClient(Client: TControl): TAdvOfficePage;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DefineProperties(Filer : TFiler); override;
    procedure DrawAllTabs;
    procedure DrawTabScrollButtons;
    procedure DrawInsertButton; overload;
    procedure DrawInsertButton(Canvas: TCanvas; R: TRect); overload;
    procedure Paint; override;

    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    procedure SetAdvPageBounds(AdvPage: TAdvOfficePage; var ALeft, ATop, AWidth, AHeight: Integer);

    procedure UpdateGlowTimer;
    function IsTabShowing(TabIndex: Integer): Boolean;
    function IsGlass: Boolean;
    function IsOnGlass: Boolean;

    procedure BeginMove(Shift: TShiftState; X, Y: Integer; Page: TAdvOfficePage);
    procedure Move(Shift: TShiftState; X, Y: Integer);
    procedure EndMove;
    function CreateClonedPager(PrtWindow: TWinControl): TAdvOfficePager;
    procedure SetFloatingPage(Page: TAdvOfficePage; X: integer = 0; Y: integer = 0); overload;
    procedure SetFloatingPage(Page: TAdvOfficePage; X, Y, W, H: integer); overload;

    procedure AssignPager(Source: TAdvOfficePager);

    procedure UpdateMe(PropID: integer);
    procedure ChangeActivePage(PageIndex: Integer);

    procedure UpdateTabScroller;
    procedure ScrollLeftBtnClick;
    procedure ScrollRightBtnClick;
    procedure ScrollLastBtnClick;
    procedure ScrollFirstBtnClick;

    procedure ShowShortCutHintOfAllPages;
    procedure HideShortCutHintOfAllPages;
    function CreateShortCutHintWin: TShortCutHintWindow;
    procedure DestroyShortCutHintWin(ShortCutWin: TShortCutHintWindow);

    procedure InitializeAndUpdateButtons;
    function AnyButtonVisible: Boolean;

    function CanShowTab(PageIndex: Integer): Boolean;
    function GetVisibleTabCount: Integer;

    function GetTextSize(PageIndex: Integer): TSize;
    function GetTabImageSize(PageIndex: Integer): TSize;
    procedure GetCloseBtnImageAndTextRect(PageIndex: Integer; var CloseBtnR, TextR: TRect; var ImgP: TPoint); // used when TabSettings.Width > 0
    procedure InvalidateTab(PageIndex: Integer);
    function GetAdvPageRect: TRect;
    function GetButtonsRect: TRect;
    function GetTabsArea: TRect;
    function GetTabsRect: TRect;
    function GetTabRect(StartIndex, PageIndex: Integer; ConsiderTabScroller: Boolean): TRect;  overload;
    function PTOnTab(X, Y: Integer): Integer;
    function PTOnCheckBox(PageIndex, X, Y: integer): Boolean;
    function GetCheckBoxRect(PageIndex: integer): TRect;
    function GetCloseButtonRect(PageIndex: integer): TRect;
    function GetPageListRect: TRect;
    function GetClosedListButtonRect: TRect;
    function GetTabScrollerRect: TRect;
    function GetTabScrollerFirstRect: TRect;
    function GetTabScrollerLastRect: TRect;
    function GetTabScrollerLeftRect: TRect;
    function GetTabScrollerRightRect: TRect;
    function PtOnTabScrollLeftBtn(X, Y: integer): Boolean;
    function PtOnTabScrollRightBtn(X, Y: integer): Boolean;
    function PtOnInsertButton(X, Y: Integer): Boolean;
    function GetInsertButtonRect: TRect;
    procedure InvalidateInsertButton;
    procedure DoInsertButtonClick; virtual;
    function GetLastDisplayTab: Integer;   // returns visually last tab index

    function IsActivePageNeighbour(PageIndex: Integer): Integer;   // -1= previous;  0= No;   +1= Next
    function GetLeftRoundingOffset: Integer;
    function GetRightRoundingOffset: Integer;

    procedure GlassChanged;

    function CanGlow: Boolean;
    function CanShow3D: Boolean;
    function CanShowShadow: Boolean;

    function CanShowCloseButton: Boolean;
    function UseOldDrawing: Boolean;
    procedure UpdatePageAppearanceOfPages(PageAppearance: TVistaBackground);
    procedure UpdateTabAppearanceOfPages(TabAppearance: TTabAppearance);

    property NextPictureChanged: Boolean read GetNextPictureChanged write SetNextPictureChanged;
    property PrevPictureChanged: Boolean read GetPrevPictureChanged write SetPrevPictureChanged;

    procedure UpdateMultiLineTabs;
    procedure UpdateMultiLineActiveTab;

    property TabRoundEdges: Boolean read FTabRoundEdges write SetTabRoundEdges default True;
    property Show3D: Boolean read FShow3D write SetShow3D default True;
    property Shadow: Boolean read FShadow write SetShadow default True;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Init;
    procedure InitOrder;

    function CloseActivePage : Boolean;
    function GetTabRect(PageIndex: Integer): TRect;  overload;
    function GetTabRect(Page: TAdvOfficePage): TRect; overload;
    procedure DrawTab(PageIndex: Integer);
    procedure DrawTabContent(PageIndex: Integer; R: TRect);

    procedure DragDrop(Source: TObject; X, Y: Integer); override;

    function GetVersionNr: integer;

    procedure OpenAllClosedPages;
    function OpenClosedPage(APage: TAdvOfficePage): Boolean;
    procedure ClearAdvPages;  // Clear/remove all pages
    procedure SaveToFile(FileName: String); // Save tab order and visibility
    procedure LoadFromFile(FileName: String); // load saved state ie: tab order, visibility
    procedure SetColorTones(ATones: TColorTones);
    procedure ResetPages;
    function AddAdvPage(AdvPage: TAdvOfficePage): integer; overload;
    function AddAdvPage(PageCaption: TCaption): integer; overload;
    procedure RemoveAdvPage(AdvPage: TAdvOfficePage);
    procedure MoveAdvPage(CurIndex, NewIndex: Integer);
    function FindNextPage(CurPage: TAdvOfficePage; GoForward, CheckTabVisible: Boolean): TAdvOfficePage;
    procedure SelectNextPage(GoForward: Boolean);
    function IndexOfPage(AdvPage: TAdvOfficePage): Integer;
    function IndexOfTabAt(X,Y: Integer): integer;
    function PageByName(Value: string): TAdvOfficePage;
    function PageByCaption(Value: string): TAdvOfficePage;
    function PageAtTabIndex(Value: integer): TAdvOfficePage;
    property Canvas;
    property Settings: string read GetSettings write SetSettings;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property BufferedPages: boolean read FBufferedPages write FBufferedPages;
    property AdvPageCount: integer read GetAdvOfficePageCount;
    property AdvPageVisibleCount: integer read GetAdvOfficePageVisibleCount;
    property AdvPages[index: integer]: TAdvOfficePage read GetAdvPages;
    property FloatingPages[index: integer]: TAdvOfficePage read GetFloatingPages;
    property FloatingPageCount: integer read GetFloatingPageCount;
    property FloatingPagerWindowClass: TFloatingPagerWindowClass read FFloatingPagerWindowClass write FFloatingPagerWindowClass;
    property TabScroller: TPagerTabScroller read FTabScroller;
    property HotPageIndex: Integer read FHotPageIndex;
    property RowCount: integer read FMultiLineRowCount;
    property ScrollNextButton: TAdvGlowButton read FScrollNextButton;
    property ScrollPrevButton: TAdvGlowButton read FScrollPrevButton;
    property ScrollFirstButton: TAdvGlowButton read FScrollFirstButton;
    property ScrollLastButton: TAdvGlowButton read FScrollLastButton;
  published
    property AdvOfficePagerStyler: TCustomAdvOfficePagerStyler read FOfficePagerStyler write SetOfficePagerStyler;
    property Align;
    property Anchors;
    property ActivePage: TAdvOfficePage read GetActivePage write SetActivePage;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property AllowTabUndock: Boolean read FAllowTabUndock write SetAllowTabUndock default False;
    property ButtonSettings: TPageButtonSettings read FButtonSettings write SetButtonSettings;
    property Constraints;
    property CloseOnTab: Boolean read FCloseOnTab write SetCloseOnTab default false;
    property CloseOnTabPosition: TCloseOnTabPos read FCloseOnTabPosition write SetCloseOnTabPosition default cpRight;
    property ClosedListMenu: TPopupMenu read FClosedListMenu write FClosedListMenu;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DoubleBuffered;
    property FreeOnClose: boolean read FFreeOnClose write FFreeOnClose default false;
    property FixedTabs: Integer read FFixedTabs write SetFixedTabs default 0;
    property FloatingBorderIcons: TBorderIcons read FFloatingBorderIcons write FFloatingBorderIcons default [biSystemMenu, biMinimize, biMaximize];
    property FloatingBorderStyle: TFormBorderStyle read FFloatingBorderStyle write FFloatingBorderStyle default bsSizeable;
    property DockSite;
    property Glow: Boolean read FGlow write FGlow default true;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine write SetMultiLine default False;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property PageMargin: integer read FPageMargin write SetPageMargin default 1;
    property PageListMenu: TPopupMenu read FPageListMenu write SetPageListMenu;
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenuEx;
    property RotateTabLeftRight: Boolean read FRotateTabLeftRight write SetRotateTabLeftRight default true;
    property ShowCloseOnNonSelectedTabs: Boolean read FShowCloseOnNonSelectedTabs write SetShowCloseOnNonSelectedTabs default false;
    property ShowNonSelectedTabs: Boolean read FShowNonSelectedTabs write SetShowNonSelectedTabs default False;
    property ShowTabHint: Boolean read FShowTabHint write FShowTabHint default false;
    property ShowHint;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
    property TabSettings: TPagerTabSettings read FTabSettings write SetTabSettings;
    property TabReorder: Boolean read FTabReorder write FTabReorder default false;
    property TabReorderIndicatorColor: TColor read FTabReorderIndicatorColor write SetTabReorderIndicatorColor default clBlue;
{$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
{$ENDIF}
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property ShowShortCutHints: Boolean read FShowShortCutHints write FShowShortCutHints;
    property Version: string read GetVersion write SetVersion stored false;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnClosePage: TOnClosePage read FOnClosePage write FOnClosePage;
    property OnClosedPage: TOnClosedPage read FOnClosedPage write FOnClosedPage;
    property OnClosedListClick: TClosedListClick read FOnClosedListClick write FOnClosedListClick;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnStartDock;
    property OnInsertPage: TPageEvent read FOnInsertPage write FOnInsertPage;
    property OnUnDock;

    property OnEnter;
    property OnExit;
    property OnPageListClick: TOnPageListClick read FOnPageListClick write FOnPageListClick;
    property OnTabMoved: TTabMovedEvent read FOnTabMoved write FOnTabMoved;
    property OnTabClick: TTabClickEvent read FOnTabClick write FOnTabClick;
    property OnTabCheckBoxClick: TTabClickEvent read FOnTabCheckBoxClick write FOnTabCheckBoxClick;
    property OnTabDblClick: TTabClickEvent read FOnTabDblClick write FOnTabDblClick;
    property OnTabRightClick: TTabClickEvent read FOnTabRightClick write FOnTabRightClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property TabOrder;
    property TabStop;
    property OnPrevClick: TNotifyEvent read FOnPrevClick write FOnPrevClick;
    property OnNextClick: TNotifyEvent read FOnNextClick write FOnNextClick;
    property OnFirstClick: TNotifyEvent read FOnFirstClick write FOnFirstClick;
    property OnLastClick: TNotifyEvent read FOnLastClick write FOnLastClick;
    property OnTabMouseEnter: TTabEvent read FOnTabMouseEnter write FOnTabMouseEnter;
    property OnTabMouseLeave: TTabEvent read FOnTabMouseLeave write FOnTabMouseLeave;
    property OnTabDock: TPageEvent read FOnTabDock write FOnTabDock;
    property OnTabUnDock: TPageEvent read FOnTabUnDock write FOnTabUnDock;
    property OnTabUnDocking: TTabUndockingEvent read FOnTabUnDocking write FOnTabUnDocking;
  end;


  {function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; r: TRect; Caption:string; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Direction: TTabPosition): TRect;
  procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
     GradientU,GradientB: TGDIPGradient; Caption:string; AFont: TFont; Enabled: Boolean; Focus: Boolean;
     AntiAlias: TAntiAlias; RoundEdges: Boolean; Direction: TTabPosition = tpTop; X: Integer = 0; Y: Integer =0; Ang: Single = 0); overload;
   }
procedure DrawVistaTab(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor; GradientU,GradientB: TGDIPGradient;
   Enabled: Boolean; Shape: TAdvTabShape; Focus: Boolean; {AntiAlias: TAntiAlias; }Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition; Progress: TTabProgress);

implementation

uses
  AdvOfficePagerStylers
  {$IFDEF DELPHI2007_LVL}
  , uxtheme
  {$ENDIF}
  , ActiveX;


type
  TAccessCanvas = class(TCanvas);

  PDockInfo = ^TDockInfo;
  TDockInfo = record
    Dock: TAdvOfficePager;
    DockRect: TRect;
  end;


var
  WM_OPDESTROYCLOSEBTN: Word;

function CalculateDPIScale(FHDC: HDC): single;
var
  FDPI: integer;

  function FontHeightAtDpi(iDPI, iFontSize: Integer): Integer;
  var
    FTmpCanvas: TCanvas;
  begin
    FTmpCanvas := TCanvas.Create;
    try
      FTmpCanvas.Handle := GetDC(0);
      FTmpCanvas.Font.PixelsPerInch := iDPI; //must be set BEFORE size
      FTmpCanvas.Font.Size := iFontSize;
      Result := FTmpCanvas.TextHeight('0');
    finally
      FTmpCanvas.Free;
    end;
  end;

begin
  Result := 1.0;

  if FHDC = 0 then
    FHDC:= GetDC(0);
  try
    FDPI := GetDeviceCaps(FHDC, LOGPIXELSX);
    if FDPI <> 96 then
      Result := FontHeightAtDpi(FDPI, 9) / FontHeightAtDpi(96, 9);
  finally
    if FHDC = 0 then
      ReleaseDC(0, FHDC);
  end;

end;

function GetDPIScale(Scaled: boolean; FHandle: HDC = 0): single;
begin
  Result := 1.0;
  if Scaled then
    Result := CalculateDPIScale(FHandle);
end;


{$IFNDEF DELPHI7_LVL}
function GetFileVersion(FileName:string): Integer;
var
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

  
  
//------------------------------------------------------------------------------

function ColorToARGB(Color: TColor): ARGB;
var
  c: TColor;
begin
  c := ColorToRGB(Color);
  Result := ARGB( $FF000000 or ((DWORD(c) and $FF) shl 16) or ((DWORD(c) and $FF00) or ((DWORD(c) and $ff0000) shr 16)));
end;

//------------------------------------------------------------------------------

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    R.Right := R.Right - 1
  else
    R.Bottom := R.Bottom - 1;

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
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if (BlendFactor >= 100) then
  begin
    Result := Col1;
    Exit;
  end;
  if (BlendFactor <= 0) then
  begin
    Result := Col2;
    Exit;
  end;

  Col1 := Longint(ColorToRGB(Col1));
  r1 := GetRValue(Col1);
  g1 := GetGValue(Col1);
  b1 := GetBValue(Col1);

  Col2 := Longint(ColorToRGB(Col2));
  r2 := GetRValue(Col2);
  g2 := GetGValue(Col2);
  b2 := GetBValue(Col2);

  r1 := Round( BlendFactor/100 * r1 + (1 - BlendFactor/100) * r2);
  g1 := Round( BlendFactor/100 * g1 + (1 - BlendFactor/100) * g2);
  b1 := Round( BlendFactor/100 * b1 + (1 - BlendFactor/100) * b2);

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure DrawRectangle(Canvas: TCanvas; R: TRect; aColor: TColor);
begin
  Canvas.Brush.Color := aColor;
  Canvas.FillRect(R);
end;

//-------------------------------------------------------------------- DrawGauge

type
  TGaugeOrientation = (goHorizontal, goVertical);
  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo: TColor;
    Level1Color: TColor;
    Level1ColorTo: TColor;
    Level2Color: TColor;
    Level2ColorTo: TColor;
    Level3Color: TColor;
    Level3ColorTo: TColor;
    Level1Perc: Integer;
    Level2Perc: Integer;
    BorderColor: TColor;
    ShowBorder: Boolean;
    Stacked: Boolean;
    ShowPercentage: Boolean;
    Font: TFont;
    CompletionSmooth: Boolean;
    ShowGradient: Boolean;
    Steps: Integer;
    Position: Integer;
    BackgroundColor: TColor;
    Orientation: TGaugeOrientation;
    Min: Integer;
    Max: Integer;
  end;

procedure DrawGauge(Canvas: TCanvas; R: TRect; Position: Integer;
  Settings: TGaugeSettings);
var
  RectL: TRect;
  RectM: TRect;
  RectR: TRect;

  WidthBar: integer;
  WidthPart: Integer;
  Continue: Boolean;
  GradDir: Boolean;
  BrushColor: TColor;
  BrushColorTo: TColor;
  Percentage: Integer;
  BarFilled: Integer;
  NumberOfBlock: Integer;
  i: Integer;
  EmptyWidth: integer;

{$IFNDEF TMSCLX}
  lf: TLogFont;
{$ENDIF}
  tf: TFont;

  R1: TRect;
  R2: TRect;
  Max, OrgPosition: Integer;
begin
  if (Settings.Orientation = goHorizontal) then
    WidthBar := R.Right - R.Left
  else
    WidthBar := R.Bottom - R.Top;

  Continue := true;
  Percentage := -1;
  Canvas.Brush.Color := Settings.BackgroundColor;
  GradDir := not (Settings.Orientation = goHorizontal);

  if (Settings.ShowPercentage) then
    Percentage := Position;

  OrgPosition := Position;
  Max := Settings.Max - Settings.Min;
  Position := Math.Max(Position, Settings.Min);
  Position := Min(Position, Settings.Max);
  Position := abs(Position - Settings.Min);

  //Draw Border
  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

  WidthBar := WidthBar - 2;

  if (OrgPosition > Settings.Min) then
  begin
    if (Settings.Stacked) then
    begin
      if (OrgPosition >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / Max) * WidthBar)
      else
      begin
        WidthPart := Round((Position / Max) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      if (Settings.Orientation = goHorizontal) then
      begin
        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := r.Bottom - 1;
      end
      else
      begin
        RectL.Left := r.Left + 1;
        RectL.Right := R.Right - 1;
        RectL.Top := R.Bottom - WidthPart;
        RectL.Bottom := R.Bottom - 1;
      end;

      if (Settings.ShowGradient) then
      begin
        if not (Settings.Orientation = goHorizontal) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
          R1.Bottom := RectL.Bottom;
          R1.Top := RectL.Top;
          R2.Left := R1.Right;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          R2.Top := RectL.Top;
        end
        else
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
        end;
        DrawGradient(Canvas, Settings.Level0ColorTo,
          Settings.Level0Color, Settings.Steps, R1, GradDir);
        DrawGradient(Canvas, Settings.Level0Color,
          Settings.Level0ColorTo, Settings.Steps, R2, GradDir);
      end
      else
        DrawRectangle(Canvas, RectL, Settings.Level0Color);

      BarFilled := WidthPart;

      if (Continue) then
      begin
        //Draw second part
        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Left := RectL.Right;
          RectM.Top := r.Top + 1;
          RectM.Bottom := r.Bottom - 1;
        end
        else
        begin
          RectM.Left := R.Left + 1;
          RectM.Right := R.Right - 1;
          RectM.Bottom := RectL.Top;
        end;

        if (OrgPosition >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc -
            Settings.Level1Perc) / Max))
        else
        begin
          WidthPart := Round(WidthBar * ((Position -
            Settings.Level1Perc) / Max));
          Continue := false;
        end;

        if (Settings.Orientation = goHorizontal) then
          RectM.Right := WidthPart + RectM.Left
        else
          RectM.Top := RectM.Bottom - WidthPart;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Left + (RectM.Right - RectM.Left) div 2;
            R1.Bottom := RectM.Bottom;
            R1.Top := RectM.Top;
            R2.Left := R1.Right;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
            R2.Top := RectM.Top;
          end
          else
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Right;
            R1.Top := RectM.Top;
            R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectM.Left;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
          end;
          DrawGradient(Canvas, Settings.Level1ColorTo,
            Settings.Level1Color, Settings.Steps, R1, GradDir);
          DrawGradient(Canvas,
            Settings.Level1Color, Settings.Level1ColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectM, Settings.Level1Color);

        BarFilled := BarFilled + WidthPart;
        if (Continue) then
        begin
          //Draw third part
          if (OrgPosition = Max) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Position -
              Settings.Level2Perc) / Max));

          if (Settings.Orientation = goHorizontal) then
          begin
            RectR.Left := RectM.Right;
            RectR.Top := R.Top + 1;
            RectR.Bottom := r.Bottom - 1;
            RectR.Right := RectR.Left + WidthPart;
          end
          else
          begin
            RectR.Left := R.Left + 1;
            RectR.Right := R.Right - 1;
            RectR.Bottom := RectM.Top - 1;
            RectR.Top := RectR.Bottom - WidthPart;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div
                2;
              R1.Bottom := RectR.Bottom;
              R1.Top := RectR.Top;
              R2.Left := R1.Right;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
              R2.Top := RectR.Top;
            end
            else
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Right;
              R1.Top := RectR.Top;
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level3ColorTo,
              Settings.Level3Color, Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, Settings.Level3Color,
              Settings.Level3ColorTo, Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectR, Settings.Level3Color);
        end;
      end;
    end
    else
    begin
      if (OrgPosition < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (OrgPosition < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (OrgPosition < Max) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        if (Round((Position * WidthBar) / Max) > 9) then
        begin
          if (Settings.Orientation = goHorizontal) then
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := RectL.Left + 7;
            RectL.Top := R.Top + 2;
            RectL.Bottom := R.Bottom - 2;
          end
          else
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := R.Right - 2;
            RectL.Bottom := R.Bottom - 2;
            RectL.Top := RectL.Bottom - 7;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                2;
              R1.Bottom := RectL.Bottom;
              R1.Top := RectL.Top;
              R2.Left := R1.Right;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
              R2.Top := RectL.Top;
            end
            else
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Right;
              R1.Top := RectL.Top;
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor,
              Settings.Steps, R1, GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo,
              Settings.Steps, R2, GradDir);
          end
          else
            DrawRectangle(Canvas, RectL, BrushColor);

          NumberOfBlock := (Round((Position * WidthBar) / Max) div 9) -
            1;
          EmptyWidth := Round((Position * WidthBar) / Max) mod 9;

          for i := 0 to NumberOfBlock - 1 do
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + 7;
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - 7;
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;

          if (EmptyWidth > 2) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + (EmptyWidth - 1);
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - (EmptyWidth - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Position * WidthBar) / Max) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Position *
                WidthBar) / Max) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Position *
                WidthBar) / Max) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div
                  2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div
                  2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor,
                Settings.Steps, R1, GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo,
                Settings.Steps, R2, GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
        end;
      end
      else
      begin
        WidthPart := Round((Position / Max) * WidthBar);

        if (Settings.Orientation = goHorizontal) then
        begin
          RectL.Left := R.Left + 1;
          RectL.Top := R.Top + 1;
          RectL.Right := RectL.Left + WidthPart;
          RectL.Bottom := R.Bottom - 1;
        end
        else
        begin
          RectL.Left := r.Left + 1;
          RectL.Bottom := R.Bottom - 1;
          RectL.Top := RectL.Bottom - WidthPart;
          RectL.Right := r.Right - 1;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor,
            Settings.Steps, R1, GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo,
            Settings.Steps, R2, GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  //Draw text with PositionPercentage
  if (Percentage <> -1) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := Settings.Font.Name;
    Canvas.Font.Size := Settings.Font.Size;
    Canvas.Font.Color := Settings.Font.Color;
    Canvas.Font.Style := Settings.Font.Style;
    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);

{$IFNDEF TMSCLX}

        GetObject(tf.Handle, sizeof(lf), @lf);

        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
{$ENDIF}

        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 -
          (Canvas.TextHeight(IntToStr(Percentage) + '%') div 2)), R.Top +
          ((R.Bottom
          - R.Top) div 2) + Canvas.TextWidth(IntToStr(Percentage) + '%') div 2
          , IntToStr(Percentage) + '%');
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) -
        (Canvas.TextWidth(IntToStr(Percentage) + '%') div 2) + r.Left, r.Top +
        ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(IntToStr(Percentage) +
        '%') div 2, IntToStr(Percentage) + '%');
    end;
  end;

  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
end;

//------------------------------------------------------------------------------

procedure DrawRoundRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

procedure DrawRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X, Y, X + width, Y);
  path.AddLine(X + width, Y, X + width, Y + height);
  path.AddLine(X + width, Y + height, X, Y + height);
  path.AddLine(X, Y + height, X, Y);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

procedure ClearAppearance(Appearance: TGlowButtonAppearance);
begin
  if not Assigned(Appearance) then
    Exit;

  with Appearance do
  begin
    BorderColor := clNone;
    BorderColorHot := clNone;
    BorderColorDown := clNone;
    Color := clNone;
    ColorTo := clNone;
    ColorDown := clNone;
    ColorDownTo := clNone;
    ColorHot := clNone;
    ColorHotTo := clNone;
    ColorMirror := clNone;
    ColorMirrorTo := clNone;
    ColorMirrorHot := clNone;
    ColorMirrorHotTo  := clNone;
    ColorMirrorDown := clNone;
    ColorMirrorDownTo := clNone;
    ColorDisabled := clNone;
    ColorDisabledTo := clNone;
  end;
end;

//------------------------------------------------------------------------------

function IsComCtl6: Boolean;
var
  i: Integer;
begin
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  Result := (i > 5);
end;

//------------------------------------------------------------------------------

function AeroIsEnabled: boolean;
var
  enabled: bool;
begin
  Result := False;
  //if (DWMlibrary = 0) then
  begin
    if (@DwmIsCompositionEnabled <> nil) then
    begin
      DwmIsCompositionEnabled(enabled);
      Result := enabled;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImageFromImageList(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; Images: TCustomImageList; ImageIndex: Integer; Enable: Boolean);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  bmp: TBitmap;
  graphics: TGPGraphics;
  hr: HResult;
begin
  if not Assigned(Images) or (ImageIndex < 0) or (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;

  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  bmp := TBitmap.Create;
  try
    bmp.Width := Images.Width;
    bmp.Height := Images.Height;
    //bmp.Canvas.Brush.Color := clFuchsia;
    //bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
    Images.Draw(bmp.Canvas, 0, 0, ImageIndex, Enable);

    ms := TMemoryStream.Create;
    bmp.SaveToStream(ms);
  finally
    bmp.Free;
  end;

  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      GPBmp := TGPBitmap.Create(pstm);
      GPBmp.GetPixel(0, Img.GetHeight - 1, AClr);
      GPBmp.Free;

      r := ADVGDIP.GetRed(AClr);
      g := ADVGDIP.GetGreen(AClr);
      b := ADVGDIP.GetBlue(AClr);

      ImageAttributes := TGPImageAttributes.Create;
      ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
      graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
        0, 0,        // upper-left corner of source rectangle
        Img.GetWidth,       // width of source rectangle
        Img.GetHeight,      // height of source rectangle
        UnitPixel,
        ImageAttributes);

      ImageAttributes.Free;
      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hr);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(gr: TGPGraphics; Canvas: TCanvas; P: TPoint; bmp: TGraphic; Transparent: Boolean = False); overload;
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  graphics: TGPGraphics;
  ImageAttributes: TGPImageAttributes;
  r, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  if (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;

  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  ms := TMemoryStream.Create;
  bmp.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Transparent and (Img.GetFormat <> ifPNG) then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, 0, AClr);
        GPBmp.Free;

        r := ADVGDIP.GetRed(AClr);
        g := ADVGDIP.GetGreen(AClr);
        b := ADVGDIP.GetBlue(AClr);

        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorKey(MakeColor(r, g, b), MakeColor(r, g, b), ColorAdjustTypeDefault);
        graphics.DrawImage(Img, MakeRect(P.X, P.Y, Img.GetWidth, Img.Getheight),  // destination rectangle
          0, 0,        // upper-left corner of source rectangle
          Img.GetWidth,       // width of source rectangle
          Img.GetHeight,      // height of source rectangle
          UnitPixel,
          ImageAttributes);

        ImageAttributes.Free;
      end
      else
        graphics.DrawImage(Img, P.X, P.y);

      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hr);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(graphics: TGPGraphics; R: TRect; Pic: TGDIPPicture); overload;
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  ImageAttributes: TGPImageAttributes;
  rd, g, b: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  ms := TMemoryStream.Create;
  pic.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Img.GetFormat = ifBMP then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, Img.GetHeight - 1, AClr);
        GPBmp.Free;

        rd := ADVGDIP.GetRed(AClr);
        g := ADVGDIP.GetGreen(AClr);
        b := ADVGDIP.GetBlue(AClr);

        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorKey(MakeColor(rd, g, b), MakeColor(rd, g, b), ColorAdjustTypeDefault);
        graphics.DrawImage(Img, MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top),  // destination rectangle
          0, 0,        // upper-left corner of source rectangle
          Img.GetWidth,       // width of source rectangle
          Img.GetHeight,      // height of source rectangle
          UnitPixel,
          ImageAttributes);

        ImageAttributes.Free;
      end
      else
      begin
        graphics.DrawImageRect(Img, R.Left, R.Top, Img.GetWidth, Img.GetHeight);
      end;
      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;
end;

//------------------------------------------------------------------------------

function TrimText(Text: String; r: TRect; GDIPDraw: Boolean; graphics : TGPGraphics; Canvas: TCanvas; font: TGPFont; stringFormat: TGPStringFormat; Ellipsis: Boolean; Direction: TTabPosition; WordWrap: boolean): string;
var
  rectf: TGPRectF;
  w, h: Integer;
  x1,y1,y2: single;
  sizerect: TGPRectF;
  s, s2: string;
  i, j: integer;
  R2: TRect;
begin
  if WordWrap then
  begin
    Result := Text;
    Exit;
  end;

  //R.Right := R.Right - 2;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  x1 := r.Left;
  y1 := r.Top;
  y2 := h;

  if Direction in [tpLeft, tpRight] then
  begin
    //h := R.Right - R.Left;
    w := R.Bottom - R.Top;
  end;

  if Ellipsis then
    s := '...'
  else
    s := '';

  if GDIPDraw then
  begin
    stringFormat := TGPStringFormat.Create;
    w := w - 2;
    rectf := MakeRect(x1,y1,1000,y2);
    graphics.MeasureString(Text, Length(Text), font, rectf, stringFormat, sizerect);

    //-- Add ellipsis
    if (sizerect.Width >= w) then
    begin
      rectf := MakeRect(x1,y1,1000,y2);
      j := Length(Text);
      for i := 0 to j do
      begin
        s2 := Text + s;
        graphics.MeasureString(s2, Length(s2), font, rectf, stringFormat, sizerect);
        if (sizerect.Width >= w) and (Text <> '') then
        begin
          Text := Copy(Text, 1, Length(Text)-1);
        end
        else
        begin
          Break;
        end;
      end;
      Text := Text + s;
    end;
    stringFormat.Free;
  end
  else
  begin
    R2 := Rect(0, 0, 1000, 100);
    DrawText(Canvas.Handle,PChar(Text),Length(Text), R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    if (R2.Right >= w) then
    begin
      j := Length(Text);
      for i := 0 to j do
      begin
        s2 := Text + s;
        DrawText(Canvas.Handle,PChar(s2),Length(s2), R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
        if (R2.Right >= w) and (Text <> '') then
        begin
          Text := Copy(Text, 1, Length(Text)-1);
        end
        else
        begin
          Break;
        end;
      end;
      Text := Text + s;
    end;
  end;

  Result := Text;
end;

function TrimTextW(Text: WideString; r: TRect; GDIPDraw: Boolean; graphics : TGPGraphics; Canvas: TCanvas; font: TGPFont; stringFormat: TGPStringFormat; Ellipsis: Boolean; Direction: TTabPosition; WordWrap: boolean): WideString;
var
  rectf: TGPRectF;
  w, h: Integer;
  x1,y1,y2: single;
  sizerect: TGPRectF;
  s, s2: WideString;
  i, j: integer;
  R2: TRect;
begin
  if WordWrap then
  begin
    Result := Text;
    Exit;
  end;


  //R.Right := R.Right - 2;
  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  x1 := r.Left;
  y1 := r.Top;
  y2 := h;

  if Direction in [tpLeft, tpRight] then
  begin
    //h := R.Right - R.Left;
    w := R.Bottom - R.Top;
  end;

  if Ellipsis then
    s := '...'
  else
    s := '';
    
  if GDIPDraw then
  begin
    stringFormat := TGPStringFormat.Create;
    w := w - 2;
    rectf := MakeRect(x1,y1,1000,y2);
    graphics.MeasureString(Text, Length(Text), font, rectf, stringFormat, sizerect);
    //-- Add ellipsis
    if (sizerect.Width >= w) then
    begin
      rectf := MakeRect(x1,y1,1000,y2);
      j := Length(Text);
      for i := 0 to j do
      begin
        s2 := Text + s;
        graphics.MeasureString(s2, Length(s2), font, rectf, stringFormat, sizerect);
        if (sizerect.Width >= w) and (Text <> '') then
        begin
          Text := Copy(Text, 1, Length(Text)-1);
        end
        else
        begin
          Break;
        end;
      end;
      Text := Text + s;
    end;
    stringFormat.Free;
  end
  else
  begin
    R2 := Rect(0, 0, 1000, 100);
    DrawTextW(Canvas.Handle, PWideChar(Text), -1, R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    if (R2.Right >= w) then
    begin
      j := Length(Text);
      for i := 0 to j do
      begin
        s2 := Text + s;
        DrawTextW(Canvas.Handle, PWideChar(s2), -1, R2, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
        if (R2.Right >= w) and (Text <> '') then
        begin
          Text := Copy(Text, 1, Length(Text)-1);
        end
        else
        begin
          Break;
        end;
      end;
      Text := Text + s;
    end;
  end;

  Result := Text;
end;

//------------------------------------------------------------------------------

function WideDCTextExtent(hDC: THandle; const Text: WideString): TSize;
begin
  Result.cx := 0;
  Result.cy := 0;
  Windows.GetTextExtentPoint32W(hDC, PWideChar(Text), Length(Text), Result);
end;

function WideCanvasTextExtent(Canvas: TCanvas; const Text: WideString): TSize;
begin
  with TAccessCanvas(Canvas) do begin
    RequiredState([csHandleValid, csFontValid]);
    Result := WideDCTextExtent(Handle, Text);
  end;
end;

function WideCanvasTextWidth(Canvas: TCanvas; const Text: WideString): Integer;
begin
  Result := WideCanvasTextExtent(Canvas, Text).cX;
end;

function WideCanvasTextHeight(Canvas: TCanvas; const Text: WideString): Integer;
begin
  Result := WideCanvasTextExtent(Canvas, Text).cY;
end;

//------------------------------------------------------------------------------

function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; r: TRect; Caption: string; WideCaption: WideString; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Direction: TTabPosition; Ellipsis, WordWrap: Boolean): TRect;
var
  graphics : TGPGraphics;
  w,h: Integer;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  szRect: TRect;
  DTFLAG: DWORD;
begin
  stringFormat := nil;
  graphics := nil;
  fontFamily := nil;
  font := nil;
  solidBrush := nil;

  if (Caption <> '') or (WideCaption <> '') then
  begin
    try
      w := R.Right - R.Left;
      h := R.Bottom - R.Top;

      x1 := r.Left;
      y1 := r.Top;
      x2 := w;
      y2 := h;

      rectf := MakeRect(x1,y1,x2,y2);

      try
        if AntiAlias <> aaNone then
        begin
          graphics := TGPGraphics.Create(Canvas.Handle);
          fontFamily:= TGPFontFamily.Create(AFont.Name);

          if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
          begin
            fontFamily.Free;
            fontFamily := TGPFontFamily.Create('Arial');
          end;

          fs := 0;

          if (fsBold in AFont.Style) then
            fs := fs + 1;

          if (fsItalic in AFont.Style) then
            fs := fs + 2;

          if (fsUnderline in AFont.Style) then
            fs := fs + 4;

          font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

          graphics.SetSmoothingMode(SmoothingModeAntiAlias);

          if RealDraw then
          begin
            case Direction of
              tpTop, tpBottom: stringFormat := TGPStringFormat.Create;
              tpLeft:  stringFormat := TGPStringFormat.Create;
              tpRight: stringFormat := TGPStringFormat.Create($00000002);
            end;
          end
          else
            stringFormat := TGPStringFormat.Create;

          if Enabled then
            solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
          else
            solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

          case Alignment of
            taLeftJustify: stringFormat.SetAlignment(StringAlignmentNear);
            taCenter: stringFormat.SetAlignment(StringAlignmentCenter);
            taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
          end;

          // Center the block of text (top to bottom) in the rectangle.
          stringFormat.SetLineAlignment(StringAlignmentCenter);
          stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

          case AntiAlias of
          aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
          aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
          end;
        end;

        if AntiAlias = aaNone then
        begin
          szRect.Left := round(rectf.X);
          szRect.Top := round(rectf.Y);

          szRect.Right := szRect.Left + $FFFF;
          DTFLAG := DT_CALCRECT or DT_LEFT;
          if Ellipsis then
            DTFLAG := DTFLAG or DT_END_ELLIPSIS
          else
            DTFLAG := DTFLAG or DT_WORDBREAK;

          if RealDraw and Ellipsis then
          begin
            if (Caption <> '')  then
              Caption := TrimText(Caption, r, False, nil, Canvas, font, nil, True, Direction, WordWrap)
            else if (WideCaption <> '') then
              WideCaption := TrimTextW(WideCaption, r, False, nil, Canvas, font, nil, True, Direction, WordWrap);
          end;

          if (Caption <> '') then
            szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG)
          else
            szRect.Bottom := DrawTextW(Canvas.Handle,PWideChar(WideCaption), -1, szrect, DTFLAG);

          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          sizeRect.Width := szRect.Right - szRect.Left;
          sizeRect.Height := szRect.Bottom - szRect.Top;
        end
        else
        begin
          if RealDraw and Ellipsis then
          begin
            //stringFormat.SetTrimming(StringTrimmingEllipsisCharacter);
            if (Caption <> '') then
              Caption := TrimText(Caption, r, True, graphics, nil, font, stringformat, True, Direction, WordWrap)
            else if (WideCaption <> '') then
              WideCaption := TrimTextW(WideCaption, r, True, graphics, nil, font, stringformat, True, Direction, WordWrap);
          end;

          if (Caption <> '') then
            graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect)
          else
            graphics.MeasureString(WideCaption, Length(WideCaption), font, rectf, stringFormat, sizerect);
        end;

        Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
        rectf := MakeRect(x1,y1,x2,y2);

        if RealDraw then
        begin
          //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
          if AntiAlias = aaNone then
          begin
            szRect.Left := round(rectf.X) + 3;
            szRect.Top := round(rectf.Y);
            szRect.Right := szRect.Left + round(rectf.Width);
            szRect.Bottom := szRect.Top + round(rectf.Height);
            Canvas.Brush.Style := bsClear;

            DTFLAG := DT_LEFT;
            case Alignment of
            taRightJustify: DTFLAG := DT_RIGHT;
            taCenter: DTFLAG := DT_CENTER;
            end;

            {if Ellipsis then
            begin
              Caption := TrimText(Caption, r, False, nil, Canvas, font, stringformat, True);
            end;}
            if (Caption <> '') then
              DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE)
            else
              DrawTextW(Canvas.Handle,PWideChar(WideCaption), -1, szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE);
          end
          else
          begin
            {if Ellipsis then
            begin
              //stringFormat.SetTrimming(StringTrimmingEllipsisCharacter);
              Caption := TrimText(Caption, r, True, graphics, nil, font, stringformat, True);
            end;}


            if (Caption <> '') then
              graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush)
            else
              graphics.DrawString(WideCaption, Length(WideCaption), font, rectf, stringFormat, solidBrush);
          end;
        end;
      except

      end;

    finally

      if (AntiAlias <> aaNone) then
      begin
        if Assigned(stringformat) then
          FreeAndNil(stringformat);

        if Assigned(solidBrush) then
          FreeAndNil(solidBrush);

        if Assigned(font) then
          FreeAndNil(font);

        if Assigned(fontfamily) then
          FreeAndNil(fontfamily);

        if Assigned(graphics) then
          FreeAndNil(graphics);
      end;

    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; Caption: string; AFont: TFont;
   Images: TImageList; ImageIndex: integer; EnabledImage: Boolean; Layout: TButtonLayout;
   DropDownButton: Boolean; DrawDwLine: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
   Picture: TGDIPPicture; AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition; Progress: TTabProgress; IsOnGlass, MultiLine: Boolean); overload;
var
  graphics : TGPGraphics;
  path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  solGrBrush: TGPSolidBrush;
  linGrBrush: TGPLinearGradientBrush;
  gppen : tgppen;
  count: Integer;
  w,h,h2,w2: Integer;
  colors : array[0..0] of TGPColor;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  ImgX, ImgY, ImgW, ImgH: Integer;
  BtnR, DwR: TRect;
  AP: TPoint;
  szRect: TRect;

  procedure DrawArrow(ArP: TPoint; ArClr: TColor);
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
  end;

begin
  BtnR := R;
  if DropDownPos = dpRight then
  begin
    DwR := Rect(BtnR.Right - DropDownSectWidth, BtnR.Top, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Right := DwR.Left;
  end
  else // DropDownPos = doBottom
  begin
    DwR := Rect(BtnR.Left, BtnR.Bottom - DropDownSectWidth, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Bottom := DwR.Top;
  end;

  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;
  w2 := w div 2;

  graphics := TGPGraphics.Create(Canvas.Handle);

  if Assigned(Progress) and (Progress.Visible) then
  begin
    Progress.Draw(graphics, Rect(R.Left, R.Top, R.Right - 2, R.Bottom), RotateLeftRight, Direction);
  end
  else
  begin
    if (CTU = clNone) and (CFB = clNone) then
    begin
      solGrBrush := TGPSolidBrush.Create(ColorToARGB(CFU));
      graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w, h));
      solGrBrush.Free;
    end
    else
    case (Direction) of
      tpTop:
      begin
        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top +  h2, w, h2));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfb;
        //Canvas.FillRect(rect(r.Left , r.top +  h2, r.Right , r.Bottom ));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
        path.AddEllipse(r.Left, r.Top +  h2, w , h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2, w - 1, h2+1);
          pthGrBrush.Free;
        end
        else
        begin
          if not RotateLeftRight or MultiLine then
            graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1)
          else
            graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 + 1);
          linGrBrush.Free;
        end;

        path.Free;

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.Top , w , h2));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfu;
        //Canvas.FillRect(rect(r.Left , r.Top , r.Right , r.top +  h2));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left, r.Top - h2 , w , h);

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2+1),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.top));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w - 1, h - h2 - 1);
          pthGrBrush.Free;
        end
        else
        begin
          if (PC <> clNone) then
            graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2)
          else
            graphics.FillRectangle(linGrBrush, r.Left,r.Top + 1, w, h2);
          linGrBrush.Free;
        end;

        path.Free;

      end;
      tpBottom:
      begin
        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w , h2));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfb;
        //Canvas.FillRect(rect(r.Left , r.top, r.Right , r.top +  h2));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
        path.AddEllipse(r.Left, r.Top, w , h2);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Top));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top, w - 1, h2+1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2 + 1);
          linGrBrush.Free;
        end;

        path.Free;

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top +  h2, w , h2));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfu;
        //Canvas.FillRect(rect(r.Left , r.top +  h2, r.Right , r.Bottom));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left, r.Bottom - h2 , w , h);

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2-1,w,h2),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2, w - 1, h2 - 1);
          linGrBrush.Free;
        end;

        path.Free;
      end;
      tpLeft:
      begin
        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left + w2, r.top, w2 , h));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfb;
        //Canvas.FillRect(rect(r.Left + w2, r.top, r.Right , r.Bottom));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
        path.AddEllipse(r.Left + w2, r.Top, w , h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.Top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left + w2, r.Top, w2 + 1, h-1);
          pthGrBrush.Free;
        end
        else
        begin
          if MultiLine then
            graphics.FillRectangle(linGrBrush, r.Left + w2 + 1,r.Top, w2 - 2, h - 2)
          else
            graphics.FillRectangle(linGrBrush, r.Left + w2 + 1,r.Top, w2 + 1, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.Top , w2 , h));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfu;
        //Canvas.FillRect(rect(r.Left , r.Top , r.Left + w2 , r.Bottom));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left - w2, r.Top, w , h);

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

      end;
      tpRight:
      begin

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Right - w2 , r.Top , w2, h));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfu;
        //Canvas.FillRect(rect(r.Right - w2 , r.Top , r.Right ,r.Bottom));

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Right - w2, r.Top, w, h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w2,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Right - w2 + 1,r.Top + 1, w2 - 1, h - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Right - w2, r.Top + 1, w2, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w2, h));
        solGrBrush.Free;

        //Canvas.Brush.Color := cfb;
        //Canvas.FillRect(rect(r.Left , r.top, r.Left + w2, r.Bottom ));


        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
        path.AddEllipse(r.Left - w2, r.Top, w , h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2+2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.Top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left,r.Top, w2 + 1, h-1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left,r.Top, w2 + 2, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

      end;
    end;
  end;


  gppen := tgppen.Create(ColorToARGB(PC),1);

  graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if (PC <> clNone) then
  begin
    if IsOnGlass then
    begin
      if Direction in [tpTop, tpBottom] then
        DrawRect(graphics, gppen,r.Left - 1,r.Top, w + 1, h - 1)
      else if (Direction = tpLeft) then
        DrawRect(graphics, gppen,r.Left,r.Top, w, h - 1)
      else
        DrawRect(graphics, gppen,r.Left - 1,r.Top, w, h - 1);
    end
    else
    begin
      if not RoundEdges then
        DrawRect(graphics, gppen,r.Left,r.Top, w - 1, h - 1)
      else
        DrawRoundRect(graphics, gppen,r.Left,r.Top, w - 1, h - 1, 3);
    end;
  end;

  gppen.Free;

  if Focus then
  begin
    gppen := tgppen.Create(ColorToARGB($E4AD89),1);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 1,r.Top + 1, r.Right - 3, r.Bottom - 3, 3);
    gppen.Free;
    gppen := tgppen.Create(ColorToARGB(clgray),1);
    gppen.SetDashStyle(DashStyleDot);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 2,r.Top + 2, r.Right - 5, r.Bottom - 5, 3);
    gppen.Free;
  end;

  fontFamily:= TGPFontFamily.Create(AFont.Name);

  fs := 0;

  ImgH := 0;
  ImgW := 0;
  ImgY := 0;
  ImgX := 0;

  if (fsBold in AFont.Style) then
    fs := fs + 1;
  if (fsItalic in AFont.Style) then
    fs := fs + 2;
  if (fsUnderline in AFont.Style) then
    fs := fs + 4;

  if Assigned(Picture) and not Picture.Empty then
  begin
    Picture.GetImageSizes;
    ImgW := Picture.Width;
    ImgH := Picture.Height;
  end
  else
  begin
    if (ImageIndex > -1) and Assigned(Images) then
    begin
      ImgW := Images.Width;
      ImgH := Images.Height;
    end;
  end;

  if (Caption <> '') then
  begin
    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    w := BtnR.Right - BtnR.Left;
    h := BtnR.Bottom - BtnR.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    rectf := MakeRect(x1,y1,x2,y2);

    stringFormat := TGPStringFormat.Create;

    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    // Center-justify each line of text.
    stringFormat.SetAlignment(StringAlignmentCenter);

    // Center the block of text (top to bottom) in the rectangle.
    stringFormat.SetLineAlignment(StringAlignmentCenter);

    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    //graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);
    if AntiAlias = aaNone then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;
      szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
      graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);

    if (ImgW > 0) then
    begin
      case Layout of
        blGlyphLeft:
        begin
          x1 := r.Left + 2 + ImgW;
          x2 := w - 2 - ImgW;

          ImgX := round(sizerect.X - ImgW div 2);
          if ImgX < 2 then ImgX := 2;
          ImgY := r.Top + Max(0, (h - ImgH) div 2);
        end;
        blGlyphTop:
        begin
          y1 := r.Top{ + 2} + ImgH;
          y2 := h - 2 - ImgH;

          ImgX := r.Left + Max(0, (w - ImgW) div 2);
          ImgY := round(y2 - sizerect.Height);
          ImgY := Max(0, ImgY div 2);
          ImgY := round(y1) - ImgH + ImgY; //round(sizerect.Height) - ImgY - 4;
          if ImgY < 2 then ImgY := 2;
        end;
        blGlyphRight:
        begin
          x1 := 2;
          x2 := w - 4 - ImgW;

          ImgX := Round(X2 - sizerect.width);
          ImgX := Max(0, ImgX div 2);
          ImgX := ImgX + Round(sizerect.width) + 4;
          if ImgX > (w - ImgW) then
            ImgX := w - ImgW - 2;
          ImgY := r.Top + Max(0, (h - ImgH) div 2);
        end;
        blGlyphBottom:
        begin
          y1 := 2;
          y2 := h - 2 - ImgH;

          ImgX := r.Left + Max(0, (w - ImgW) div 2);
          ImgY := Round(y2 - sizerect.Height);
          ImgY := Max(0, ImgY div 2);
          ImgY := round(sizerect.Height + 2) + ImgY;
          if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
        end;
      end;
    end;

    rectf := MakeRect(x1,y1,x2,y2);

    //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
    if AntiAlias = aaNone then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);
      szRect.Right := szRect.Left + round(rectf.Width);
      szRect.Bottom := szRect.Top + round(rectf.Height);
      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CENTER or DT_VCENTER or DT_SINGLELINE)
    end
    else
      graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);

    stringformat.Free;
    font.Free;
  end;

  fontfamily.Free;

  if DropDownButton then
  begin

    if DropDownPos = dpRight then
      w := w - 8
    else
      h := h - 8;
  end;

  if Assigned(Picture) and not Picture.Empty then
  begin
     if Caption = '' then
       Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Picture)
     else
       Canvas.Draw(ImgX, ImgY, Picture);
  end
  else
    if (ImageIndex <> -1) and Assigned(Images) then
    begin
      if Caption = '' then
        Images.Draw(Canvas, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2), ImageIndex, EnabledImage)
      else
      begin
        Images.Draw(Canvas, ImgX, ImgY, ImageIndex, EnabledImage);
      end;
    end;

  Canvas.Brush.Style := bsClear;
  if DropDownButton then
  begin
    if DrawDwLine then
    begin
      Canvas.Pen.Color := PC;
      //Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 6, 6);
      if (DropDownPos = dpRight) then
      begin
        Canvas.MoveTo(DwR.Left, DwR.Top);
        Canvas.LineTo(DwR.Left, DwR.Bottom);
      end
      else
      begin
        Canvas.MoveTo(DwR.Left, DwR.Top);
        Canvas.LineTo(DwR.Right, DwR.Top);
      end;
    end;
    AP.X := DwR.Left + ((DwR.Right - DwR.Left - 5) div 2);
    AP.Y := DwR.Top + ((DwR.Bottom - DwR.Top - 3) div 2) + 1;
    if not Enabled then
      DrawArrow(AP, clGray)
    else
      DrawArrow(AP, clBlack);
  end;

  graphics.Free;
end;


procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; Caption:string; AFont: TFont; Layout: TButtonLayout; Enabled: Boolean; Focus: Boolean;
   AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Progress: TTabProgress; IsOnGlass, MultiLine: Boolean; Direction: TTabPosition = tpTop); overload;
begin
  DrawVistaGradient(Canvas, r, CFU, CTU, CFB, CTB, PC, GradientU,GradientB, Caption, AFont,
   nil, -1, True, Layout, False, False, Enabled, Focus, dpRight, nil, AntiAlias, RoundEdges, RotateLeftRight, Direction, Progress, IsOnGlass, MultiLine);
end;

//------------------------------------------------------------------------------

function GetTabPath(R: TRect; Shape: TAdvTabShape; Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition): TGPGraphicsPath;
var
  p, P2: array[0..2] of TGPPoint;
  P5, p6: array[0..3] of TGPPoint;
  tension: double;
  w, h, i, j, h3, w3, rd2: Integer;
begin
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;
  //h2 := h div 2;
  h3 := h div 3;
  w3 := w div 3;
  tension := 0.8;
  i := 3;

  Result := TGPGraphicsPath.Create;
  case Shape of
    tsRectangle:
    begin
      case (Direction) of
        tpTop:
        begin
          p[0] := MakePoint(R.Left, R.Top + Rounding * i);
          p[1] := MakePoint(R.Left + Rounding, R.Top + Rounding);
          p[2] := MakePoint(R.Left + Rounding * i, R.Top);
          Result.AddLine(R.Left, R.Bottom, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right - (Rounding * i), R.Top);
          p2[1] := MakePoint(R.Right - Rounding, R.Top + Rounding);
          p2[2] := MakePoint(R.Right, R.Top + (Rounding * i));
          Result.AddLine(P[2].x, R.Top, P2[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(R.Right, P2[2].Y, R.Right, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          p[0] := MakePoint(R.Left, R.Bottom - Rounding * i);
          p[1] := MakePoint(R.Left + Rounding, R.Bottom - Rounding);
          p[2] := MakePoint(R.Left + Rounding * i, R.Bottom);
          Result.AddLine(R.Left, R.Top, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right - (Rounding * i), R.Bottom);
          p2[1] := MakePoint(R.Right - Rounding, R.Bottom - Rounding);
          p2[2] := MakePoint(R.Right, R.Bottom - (Rounding * i));
          Result.AddLine(P[2].x, R.Bottom, P2[0].X, R.Bottom);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(R.Right, P2[2].Y, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          p[0] := MakePoint(R.Left + Rounding * i, R.Top);
          p[1] := MakePoint(R.Left + Rounding, R.Top + Rounding);
          p[2] := MakePoint(R.Left, R.Top + Rounding * i);
          Result.AddLine(R.Right, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Left, R.Bottom - (Rounding * i));
          p2[1] := MakePoint(R.Left + Rounding, R.Bottom - Rounding);
          p2[2] := MakePoint(R.Left + (Rounding * i), R.Bottom);
          Result.AddLine(R.Left, P[2].Y, R.Left, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(p2[2].X, R.Bottom, R.Right, R.Bottom);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          p[0] := MakePoint(R.Right - Rounding * i, R.Top);
          p[1] := MakePoint(R.Right - Rounding, R.Top + Rounding);
          p[2] := MakePoint(R.Right, R.Top + Rounding * i);
          Result.AddLine(R.Left, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom - (Rounding * i));
          p2[1] := MakePoint(R.Right - Rounding, R.Bottom - Rounding);
          p2[2] := MakePoint(R.Right - (Rounding * i), R.Bottom);
          Result.AddLine(R.Right, P[2].Y, R.Right, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(p2[2].X, R.Bottom, R.Left, R.Bottom);
          Result.CloseFigure;
        end;
      end;
    end;
    tsLeftRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Bottom);
          p5[1] := MakePoint(R.Left + Rounding*i, R.Bottom - Rounding{* 2});
          p5[2] := MakePoint(R.Left - Rounding + j, R.Top + Rounding);
          p5[3] := MakePoint(R.Left + (Rounding * 2) + j, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          rd2 := Max(0, Rounding div 2);
          
          p2[0] := MakePoint(R.Right - (rd2 * i), R.Top);
          p2[1] := MakePoint(R.Right - rd2, R.Top + rd2);
          p2[2] := MakePoint(R.Right, R.Top + (rd2 * i));
          Result.AddLine(P5[3].x, R.Top, P2[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.AddLine(R.Right, p2[2].Y, R.Right, R.Bottom);

          //Result.AddLine(R.Right, R.Bottom, R.Left, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left + (Rounding * 2) + j, R.Bottom);
          p5[1] := MakePoint(R.Left - Rounding + j, R.Bottom - Rounding);
          p5[2] := MakePoint(R.Left + Rounding*i, R.Top + Rounding{* 2});
          p5[3] := MakePoint(R.Left, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          rd2 := Max(0, Rounding div 2);

          p2[0] := MakePoint(R.Right, R.Bottom - (rd2 * i));
          p2[1] := MakePoint(R.Right - rd2, R.Bottom - rd2);
          p2[2] := MakePoint(R.Right - (rd2 * i), R.Bottom);

          Result.AddLine(R.Left, R.top, R.Right, R.Top);
          Result.AddLine(R.Right, R.Top, R.Right, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          //Result.AddLine(P2[2].X, R.Bottom, P5[0].x, R.Bottom);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Left + Rd2 * i, R.Top);
          p[1] := MakePoint(R.Left + Rd2, R.Top + Rd2);
          p[2] := MakePoint(R.Left, R.Top + Rd2 * i);
          Result.AddLine(R.Right, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Left, R.Bottom - (Rounding * 2) - j);
          p5[1] := MakePoint(R.Left + Rounding, R.Bottom + Rounding - j);
          p5[2] := MakePoint(R.Right - Rounding{* 2}, R.Bottom - Rounding * i);
          p5[3] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(R.Left, P[2].Y, R.Left, P5[0].Y);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);
          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Top);
          p5[1] := MakePoint(R.Left + Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Right - Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Right, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          rd2 := Max(0, Rounding div 2);
          p2[0] := MakePoint(R.Right, R.Bottom - (Rd2 * i));
          p2[1] := MakePoint(R.Right - Rd2, R.Bottom - Rd2);
          p2[2] := MakePoint(R.Right - (Rd2 * i), R.Bottom);

          Result.AddLine(R.Right, P5[3].Y, R.Right, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);

          Result.AddLine(p2[2].X, R.Bottom, R.Left, R.Bottom);
          //Result.AddLine(R.Right, R.Bottom, R.Right, Top);
          Result.CloseFigure;
        end;
      end;
    end;
    tsRightRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          //k := 0;
          //if (Rounding * i > h2) then
          //  k := i div 2;

          j := h3 + Rounding;
          //k := (j div 2);

          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Left, R.Top + rd2 * i);
          p[1] := MakePoint(R.Left + rd2, R.Top + rd2);
          p[2] := MakePoint(R.Left + rd2 * i, R.Top);

          Result.AddLine(R.Left, R.Bottom, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Right - (Rounding * 2) - j, R.Top);
          p5[1] := MakePoint(R.Right + Rounding - j, R.Top + Rounding);
          p5[2] := MakePoint(R.Right - Rounding*i, R.Bottom - Rounding{* 2});
          p5[3] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(P[2].x, R.Top, P5[0].X, R.Top);
          //Result.AddCurve(PGPPoint(@p5), 4, tension);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          Result.AddLine(R.Left, R.Bottom, R.Right, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Left, R.Bottom - rd2 * i);
          p[1] := MakePoint(R.Left + Rd2, R.Bottom - Rd2);
          p[2] := MakePoint(R.Left + Rd2 * i, R.Bottom);
          Result.AddLine(R.Left, R.Top, R.Left, P[0].Y);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Right - (Rounding * 2) - j, R.Bottom);
          p5[1] := MakePoint(R.Right + Rounding - j, R.Bottom - Rounding);
          p5[2] := MakePoint(R.Right - Rounding*i, R.Top + Rounding{* 2});
          p5[3] := MakePoint(R.Right, R.Top);
          Result.AddLine(P[2].x, R.Bottom, P5[0].X, R.Bottom);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          Result.AddLine(R.Left, R.Top, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p5[0] := MakePoint(R.Right, R.Top);
          p5[1] := MakePoint(R.Right - Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Left + Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Left, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p2[0] := MakePoint(R.Left, R.Bottom - (Rd2 * i));
          p2[1] := MakePoint(R.Left + Rd2, R.Bottom - Rd2);
          p2[2] := MakePoint(R.Left + (Rd2 * i), R.Bottom);

          Result.AddLine(R.Left, P5[3].Y, R.Left, p2[0].Y);
          Result.AddCurve(PGPPoint(@p2), 3, tension);

          Result.AddLine(p2[2].X, R.Bottom, R.Right, R.Bottom);
          //Result.AddLine(R.Right, R.Bottom, R.Right, Top);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;
          rd2 := Max(0, Rounding div 2);

          p[0] := MakePoint(R.Right - Rd2 * i, R.Top);
          p[1] := MakePoint(R.Right - Rd2, R.Top + Rd2);
          p[2] := MakePoint(R.Right, R.Top + Rd2 * i);
          Result.AddLine(R.Left, R.Top, p[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p5[0] := MakePoint(R.Right, R.Bottom - (Rounding * 2) - j);
          p5[1] := MakePoint(R.Right - Rounding, R.Bottom + Rounding - j);
          p5[2] := MakePoint(R.Left + Rounding{* 2}, R.Bottom - Rounding * i);
          p5[3] := MakePoint(R.Left, R.Bottom);
          Result.AddLine(R.Right, P[2].Y, R.Right, P5[0].Y);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);
          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
      end;
    end;
    tsLeftRightRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Bottom);
          p5[1] := MakePoint(R.Left + Rounding*i, R.Bottom - Rounding{* 2});
          p5[2] := MakePoint(R.Left - Rounding + j, R.Top + Rounding);
          p5[3] := MakePoint(R.Left + (Rounding * 2) + j, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Right - (Rounding * 2) - j, R.Top);
          p6[1] := MakePoint(R.Right + Rounding - j, R.Top + Rounding);
          p6[2] := MakePoint(R.Right - Rounding*i, R.Bottom - Rounding{* 2});
          p6[3] := MakePoint(R.Right, R.Bottom);

          Result.AddLine(p5[3].X, R.Top, p6[0].X, R.Top);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);
          
          //Result.AddLine(R.Right, R.Bottom, R.Left, R.Bottom);
          Result.CloseFigure;
        end;
        tpBottom:
        begin
          j := h3 + Rounding;

          p5[0] := MakePoint(R.Left + (Rounding * 2) + j, R.Bottom);
          p5[1] := MakePoint(R.Left - Rounding + j, R.Bottom - Rounding);
          p5[2] := MakePoint(R.Left + Rounding*i, R.Top + Rounding{* 2});
          p5[3] := MakePoint(R.Left, R.Top);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Right, R.Top);
          p6[1] := MakePoint(R.Right - Rounding*i, R.Top + Rounding{* 2});
          p6[2] := MakePoint(R.Right + Rounding - j, R.Bottom - Rounding);
          p6[3] := MakePoint(R.Right - (Rounding * 2) - j, R.Bottom);

          Result.AddLine(R.Left, R.top, R.Right, R.Top);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);
          Result.AddLine(P6[3].x, R.Bottom, P5[0].X, R.Bottom);

          Result.CloseFigure;
        end;
        tpLeft:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;

          p5[0] := MakePoint(R.Right, R.Top);
          p5[1] := MakePoint(R.Right - Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Left + Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Left, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Left, R.Bottom - (Rounding * 2) - j);
          p6[1] := MakePoint(R.Left + Rounding, R.Bottom + Rounding - j);
          p6[2] := MakePoint(R.Right - Rounding{* 2}, R.Bottom - Rounding * i);
          p6[3] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(R.Left, P5[3].Y, R.Left, P6[0].Y);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);
          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          j := w3 + Rounding;
          if not RotateLeftRight then
            j := h3 + Rounding;

          p5[0] := MakePoint(R.Left, R.Top);
          p5[1] := MakePoint(R.Left + Rounding{* 2}, R.Top + Rounding*i);
          p5[2] := MakePoint(R.Right - Rounding, R.Top - Rounding + j);
          p5[3] := MakePoint(R.Right, R.Top + (Rounding * 2) + j);
          Result.AddBezier(P5[0],P5[1], P5[2], P5[3]);

          p6[0] := MakePoint(R.Right, R.Bottom - (Rounding * 2) - j);
          p6[1] := MakePoint(R.Right - Rounding, R.Bottom + Rounding - j);
          p6[2] := MakePoint(R.Left + Rounding{* 2}, R.Bottom - Rounding * i);
          p6[3] := MakePoint(R.Left, R.Bottom);
          Result.AddLine(R.Right, P5[3].Y, R.Right, P6[0].Y);
          Result.AddBezier(P6[0],P6[1], P6[2], P6[3]);

          //Result.AddLine(R.Right, R.Bottom, R.Right, R.Top);
          Result.CloseFigure;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawVistaTab(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor; GradientU,GradientB: TGDIPGradient;
   Enabled: Boolean; Shape: TAdvTabShape; Focus: Boolean; {AntiAlias: TAntiAlias; }Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition; Progress: TTabProgress);
var
  graphics : TGPGraphics;
  TabPath, path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  solGrBrush: TGPSolidBrush;
  linGrBrush: TGPLinearGradientBrush;
  gppen : tgppen;
  count: Integer;
  w,h,h2,w2: Integer;
  colors : array[0..0] of TGPColor;
  BtnR: TRect;
  Rgn: TGPRegion;
begin
  BtnR := R;

  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;
  w2 := w div 2;


  graphics := TGPGraphics.Create(Canvas.Handle);

  Tabpath := GetTabPath(R, Shape, Rounding, RotateLeftRight, Direction);

  if (Direction in [tpLeft, tpRight]) and not RotateLeftRight then
  begin
    Direction := tpTop;
    RotateLeftRight := False;
  end;

  Rgn := TGPRegion.Create(TabPath);
  Graphics.SetClip(Rgn);

  if Assigned(Progress) and (Progress.Visible) then
  begin
    Progress.Draw(graphics, R, RotateLeftRight, Direction);
  end
  else
  begin
    case (Direction) of
      tpTop:
      begin
        // down ellips brush
        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        Graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top +  h2, w, h2));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left, r.Top +  h2, w , h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          Graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2, w - 1, h2+1);
          pthGrBrush.Free;
        end
        else
        begin
          if not RotateLeftRight then
            Graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1)
          else
            Graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 + 1);
          linGrBrush.Free;
        end;

        path.Free;
        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.Top , w , h2));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left, r.Top - h2 , w , h);

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2+1),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.top));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w - 1, h - h2 - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2);
          linGrBrush.Free;
        end;

        path.Free;
      end;
      tpBottom:
      begin
        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w , h2));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
        path.AddEllipse(r.Left, r.Top, w , h2);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Top));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top, w - 1, h2+1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2 + 1);
          linGrBrush.Free;
        end;

        path.Free;

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top +  h2, w , h2));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left, r.Bottom - h2 , w , h);

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2-1,w,h2),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeVertical);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2, w - 1, h2 - 1);
          linGrBrush.Free;
        end;

        path.Free;
      end;
      tpLeft:
      begin
        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left + w2, r.top, w2 , h));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left + w2, r.Top, w , h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.Top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left + w2, r.Top, w2 + 1, h-1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + w2 + 1,r.Top, w2 + 1, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.Top , w2 , h));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left - w2, r.Top, w , h);

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

      end;
      tpRight:
      begin

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfu));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Right - w2 , r.Top , w2, h));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Right - w2, r.Top, w, h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientU of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w2,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
        end;

        if GradientU = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTU));

          colors[0] := ColorToARGB(CFU);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);

          graphics.FillRectangle(pthGrBrush, r.Right - w2 + 1,r.Top + 1, w2 - 1, h - 1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Right - w2, r.Top + 1, w2, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

        // down ellips brush

        solGrBrush := TGPSolidBrush.Create(ColorToARGB(cfb));
        graphics.FillRectangle(solGrBrush, MakeRect(r.Left , r.top, w2, h));
        solGrBrush.Free;

        // Create a path that consists of a single ellipse.
        path := TGPGraphicsPath.Create;
        path.AddEllipse(r.Left - w2, r.Top, w , h);

        pthGrBrush := nil;
        linGrBrush := nil;

        case GradientB of
        ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
        ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2+2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeHorizontal);
        ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
        ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
        end;

        if GradientB = ggRadial then
        begin
          pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.Top + h2));

          // Set the color at the center point to blue.
          pthGrBrush.SetCenterColor(ColorToARGB(CTB));

          colors[0] := ColorToARGB(CFB);
          count := 1;
          pthGrBrush.SetSurroundColors(@colors, count);
          graphics.FillRectangle(pthGrBrush, r.Left,r.Top, w2 + 1, h-1);
          pthGrBrush.Free;
        end
        else
        begin
          graphics.FillRectangle(linGrBrush, r.Left,r.Top, w2 + 2, h - 1);
          linGrBrush.Free;
        end;

        path.Free;

      end;
    end;
  end;

  if Rounding > 0 then
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if (PC <> clNone) then
  begin
    graphics.ResetClip;
    gppen := TGPPen.Create(ColorToARGB(PC), 1.6);
    graphics.DrawPath(gpPen, TabPath);
    gppen.Free;
  end;

  if Focus then
  begin
    gppen := TGPPen.Create(ColorToARGB($E4AD89),1);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(Graphics, gppen,r.Left + 1,r.Top + 1, r.Right - 3, r.Bottom - 3, 3);
    gppen.Free;

    gppen := TGPPen.Create(ColorToARGB(clgray),1);
    try
      gppen.SetDashStyle(DashStyleDot);
      graphics.SetSmoothingMode(SmoothingModeAntiAlias);
      DrawRoundRect(Graphics, gppen,r.Left + 2,r.Top + 2, r.Right - 5, r.Bottom - 5, 3);
    finally
      gppen.Free;
    end;
  end;

  if Assigned(Rgn) then
    Rgn.Free;

  TabPath.Free;
  Graphics.Free;
end;

//------------------------------------------------------------------------------

function GetInsertButtonPath(R: TRect; Shape: TAdvTabShape; Rounding: TTabRounding; RotateLeftRight: Boolean; Direction: TTabPosition): TGPGraphicsPath;
var
  p, P2: array[0..2] of TGPPoint;
  tension: double;
  w, h, rd2: Integer;
begin
  h := r.Bottom - r.Top;
  tension := 0.3;

  Result := TGPGraphicsPath.Create;
  case Shape of
    tsRectangle:
    begin
      case (Direction) of
        tpTop, tpBottom:
        begin
          R.Left := R.Left + 2;
          R.Right := R.Right - 1;
          w := r.Right - r.Left;
          rd2 := Min(6, Max(0, Rounding));

          Result.AddLine(R.Left + rd2, R.Top, R.Left + w - (rd2*2), R.Top);
          Result.AddArc(R.Left + w - (rd2*2), R.Top, rd2*2, rd2*2, 270, 90);
          Result.AddLine(R.Left + w, R.Top + rd2, R.Left + w, R.Top + h - (rd2*2));
          Result.AddArc(R.Left + w - (rd2*2), R.Top + h - (rd2*2), rd2*2, rd2*2,0,90);
          Result.AddLine(R.Left + w - (rd2*2), R.Top + h, R.Left + rd2, R.Top + h);
          Result.AddArc(R.Left, R.Top + h - (rd2*2), rd2*2, rd2*2, 90, 90);
          Result.AddLine(R.Left, R.Top + h - (rd2*2), R.Left, R.Top + rd2);
          Result.AddArc(R.Left, R.Top, rd2*2, rd2*2, 180, 90);
          Result.CloseFigure;
        end;
        tpLeft, tpRight:
        begin
          R.Top := R.Top + 2;
          R.Bottom := R.Bottom - 2;
          w := r.Right - r.Left;
          h := r.Bottom - r.Top;
          rd2 := Min(6, Max(0, Rounding));

          Result.AddLine(R.Left + rd2, R.Top, R.Left + w - (rd2*2), R.Top);
          Result.AddArc(R.Left + w - (rd2*2), R.Top, rd2*2, rd2*2, 270, 90);
          Result.AddLine(R.Left + w, R.Top + rd2, R.Left + w, R.Top + h - (rd2*2));
          Result.AddArc(R.Left + w - (rd2*2), R.Top + h - (rd2*2), rd2*2, rd2*2,0,90);
          Result.AddLine(R.Left + w - (rd2*2), R.Top + h, R.Left + rd2, R.Top + h);
          Result.AddArc(R.Left, R.Top + h - (rd2*2), rd2*2, rd2*2, 90, 90);
          Result.AddLine(R.Left, R.Top + h - (rd2*2), R.Left, R.Top + rd2);
          Result.AddArc(R.Left, R.Top, rd2*2, rd2*2, 180, 90);
          Result.CloseFigure;
        end;
      end;
    end;
    tsLeftRightRamp:
    begin
      case (Direction) of
        tpTop:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Left, R.Top);
          p[1] := MakePoint(R.Right - rd2 -2, R.Top + 1);
          p[2] := MakePoint(R.Right, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom);
          p2[1] := MakePoint(R.Left + rd2 + 2, R.Bottom - 1);
          p2[2] := MakePoint(R.Left, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
          {
          p[0] := MakePoint(R.Right - Rounding*4, R.Top);
          p[1] := MakePoint(R.Right - Rounding*2 -1, R.Top + 1);
          p[2] := MakePoint(R.Right, R.Bottom);
          Result.AddLine(R.Left, R.Top, P[0].X, R.Top);
          Result.AddCurve(PGPPoint(@p), 3, 0.1);

          p2[0] := MakePoint(R.Left + Rounding*6 +1, R.Bottom);
          p2[1] := MakePoint(R.Left + Rounding*2 +1, R.Bottom - 1);
          p2[2] := MakePoint(R.Left, R.Top);
          Result.AddLine(P2[0].X, R.Bottom, R.Right, R.Bottom);
          Result.AddCurve(PGPPoint(@p2), 3, 0.1);
          Result.CloseFigure;
          }
          {
          p[0] := MakePoint(R.Left, R.Top);
          p[1] := MakePoint(R.Right - Rounding*2 -1, R.Top + 1);
          p[2] := MakePoint(R.Right, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom);
          p2[1] := MakePoint(R.Left + Rounding*2 +1, R.Bottom - 1);
          p2[2] := MakePoint(R.Left, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
          }
        end;
        tpBottom:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Right, R.Top);
          p[1] := MakePoint(R.Left + rd2 +2, R.Top + 1);
          p[2] := MakePoint(R.Left, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Left, R.Bottom);
          p2[1] := MakePoint(R.Right - rd2 - 2, R.Bottom - 1);
          p2[2] := MakePoint(R.Right, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
        tpLeft:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Left, R.Top);
          p[1] := MakePoint(R.Right - rd2, R.Top + rd2);
          p[2] := MakePoint(R.Right, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Right, R.Bottom);
          p2[1] := MakePoint(R.Left + rd2, R.Bottom - rd2);
          p2[2] := MakePoint(R.Left, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
        tpRight:
        begin
          rd2 := Min(6, Max(0, Rounding));
          p[0] := MakePoint(R.Right, R.Top);
          p[1] := MakePoint(R.Left + rd2, R.Top + rd2);
          p[2] := MakePoint(R.Left, R.Bottom);
          Result.AddCurve(PGPPoint(@p), 3, tension);

          p2[0] := MakePoint(R.Left, R.Bottom);
          p2[1] := MakePoint(R.Right - rd2, R.Bottom - rd2);
          p2[2] := MakePoint(R.Right, R.Top);
          Result.AddCurve(PGPPoint(@p2), 3, tension);
          Result.CloseFigure;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TPagerTabSettings }

constructor TPagerTabSettings.Create;
begin
  inherited;
  FLeftMargin := 4;
  FRightMargin := 4;
  FHeight := DEFAULT_TABHEIGHT;
  FStartMargin := 4;
  FEndMargin := 0;
  FSpacing := 4;
  FWidth := 0;
  FWordWrap := False;
  FImagePosition := ipLeft;
  FShape := tsRectangle;
  FRounding := 1;
  FAlignment := taLeftJustify;
  FGlass := False;
  FButtonBorder := true;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.Assign(Source: TPersistent);
begin
  if (Source is TPagerTabSettings) then
  begin
    LeftMargin := (Source as TPagerTabSettings).LeftMargin;
    RightMargin := (Source as TPagerTabSettings).RightMargin;
    Height := (Source as TPagerTabSettings).Height;
    StartMargin := (Source as TPagerTabSettings).StartMargin;
    EndMargin := (Source as TPagerTabSettings).EndMargin;
    Width := (Source as TPagerTabSettings).Width;
    WordWrap := (Source as TPagerTabSettings).WordWrap;
    ImagePosition := (Source as TPagerTabSettings).ImagePosition;
    Shape := (Source as TPagerTabSettings).Shape;
    Rounding := (Source as TPagerTabSettings).Rounding;
    Alignment := (Source as TPagerTabSettings).Alignment;
    Glass := (Source as TPagerTabSettings).Glass;
    ButtonBorder := (Source as TPagerTabSettings).ButtonBorder;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetLeftMargin(const Value: Integer);
begin
  if (FLeftMargin <> Value) then
  begin
    FLeftMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetRightMargin(const Value: Integer);
begin
  if (FRightMargin <> Value) then
  begin
    FRightMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
  begin
    FHeight := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetStartMargin(const Value: Integer);
begin
  if (FStartMargin <> Value) then
  begin
    FStartMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetEndMargin(const Value: Integer);
begin
  if (FEndMargin <> Value) then
  begin
    FEndMargin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetGlass(const Value: Boolean);
begin
  if (FGlass <> Value) then
  begin
    FGlass := Value;
    if Assigned(FPager) then
      FPager.GlassChanged;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetSpacing(const Value: Integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetWordWrap(const Value: Boolean);
begin
  if (FWordWrap <> Value) then
  begin
    FWordWrap := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetImagePosition(const Value: TImagePosition);
begin
  if (FImagePosition <> Value) then
  begin
    FImagePosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetRounding(const Value: TTabRounding);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetShape(const Value: TAdvTabShape);
begin
  if (FShape <> Value) then
  begin
    FShape := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetAlignment(const Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPagerTabSettings.SetButtonBorder(const Value: boolean);
begin
  if (FButtonBorder <> Value) then
  begin
    FButtonBorder := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TPagerTabScroller }

constructor TPagerTabScroller.Create;
begin
  inherited;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FVisible := False;
end;

//------------------------------------------------------------------------------

function TPagerTabScroller.CanGoBack: Boolean;
begin
  Result := Position > Min;
end;

//------------------------------------------------------------------------------

function TPagerTabScroller.CanGoForward: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------------------

procedure TPagerTabScroller.SetMax(const Value: integer);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------------------

procedure TPagerTabScroller.SetMin(const Value: integer);
begin
  if Value <= FMax then FMin := Value;
end;

//------------------------------------------------------------------------------

procedure TPagerTabScroller.SetPosition(const Value: integer);
begin
  FPosition := Value;
end;

//------------------------------------------------------------------------------

procedure TPagerTabScroller.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

//------------------------------------------------------------------------------

{ TGradientBackground }

procedure TGradientBackground.Assign(Source: TPersistent);
begin
  if (Source is TGradientBackground) then
  begin
    FColor := (Source as TGradientBackground).Color;
    FColorTo := (Source as TGradientBackground).ColorTo;
    FDirection := (Source as TGradientBackground).Direction;
    FSteps := (Source as TGradientBackground).Steps;
  end;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TGradientBackground.Create;
begin
  inherited;
  Color := clWhite;
  ColorTo := clBtnFace;
  Steps := 64;
  Direction := gdHorizontal;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetColor(const Value: TColor);
begin
  FColor := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetColorTo(const Value: TColor);
begin
  FColorTo := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetDirection(
  const Value: TGradientDirection);
begin
  FDirection := Value;
  Changed;
end;

//------------------------------------------------------------------------------

procedure TGradientBackground.SetSteps(const Value: Integer);
begin
  FSteps := Value;
  Changed;
end;


//------------------------------------------------------------------------------

{ TVistaBackground }

constructor TVistaBackground.Create;
begin
  inherited;
  FSteps := 64;
  FColor := clWhite;
  FColorTo := clWhite;
  FColorMirror := clSilver;
  FColorMirrorTo := clWhite;
  FBorderColor := clGray;
  FGradient := ggVertical;
  FGradientMirror := ggVertical;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.Assign(Source: TPersistent);
begin
  if (Source is TVistaBackground) then
  begin
    FSteps := (Source as TVistaBackground).Steps;
    FColor := (Source as TVistaBackground).Color;
    FColorTo := (Source as TVistaBackground).ColorTo;
    FColorMirror := (Source as TVistaBackground).ColorMirror;
    FColorMirrorTo := (Source as TVistaBackground).ColorMirrorTo;
    FBorderColor := (Source as TVistaBackground).BorderColor;
    Gradient := (Source as TVistaBackground).Gradient;
    GradientMirror := (Source as TVistaBackground).GradientMirror;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorTo(const Value: TColor);
begin
  if (FColorTo  <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetGradient(const Value: TGDIPGradient);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetGradientMirror(const Value: TGDIPGradient);
begin
  if(FGradientMirror <> Value) then
  begin
    FGradientMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TVistaBackground.SetSteps(const Value: Integer);
begin
  if (FSteps <> Value) then
  begin
    FSteps := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TCustomTabAppearance }

constructor TCustomTabAppearance.Create;
begin
  inherited;
  FBorderColor := clBtnFace;
  FBorderColorHot := clBlue;
  FBorderColorSelectedHot := clNone;
  FBorderColorDown := clNone;
  FColor := clBtnFace;
  FColorTo := clWhite;
  FColorHot := clYellow;
  FColorHotTo := clNone;
  FColorSelectedTo := clBtnFace;
  FBorderColorDisabled := clNone;
  FBorderColorSelected := clBtnFace;
  FColorDisabled := clNone;
  FColorDisabledTo := clNone;
  FColorSelected := clWhite;
  FColorMirror := clWhite;
  FColorMirrorTo := clWhite;
  FColorMirrorHot := clNone;
  FColorMirrorHotTo := clNone;
  FGradientMirror := ggVertical;
  FGradientMirrorHot := ggVertical;
  FGradient := ggVertical;
  FGradientHot := ggVertical;
  FColorMirrorDisabledTo := clNone;
  FColorMirrorDisabled := clNone;
  FColorMirrorSelectedTo := clWhite;
  FColorMirrorSelected := clWhite;
  FGradientSelected := ggVertical;
  FGradientDisabled := ggVertical;
  FGradientMirrorSelected := ggVertical;
  FGradientMirrorDisabled := ggVertical;
  FTextColorDisabled := clWhite;
  FTextColorSelected := clBlue;
  FTextColor := clBlue;
  FTextColorHot := clBlue;
  FBackGround := TGradientBackground.Create;
  FBackGround.OnChange := OnBackGroundChanged;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 8;
  FFont.Style := [];
  FFont.OnChange := OnFontChanged;
  FShadowColor := RGB(174, 199, 232);
  FHighLightColor := RGB(191, 250, 255);
  FHighLightColorSelected := RGB(248, 204, 99);
  FHighLightColorSelectedHot := RGB(255, 255, 189);
  FHighLightColorDown := RGB(208, 251, 255);
  FHighLightColorHot := RGB(237, 244, 253);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.Assign(Source: TPersistent);
begin
  if (Source is TCustomTabAppearance) then
  begin
    FBorderColor := (Source as TCustomTabAppearance).BorderColor;
    FBorderColorHot := (Source as TCustomTabAppearance).BorderColorHot;
    FBorderColorSelectedHot := (Source as TCustomTabAppearance).BorderColorSelectedHot;
    FBorderColorDown := (Source as TCustomTabAppearance).BorderColorDown;
    FColor := (Source as TCustomTabAppearance).Color;
    FColorTo := (Source as TCustomTabAppearance).ColorTo;
    FColorHot := (Source as TCustomTabAppearance).ColorHot;
    FColorHotTo := (Source as TCustomTabAppearance).ColorHotTo;
    FColorSelectedTo := (Source as TCustomTabAppearance).ColorSelectedTo;
    FBorderColorDisabled := (Source as TCustomTabAppearance).BorderColorDisabled;
    FBorderColorSelected := (Source as TCustomTabAppearance).BorderColorSelected;
    FColorDisabled := (Source as TCustomTabAppearance).ColorDisabled;
    FColorDisabledTo := (Source as TCustomTabAppearance).ColorDisabledTo;
    FColorSelected := (Source as TCustomTabAppearance).ColorSelected;
    FColorMirror := (Source as TCustomTabAppearance).ColorMirror;
    FColorMirrorTo := (Source as TCustomTabAppearance).ColorMirrorTo;
    FColorMirrorHot := (Source as TCustomTabAppearance).ColorMirrorHot;
    FColorMirrorHotTo := (Source as TCustomTabAppearance).ColorMirrorHotTo;
    FGradientMirror := (Source as TCustomTabAppearance).GradientMirror;
    FGradientMirrorHot := (Source as TCustomTabAppearance).GradientMirrorHot;
    FGradient := (Source as TCustomTabAppearance).Gradient;
    FGradientHot := (Source as TCustomTabAppearance).GradientHot;
    FColorMirrorDisabledTo := (Source as TCustomTabAppearance).ColorMirrorDisabledTo;
    FColorMirrorDisabled := (Source as TCustomTabAppearance).ColorMirrorDisabled;
    FColorMirrorSelectedTo := (Source as TCustomTabAppearance).ColorMirrorSelectedTo;
    FColorMirrorSelected := (Source as TCustomTabAppearance).ColorMirrorSelected;
    FGradientSelected := (Source as TCustomTabAppearance).GradientSelected;
    FGradientDisabled := (Source as TCustomTabAppearance).GradientDisabled;
    FGradientMirrorSelected := (Source as TCustomTabAppearance).GradientMirrorSelected;
    FGradientMirrorDisabled := (Source as TCustomTabAppearance).GradientMirrorDisabled;
    FTextColorDisabled := (Source as TCustomTabAppearance).TextColorDisabled;
    FTextColorSelected := (Source as TCustomTabAppearance).TextColorSelected;
    Font.Assign((Source as TCustomTabAppearance).Font);
    TextColor := (Source as TCustomTabAppearance).TextColor;
    TextColorHot := (Source as TCustomTabAppearance).TextColorHot;
    FShadowColor := (Source as TCustomTabAppearance).ShadowColor;
    FHighLightColor := (Source as TCustomTabAppearance).HighLightColor;
    FHighLightColorHot := (Source as TCustomTabAppearance).HighLightColorHot;
    FHighLightColorDown := (Source as TCustomTabAppearance).HighLightColorDown;
    FHighLightColorSelected := (Source as TCustomTabAppearance).HighLightColorSelected;
    FHighLightColorSelectedHot := (Source as TCustomTabAppearance).HighLightColorSelectedHot;
    BackGround.Assign((Source as TCustomTabAppearance).BackGround);
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.ClearValues;
begin
  FBorderColor := clNone;
  FBorderColorHot := clNone;
  FColor := clNone;
  FColorTo := clNone;
  FColorHot := clNone;
  FColorHotTo := clNone;
  FColorSelectedTo := clNone;
  FBorderColorDisabled := clNone;
  FBorderColorSelected := clNone;
  FColorDisabled := clNone;
  FColorDisabledTo := clNone;
  FColorSelected := clNone;
  FColorMirror := clNone;
  FColorMirrorTo := clNone;
  FColorMirrorHot := clNone;
  FColorMirrorHotTo := clNone;
  FColorMirrorDisabledTo := clNone;
  FColorMirrorDisabled := clNone;
  FColorMirrorSelectedTo := clNone;
  FColorMirrorSelected := clNone;
  FTextColorDisabled := clNone;
  FTextColorSelected := clNone;
  FTextColor := clNone;
  FTextColorHot := clNone;
  FBorderColorSelectedHot := clNone;
  FBorderColorDown := clNone;
  FHighLightColorHot := clNone;
  FShadowColor := clNone;
  FHighLightColorDown := clNone;
  FHighLightColorSelected := clNone;
  FHighLightColorSelectedHot := clNone;
  FHighLightColor := clNone;
end;

//------------------------------------------------------------------------------

destructor TCustomTabAppearance.Destroy;
begin
  FBackGround.Free;
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBackGround(const Value: TGradientBackground);
begin
  FBackGround.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorDisabled(const Value: TColor);
begin
  if (FBorderColorDisabled <> Value) then
  begin
    FBorderColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorSelected(const Value: TColor);
begin
  if (FBorderColorSelected <> Value) then
  begin
    FBorderColorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorSelectedHot(
  const Value: TColor);
begin
  if (FBorderColorSelectedHot <> Value) then
  begin
    FBorderColorSelectedHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorDisabled(const Value: TColor);
begin
  if (FColorDisabled <> Value) then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorDisabledTo(const Value: TColor);
begin
  if (FColorDisabledTo <> Value) then
  begin
    FColorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorHot(const Value: TColor);
begin
  if (FColorHot <> Value) then
  begin
    FColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorHotTo(const Value: TColor);
begin
  if (FColorHotTo <> Value) then
  begin
    FColorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorDisabled(const Value: TColor);
begin
  if (FColorMirrorDisabled <> Value) then
  begin
    FColorMirrorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorDisabledTo(
  const Value: TColor);
begin
  if (FColorMirrorDisabledTo <> Value) then
  begin
    FColorMirrorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorHot(const Value: TColor);
begin
  if (FColorMirrorHot <> Value) then
  begin
    FColorMirrorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorHotTo(const Value: TColor);
begin
  if (FColorMirrorHotTo <> Value) then
  begin
    FColorMirrorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorSelected(const Value: TColor);
begin
  if (FColorMirrorSelected <> Value) then
  begin
    FColorMirrorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorSelectedTo(
  const Value: TColor);
begin
  if (FColorMirrorSelectedTo <> Value) then
  begin
    FColorMirrorSelectedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorSelected(const Value: TColor);
begin
  if (FColorSelected <> Value) then
  begin
    FColorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorSelectedTo(const Value: TColor);
begin
  if (FColorSelectedTo <> Value) then
  begin
    FColorSelectedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradient(const Value: TGDIPGradient);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientDisabled(
  const Value: TGDIPGradient);
begin
  if (FGradientDisabled <> Value) then
  begin
    FGradientDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientHot(const Value: TGDIPGradient);
begin
  if (FGradientHot <> Value) then
  begin
    FGradientHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirror(
  const Value: TGDIPGradient);
begin
  if (FGradientMirror <> Value) then
  begin
    FGradientMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirrorDisabled(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorDisabled <> Value) then
  begin
    FGradientMirrorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirrorHot(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorHot <> Value) then
  begin
    FGradientMirrorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientMirrorSelected(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorSelected <> Value) then
  begin
    FGradientMirrorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetGradientSelected(
  const Value: TGDIPGradient);
begin
  if (FGradientSelected <> Value) then
  begin
    FGradientSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColorDisabled(const Value: TColor);
begin
  if (FTextColorDisabled <> Value) then
  begin
    FTextColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColorHot(const Value: TColor);
begin
  if (FTextColorHot <> Value) then
  begin
    FTextColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetTextColorSelected(const Value: TColor);
begin
  if (FTextColorSelected <> Value) then
  begin
    FTextColorSelected := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.OnBackGroundChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetBorderColorDown(const Value: TColor);
begin
  if (FBorderColorDown <> Value) then
  begin
    FBorderColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomTabAppearance.OnFontChanged(Sender: TObject);
begin
  if Assigned(FOnFontChange) then
    FOnFontChange(Self);
end;

//------------------------------------------------------------------------------

{ TDbgList }

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
    Exit;
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;

//------------------------------------------------------------------------------

{ TCustomAdvOfficePagerStyler }

procedure TCustomAdvOfficePagerStyler.AddControl(AControl: TCustomControl);
begin
  FControlList.Add(AControl);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.Assign(Source: TPersistent);
begin
  if Source is TCustomAdvOfficePagerStyler then
  begin
    TabAppearance.Assign((Source as TCustomAdvOfficePagerStyler).TabAppearance);
    PageAppearance.Assign((Source as TCustomAdvOfficePagerStyler).PageAppearance);
    RoundEdges := (Source as TCustomAdvOfficePagerStyler).RoundEdges;
    BlendFactor := (Source as TCustomAdvOfficePagerStyler).BlendFactor;
    ShowShadow := (Source as TCustomAdvOfficePagerStyler).ShowShadow;
    Show3D := (Source as TCustomAdvOfficePagerStyler).Show3D;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TCustomAdvOfficePagerStyler.Create(AOwner: TComponent);
begin
  inherited;
  FControlList := TDbgList.Create;
  FRoundEdges := True;
  FBlendFactor := 50;
  FShowShadow := True;
  FShow3D := True;
  FTabRounding := 1;

  FTabAppearance := TTabAppearance.Create;
  FTabAppearance.OnChange := OnTabAppearanceChanged;
  FTabAppearance.OnFontChange := OnTabAppearanceFontChanged;
  FPageAppearance := TVistaBackground.Create;
  FPageAppearance.OnChange := OnPageAppearanceChanged;
  FGlowButtonAppearance := TGlowButtonAppearance.Create;
  with FGlowButtonAppearance do
  begin
    Color := $EEDBC8;
    ColorTo := $F6DDC9;
    ColorMirror := $EDD4C0;
    ColorMirrorTo := $F7E1D0;
    BorderColor := $E0B99B;
    Gradient := ggVertical;
    GradientMirror := ggVertical;

    ColorHot := $EBFDFF;
    ColorHotTo := $ACECFF;
    ColorMirrorHot := $59DAFF;
    ColorMirrorHotTo := $A4E9FF;
    BorderColorHot := $99CEDB;
    GradientHot := ggVertical;
    GradientMirrorHot := ggVertical;

    ColorDown := $76AFF1;
    ColorDownTo := $4190F3;
    ColorMirrorDown := $0E72F1;
    ColorMirrorDownTo := $4C9FFD;
    BorderColorDown := $45667B;
    GradientDown := ggVertical;
    GradientMirrorDown := ggVertical;

    ColorChecked := $B5DBFB;
    ColorCheckedTo := $78C7FE;
    ColorMirrorChecked := $9FEBFD;
    ColorMirrorCheckedTo := $56B4FE;
    BorderColorChecked := $45667B;
    GradientChecked := ggVertical;
    GradientMirrorChecked := ggVertical;
  end;

  FGlowButtonAppearance.onChange := OnGlowButtonAppearanceChanged;
end;

//------------------------------------------------------------------------------

destructor TCustomAdvOfficePagerStyler.Destroy;
var
  i: Integer;
begin
  if (csDesigning in ComponentState) then
  begin

    for i := FControlList.Count - 1 downto 0 do
    begin
      if (TCustomControl(FControlList[i]) is TAdvOfficePager) then
      begin                            
        if (TAdvOfficePager(FControlList[i]).AdvOfficePagerStyler = Self) and not (csDestroying in TAdvOfficePager(FControlList[i]).ComponentState) then
          TAdvOfficePager(FControlList[i]).AdvOfficePagerStyler := nil;
      end;
    end;
  end;
  FControlList.Free;
  FTabAppearance.Free;
  FPageAppearance.Free;
  FGlowButtonAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.InitColorTones;
begin
 //
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.Change(PropID: integer);
var
  i: integer;
begin
  if (csDestroying in ComponentState) then
    Exit;

  for i := 0 to FControlList.Count - 1 do
  begin
    if (TCustomControl(FControlList[i]) is TAdvOfficePager) then
      TAdvOfficePager(FControlList[i]).UpdateMe(PropID);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetColorTones(ATones: TColorTones);
var
  i: integer;
begin
  if (csDestroying in ComponentState) then
    Exit;

  for i := 0 to FControlList.Count - 1 do
  begin
    if (TCustomControl(FControlList[i]) is TAdvOfficePager) then
      TAdvOfficePager(FControlList[i]).SetColorTones(ATones);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: integer;
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    i := FControlList.IndexOf(AComponent);
    if i >= 0 then
      FControlList.Remove(AComponent);
  end;

end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.RemoveControl(AControl: TCustomControl);
var
  i: integer;
begin
  i := FControlList.IndexOf(AControl);
  if i >= 0 then
    FControlList.Delete(i);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.OnTabAppearanceChanged(Sender: TObject);
begin
  Change(1);
end;

procedure TCustomAdvOfficePagerStyler.OnTabAppearanceFontChanged(
  Sender: TObject);
begin
  Change(5);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.OnPageAppearanceChanged(Sender: TObject);
begin
  Change(2);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.OnGlowButtonAppearanceChanged(Sender: TObject);
begin
  Change(4);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetRoundEdges(const Value: boolean);
begin
  FRoundEdges := Value;
  Change(3);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetShow3D(const Value: Boolean);
begin
  if (FShow3D <> Value) then
  begin
    FShow3D := Value;
    Change(1);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetShowShadow(const Value: Boolean);
begin
  if (FShowShadow <> Value) then
  begin
    FShowShadow := Value;
    Change(1);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetTabAppearance(
  const Value: TTabAppearance);
begin
  FTabAppearance.Assign(Value);
end;

procedure TCustomAdvOfficePagerStyler.SetTabRounding(const Value: TTabRounding);
begin
  if (FTabRounding <> Value) then
  begin
    FTabRounding := Value;
    Change(2);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetPageAppearance(
  const Value: TVistaBackground);
begin
  FPageAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficePagerStyler.SetAutoThemeAdapt(const Value: boolean);
begin
  FAutoThemeAdapt := Value;
end;

procedure TCustomAdvOfficePagerStyler.SetGlowButtonAppearance(
  const Value: TGlowButtonAppearance);
begin
  FGlowButtonAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

{ TTabProgress }

procedure TTabProgress.Assign(Source: TPersistent);
begin
  if (Source is TTabProgress) then
  begin
    FColor := TTabProgress(Source).Color;
    FColorTo := TTabProgress(Source).ColorTo;
    FCompleteColor := TTabProgress(Source).CompleteColor;
    FCompleteColorTo := TTabProgress(Source).CompleteColorTo;
    FMax := TTabProgress(Source).Max;
    FMin := TTabProgress(Source).Min;
    FVisible := TTabProgress(Source).Visible;
    FPosition := TTabProgress(Source).Position;
    Changed;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TTabProgress.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TTabProgress.Create;
begin
  inherited;
  FColor := clWhite;
  FColorTo := clSilver;
  FCompleteColor := clWhite;
  FCompleteColorTo := clRed;
  FMax := 100;
  FMin := 0;
  FVisible := false;
  FPosition := 50;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.Draw(g: TGPGraphics; R: TRect; RotateTabLeftRight: Boolean;
  Direction: TTabPosition);
var
  w, cs: Integer;
  GPBrsh: TGPBrush;
  LGM: LinearGradientMode;
  gpRF: TGPRectF;
  Clr, ClrTo, cmClr, cmClrTo: TColor;
begin
  if not Assigned(g) then
    Exit;

  case Direction of
    tpTop:
    begin
      R.Top := R.Top + 1;
      R.Right := R.Right + 2;
    end;
    tpBottom: R.Right := R.Right + 1;
    tpLeft: R.Bottom := R.Bottom - 1;
    tpRight: R.Right := R.Right + 2;
  end;

  Clr := Color;
  ClrTo := ColorTo;
  cmClr := CompleteColor;
  cmClrTo := CompleteColorTo;
  if (Direction in [tpTop, tpBottom]) or not RotateTabLeftRight then
  begin
    w := R.Right - R.Left;
    LGM := LinearGradientModeVertical;
  end
  else if Direction = tpLeft then
  begin
    w := R.Bottom - R.Top;
    LGM := LinearGradientModeHorizontal;
  end
  else
  begin
    w := R.Bottom - R.Top;
    LGM := LinearGradientModeHorizontal;
    Clr := ColorTo;
    ClrTo := Color;
    cmClr := CompleteColorTo;
    cmClrTo := CompleteColor;
  end;

  if (Max > 0) then
    cs := Round((Position / Max) * w)
  else
    cs := 0;

  //--- Color, Color
  gpRF := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  if (ColorTo <> clNone) then
    GPBrsh := TGPLinearGradientBrush.Create(gpRF, ColorToARGB(Clr),ColorToARGB(ClrTo), LGM)
  else
    GPBrsh := TGPSolidBrush.Create(ColorToARGB(Color));

  g.FillRectangle(GPBrsh, gpRF);
  GPBrsh.Free;

  //--- CompleteColor, CompleteColorTo
  if (Direction in [tpTop, tpBottom]) or (not RotateTabLeftRight) then
    gpRF := MakeRect(R.Left, R.Top, cs, R.Bottom - R.Top)
  else if (Direction = tpLeft) then
    gpRF := MakeRect(R.Left, R.Bottom - cs, R.Right - R.Left, cs)
  else
    gpRF := MakeRect(R.Left, R.Top, R.Right - R.Left, cs);

  if (CompleteColorTo <> clNone) then
    GPBrsh := TGPLinearGradientBrush.Create(gpRF, ColorToARGB(cmClr),ColorToARGB(cmClrTo), LGM)
  else
    GPBrsh := TGPSolidBrush.Create(ColorToARGB(cmClr));

  g.FillRectangle(GPBrsh, gpRF);
  GPBrsh.Free;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetCompleteColor(const Value: TColor);
begin
  if FCompleteColor <> Value then
  begin
    FCompleteColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetCompleteColorTo(const Value: TColor);
begin
  if FCompleteColorTo <> Value then
  begin
    FCompleteColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetMax(const Value: Integer);
begin
  if (FMax <> Value) then
  begin
    FMax := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetMin(const Value: Integer);
begin
  if (FMin <> Value) then
  begin
    FMin := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetPosition(const Value: Integer);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TTabProgress.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TArrowWindow }

procedure TArrowWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP; // or WS_BORDER;
  end;
end;

//------------------------------------------------------------------------------

procedure TArrowWindow.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TArrowWindow.CreateWnd;
var
  hrgn: THandle;

begin
  inherited;

  case Dir of
  arrDown:begin
         arrow[0] := point(3,0);
         arrow[1] := point(7,0);
         arrow[2] := point(7,4);
         arrow[3] := point(9,4);
         arrow[4] := point(5,8);
         arrow[5] := point(1,4);
         arrow[6] := point(3,4);
         end;
  arrUp:begin
         arrow[0] := point(5,0);
         arrow[1] := point(10,5);
         arrow[2] := point(7,5);
         arrow[3] := point(7,9);
         arrow[4] := point(3,9);
         arrow[5] := point(3,5);
         arrow[6] := point(0,5);
       end;
  arrLeft:begin
         arrow[0] := point(0,3);
         arrow[1] := point(0,7);
         arrow[2] := point(4,7);
         arrow[3] := point(4,10);
         arrow[4] := point(8,5);
         arrow[5] := point(4,0);
         arrow[6] := point(4,3);
         end;
  arrRight:begin
         arrow[0] := point(0,5);
         arrow[1] := point(4,10);
         arrow[2] := point(4,7);
         arrow[3] := point(8,7);
         arrow[4] := point(8,3);
         arrow[5] := point(4,3);
         arrow[6] := point(4,0);
         end;
  end;
  hrgn := CreatePolygonRgn(arrow,7{7},WINDING);
  SetWindowRgn(Handle,hrgn,True);
end;

//------------------------------------------------------------------------------

procedure TArrowWindow.Paint;
begin
//  inherited;  // remove, is not working in Windows XP
  Canvas.Brush.Color := Color;
  Canvas.Pen.Color := Color;
  Canvas.Rectangle(ClientRect.Left,ClientRect.Top,ClientRect.Right,ClientRect.Bottom);

  {
  Canvas.Pen.Color := FBrColor;
  Canvas.Brush.Style := bsClear;
  Canvas.Polygon(Arrow);
  Canvas.MoveTo(Arrow[4].X, Arrow[4].Y-1);
  Canvas.LineTo(Arrow[3].X, Arrow[3].Y);
  }
end;

//------------------------------------------------------------------------------

constructor TArrowWindow.Init(AOwner: TComponent; Direction:TArrowDirection);
begin
  Dir := Direction;
  inherited Create(aOwner);
  Color := clBlue; //clSilver;
  FBrColor := clGray;
  Parent := TWinControl(AOwner);
  Visible := False;
  ControlStyle := [csOpaque];
  if Direction in [arrUp, arrDown] then
  begin
    Height := 9;
    Width := 9; //9
  end
  else
  begin
    Height := 9;
    Width := 9;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvOfficePage }

constructor TAdvOfficePage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

//  if (csDesigning in ComponentState) then
  ControlStyle := ControlStyle + [csAcceptsControls] {- [csOpaque]};

  FProgress := TTabProgress.Create;
  FProgress.OnChange := OnProgressChanged;

  FCaption := '';
  FBadge := '';
  FBadgeColor := clRed;
  FBadgeTextColor := clWhite;
  FWideCaption := '';
  FTabVisible := True;
  FTabEnabled := True;
  FImageIndex := -1;
  FTimer := nil;
  FTabHint := '';
  FCloseButton := nil;
  FCloseButtonChrome := nil;
  FOfficeHint := TAdvHintInfo.Create;

  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := PictureChanged;

  FIDisabledPicture := TGDIPPicture.Create;
  FIDisabledPicture.OnChange := PictureChanged;

  FBkgCache := TBitmap.Create;

  FShortCutHint := nil;
  FShortCutHintPos := shpTop;

  FShowClose := True;

  FChecked := false;
  FShowCheckBox := false;

  FGlow := False;
  FGlowColor := clRed;

  FTabAppearance := TTabAppearance.Create;
  FTabAppearance.OnChange := OnTabAppearanceChanged;
  FUseTabAppearance := false;
  FTabAppearance.OnFontChange := OnTabAppearanceFontChanged;

  FPageAppearance := TVistaBackground.Create;
  FPageAppearance.OnChange := OnPageAppearanceChanged;
  FUsePageAppearance := false;
  if (AOwner is TAdvOfficePager) then
    FTabIndex := (AOwner as TAdvOfficePager).AdvPageCount;
  DoubleBuffered := true;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficePage.Destroy;
var
  i: Integer;
begin
  if (FAdvOfficePager <> nil) then
  begin
    FAdvOfficePager.RemoveAdvPage(Self);
  end;

  if (FParentPager <> nil) then
  begin
    i := FParentPager.FFloatingPages.IndexOf(Self);
    if (i >= 0) then
      FParentPager.FFloatingPages.Delete(i);
  end;

  if Assigned(FTimer) then
  begin
    FTimer.Enabled := false;
    FreeAndNil(FTimer);
  end;

  FOfficeHint.Free;
  FIPicture.Free;
  FIDisabledPicture.Free;
  FTabAppearance.Free;
  FPageAppearance.Free;
  FProgress.Free;
  FBkgCache.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if Showing then
  begin
    if Assigned(FOnShow) then
      FOnShow(self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.Loaded;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.Paint;
var
  R, TabR: TRect;
  LnClr: TColor;
  i: Integer;
  Layout: TButtonLayout;
  aPageAppearance: TVistaBackground;
  IsOnGlass: Boolean;
begin
  if not Assigned(FAdvOfficePager) or not Assigned(FAdvOfficePager.FCurrentOfficePagerStyler) then
    Exit;

  if UsePageAppearance then
    aPageAppearance := PageAppearance
  else if Assigned(FPageAppearance) and FAdvOfficePager.FItones then
    aPageAppearance := FAdvOfficePager.FPageAppearance
  else
    aPageAppearance := FAdvOfficePager.FCurrentOfficePagerStyler.PageAppearance;

  if (Self.Color <> aPageAppearance.Color) then
    Self.Color := aPageAppearance.Color;

  R := ClientRect;
  IsOnGlass := Assigned(AdvOfficePager) and (AdvOfficePager.IsOnGlass);

  case FAdvOfficePager.TabSettings.ImagePosition of
    ipTop: Layout := blGlyphTop;
    ipBottom: Layout := blGlyphBottom;
    ipLeft: Layout := blGlyphLeft;
    ipRight: Layout := blGlyphRight;
    else Layout := blGlyphLeft;
  end;

  with aPageAppearance do
  begin
    LnClr := Color;

    if FValidCache and FAdvOfficePager.BufferedPages then
      Canvas.Draw(R.Left, R.Top, FBkgCache)
    else
    begin
      if FAdvOfficePager.BufferedPages then
      begin
        FBkgCache.Height := R.Bottom - R.Top;
        FBkgCache.Width := R.Right - R.Left;
        DrawVistaGradient(FBkgCache.Canvas, Rect(0, 0, FBkgCache.Width, FBkgCache.Height), Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
          Gradient, GradientMirror, '', Font, Layout, Enabled, False, FAdvOfficePager.AntiAlias, FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges and not FAdvOfficePager.FItones, True, nil, IsOnGlass, False, FAdvOfficePager.TabPosition);
        Canvas.Draw(R.Left, R.Top, FBkgCache);
        FValidCache := true;
      end
      else
        DrawVistaGradient(Canvas, Rect(0, 0, Width, Height), Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
          Gradient, GradientMirror, '', Font, Layout, Enabled, False, FAdvOfficePager.AntiAlias, FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges and not FAdvOfficePager.FItones, True, nil, IsOnGlass, False, FAdvOfficePager.TabPosition);

      {
      bmp := TBitMap.Create;
      bmp.Height := R.Bottom - R.Top;
      bmp.Width := R.Right - R.Left;
      DrawVistaGradient(bmp.Canvas, Rect(0, 0, bmp.Width, bmp.Height), Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
        Gradient, GradientMirror, '', Font, Layout, Enabled, False, FAdvOfficePager.AntiAlias, FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges, True, FAdvOfficePager.TabPosition);
      //DrawVistaGradient(Canvas, R, Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
        //Gradient, GradientMirror, '', Font, Enabled, False, FAdvOfficePager.AntiAlias, FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges, FAdvOfficePager.TabPosition);
      Canvas.Draw(R.Left, R.Top, bmp);
      bmp.Free;
      }
    end;

    //DrawVistaGradient(Canvas, R, Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
    //  Gradient, GradientMirror, '', Font, Enabled, False, FAdvOfficePager.AntiAlias, FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges, FAdvOfficePager.TabPosition);

    i := 3; // 10
    if not FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges or FAdvOfficePager.FItones then
      i := 2;

    if (BorderColor <> clNone) and FAdvOfficePager.CanShow3D then
    begin
      case (FAdvOfficePager.TabPosition) of
        tpTop:
        begin
          // Draw 3D effect
          Canvas.Pen.Color := BlendColor(clWhite, BorderColor, FAdvOfficePager.FCurrentOfficePagerStyler.BlendFactor);
          if not IsOnGlass then
          begin
            Canvas.MoveTo(R.Left+1, R.Top + i);
            Canvas.LineTo(R.Left+1, R.Bottom-2);
            //Canvas.Pixels[R.Left+2, R.Bottom-3] := Canvas.Pen.Color;
            Canvas.MoveTo(R.Right-2, R.Top + i);
            Canvas.LineTo(R.Right-2, R.Bottom-2);
            //Canvas.Pixels[R.Right-3, R.Bottom-3] := Canvas.Pen.Color;
          end;

          if not (IsOnGlass and (AdvOfficePager.Align = alClient)) then
          begin
            Canvas.MoveTo(R.Left+3, R.Bottom -2);
            Canvas.LineTo(R.Right-2, R.Bottom-2);
          end;
        end;
        tpBottom:
        begin
          // Draw 3D effect
          Canvas.Pen.Color := BlendColor(clWhite, BorderColor, FAdvOfficePager.FCurrentOfficePagerStyler.BlendFactor);
          if not IsOnGlass then
          begin
            Canvas.MoveTo(R.Left+1, R.Top + 2);
            Canvas.LineTo(R.Left+1, R.Bottom-i);
            Canvas.MoveTo(R.Right-2, R.Top + 2);
            Canvas.LineTo(R.Right-2, R.Bottom-i);
          end;

          Canvas.MoveTo(R.Left+3, R.Top +1);
          Canvas.LineTo(R.Right-2, R.Top +1);
        end;
        tpLeft:
        begin
          // Draw 3D effect
          Canvas.Pen.Color := BlendColor(clWhite, BorderColor, FAdvOfficePager.FCurrentOfficePagerStyler.BlendFactor);
          Canvas.MoveTo(R.Left+i, R.Top + 1);
          Canvas.LineTo(R.Right-2, R.Top + 1);
          Canvas.MoveTo(R.Left+i, R.Bottom - 2);
          Canvas.LineTo(R.Right-2, R.Bottom - 2);

          if not IsOnGlass then
          begin
            Canvas.MoveTo(R.Right-2, R.Top +3);
            Canvas.LineTo(R.Right-2, R.Bottom-2);
          end;
        end;
        tpRight:
        begin
          // Draw 3D effect
          Canvas.Pen.Color := BlendColor(clWhite, BorderColor, FAdvOfficePager.FCurrentOfficePagerStyler.BlendFactor);
          Canvas.MoveTo(R.Left+2, R.Top + 1);
          Canvas.LineTo(R.Right-i, R.Top + 1);
          Canvas.MoveTo(R.Left+2, R.Bottom - 2);
          Canvas.LineTo(R.Right-i, R.Bottom - 2);

          if not IsOnGlass then
          begin
            Canvas.MoveTo(R.Left+1, R.Top +3);
            Canvas.LineTo(R.Left+1, R.Bottom-2);
          end;
        end;
      end;
    end;

  end;


  if (FAdvOfficePager.ActivePage = self) then
  begin
    TabR := FAdvOfficePager.GetTabRect(Self);

    if (FAdvOfficePager.TabSettings.Height > 0) then
    begin
    // Attaching to Tab
      case (FAdvOfficePager.TabPosition) of
        tpTop:
        begin
          TabR.Left := TabR.Left - FAdvOfficePager.FPageMargin;
          TabR.Right := Min(TabR.Right - FAdvOfficePager.FPageMargin, FAdvOfficePager.GetButtonsRect.Left);
          if not FAdvOfficePager.UseOldDrawing then
          begin
            case FAdvOfficePager.TabSettings.Shape of
              tsRectangle: TabR.Left := TabR.Left + 1;
              tsLeftRamp: TabR.Left := TabR.Left + 2 + FAdvOfficePager.TabSettings.Rounding div 2;
              tsRightRamp:
              begin
                TabR.Left := TabR.Left + 1;
                TabR.Right := TabR.Right - 1 - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
              tsLeftRightRamp:
              begin
                TabR.Left := TabR.Left + 2 + FAdvOfficePager.TabSettings.Rounding div 2;
                TabR.Right := TabR.Right - 1 - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
            end;
          end;

          Canvas.Pen.Color := LnClr;
          if not FAdvOfficePager.TabRoundEdges and not FAdvOfficePager.FItones and (FAdvOfficePager.TabSettings.Shape = tsRectangle) then
          begin
            Canvas.MoveTo(TabR.Left + 1, 0);
            Canvas.LineTo(TabR.Right - 1, 0);
          end
          else
          begin
            Canvas.MoveTo(TabR.Left, 0);
            Canvas.LineTo(TabR.Right, 0);
          end;
        end;
        tpBottom:
        begin
          TabR.Left := TabR.Left - FAdvOfficePager.FPageMargin;
          TabR.Right := Min(TabR.Right - FAdvOfficePager.FPageMargin, FAdvOfficePager.GetButtonsRect.Left);
          if not FAdvOfficePager.UseOldDrawing then
          begin
            case FAdvOfficePager.TabSettings.Shape of
              tsRectangle: TabR.Left := TabR.Left + 1;
              tsLeftRamp: TabR.Left := TabR.Left + 2 + FAdvOfficePager.TabSettings.Rounding div 2;
              tsRightRamp:
              begin
                TabR.Left := TabR.Left + 1;
                TabR.Right := TabR.Right - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
              tsLeftRightRamp:
              begin
                TabR.Left := TabR.Left + 1 + FAdvOfficePager.TabSettings.Rounding div 2;
                TabR.Right := TabR.Right - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
            end;
          end;
          Canvas.Pen.Color := LnClr;
          if not FAdvOfficePager.TabRoundEdges and not FAdvOfficePager.FITones and (FAdvOfficePager.TabSettings.Shape = tsRectangle) then
          begin
            Canvas.MoveTo(TabR.Left + 1, Height-1);
            Canvas.LineTo(TabR.Right - 1, Height-1);
          end
          else
          begin
            Canvas.MoveTo(TabR.Left, Height-1);
            Canvas.LineTo(TabR.Right, Height-1);
          end;
        end;
        tpLeft:
        begin
          TabR.Top := TabR.Top - FAdvOfficePager.FPageMargin;
          TabR.Bottom := Min(TabR.Bottom - FAdvOfficePager.FPageMargin, FAdvOfficePager.GetButtonsRect.Top);
          if not FAdvOfficePager.UseOldDrawing then
          begin
            case FAdvOfficePager.TabSettings.Shape of
              tsRectangle: TabR.Top := TabR.Top + 1;
              tsLeftRamp:
              begin
                TabR.Top := TabR.Top + 1;
                TabR.Bottom := TabR.Bottom - 1 - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
              tsRightRamp: TabR.Top := TabR.Top + 2 + FAdvOfficePager.TabSettings.Rounding div 2;
              tsLeftRightRamp:
              begin
                TabR.Top := TabR.Top + 2 + FAdvOfficePager.TabSettings.Rounding div 2;
                TabR.Bottom := TabR.Bottom - 1 - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
            end;
          end;
          Canvas.Pen.Color := LnClr;
          if not FAdvOfficePager.TabRoundEdges and not FAdvOfficePager.FItones and (FAdvOfficePager.TabSettings.Shape = tsRectangle) then
          begin
            Canvas.MoveTo(0, TabR.Top);
            Canvas.LineTo(0, TabR.Bottom-1);
          end
          else
          begin
            Canvas.MoveTo(0, TabR.Top-1);
            Canvas.LineTo(0, TabR.Bottom-1);
          end;
        end;
        tpRight:
        begin
          TabR.Top := TabR.Top - FAdvOfficePager.FPageMargin;
          TabR.Bottom := Min(TabR.Bottom - FAdvOfficePager.FPageMargin, FAdvOfficePager.GetButtonsRect.Top);
          if not FAdvOfficePager.UseOldDrawing then
          begin
            case FAdvOfficePager.TabSettings.Shape of
              tsRectangle: TabR.Top := TabR.Top + 1;
              tsLeftRamp: TabR.Top := TabR.Top + 1 + FAdvOfficePager.TabSettings.Rounding div 2;
              tsRightRamp:
              begin
                TabR.Top := TabR.Top + 1;
                TabR.Bottom := TabR.Bottom - 1 - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
              tsLeftRightRamp:
              begin
                TabR.Top := TabR.Top + 2 + FAdvOfficePager.TabSettings.Rounding div 2;
                TabR.Bottom := TabR.Bottom - 1 - FAdvOfficePager.TabSettings.Rounding div 2;
              end;
            end;
          end;
          Canvas.Pen.Color := LnClr;
          if not FAdvOfficePager.TabRoundEdges and not FAdvOfficePager.FItones and (FAdvOfficePager.TabSettings.Shape = tsRectangle) then
          begin
            Canvas.MoveTo(Width-1, TabR.Top);
            Canvas.LineTo(Width-1, TabR.Bottom-1);
          end
          else
          begin
            Canvas.MoveTo(Width-1, TabR.Top-1);
            Canvas.LineTo(Width-1, TabR.Bottom-1);
          end;
        end;
      end;
    end;

    if not FAdvOfficePager.IsGlass and (FAdvOfficePager.FCurrentOfficePagerStyler.RoundEdges and not FAdvOfficePager.FItones) and (FAdvOfficePager.PageMargin > 0) then
    begin
      // Clean up edges
      Canvas.Pixels[R.Left, R.Top] := FAdvOfficePager.Canvas.Pixels[self.Left - 1, self.Top - 1];
      Canvas.Pixels[R.Left + 1, R.Top] := FAdvOfficePager.Canvas.Pixels[self.Left + 1, self.Top - 1];
      Canvas.Pixels[R.Left, R.Top + 1] := FAdvOfficePager.Canvas.Pixels[self.Left - 1, self.Top];

      Canvas.Pixels[R.Left, R.Bottom - 1] := FAdvOfficePager.Canvas.Pixels[self.Left - 1, self.Top + Height];
      Canvas.Pixels[R.Left + 1, R.Bottom - 1] := FAdvOfficePager.Canvas.Pixels[self.Left - 1, self.Top + Height];
      Canvas.Pixels[R.Left, R.Bottom - 2] := FAdvOfficePager.Canvas.Pixels[self.Left - 1, self.Top + Height];

      Canvas.Pixels[R.Right - 1, R.Top] := FAdvOfficePager.Canvas.Pixels[self.Left + Width, self.Top];
      Canvas.Pixels[R.Right - 2, R.Top] := FAdvOfficePager.Canvas.Pixels[self.Left + Width, self.Top];
      Canvas.Pixels[R.Right - 1, R.Top + 1] := FAdvOfficePager.Canvas.Pixels[self.Left + Width, self.Top];

      Canvas.Pixels[R.Right - 1, R.Bottom - 1] := FAdvOfficePager.Canvas.Pixels[self.Left + Width, self.Top + Height];
      Canvas.Pixels[R.Right - 2, R.Bottom - 1] := FAdvOfficePager.Canvas.Pixels[self.Left + Width, self.Top + Height];
      Canvas.Pixels[R.Right - 1, R.Bottom - 2] := FAdvOfficePager.Canvas.Pixels[self.Left + Width, self.Top + Height];
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SelectFirstControl;
begin
  SelectFirst;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetAdvOfficePager(const Value: TAdvOfficePager);
begin
  if (FAdvOfficePager <> Value) then
  begin
    if FAdvOfficePager <> nil then FAdvOfficePager.RemoveAdvPage(Self);
    Parent := Value;
    if (Value <> nil) then
    begin
      Value.AddAdvPage(Self);
      TabIndex := FAdvOfficePager.AdvPageCount - 1;
    end;
    OnTabAppearanceFontChanged(FTabAppearance);

    if Glow and Assigned(Value) then
      FAdvOfficePager.UpdateGlowTimer;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetBadge(const Value: string);
begin
  if (FBadge <> Value) then
  begin
    FBadge := Value;
    Invalidate;
    if Assigned(FAdvOfficePager) then
      FAdvOfficePager.Invalidate;
  end;
end;

procedure TAdvOfficePage.SetBadgeColor(const Value: TColor);
begin
  if (FBadgeColor <> Value) then
  begin
    FBadgeColor := Value;
    Invalidate;
    if Assigned(FAdvOfficePager) then
      FAdvOfficePager.Invalidate;
  end;
end;

procedure TAdvOfficePage.SetBadgeTextColor(const Value: TColor);
begin
  if (FBadgeTextColor <> Value) then
  begin
    FBadgeTextColor := Value;
    Invalidate;
    if Assigned(FAdvOfficePager) then
      FAdvOfficePager.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetParent(AParent: TWinControl);
var
  ci, ni: Integer;
  AOfficePager: TAdvOfficePager;
begin
  if ((AParent is TAdvOfficePage) or (AParent is TAdvOfficePager)) and not (FUpdatingParent) then
  begin
    AOfficePager := nil;
    if (AParent is TAdvOfficePage) then
    begin
      AOfficePager := TAdvOfficePage(AParent).FAdvOfficePager;
    end
    else if (AParent is TAdvOfficePager) then
    begin
      AOfficePager := TAdvOfficePager(AParent);
    end;

    if Assigned(FAdvOfficePager) and Assigned(AOfficePager) then
    begin

      if (FAdvOfficePager <> AOfficePager) then
      begin
        FUpdatingParent := True;
        AdvOfficePager := AOfficePager;
        FUpdatingParent := False;
      end;

      if (FAdvOfficePager = AOfficePager) then
      begin
        if (AParent is TAdvOfficePage) then
        begin
          ci := FAdvOfficePager.IndexOfPage(self);
          ni := FAdvOfficePager.IndexOfPage(TAdvOfficePage(AParent));
          AParent := AOfficePager;
          if (ci >= 0) and (ci < FAdvOfficePager.FAdvPages.Count) and
             (ni >= 0) and (ni < FAdvOfficePager.FAdvPages.Count) then
          begin
            FAdvOfficePager.MoveAdvPage(ci, ni);
          end
          else
            raise Exception.Create('Invalid Parent '+inttostr(ci)+':'+inttostr(ni));
        end
        else if (AParent is TAdvOfficePager) then
        begin
          AParent := AOfficePager;
        end;
        
        FAdvOfficePager.Invalidate;
        Invalidate;
      end
      else
        raise Exception.Create('Invalid Parent');
    end;
    //else
      //raise Exception.Create('Invalid Parent3');
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetTabVisible(const Value: Boolean);
var
  i: Integer;
begin
  if (FTabVisible <> Value) then
  begin
    if Assigned(FAdvOfficePager) then
    begin
      if not Value then
        FAdvOfficePager.FClosedPageList.AddObject(Self.Name, Self)
      else
      begin
        i := FAdvOfficePager.FClosedPageList.IndexOfObject(Self);
        if (i >= 0) then
          FAdvOfficePager.FClosedPageList.Delete(i);
      end;
      FAdvOfficePager.UpdateClosedListButton;
    end;
    FTabVisible := Value;
    if Assigned(FAdvOfficePager) then
    begin
      if FAdvOfficePager.CloseOnTab then
        FAdvOfficePager.InitializeAndUpdateButtons;
      if Assigned(FAdvOfficePager.ActivePage) then
        FAdvOfficePager.ActivePage.Invalidate;
      FAdvOfficePager.Invalidate;
      FAdvOfficePager.UpdateTabScroller;
      FAdvOfficePager.UpdateMultiLineTabs;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.TimerProc(Sender: TObject);
var
  P: TPoint;
begin
  case FGlowState of
    gsHover:
    begin
      FStepHover := FStepHover + FTimeInc;
      if ((FStepHover > 100) and (FTimeInc > 0))
         or ((FStepHover < 0) and (FTimeInc < 0)) then
      begin
        if (FStepHover > 100) and (FTimeInc > 0) and Assigned(FAdvOfficePager) then
        begin
          FStepHover := 120;
          GetCursorPos(P);
          P := FAdvOfficePager.ScreenToClient(P);
          if not PtInRect(FAdvOfficePager.GetTabRect(Self), P) then
          begin
            FTimeInc := -GLOWSTEP;
            FGlowState := gsHover;
            FAdvOfficePager.FHotPageIndex := -1;
            Exit;
          end;
        end
        else if ((FStepHover < 0) and (FTimeInc < 0)) then
        begin
          FreeAndNil(FTimer);
          FGlowState := gsNone;
          if Assigned(FAdvOfficePager) then
            FAdvOfficePager.InvalidateTab(-1);
        end;

        FStepPush := 0;
        if (FStepHover > 100) then
          FStepHover := 120;
        if (FStepHover < 0) then
          FStepHover := -20;
      end
      else if Assigned(FAdvOfficePager) then
        FAdvOfficePager.InvalidateTab(-1);
    end;
    gsPush:
    begin
      FStepPush := FStepPush + FTimeInc;
      if ((FStepPush > 100) and (FTimeInc > 0))
         or ((FStepPush < 0) and (FTimeInc < 0)) then
      begin
        FreeAndNil(FTimer);
        FGlowState := gsNone;
        FStepPush := 0;
        //FStepHover := 0;
      end
      else if Assigned(FAdvOfficePager) then
        FAdvOfficePager.InvalidateTab(-1);
    end;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMControlChange(var Message: TCMControlChange);
begin
  inherited;
  with Message do
  begin

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMControlListChange(var Message: TCMControlListChange);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FAdvOfficePager) then
    FAdvOfficePager.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.CMMouseLeave(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TAdvOfficePager then
    AdvOfficePager := TAdvOfficePager(Reader.Parent);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.PictureChanged(Sender: TObject);
begin
  if Assigned(FAdvOfficePager) then
  begin
    FAdvOfficePager.Invalidate;
    if not (csLoading in ComponentState) then
    begin
      if FAdvOfficePager.CloseOnTab and (FAdvOfficePager.ActivePage = self) then
        FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.UpdateTabScroller;
      FAdvOfficePager.UpdateMultiLineTabs;
    end;
  end;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetDisabledPicture(const Value: TGDIPPicture);
begin
  FIDisabledPicture.Assign(Value);
  if Assigned(FAdvOfficePager) then
    FAdvOfficePager.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetFloating(X, Y, W, H: integer);
begin
  if Assigned(FAdvOfficePager) and not (csDesigning in ComponentState) then
  begin
    FAdvOfficePager.SetFloatingPage(Self, X, Y);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetFloating(X, Y: integer);
begin
  if Assigned(FAdvOfficePager) and not (csDesigning in ComponentState) then
  begin
    FAdvOfficePager.SetFloatingPage(Self, X, Y);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetGlow(const Value: Boolean);
begin
  if (FGlow <> Value) then
  begin
    FGlow := Value;
    if Assigned(FAdvOfficePager) then
    begin
      FAdvOfficePager.UpdateGlowTimer;
      FAdvOfficePager.Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetGlowColor(const Value: TColor);
begin
  if (FGlowColor <> Value) then
  begin
    FGlowColor := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  if Assigned(FAdvOfficePager) then
    FAdvOfficePager.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Invalidate;
    if Assigned(FAdvOfficePager) then
    begin
      FAdvOfficePager.Invalidate;
      if FAdvOfficePager.CloseOnTab and (FAdvOfficePager.ActivePage = self) then
        FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.UpdateTabScroller;
      FAdvOfficePager.UpdateMultiLineTabs;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetChecked(const Value: Boolean);
begin
  if FChecked <> value then
  begin
    FChecked := Value;
    if Assigned(FAdvOfficePager) then
      FAdvOfficePager.Invalidate;

    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetTabEnabled(const Value: Boolean);
begin
  if (FTabEnabled <> Value) then
  begin
    FTabEnabled := Value;
    Invalidate;
    if Assigned(FAdvOfficePager) then
      FAdvOfficePager.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  if Assigned(FAdvOfficePager) then
  begin
    if not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    begin
      FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.UpdateTabScroller;
      FAdvOfficePager.UpdateMultiLineTabs;
    end;
    FAdvOfficePager.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (csSetCaption in ControlStyle) and
    not (csLoading in ComponentState) and (Name = Caption) and
    ((Owner = nil) or not (Owner is TControl) or
    not (csLoading in TControl(Owner).ComponentState));
  inherited SetName(Value);
  if ChangeText then Caption := Value;
end;

//------------------------------------------------------------------------------

function TAdvOfficePage.FloatingWindow: IFloatingPagerWindow;
var
  FWinIntf: IFloatingPagerWindow;
begin
  Result := nil;
  if not (csDesigning in ComponentState) and Assigned(Parent) then
  begin
    if Assigned(Parent.Parent) and ((Parent.Parent).GetInterface(IFloatingPagerWindow,FWinIntf)) then
    begin
      Result := FWinIntf;
    end;
  end;
end;

//------------------------------------------------------------------------------


function TAdvOfficePage.GetFloatingWindow: TForm;
var
  FWinIntf: IFloatingPagerWindow;
begin
  Result := nil;
  if not (csDesigning in ComponentState) and Assigned(Parent) then
  begin
    if Assigned(Parent.Parent) and ((Parent.Parent).GetInterface(IFloatingPagerWindow,FWinIntf)) then
    begin
      Result := TForm(Parent.Parent);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

function TAdvOfficePage.GetPageIndex: Integer;
begin
  if Assigned(FAdvOfficePager) then
    Result := FAdvOfficePager.IndexOfPage(Self)
  else
    Result := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetPageIndex(const Value: Integer);
begin
  if Assigned(FAdvOfficePager) and (Value >= 0) and (Value < FAdvOfficePager.AdvPageCount) then
  begin
    FAdvOfficePager.MoveAdvPage(FAdvOfficePager.IndexOfPage(Self), Value);
    FAdvOfficePager.Invalidate;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.HideShortCutHint;
begin
  if Assigned(FShortCutHint) and Assigned(AdvOfficePager) then
  begin
    FShortCutHint.Visible := false;
    AdvOfficePager.DestroyShortCutHintWin(FShortCutHint);
    //FShortCutHint.Free;
    FShortCutHint := nil;
  end;
end;


//------------------------------------------------------------------------------

procedure TAdvOfficePage.ShowShortCutHint;
var
  pt: TPoint;
  TabR: TRect;
begin
  if not Assigned(AdvOfficePager) then
    Exit;

  if not Assigned(FShortCutHint) then
  begin
    FShortCutHint := AdvOfficePager.CreateShortCutHintWin;
  end;

  FShortCutHint.Visible := false;
  FShortCutHint.Caption := FShortCutHintText;

  TabR := AdvOfficePager.GetTabRect(Self);
  pt := AdvOfficePager.ClientToScreen(Point(TabR.Left, TabR.Top));

  case ShortCutHintPos of
  shpLeft:
    begin
      FShortCutHint.Left := pt.X - (FShortCutHint.Width div 2);
      FShortCutHint.Top := pt.Y + ((TabR.bottom - TabR.Top) - FShortCutHint.Height) div 2;
    end;
  shpTop:
    begin
      FShortCutHint.Left := pt.X + ((TabR.Right - TabR.Left) - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  shpRight:
    begin
      FShortCutHint.Left := pt.X + (TabR.Right - TabR.Left) - (FShortCutHint.Width div 2);
      FShortCutHint.Top := pt.Y + ((TabR.bottom - TabR.Top) - FShortCutHint.Height) div 2;
    end;
  shpBottom:
    begin
      FShortCutHint.Left := pt.X + ((TabR.Right - TabR.Left) - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + (TabR.bottom - TabR.Top) - (FShortCutHint.Height div 2);
    end;
  end;

  FShortCutHint.Visible := true;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.AdjustClientRect(var Rect: TRect);
begin
  Rect := Classes.Rect(2, 2, Rect.Right-2, Rect.Bottom - 2);
  inherited AdjustClientRect(Rect);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetShowCheckBox(const Value: Boolean);
begin
  if FShowCheckBox <> value then
  begin
    FShowCheckBox := Value;
    if Assigned(FAdvOfficePager) then
    begin
      FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.Invalidate;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetShowClose(const Value: Boolean);
begin
  if (FShowClose <> Value) then
  begin
    FShowClose := Value;
    
    if not FShowClose then
    begin
      if Assigned(FCloseButton) then
      begin
        FCloseButton.Free;
        FCloseButton := nil;
      end;

      if Assigned(FCloseButtonChrome) then
      begin
        FCloseButtonChrome.Free;
        FCloseButtonChrome := nil;
      end;
    end;

    if Assigned(FAdvOfficePager) then
    begin
      FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.Invalidate;
    end;  
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetTabAppearance(const Value: TTabAppearance);
begin
  FTabAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetUseTabAppearance(const Value: Boolean);
begin
  if (FUseTabAppearance <> Value) then
  begin
    FUseTabAppearance := Value;
    OnTabAppearanceFontChanged(FTabAppearance);
    if Assigned(FAdvOfficePager) then
      FAdvOfficePager.Invalidate;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.OnTabAppearanceFontChanged(Sender: TObject);
begin
  if (FUseTabAppearance) then
  begin
    Invalidate;
    if Assigned(FAdvOfficePager) then
    begin
      FAdvOfficePager.Invalidate;
      if FAdvOfficePager.CloseOnTab and (FAdvOfficePager.ActivePage = self) then
        FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.UpdateTabScroller;
      FAdvOfficePager.UpdateMultiLineTabs;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.OnTabAppearanceChanged(Sender: TObject);
begin
  if Assigned(FAdvOfficePager) then
    FAdvOfficePager.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetPageAppearance(const Value: TVistaBackground);
begin
  FPageAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.OnPageAppearanceChanged(Sender: TObject);
begin
  FValidCache := False;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetProgress(const Value: TTabProgress);
begin
  FProgress.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.OnProgressChanged(Sender: TObject);
begin
  if Assigned(FAdvOfficePager) then
    FAdvOfficePager.Invalidate;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetUsePageAppearance(const Value: Boolean);
begin
  if (FUsePageAppearance <> Value) then
  begin
    FUsePageAppearance := Value;
    FValidCache := False;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePage.SetWideCaption(const Value: widestring);
begin
  if (FWideCaption <> Value) then
  begin
    FWideCaption := Value;
    Invalidate;
    if Assigned(FAdvOfficePager) and (Caption = '') then
    begin
      FAdvOfficePager.Invalidate;
      if FAdvOfficePager.CloseOnTab and (FAdvOfficePager.ActivePage = self) then
        FAdvOfficePager.InitializeAndUpdateButtons;
      FAdvOfficePager.UpdateTabScroller;
      FAdvOfficePager.UpdateMultiLineTabs;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvOfficePager }

constructor TAdvOfficePager.Create(AOwner: TComponent);
var
  ps: TAdvOfficePagerOfficeStyler;
  i: integer;
begin
{$IFNDEF DELPHI6_LVL}
  FIsAeroVista := False;
{$ELSE}
  FIsAeroVista := IsComCtl6 and IsVista {and ThemeServices.ThemesEnabled} and AeroIsEnabled and not (csDesigning in ComponentState);
{$ENDIF}

{$IFNDEF TMS_DWM}
  FIsAeroVista := False;
{$ENDIF}

{$IFDEF TMS_FORCEAERO}
  FIsAeroVista := True;
{$ENDIF}


  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];

  FFloatingPagerWindowClass := TFloatingPagerWindow;

  if (csDesigning in ComponentState) then
    FIsAeroVista := False;

  FBufferedPages := false;
  FFormScaled := true;

  FClosedPageList := TStringList.Create;

  FFixedTabs := 0;

  FTabRoundEdges := True;
  FShow3D := True;
  FShadow := True;
  FItones := False;
  FPageAppearance := nil;
  FTabAppearance := nil;
  FGlowButtonAppearance := nil;

  //inherited Align := alTop;

  {$IFDEF DELPHI7_LVL}
  //if FIsAeroVista then
    //ControlStyle := ControlStyle + [csParentBackground];
  {$ENDIF}

  FInternalOfficePagerStyler := TCustomAdvOfficePagerStyler.Create(self);
  FInternalOfficePagerStyler.Name := 'InternalStyler';

  FOfficePagerStyler := nil;
  FCurrentOfficePagerStyler := FInternalOfficePagerStyler;
  FCurrentOfficePagerStyler.AddControl(self);
  {$IFDEF DELPHI6_LVL}
  FInternalOfficePagerStyler.SetSubComponent(True);
  {$ENDIF}

  FOffSetX := 0;
  FOffSetY := 0;

  FClosedListButton := nil;
  FInternalClosedListMenu := nil;

  FTabOffSet := 4;
  FPageMargin := ADVPAGE_OFFSET;
  FIsClosing := false;

  FTabPosition := tpTop;

  FAntiAlias := aaClearType;

  FAdvPages := TDbgList.Create;

  FTabScroller := TPagerTabScroller.Create;

  FTabSettings := TPagerTabSettings.Create;
  FTabSettings.OnChange := OnTabSettingsChanged;
  FTabSettings.FPager := Self;

  FAllowTabUndock := False;

  FActivePageIndex := -1;
  FHotPageIndex := -1;
  FOldHotPageIndex := -1;
  FDownPageIndex := -1;

  FShowTabHint := false;
  FHintPageIndex := -1;
  ShowHint := false;
  FShowCloseOnNonSelectedTabs := false;

  FButtonSettings := TPageButtonSettings.Create;
  FButtonSettings.OnChange := OnButtonSettingChanged;
  FPageListMenu := nil;
  FRotateTabLeftRight := true;
  FCloseOnTab := false;
  FCloseOnTabPosition := cpRight;

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsWinXP := (i > 5);

  DoubleBuffered := true;
  Height := 200;
  Width := 400;
  FOldCapRightIndent := 0;

  FOfficeHint := TAdvHintInfo.Create;

  FPageListButton := nil;
  FScrollPrevButton := nil;
  FScrollNextButton := nil;
  FScrollFirstButton := nil;
  FScrollLastButton := nil;
  FCloseButtonGlobal := nil;

  FShortCutHintWinList := TDbgList.Create;
  FShowShortCutHints := False;

  FTabReorder := False;
  FButtonsBkg := TBitmap.Create;

  FDesignTime := (csDesigning in ComponentState) and not
      ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  ps := TAdvOfficePagerOfficeStyler.Create(self);
  ps.Style := psOffice2007Luna;
  FInternalOfficePagerStyler.Assign(ps);
  ps.Free;

  FGlow := true;
  FTabReorderIndicatorColor := clBlue;
  FFloatingBorderIcons := [biSystemMenu, biMinimize, biMaximize];
  FFloatingBorderStyle := bsSizeable;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CreateParams(var Params: TCreateParams);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CreateWnd;
var
  p: TWinControl;
  t: TAdvOfficePage;
  gotpages: boolean;
  i: integer;

begin
  inherited;

  gotpages := false;

  if not (csDesigning in ComponentState) then
  begin
    p := self;

    repeat
      p := p.Parent;
    until (p is TForm) or (p is TActiveForm) or not Assigned(p);

    if Assigned(p) then
    begin
      if (p is TForm) then
        FFormScaled := (p as TForm).Scaled;

      for i := 0 to p.ComponentCount - 1 do
      begin
        if p.Components[i].Name = Name + '1' then
          gotpages := true;
      end;
    end;

    TabSettings.Height := Round(GetDPIScale(FFormScaled, Canvas.Handle)* TabSettings.Height);
  end;

  if FDesignTime and (Name <> '') and not gotpages then
  begin
    FDesignTime := false;
    t := TAdvOfficePage.Create(Owner);
    t.AdvOfficePager := self;
    t.Name := Name + '1';
    t.Caption := t.Name;
    t.TabIndex := 0;
    t := TAdvOfficePage.Create(Owner);
    t.AdvOfficePager := self;
    t.Name := Name + '2';
    t.Caption := t.Name;
    t.TabIndex := 1;
    t := TAdvOfficePage.Create(Owner);
    t.AdvOfficePager := self;
    t.Name := Name + '3';
    t.Caption := t.Name;
    t.TabIndex := 2;
    ActivePageIndex := 0;
  end;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficePager.Destroy;
var
  i: Integer;
begin
  if Assigned(FBrGlowTimer) then
    FBrGlowTimer.Free;

  if not (csDesigning in ComponentState) and Assigned(FArrow) then
    FreeAndNil(FArrow);

  if Assigned(FOfficePagerStyler)
     and (FOfficePagerStyler <> FInternalOfficePagerStyler) then
       FOfficePagerStyler.RemoveControl(self);

  FreeAndNil(FInternalOfficePagerStyler);

  for I := 0 to FAdvPages.Count - 1 do
    TAdvOfficePage(FAdvPages[I]).FAdvOfficePager := nil;

  if Assigned(FFloatingPages) then
  begin
    for I := 0 to FFloatingPages.Count - 1 do
      TAdvOfficePage(FFloatingPages[i]).FParentPager := nil;
    FFloatingPages.Free;
  end;

  FAdvPages.Free;
  FTabSettings.Free;
  FTabScroller.Free;
  FOfficeHint.Free;
  FButtonSettings.Free;

  if Assigned(FDockList) then
    FDockList.Free;

  if (FPageListButton <> nil) then
    FPageListButton.Free;
  if (FScrollPrevButton <> nil) then
    FScrollPrevButton.Free;
  if (FScrollNextButton <> nil) then
    FScrollNextButton.Free;
  if (FScrollFirstButton <> nil) then
    FScrollFirstButton.Free;
  if (FScrollLastButton <> nil) then
    FScrollLastButton.Free;
  if (FCloseButtonGlobal <> nil) then
    FCloseButtonGlobal.Free;

  if Assigned(FClosedListButton) then
    FClosedListButton.Free;
  if Assigned(FInternalClosedListMenu) then
    FInternalClosedListMenu.Free;

  if FClosedPageList <> nil then
    FClosedPageList.Free;

  for i := 0 to FShortCutHintWinList.Count - 1 do
  begin
    if (FShortCutHintWinList.Items[i] <> nil) then
    begin
      TShortCutHintWindow(FShortCutHintWinList.Items[i]).Free;
      FShortCutHintWinList.Items[i] := nil;
    end;
  end;

  if Assigned(FPageAppearance) then
    FPageAppearance.Free;
  if Assigned(FTabAppearance) then
    FTabAppearance.Free;
  if Assigned(FGlowButtonAppearance) then
    FGlowButtonAppearance.Free;

  FShortCutHintWinList.Free;
  FButtonsBkg.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.MoveAdvPage(CurIndex, NewIndex: Integer);
var
  OldActivePage: TAdvOfficePage;
begin
  if (CurIndex >= 0) and (CurIndex < FAdvPages.Count) and
     (NewIndex >= 0) and (NewIndex < FAdvPages.Count) then
  begin
    OldActivePage := ActivePage;
    FAdvPages.Move(CurIndex, NewIndex);
    ActivePage := OldActivePage;
    UpdateTabScroller;
    UpdateMultiLineTabs;

    if Assigned(FOnTabMoved) then
      FOnTabMoved(Self, CurIndex, NewIndex);
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.AddAdvPage(AdvPage: TAdvOfficePage): integer;
var
  i: Integer;
  floatingWndow: TForm;
begin
  Result := -1;
  if not Assigned(AdvPage) then
    Exit;

  Result := FAdvPages.IndexOf(AdvPage);
  floatingWndow := AdvPage.GetFloatingWindow;
  if (FAdvPages.IndexOf(AdvPage) < 0) then
  begin
    FAdvPages.Add(AdvPage);
    Result := FAdvPages.Count - 1;

    if Assigned(FFloatingPages) then
    begin
      i := FFloatingPages.IndexOf(AdvPage);
      if (i >= 0) then
        FFloatingPages.Delete(i);
    end;

    if (csDesigning in ComponentState) and Assigned(FCurrentOfficePagerStyler) then
    begin
      if not AdvPage.UsePageAppearance then
      begin
        if Assigned(FPageAppearance) and FItones then
          AdvPage.PageAppearance.Assign(FPageAppearance)
        else
          AdvPage.PageAppearance.Assign(FCurrentOfficePagerStyler.PageAppearance);
      end;

      if not AdvPage.UseTabAppearance then
      begin
        if Assigned(FTabAppearance) and FItones then
          AdvPage.TabAppearance.Assign(FTabAppearance)
        else
          AdvPage.TabAppearance.Assign(FCurrentOfficePagerStyler.TabAppearance);
      end;
    end;
  end;

  if (AdvPage.Parent <> Self) then
    AdvPage.Parent := Self;

  AdvPage.FAdvOfficePager := Self;

  SetPagePosition(AdvPage);

  if not Assigned(FClonedOfPager) then
    AdvPage.FFloating := False;

  if Assigned(floatingWndow) then
  begin
    if (FFloatingWindow = floatingWndow) then
      FFloatingWindow := nil;
    floatingWndow.Free;
  end;

  if (AdvPage <> ActivePage) then
    AdvPage.Visible := False;

  InvalidateTab(-1);
  if Assigned(ActivePage) then
  begin
    ActivePage.BringToFront;
    ActivePage.Invalidate;
  end;

  UpdateTabScroller;
  UpdateMultiLineTabs;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.AddAdvPage(PageCaption: TCaption): integer;
var
  aPage: TAdvOfficePage;
begin
  aPage := TAdvOfficePage.Create(Self);
  aPage.Caption := PageCaption;
  Result := AddAdvPage(aPage);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.AlignControls(AControl: TControl;
  var ARect: TRect);
begin
  inherited;
  {if (AControl <> nil) and (AControl is TAdvOfficePage) then
    SetPagePosition(TAdvOfficePage(AControl))
  else if (AControl is TAdvOfficePage) then} 
  SetAllPagesPosition;  
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.Loaded;
begin
  inherited;
  FPropertiesLoaded := True;
  UpdateTabScroller;
  UpdateMultiLineTabs;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SaveToFile(FileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Add(Settings);
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.LoadFromFile(FileName: String);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    if sl.Count = 1 then
      Settings := sl.Strings[0];
  finally
    sl.Free;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PageByName(Value: string): TAdvOfficePage;
var
  i: Integer;
begin
  Result := nil;
  if (Value = '') then
    Exit;

  for I := 0 to FAdvPages.Count - 1 do
  begin
    if (UpperCase(Value) = UpperCase(AdvPages[I].Name)) then
    begin
      Result := AdvPages[I];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PageByCaption(Value: string): TAdvOfficePage;
var
  i: Integer;
begin
  Result := nil;
  if (Value = '') then
    Exit;

  for I := 0 to FAdvPages.Count - 1 do
  begin
    if (UpperCase(Value) = UpperCase(AdvPages[I].Caption)) then
    begin
      Result := AdvPages[I];
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if AComponent = AdvOfficePagerStyler then
      AdvOfficePagerStyler := nil; 
    if (AComponent = PopupMenu) then
      PopupMenu := nil;
    if (AComponent = Images) then
      Images := nil;
    if (AComponent = DisabledImages) then
      DisabledImages := nil;    
    if (AComponent = PageListMenu) then
      PageListMenu := nil;
    if (AComponent = ClosedListMenu) then
      ClosedListMenu := nil;
    if (AComponent is TAdvOfficePage) then
      RemoveAdvPage(TAdvOfficePage(AComponent));
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.AdjustClientRect(var Rect: TRect);
begin
  { if Align in [daTop, daBottom] then
   begin
     if (FAdvPages.Count > 0) then
       Rect.Top := Rect.Top + TabSettings.Height;
     if (FCaption.Visible) then
       Rect.Top := Rect.Top + FCaption.Height;
   end; }
  inherited AdjustClientRect(Rect);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabImageSize(PageIndex: Integer): TSize;
var
  Pic: TGDIPPicture;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (PageIndex < 0) or (PageIndex >= FAdvPages.Count) then
    Exit;

  if AdvPages[PageIndex].Enabled or AdvPages[PageIndex].DisabledPicture.Empty then
    Pic := AdvPages[PageIndex].Picture
  else
    Pic := AdvPages[PageIndex].DisabledPicture;

  if Assigned(Pic) and not Pic.Empty then
  begin
    Pic.GetImageSizes;
    Result.cx := Pic.Width;
    Result.cy := Pic.Height;
  end
  else
  if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[PageIndex].ImageIndex >= 0) then
  begin
    if AdvPages[PageIndex].Enabled then
    begin
      if Assigned(FImages) then
      begin
        Result.cx := FImages.Width;
        Result.cy := FImages.Height;
      end;
    end
    else
    begin
      if Assigned(FDisabledImages) then
      begin
        Result.cx := FDisabledImages.Width;
        Result.cy := FDisabledImages.Height;
      end
      else if Assigned(FImages) then
      begin
        Result.cx := FImages.Width;
        Result.cy := FImages.Height;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTextSize(PageIndex: Integer): TSize;
var
  R: TRect;
  //Ellipsis: Boolean;
  OldF: TFont;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (PageIndex < 0) or (PageIndex >= FAdvPages.Count) then
    Exit;

  //Ellipsis := (TabSettings.Width > 0) and not TabSettings.WordWrap;
  OldF := TFont.Create;
  OldF.assign(Canvas.Font);
  Canvas.Font.Assign(FCurrentOfficePagerStyler.TabAppearance.Font);
  Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

  if (AdvPages[PageIndex].Caption <> '') then
  begin
    R := Rect(0,0, 1000, 100);
    DrawText(Canvas.Handle,PChar(AdvPages[PageIndex].Caption),Length(AdvPages[PageIndex].Caption), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    Result.cx := R.Right;
    Result.cy := R.Bottom;
    case AntiAlias of
      aaNone, aaClearType:
        Result.cx := Result.cx + length(AdvPages[PageIndex].Caption) div 3;
    end;
  end
  else if (AdvPages[PageIndex].WideCaption <> '') then
  begin
    R := Rect(0,0, 1000, 100);
    DrawTextW(Canvas.Handle, PWideChar(AdvPages[PageIndex].WideCaption), -1, R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    Result.cx := R.Right;
    Result.cy := R.Bottom;
    case AntiAlias of
      aaNone, aaClearType:
        Result.cx := Result.cx + length(AdvPages[PageIndex].WideCaption);
    end;
  end;

  Canvas.Font.Assign(OldF);
  OldF.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.GetCloseBtnImageAndTextRect(PageIndex: Integer;
  var CloseBtnR, TextR: TRect; var ImgP: TPoint);
var
  ActivePg: Boolean;
  R, R1: TRect;
  i, tw: Integer;
  ImgSize, TxtSize, CloseBtnSize: TSize;
begin
  if (PageIndex < 0) or (PageIndex >= FAdvPages.Count) or (TabSettings.Height <= 0) or (TabSettings.Width <= 0) then
    Exit;

  R := GetTabRect(PageIndex);
  if (R.Left <= -1) and (R.Right <= -1) then
    Exit;

  R1 := R;
  ActivePg := (ActivePageIndex = PageIndex);

  ImgSize := GetTabImageSize(PageIndex);
  TxtSize := GetTextSize(PageIndex);
  if CloseOnTab then
  begin
    CloseBtnSize.cx := ButtonSettings.ButtonSize;
    CloseBtnSize.cy := ButtonSettings.ButtonSize;
  end
  else
  begin
    CloseBtnSize.cx := 0;
    CloseBtnSize.cy := 0;
  end;
  
  if (TabPosition in [tpTop, tpBottom]) or (not RotateTabLeftRight) then
  begin
    i := R.Right - R.Left;
    tw := i;

    if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
      R.Right := R.Right - TabSettings.Rounding;

    case TabSettings.ImagePosition of
      ipTop, ipBottom:
        ImgSize.cx := 0;
      ipLeft, ipRight:
      begin
      end;
    end;
    
    case TabSettings.Alignment of
      taLeftJustify:
      begin
      end;
      taCenter:
      begin
        if (CloseBtnSize.cx > 0) and ActivePg then
          i := (i - CloseBtnSize.cx - 4);
        if (ImgSize.cx > 0) then
          i := i - ImgSize.cx - IMG_SPACE;
        if (i - TxtSize.cx) < 0 then
          i := 0
        else
          i := (i - TxtSize.cx) div 2;
        R.Left := Max(R.Left + i, R.Left);
        
        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Left := R.Left;
          CloseBtnR.Right := CloseBtnR.Left + ButtonSettings.ButtonSize;
          R.Left := CloseBtnR.Right + 4;
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.X := R.Left;
          R.Left := R.Left + ImgSize.cx + IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Left := R.Left;
          TextR.Right := TextR.Left + TxtSize.cx;
          R.Left := Min(R.Left + TxtSize.cx, R.Right);
        end;
        
        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.X := R.Left + IMG_SPACE;
          R.Left := R.Left + ImgSize.cx + IMG_SPACE;
        end;

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          if (R.Left + ButtonSettings.ButtonSize) > (R1.Left + tw) then
            CloseBtnR.Left := R.Right - ButtonSettings.ButtonSize - 4
          else
            CloseBtnR.Left := R.Left + 4;
          CloseBtnR.Right := CloseBtnR.Left + ButtonSettings.ButtonSize;
        end;
      end;
      taRightJustify:
      begin
      end;
    end;
  end
  else if (TabPosition = tpLeft) then
  begin
    i := R.Bottom - R.Top;
    if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) then
      R.Bottom := R.Bottom - TabSettings.Rounding;
    if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
      R.Top := R.Top + TabSettings.Rounding;

    case TabSettings.ImagePosition of
      ipTop, ipBottom:
        ImgSize.cx := 0;
      ipLeft, ipRight:
      begin
      end;
    end;

    if (CloseBtnSize.cx > 0) and ActivePg then
      i := (i - CloseBtnSize.cx - 4);
    if (ImgSize.cx > 0) then
      i := i - ImgSize.cx - IMG_SPACE;
    i := (i - TxtSize.cx) div 2;
    
    case TabSettings.Alignment of
      taLeftJustify:
      begin
      end;
      taCenter:
      begin
        R.Bottom := Min(R.Bottom - i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Top := R.Bottom - ButtonSettings.ButtonSize;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Bottom := CloseBtnR.Top - 4;
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - ImgSize.cy;
          R.Bottom := R.Bottom - ImgSize.cy - IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Bottom := R.Bottom;
          TextR.Top := TextR.Bottom - TxtSize.cx;
          R.Bottom := Max(R.Bottom - TxtSize.cx, R.Top);
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - ImgSize.cy - IMG_SPACE;
          R.Bottom := ImgP.Y;
        end;

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          if ((R.Bottom - ButtonSettings.ButtonSize) < R1.Top) then
            CloseBtnR.Bottom := R1.Top + ButtonSettings.ButtonSize + 4
          else
            CloseBtnR.Bottom := R.Bottom - 4;
          CloseBtnR.Top := CloseBtnR.Bottom - ButtonSettings.ButtonSize;
        end;
      end;
      taRightJustify:
      begin
        R.Bottom := Min(R.Bottom - i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Top := R.Top;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Top := CloseBtnR.Bottom + 4;
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top;
          R.Top := ImgP.Y + ImgSize.cy + IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Top := R.Top;
          TextR.Bottom := TextR.Top + TxtSize.cx;
          R.Top := Min(R.Top + TxtSize.cx, R.Bottom);
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top + IMG_SPACE;
          R.Top := ImgP.Y + ImgSize.cy;
        end;
        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Top := R.Top + 4;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Top := CloseBtnR.Bottom;
        end;
      end;
    end;
  end
  else if (TabPosition = tpRight) then
  begin
    i := R.Bottom - R.Top;
    if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) then
      R.Bottom := R.Bottom - TabSettings.Rounding;
    if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
      R.Top := R.Top + TabSettings.Rounding;

    case TabSettings.ImagePosition of
      ipTop, ipBottom:
        ImgSize.cx := 0;
      ipLeft, ipRight:
      begin
      end;
    end;

    if (CloseBtnSize.cx > 0) and ActivePg then
      i := (i - CloseBtnSize.cx - 4);
    if (ImgSize.cx > 0) then
      i := i - ImgSize.cx - IMG_SPACE;
    i := (i - TxtSize.cx) div 2;
    
    case TabSettings.Alignment of
      taLeftJustify:
      begin
      end;
      taCenter:
      begin
        R.Top := Min(R.Top + i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Top := R.Top;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Top := CloseBtnR.Top + ButtonSettings.ButtonSize + 4;
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top;
          R.Top := R.Top + ImgSize.cy + IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Top := R.Top;
          TextR.Bottom := TextR.Top + TxtSize.cx;
          R.Top := Min(R.Top + TxtSize.cx, R.Bottom);
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Top + IMG_SPACE;
          R.Top := ImgP.Y + ImgSize.cy;
        end;

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          if (R.Top + ButtonSettings.ButtonSize > R1.Bottom) then
            CloseBtnR.Top := R1.Bottom - ButtonSettings.ButtonSize - 4
          else
            CloseBtnR.Top := R.Top + 4;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
        end;
      end;
      taRightJustify:
      begin
        //R.Top := Min(R.Top + i, R.Bottom);

        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpRight) then
        begin
          CloseBtnR.Top := R.Bottom - ButtonSettings.ButtonSize;
          CloseBtnR.Bottom := CloseBtnR.Top + ButtonSettings.ButtonSize;
          R.Bottom := CloseBtnR.Top - 4;
        end;

        if (TabSettings.ImagePosition = ipRight) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - ImgSize.cy;
          R.Bottom := ImgP.Y - ImgSize.cy - IMG_SPACE;
        end;

        if (TxtSize.cx > 0) then
        begin
          TextR.Bottom := R.Bottom;
          TextR.Top := TextR.Bottom - TxtSize.cx;
          R.Bottom := Max(R.Bottom - TxtSize.cx, R.Top);
        end;

        if (TabSettings.ImagePosition = ipLeft) and (ImgSize.cx > 0) then
        begin
          ImgP.Y := R.Bottom - IMG_SPACE - ImgSize.cy;
          R.Bottom := ImgP.Y;
        end;
        if ActivePg and CloseOnTab and (CloseOnTabPosition = cpLeft) then
        begin
          CloseBtnR.Bottom := R.Bottom - 4;
          CloseBtnR.Top := CloseBtnR.Bottom - ButtonSettings.ButtonSize;
          R.Bottom := CloseBtnR.Top;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DrawTab(PageIndex: Integer);
var
  GradColor: TColor;
  GradColorTo: TColor;
  GradColorMirror: TColor;
  GradColorMirrorTo: TColor;
  PenColor, TempClr: TColor;
  GradB, GradU: TGDIPGradient;
  R,BR: TRect;
  DoRepaint: Boolean;
  TxtClr: TColor;
  TabAppearance: TCustomTabAppearance;
  bmp: TBitMap;
  TabPos: TTabPosition;
  RotateLR: Boolean;
  SelectedHot: Boolean;
  Layout: TButtonLayout;
  R3: TRect;
  Shape: TAdvTabShape;
  HighLightClr: TColor;
  htheme: THandle;
  ThemeStyle: DWord;
  rc: TRect;
  DChecked: Cardinal;
  c1, c2, c3, c4: TColor;
  gpgraphics: TGPGraphics;
  gpbrush: TGPBrush;
  badgexy: TPoint;
begin
  if (PageIndex < 0) or (PageIndex >= FAdvPages.Count) or (TabSettings.Height <= 0) then
    Exit;

  GradColor := clNone;
  GradColorTo := clNone;
  GradColorMirror := clNone;
  GradColorMirrorTo := clNone;
  PenColor := clNone;
  TxtClr := clNone;
  GradB := ggRadial;
  GradU := ggRadial;

  DoRepaint := True;

  SelectedHot := False;

  R := GetTabRect(PageIndex);

  if (R.Left <= -1) and (R.Right <= -1) then
    Exit;

  Layout := blGlyphLeft;

  if AdvPages[PageIndex].UseTabAppearance then
    TabAppearance := AdvPages[PageIndex].TabAppearance
  else if Assigned(FTabAppearance) and FItones then
    TabAppearance := FTabAppearance
  else
    TabAppearance := FCurrentOfficePagerStyler.TabAppearance;

  HighLightClr := TabAppearance.HighLightColor;
    
  with TabAppearance do
  begin
    //DrawDwLn := False;
    if not(AdvPages[PageIndex].TabEnabled) and ShowNonSelectedTabs then
    begin
      if ShowNonSelectedTabs then
      begin
        GradColor := ColorDisabled;
        GradColorTo := ColorDisabledTo;
        GradColorMirror := ColorMirrorDisabled;
        GradColorMirrorTo := ColorMirrorDisabledTo;
        PenColor := BorderColorDisabled;
        GradU := GradientDisabled;
        GradB := GradientMirrorDisabled;
        TxtClr := TextColorDisabled;
      end;
    end
    else
    if (PageIndex = ActivePageIndex) then
    begin
      GradColor := ColorSelected;
      GradColorTo := ColorSelectedTo;
      GradColorMirror := ColorMirrorSelected;
      GradColorMirrorTo := ColorMirrorSelectedTo;
      PenColor := BorderColorSelected;
      GradU := GradientSelected;
      GradB := GradientMirrorSelected;
      TxtClr := TextColorSelected;
      HighLightClr := TabAppearance.HighLightColor;

      if (PageIndex = FHotPageIndex) then
      begin
        PenColor := BorderColorSelectedHot;
        HighLightClr := TabAppearance.HighLightColorSelectedHot;
      end;

      if Assigned(AdvPages[PageIndex].FTimer) then
      begin
        if (AdvPages[PageIndex].FGlowState = gsPush) then
        begin
          GradColor := BlendColor(GradColor, FColorHot, AdvPages[PageIndex].FStepPush);
          GradColorTo := BlendColor(GradColorTo, FColorHotTo, AdvPages[PageIndex].FStepPush);
          GradColorMirror := BlendColor(GradColorMirror, FColorMirrorHot, AdvPages[PageIndex].FStepPush);
          GradColorMirrorTo := BlendColor(GradColorMirrorTo, FColorMirrorHotTo, AdvPages[PageIndex].FStepPush);
          PenColor := BlendColor(PenColor, BorderColorHot, AdvPages[PageIndex].FStepPush);
        end
        else if (AdvPages[PageIndex].FGlowState = gsHover) then
          PenColor := BlendColor(BorderColorSelectedHot, BorderColorSelected, AdvPages[PageIndex].FStepHover);
      end;

      if (FDownPageIndex = PageIndex) and not (csDesigning in ComponentState) then
      begin
        PenColor := BorderColorDown;
        HighLightClr := TabAppearance.HighLightColorDown;
      end;
    end
    else //if State = absUp then
    begin
      if (PageIndex = FHotPageIndex) then
      begin
        GradColor := ColorHot;
        GradColorTo := ColorHotTo;
        GradColorMirror := ColorMirrorHot;
        GradColorMirrorTo := ColorMirrorHotTo;
        PenColor := BorderColorHot;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        TxtClr := TextColorHot;
        HighLightClr := TabAppearance.HighLightColorHot;
        //DrawDwLn := True;
        if Assigned(AdvPages[PageIndex].FTimer) and (AdvPages[PageIndex].FGlowState = gsHover) then
        begin
          if ShowNonSelectedTabs then
          begin
            GradColor := BlendColor(FColorHot, FColor, AdvPages[PageIndex].FStepHover);
            GradColorTo := BlendColor(FColorHotTo, FColorTo, AdvPages[PageIndex].FStepHover);
            GradColorMirror := BlendColor(FColorMirrorHot, FColorMirror, AdvPages[PageIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, FColorMirrorTo, AdvPages[PageIndex].FStepHover);

            PenColor := BlendColor(BorderColorHot, BorderColor, AdvPages[PageIndex].FStepHover);
          end
          else
          begin
            GradColor := BlendColor(FColorHot, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);
            if ({FCurrentOfficePagerStyler.}TabAppearance.BackGround.ColorTo <> clNone) then
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.ColorTo, AdvPages[PageIndex].FStepHover)
            else
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);

            GradColorMirror := BlendColor(FColorMirrorHot, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);

            if BorderColorHot <> clNone then
              PenColor := BlendColor(BorderColorHot, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover)
          end;
        end;
      end
      else // Normal draw
      begin
        if ShowNonSelectedTabs then
        begin
          GradColor := Color;
          GradColorTo := ColorTo;
          GradColorMirror := ColorMirror;
          GradColorMirrorTo := ColorMirrorTo;
          PenColor := BorderColor;
          GradU := Gradient;
          GradB := GradientMirror;
          TxtClr := TextColor;
          if Assigned(AdvPages[PageIndex].FTimer) and (AdvPages[PageIndex].FGlowState = gsHover) {and (PageIndex = FOldHotPageIndex)} then
          begin
            GradColor := BlendColor(FColorHot, FColor, AdvPages[PageIndex].FStepHover);
            if FColorTo <> clNone then
              GradColorTo := BlendColor(FColorHotTo, FColorTo, AdvPages[PageIndex].FStepHover)
            else
              GradColorTo := BlendColor(FColorHotTo, FColor, AdvPages[PageIndex].FStepHover);

            GradColorMirror := BlendColor(FColorMirrorHot, FColorMirror, AdvPages[PageIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, FColorMirrorTo, AdvPages[PageIndex].FStepHover);

            PenColor := BlendColor(BorderColorHot, BorderColor, AdvPages[PageIndex].FStepHover);
          end;
        end
        else
        begin
          DoRepaint := False;
          TxtClr := TextColor;
          GradU := GradientHot;
          GradB := GradientMirrorHot;

          if not AdvPages[PageIndex].TabEnabled then
           TxtClr := TextColorDisabled;

          //if Transparent then
          //  TxtClr := clBlack;

          if Assigned(AdvPages[PageIndex].FTimer) and (AdvPages[PageIndex].FGlowState = gsHover){ and (PageIndex = FOldHotPageIndex)} then
          begin
            GradColor := BlendColor(FColorHot, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);
            if ({FCurrentOfficePagerStyler.}TabAppearance.BackGround.ColorTo <> clNone) then
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.ColorTo, AdvPages[PageIndex].FStepHover)
            else
              GradColorTo := BlendColor(FColorHotTo, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);

            GradColorMirror := BlendColor(FColorMirrorHot, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);
            GradColorMirrorTo := BlendColor(FColorMirrorHotTo, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);

            if BorderColorHot <> clNone then
              PenColor := BlendColor(BorderColorHot, {FCurrentOfficePagerStyler.}TabAppearance.BackGround.Color, AdvPages[PageIndex].FStepHover);
            DoRepaint := True;
          end;
        end;
      end;
    end;

    if Focused and (PageIndex = ActivePageIndex) then
    begin
      GradColor := ColorHot;
      GradColorTo := ColorHotTo;
      GradColorMirror := ColorMirrorHot;
      GradColorMirrorTo := ColorMirrorHotTo;
      PenColor := BorderColorSelectedHot;
      GradU := GradientHot;
      GradB := GradientMirrorHot;
      TxtClr := TextColorHot;
      HighLightClr := TabAppearance.HighLightColorSelected;
      SelectedHot := True;
    end;

    if not (csDesigning in ComponentState) and CanGlow and Assigned(FBrGlowTimer) and AdvPages[PageIndex].Glow and (AdvPages[PageIndex].GlowColor <> clNone) then
    begin
      PenColor := BlendColor(AdvPages[PageIndex].GlowColor, PenColor, FStepHover);
    end;

    c1 := GradColor;
    c2 := c1;
    c3 := c1;
    c4 := c3;

    if DoRepaint then
    begin
      Shape := TabSettings.Shape;
      RotateLR := True;
      TabPos := TabPosition;
      if (TabPos in [tpLeft, tpRight]) and not RotateTabLeftRight then
      begin
        TabPos := tpTop;
        RotateLR := False;
        Shape := tsRectangle;
      end;

      case TabPosition of
        tpTop:
        begin
          c1 := Canvas.Pixels[R.Left, R.Top];
          c2 := Canvas.Pixels[R.Right - 1, R.Top];
        end;
        tpBottom:
        begin
          c1 := Canvas.Pixels[R.Left, R.Bottom-2];
          c2 := Canvas.Pixels[R.Right - 1, R.Bottom-1];
          c3 := Canvas.Pixels[R.Left, R.Top];
          c4 := Canvas.Pixels[R.Right - 1, R.Top];
        end;
        tpLeft:
        begin
          c1 := Canvas.Pixels[R.Left, R.Top];
          c2 := Canvas.Pixels[R.Left, R.Bottom-1];
        end;
        tpRight:
        begin
          c1 := Canvas.Pixels[R.Right, R.Top];
          c2 := Canvas.Pixels[R.Right, R.Bottom-1];
          c3 := Canvas.Pixels[R.Right - 1, R.Top];
          c4 := Canvas.Pixels[R.Right - 1, R.Bottom-1];
        end;
      end;

      if (ActivePageIndex = PageIndex) and (CloseOnTab) and (ButtonSettings.CloseButton) then
      begin
        bmp := TBitmap.Create;
        try
          bmp.Height := R.Bottom - R.Top;
          bmp.Width := R.Right - R.Left;
          R3 := Rect(0, 0, R.Right - R.Left, R.Bottom-R.Top);
          if UseOldDrawing then
            DrawVistaGradient(bmp.Canvas, Rect(0, 0, R.Right - R.Left, R.Bottom-R.Top),GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
              GradU, GradB, '', {Canvas.}Font, Layout, Enabled, False, FAntiAlias, {True}TabRoundEdges, RotateLR, AdvPages[PageIndex].Progress, False, False, TabPos)
          else
          begin
            case TabPosition of
              tpTop: R3.Bottom := R3.Bottom - 3;
              tpBottom: R3.Top := R3.Top + 2;
              tpLeft: R3.Right := R3.Right - 3;
              tpRight: R3.Left := R3.Left + 2;
            end;

            TempClr := BlendColor(PenColor, clWhite, 50);
            bmp.Canvas.Brush.Color := TempClr;
            bmp.Canvas.FillRect(R3);
            case TabPosition of
              tpTop: R3.Right := R3.Right - 1;
              tpBottom:
              begin
                R3.Right := R3.Right - 1;
                R3.Bottom := R3.Bottom - 1;
              end;
              tpLeft: R3.Bottom := R3.Bottom - 1;
              tpRight:
              begin
                R3.Right := R3.Right - 1;
                R3.Bottom := R3.Bottom - 2;
              end;
            end;

            DrawVistaTab(bmp.Canvas, R3, GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
              GradU, GradB, Enabled, Shape, False, TabSettings.Rounding, RotateTabLeftRight, TabPosition, AdvPages[PageIndex].Progress);

            bmp.TransparentColor := TempClr;
            bmp.Transparent := True;
          end;
          if IsGlass then
            DrawGDIPImage(nil, Canvas, Point(R.Left, R.Top), bmp, (Shape <> tsRectangle) or (TabSettings.Rounding > 3))
          else
            Canvas.Draw(R.Left, R.Top, bmp);
        finally
          bmp.Free;
        end;
      end
      else
      begin
        if UseOldDrawing then
          DrawVistaGradient(Canvas, R,GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor,
           GradU, GradB, '', {Canvas.}Font, Layout, Enabled, False, FAntiAlias, {True}TabRoundEdges, RotateLR, AdvPages[PageIndex].Progress, False, MultiLine, TabPos)
        else
        begin
          R3 := R;
          case TabPosition of
            tpTop: R3.Bottom := R3.Bottom - 3;
            tpBottom: R3.Top := R3.Top + 2;
            tpLeft: R3.Right := R3.Right - 3;
            tpRight: R3.Left := R3.Left + 2;
          end;

          DrawVistaTab(Canvas, R3, GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo, PenColor, GradU, GradB,
            Enabled, Shape, False, TabSettings.Rounding, RotateTabLeftRight, TabPosition, AdvPages[PageIndex].Progress);
        end;
      end;

      if UseOldDrawing then
      begin
        case TabPosition of
          tpTop:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Left, R.Top] := c1; //Canvas.Pixels[R.Left - 1, R.Top - 1];
              Canvas.Pixels[R.Left + 1, R.Top] := c1; //Canvas.Pixels[R.Left + 1, R.Top - 1];
              Canvas.Pixels[R.Left, R.Top + 1] := c1; //Canvas.Pixels[R.Left - 1, R.Top];

              Canvas.Pixels[R.Right - 1, R.Top] := c2; //Canvas.Pixels[R.Right + 1, R.Top];
              Canvas.Pixels[R.Right - 2, R.Top] := c2; //Canvas.Pixels[R.Right + 1, R.Top];
              Canvas.Pixels[R.Right - 1, R.Top + 1] := c2; //Canvas.Pixels[R.Right + 1, R.Top];
            end;

            //--- Draw 3D effect
            if not AdvPages[PageIndex].Progress.Visible and CanShow3D then
            begin
              if not Assigned(AdvPages[PageIndex].FTimer) then
              begin
                if SelectedHot then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Top + 3);
                Canvas.LineTo(R.Right-3, R.Top+3);
              end
              else
              begin
                if (AdvPages[PageIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20) //Min(20, AdvPages[PageIndex].FStepHover))
                else
                begin
                  if SelectedHot then
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                  else
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                end;
                Canvas.MoveTo(R.Left+3, R.Top + 3);
                Canvas.LineTo(R.Right-3, R.Top+3);
              end;
            end;

            //-- Draw Shadow
            if (TabAppearance.ShadowColor <> clNone) and not IsGlass and CanShowShadow then
            begin
              Canvas.Pen.Color := TabAppearance.ShadowColor;
              Canvas.MoveTo(R.Right, R.Top + 3);
              Canvas.LineTo(R.Right, R.Bottom-4);
              Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
              Canvas.MoveTo(R.Right + 1, R.Top + 4);
              Canvas.LineTo(R.Right + 1, R.Bottom-4);
            end;

            if CanShow3D and (HighLightClr <> clNone) and not (AdvPages[PageIndex].Progress.Visible and ShowNonSelectedTabs and (PageIndex <> ActivePageIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Left+3, R.Top+1);
              Canvas.LineTo(R.Right-3, R.Top+1);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficePagerStyler.BlendFactor);
              Canvas.MoveTo(R.Left+1, R.Top + 3);
              Canvas.LineTo(R.Left+1, R.Bottom-5);
              Canvas.MoveTo(R.Right-2, R.Top + 3);
              Canvas.LineTo(R.Right-2, R.Bottom-5);
            end;
          end;
          tpBottom:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Left, R.Bottom-2] := c1; //Canvas.Pixels[R.Left - 1, R.Bottom - 1];
              Canvas.Pixels[R.Left + 1, R.Bottom-1] := c1; //Canvas.Pixels[R.Left - 1, R.Bottom - 1];
              Canvas.Pixels[R.Left, R.Bottom - 1] := c1; //Canvas.Pixels[R.Left - 1, R.Bottom -1];

              Canvas.Pixels[R.Right - 1, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right - 2, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right - 1, R.Bottom - 2] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
            end;

            //--- Draw 3D effect
            if not AdvPages[PageIndex].Progress.Visible and CanShow3D then
            begin
              if not Assigned(AdvPages[PageIndex].FTimer) then
              begin
                if SelectedHot then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                Canvas.MoveTo(R.Left+3, R.Bottom - 3);
                Canvas.LineTo(R.Right-3, R.Bottom - 3);
              end
              else
              begin
                if (AdvPages[PageIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20) //Min(20, AdvPages[PageIndex].FStepHover))
                else
                begin
                  if SelectedHot then
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                  else
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                end;
                Canvas.MoveTo(R.Left+3, R.Bottom - 3);
                Canvas.LineTo(R.Right-3, R.Bottom - 3);
              end;
            end;

            if (TabAppearance.ShadowColor <> clNone) and not IsGlass and CanShowShadow then
            begin
              Canvas.Pen.Color := TabAppearance.ShadowColor;
              Canvas.MoveTo(R.Right, R.Top + 4);
              Canvas.LineTo(R.Right, R.Bottom-2);
              Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
              Canvas.MoveTo(R.Right + 1, R.Top + 4);
              Canvas.LineTo(R.Right + 1, R.Bottom-3);
            end;

            if CanShow3D and (HighLightClr <> clNone) and not (AdvPages[PageIndex].Progress.Visible and ShowNonSelectedTabs and (PageIndex <> ActivePageIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Left+3, R.Bottom -2);
              Canvas.LineTo(R.Right-3, R.Bottom -2);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficePagerStyler.BlendFactor);
              Canvas.MoveTo(R.Left+1, R.Bottom - 3);
              Canvas.LineTo(R.Left+1, R.Top + 5);
              Canvas.MoveTo(R.Right-2, R.Bottom - 3);
              Canvas.LineTo(R.Right-2, R.Top + 5);
            end;
          end;
          tpLeft:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Left, R.Top] := c1; //Canvas.Pixels[R.Left - 1, R.Top - 1];
              Canvas.Pixels[R.Left + 1, R.Top] := c1; //Canvas.Pixels[R.Left + 1, R.Top - 1];
              Canvas.Pixels[R.Left, R.Top + 1] := c1; //Canvas.Pixels[R.Left - 1, R.Top];

              Canvas.Pixels[R.Left, R.Bottom-1] := c2; //Canvas.Pixels[R.Left - 1, R.Bottom];
              Canvas.Pixels[R.Left + 1, R.Bottom-1] := c2; //Canvas.Pixels[R.Left - 1, R.Bottom];
              Canvas.Pixels[R.Left, R.Bottom - 2] := c2; //Canvas.Pixels[R.Left - 1, R.Bottom];
            end;

            //--- Draw 3D effect
            if not AdvPages[PageIndex].Progress.Visible and CanShow3D then
            begin
              if not Assigned(AdvPages[PageIndex].FTimer) then
              begin
                if SelectedHot then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);

                if RotateTabLeftRight then
                begin
                  Canvas.MoveTo(R.Left+3, R.Top + 3);
                  Canvas.LineTo(R.Left+3, R.Bottom - 3);
                end
                else
                begin
                  Canvas.MoveTo(R.Left+3, R.Top + 3);
                  Canvas.LineTo(R.Right-5, R.Top + 3);
                end;
              end
              else
              begin
                if (AdvPages[PageIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20) //Min(20, AdvPages[PageIndex].FStepHover))
                else
                begin
                  if SelectedHot then
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                  else
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                end;

                if RotateTabLeftRight then
                begin
                  Canvas.MoveTo(R.Left+3, R.Top + 3);
                  Canvas.LineTo(R.Left+3, R.Bottom - 3);
                end
                else
                begin
                  Canvas.MoveTo(R.Left+3, R.Top + 3);
                  Canvas.LineTo(R.Right-5, R.Top + 3);
                end
              end;
            end;

            //--- Draw Shadow
            if CanShowShadow then
            begin
              if RotateTabLeftRight then
              begin
                if (TabAppearance.ShadowColor <> clNone) and not IsGlass then
                begin
                  Canvas.Pen.Color := TabAppearance.ShadowColor;
                  Canvas.MoveTo(R.Left + 3, R.Top - 1);
                  Canvas.LineTo(R.Right, R.Top - 1);
                  Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
                  Canvas.MoveTo(R.Left + 4, R.Top - 2);
                  Canvas.LineTo(R.Right, R.Top - 2);
                end;
              end
              else
              begin
                if (TabAppearance.ShadowColor <> clNone) and not IsGlass then
                begin
                  Canvas.Pen.Color := TabAppearance.ShadowColor;
                  Canvas.MoveTo(R.Left + 3, R.Bottom);
                  Canvas.LineTo(R.Right, R.Bottom);
                  Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
                  Canvas.MoveTo(R.Left + 4, R.Bottom + 1);
                  Canvas.LineTo(R.Right, R.Bottom + 1);
                end;
              end;
            end;

            if CanShow3D and (HighLightClr <> clNone) and not (AdvPages[PageIndex].Progress.Visible and ShowNonSelectedTabs and (PageIndex <> ActivePageIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Left+1, R.Top+3);
              Canvas.LineTo(R.Left+1, R.Bottom-3);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficePagerStyler.BlendFactor);
              Canvas.MoveTo(R.Left+3, R.Top + 1);
              Canvas.LineTo(R.Right-5, R.Top+1);
              Canvas.MoveTo(R.Left+3, R.Bottom-2);
              Canvas.LineTo(R.Right-5, R.Bottom-2);
            end;
          end;
          tpRight:
          begin
            if {True}TabRoundEdges then
            begin
              Canvas.Pixels[R.Right, R.Top] := c1; //Canvas.Pixels[R.Right + 1, R.Top - 1];
              Canvas.Pixels[R.Right - 1, R.Top] := c1; //Canvas.Pixels[R.Right + 1, R.Top - 1];
              Canvas.Pixels[R.Right, R.Top + 1] := c1; //Canvas.Pixels[R.Right + 1, R.Top];

              Canvas.Pixels[R.Right, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right - 1, R.Bottom-1] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];
              Canvas.Pixels[R.Right, R.Bottom - 2] := c2; //Canvas.Pixels[R.Right + 1, R.Bottom];

              Canvas.Pixels[R.Right - 1, R.Top] := c3;
              Canvas.Pixels[R.Right - 1, R.Bottom-1] := c4;
            end;

            //--- Draw 3D effect
            if not AdvPages[PageIndex].Progress.Visible and CanShow3D then
            begin
              if not Assigned(AdvPages[PageIndex].FTimer) then
              begin
                if SelectedHot then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                else
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);

                if RotateTabLeftRight then
                begin
                  Canvas.MoveTo(R.Right-3, R.Top + 3);
                  Canvas.LineTo(R.Right-3, R.Bottom - 3);
                end
                else
                begin
                  Canvas.MoveTo(R.Left+5, R.Top + 3);
                  Canvas.LineTo(R.Right-3, R.Top + 3);
                end;
              end
              else
              begin
                if (AdvPages[PageIndex].FGlowState = gsHover) then
                  Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20) //Min(20, AdvPages[PageIndex].FStepHover))
                else
                begin
                  if SelectedHot then
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 20)
                  else
                    Canvas.Pen.Color := BlendColor(clWhite, GradColor, 50);
                end;

                if RotateTabLeftRight then
                begin
                  Canvas.MoveTo(R.Right-3, R.Top + 3);
                  Canvas.LineTo(R.Right-3, R.Bottom - 3);
                end
                else
                begin
                  Canvas.MoveTo(R.Left+5, R.Top + 3);
                  Canvas.LineTo(R.Right-3, R.Top + 3);
                end;
              end;
            end;

            //-- Draw Shadow
            if (TabAppearance.ShadowColor <> clNone) and not IsGlass and CanShowShadow then
            begin
              Canvas.Pen.Color := TabAppearance.ShadowColor;
              Canvas.MoveTo(R.Left + 3, R.Bottom);
              Canvas.LineTo(R.Right - 3, R.Bottom);
              Canvas.Pen.Color := BlendColor(TabAppearance.ShadowColor, TabAppearance.BackGround.Color, 40);
              Canvas.MoveTo(R.Left + 4, R.Bottom + 1);
              Canvas.LineTo(R.Right - 4, R.Bottom + 1);
            end;

            if CanShow3D and (HighLightClr <> clNone) and not (AdvPages[PageIndex].Progress.Visible and ShowNonSelectedTabs and (PageIndex <> ActivePageIndex)) then
            begin
              Canvas.Pen.Color := BlendColor(GradColor, PenColor, 80);
              Canvas.MoveTo(R.Right-2, R.Top+3);
              Canvas.LineTo(R.Right-2, R.Bottom-3);
              Canvas.Pen.Color := HighLightClr; //BlendColor(GradColor, PenColor, FCurrentOfficePagerStyler.BlendFactor);
              Canvas.MoveTo(R.Left+5, R.Top + 1);
              Canvas.LineTo(R.Right-3, R.Top+1);
              Canvas.MoveTo(R.Left+5, R.Bottom-2);
              Canvas.LineTo(R.Right-3, R.Bottom-2);
            end;
          end;
        end;
      end;
    end;

    if AdvPages[PageIndex].ShowCheckBox then
    begin
      ThemeStyle := 0;
      DChecked := 0;
      if AdvPages[PageIndex].Checked then
      begin
        DChecked := DFCS_BUTTONCHECK or DFCS_CHECKED;
        if AdvPages[PageIndex].Enabled then
          ThemeStyle := CBS_CHECKEDNORMAL
        else
          ThemeStyle := CBS_CHECKEDDISABLED;

        if not AdvPages[PageIndex].Enabled then
          DChecked := DChecked or DFCS_INACTIVE;
      end;

      rc := GetCheckBoxRect(PageIndex);

      if FIsWinXP and IsThemeActive and not (csDesigning in ComponentState) then
      begin
        htheme := OpenThemeData(Self.Handle,'button');
        ACXPVS.DrawThemeBackground(HTheme, Canvas.Handle, BP_CHECKBOX,ThemeStyle,@rc,nil);
        CloseThemeData(htheme);
      end
      else
        DrawFrameControl(Canvas.Handle,rc,DFC_BUTTON, DChecked);
    end;

    // badge
    if (AdvPages[PageIndex].Badge <> '') then
    begin
      gpgraphics := TGPGraphics.Create(Canvas.Handle);
      gpgraphics.SetSmoothingMode(SmoothingModeAntiAlias);
      gpbrush := TGPSolidBrush.Create(ColorToARGB(AdvPages[PageIndex].BadgeColor));

      case TabPosition of
      tpTop: badgexy := Point(r.Right - 8, r.Top - 2);
      tpLeft: badgexy := Point(r.Left, r.Top - 2);
      tpRight: badgexy := Point(r.Right - 14, r.Bottom  - 16);
      tpBottom: badgexy := Point(r.Right - 8, r.Top + 2);
      end;

      gpgraphics.FillEllipse(gpbrush, badgexy.X, badgexy.Y, 14,14);

      gpbrush.Free;
      gpgraphics.Free;

      Canvas.Font.Color := AdvPages[PageIndex].BadgeTextColor;
      Canvas.Font.Size := 8;
      Canvas.Brush.Style := bsClear;
      BR := Rect(badgexy.x, badgexy.y, badgexy.X + 14, badgexy.y + 14);
      DrawText(Canvas.Handle,PChar(AdvPages[PageIndex].Badge),Length(AdvPages[PageIndex].Badge),BR, DT_CENTER or DT_SINGLELINE);
    end;

    AdvPages[PageIndex].TxtColor := TxtClr;

    if Assigned(FOnDrawTab) then
    begin
      FOnDrawTab(Self, PageIndex, R);
      Exit;
    end;

    DrawTabContent(PageIndex, R);
  end;
end;

procedure TAdvOfficePager.DrawTabContent(PageIndex: integer; R: TRect);
var
  cbr, TR, CapR, CapR2, TxtR: TRect;
  ImgP: TPoint;
  Pic: TGDIPPicture;
  ImgW, ImgH, ImgX, ImgY, FS: Integer;
  ImgTxtSp: integer;
  ImgList: TCustomImageList;
  ImgEnabled: boolean;
  AAlign: TAlignment;
  DCaption: string;
  WCaption: widestring;
  TxtClr: TColor;
  Ellipsis: boolean;
  TabAppearance: TCustomTabAppearance;
  tf: TFont;
  lf: TLogFont;
  R2: TRect;
  aa: TAntiAlias;
begin
  ImgTxtSp := IMG_SPACE;
  ImgY := 0;
  ImgX := 0;
  ImgH := 0;
  ImgW := 0;
  ImgList := nil;
  ImgEnabled := true;

  Ellipsis := (TabSettings.Width > 0) and not TabSettings.WordWrap;

  if AdvPages[PageIndex].UseTabAppearance then
    TabAppearance := AdvPages[PageIndex].TabAppearance
  else if Assigned(FTabAppearance) and FItones then
    TabAppearance := FTabAppearance
  else
    TabAppearance := FCurrentOfficePagerStyler.TabAppearance;

  TxtClr := AdvPages[PageIndex].TxtColor;
  DCaption := AdvPages[PageIndex].Caption;
  WCaption := AdvPages[PageIndex].WideCaption;

  Canvas.Font.Assign(TabAppearance.Font);
  Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

  Canvas.Font.Color := TxtClr;

  if IsGlass then
    aa := aaAntiAlias
  else
    aa := FAntiAlias;

    if not UseOldDrawing then
    begin
      if TabPosition in [tpTop, tpBottom] then
      begin
        R.Left := R.Left + GetLeftRoundingOffset;
      end
      else if RotateTabLeftRight then
      begin
        if TabPosition = tpLeft then
          R.Bottom := R.Bottom - GetLeftRoundingOffset
        else
        begin
          if TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp] then
            R.Top := R.Top + GetLeftRoundingOffset;
        end;
      end;
    end;

    if (TabSettings.Width > 0) then
      GetCloseBtnImageAndTextRect(PageIndex, cbr, TR, ImgP);


    case TabPosition of
      tpTop, tpBottom:
      begin
        CapR := Rect(R.Left + FTabSettings.LeftMargin, R.Top, R.Right, R.Bottom );

        if not ShowCloseOnNonSelectedTabs then
        begin
          if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex <> PageIndex) and AdvPages[PageIndex].ShowClose) then
            CapR.Left := CapR.Left + (ButtonSettings.ButtonSize + 4 ) div 2
          else if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
            CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 4;
        end
        else
        begin
//          if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex <> PageIndex) and AdvPages[PageIndex].ShowClose) then
//            CapR.Left := CapR.Left + (PAGEBUTTON_SIZE + 4 ) div 2
          if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
            CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 4;
        end;

        if AdvPages[PageIndex].ShowCheckBox then
          Capr.Left := Capr.Left + 20;
      end;
      tpLeft:
      begin
        if RotateTabLeftRight then
        begin
          CapR := Rect(R.Left, R.Top, R.Right, R.Bottom - FTabSettings.LeftMargin);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex <> PageIndex) and AdvPages[PageIndex].ShowClose) then
              CapR.Bottom := CapR.Bottom - (ButtonSettings.ButtonSize + 4) div 2
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
              CapR.Bottom := CapR.Bottom - ButtonSettings.ButtonSize - 4;
          end
          else
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
              CapR.Bottom := CapR.Bottom - ButtonSettings.ButtonSize - 4;
          end;

          if AdvPages[PageIndex].ShowCheckBox then
            Capr.Bottom := Capr.Bottom - 25;
        end
        else
        begin
          CapR := Rect(R.Left + FTabSettings.LeftMargin, R.Top, R.Right, R.Bottom);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
              CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 4
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4);
          end
          else
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4);
          end;

          if AdvPages[PageIndex].ShowCheckBox then
            Capr.Left := Capr.Left + 20;
        end;
      end;
      tpRight:
      begin
        if RotateTabLeftRight then
        begin
          CapR := Rect(R.Left, R.Top + FTabSettings.LeftMargin, R.Right, R.Bottom);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex <> PageIndex) and AdvPages[PageIndex].ShowClose) then
              CapR.Top := CapR.Top + (ButtonSettings.ButtonSize + 4) div 2
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
              CapR.Top := CapR.Top + ButtonSettings.ButtonSize + 4;
          end
          else
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
              CapR.Top := CapR.Top + ButtonSettings.ButtonSize + 4;
          end;

          if AdvPages[PageIndex].ShowCheckBox then
            Capr.Top := Capr.Top + 20;
        end
        else
        begin
          CapR := Rect(R.Left + FTabSettings.LeftMargin + 3, R.Top, R.Right, R.Bottom);
          if not ShowCloseOnNonSelectedTabs then
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpLeft) and (TabSettings.Alignment <> taCenter) then
              CapR.Left := CapR.Left + ButtonSettings.ButtonSize + 5
            else if (ButtonSettings.CloseButton and CloseOnTab and (ActivePageIndex = PageIndex) and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4);
          end
          else
          begin
            if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpRight) and ((TabSettings.Width <= 0) or (TabSettings.Alignment <> taCenter)) then
              CapR.Right := CapR.Right - (ButtonSettings.ButtonSize + 4);
          end;

          if AdvPages[PageIndex].ShowCheckBox then
            Capr.Left := Capr.Left + 20;
        end;
      end;
    end;

    if AdvPages[PageIndex].Enabled or AdvPages[PageIndex].DisabledPicture.Empty then
      Pic := AdvPages[PageIndex].Picture
    else
      Pic := AdvPages[PageIndex].DisabledPicture;

    if Assigned(Pic) and not Pic.Empty then
    begin
      Pic.GetImageSizes;
      ImgW := Pic.Width;
      ImgH := Pic.Height;

      ImgY := CapR.Top;
      ImgX := CapR.Left;
      case TabPosition of
        tpTop, tpBottom:
        begin
          case TabSettings.ImagePosition of
            ipTop:
            begin
              ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
              ImgY := CapR.Top + TabSettings.StartMargin;
              CapR.Top := CapR.Top + ImgH{ + ImgTxtSp};
            end;
            ipBottom:
            begin
              ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
              ImgY := CapR.Bottom - ImgH - TabSettings.StartMargin;
              CapR.Bottom := CapR.Bottom - ImgH;
            end;
            ipLeft:
            begin
              if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                ImgX := ImgP.X
              else
                ImgX := CapR.Left + 2;
              CapR.Left := CapR.Left + ImgW + ImgTxtSp;

              ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
            end;
            ipRight:
            begin
              if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                ImgX := ImgP.X
              else
                ImgX := CapR.Right - ImgW - FTabSettings.RightMargin;
              CapR.Right := ImgX - ImgTxtSp;
              if (ButtonSettings.CloseButton and CloseOnTab and
                 ((ActivePageIndex = PageIndex) or ShowCloseOnNonSelectedTabs) and
                 AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpRight) and not ((TabSettings.Width > 0) and (TabSettings.Alignment = taCenter)) then
                ImgX := ImgX - ButtonSettings.ButtonSize;

              if AdvPages[PageIndex].ShowCheckBox then
                ImgX := ImgX - 25;

              ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
            end;
          end;
        end;
        tpLeft:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgH;
              end;
              ipBottom:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Bottom - ImgH;
                CapR.Bottom := CapR.Bottom - ImgH;
              end;
              ipLeft:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                  ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW + ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
              ipRight:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                ImgX := CapR.Right - ImgW - FTabSettings.RightMargin;
                CapR.Right := ImgX - ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW;
              end;
              ipBottom:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Right - ImgW;
                CapR.Right := CapR.Right - ImgW;
              end;
              ipLeft:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                  ImgY := CapR.Bottom - ImgH;
                CapR.Bottom := ImgY - ImgTxtSp;
              end;
              ipRight:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                  ImgY := CapR.Top + TabSettings.RightMargin;
                CapR.Top := ImgY + ImgTxtSp;
              end;
            end;
          end;
        end;
        tpRight:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgH;
              end;
              ipBottom:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                ImgY := CapR.Bottom - ImgH;
                CapR.Bottom := CapR.Bottom - ImgH;
              end;
              ipLeft:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                  ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW + ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
              ipRight:
              begin
                ImgX := CapR.Right - ImgW;
                CapR.Right := ImgX - ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
              end;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Right - ImgW;
                CapR.Right := CapR.Right - ImgW;
              end;
              ipBottom:
              begin
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgH) div 2;
                ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgW;
              end;
              ipLeft:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                  ImgY := CapR.Top;
                CapR.Top := CapR.Top + ImgH + ImgTxtSp;
              end;
              ipRight:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgW) div 2;
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgY := ImgP.Y
                else
                  ImgY := CapR.Bottom - ImgH - TabSettings.RightMargin;
                CapR.Bottom := ImgY - ImgTxtSp;
              end;
            end;
          end;
        end;
      end;
      //Canvas.Draw(ImgX, ImgY, Pic);
    end
    else
    if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[PageIndex].ImageIndex >= 0) then
    begin

      if AdvPages[PageIndex].Enabled then
      begin
        if Assigned(FImages) then
          ImgList := FImages;

        ImgEnabled := AdvPages[PageIndex].TabEnabled;
      end
      else
      begin
        if Assigned(FDisabledImages) then
          ImgList := FDisabledImages
        else if Assigned(FImages) then
        begin
          ImgList := FImages;
          ImgEnabled := False;
        end;
      end;

      if (ImgList <> nil) then
      begin
        ImgY := CapR.Top;
        ImgX := CapR.Left;

        case TabPosition of
          tpTop, tpBottom:
          begin
            case TabSettings.ImagePosition of
              ipTop:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                ImgY := CapR.Top + TabSettings.StartMargin;
                CapR.Top := CapR.Top + ImgList.Height{ + ImgTxtSp};
              end;
              ipBottom:
              begin
                ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                ImgY := CapR.Bottom - ImgList.Height - TabSettings.StartMargin;
                CapR.Bottom := CapR.Bottom - ImgList.Height;
              end;
              ipLeft:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                  ImgX := CapR.Left;
                CapR.Left := CapR.Left + ImgList.Width + ImgTxtSp;
                ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
              end;
              ipRight:
              begin
                if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                  ImgX := ImgP.X
                else
                  ImgX := CapR.Right - ImgList.Width - FTabSettings.RightMargin;
                CapR.Right := ImgX - ImgTxtSp;
                if (ButtonSettings.CloseButton and CloseOnTab and ((ActivePageIndex = PageIndex) or ShowCloseOnNonSelectedTabs) and
                    AdvPages[PageIndex].ShowClose) and (CloseOnTabPosition = cpRight) and not ((TabSettings.Width > 0) and (TabSettings.Alignment = taCenter)) then
                  ImgX := ImgX - ButtonSettings.ButtonSize;

                if AdvPages[PageIndex].ShowCheckBox then
                  ImgX := ImgX - 25;

                ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
              end;
            end;
          end;
          tpLeft:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Top;
                  CapR.Top := CapR.Top + ImgList.Height{ + ImgTxtSp};
                end;
                ipBottom:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;

                  FS := CapR.Bottom - CapR.Top - Canvas.TextHeight('gh') - ImgList.Height;

                  ImgY := CapR.Bottom - ImgList.Height - FS div 2;
                  CapR.Bottom := CapR.Bottom - ImgList.Height - 4;
                end;
                ipLeft:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                    ImgX := CapR.Left + 2;
                  CapR.Left := CapR.Left + ImgList.Width + ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
                ipRight:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                    ImgX := CapR.Right - ImgList.Width - FTabSettings.RightMargin;
                  CapR.Right := ImgX - ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width;
                end;
                ipBottom:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Right - ImgList.Width;
                  CapR.Right := CapR.Right - ImgList.Width;
                end;
                ipLeft:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                    ImgY := CapR.Bottom - ImgList.Height;
                  CapR.Bottom := ImgY - ImgTxtSp;
                end;
                ipRight:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                    ImgY := CapR.Top + TabSettings.RightMargin;
                  CapR.Top := ImgY + ImgTxtSp;
                end;
              end;
            end;
          end;
          tpRight:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Top;
                  CapR.Top := CapR.Top + ImgList.Height;
                end;
                ipBottom:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  ImgY := CapR.Bottom - ImgList.Height;
                  CapR.Bottom := CapR.Bottom - ImgList.Height;
                end;
                ipLeft:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                    ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width + ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
                ipRight:
                begin
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgX := ImgP.X
                  else
                    ImgX := CapR.Right - ImgList.Width;
                  CapR.Right := ImgX - ImgTxtSp;
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                end;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Right - ImgList.Width;
                  CapR.Right := CapR.Right - ImgList.Width;
                end;
                ipBottom:
                begin
                  ImgY := R.Top + ((R.Bottom - R.Top) - ImgList.Height) div 2;
                  ImgX := CapR.Left;
                  CapR.Left := CapR.Left + ImgList.Width;
                end;
                ipLeft:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                    ImgY := CapR.Top;
                  CapR.Top := CapR.Top + ImgList.Height + ImgTxtSp;
                end;
                ipRight:
                begin
                  ImgX := R.Left + ((R.Right - R.Left) - ImgList.Width) div 2;
                  if (TabSettings.Width > 0) and (TabSettings.Alignment = taCenter) then
                    ImgY := ImgP.Y
                  else
                    ImgY := CapR.Bottom - ImgList.Height - TabSettings.RightMargin;
                  CapR.Bottom := ImgY - ImgTxtSp;
                end;
              end;
            end;
          end;
        end;

        if IsGlass then
          DrawGDIPImageFromImageList(nil, Canvas, Point(ImgX, ImgY), ImgList, AdvPages[PageIndex].ImageIndex, ImgEnabled)
        else
          ImgList.Draw(Canvas, ImgX, ImgY, AdvPages[PageIndex].ImageIndex, ImgEnabled);
        ImgList := nil;
      end;


    end;

    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := TxtClr;
    if (not RotateTabLeftRight and (TabPosition in [tpLeft, tpRight])) then
    begin
      CapR2 := CapR;
      AAlign := taLeftJustify;
      if (TabSettings.Width <> 0) then
      begin
        case TabSettings.Alignment of
          taLeftJustify:
          begin
            AAlign := taLeftJustify;
          end;
          taCenter:
          begin
            AAlign := taCenter;
          end;
          taRightJustify:
          begin
            AAlign := taRightJustify;
            CapR2.Right := CapR.Right - 3;
          end;
        end;
      end
      else
      begin
        if TabSettings.ImagePosition in [ipTop,ipBottom] then
        begin
          AAlign := taCenter;
          if (TabPosition = tpLeft) then
          begin
            CapR2.Left := CapR2.Left - FTabSettings.LeftMargin;
            CapR2.Right := CapR2.Right - 3;
          end;
        end
        else
          AAlign := taLeftJustify;
      end;

      TxtR := DrawVistaText(Canvas, AAlign, CapR2, DCaption, WCaption, Canvas.Font, AdvPages[PageIndex].Enabled, True, aa, tpTop, Ellipsis, TabSettings.WordWrap);
    end
    else
    if (TabPosition = tpLeft) then
    begin
      if (DCaption <> '') or (WCaption <> '') then
      begin
        CapR2 := CapR;
        TxtR.Left := CapR.Left + ((CapR.Right - CapR.Left) - Canvas.TextHeight('gh')) div 2;
        if (TabSettings.Width <> 0) then
        begin
          case TabSettings.Alignment of
            taCenter: CapR2.Bottom := TR.Bottom;
            taRightJustify: CapR2.Bottom := TR.Bottom;
          end;
        end;

        TxtR.Top := CapR.Bottom;
        TxtR.Right := TxtR.Left + Canvas.TextHeight('gh');
        if (DCaption <> '') then
          TxtR.Bottom := TxtR.Top + Canvas.TextWidth(DCaption)
        else
          TxtR.Bottom := TxtR.Top + WideCanvasTextWidth(Canvas, WCaption);

        // Make sure to use a truetype font!
        // Font.Name := 'Tahoma';

        tf := TFont.Create;
        try
          if (TabPosition = tpLeft) or (TabPosition = tpRight) then
          begin
            FillChar(lf, SizeOf(lf), 0);
            tf.Assign(Canvas.Font);
            GetObject(tf.Handle, SizeOf(Lf), @Lf);

            if TabPosition = tpLeft then lf.lfEscapement := -2700
            else lf.lfEscapement := -900;
            lf.lfOrientation := 30;

            tf.Handle := CreateFontIndirect(Lf);
            Canvas.Font.Assign(tf);
          end;
        finally
          tf.Free;
        end;
        if (DCaption <> '') then
        begin
          DCaption := TrimText(DCaption, CapR, False, nil, Canvas, nil, nil, Ellipsis, TabPosition, TabSettings.WordWrap);
          Canvas.TextOut(CapR.Left + ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2, CapR2.Bottom, DCaption);
        end
        else
        begin
          WCaption := TrimTextW(WCaption, CapR, False, nil, Canvas, nil, nil, Ellipsis, TabPosition, TabSettings.WordWrap);
          TextOutW(Canvas.Handle, CapR.Left + ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2, CapR2.Bottom, PWideChar(WCaption), Length(WCaption));
        end;
      end;
    end
    else
    if (TabPosition = tpRight) and ((AntiAlias = aaNone) or (TabSettings.Width > 0)) then
    begin
      if (DCaption <> '') or (WCaption <> '') then
      begin
        CapR2 := CapR;
        TxtR.Left := CapR.Left + ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2;
        if (TabSettings.Width <> 0) then
        begin
          case TabSettings.Alignment of
            taCenter: CapR2.Top := TR.Top;
            taRightJustify: CapR2.Top := TR.Top;
          end;
        end;

        TxtR.Top := CapR.Bottom;
        TxtR.Right := TxtR.Left + Canvas.TextHeight('gh');
        if (DCaption <> '') then
          TxtR.Bottom := TxtR.Top + Canvas.TextWidth(DCaption)
        else
          TxtR.Bottom := TxtR.Top + WideCanvasTextWidth(Canvas, WCaption);

        // Make sure to use a truetype font!
        // Font.Name := 'Tahoma';

        tf := TFont.Create;
        try
          if (TabPosition = tpLeft) or (TabPosition = tpRight) then
          begin
            FillChar(lf, SizeOf(lf), 0);
            tf.Assign(Canvas.Font);

            GetObject(tf.Handle, SizeOf(Lf), @Lf);

            if TabPosition = tpLeft then lf.lfEscapement := -900
            else lf.lfEscapement := -900;
            lf.lfOrientation := 30;

            tf.Handle := CreateFontIndirect(Lf);
            Canvas.Font.Assign(tf);
          end;
        finally
          tf.Free;
        end;

        if (DCaption <> '') then
        begin
          DCaption := TrimText(DCaption, CapR, False, nil, Canvas, nil, nil, Ellipsis, TabPosition, TabSettings.WordWrap);
          Canvas.TextOut(CapR.Right - ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2, CapR2.Top, DCaption);
        end
        else
        begin
          WCaption := TrimTextW(WCaption, CapR, False, nil, Canvas, nil, nil, Ellipsis, TabPosition, TabSettings.WordWrap);
          TextOutW(Canvas.Handle, CapR.Right - ((CapR.Right - CapR.Left)-Canvas.TextHeight('gh')) div 2, CapR2.Top, PWideChar(WCaption), Length(WCaption));
        end;
      end;
    end
    else
    begin
      CapR2 := CapR;

      AAlign := taLeftJustify;

      if (TabSettings.Width <> 0) then
      begin
        case TabSettings.Alignment of
          taLeftJustify:
          begin
            AAlign := taLeftJustify;
          end;
          taCenter:
          begin
            AAlign := taCenter;
           // CapR2.Left := TR.Left;
          end;
          taRightJustify:
          begin
            AAlign := taRightJustify;
            //CapR2.Right := CapR2.Right - 3;
            if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
              CapR2.Right := CapR2.Right - GetRightRoundingOffset;
          end;
        end;
      end
      else
      begin
        if TabSettings.ImagePosition in [ipTop,ipBottom] then
        begin
          AAlign := taCenter;
        end
        else
        begin
          AAlign := taLeftJustify;
          capr2.Right := capR2.Right + 10;
        end;
      end;

      if CloseOnTab and (TabSettings.Width <> 0) and ((PageIndex = ActivePageIndex) or ShowCloseOnNonSelectedTabs) and AdvPages[PageIndex].ShowClose then
      begin
        if CloseOnTabPosition = cpRight then
          CapR2.Right := CapR2.Right - ButtonSettings.ButtonSize - 4;
      end;

      if (DCaption <> '') then
        DCaption := TrimText(DCaption, CapR2, False, nil, Canvas, nil, nil, Ellipsis, TabPosition, TabSettings.WordWrap)
      else
        WCaption := TrimTextW(WCaption, CapR2, False, nil, Canvas, nil, nil, Ellipsis, TabPosition, TabSettings.WordWrap);

      TxtR := DrawVistaText(Canvas, AAlign, CapR2, DCaption, WCaption, Canvas.Font, AdvPages[PageIndex].Enabled, True, aa, TabPosition, Ellipsis, TabSettings.WordWrap);
    end;

    if Assigned(Pic) and not Pic.Empty then
    begin
      case TabPosition of
        tpTop, tpBottom:
        begin
          case TabSettings.ImagePosition of
            ipTop: ImgY := Max(TxtR.Top - ImgH - ImgTxtSp, 4);
            ipBottom: ImgY := Min(TxtR.Bottom + ImgTxtSp, CapR.Bottom);
          end;
        end;
        tpLeft:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgY := Max(TxtR.Top - ImgH - ImgTxtSp, 4);
              ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
              ipRight:  ImgX := TxtR.Right + ImgTxtSp*2;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgX := Max(TxtR.Left - ImgW - ImgTxtSp, 4);
              ipBottom: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
            end;
          end;
        end;
        tpRight:
        begin
          if not RotateTabLeftRight then
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgY := Max(TxtR.Top - ImgH - ImgTxtSp, 4);
              ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
              ipRight: ImgX := TxtR.Right + ImgTxtSp*2;
            end;
          end
          else
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
              ipBottom: ImgX := Max(TxtR.Left - ImgW - ImgTxtSp, 4);
            end;
          end;
        end;
      end;

      if IsGlass then
        DrawGDIPImage(nil, Canvas, Point(ImgX, ImgY), Pic, True)
      else
        Canvas.Draw(ImgX, ImgY, Pic);
    end
    else
    if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[PageIndex].ImageIndex >= 0) then
    begin
      if (ImgList <> nil) then
      begin
        case TabPosition of
          tpTop, tpBottom:
          begin
            case TabSettings.ImagePosition of
              ipTop: ImgY := Max(TxtR.Top - ImgList.Height - ImgTxtSp, 4);
              ipBottom: ImgY := Min(TxtR.Bottom + ImgTxtSp, CapR.Bottom);
            end;
          end;
          tpLeft:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgY := Max(TxtR.Top - ImgList.Height - ImgTxtSp, 4);
                ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
                ipRight: ImgX := TxtR.Right + ImgTxtSp*2;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgX := Max(TxtR.Left - ImgList.Width - ImgTxtSp, 4);
                ipBottom: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
              end;
            end;
          end;
          tpRight:
          begin
            if not RotateTabLeftRight then
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgY := Max(TxtR.Top - ImgList.Height - ImgTxtSp, 4);
                ipBottom: ImgY := Max(TxtR.Bottom + ImgTxtSp, 4);
                ipRight: ImgX := TxtR.Right + ImgTxtSp*2;
              end;
            end
            else
            begin
              case TabSettings.ImagePosition of
                ipTop: ImgX := Max(TxtR.Right + ImgTxtSp, 4);
                ipBottom: ImgX := Max(TxtR.Left - ImgList.Width - ImgTxtSp, 4);
              end;
            end;
          end;
        end;

        if IsGlass then
          DrawGDIPImageFromImageList(nil, Canvas, Point(ImgX, ImgY), ImgList, AdvPages[PageIndex].ImageIndex, ImgEnabled)
        else
          ImgList.Draw(Canvas, ImgX, ImgY, AdvPages[PageIndex].ImageIndex, true);
      end;
    end;

    if FTabScroller.Visible or (ButtonSettings.PageListButton) or (ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0)) or (ButtonSettings.CloseButton and not CloseOnTab) then
    begin
      if IntersectRect(R2, R, GetButtonsRect) then
      begin
        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect(R2, FButtonsBkg.Canvas, Rect(0, 0, R2.Right - R2.Left, R2.Bottom - R.Top));
      end;
    end;

    if (PageIndex <> ActivePageIndex) and (IsActivePageNeighbour(PageIndex) <> 0) then
      DrawTab(ActivePageIndex);

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DrawAllTabs;
var
  i: Integer;
  R: TRect;
  TabAppearance: TTabAppearance;
begin
  // Draw TabBackGround

  R := GetTabsArea;

  case TabPosition of
    tpTop: R.Bottom := ClientRect.Bottom;
    tpBottom: R.Top := ClientRect.Top;
    tpLeft: R.Right := ClientRect.Right;
    tpRight: R.Left := ClientRect.Left;
  end;

  if Assigned(FTabAppearance) and FItones then
    TabAppearance := FTabAppearance
  else
    TabAppearance := FCurrentOfficePagerStyler.TabAppearance;
  if not FTransparent then
  begin
    with TabAppearance do
    begin
      if (BackGround.Color <> clNone) and (BackGround.ColorTo <> clNone) then
        DrawGradient(Canvas, BackGround.Color, BackGround.ColorTo, BackGround.Steps, R, BackGround.Direction = gdHorizontal)
      else if (BackGround.Color <> clNone) then
      begin
        Canvas.Brush.Color := BackGround.Color;
        Canvas.Pen.Color := BackGround.Color;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
    end;
  end;

  if FTabScroller.Visible or (ButtonSettings.PageListButton) or (ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0)) or (ButtonSettings.CloseButton and not CloseOnTab) then
  begin
    R := GetButtonsRect;
    FButtonsBkg.Height := (R.Bottom - R.Top);
    FButtonsBkg.Width := (R.Right - R.Left);
    FButtonsBkg.Canvas.CopyMode := cmSrcCopy;
    FButtonsBkg.Canvas.CopyRect(Rect(0, 0, FButtonsBkg.Width, FButtonsBkg.Height), Canvas, R);
  end;

  for i := 0 to FAdvPages.Count - 1 do
    DrawTab(i);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DrawInsertButton;
begin
  if not ButtonSettings.ShowInsertButton then
    Exit;

  DrawInsertButton(Canvas, GetInsertButtonRect);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DrawInsertButton(Canvas: TCanvas; R: TRect);
var
  g: TGPGraphics;
  btnpath: TGPGraphicsPath;
  Pen: TGPPen;
  brClr, Clr, PBrClr, PClr: Cardinal;
  br: TGPSolidBrush;
  l, w: Integer;
  P1, P2, P3, P4: TPoint;
  Shape: TAdvTabShape;
begin
  if not Assigned(Canvas) then
    Exit;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  if FInsertButtonDown then
  begin
    Clr := MakeColor(25, 10, 10, 10);
    BrClr := MakeColor(170, 50, 79, 83);

    //PClr := MakeColor(230, 168, 188, 199);
    PClr := MakeColor(130, 255, 255, 255);
    PBrClr := MakeColor(180, 52, 87, 120);
  end
  else
  begin
    if FInsertButtonHot then
    begin
      Clr := MakeColor(70, clWhite);

      PClr := MakeColor(230, 217, 226, 231);
      PBrClr := MakeColor(240, 37, 70, 112);
    end
    else
    begin
      Clr := MakeColor(40, clWhite);

      //PClr := MakeColor(230, 196, 209, 216);
      PClr := MakeColor(180, 255, 255, 255);
      PBrClr := MakeColor(200, 52, 83, 121);
    end;
    BrClr := MakeColor(130, 41, 69, 77);
  end;

  if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
    Shape := tsLeftRightRamp
  else
    Shape := tsRectangle;

  case TabPosition of
    tpLeft:
    begin
      if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) then
        Shape := tsLeftRightRamp
      else
        Shape := tsRectangle;
    end;
    tpRight:
    begin
      if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) then
        Shape := tsLeftRightRamp
      else
        Shape := tsRectangle;
    end;
  end;

  //--- body
  btnpath := GetInsertButtonPath(R, Shape, Max(TabSettings.Rounding, 1), RotateTabLeftRight, TabPosition);
  br := TGPSolidBrush.Create(Clr);
  g.FillPath(br, btnPath);
  br.Free;

  //--- border
  Pen := TGPPEn.Create(BrClr, 1.5);
  g.DrawPath(Pen, btnPath);
  Pen.Free;

  //--- Draw Plus sign
  g.SetSmoothingMode(SmoothingModeDefault);
  l := Min(R.Right - R.Left, R.Bottom - R.Top);
  w := Max(l div 4, 4);
  l := l - 6;
  if ((l mod 2) = 1) then
    l := l + 1;

  // -
  P1.X := R.Left + 1 + ((R.Right - R.Left) - l) div 2;
  P1.Y := R.Top + 1 + ((R.Bottom - R.Top)) div 2;
  P2.X := P1.X + l;
  P2.Y := P1.Y;

  // |
  P3.X := R.Left + 1 + (R.Right - R.Left) div 2;
  P3.Y := R.Top + 1 + ((R.Bottom - R.Top) - l) div 2;
  P4.X := P3.X;
  P4.Y := P3.Y + l;

  Pen := TGPPEn.Create(PBrClr, w);
  g.DrawLine(Pen,P1.X, P1.Y, P2.X, P2.Y);    // -
  g.DrawLine(Pen,P3.X, P3.Y, P4.X, P4.Y);    // |
  Pen.Free;

  Pen := TGPPEn.Create(PClr, w-2);
  g.DrawLine(Pen,P1.X+1, P1.Y, P2.X-1, P2.Y);   // -
  g.DrawLine(Pen,P3.X, P3.Y+1, P4.X, P4.Y-1);   // |
  Pen.Free;

  g.Free;
end;

//------------------------------------------------------------------------------


function TAdvOfficePager.PageAtTabIndex(Value: integer): TAdvOfficePage;
var
  i: integer;
begin
  Result := nil;

  for i := 0 to AdvPageCount - 1 do
  begin
    if AdvPages[i].TabIndex = Value then
    begin
      Result := AdvPages[i];
      break;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvOfficePager.Paint;
var
  R: TRect;
  th: integer;
  P: TPoint;
  DC: Integer;
begin
  R := ClientRect;
  if FTransparent then
  begin
    DC := SaveDC(Canvas.Handle);
    P := ClientOrigin;
    Windows.ScreenToClient(Parent.Handle, P);
    P.x := -P.x;
    P.y := -P.y;
    MoveWindowOrg(Canvas.Handle, P.x, P.y);
    SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
    // transparency ?
    SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);
    if (Parent is TWinCtrl) then
      (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);
    RestoreDC(Canvas.Handle, DC);
  end
  else
  begin
    inherited;
  end;

  with FCurrentOfficePagerStyler, Canvas do
  begin
   (* if not BackGround.Empty then
    begin

      case BackGroundDisplay of
        bdTile:
          begin
            c := 1;
            ro := 1;
            while ro < Height - 2 do
            begin
              while c < width - 2 do
              begin
                Draw(c, ro, BackGround);
                c := c + BackGround.Width;
              end;
              c := 1;
              ro := ro + BackGround.Height;
            end;
          end;
        bdCenter:
          begin
            Draw((Width - BackGround.Width) div 2, (Height - BackGround.Height) div 2, BackGround);
          end;
        bdStretch:
          begin
            StretchDraw(Rect(R.Left + 2, R.Top + 2, R.Right - 2, R.Bottom - 2), BackGround);
          end;
      end;
    end;  *)
  end;

  DrawAllTabs;
  DrawInsertButton;
  DrawTabScrollButtons;

  if (csDesigning in ComponentState) and (FAdvPages.Count = 0) then
  begin
    Canvas.Font.Assign(self.Font);
    Canvas.Brush.Style := bsClear;
    th := Canvas.TextHeight('gh');
    Canvas.TextOut(10, Height div 2, 'Right-click and choose "New Page"');
    Canvas.TextOut(10, (Height div 2) + th,'to insert a new tabsheet');
    Canvas.Font.Style := [fsItalic];
    Canvas.TextOut(10, Height div 2 + 3*th, 'If no such right-click menu option appears');
    Canvas.TextOut(10, Height div 2 + 4*th, 'please install designtime package!');
  end;

  //Canvas.Draw(0, 0, FMyImage);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ClearAdvPages;
var
  I: Integer;
  aPage: TAdvOfficePage;
begin
  for I := FAdvPages.Count - 1 downto 0 do
  begin
    aPage := FAdvPages[I];
    RemoveAdvPage(aPage);
    aPage.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.RemoveAdvPage(AdvPage: TAdvOfficePage);
var
  i, j, ni, nii: Integer;
begin
  i := FAdvPages.IndexOf(AdvPage);
  if (i >= 0) then
  begin
    (*
    if i < ActivePageIndex then
      ni := ActivePageIndex - 1
    else
      ni := ActivePageIndex;
    *)

    if (ActivePage = AdvPage) then
    begin
      SelectNextPage(True);
      if ActivePageIndex - 1 >= 0 then
         ni := ActivePageIndex - 1
      else
         ni := ActivePageIndex;
    end else
    begin
       if i < ActivePageIndex then
         ni := ActivePageIndex - 1
       else
         ni := ActivePageIndex;
    end;

    nii := ni;

    if (ActivePage = AdvPage) then
    begin
      SelectNextPage(True);
      nii := ActivePageIndex;
    end;

    j := FClosedPageList.IndexOf(AdvPage.Name);
    if j >= 0 then
      FClosedPageList.Delete(j);

    FAdvPages.Delete(i);
    AdvPage.FAdvOfficePager := nil;

    if Assigned(AdvPage.FCloseButton) then
    begin
      //AdvPage.FCloseButton.Free;
      PostMessage(Self.Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(AdvPage.FCloseButton)), 0);
      AdvPage.FCloseButton := nil;
    end;

    if Assigned(AdvPage.FCloseButtonChrome) then
    begin
      PostMessage(Self.Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(AdvPage.FCloseButtonChrome)), 1);
      AdvPage.FCloseButtonChrome := nil;
    end;

    if GetVisibleTabCount = 0 then
      ActivePageIndex := -1
    else
      if nii < GetVisibleTabCount then
        ActivePageIndex := nii
      else
        ActivePageIndex := ni;

    if not (csDestroying in ComponentState) then
    begin
      InitializeAndUpdateButtons;
      UpdateTabScroller;
      UpdateMultiLineTabs;
    end;

    InvalidateTab(-1);
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate
    else
    begin
      if Assigned(FClonedOfPager) and Assigned(Parent) then
      begin
        if Assigned(AdvPage.FTimer) then
        begin
          AdvPage.FTimer.Enabled := False;
          FreeAndNil(AdvPage.FTimer);
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvOfficePager.ResetPages;
var
  i: integer;
  pg: TAdvOfficePage;
  tbi: integer;
begin
  // add all floating pages back to pager
  if FloatingPageCount > 0 then
  begin
    for i := FloatingPageCount - 1 downto 0 do
    begin
      tbi := FloatingPages[i].TabIndex;
      AddAdvPage(FloatingPages[i]);
      AdvPages[AdvPageCount - 1].TabIndex := tbi;
    end;
  end;

  // unhide hidden tabs
  for i := 0 to AdvPageCount - 1 do
    AdvPages[i].TabVisible := true;

  // reset sequence
  for i := 0 to AdvPageCount - 1 do
  begin
    pg := PageAtTabIndex(i);
    if Assigned(pg) then
      MoveAdvPage(pg.PageIndex, i);
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvOfficePager.SetParent(AParent: TWinControl);
begin
  if (AParent is TAdvOfficePager) then
    raise Exception.Create('Invalid Parent');

  inherited;

  if (not FPropertiesLoaded) and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    Init;
    InitializeAndUpdateButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetOfficePagerStyler(
  const Value: TCustomAdvOfficePagerStyler);
var
  i: integer;
begin
  SetPageValidCache(false);
  if (FOfficePagerStyler <> Value) or (Value = nil) then
  begin
    if Assigned(FOfficePagerStyler) and (FOfficePagerStyler <> FInternalOfficePagerStyler) then
      FOfficePagerStyler.RemoveControl(self);

    FOfficePagerStyler := Value;

    if FOfficePagerStyler = nil then
    begin
      FCurrentOfficePagerStyler := FInternalOfficePagerStyler;
    end
    else
    begin
      FCurrentOfficePagerStyler := FOfficePagerStyler;
      FOfficePagerStyler.AddControl(self);
    end;

    if not (csDestroying in ComponentState) and not (csLoading in ComponentState) then
    begin
      for i := 0 to FAdvPages.Count - 1 do
        AdvPages[i].Invalidate;
    end;

    UpdateMe(0);

    if Assigned(FOfficePagerStyler) then
      FOfficePagerStyler.InitColorTones;

    PopupMenu := PopupMenu;   // Refresh Styler
    SetPageValidCache(false);
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetAdvOfficePageCount: integer;
begin
  Result := FAdvPages.Count;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetAdvOfficePageVisibleCount: integer;
var
  i,j: integer;
begin
  j := 0;
  for i := 0 to AdvPageCount - 1 do
    if AdvPages[i].Visible then inc(j);
  Result := j;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetAdvPages(index: integer): TAdvOfficePage;
begin
  Result := TAdvOfficePage(FAdvPages[index]);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetAdvPageBounds(AdvPage: TAdvOfficePage;
  var ALeft, ATop, AWidth, AHeight: Integer);
begin
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetChildOrder(Child: TComponent; Order: Integer);
begin
  //inherited SetChildOrder(Child, Order);
  TAdvOfficePage(Child).PageIndex := Order;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMSize(var Message: TWMSize);
begin
  inherited;
  SetAllPagesPosition;
  UpdateTabScroller;
  if MultiLine then
    InitializeAndUpdateButtons;
  UpdateMultiLineTabs;
  if ButtonSettings.ClosedListButton then
    UpdateClosedListButton;
  SetPageValidCache(false);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetPopupMenuEx(const Value: TPopupMenu);
begin
  Inherited PopupMenu := Value;
  (*if Assigned(PopupMenu) and (PopupMenu is TAdvPopupMenu) and Assigned(FCurrentOfficePagerStyler) then
    TAdvPopupMenu(PopupMenu).MenuStyler := FCurrentOfficePagerStyler.CurrentAdvMenuStyler; *)
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMShowingChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(ActivePage) then
    ActivePage.Visible := Visible;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetPopupMenuEx: TPopupMenu;
begin
  Result := Inherited PopupMenu;
end;

//------------------------------------------------------------------------------


function VarPos(sub,s:string; var vp: Integer): Integer;
begin
  vp := pos(sub,s);
  Result := vp;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetSettings(const Value: string);
var
  sl: TStringList;
  i,idx,e,vp,k: integer;
  s: string;
  x,y,w,h,pi: integer;
  idf: char;
  il: TList;
  page: TAdvOfficePage;

begin
  // reset default sequence
  ResetPages;
  pi := 0;

  sl := TStringList.Create;
  il := TList.Create;

  try
    sl.CommaText := Value;

    for i := 0 to sl.Count - 1 do
    begin
      s := sl.Strings[i];

      if length(s) > 0 then
      begin
        idf := s[1];
        delete(s,1,2);

        case idf of
        'P': val(s,pi,e);
        'V','H':
          begin
            val(s,idx,e);
            PageAtTabIndex(idx).TabVisible := idf = 'V';
            if PageAtTabIndex(idx).TabVisible then
              il.Add(pointer(idx));
          end;
        'F':
          begin
            val(copy(s,1,pos('[',s)- 1),idx,e);
            delete(s,1,pos('[',s));
            vp := Pos(':',s);
            val(copy(s,1,vp - 1),x,e);
            delete(s,1,vp);
            vp := Pos(':',s);
            val(copy(s,1,vp - 1),y,e);
            delete(s,1,vp);
            vp := Pos(':',s);
            val(copy(s,1,vp - 1),w,e);
            delete(s,1,vp);
            vp := Pos(']',s);
            val(copy(s,1,vp - 1),h,e);

            SetFloatingPage(PageAtTabIndex(idx), x, y, w, h);
          end;
        end;
      end;
    end;

    for i := 0 to il.Count - 1 do
    begin
      k := Integer(il.Items[i]);
      page := PageAtTabIndex(k);
      MoveAdvPage(page.PageIndex,i);
    end;

    il.Clear;

  finally
    il.Free;
    sl.Free;
  end;

  ActivePageIndex := pi;
  //
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetShowCloseOnNonSelectedTabs(const Value: Boolean);
begin
  if FShowCloseOnNonSelectedTabs <> value then
  begin
    FShowCloseOnNonSelectedTabs := Value;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    InvalidateTab(-1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetShowNonSelectedTabs(const Value: Boolean);
begin
  FShowNonSelectedTabs := Value;
  InvalidateTab(-1);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMControlChange(var Message: TCMControlChange);
begin
  inherited;

  with Message do
  begin
    if (Control is TAdvOfficePage) then
    begin
      if Inserting then
        //InsertControl(Control)
      else
        //RemoveControl(Control);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMControlListChange(
  var Message: TCMControlListChange);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMMouseLeave(var Message: TMessage);
var
  P: TPoint;
  R: TRect;
begin
  inherited;

  {if FScrollerHoverLeftBtn or FScrollerHoverRightBtn then
  begin
    FScrollerHoverLeftBtn := false;
    FScrollerHoverRightBtn := false;
    DrawTabScrollButtons;
  end;
  }
  FHintPageIndex := -1;

  if (csDesigning in ComponentState) then
    Exit;

  // work around to avoid false call  
  GetCursorPos(P);
  P := ScreenToClient(P);
  R := GetTabsRect;
  case (TabPosition) of
    tpTop: R.Bottom := R.Bottom - 4;
    tpBottom: R.Top := R.Top + 4;
    tpLeft: R.Right := R.Right - 4;
    tpRight: R.Left := R.Left + 4;
  end;

  if PtInRect(R, P) then
    Exit;

  if (FHotPageIndex = FActivePageIndex) then
  begin
    FHotPageIndex := -1;
    Invalidate;
  end
  else if (FHotPageIndex >= 0) then
  begin
    if (FHotPageIndex < FAdvPages.Count) then
    begin
      if not Assigned(AdvPages[FHotPageIndex].FTimer) and CanGlow then
      begin
        AdvPages[FHotPageIndex].FTimer := TTimer.Create(self);
        AdvPages[FHotPageIndex].FTimer.OnTimer := AdvPages[FHotPageIndex].TimerProc;
        AdvPages[FHotPageIndex].FTimer.Interval := GlowSpeed;
        AdvPages[FHotPageIndex].FTimer.Enabled := true;
      end;

      AdvPages[FHotPageIndex].FTimeInc := -GLOWSTEP;
      AdvPages[FHotPageIndex].FGlowState := gsHover;
    end;
    FHotPageIndex := -1;
    InvalidateTab(-1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: Integer;
  IsActive: boolean;
  Allow: boolean;
begin
  inherited;
  P := Point(X, Y);

  FDownPageIndex := -1;

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);
    if (Tab >= 0) then
    begin
      if (Button = mbLeft) then
      begin
        if Assigned(FOnTabClick) then
          FOnTabClick(Self, Tab);
      end
      else if (Button = mbRight) then
      begin
        if Assigned(FOnTabRightClick) then
          FOnTabRightClick(Self, Tab);
      end;

      IsActive := (Tab = ActivePageIndex);
    
      if (Button <> mbMiddle) and (Tab <> ActivePageIndex) and AdvPages[Tab].TabEnabled then
      begin
        // Select Tab
        ChangeActivePage(Tab);
        if not (csDesigning in ComponentState) then
        begin
          if not Assigned(AdvPages[Tab].FTimer) and CanGlow then
          begin
            AdvPages[Tab].FTimer := TTimer.Create(self);
            AdvPages[Tab].FTimer.OnTimer := AdvPages[Tab].TimerProc;
            AdvPages[Tab].FTimer.Interval := GlowSpeed;
            AdvPages[Tab].FTimer.Enabled := true;
          end;
          AdvPages[Tab].FTimeInc := +GLOWSTEP;
          AdvPages[Tab].FGlowState := gsPush;
        end;
        Invalidate;
      end
      else
      begin
        FDownPageIndex := Tab;
        InvalidateTab(-1);
      end;

      if (Button = mbLeft) and AdvPages[Tab].TabEnabled and IsActive then
      begin
        if TabReorder and not AdvPages[Tab].Locked and((csDesigning in ComponentState) or not AllowTabUndock) then
        begin
          BeginDrag(false,4);

          if not (csDesigning in ComponentState) then
            CreateDropArrow;
        end
        else if not FDraging and AllowTabUndock and not AdvPages[Tab].Locked then
        begin
          Allow := true;
          if Assigned(OnTabUndocking) then
             OnTabUndocking(Self, Advpages[Tab], Allow);

          if Allow then
          begin
            P := ClientToScreen(P);
            if Assigned(FClonedOfPager)then
            begin



              AdvPages[Tab].FFloating := True;
              if (Self.Parent is TFloatingPagerWindow) then
                FClonedOfPager.FFloatingWindow := TFloatingPagerWindow(Self.Parent);
              FClonedOfPager.BeginMove(Shift, P.X, P.Y, AdvPages[Tab])
            end
            else
              BeginMove(Shift, P.X, P.Y, AdvPages[Tab]);
          end;
        end;
      end;
    end
    else
    begin
      (*
      if PtOnTabScrollLeftBtn(X, Y) then
      begin
        FScrollerDownLeftBtn := true;
        DrawTabScrollBtnLeft;
      end;
      if PtOnTabScrollRightBtn(X, Y) then
      begin
        FScrollerDownRightBtn := true;
        DrawTabScrollBtnRight;
      end;
      *)

      if PtOnInsertButton(X, Y) then
      begin
        if not FInsertButtonDown then
        begin
          FInsertButtonDown := true;
          InvalidateInsertButton; //DrawInsertButton;
        end;
      end
      else if FInsertButtonDown then
      begin
        FInsertButtonDown := false;
        InvalidateInsertButton; //DrawInsertButton;
      end;

    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: Integer;
  OnInsert: boolean;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  P := Point(X, Y);

  OnInsert := PtOnInsertButton(X,Y);

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);

    if (FHintPageIndex <> Tab)  and (Tab >= 0) and not OnInsert then
    begin
      FHintPageIndex := Tab;
      Application.CancelHint;
    end;

    if (Tab >= 0) and (Tab <> FHotPageIndex) then
    begin
      if (FDownPageIndex >= 0) then
      begin
        FDownPageIndex := -1;
        InvalidateTab(-1);
      end;

      if (FHotPageIndex >= 0) then
      begin
        OnExitTab(FHotPageIndex);
        begin
          if (FHotPageIndex < FAdvPages.Count) then
          begin
            if not Assigned(AdvPages[FHotPageIndex].FTimer) and CanGlow then
            begin
              AdvPages[FHotPageIndex].FTimer := TTimer.Create(self);
              AdvPages[FHotPageIndex].FTimer.OnTimer := AdvPages[FHotPageIndex].TimerProc;
              AdvPages[FHotPageIndex].FTimer.Interval := GlowSpeed;
              AdvPages[FHotPageIndex].FTimer.Enabled := true;
            end;
            AdvPages[FHotPageIndex].FTimeInc := -GLOWSTEP;

            AdvPages[FHotPageIndex].FGlowState := gsHover;
          end;
          FHotPageIndex := -1;
          InvalidateTab(-1);
        end;
      end;

      // Hot Tab
      OnEnterTab(Tab);

      //InvalidateTab(-1);
      //if (Tab <> FActivePageIndex) then
      if AdvPages[Tab].TabEnabled then
      begin
        FHotPageIndex := Tab;
        FOldHotPageIndex := FHotPageIndex;
        if not Assigned(AdvPages[FHotPageIndex].FTimer) and CanGlow then
        begin
          AdvPages[FHotPageIndex].FTimer := TTimer.Create(self);
          AdvPages[FHotPageIndex].FTimer.OnTimer := AdvPages[FHotPageIndex].TimerProc;
          AdvPages[FHotPageIndex].FTimer.Interval := GlowSpeed;
          AdvPages[FHotPageIndex].FTimer.Enabled := true;
        end;

        AdvPages[FHotPageIndex].FTimeInc := GLOWSTEP;
        Invalidate;
        AdvPages[FHotPageIndex].FGlowState := gsHover;

     { end
      else if (FHotPageIndex >= 0) then
      begin
        OnExitTab(FHotPageIndex);
        begin
          if not Assigned(AdvPages[FHotPageIndex].FTimer) then
          begin
            AdvPages[FHotPageIndex].FTimer := TTimer.Create(self);
            AdvPages[FHotPageIndex].FTimer.OnTimer := AdvPages[FHotPageIndex].TimerProc;
            AdvPages[FHotPageIndex].FTimer.Interval := GlowSpeed;
            AdvPages[FHotPageIndex].FTimer.Enabled := true;
          end;
          AdvPages[FHotPageIndex].FTimeInc := -20;

          AdvPages[FHotPageIndex].FGlowState := gsHover;
          FHotPageIndex := -1;
          InvalidateTab(-1);
        end; }
      end;
    end
    else if (Tab < 0) and (FHotPageIndex >= 0) then
    begin
      if (FDownPageIndex >= 0) then
      begin
        FDownPageIndex := -1;
        InvalidateTab(-1);
      end;
      OnExitTab(FHotPageIndex);
      if (FHotPageIndex = FActivePageIndex) and false then
      begin
        FHotPageIndex := -1;
        Invalidate;
      end
      else
      begin
        if (FHotPageIndex < FAdvPages.Count) then
        begin
          if not Assigned(AdvPages[FHotPageIndex].FTimer) and CanGlow then
          begin
            AdvPages[FHotPageIndex].FTimer := TTimer.Create(self);
            AdvPages[FHotPageIndex].FTimer.OnTimer := AdvPages[FHotPageIndex].TimerProc;
            AdvPages[FHotPageIndex].FTimer.Interval := GlowSpeed;
            AdvPages[FHotPageIndex].FTimer.Enabled := true;
          end;
          AdvPages[FHotPageIndex].FTimeInc := -GLOWSTEP;

          AdvPages[FHotPageIndex].FGlowState := gsHover;
        end;
        FHotPageIndex := -1;
        InvalidateTab(-1);
      end;
    end;

    if (Tab < 0) and not OnInsert then
    begin
      FHintPageIndex := -1;
      Application.CancelHint;
    end;

    if (Tab < 0) and PtOnInsertButton(X, Y) then
    begin
      if not FInsertButtonHot then
      begin
        FInsertButtonHot := true;
        //DrawInsertButton;
        InvalidateInsertButton;
      end;
    end
    else if FInsertButtonHot then
    begin
      FInsertButtonHot := false;
      //DrawInsertButton;
      InvalidateInsertButton;
    end;

    {// Checking for TabScrollButtons
    if PtOnTabScrollLeftBtn(X, Y) then
    begin
      if not FScrollerHoverLeftBtn then
      begin
        FScrollerHoverLeftBtn := true;
        DrawTabScrollBtnLeft;
      end;
    end
    else if FScrollerHoverLeftBtn then
    begin
      FScrollerHoverLeftBtn := false;
      DrawTabScrollBtnLeft;
    end;

    if PtOnTabScrollRightBtn(X, Y) then
    begin
      if not FScrollerHoverRightBtn then
      begin
        FScrollerHoverRightBtn := true;
        DrawTabScrollBtnRight;
      end;
    end
    else if FScrollerHoverRightBtn then
    begin
      FScrollerHoverRightBtn := false;
      DrawTabScrollBtnRight;
    end;
    }
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  Tab: integer;
begin
  inherited;
  P := Point(X, Y);

  if not (csDesigning in ComponentState) and Assigned(FArrow) then
    FArrow.visible := False;

  if (FDownPageIndex >= 0) then
  begin
    FDownPageIndex := -1;
    InvalidateTab(-1);
  end;

  if FInsertButtonDown then
  begin
    FInsertButtonDown := False;
    InvalidateInsertButton; //DrawInsertButton;
    if PtOnInsertButton(X, Y) then
    begin
      DoInsertButtonClick;
    end;
  end;

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(X, Y);
    if (Tab >= 0) then
    begin
      if PTOnCheckBox(Tab, X, Y) then
      begin
        AdvPages[Tab].Checked := not AdvPages[Tab].Checked;
        if Assigned(FOnTabCheckBoxClick) then
          FOnTabCheckBoxClick(Self, Tab);
        InvalidateTab(Tab);
      end;
    end;

    {if PtOnTabScrollLeftBtn(X, Y) then
    begin
      FScrollerDownLeftBtn := false;
      DrawTabScrollBtnLeft;
      ScrollLeftBtnClick;
    end
    else if PtOnTabScrollRightBtn(X, Y) then
    begin
      FScrollerDownRightBtn := false;
      DrawTabScrollBtnRight;
      ScrollRightBtnClick;
    end; }
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.BeginMove(Shift: TShiftState; X, Y: Integer; Page: TAdvOfficePage);
var
  DockInfo: PDockInfo;
  Msg: TMsg;
  CurP: TPoint;
  R: TRect;
  CP: TPoint;
begin
  if (csDesigning in ComponentState) or not AllowTabUndock or not Assigned(Page) or not Assigned(Page.AdvOfficePager) then
    Exit;

  FDockList := TDbgList.Create;
  //---
  //BuildPagerList;
  New(DockInfo);
  DockInfo.Dock := Self;
  GetWindowRect(DockInfo.Dock.Handle, DockInfo.DockRect);

  case DockInfo.Dock.TabPosition of
    tpLeft: DockInfo.DockRect.Right := DockInfo.DockRect.Left + TabSettings.Height;
    tpTop: DockInfo.DockRect.Bottom := DockInfo.DockRect.Top + TabSettings.Height;
    tpRight: DockInfo.DockRect.Left := DockInfo.DockRect.Right - TabSettings.Height;
    tpBottom: DockInfo.DockRect.Top := DockInfo.DockRect.Bottom - TabSettings.Height;
  end;

  FDockList.Add(DockInfo);
  //---

  FOldMouseX := X;
  FOldMouseY := Y;
  FDragingPage := Page;
  SetCapture(Handle);
  FDraging := true;
  R := Page.AdvOfficePager.GetTabRect(Page);
  CP := Point(X, Y);
  CP := Page.AdvOfficePager.ScreenToClient(CP);
  FDragTabX := (CP.X - R.Left) + Page.AdvOfficePager.TabSettings.StartMargin;
  FDragTabY := (CP.Y - R.Top) + 28;
  Page.FDraggingAccept := False;

  while GetCapture = Handle do
  begin
    case integer(GetMessage(Msg, 0, 0, 0)) of
      -1: break;
      0:
        begin
          PostQuitMessage(Msg.WParam);
          Break;
        end;
    end;

    case Msg.message of
      WM_MOUSEMOVE:
        begin
          GetCursorPos(CurP);
          //CurP := ScreenToClient(CurP);
          Move(Shift, curP.X, Curp.Y);
        end;
      WM_LBUTTONUP:
        begin
          EndMove;
          break;
        end;
    else
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
  EndMove;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.Move(Shift: TShiftState; X, Y: Integer);
var
  i, ax, ay: integer;
  P, CP: TPoint;
  ExceedBounds: Boolean;
  R: TRect;
begin
  if FDraging and Assigned(FDragingPage) then
  begin
    FDragingPage.FDraggingAccept := False;
    ExceedBounds := false;
    P := Point(X, Y);
    if FDragingPage.FFloating then
    begin
      if Assigned(FFloatingWindow) then
      begin
        FFloatingWindow.Left := P.X - FDragTabX;
        FFloatingWindow.Top := P.Y - FDragTabY;
      end;

      for i := 0 to FDockList.Count - 1 do
      begin
        if PtInRect(PDockInfo(FDockList[i]).DockRect, P) then
        begin
          if FDragingPage.AdvOfficePager <> PDockInfo(FDockList[i]).Dock then
          begin
            PDockInfo(FDockList[i]).Dock.AddAdvPage(FDragingPage);
            PDockInfo(FDockList[i]).Dock.ActivePage := FDragingPage;

            FDragingPage.FFloating := False;

            if FFloatingWindow <> nil then
            begin
              FFloatingWindow.Free;
              FFloatingWindow := nil;
            end;

            if Assigned(OnTabDock) then
              OnTabDock(Self, FDragingPage);
            break;
          end;
        end;
      end;

    end
    else if Assigned(FDragingPage.AdvOfficePager) then
    begin
      R := FDragingPage.AdvOfficePager.GetTabsRect;
      CP := FDragingPage.AdvOfficePager.ScreenToClient(P);
      ay := CP.Y;
      ax := CP.X;
      if ((TabPosition in [tpTop, tpBottom]) and ((ay >= R.Bottom + 10) or (ay < R.Top - 10) or (ax < R.Left - 100) or (ax > R.Right + 100))) or
         ((TabPosition in [tpLeft, tpRight]) and ((ax >= R.Right + 10) or (ax < R.Left - 10) or (ay < R.Top - 100) or (ay > R.Bottom + 100))) then
      begin
        ExceedBounds := True;
        if Assigned(FDragingPage.AdvOfficePager.FArrow) then
          FDragingPage.AdvOfficePager.DragCanceled;
      end
      else
      begin
        if FDragingPage.AdvOfficePager.TabReorder and PtInRect(R, CP) then
        begin
          if ((TabPosition in [tpTop, tpBottom]) and ((P.X > FOldMouseX + 5) or (P.X < FOldMouseX - 5))) or
             ((TabPosition in [tpLeft, tpRight]) and ((P.Y > FOldMouseY + 5) or (P.Y < FOldMouseY - 5))) then
          begin
            CreateDropArrow;
            FDragingPage.FDraggingAccept := True;
            FDragingPage.AdvOfficePager.DragOver(Self, CP.X, CP.Y, dsDragMove, FDragingPage.FDraggingAccept);
          end;
        end;
      end;

      if ExceedBounds and Assigned(FDragingPage.AdvOfficePager) then
      begin
        FDragingPage.AdvOfficePager.SetFloatingPage(FDragingPage, P.X, P.Y);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.EndMove;
var
  i: integer;
  DockInfo: PDockInfo;
  P: TPoint;
begin
  FDraging := false;
  if GetCapture = Handle then
    ReleaseCapture;

  if Assigned(FDragingPage) then
  begin
    if Assigned(FDragingPage.AdvOfficePager) and (FDragingPage.AdvOfficePager.TabReorder) and FDragingPage.FDraggingAccept then
    begin
      GetCursorPos(P);
      P := FDragingPage.AdvOfficePager.ScreenToClient(P);
      FDragingPage.AdvOfficePager.DragDrop(FDragingPage.AdvOfficePager, P.X, P.Y);
    end;

    FDragingPage.FFloating := False;
    FDragingPage := nil;
  end;
  FFloatingWindow := nil;

  // Dispose DockList
  if Assigned(FDockList) then
  begin
    for i := 0 to FDockList.Count - 1 do
    begin
      DockInfo := FDockList[i];
      Dispose(DockInfo);
    end;
    FDockList.free;
    FDockList := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetFloatingPage(Page: TAdvOfficePage; X, Y, W,
  H: integer);

var
  ClonedPager: TAdvOfficePager;
  {$IFDEF DELPHI2007_LVL}
  PrtForm: TCustomForm;
  {$ENDIF}
  P, P2: TPoint;
  R: TRect;
begin
  if (csDesigning in ComponentState) or not Assigned(Page) or (Page.AdvOfficePager <> Self) or (Page.FFloating) then
    Exit;

  if False and Assigned(FClonedOfPager) then
  begin

  end
  else
  begin
    if not Assigned(Page.FParentPager) and not Assigned(FClonedOfPager) then
      Page.FParentPager := Self;

    RemoveAdvPage(Page);
    if not Assigned(FFloatingPages) then
      FFloatingPages := TDbgList.Create;
    FFloatingPages.Add(Page);
    Page.FFloating := True;

    FFloatingWindow := FFloatingPagerWindowClass.CreateNew(Owner);
    TForm(FFloatingWindow).BorderIcons := FloatingBorderIcons;
    FFloatingWindow.BorderStyle := FloatingBorderStyle;

    {$IFDEF DELPHI2007_LVL}
    if FFloatingBorderStyle = bsNone then
    begin
      FFloatingWindow.Padding.Left := 2;
      FFloatingWindow.Padding.Top := 2;
      FFloatingWindow.Padding.Right := 2;
      FFloatingWindow.Padding.Bottom := 2;
    end;

    PrtForm := GetParentForm(Self);
    if Assigned(PrtForm) then
    begin
      FFloatingWindow.GlassFrame.Assign(PrtForm.GlassFrame);
    end;
    {$ENDIF}

    ClonedPager := CreateClonedPager(FFloatingWindow);
    ClonedPager.AddAdvPage(Page);
    ClonedPager.ActivePage := Page;
    ClonedPager.AllowTabUndock := true;

//    FFloatingWindow.AdvPager := ClonedPager;
    FFloatingWindow.ClientWidth := ClonedPager.Width;
    FFloatingWindow.ClientHeight := ClonedPager.Height;

    ClonedPager.Align := alClient;

    if (X > 0) or (Y > 0) then
    begin
      P := Point(X, Y);
    end
    else
    begin
      GetCursorPos(P);
      R := ClonedPager.GetTabRect(Page);
      P2 := Point(R.Left + 10, R.Top + 10);
      P2 := ClonedPager.ClientToScreen(P2);
      P.X := P2.X - (R.Left + 10);
      P.Y := P2.Y - (R.Top + 10);
    end;

    FFloatingWindow.Left := P.X;
    FFloatingWindow.Top := P.Y;

    if W <> -1 then
      FFloatingWindow.Width := W;

    if H <> -1 then
      FFloatingWindow.Height := H;

    FFloatingWindow.Visible := True;

    if Assigned(OnTabUnDock) then
      OnTabUnDock(Self, Page);
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvOfficePager.SetFloatingPage(Page: TAdvOfficePage; X: integer = 0; Y: integer = 0);
begin
  SetFloatingPage(Page, X, Y, -1, -1);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CreateClonedPager(
  PrtWindow: TWinControl): TAdvOfficePager;
begin
  Result := TAdvOfficePager.Create(Self);
  Result.Parent := PrtWindow;
  Result.FClonedOfPager := Self;
  Result.AssignPager(Self);
  Result.TabPosition := tpTop;

  Result.ButtonSettings.CloseButton := False;
  Result.ButtonSettings.PageListButton := False;
  Result.ButtonSettings.ClosedListButton := False;
  Result.ButtonSettings.FirstButton := False;
  Result.ButtonSettings.LastButton := False;
  Result.ButtonSettings.ShowInsertButton := False;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.AssignPager(Source: TAdvOfficePager);
begin
  if not Assigned(Source) then
    Exit;

  Height := Source.Height;
  Width := Source.Width;
  Transparent := Source.Transparent;
  AdvOfficePagerStyler := Source.AdvOfficePagerStyler;
  ButtonSettings.Assign(Source.ButtonSettings);
  TabSettings.Assign(Source.TabSettings);
  AntiAlias := Source.AntiAlias;
  CloseOnTab := Source.CloseOnTab;
  CloseOnTabPosition := Source.CloseOnTabPosition;
  DisabledImages := Source.DisabledImages;
  FreeOnClose := Source.FreeOnClose;
  Glow := Source.Glow;
  Images := Source.Images;
  PageMargin := Source.PageMargin;
  ShowTabHint := Source.ShowTabHint;

  if Source.FITones then
    SetColorTones(Source.FTones);
end;

//------------------------------------------------------------------------------


function TAdvOfficePager.GetCheckBoxRect(PageIndex: integer): TRect;
var
  rb, R: TRect;
  h, w: integer;
begin
  R := GetTabRect(PageIndex);
  w := 0;
  h := 0;
  if CloseOnTab and (CloseOnTabPosition = cpLeft) and (ShowCloseOnNonSelectedTabs or ((ActivePageIndex = PageIndex) and not ShowCloseOnNonSelectedTabs)) then
  begin
    rb := GetCloseButtonRect(PageIndex);
    h := rb.Bottom - rb.Top;
    w := rb.Right - rb.Left;
  end;
  case TabPosition of
    tpTop, tpBottom: Result := Bounds(R.Left + 5 + w, r.Top + (r.Bottom - R.Top - 15) div 2, 15, 15);
    tpLeft:
      begin
        if not RotateTabLeftRight then
          Result := Bounds(R.Left + 5, r.Top + 5, 15, 15)
        else
          Result := Bounds(R.Left + (r.right - R.Left - 15) div 2, r.Bottom - h - 5 - 15, 15, 15);
      end;
    tpRight:
      begin
        if not RotateTabLeftRight then
          Result := Bounds(R.Right - 15 - 5, r.Top + 5, 15, 15)
        else
          Result := Bounds(R.Left + (r.right - R.Left - 15) div 2, r.Top + 5 + h, 15, 15);
      end;
  end;
end;

procedure TAdvOfficePager.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  for I := 0 to FAdvPages.Count - 1 do Proc(TComponent(FAdvPages[I]));

  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if (Control.Owner = Root) and (FAdvPages.IndexOf(Control) < 0) then Proc(Control);
  end;

end;

//------------------------------------------------------------------------------

function TAdvOfficePager.FindNextPage(CurPage: TAdvOfficePage; GoForward,
  CheckTabVisible: Boolean): TAdvOfficePage;
var
  i, j, CurIndex: Integer;
begin
  Result := nil;
  CurIndex := FAdvPages.IndexOf(CurPage);


  if (CurPage = nil) or (CurIndex < 0) then
  begin

    if FAdvPages.Count > 0 then
    begin
      if GoForward then
        Result := FAdvPages[0]
      else
        Result := FAdvPages[FAdvPages.Count - 1];
    end;
    Exit;
  end;
    
  if GoForward then
  begin
    i := CurIndex;
    j := 0; //1;
    while (j < FAdvPages.Count) do
    begin
      Inc(i);    
      if (i >= FAdvPages.Count) then
        i := 0;
      if (CheckTabVisible and AdvPages[i].TabVisible) or not CheckTabVisible then
      begin
        Result := AdvPages[i];
        Break;
      end;
      Inc(j);
    end;
  end
  else  // BackWard
  begin
    i := CurIndex;
    j := 0; //1;
    while (j < FAdvPages.Count) do
    begin
      dec(i);
      if (i >= FAdvPages.Count) then
        i := 0;
      if (i < 0) then
        i := FAdvPages.Count-1;
      if (CheckTabVisible and AdvPages[i].TabVisible) or not CheckTabVisible then
      begin
        Result := AdvPages[i];
        Break;
      end;
      Inc(j);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetActivePage: TAdvOfficePage;
begin
  Result := nil;
  if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvPages.Count) then
    Result := AdvPages[FActivePageIndex];
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetActivePageIndex: Integer;
begin
  Result := FActivePageIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SelectNextPage(GoForward: Boolean);
var
  i, j: Integer;
begin
  if (ActivePageIndex < 0) then
    Exit;

  i := ActivePageIndex;
  j := 0;

  while (j < AdvPageCount) do
  begin
    if GoForward then
      Inc(i)
    else
      Dec(i);

    if (i >= FAdvPages.Count) then
      i := 0;
    if (i < 0) then
      i := FAdvPages.Count - 1;

    if (ActivePageIndex <> i) and AdvPages[i].TabVisible and AdvPages[i].TabEnabled then
    begin
      ActivePageIndex := i;
      Break;
    end;

    Inc(j);
  end;



end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IndexOfPage(AdvPage: TAdvOfficePage): Integer;
begin
  Result := FAdvPages.IndexOf(AdvPage);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetActivePage(const Value: TAdvOfficePage);
begin
  if (FAdvPages.IndexOf(Value) >= 0) then
    ActivePageIndex := FAdvPages.IndexOf(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ChangeActivePage(PageIndex: Integer);
var
  aForm: TCustomForm;
  AllowChange, WasPageFocused: Boolean;
  OldActivePage: TAdvOfficePage;
  FOnShow: TNotifyEvent;
begin

  if (PageIndex >= 0) and (PageIndex < FAdvPages.Count) and ((PageIndex <> ActivePageIndex) or (FAdvPages.Count = 1)) then
  begin
    if FTabShortCutHintShowing then
    begin
      HideShortCutHintOfAllPages;
    end;

    AllowChange := True;

    if Assigned(FOnChanging) and FPropertiesLoaded and not (csDestroying in ComponentState) then
      FOnChanging(Self, ActivePageIndex, PageIndex, AllowChange);

    if not AllowChange then
      Exit;

    aForm := GetParentForm(Self);
    WasPageFocused := (aForm <> nil) and (ActivePage <> nil) and ((aForm.ActiveControl = ActivePage) or ActivePage.ContainsControl(aForm.ActiveControl));

    if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvPages.Count) then
    begin
      AdvPages[FActivePageIndex].Visible := False;

      if Assigned(AdvPages[FActivePageIndex].FOnHide) then
        AdvPages[FActivePageIndex].FOnHide(AdvPages[FActivePageIndex]);
    end;

    OldActivePage := ActivePage;

    FActivePageIndex := PageIndex;

    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      //aForm := GetParentForm(Self);
      if (aForm <> nil) and (aForm.Designer <> nil) then
        aForm.Designer.Modified;
    end;

    InitializeAndUpdateButtons;

    if Assigned(ActivePage) and (Visible or (csDesigning in ComponentState)) then
    begin
      if (ActivePage.ControlCount = 0) then
      begin
        FOnShow := AdvPages[FActivePageIndex].OnShow;
        AdvPages[FActivePageIndex].OnShow := nil;
        AdvPages[FActivePageIndex].Visible := True;
        AdvPages[FActivePageIndex].BringToFront;
        AdvPages[FActivePageIndex].OnShow := FOnShow;
      end;

      if Assigned(aForm) and Assigned(OldActivePage) and WasPageFocused then
      begin
        if ActivePage.CanFocus then
          aForm.ActiveControl := ActivePage
        else
          aForm.ActiveControl := Self;
      end;
    end;

    if Visible or (csDesigning in ComponentState) then
    begin
      FOnShow := AdvPages[FActivePageIndex].OnShow;
      AdvPages[FActivePageIndex].OnShow := nil;

      AdvPages[FActivePageIndex].Visible := True;
      AdvPages[FActivePageIndex].BringToFront;

      if not (csDesigning in ComponentState) and (aForm <> nil) and (ActivePage <> nil) and {(aForm.ActiveControl = ActivePage)} WasPageFocused then
      begin
        ActivePage.SelectFirstControl;
      end;

      AdvPages[FActivePageIndex].OnShow := FOnShow;

      if not (csLoading in ComponentState) then
      begin
        if self.HandleAllocated and self.Visible then
          if Assigned(AdvPages[FActivePageIndex].OnShow) and (AdvPages[FActivePageIndex].Visible) then
             AdvPages[FActivePageIndex].OnShow(AdvPages[FActivePageIndex]);
      end;
    end;

    if Assigned(FOnChange) and not (csDestroying in ComponentState)
      and not (csLoading in ComponentState) then
      FOnChange(Self);

    UpdateMultiLineActiveTab;
    if FMultiLine then
      InitializeAndUpdateButtons;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetActivePageIndex(const Value: Integer);
var
  R: TRect;
  i, j: Integer;
begin
  { if (Value >= 0) and (Value < FAdvPages.Count) and (Value <> ActivePageIndex) then
  begin
    if (ActivePageIndex >= 0) and (ActivePageIndex < FAdvPages.Count) then
    begin
      AdvPages[FActivePageIndex].Visible := False;
    end;

    FActivePageIndex := Value;
    AdvPages[FActivePageIndex].Visible := True;
    AdvPages[FActivePageIndex].BringToFront;
 }
    R := GetTabRect(Value);

    if (R.Left = -1) and (Value >= 0) and (Value < FAdvPages.Count) then // force activated tab to be visible
    begin
      //FTabScroller.Position := Value;
      j := 0;
      for i:= 0 to Value - 1 do
      begin
        if AdvPages[i].TabVisible then
          inc(j);
      end;

      if (FixedTabs > 0) and not (csDesigning in ComponentState) then
        j := j - FixedTabs;
      FTabScroller.Position := Min(j, FTabScroller.Max);
    end;

    ChangeActivePage(Value);
    R := GetTabsArea;
    InvalidateRect(Handle, @R, True);
 // end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetTabSettings(const Value: TPagerTabSettings);
begin
  FTabSettings.Assign(Value);
end;

procedure TAdvOfficePager.SetTransparent(const Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetAllowTabUndock(const Value: Boolean);
begin
  FAllowTabUndock := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetAllPagesPosition;
var
  i: Integer;
begin
  for i:= 0 to FAdvPages.Count-1 do
  begin
    SetPagePosition(TAdvOfficePage(FAdvPages[i]));
  end;
end;

//------------------------------------------------------------------------------
(*
function TAdvOfficePager.GetCaptionRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FCaption.Visible then
  begin
    Result := ClientRect;
    Result.Bottom := Result.Top + Caption.Height;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetAvailableCaptionRect: TRect;
begin
  Result := GetCaptionRect;
  if FCaption.Visible then
  begin
    Result.Left := Result.Left + FCaption.Indent;
    Result.Right := Result.Right - FCaption.RightIndent;
  end;
end;
*)
//------------------------------------------------------------------------------

function TAdvOfficePager.GetAdvPageRect: TRect;
begin
  Result := ClientRect;
  case TabPosition of
    tpTop:
    begin
      if MultiLine then
        Result.Top := Result.Top + FTabRowSize
      else
        Result.Top := Result.Top + TabSettings.Height;
      if not IsOnGlass then
      begin
        Result.Left := Result.Left + FPageMargin;
        Result.Right := Result.Right-FPageMargin;
      end;
      if not (IsOnGlass and (Align = alClient)) then
        Result.Bottom := Result.Bottom - FPageMargin-1;

      if FItones then
        Result.Bottom := Result.Bottom + 1;
    end;
    tpBottom:
    begin
      Result.Top := Result.Top + FPageMargin+1;
      if not IsOnGlass then
      begin
        Result.Left := Result.Left + FPageMargin;
        Result.Right := Result.Right-FPageMargin;
      end;
      if MultiLine then
        Result.Bottom := Result.Bottom - FTabRowSize
      else
        Result.Bottom := Result.Bottom - TabSettings.Height;

      if FItones then
        Result.Top := Result.Top - 1;
    end;
    tpLeft:
    begin
      Result.Top := Result.Top + FPageMargin+1;
      if MultiLine then
        Result.Left := Result.Left + FTabRowSize
      else
        Result.Left := Result.Left + TabSettings.Height;
      if not IsOnGlass then
        Result.Right := Result.Right-FPageMargin;
      Result.Bottom := Result.Bottom - FPageMargin-1;

      if FItones then
        Result.Bottom := Result.Bottom + 1;
    end;
    tpRight:
    begin
      Result.Top := Result.Top + FPageMargin+1;
      if not IsOnGlass then
        Result.Left := Result.Left + FPageMargin;
      if MultiLine then
        Result.Right := Result.Right - FTabRowSize
      else
        Result.Right := Result.Right - TabSettings.Height;
      Result.Bottom := Result.Bottom - FPageMargin-1;

      if FItones then
        Result.Bottom := Result.Bottom + 1;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetPagePosition(AdvPage: TAdvOfficePage);
var
  R: TRect;
begin
  if (AdvPage <> nil) and (FAdvPages.IndexOf(AdvPage) >= 0) then
  begin
    R := GetAdvPageRect;
    {
    AdvPage.Left := R.Left;
    AdvPage.Top := R.Top;
    AdvPage.Width := R.Right - R.Left;
    AdvPage.Height := R.Bottom - R.Top;
    }
    AdvPage.SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IndexOfTabAt(X,Y: Integer): integer;
begin
  Result := PtOnTab(x,y);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PTOnCheckBox(PageIndex, X, Y: integer): Boolean;
begin
  Result := false;
  if AdvPages[PageIndex].ShowCheckBox then
    Result := PtInRect(GetCheckBoxRect(PageIndex), Point(X, Y));
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PTOnTab(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
  TabR: TRect;
begin
  Result := -1;
  P := Point(X, Y);
  for i:= 0 to FAdvPages.Count-1 do
  begin
    TabR := GetTabRect(i);
    if PtInRect(TabR, P) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetButtonsRect: TRect;
var
  Spc: Integer;
  cnt: integer;
begin
  cnt := 0;
  if ButtonSettings.FirstButton and ButtonSettings.LastButton then
    cnt := 4
  else if ButtonSettings.FirstButton or ButtonSettings.LastButton then
    cnt := 3
  else if not ButtonSettings.FirstButton and not ButtonSettings.LastButton then
    cnt := 2;

  Result := ClientRect;
  Spc := 3;
  case TabPosition of
    tpTop:
    begin
      Result.Top := Result.Top + FTabOffset;
      Result.Bottom := Result.Top + FTabSettings.Height;
      Result.Top := Result.Bottom - ButtonSettings.ButtonSize - 8;
      Result.Right := Result.Right - FTabSettings.EndMargin - FPageMargin - 3;
      Result.Left := Result.Right-3;
      if FTabScroller.Visible then
        Result.Left := Result.Left -(ButtonSettings.ButtonSize * cnt + cnt * 2);
      if (ButtonSettings.PageListButton) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0)) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
    end;
    tpBottom:
    begin
      Result.Top := Result.Bottom - FTabSettings.Height - FTabOffSet;
      //Result.Bottom := Result.Bottom - FTabOffSet;
      Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 8;
      Result.Right := Result.Right - FTabSettings.EndMargin - FPageMargin-3;
      Result.Left := Result.Right -3;
      if FTabScroller.Visible then
        Result.Left := Result.Left -(ButtonSettings.ButtonSize*cnt + cnt * 2);
      if (ButtonSettings.PageListButton) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0)) then
        Result.Left := Result.Left - ButtonSettings.ButtonSize - Spc;
    end;
    tpLeft:
    begin
      Result.Bottom := Result.Bottom - FTabSettings.EndMargin - FPageMargin-3;
      Result.Top := Result.Bottom -3;
      Result.Left := Result.Left + FTabOffSet;
      Result.Right := Result.Left + FTabSettings.Height;
      Result.Left := Result.Right - ButtonSettings.ButtonSize - 8;
      if FTabScroller.Visible then
        Result.Top := Result.Top -(ButtonSettings.ButtonSize*cnt + cnt * 2);
      if (ButtonSettings.PageListButton) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0)) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;

      if not RotateTabLeftRight then
        Result.Left := Result.Right - 5 - ButtonSettings.ButtonSize - 3;
    end;
    tpRight:
    begin
      Result.Bottom := Result.Bottom - FTabSettings.EndMargin - FPageMargin-3;
      Result.Top := Result.Bottom -3;
      Result.Left := Result.Right - TabSettings.Height - FTabOffSet;
      //Result.Right := Result.Right - FTabOffSet;
      Result.Right := Result.Left + ButtonSettings.ButtonSize + 8;
      if FTabScroller.Visible then
        Result.Top := Result.Top -(ButtonSettings.ButtonSize*cnt + cnt * 2);
      if (ButtonSettings.PageListButton) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.CloseButton and not CloseOnTab) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;
      if (ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0)) then
        Result.Top := Result.Top - ButtonSettings.ButtonSize - Spc;

      if not RotateTabLeftRight then
        Result.Right := Result.Left + 5 + ButtonSettings.ButtonSize + 3;
    end;
  end;
end;

//------------------------------------------------------------------------------

// Independent to Start/End Margins and Scroller
function TAdvOfficePager.GetTabsArea: TRect;
var
  h: Integer;
begin
  Result := ClientRect;
  if MultiLine then
    h := FTabRowSize
  else
    h := FTabSettings.Height;

  case TabPosition of
    tpTop: Result.Bottom := Result.Top + h;
    tpBottom: Result.Top := Result.Bottom - h;
    tpLeft: Result.Right := Result.Left + h;
    tpRight: Result.Left := Result.Right - h;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabsRect: TRect;
begin
  Result := ClientRect;
  case TabPosition of
    tpTop:
    begin
      Result.Top := Result.Top + FTabOffSet;
      Result.Bottom := Result.Top + FTabSettings.Height;
      Result.Left := Result.Left + FTabSettings.StartMargin + FPageMargin;
      Result.Right := GetButtonsRect.Left; //Result.Right - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Right := Result.Right - ButtonSettings.ButtonSize - 6;
    end;
    tpBottom:
    begin
      Result.Top := Result.Bottom - FTabSettings.Height - FTabOffSet;
      Result.Bottom := Result.Bottom - FTabOffSet;
      Result.Left := Result.Left + FTabSettings.StartMargin + FPageMargin;
      Result.Right := GetButtonsRect.Left; //Result.Right - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Right := Result.Right - ButtonSettings.ButtonSize - 6;
    end;
    tpLeft:
    begin
      Result.Top := Result.Top + FTabSettings.StartMargin + FPageMargin;
      Result.Bottom := GetButtonsRect.Top; // Result.Bottom - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Bottom := Result.Bottom - ButtonSettings.ButtonSize - 6;
      Result.Left := Result.Left + FTabOffSet;
      Result.Right := Result.Left + FTabSettings.Height;
    end;
    tpRight:
    begin
      Result.Top := Result.Top + FTabSettings.StartMargin + FPageMargin;
      Result.Bottom := GetButtonsRect.Top; //Result.Bottom - FTabSettings.EndMargin;
      if ButtonSettings.ShowInsertButton then
        Result.Bottom := Result.Bottom - ButtonSettings.ButtonSize - 6;
      Result.Left := Result.Right - TabSettings.Height - FTabOffSet;
      Result.Right := Result.Right - FTabOffSet;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabRect(PageIndex: Integer): TRect;
begin
  Result := GetTabRect(0, PageIndex, True);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabRect(StartIndex, PageIndex: Integer; ConsiderTabScroller: Boolean): TRect;
var
  i, TbW, TbH, j, Sp, fdW, ImgTxtSp, ImgW, ImgH: Integer;
  R, CR, R2: TRect;
  TabAppearance: TCustomTabAppearance;
  Pic: TGDIPPicture;
  fixTabs: Integer;
begin
  Result := Rect(-1, -1, -1, -1);
  Sp := FTabSettings.Spacing; //0;
  fdW := 5;
  ImgTxtSp := IMG_SPACE;

  //Ellipsis := (TabSettings.Width > 0) and not TabSettings.WordWrap;

  if (PageIndex >= 0) and (PageIndex < FAdvPages.Count) then
  begin
    if not AdvPages[PageIndex].TabVisible then
      Exit;

    if MultiLine then
    begin
      Result := AdvPages[PageIndex].FTabRect;
      Exit;
    end;

    CR := GetTabsRect; // ClientRect;
    fixTabs := FixedTabs;
    if (csDesigning in ComponentState) then  // No fixedtabs at design time
      fixTabs := 0;

    //if Align in [daTop, daBottom] then
    begin
      {if FCaption.Visible then
        CR.Top := CR.Top + FCaption.Height;

      CR.Bottom := CR.Top + FTabSettings.Height;
      CR.Left := CR.Left + FTabSettings.StartMargin;
      CR.Right := CR.Right - FTabSettings.EndMargin;
      if FTabScroller.Visible then
        CR.Right := CR.Right - SCROLLER_SIZE; }
      //TbW := 0;
      j := 0;
      for i := 0{StartIndex} to PageIndex do
      begin
        if not AdvPages[i].TabVisible or ((i < StartIndex) and (i > fixTabs - 1)) then
          Continue;

        if (AdvPages[i].UseTabAppearance) then
          TabAppearance := AdvPages[i].TabAppearance
        else if Assigned(FTabAppearance) and FItones then
          TabAppearance := FTabAppearance
        else
          TabAppearance := FCurrentOfficePagerStyler.TabAppearance;

        Canvas.Font.Assign(TabAppearance.Font);
        Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

        ImgW := 0;
        ImgH := 0;

        if (TabPosition in [tpTop, tpBottom]) then
        begin
          if FUseMaxSpace then
            CR.Right := GetTabsArea.Right - FTabSettings.EndMargin - FPageMargin - 3;
          {
          if TabSettings.WordWrap then
            DTSTYLE := DT_WORDBREAK
          else
            DTSTYLE := DT_SINGLELINE;
          }

          if (AdvPages[i].Caption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);
            //DrawText(Canvas.Handle,PChar(AdvPages[i].Caption),Length(AdvPages[i].Caption), R2, DT_CALCRECT or DT_LEFT or DTSTYLE);
            R2 := DrawVistaText(Canvas, TabSettings.Alignment, R2, AdvPages[i].Caption, '', Canvas.Font, AdvPages[i].Enabled, False, FAntiAlias, tpTop, False, TabSettings.WordWrap);
            R2.Right := R2.Right - fdw;
          end
          else if (AdvPages[i].WideCaption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);
            //R2 := DrawVistaText(Canvas, taLeftJustify, R2, '', AdvPages[i].WideCaption, Canvas.Font, Enabled, False, FAntiAlias, TabPosition, Ellipsis);
            (*
            DrawTextW(Canvas.Handle, PWideChar(AdvPages[i].WideCaption), -1, R2, DT_CALCRECT or DT_LEFT or DTSTYLE);
            *)
            //R2.Right := WideCanvasTextWidth(Canvas, AdvPages[i].WideCaption);

            R2 := DrawVistaText(Canvas, taLeftJustify, R2, '', AdvPages[i].WideCaption, Canvas.Font, AdvPages[i].Enabled, False, FAntiAlias, tpTop, False, TabSettings.WordWrap);
            R2.Right := R2.Right - fdw;
          end
          else
            R2 := Rect(0, 0, 0, 0);
          //R2 := DrawVistaText(Canvas, R2, AdvPages[i].Caption, Canvas.Font, AdvPages[PageIndex].Enabled, False);

          TbW := GetLeftRoundingOffset + TabSettings.LeftMargin + R2.Right + fdW + TabSettings.RightMargin + GetRightRoundingOffset + 3;

          if AdvPages[i].Enabled or AdvPages[i].DisabledPicture.Empty then
            Pic := AdvPages[i].Picture
          else
            Pic := AdvPages[i].DisabledPicture;

          if Assigned(Pic) and not Pic.Empty then
          begin
            Pic.GetImageSizes;
            //TbW := TbW + Pic.Width + ImgTxtSp;
            ImgW := Pic.Width;
          end
          else
          if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[i].ImageIndex >= 0) then
          begin
            if AdvPages[i].Enabled then
            begin
              if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Width + ImgTxtSp;
                ImgW := FImages.Width;
              end;
            end
            else
            begin
              if Assigned(FDisabledImages) then
              begin
                //TbW := TbW + FDisabledImages.Width + ImgTxtSp
                ImgW := FDisabledImages.Width;
              end
              else if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Width + ImgTxtSp;
                ImgW := FImages.Width;
              end;
            end;
          end;

          //TbW := TbW + ImgW;
          case TabSettings.ImagePosition of
            ipTop, ipBottom:
            begin
              // do nothing
            end;
            ipLeft, ipRight:
            begin
              TbW := TbW + ImgW;
              if ImgW > 0 then
               TbW := TbW + ImgTxtSp;
            end;
          end;

          if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[i].ShowClose) then
            TbW := Tbw + ButtonSettings.ButtonSize + 4;

          if AdvPages[I].ShowCheckBox then
            TbW := TbW + 20;

          if (TabSettings.Width > 0) then
            TbW := TabSettings.Width;

          if ((CR.Left + TbW) > (CR.Right - FPageMargin)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = PageIndex) and (FTabScroller.Position <> PageIndex))) then
          begin
            if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
              Break;
          end;

          if FTabScroller.Visible and ConsiderTabScroller then
          begin
            if (j >= FTabScroller.Position) then
            begin
              R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
              if (i = PageIndex) then
                Result := R;
              CR.Left := CR.Left + TbW + Sp;
            end
            else if (fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0) then
            begin
              R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
              if (i = PageIndex) then
                Result := R;
              CR.Left := CR.Left + TbW + Sp;
              Dec(j); // Counter to inc(j) at last
            end;
          end
          else
          begin
            R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
            if (i = PageIndex) then
              Result := R;
            CR.Left := CR.Left + TbW + Sp;
          end;
        end
        else // TabPosition in [tpLeft, tpRight]
        begin
          if FUseMaxSpace then
            CR.Bottom := GetTabsArea.Bottom - FTabSettings.EndMargin - FPageMargin - 3;

          if (AdvPages[i].Caption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);
            DrawText(Canvas.Handle,PChar(AdvPages[i].Caption),Length(AdvPages[i].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE);
          end
          else if (AdvPages[i].WideCaption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);
            DrawTextW(Canvas.Handle, PWideChar(AdvPages[i].WideCaption), -1, R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE);
          end
          else
            R2 := Rect(0, 0, 0, 0);
          //R2 := DrawVistaText(Canvas, R2, AdvPages[i].Caption, Canvas.Font, AdvPages[PageIndex].Enabled, False);

          TbW := TabSettings.LeftMargin + R2.Right+ fdW + TabSettings.RightMargin;
          TbH := R2.Bottom;

          if AdvPages[i].Enabled or AdvPages[i].DisabledPicture.Empty then
            Pic := AdvPages[i].Picture
          else
            Pic := AdvPages[i].DisabledPicture;

          if Assigned(Pic) and not Pic.Empty then
          begin
            Pic.GetImageSizes;
            //TbW := TbW + Pic.Height + ImgTxtSp;
            //TbH := Max(TbH, Pic.Height);
            ImgW := Pic.Width;
            ImgH := Pic.Height;
          end
          else
          if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[i].ImageIndex >= 0) then
          begin
            if AdvPages[i].Enabled then
            begin
              if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Height + ImgTxtSp;
                //TbH := Max(TbH, FImages.Height);
                ImgW := FImages.Width;
                ImgH := FImages.Height;
              end;  
            end
            else
            begin
              if Assigned(FDisabledImages) then
              begin
                //TbW := TbW + FDisabledImages.Height + ImgTxtSp;
                //TbH := Max(TbH, FDisabledImages.Height);
                ImgW := FDisabledImages.Width;
                ImgH := FDisabledImages.Height;
              end
              else if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Height + ImgTxtSp;
                //TbH := Max(TbH, FImages.Height);
                ImgW := FImages.Width;
                ImgH := FImages.Height;
              end;
            end;
          end;

          case TabSettings.ImagePosition of
            ipTop, ipBottom:
            begin
              TbH := Tbh + ImgH + ImgTxtSp;
            end;
            ipLeft, ipRight:
            begin
              TbW := TbW + ImgW + ImgTxtSp;
              TbH := Max(TbH, ImgH);
            end;
          end;

          TbH := TbH + 12; //TabSettings.RightMargin;

          if not RotateTabLeftRight then
          begin
            if (TabSettings.Width > 0) then
              TbH := TabSettings.Width;

            if ((CR.Top + TbH) > (CR.Bottom - FPageMargin)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = PageIndex) and (FTabScroller.Position <> PageIndex))) then
            begin
              if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
                Break;
            end;

            if FTabScroller.Visible and ConsiderTabScroller then
            begin
              if (j >= FTabScroller.Position) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbH);
                if (i = PageIndex) then
                  Result := R;
                CR.Top := CR.Top + TbH + Sp;
              end
              else if (fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbH);
                if (i = PageIndex) then
                  Result := R;
                CR.Top := CR.Top + TbH + Sp;
                Dec(j); // Counter to inc(j) at last
              end;
            end
            else
            begin
              R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbH);
              if (i = PageIndex) then
                Result := R;
              CR.Top := CR.Top + TbH + Sp;
            end;
          end
          else
          begin
            TbW := TbW + GetLeftRoundingOffset + GetRightRoundingOffset;
            if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[i].ShowClose) then
              TbW := Tbw + ButtonSettings.ButtonSize + 4;

            if AdvPages[I].ShowCheckBox then
              TbW := TbW + 20;

            if (TabSettings.Width > 0) then
              TbW := TabSettings.Width;

            if ((CR.Top + TbW) > (CR.Bottom - FPageMargin)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = PageIndex) and (FTabScroller.Position <> PageIndex))) then
            begin
              if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
                Break;
            end;

            if FTabScroller.Visible and ConsiderTabScroller then
            begin
              if (j >= FTabScroller.Position) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
                if (i = PageIndex) then
                  Result := R;
                CR.Top := CR.Top + TbW + Sp;
              end
              else if (fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0) then
              begin
                R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
                if (i = PageIndex) then
                  Result := R;
                CR.Top := CR.Top + TbW + Sp;
                Dec(j); // Counter to inc(j) at last
              end;
            end
            else
            begin
              R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
              if (i = PageIndex) then
                Result := R;
              CR.Top := CR.Top + TbW + Sp;
            end;
          end;
        end;
        Inc(j);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabRect(Page: TAdvOfficePage): TRect;
begin
  Result := GetTabRect(FAdvPages.IndexOf(Page));
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetMultiLine(const Value: Boolean);
begin
  if (FMultiLine <> Value) then
  begin
    if Value then
    begin
      if (FixedTabs > 0) then
      begin
        MessageDlg('Can not have multiline with Fixed tabs.', mtInformation, [mbOK], 0);
        Exit;
      end;

      if (not RotateTabLeftRight) then
      begin
        MessageDlg('RotateTabLeftRight must be True with MultiLine.', mtInformation, [mbOK], 0);
        Exit;
      end;
    end;

    FMultiLine := Value;

    if FMultiLine then
      UpdateMultiLineTabs
    else
      UpdateTabScroller;

    InitializeAndUpdateButtons;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateMultiLineTabs;
var
  avlspace, tbRowCount, i1, i2, acvTabR, RwSpace: Integer;
  R, TR, R2, CR: TRect;
  i, TbW, TbH, j, Sp, fdW, ImgTxtSp, ImgW, ImgH: Integer;
  TabAppearance: TCustomTabAppearance;
  Pic: TGDIPPicture;
  multiRow: Boolean;
  tbRows: array of TPoint;
begin
  if FMultiLine and not (csLoading in ComponentState) and not (csReading in ComponentState) and
     not (csDestroying in ComponentState) then
  begin
    FTabScroller.Visible := False;

    TR := GetTabsRect;
    CR := TR;

    if TabPosition in [tpTop, tpBottom] then
      avlspace := TR.Right - TR.Left
    else
      avlspace := TR.Bottom - TR.Top;

    Sp := FTabSettings.Spacing;
    fdW := 5;
    ImgTxtSp := IMG_SPACE;
    MultiRow := False;
    if ShowNonSelectedTabs then
      RwSpace := 2   // Row space
    else
      RwSpace := 0;  // Row space

    for i := 0 to AdvPageCount - 1 do
    begin
      if not AdvPages[i].TabVisible then
      begin
        AdvPages[i].FTabRect := Rect(-1, -1, -1, -1);
        Continue;
      end;

      if (AdvPages[i].UseTabAppearance) then
        TabAppearance := AdvPages[i].TabAppearance
      else if Assigned(FTabAppearance) and FItones then
        TabAppearance := FTabAppearance
      else
        TabAppearance := FCurrentOfficePagerStyler.TabAppearance;

      Canvas.Font.Assign(TabAppearance.Font);
      Canvas.Font.Size := Round(GetDPIScale(FFormScaled, Canvas.Handle)* Canvas.Font.Size);

      ImgW := 0;
      ImgH := 0;

      if (TabPosition in [tpTop, tpBottom]) then
      begin
        //if FUseMaxSpace then
          //CR.Right := GetTabsArea.Right;

          if (AdvPages[i].Caption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);
            //DrawText(Canvas.Handle,PChar(AdvPages[i].Caption),Length(AdvPages[i].Caption), R2, DT_CALCRECT or DT_LEFT or DTSTYLE);
            R2 := DrawVistaText(Canvas, taLeftJustify, R2, AdvPages[i].Caption, '', Canvas.Font, AdvPages[i].Enabled, False, FAntiAlias, tpTop, False, TabSettings.WordWrap);
            R2.Right := R2.Right - fdw;
          end
          else if (AdvPages[i].WideCaption <> '') then
          begin
            R2 := Rect(0,0, 1000, 100);
            //R2 := DrawVistaText(Canvas, taLeftJustify, R2, '', AdvPages[i].WideCaption, Canvas.Font, Enabled, False, FAntiAlias, TabPosition, Ellipsis);
            (*
            DrawTextW(Canvas.Handle, PWideChar(AdvPages[i].WideCaption), -1, R2, DT_CALCRECT or DT_LEFT or DTSTYLE);
            *)
            //R2.Right := WideCanvasTextWidth(Canvas, AdvPages[i].WideCaption);

            R2 := DrawVistaText(Canvas, taLeftJustify, R2, '', AdvPages[i].WideCaption, Canvas.Font, AdvPages[i].Enabled, False, FAntiAlias, tpTop, False, TabSettings.WordWrap);
            R2.Right := R2.Right - fdw;
          end
          else
            R2 := Rect(0, 0, 0, 0);
          //R2 := DrawVistaText(Canvas, R2, AdvPages[i].Caption, Canvas.Font, AdvPages[PageIndex].Enabled, False);

          TbW := GetLeftRoundingOffset + TabSettings.LeftMargin + R2.Right + fdW + TabSettings.RightMargin + GetRightRoundingOffset + 3;

          if AdvPages[i].Enabled or AdvPages[i].DisabledPicture.Empty then
            Pic := AdvPages[i].Picture
          else
            Pic := AdvPages[i].DisabledPicture;

          if Assigned(Pic) and not Pic.Empty then
          begin
            Pic.GetImageSizes;
            //TbW := TbW + Pic.Width + ImgTxtSp;
            ImgW := Pic.Width;
          end
          else
          if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[i].ImageIndex >= 0) then
          begin
            if AdvPages[i].Enabled then
            begin
              if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Width + ImgTxtSp;
                ImgW := FImages.Width;
              end;
            end
            else
            begin
              if Assigned(FDisabledImages) then
              begin
                //TbW := TbW + FDisabledImages.Width + ImgTxtSp
                ImgW := FDisabledImages.Width;
              end
              else if Assigned(FImages) then
              begin
                //TbW := TbW + FImages.Width + ImgTxtSp;
                ImgW := FImages.Width;
              end;
            end;
          end;

          //TbW := TbW + ImgW;
          case TabSettings.ImagePosition of
            ipTop, ipBottom:
            begin
              // do nothing
            end;
            ipLeft, ipRight:
            begin
              TbW := TbW + ImgW;
              if ImgW > 0 then
               TbW := TbW + ImgTxtSp;
            end;
          end;

          if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[i].ShowClose) then
            TbW := Tbw + ButtonSettings.ButtonSize + 4;

          if AdvPages[I].ShowCheckBox then
            TbW := TbW + 20;

          if (TabSettings.Width > 0) then
            TbW := TabSettings.Width;

          {
          if ((CR.Left + TbW) > (CR.Right - FPageMargin)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = PageIndex) and (FTabScroller.Position <> PageIndex))) then
          begin
            if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
              Break;
          end;}

        if ((CR.Left + TbW) > (CR.Right - ADVPAGE_OFFSET)) then
          MultiRow := True;

        R := Rect(CR.Left, CR.Top, CR.Left + TbW, CR.Bottom);
        //if (i = TabIndex) then
          AdvPages[i].FTabRect := R;
        CR.Left := CR.Left + TbW + Sp;
      end
      else // TabPosition in [tpLeft, tpRight]
      begin
        //if FUseMaxSpace then
          //CR.Bottom := GetTabsArea.Bottom;

        if (AdvPages[i].Caption <> '') then
        begin
          R2 := Rect(0,0, 1000, 100);
          DrawText(Canvas.Handle,PChar(AdvPages[i].Caption),Length(AdvPages[i].Caption), R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE);
        end
        else if (AdvPages[i].WideCaption <> '') then
        begin
          R2 := Rect(0,0, 1000, 100);
          DrawTextW(Canvas.Handle, PWideChar(AdvPages[i].WideCaption), -1, R2, DT_CALCRECT or DT_LEFT or DT_SINGlELINE);
        end
        else
          R2 := Rect(0, 0, 0, 0);
        //R2 := DrawVistaText(Canvas, R2, AdvPages[i].Caption, Canvas.Font, AdvPages[PageIndex].Enabled, False);

        TbW := TabSettings.LeftMargin + R2.Right+ fdW + TabSettings.RightMargin;
        TbH := R2.Bottom;

        if AdvPages[i].Enabled or AdvPages[i].DisabledPicture.Empty then
          Pic := AdvPages[i].Picture
        else
          Pic := AdvPages[i].DisabledPicture;

        if Assigned(Pic) and not Pic.Empty then
        begin
          Pic.GetImageSizes;
          //TbW := TbW + Pic.Height + ImgTxtSp;
          //TbH := Max(TbH, Pic.Height);
          ImgW := Pic.Width;
          ImgH := Pic.Height;
        end
        else
        if (Assigned(FImages) or Assigned(DisabledImages)) and (AdvPages[i].ImageIndex >= 0) then
        begin
          if AdvPages[i].Enabled then
          begin
            if Assigned(FImages) then
            begin
              //TbW := TbW + FImages.Height + ImgTxtSp;
              //TbH := Max(TbH, FImages.Height);
              ImgW := FImages.Width;
              ImgH := FImages.Height;
            end;
          end
          else
          begin
            if Assigned(FDisabledImages) then
            begin
              //TbW := TbW + FDisabledImages.Height + ImgTxtSp;
              //TbH := Max(TbH, FDisabledImages.Height);
              ImgW := FDisabledImages.Width;
              ImgH := FDisabledImages.Height;
            end
            else if Assigned(FImages) then
            begin
              //TbW := TbW + FImages.Height + ImgTxtSp;
              //TbH := Max(TbH, FImages.Height);
              ImgW := FImages.Width;
              ImgH := FImages.Height;
            end;
          end;
        end;

        case TabSettings.ImagePosition of
          ipTop, ipBottom:
          begin
            TbH := Tbh + ImgH + ImgTxtSp;
          end;
          ipLeft, ipRight:
          begin
            TbW := TbW + ImgW + ImgTxtSp;
            TbH := Max(TbH, ImgH);
          end;
        end;

        TbH := TbH + 12; //TabSettings.RightMargin;

        if RotateTabLeftRight then
        begin
          TbW := TbW + GetLeftRoundingOffset + GetRightRoundingOffset;
          if (ButtonSettings.CloseButton and CloseOnTab and AdvPages[i].ShowClose) then
            TbW := Tbw + ButtonSettings.ButtonSize + 4;

          if AdvPages[I].ShowCheckBox then
            TbW := TbW + 20;

          if (TabSettings.Width > 0) then
            TbW := TabSettings.Width;
          {
          if ((CR.Top + TbW) > (CR.Bottom - FPageMargin)) and (not ConsiderTabScroller or (FTabScroller.Visible and(i = PageIndex) and (FTabScroller.Position <> PageIndex))) then
          begin
            if not ((fixTabs > 0) and (i <= fixTabs - 1) and (StartIndex = 0)) then
              Break;
          end;}

          if Tbh < -1 then  // dummy and to be deleted
            TbW := Tbw;

          if ((CR.Top + TbW) > (CR.Bottom - ADVPAGE_OFFSET)) then
            MultiRow := True;

          R := Rect(CR.Left, CR.Top, CR.Right, CR.Top + TbW);
          AdvPages[i].FTabRect := R;
          CR.Top := CR.Top + TbW + Sp;
        end;
      end;

    end;  // end of for loop

    SetLength(tbRows, AdvPageCount + 1);   // Max possible rows
    tbRowCount := 0;
    if not multiRow then
    begin
      FTabRowSize := TabSettings.Height;
      FMultiLineRowCount := 1;
      SetLength(FTabRows, FMultiLineRowCount + 1);
      FTabRows[1].X := -1;
      j := AdvPageCount - 1;
      for i := 0 to AdvPageCount - 1 do
      begin
        if not AdvPages[i].TabVisible then
          Continue;

        if (FTabRows[1].X = -1) then
          FTabRows[1].X := i;
        j := i;
      end;

      FTabRows[1].Y := j;
    end
    else  // in case multi row
    begin
      i1 := 0;
      i2 := 0;
      j := 0;
      acvTabR := 1;
      Sp := 0;
      for i := 0 to AdvPageCount - 1 do
      begin
        if not AdvPages[i].TabVisible then
          Continue;

        if (i = ActivePageIndex) then
          acvTabR := tbRowCount + 1;   // note active tab row

        if (TabPosition in [tpTop, tpBottom]) then
        begin
          if (j + sp + AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left < avlspace) or (i = 0) then
          begin
            j := j + sp + AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left;
            i2 := i;
            Sp := FTabSettings.Spacing;
          end
          else
          begin
           // if (j = 0) then   // in case only one tab in row
           //   i2 := i;

            SetLength(tbRows, tbRowCount + 2);

            tbRows[tbRowCount + 1].X := i1;     // tab row start index
            tbRows[tbRowCount + 1].Y := i2;     // tab row end index
            tbRowCount := tbRowCount + 1;
            i1 := i;

            if (i = ActivePageIndex) then
              acvTabR := tbRowCount + 1;   // note active tab row

            if (i = AdvPageCount - 1) then  // last tab
            begin
              SetLength(tbRows, tbRowCount + 2);
              tbRows[tbRowCount + 1].X := i1;     // tab row start index
              tbRows[tbRowCount + 1].Y := i;     // tab row end index
              tbRowCount := tbRowCount + 1;
              j := 0;
            end
            else
            begin
              j := AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left;
              i2 := i;
              Sp := FTabSettings.Spacing;
            end;
          end;
        end
        else // TabPosition in [tpLeft, tpRight]
        begin
          if (j + sp + AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top < avlspace) or (i = 0) then
          begin
            j := j + sp + AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top;
            i2 := i;
            Sp := FTabSettings.Spacing;
          end
          else
          begin
            //if (j = 0) then   // in case only one tab in row
            //  i2 := i;

            SetLength(tbRows, tbRowCount + 2);

            tbRows[tbRowCount + 1].X := i1;     // tab row start index
            tbRows[tbRowCount + 1].Y := i2;     // tab row end index
            tbRowCount := tbRowCount + 1;
            i1 := i;

            if (i = ActivePageIndex) then
              acvTabR := tbRowCount + 1;   // note active tab row

            if (i = AdvPageCount - 1) then  // last tab
            begin
              SetLength(tbRows, tbRowCount + 2);
              tbRows[tbRowCount + 1].X := i1;     // tab row start index
              tbRows[tbRowCount + 1].Y := i;     // tab row end index
              tbRowCount := tbRowCount + 1;
              j := 0;
            end
            else
            begin
              j := AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top;
              i2 := i;
              Sp := FTabSettings.Spacing;
            end;
          end;
        end;   // TabPosition in [tpLeft, tpRight]

      end;

      if (j > 0) then
      begin
        SetLength(tbRows, tbRowCount + 2);
        tbRows[tbRowCount + 1].X := i1;     // tab row start index
        tbRows[tbRowCount + 1].Y := i2;     // tab row end index
        tbRowCount := tbRowCount + 1;
      end;

      FMultiLineRowCount := tbRowCount;

      SetLength(FTabRows, FMultiLineRowCount + 1);
      i := 1;
      for j := 1 to tbRowCount do
      begin
        if (j <> acvTabR) then
        begin
          FTabRows[i].X := tbRows[j].X;     // tab row start index
          FTabRows[i].Y := tbRows[j].Y;     // tab row end index
          i := i + 1;
        end;
      end;
      if (acvTabR >= 1) then
      begin
        FTabRows[tbRowCount].X := tbRows[acvTabR].X;     // tab row start index
        FTabRows[tbRowCount].Y := tbRows[acvTabR].Y;     // tab row end index
      end;

      Sp := FTabSettings.Spacing;
      if (TabPosition in [tpTop, tpBottom]) then
      begin
        R := TR;
        //R2 := TR;
        TbH := 0;
        for j := 1 to tbRowCount do
        begin
          //if (j <> acvTabR) then
          begin
            R2 := TR;
            TbH := 0;
            for i := FTabRows[j].X to FTabRows[j].Y do
            begin
              if not AdvPages[i].TabVisible then
                Continue;

              if TbH = 0 then
                TbH := AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top;
              AdvPages[i].FTabRect := Rect(R2.Left, R.Top, R2.Left + AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left, R.Top + TbH);
              R2.Left := AdvPages[i].FTabRect.Right + Sp;
            end;
            R.Top := R.Top + TbH + RwSpace;
            R.Bottom := R.Bottom + TbH + RwSpace;
          end;
        end;

        if TbH = 0 then
          TbH := TabSettings.Height;
        FTabRowSize := FMultiLineRowCount * Tbh + ((FMultiLineRowCount - 1) * RwSpace);
      end
      else  // (TabPosition in [tpLeft, tpRight]) then
      begin
        R := TR;
        TbW := 0;
        for j := 1 to tbRowCount do
        begin
          R2 := TR;
          TbW := 0;
          for i := FTabRows[j].X to FTabRows[j].Y do
          begin
            if not AdvPages[i].TabVisible then
              Continue;

            if TbW = 0 then
              TbW := AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left;
            AdvPages[i].FTabRect := Rect(R.Left, R2.Top, R.Left + TbW, R2.Top + AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top);
            R2.Top := AdvPages[i].FTabRect.Bottom + Sp;
          end;
          R.Left := R.Left + TbW + RwSpace;
          R.Right := R.Right + TbW + RwSpace;
        end;

        if TbW = 0 then
          TbW := TabSettings.Height;
        FTabRowSize := FMultiLineRowCount * TbW + ((FMultiLineRowCount - 1) * RwSpace);
      end;

    end; // if multirow

    SetAllPagesPosition;
  end;
  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateMultiLineActiveTab;
var
  i, j, acvTabR, TbH, tbW, sp, RwSpace: Integer;
  R, TR, R2: TRect;
  tbRows: array of TPoint;
begin
  if MultiLine and (FMultiLineRowCount > 1) and (ActivePageIndex >= 0) and (AdvPageCount > 0) then
  begin
    acvTabR := -1;
    sp := TabSettings.Spacing;

    if ShowNonSelectedTabs then
      RwSpace := 2   // Row space
    else
      RwSpace := 0;  // Row space

    for i := 1 to FMultiLineRowCount do
    begin
      if (ActivePageIndex >= FTabRows[i].X) and (ActivePageIndex <= FTabRows[i].Y) then
      begin
        acvTabR := i;
        Break;
      end;
    end;

    if (acvTabR <= 0) then    // invalid row index
      Exit;

    SetLength(tbRows, FMultiLineRowCount + 1);
    i := 1;
    for j := 1 to FMultiLineRowCount do
    begin
      if (j <> acvTabR) then
      begin
        tbRows[i].X := FTabRows[j].X;     // tab row start index
        tbRows[i].Y := FTabRows[j].Y;     // tab row end index
        i := i + 1;
      end;
    end;
    tbRows[FMultiLineRowCount].X := FTabRows[acvTabR].X;     // tab row start index
    tbRows[FMultiLineRowCount].Y := FTabRows[acvTabR].Y;     // tab row end index
    for j := 1 to FMultiLineRowCount do
    begin
      FTabRows[j].X := tbRows[j].X;     // tab row start index
      FTabRows[j].Y := tbRows[j].Y;     // tab row end index
    end;

    TR := GetTabsRect;
    if (TabPosition in [tpTop, tpBottom]) then
    begin
      R := TR;
      for j := 1 to FMultiLineRowCount do
      begin
        //if (j <> acvTabR) then
        begin
          R2 := TR;
          TbH := 0;
          for i := FTabRows[j].X to FTabRows[j].Y do
          begin
            if (i >= AdvPageCount) then
              Continue;

            if not AdvPages[i].TabVisible then
              Continue;

            if TbH = 0 then
              TbH := AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top;
            AdvPages[i].FTabRect := Rect(R2.Left, R.Top, R2.Left + AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left, R.Top + TbH);
            R2.Left := AdvPages[i].FTabRect.Right + sp;
          end;
          R.Top := R.Top + TbH + RwSpace;
          R.Bottom := R.Bottom + TbH + RwSpace;
        end;
      end;
    end
    else  // (TabPosition in [tpLeft, tpRight]) then
    begin
      R := TR;
      for j := 1 to FMultiLineRowCount do
      begin
        R2 := TR;
        TbW := 0;
        for i := FTabRows[j].X to FTabRows[j].Y do
        begin
          if (i >= AdvPageCount) then
            Continue;

          if not AdvPages[i].TabVisible then
            Continue;

          if TbW = 0 then
            TbW := AdvPages[i].FTabRect.Right - AdvPages[i].FTabRect.Left;
          AdvPages[i].FTabRect := Rect(R.Left, R2.Top, R.Left + TbW, R2.Top + AdvPages[i].FTabRect.Bottom - AdvPages[i].FTabRect.Top);
          R2.Top := AdvPages[i].FTabRect.Bottom + Sp;
        end;
        R.Left := R.Left + TbW + RwSpace;
        R.Right := R.Right + TbW + RwSpace;
      end;
    end;

  end; // if MultiLine

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  p: TPoint;
  Tab: Integer;
begin
  Tab := -1;
  if (csDesigning in ComponentState) then
  begin
    GetCursorPos(P);
    P := ScreenToClient(P);

    if PtInRect(GetTabsRect, p) and (GetAsyncKeyState(VK_LBUTTON) <> 0) then
    begin
      Tab := PTOnTab(P.X, P.Y);
      if (Tab >= 0) then
      begin
        // Select Tab
        //ActivePageIndex := Tab;
        Msg.Result := 1;
      end;
    end;

  end;

  if (Tab = -1) then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.InvalidateTab(PageIndex: Integer);
var
  R: TRect;
begin
  if (PageIndex >= 0) and (PageIndex < FAdvPages.Count) then
    R := GetTabRect(PageIndex)
  else
    R := GetTabsArea;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnEnterTab(PageIndex: Integer);
begin
  if not (csDesigning in ComponentState) and Assigned(OnTabMouseEnter) then
    FOnTabMouseEnter(Self, PageIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnExitTab(PageIndex: Integer);
begin
  if not (csDesigning in ComponentState) and Assigned(OnTabMouseLeave) then
    FOnTabMouseLeave(Self, PageIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetDisabledImages(
  const Value: TCustomImageList);
begin
  FDisabledImages := Value;
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetFixedTabs(Value: Integer);
begin
  if (FFixedTabs <> Value) then
  begin
    if (Value < 0) then
      Value := 0;

    if ((Value >= FAdvPages.Count) and not (csLoading in ComponentState)) then
      Value := 0;

    if (Value > 0) and MultiLine then
    begin
      MessageDlg('Can not have Fixed tabs with multiline.', mtInformation, [mbOK], 0);
      Exit;
    end;

    FFixedTabs := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FFixedTabs > 0 then
        FTabScroller.Position := 0;
      UpdateTabScroller;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateTabScroller;
var
  AnyTabVisible: Boolean;
  i, j, VisTabCount: Integer;

  function GetScrollMax: Integer;
  var
    i, j, LastVisTab: Integer;
    R: TRect;
  begin
    Result := 0;
    LastVisTab := 0;
    for i:= FAdvPages.Count -1 downto 0 do
    begin
      if AdvPages[i].TabVisible then
      begin
        LastVisTab := i;
        Break;
      end;
    end;

    if (LastVisTab <= 0) then
      Exit;

    i := 0;
    j := 0;
    while (i < FAdvPages.Count) do
    begin
      R := GetTabRect(i, LastVisTab, False);
      if (R.Left > -1) and (R.Right > -1) then
      begin
        Result := j; //i;
        if (FixedTabs > 0) and not (csDesigning in ComponentState) and (Result > 0) then
          Result := Result - FixedTabs;
        AnyTabVisible := True;

        for j := i to LastVisTab do
        begin
          if AdvPages[j].TabVisible then
            Inc(VisTabCount);
        end;
        Break;
      end;

      if AdvPages[i].TabVisible then
        inc(j);

      inc(i);
      if (i >= FAdvPages.Count) then
        Break;

      //if AdvPages[i].TabVisible then
        //inc(j);
      FTabScroller.Visible := True;  // just to be counted in calculation
    end;
  end;

begin
  if  MultiLine then
    Exit;

  AnyTabVisible := False;
  VisTabCount := 0;
  FUseMaxSpace := False;
  if not ButtonSettings.ScrollButtonsAlways then
    FTabScroller.Visible := False;
  FTabScroller.Min := 0;
  FTabScroller.Max := GetScrollMax;
  if (FTabScroller.Max = 0) and not AnyTabVisible then
  begin
    if (FixedTabs > 0) and not (csDesigning in ComponentState) then
    begin
      FTabScroller.Max := max(0, GetVisibleTabCount - FixedTabs);
    end
    else
    begin
      FUseMaxSpace := True;
      FTabScroller.Max := GetScrollMax;
      if (FTabScroller.Max = 0) then
      begin
        //FUseMaxSpace := False;
        FTabScroller.Max := max(0, GetVisibleTabCount-1);
      end;
    end;
  end
  else
  if (AnyTabVisible) then
  begin
    if (FTabScroller.Position > FTabScroller.Max) then
      FTabScroller.Position := FTabScroller.Max;

    if (FixedTabs > 0) and not (csDesigning in ComponentState) then
    begin
      j := 0;
      for i:= 0 to FAdvPages.Count -1 do
      begin
        if AdvPages[i].TabVisible and (I > FixedTabs - 1) then
        begin
          inc(j);
          if (j > FTabScroller.Position) then
          begin
            j := i;
            Break;
          end;
        end;
      end;

      if (GetTabRect(j).Right = -1) and (FTabScroller.Position > 0) and (TabSettings.EndMargin = 0) then
      begin
        FUseMaxSpace := True;
        FTabScroller.Max := GetScrollMax;
        if (FTabScroller.Max = 0) then
          FUseMaxSpace := False;
      end;
    end
    else
    begin
      j := 0;
      for i:= 0 to FAdvPages.Count -1 do
      begin
        if AdvPages[i].TabVisible then
        begin
          inc(j);
          if (j >= FTabScroller.Position) then
            Break;
        end;
      end;
                                         // FF: 2nd tab overlaping scroll buttons
      if (GetTabRect(j).Right = -1) and (FTabScroller.Position > 0) and (TabSettings.EndMargin = 0) then
      begin
        FUseMaxSpace := True;
        FTabScroller.Max := GetScrollMax;
        if (FTabScroller.Max = 0) then
          FUseMaxSpace := False;
      end;
    end;
  end;
  FTabScroller.Visible := (FTabScroller.Min < FTabScroller.Max) or ButtonSettings.ScrollButtonsAlways;
  if (FTabScroller.Position > FTabScroller.Max) then
    FTabScroller.Position := FTabScroller.Max;

  InitializeAndUpdateButtons;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PtOnTabScrollLeftBtn(X, Y: integer): Boolean;
var
  P: TPoint;
begin
  P := Point(X, Y);
  Result := PtInRect(GetTabScrollerLeftRect, P);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PtOnTabScrollRightBtn(X, Y: integer): Boolean;
var
  P: TPoint;
begin
  P := Point(X, Y);
  Result := PtInRect(GetTabScrollerRightRect, P);
end;

//------------------------------------------------------------------------------
{
procedure TAdvOfficePager.DrawTabScrollBtnLeft;
var
  R: TRect;
  y: Integer;
  Clr: TColor;
begin
  if FTabScroller.Visible then
  begin
    R := GetTabScrollerLeftRect;
    y := 0;
    case (TabPosition) of
      tpTop: y := ((R.Bottom - R.Top) - 12) div 2;
      tpBottom: y := 4 + ((R.Bottom - R.Top) - 12) div 2;
      tpLeft: y := ((R.Right - R.Left) - 12) div 2;
      tpRight: y := 4 + ((R.Right - R.Left) - 12) div 2;
    end;

    Clr := FCurrentOfficePagerStyler.TabAppearance.TextColor;
    if FScrollerDownLeftBtn then
      Clr := FCurrentOfficePagerStyler.TabAppearance.ColorSelected
    else if FScrollerHoverLeftBtn then
      Clr := FCurrentOfficePagerStyler.TabAppearance.ColorMirrorHotTo;

    if not FTabScroller.CanGoBack then
      Clr := clGray;
      
    with Canvas do
    begin
      //Brush.color:= Color;
      //FillRect(Rect(0,0,12,13));
      //Brush.Style := bsClear;
      //Rectangle(R);

      Pen.Color:= Clr;
      if TabPosition in [tpTop, tpBottom] then
      begin
          // |
        MoveTo(R.left+9, R.Top+y+0);
        LineTo(R.left+9, R.Top+y+8);
          // /
        MoveTo(R.left+9, R.Top+y+0);
        LineTo(R.left+5, R.Top+y+4);
          // \
        MoveTo(R.left+9, R.Top+y+8);
        LineTo(R.left+4, R.Top+y+3);
          // Fill arrow |
        MoveTo(R.left+8, R.Top+y+2);
        LineTo(R.left+8, R.Top+y+7);
        MoveTo(R.left+7, R.Top+y+3);
        LineTo(R.left+7, R.Top+y+6);
        Pixels[R.left+6, R.Top+y+4]:= Pen.Color;
      end
      else
      begin
          // |
        MoveTo(R.left+y, R.Top+9);
        LineTo(R.left+y+8, R.Top+9);
          // /
        MoveTo(R.left+y, R.Top+9);
        LineTo(R.left+y+4, R.Top+5);
          // \
        MoveTo(R.left+y+8, R.Top+9);
        LineTo(R.left+y+3, R.Top+4);
          // Fill arrow |
        MoveTo(R.left+y+2, R.Top+8);
        LineTo(R.left+y+7, R.Top+8);
        MoveTo(R.left+y+3, R.Top+7);
        LineTo(R.left+y+6, R.Top+7);
        Pixels[R.left+y+4, R.Top+6]:= Pen.Color;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DrawTabScrollBtnRight;
var
  R: TRect;
  y: Integer;
  Clr: TColor;
begin
  if FTabScroller.Visible then
  begin
    R := GetTabScrollerRightRect;
    y := 0;
    case (TabPosition) of
      tpTop: y := ((R.Bottom - R.Top) - 12) div 2;
      tpBottom: y := 4 +((R.Bottom - R.Top) - 12) div 2;
      tpLeft: y := ((R.Right - R.Left) - 12) div 2;
      tpRight: y := 4 +((R.Right - R.Left) - 12) div 2;
    end;

    Clr := FCurrentOfficePagerStyler.TabAppearance.TextColor;
    if FScrollerDownRightBtn then
      Clr := FCurrentOfficePagerStyler.TabAppearance.ColorSelected
    else if FScrollerHoverRightBtn then
      Clr := FCurrentOfficePagerStyler.TabAppearance.ColorMirrorHotTo;

    if not FTabScroller.CanGoForward then
      Clr := clGray;

    with Canvas do
    begin
      //Brush.color:= Color;
      //FillRect(Rect(12,0,24,13));
      //Brush.Style := bsClear;
      //Rectangle(R);

      Pen.Color:= Clr;
      if TabPosition in [tpTop, tpBottom] then
      begin
          // |
        MoveTo(R.Left+3, R.Top+y);
        LineTo(R.Left+3, R.Top+y+8);
          // \
        MoveTo(R.Left+3, R.Top+y);
        LineTo(R.Left+7, R.Top+y+4);
          // /
        MoveTo(R.Left+3, R.Top+y+8);
        LineTo(R.Left+8, R.Top+y+3);
          // Fill Arrow |
        MoveTo(R.Left+4, R.Top+y+2);
        LineTo(R.Left+4, R.Top+y+7);
        MoveTo(R.Left+5, R.Top+y+3);
        LineTo(R.Left+5, R.Top+y+6);
        Pixels[R.Left+6, R.Top+y+4]:= Pen.Color;
      end
      else
      begin
          // |
        MoveTo(R.Left+y, R.Top+3);
        LineTo(R.Left+y+8, R.Top+3);
          // \
        MoveTo(R.Left+y, R.Top+3);
        LineTo(R.Left+y+4, R.Top+7);
          // /
        MoveTo(R.Left+y+8, R.Top+3);
        LineTo(R.Left+y+3, R.Top+8);
          // Fill Arrow |
        MoveTo(R.Left+y+2, R.Top+4);
        LineTo(R.Left+y+7, R.Top+4);
        MoveTo(R.Left+y+3, R.Top+5);
        LineTo(R.Left+y+6, R.Top+5);
        Pixels[R.Left+y+4, R.Top+6]:= Pen.Color;
      end;
    end;
  end;
end;
}
//------------------------------------------------------------------------------

procedure TAdvOfficePager.DrawTabScrollButtons;
var
  R: TRect;
  g: TGPGraphics;
  gppen: TGPPen;
  PageAppearance: TVistaBackground;
begin
  //DrawTabScrollBtnLeft;
  //DrawTabScrollBtnRight;

  if (FTabScroller.Visible or (ButtonSettings.CloseButton and not CloseOnTab) or ButtonSettings.PageListButton) then
  begin
    R := GetButtonsRect;
    if Assigned(FPageAppearance) and FItones then
      PageAppearance := FPageAppearance
    else
      PageAppearance := FCurrentOfficePagerStyler.PageAppearance;

    {case (TabPosition) of
      tpTop, tpBottom:
      begin
        R.Left := R.Right;
        R.Right := ClientRect.Right;
      end;
      tpLeft, tpRight:
      begin
        R.Top := R.Bottom;
        R.Bottom := ClientRect.Bottom;
      end;
    end; }

    if TabSettings.ButtonBorder then
    begin
      if IsGlass then
      begin
        g := TGPGraphics.Create(Canvas.Handle);
        g.SetSmoothingMode(SmoothingModeAntiAlias);
        gpPen := TGPPen.Create(ColorToARGB(PageAppearance.BorderColor),1);


        if (TabSettings.Rounding = 0) then
        begin
          DrawRoundRect(g, gppen, R.Left, R.Top, (R.Right - R.Left) - 1, (R.Bottom - R.Top) - 1, 0);
        end
        else
        begin
          if FItones then
            DrawRoundRect(g, gppen, R.Left, R.Top, (R.Right - R.Left) - 1, (R.Bottom - R.Top) - 1, 1)
          else
            DrawRoundRect(g, gppen, R.Left, R.Top, (R.Right - R.Left) - 1, (R.Bottom - R.Top) - 1, 3);
        end;
      end
      else
      begin
        Canvas.Brush.Style := bsClear;
        Canvas.Pen.Color := PageAppearance.BorderColor;
        if FItones or (TabSettings.Rounding = 0) then
          Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom)
        else
          Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 5, 5);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ScrollFirstBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoBack then
    begin
      FTabScroller.Position := FTabScroller.Min;
      InvalidateTab(-1);
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;
end;

procedure TAdvOfficePager.ScrollLastBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoForward then
    begin
      FTabScroller.Position := FTabScroller.Max;
      InvalidateTab(-1);
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;
end;

procedure TAdvOfficePager.ScrollLeftBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoBack then
    begin
      FTabScroller.Position := FTabScroller.Position - 1;
      InvalidateTab(-1);
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ScrollRightBtnClick;
begin
  if FTabScroller.Visible then
  begin
    if FTabScroller.CanGoForward then
    begin
      FTabScroller.Position := FTabScroller.Position + 1;
      InvalidateTab(-1);
      if ActivePage <> nil then
        ActivePage.Invalidate;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabScrollerFirstRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetTabScrollerRect;
    case TabPosition of
      tpTop, tpBottom:
      begin
        Result.Left := Result.Left + 3;
        Result.Right := Result.Left + ButtonSettings.ButtonSize; // (Result.Right - Result.Left) div 2 - 1;
      end;
      tpLeft, tpRight:
      begin
        //Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2 - 1;
        Result.Top := Result.Top + 3;
        Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
      end;
    end;
  end;
end;

function TAdvOfficePager.GetTabScrollerLeftRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    if ButtonSettings.FirstButton then
    begin
      Result := GetTabScrollerFirstRect;
      case TabPosition of
        tpTop, tpBottom:
        begin
          Result.Left := Result.Right + 2;
          Result.Right := Result.Left + ButtonSettings.ButtonSize; // (Result.Right - Result.Left) div 2 - 1;
        end;
        tpLeft, tpRight:
        begin
          //Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2 - 1;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
    end
    else
    begin
      Result := GetTabScrollerRect;
      case TabPosition of
        tpTop, tpBottom:
        begin
          Result.Left := Result.Left + 2;
          Result.Right := Result.Left + ButtonSettings.ButtonSize; // (Result.Right - Result.Left) div 2 - 1;
        end;
        tpLeft, tpRight:
        begin
          //Result.Bottom := Result.Top + (Result.Bottom - Result.Top) div 2 - 1;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetCloseButtonRect(PageIndex: integer): TRect;
var
  i: Integer;
  cbr, tr: TRect;
  ip: TPoint;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.CloseButton then
  begin
    if CloseOnTab and (ShowCloseOnNonSelectedTabs or ((ActivePageIndex = PageIndex) and not ShowCloseOnNonSelectedTabs)) then
    begin
      case TabPosition of
        tpTop:
        begin
          Result := GetTabRect(PageIndex);

          if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (PageIndex >= 0) then
          begin
            GetCloseBtnImageAndTextRect(PageIndex, cbr, tr, ip);
            Result.Left := cbr.Left;
          end
          else
          begin
            if (CloseOnTabPosition = cpRight) then
              Result.Left := Result.Right - ButtonSettings.ButtonSize - 4 - GetRightRoundingOffset
            else //CloseOnTabPosition = cpLeft
              Result.Left := Result.Left + 4 + GetLeftRoundingOffset
          end;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Bottom := Result.Bottom - 5;
          Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
        end;
        tpBottom:
        begin
          Result := GetTabRect(PageIndex);
          if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (PageIndex >= 0) then
          begin
            GetCloseBtnImageAndTextRect(PageIndex, cbr, tr, ip);
            Result.Left := cbr.Left;
          end
          else
          begin
            if (CloseOnTabPosition = cpRight) then
              Result.Left := Result.Right - ButtonSettings.ButtonSize - 4 - GetRightRoundingOffset
            else //CloseOnTabPosition = cpLeft
              Result.Left := Result.Left + 4 + GetLeftRoundingOffset;
          end;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Top := Result.Top + 5;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
        tpLeft:
        begin
          if RotateTabLeftRight then
          begin
            Result := GetTabRect(PageIndex);
            Result.Right := Result.Right - 5;
            Result.Left := Result.Right - ButtonSettings.ButtonSize;
            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (PageIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(PageIndex, cbr, tr, ip);
              Result.Top := cbr.Top;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Top := Result.Top + 4 + GetRightRoundingOffset
              else
                Result.Top := Result.Bottom - ButtonSettings.ButtonSize - 4 - GetLeftRoundingOffset;
            end;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetTabRect(PageIndex);
            i := (Result.Bottom - Result.Top - ButtonSettings.ButtonSize) div 2;

            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (PageIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(PageIndex, cbr, tr, ip);
              Result.Left := cbr.Left;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Left := Result.Right - ButtonSettings.ButtonSize - 5
              else //CloseOnTabPosition = cpLeft
                Result.Left := Result.Left + 4;
            end;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Top := Result.Top + i;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end;
        end;
        tpRight:
        begin
          if RotateTabLeftRight then
          begin
            Result := GetTabRect(PageIndex);
            Result.Left := Result.Left + 5;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (PageIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(PageIndex, cbr, tr, ip);
              Result.Bottom := cbr.Bottom;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Bottom := Result.Bottom - 4 - GetRightRoundingOffset
              else
                Result.Bottom := Result.Top + ButtonSettings.ButtonSize  + 4 + GetLeftRoundingOffset;
            end;
            Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetTabRect(PageIndex);
            i := (Result.Bottom - Result.Top - ButtonSettings.ButtonSize) div 2;

            if (TabSettings.Width > 0) and (TabSettings.alignment = taCenter) and (PageIndex >= 0) then
            begin
              GetCloseBtnImageAndTextRect(PageIndex, cbr, tr, ip);
              Result.Left := cbr.Left;
            end
            else
            begin
              if (CloseOnTabPosition = cpRight) then
                Result.Left := Result.Right - ButtonSettings.ButtonSize - 3
              else //CloseOnTabPosition = cpLeft
                Result.Left := Result.Left + 5;
            end;

            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Top := Result.Top + i;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end;
        end;
      end;
    end
    else
    begin
      case TabPosition of
        tpTop:
        begin
          if ButtonSettings.PageListButton then
          begin
            Result := GetPageListRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Left := Result.Right + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Left := Result.Left + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Bottom := Result.Bottom - 5;
            Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
          end;
        end;
        tpBottom:
        begin
          if ButtonSettings.PageListButton then
          begin
            Result := GetPageListRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Left := Result.Right + 2;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Left := Result.Right + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Left := Result.Left + 3;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
            Result.Top := Result.Top + 5;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end;
        end;
        tpLeft:
        begin
          if ButtonSettings.PageListButton then
          begin
            Result := GetPageListRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Top := Result.Bottom + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Top := Result.Top + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
            Result.Right := Result.Right - 5;
            Result.Left := Result.Right - ButtonSettings.ButtonSize;
          end;
        end;
        tpRight:
        begin
          if ButtonSettings.PageListButton then
          begin
            Result := GetPageListRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
          begin
            Result := GetClosedListButtonRect;
            Result.Top := Result.Bottom + 2;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else if FTabScroller.Visible then
          begin
            Result := GetTabScrollerLastRect;
            Result.Top := Result.Bottom + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          end
          else
          begin
            Result := GetButtonsRect; // GetTabsRect;
            Result.Top := Result.Top + 3;
            Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
            Result.Left := Result.Left + 5;
            Result.Right := Result.Left + ButtonSettings.ButtonSize;
          end;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetClosedListButtonRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
  begin
    case TabPosition of
      tpTop:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect; // GetTabScrollerRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect; // GetTabsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Bottom := Result.Bottom - 5;
          Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
        end;
      end;
      tpBottom:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Top := Result.Top + 5;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
      tpLeft:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Right := Result.Right - 5;
          Result.Left := Result.Right - ButtonSettings.ButtonSize;
        end;
      end;
      tpRight:
      begin
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Left := Result.Left + 5;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetPageListRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.PageListButton then
  begin
    case TabPosition of
      tpTop:
      begin
        if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect; // GetTabScrollerRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect; // GetTabsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Bottom := Result.Bottom - 5;
          Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
        end;
      end;
      tpBottom:
      begin
        if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Left := Result.Right + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Left := Result.Left + 3;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
          Result.Top := Result.Top + 5;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
      tpLeft:
      begin
        if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Right := Result.Right - 5;
          Result.Left := Result.Right - ButtonSettings.ButtonSize;
        end;
      end;
      tpRight:
      begin
        if ButtonSettings.ClosedListButton and (FClosedPageList.Count > 0) then
        begin
          Result := GetClosedListButtonRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        if FTabScroller.Visible then
        begin
          Result := GetTabScrollerLastRect;
          Result.Top := Result.Bottom + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end
        else
        begin
          Result := GetButtonsRect;
          Result.Top := Result.Top + 3;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
          Result.Left := Result.Left + 5;
          Result.Right := Result.Left + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabScrollerRect: TRect;
var
  cnt: integer;
begin
  cnt := 0;
  if ButtonSettings.FirstButton and ButtonSettings.LastButton then
    cnt := 4
  else if ButtonSettings.FirstButton or ButtonSettings.LastButton then
    cnt := 3
  else if not ButtonSettings.FirstButton and not ButtonSettings.LastButton then
    cnt := 2;

  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetButtonsRect;
    case TabPosition of
      tpTop:
      begin
        Result.Right := Result.Left + 3 +(ButtonSettings.ButtonSize*cnt + 2+ 3);// SCROLLER_SIZE;
        Result.Bottom := Result.Bottom - 5;
        Result.Top := Result.Bottom - ButtonSettings.ButtonSize;
      end;
      tpBottom:
      begin
        Result.Right := Result.Left + 3 +(ButtonSettings.ButtonSize*cnt + 2+ 3);// SCROLLER_SIZE;
        Result.Top := Result.Top + 5;
        Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
      end;
      tpLeft:
      begin
        Result.Bottom := Result.Top + 3 +(ButtonSettings.ButtonSize*cnt + 2+ 3);
        Result.Right := Result.Right - 5;
        Result.Left := Result.Right - ButtonSettings.ButtonSize;
      end;
      tpRight:
      begin
        Result.Bottom := Result.Top + 3 +(ButtonSettings.ButtonSize*cnt + 2+ 3);
        Result.Left := Result.Left + 5;
        Result.Right := Result.Left + ButtonSettings.ButtonSize;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetTabScrollerRightRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetTabScrollerLeftRect;// GetTabScrollerRect;
    case TabPosition of
      tpTop, tpBottom:
      begin
        Result.Left := Result.Right + 2;
        Result.Right := Result.Left + ButtonSettings.ButtonSize; // ((Result.Right - Result.Left) div 2);
      end;
      tpLeft, tpRight:
      begin
        //Result.Top := Result.Top + ((Result.Bottom - Result.Top) div 2);
        Result.Top := Result.Bottom + 2;
        Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
      end;
    end;
  end;
end;

function TAdvOfficePager.GetTabScrollerLastRect: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if FTabScroller.Visible then
  begin
    Result := GetTabScrollerRightRect;// GetTabScrollerRect;
    if ButtonSettings.LastButton then
    begin
      case TabPosition of
        tpTop, tpBottom:
        begin
          Result.Left := Result.Right + 2;
          Result.Right := Result.Left + ButtonSettings.ButtonSize; // ((Result.Right - Result.Left) div 2);
        end;
        tpLeft, tpRight:
        begin
          //Result.Top := Result.Top + ((Result.Bottom - Result.Top) div 2);
          Result.Top := Result.Bottom + 2;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.PtOnInsertButton(X, Y: Integer): Boolean;
begin
  Result := false;
  if ButtonSettings.ShowInsertButton then
  begin
    Result := PtInRect(GetInsertButtonRect, Point(X, Y));
    //PtInRegion()
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DoInsertButtonClick;
begin
  ActivePageIndex := AddAdvPage('AdvOfficePage' + Inttostr(FAdvPages.Count));
  if Assigned(OnInsertPage) then
    OnInsertPage(Self, AdvPages[ActivePageIndex]);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.InvalidateInsertButton;
var
  R: TRect;
begin
  R := GetInsertButtonRect;
  InvalidateRect(Handle, @R, True);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetInsertButtonRect: TRect;
var
  gap: Integer;
  R: TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if ButtonSettings.ShowInsertButton then
  begin
    gap := 0;
    R := GetTabRect(GetLastDisplayTab);
    if (R.Left < 0) or (R.Right < 0) then
    begin  // No tab is displayed
      R := GetTabsRect;
      case TabPosition of
        tpTop:
        begin
          R.Right := R.Left;
        end;
        tpBottom:
        begin
          R.Right := R.Left;
        end;
        tpLeft:
        begin
          R.Bottom := R.Top;
        end;
        tpRight:
        begin
          R.Bottom := R.Top;
        end;
      end;
    end;
    case TabPosition of
      tpTop:
      begin
        Result.Left := R.Right + gap;
        Result.Right := Result.Left + ButtonSettings.ButtonSize + 6;
        Result.Top := R.Top + 5;
        Result.Bottom := R.Bottom - 8;
      end;
      tpBottom:
      begin
        Result.Left := R.Right + gap;
        Result.Right := Result.Left + ButtonSettings.ButtonSize + 6;
        Result.Top := R.Top + 8;
        Result.Bottom := R.Bottom - 5;
      end;
      tpLeft:
      begin
        if RotateTabLeftRight then
        begin
          Result.Left := R.Left + 5;
          Result.Right := R.Right - 8;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end
        else
        begin
          Result.Right := R.Right - 8;
          Result.Left := Result.Right - ButtonSettings.ButtonSize - 1;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end;
      end;
      tpRight:
      begin
        if RotateTabLeftRight then
        begin
          Result.Left := R.Left + 8;
          Result.Right := R.Right - 5;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end
        else
        begin
          Result.Left := R.Left + 8;
          Result.Right := Result.Left + ButtonSettings.ButtonSize + 1;
          Result.Top := R.Bottom + gap;
          Result.Bottom := Result.Top + ButtonSettings.ButtonSize + 6;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetLastDisplayTab: Integer;
var
  i: Integer;
  R: TRect;
begin
  Result := -1;
  for i:= FAdvPages.Count -1 downto 0 do
  begin
    if AdvPages[i].TabVisible then
    begin
      R := GetTabRect(i);
      if (R.Left > -1) and (R.Right > -1) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.GlassChanged;
begin

  if TabSettings.Glass then
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Transparent := True;
      {$IFDEF DELPHI2007_LVL}
      if (Align in [alTop, alBottom, alClient]) and (Parent is TCustomForm) then
      begin
        TCustomForm(Parent).GlassFrame.Top := 27;
        TCustomForm(Parent).GlassFrame.Enabled := True;
      end;
      {$ENDIF}
    end;
  end
  else
  begin
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Transparent := False;
      {$IFDEF DELPHI2007_LVL}
      if (Align in [alTop, alBottom, alClient]) and (Parent is TCustomForm) then
      begin
        if (TCustomForm(Parent).GlassFrame.Top = 27) then
        begin
          TCustomForm(Parent).GlassFrame.Top := 0;
          TCustomForm(Parent).GlassFrame.Enabled := False;
        end;
      end;
      {$ENDIF}
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMHintShow(var Message: TMessage);
var
  PHI: PHintInfo;
begin
  PHI := TCMHintShow(Message).HintInfo;

  if PtOnInsertButton(PHI.CursorPos.X, PHI.CursorPos.Y) then
    PHI^.HintStr := ButtonSettings.InsertButtonHint
  else
    if ShowTabHint then
    begin
      if (FHintPageIndex >= 0) then
      begin
        PHI^.HintStr := AdvPages[FHintPageIndex].TabHint;
      end;
    end
    else
      PHI^.HintStr := '';
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnTabSettingsChanged(Sender: TObject);
begin
  SetPageValidCache(false);
  SetAllPagesPosition;
  InitializeAndUpdateButtons;
  UpdateTabScroller;
  UpdateMultiLineTabs;
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateMe(PropID: integer);
begin
  if (csDesigning in ComponentState) and Assigned(FCurrentOfficePagerStyler) then
  begin
    UpdatePageAppearanceOfPages(FCurrentOfficePagerStyler.PageAppearance);
    UpdateTabAppearanceOfPages(FCurrentOfficePagerStyler.TabAppearance);
  end;

  SetPageValidCache(false);
  Invalidate;

  if Assigned(ActivePage) then
    ActivePage.Invalidate;

  case PropID of
    2, 4: InitializeAndUpdateButtons;
    5: OnTabSettingsChanged(self);
  end;

  if FITones then
  begin
    FItones := False;
    FTabRoundEdges := True;
    FShow3D := True;
    FShadow := True;
    InitializeAndUpdateButtons;
  end;

  if Assigned(AdvOfficePagerStyler) then
  begin
    TabSettings.Rounding := AdvOfficePagerStyler.TabRounding;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

{  pt := ScreenToClient(point(msg.xpos,msg.ypos));

  if (FCaption.Visible) and PtInRect(GetCaptionRect, pt)
     and (Msg.Result = htClient) and FCanMove then
  begin
    //MouseMove([],pt.X,pt.Y);

    Msg.Result := htCaption;
    //FInMove := true;

    SetWindowPos(GetParentForm(Self).Handle, HWND_TOP,0,0,0,0,  SWP_NOMOVE or SWP_NOSIZE);
  end;  }
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMPaint(var Message: TWMPaint);
{$IFDEF DELPHI2007_LVL}
var
  DC, MemDC: HDC;
  PS: TPaintStruct;
  PaintBuffer: HPAINTBUFFER;
{$ENDIF}
begin
  {$IFNDEF DELPHI2007_LVL}
  inherited;
  {$ENDIF}

  {$IFDEF DELPHI2007_LVL}
  if not IsGlass or not DwmCompositionEnabled or not Transparent then
    inherited
  else
  begin
    DC := BeginPaint(Handle, PS);
    try
      PaintBuffer := BeginBufferedPaint(DC, PS.rcPaint, BPBF_COMPOSITED, nil, MemDC);
      if PaintBuffer <> 0 then
        try
          Perform(WM_ERASEBKGND, MemDC, MemDC);

          BufferedPaintMakeOpaque(PaintBuffer, @PS.rcPaint);
          Perform(WM_PRINTCLIENT, MemDC, PRF_CLIENT);
        finally
          EndBufferedPaint(PaintBuffer, True);
        end;
    finally
      EndPaint(Handle, PS);
    end;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetTabPosition(const Value: TTabPosition);
begin
  if (FTabPosition <> Value) then
  begin
    FTabPosition := Value;
    SetPageValidCache(false);
    SetAllPagesPosition;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;

    if Assigned(FArrow) then
      FreeAndNil(FArrow);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetTabReorderIndicatorColor(const Value: TColor);
begin
  if (FTabReorderIndicatorColor <> Value) then
  begin
    FTabReorderIndicatorColor := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetTabRoundEdges(const Value: Boolean);
begin
  FTabRoundEdges := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetShadow(const Value: Boolean);
begin
  FShadow := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetShow3D(const Value: Boolean);
begin
  FShow3D := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMDialogChar(var Message: TCMDialogChar);
var
  I: Integer;
begin
  for I:= 0 to FAdvPages.Count-1 do
  begin
    if AdvPages[I].Caption <> '' then
    begin
      if IsAccel(Message.CharCode, AdvPages[I].Caption) and CanShowTab(I) and CanFocus then
      begin
        Message.Result := 1;
        ActivePageIndex := I;
        Exit;
      end;
    end
    else
    begin
      if IsAccel(Message.CharCode, AdvPages[I].WideCaption) and CanShowTab(I) and CanFocus then
      begin
        Message.Result := 1;
        ActivePageIndex := I;
        Exit;
      end;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CanShowTab(PageIndex: Integer): Boolean;
begin
  Result := (PageIndex >= 0) and (PageIndex < FAdvPages.Count) and (AdvPages[PageIndex].TabVisible);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetAntiAlias(const Value: TAntiAlias);
begin
  FAntiAlias := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetButtonSettings(
  const Value: TPageButtonSettings);
begin
  FButtonSettings.Assign(Value);
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnButtonSettingChanged(Sender: TObject);
begin
  if FButtonSettings.ClosedListButton and not (csDesigning in ComponentState) then
  begin
    CreateClosedListButton;
    UpdateClosedListButton;
  end;

  UpdateTabScroller;
  UpdateMultiLineTabs;
  Invalidate;
  if (ActivePage <> nil) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetPageListMenu(const Value: TPopupMenu);
begin
  FPageListMenu := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetPageMargin(const Value: integer);
begin
  if FPageMargin <> Value then
  begin
    FPageMargin := Value;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetDockClientFromMousePos(MousePos: TPoint): TControl;
var
  Page: TAdvOfficePage;
  Tab: integer;
begin
  Result := nil;
  if DockSite then
  begin
    Tab := PTOnTab(mousepos.X, mousepos.Y);

    if (Tab >= 0) and (Tab < AdvPageCount) then
    begin
      Page := AdvPages[Tab];
      if Page.ControlCount > 0 then
      begin
        Result := Page.Controls[0];
        if Result.HostDockSite <> Self then Result := nil;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetFloatingPageCount: integer;
begin
  Result := 0;
  if Assigned(FFloatingPages) then
    Result := FFloatingPages.Count;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetFloatingPages(index: integer): TAdvOfficePage;
begin
  Result := nil;

  if Assigned(FFloatingPages) and (index < FFloatingPages.Count) then
  begin
    Result := TAdvOfficePage(FFloatingPages[index]);
  end;

end;

//------------------------------------------------------------------------------
procedure TAdvOfficePager.SetCloseOnTab(const Value: Boolean);
begin
  if (FCloseOnTab <> Value) then
  begin
    FCloseOnTab := Value;
    UpdateTabScroller;
    UpdateMultiLineTabs;
    Invalidate;
    if (ActivePage <> nil) then
      ActivePage.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetCloseOnTabPosition(
  const Value: TCloseOnTabPos);
begin
  if (FCloseOnTabPosition <> Value) then
  begin
    FCloseOnTabPosition := Value;
    InitializeAndUpdateButtons;
    if (ActivePage <> nil) then
      ActivePage.Invalidate;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetGlowButtonColorTones(
  AdvGlowButton: TAdvGlowButton);
begin
  if not Assigned(AdvGlowButton) or not Assigned(FGlowButtonAppearance) then
    Exit;

  AdvGlowButton.Appearance.Assign(FGlowButtonAppearance);
  AdvGlowButton.Rounded := false;
  //AdvGlowButton.Font.Color := ATones.Background.TextColor;
  AdvGlowButton.Font.Name := GetMetroFont;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetColorTones(ATones: TColorTones);
begin
  FTones := ATones;
  FItones := True;
  TabRoundEdges := False;
  Show3D := False;
  Shadow := False;

  if not Assigned(FPageAppearance) then
    FPageAppearance := TVistaBackground.Create;

  FPageAppearance.Color := ATones.Background.BrushColor;
  FPageAppearance.BorderColor := ATones.Selected.BorderColor;
  FPageAppearance.ColorTo := clNone;
  FPageAppearance.ColorMirror := clNone;
  FPageAppearance.ColorMirrorTo := clNone;

  if not Assigned(FTabAppearance) then
    FTabAppearance := TTabAppearance.Create;

  FTabAppearance.ClearValues;
  FTabAppearance.BackGround.Color := ATones.Selected.BrushColor;
  FTabAppearance.BackGround.ColorTo := clNone;
  FTabAppearance.Color := FTabAppearance.BackGround.Color;
  FTabAppearance.TextColor := ATones.Background.TextColor;
  FTabAppearance.ColorSelected := ATones.Background.BrushColor;
  FTabAppearance.BorderColorSelected := ATones.Background.BrushColor;
  FTabAppearance.TextColorSelected := ATones.Background.TextColor;
  FTabAppearance.ColorHot := ATones.Hover.BrushColor;
  FTabAppearance.BorderColorHot := ATones.Hover.BorderColor;
  FTabAppearance.TextColorHot := ATones.Hover.TextColor;
  FTabAppearance.ColorDisabled := ATones.Disabled.BrushColor;
  FTabAppearance.BorderColorDisabled := ATones.Disabled.BorderColor;
  FTabAppearance.TextColorDisabled := ATones.Disabled.TextColor;
  FTabAppearance.BorderColorSelectedHot := FTabAppearance.BorderColorSelected;
  FTabAppearance.Font.Name := GetMetroFont;

  if not Assigned(FGlowButtonAppearance) then // independent to AdvGlowbutton settings
    FGlowButtonAppearance := TGlowButtonAppearance.Create;

  ClearAppearance(FGlowButtonAppearance);
  FGlowButtonAppearance.Color := ATones.Background.BrushColor;
  FGlowButtonAppearance.ColorTo := clNone;
  FGlowButtonAppearance.BorderColor := clNone; //ATones.Background.BorderColor;
  FGlowButtonAppearance.ColorMirror := ATones.Background.BrushColor;
  FGlowButtonAppearance.ColorMirrorTo := clNone;
  FGlowButtonAppearance.ColorHot := ATones.Hover.BrushColor;
  FGlowButtonAppearance.ColorHotTo := clNone;
  FGlowButtonAppearance.ColorMirrorHot := ATones.Hover.BrushColor;
  FGlowButtonAppearance.ColorMirrorHotTo := clNone;
  FGlowButtonAppearance.BorderColorHot := ATones.Hover.BorderColor;
  FGlowButtonAppearance.TextColorHot := ATones.Hover.TextColor;
  FGlowButtonAppearance.TextColorDown := ATones.Selected.TextColor;
  FGlowButtonAppearance.ColorDown := ATones.Selected.BrushColor;
  FGlowButtonAppearance.ColorDownTo := clNone;
  FGlowButtonAppearance.ColorMirrorDown := ATones.Selected.BrushColor;
  FGlowButtonAppearance.ColorMirrorDownTo := clNone;
  FGlowButtonAppearance.BorderColorDown := ATones.Selected.BorderColor;
  FGlowButtonAppearance.ColorDisabled := clSilver;
  FGlowButtonAppearance.ColorDisabledTo := clSilver;
  FGlowButtonAppearance.ColorMirrorDisabled := clSilver;
  FGlowButtonAppearance.ColorMirrorDisabledTo := clSilver;

  //FGlowButtonAppearance.TextColor := ATones.Background.TextColor;

  SetAllPagesPosition;
  InitializeAndUpdateButtons;
  Invalidate;
  if Assigned(ActivePage) then
    ActivePage.Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetRotateTabLeftRight(const Value: Boolean);
begin
  if (FRotateTabLeftRight <> Value) then
  begin
    if not Value and MultiLine then
    begin
      MessageDlg('RotateTabLeftRight must be True with MultiLine.', mtInformation, [mbOK], 0);
      Exit;
    end;

    FRotateTabLeftRight := Value;
    UpdateTabScroller;
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetVisibleTabCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FAdvPages.Count - 1 do
    if (AdvPages[I].TabVisible) then
      Result := Result + 1;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.Init;
begin
  FPropertiesLoaded := true;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CloseActivePage : Boolean;
var
  lPage : TAdvOfficePage;
begin
  Result := true;
  if Assigned(ActivePage) then
  begin
    lPage := ActivePage;
    OnCloseButtonClick(Self);
    Result := lPage <> ActivePage;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CanGlow: Boolean;
begin
  Result := Glow and not FItones;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CanShow3D: Boolean;
begin
  if FItones then
    Result := Show3D
  else
    Result := FCurrentOfficePagerStyler.Show3D;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CanShowShadow: Boolean;
begin
  if FItones then
    Result := Shadow
  else
    Result := FCurrentOfficePagerStyler.ShowShadow;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CanShowCloseButton: Boolean;
begin
  Result := ButtonSettings.CloseButton;
  if Assigned(ActivePage) then
    Result := ActivePage.ShowClose and Result;

  result := Result or (ShowCloseOnNonSelectedTabs and (AdvPageCount > 0));
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.InitializeAndUpdateButtons;
var
  R: TRect;
  I: Integer;

begin
  if (not FPropertiesLoaded) and not (csDesigning in ComponentState) then
    Exit;

  if ButtonSettings.CloseButton {and not CloseOnTab  and (CanShowCloseButton or not CloseOnTab)} then
  begin
    if not CloseOnTab then
    begin
      for I := 0 to AdvPageCount - 1 do
      begin
        with AdvPages[I] do
        begin
          if Assigned(FCloseButton) then
            FCloseButton.Width := 0;
        end;
      end;
      if (FCloseButtonGlobal = nil) then
      begin
        FCloseButtonGlobal := TAdvGlowButton.Create(Self);
        FCloseButtonGlobal.Parent := Self;
        FCloseButtonGlobal.OnClick := OnCloseButtonClick;
      end;

      R := GetCloseButtonRect(-1);
      FCloseButtonGlobal.Left := R.Left;
      FCloseButtonGlobal.Top := R.Top;
      FCloseButtonGlobal.Width := R.Right - R.Left;
      FCloseButtonGlobal.Height := R.Bottom - R.Top;
      if Assigned(FCurrentOfficePagerStyler) then
      begin
        if Assigned(FGlowButtonAppearance) and FItones then
          SetGlowButtonColorTones(FCloseButtonGlobal)
        else
          FCloseButtonGlobal.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
      end;
      FCloseButtonGlobal.Picture.Assign(ButtonSettings.CloseButtonPicture);
      FCloseButtonGlobal.Hint := ButtonSettings.CloseButtonHint;
      FCloseButtonGlobal.ShowHint := True;
      FCloseButtonGlobal.Enabled := GetVisibleTabCount > 0; //(ActivePage <> nil);
      if FCloseButtonGlobal.Enabled then
        if not CloseOnTab then
          FCloseButtonGlobal.Enabled := CanShowCloseButton;
    end
    else
    begin
      if Assigned(FCloseButtonGlobal) then
        FCloseButtonGlobal.Width := 0;

      //j := ButtonSettings.ButtonSize div 3;

      for I := 0 to AdvPageCount - 1 do
      begin
        with AdvPages[I] do
        begin
          if ShowClose then
          begin
            R := GetCloseButtonRect(I);

            if (ButtonSettings.CloseButtonLook = cblOffice) then
            begin
              if Assigned(FCloseButtonChrome) then
                FCloseButtonChrome.Visible := false;

              if (FCloseButtonChrome <> nil) then
              begin
                PostMessage(Self.Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(FCloseButtonChrome)), 1);
                FCloseButtonChrome := nil;
              end;

              if (FCloseButton = nil) then
              begin
                FCloseButton := TAdvGlowButton.Create(Self);
                FCloseButton.Parent := Self;
                //FCloseButton.OnClick := OnCloseButtonClick;
                FCloseButton.OnMouseUp := OnCloseButtonMouseUp;
              end;

              FCloseButton.Left := R.Left;
              FCloseButton.Top := R.Top;
              FCloseButton.Width := R.Right - R.Left;
              FCloseButton.Height := R.Bottom - R.Top;
              FCloseButton.Tag := I;

              FCloseButton.Picture.Assign(ButtonSettings.CloseButtonPicture);
              if Assigned(FCurrentOfficePagerStyler) then
              begin
                if Assigned(FGlowButtonAppearance) and FItones then
                  SetGlowButtonColorTones(FCloseButton)
                else
                  FCloseButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
              end;

              FCloseButton.Hint := ButtonSettings.CloseButtonHint;
              FCloseButton.ShowHint := True;
              FCloseButton.Enabled := GetVisibleTabCount > 0; //(ActivePage <> nil);
              if FCloseButton.Enabled then
                if not CloseOnTab then
                  FCloseButton.Enabled := CanShowCloseButton;

              FCloseButton.Visible := ShowCloseOnNonSelectedTabs or ((ActivePageIndex = I) and not ShowCloseOnNonSelectedTabs and ShowClose);
              if not FCloseButton.Visible then
                FCloseButton.Width := 0;
            end
            else //if (ButtonSettings.CloseButtonLook = cblChrome) then
            begin
              if Assigned(FCloseButton) then
                FCloseButton.Visible := false;

              if (FCloseButton <> nil) then
              begin
                PostMessage(Self.Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(FCloseButton)), 0);
                FCloseButton := nil;
              end;

              if (FCloseButtonChrome = nil) then
              begin
                FCloseButtonChrome := TAdvPagerButton.Create(Self);
                FCloseButtonChrome.Parent := Self;
                FCloseButtonChrome.OnClick := OnCloseButtonClick;
                //FCloseButton.OnMouseUp := OnCloseButtonMouseUp;
              end;

              FCloseButtonChrome.Left := R.Left;
              FCloseButtonChrome.Top := R.Top;
              FCloseButtonChrome.Width := R.Right - R.Left;
              FCloseButtonChrome.Height := R.Bottom - R.Top;
              FCloseButtonChrome.Tag := I;

              //FCloseButtonChrome.Picture.Assign(ButtonSettings.CloseButtonPicture);

              FCloseButtonChrome.Hint := ButtonSettings.CloseButtonHint;
              FCloseButtonChrome.ShowHint := True;
              FCloseButtonChrome.Enabled := GetVisibleTabCount > 0; //(ActivePage <> nil);
              if FCloseButtonChrome.Enabled then
                if not CloseOnTab then
                  FCloseButtonChrome.Enabled := CanShowCloseButton;

              FCloseButtonChrome.Visible := ShowCloseOnNonSelectedTabs or ((ActivePageIndex = I) and not ShowCloseOnNonSelectedTabs and ShowClose);
              if not FCloseButtonChrome.Visible then
                FCloseButtonChrome.Width := 0;

              {
              //--- Picture
              bmp := TBitmap.Create;
              bmp.Width := R.Right - R.Left;
              bmp.Height := R.Bottom - R.Top;
              bmp.Canvas.Brush.Color := clCream;
              bmp.Canvas.Pen.Color := clCream;
              bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));

              g := TGPGraphics.Create(bmp.Canvas.Handle);
              g.SetSmoothingMode(SmoothingModeAntiAlias);

              p := TGPPen.Create(MakeColor(220, 165, 165, 165), 1.6);
              g.DrawLine(p, j, j-1, bmp.Width - j + 1, bmp.Height - j);
              g.DrawLine(p, bmp.Width - j + 1, j - 1, j, bmp.Height - j);
              p.Free;
              g.Free;
              FCloseButtonChrome.Picture.Assign(bmp);
              bmp.Free;

              //--- HotPicture
              bmp := TBitmap.Create;
              bmp.Width := R.Right - R.Left;
              bmp.Height := R.Bottom - R.Top;
              bmp.Canvas.Brush.Color := clWhite;
              bmp.Canvas.Pen.Color := clWhite;
              bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));

              g := TGPGraphics.Create(bmp.Canvas.Handle);
              g.SetSmoothingMode(SmoothingModeAntiAlias);

              br := TGPSolidBrush.Create(ColorToARGB(RGB(192, 53, 53)));
              g.FillEllipse(br, 2, 1, bmp.Width-3, bmp.Height-3);
              br.Free;

              p := TGPPen.Create(MakeColor(220, 235, 235, 235), 1.6);
              g.DrawLine(p, j, j-1, bmp.Width - j + 1, bmp.Height - j);
              g.DrawLine(p, bmp.Width - j + 1, j - 1, j, bmp.Height - j);
              p.Free;

              g.Free;
              FCloseButtonChrome.HotPicture.Assign(bmp);
              bmp.Free;

              //--- DownPicture
              bmp := TBitmap.Create;
              bmp.Width := R.Right - R.Left;
              bmp.Height := R.Bottom - R.Top;
              bmp.Canvas.Brush.Color := clWhite;
              bmp.Canvas.Pen.Color := clWhite;
              bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));

              g := TGPGraphics.Create(bmp.Canvas.Handle);
              g.SetSmoothingMode(SmoothingModeAntiAlias);

              br := TGPSolidBrush.Create(ColorToARGB(RGB(67, 30, 32)));
              g.FillEllipse(br, 2, 1, bmp.Width-3, bmp.Height-3);
              br.Free;

              p := TGPPen.Create(MakeColor(220, 235, 235, 235), 1.6);
              g.DrawLine(p, j, j-1, bmp.Width - j + 1, bmp.Height - j);
              g.DrawLine(p, bmp.Width - j + 1, j - 1, j, bmp.Height - j);
              p.Free;
              g.Free;
              FCloseButtonChrome.DownPicture.Assign(bmp);
              bmp.Free;
              }
            end;

          end
          else
          begin
            if Assigned(FCloseButton) then
              FCloseButton.Visible := false;
          end;
        end;
      end;
    end;
  end
  else
  begin
    for I := 0 to AdvPageCount - 1 do
    begin
      with AdvPages[I] do
      begin
        if (FCloseButton <> nil) then
        begin
          PostMessage(Self.Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(FCloseButton)), 0);
          FCloseButton := nil;
        end;

        if (FCloseButtonChrome <> nil) then
        begin
          PostMessage(Self.Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(FCloseButtonChrome)), 1);
          FCloseButtonChrome := nil;
        end;
      end;
    end;
    if Assigned(FCloseButtonGlobal) then
    begin
      FCloseButtonGlobal.Free;
      FCloseButtonGlobal := nil;
    end;
  end;

  UpdateClosedListButton;
  
  if ButtonSettings.PageListButton then
  begin
    if (FPageListButton = nil) then
    begin
      FPageListButton := TAdvGlowButton.Create(Self);
      FPageListButton.Parent := Self;
      FPageListButton.OnClick := OnPageListButtonClick;
    end;

    R := GetPageListRect;
    FPageListButton.Left := R.Left;
    FPageListButton.Top := R.Top;
    FPageListButton.Width := R.Right - R.Left;
    FPageListButton.Height := R.Bottom - R.Top;
    if Assigned(FCurrentOfficePagerStyler) then
    begin
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FPageListButton)
      else
        FPageListButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
    end;
    FPageListButton.Picture.Assign(ButtonSettings.PageListButtonPicture);
    FPageListButton.Hint := ButtonSettings.PageListButtonHint;
    FPageListButton.ShowHint := True;
    FPageListButton.Enabled := GetVisibleTabCount > 0; //(AdvPageCount > 0);
  end
  else
  if (FPageListButton <> nil) then
  begin
    FPageListButton.Free;
    FPageListButton := nil;
  end;

  if (FTabScroller.Visible) then
  begin
    if (FScrollFirstButton = nil) then
    begin
      FScrollFirstButton := TAdvGlowButton.Create(Self);
      FScrollFirstButton.Parent := Self;
      FScrollFirstButton.OnClick := OnScrollFirstButtonClick;
    end;
    R := GetTabScrollerFirstRect;
    FScrollFirstButton.Left := R.Left;
    FScrollFirstButton.Top := R.Top;
    FScrollFirstButton.Width := R.Right - R.Left;
    FScrollFirstButton.Height := R.Bottom - R.Top;
    if Assigned(FCurrentOfficePagerStyler) then
    begin
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FScrollFirstButton)
      else
        FScrollFirstButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
    end;

    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FFirstPictureChanged then
        FScrollFirstButton.Picture.Assign(ButtonSettings.ScrollButtonFirstPicture)
      else
        FScrollFirstButton.Picture.Assign(ButtonSettings.FScrollButtonUpFirstPicture);
    end
    else
      FScrollFirstButton.Picture.Assign(ButtonSettings.ScrollButtonFirstPicture);

    FScrollFirstButton.Hint := ButtonSettings.ScrollButtonFirstHint;
    FScrollFirstButton.ShowHint := True;
    FScrollFirstButton.Enabled := FTabScroller.CanGoBack;
    FScrollFirstButton.RepeatClick := True;
    FScrollFirstButton.Visible := ButtonSettings.FirstButton;
    if not FScrollFirstButton.Visible then
      FScrollFirstButton.Width := 0;

    if (FScrollLastButton = nil) then
    begin
      FScrollLastButton := TAdvGlowButton.Create(Self);
      FScrollLastButton.Parent := Self;
      FScrollLastButton.OnClick := OnScrollLastButtonClick;
    end;
    R := GetTabScrollerLastRect;
    FScrollLastButton.Left := R.Left;
    FScrollLastButton.Top := R.Top;
    FScrollLastButton.Width := R.Right - R.Left;
    FScrollLastButton.Height := R.Bottom - R.Top;
    if Assigned(FCurrentOfficePagerStyler) then
    begin
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FScrollLastButton)
      else
        FScrollLastButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
    end;

    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FLastPictureChanged then
        FScrollLastButton.Picture.Assign(ButtonSettings.ScrollButtonLastPicture)
      else
        FScrollLastButton.Picture.Assign(ButtonSettings.FScrollButtonDownLastPicture);
    end
    else
      FScrollLastButton.Picture.Assign(ButtonSettings.ScrollButtonLastPicture);

    FScrollLastButton.Hint := ButtonSettings.ScrollButtonLastHint;
    FScrollLastButton.ShowHint := True;
    FScrollLastButton.Enabled := FTabScroller.CanGoForward;
    FScrollLastButton.RepeatClick := True;
    FScrollLastButton.Visible := ButtonSettings.LastButton;
    if not FScrollLastButton.Visible then
      FScrollLastButton.Width := 0;

    if (FScrollPrevButton = nil) then
    begin
      FScrollPrevButton := TAdvGlowButton.Create(Self);
      FScrollPrevButton.Parent := Self;
      FScrollPrevButton.OnClick := OnScrollPrevButtonClick;
    end;
    R := GetTabScrollerLeftRect;
    FScrollPrevButton.Left := R.Left;
    FScrollPrevButton.Top := R.Top;
    FScrollPrevButton.Width := R.Right - R.Left;
    FScrollPrevButton.Height := R.Bottom - R.Top;
    if Assigned(FCurrentOfficePagerStyler) then
    begin
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FScrollPrevButton)
      else
        FScrollPrevButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
    end;

    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FPrevPictureChanged then
        FScrollPrevButton.Picture.Assign(ButtonSettings.ScrollButtonPrevPicture)
      else
        FScrollPrevButton.Picture.Assign(ButtonSettings.FScrollButtonUpPicture);
    end
    else
      FScrollPrevButton.Picture.Assign(ButtonSettings.ScrollButtonPrevPicture);

    FScrollPrevButton.Hint := ButtonSettings.ScrollButtonPrevHint;
    FScrollPrevButton.ShowHint := True;
    FScrollPrevButton.Enabled := FTabScroller.CanGoBack;
    FScrollPrevButton.RepeatClick := True;

    if (FScrollNextButton = nil) then
    begin
      FScrollNextButton := TAdvGlowButton.Create(Self);
      FScrollNextButton.Parent := Self;
      FScrollNextButton.OnClick := OnScrollNextButtonClick;
    end;
    R := GetTabScrollerRightRect;
    FScrollNextButton.Left := R.Left;
    FScrollNextButton.Top := R.Top;
    FScrollNextButton.Width := R.Right - R.Left;
    FScrollNextButton.Height := R.Bottom - R.Top;
    if Assigned(FCurrentOfficePagerStyler) then
    begin
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FScrollNextButton)
      else
        FScrollNextButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
    end;

    if (TabPosition in [tpLeft, tpRight]) {and not RotateTabLeftRight} then
    begin
      if ButtonSettings.FNextPictureChanged then
        FScrollNextButton.Picture.Assign(ButtonSettings.ScrollButtonNextPicture)
      else
        FScrollNextButton.Picture.Assign(ButtonSettings.FScrollButtonDownPicture);
    end
    else
      FScrollNextButton.Picture.Assign(ButtonSettings.ScrollButtonNextPicture);

    FScrollNextButton.Hint := ButtonSettings.ScrollButtonNextHint;
    FScrollNextButton.ShowHint := True;
    FScrollNextButton.Enabled := FTabScroller.CanGoForward;
    FScrollNextButton.RepeatClick := True;

  end
  else
  begin
    if (FScrollPrevButton <> nil) then
    begin
      FScrollPrevButton.Free;
      FScrollPrevButton := nil;
    end;

    if (FScrollNextButton <> nil) then
    begin
      FScrollNextButton.Free;
      FScrollNextButton := nil;
    end;

    if (FScrollFirstButton <> nil) then
    begin
      FScrollFirstButton.Free;
      FScrollFirstButton := nil;
    end;

    if (FScrollLastButton <> nil) then
    begin
      FScrollLastButton.Free;
      FScrollLastButton := nil;
    end;
  end;
end;

procedure TAdvOfficePager.InitOrder;
var
  i: integer;
begin
  for i:= 0 to AdvPageCount - 1 do
  begin
    AdvPages[i].FTabIndex := i + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnCloseButtonClick(Sender: TObject);
var
  Allow: Boolean;
  ActTabIndex: Integer;
  NewActivePage: TAdvOfficePage;
  i: integer;
begin
  if CloseOnTab and ((Sender is TAdvGlowButton) or (Sender is TAdvPagerButton)) then
  begin
    if (Sender is TAdvGlowButton) then
      i := (Sender as TAdvGlowButton).Tag
    else
      i := (Sender as TAdvPagerButton).Tag
  end
  else
    i := ActivePageIndex;

  if (i >= 0) and not FIsClosing then
  begin
    Allow := True;
    FIsClosing := True;
    if Assigned(FOnClosePage) then
      FOnClosePage(Self, i, Allow);

    if Allow then
    begin
      if Assigned(AdvPages[i].FTimer) then
        FreeAndNil(AdvPages[i].FTimer);

      ActTabIndex := i;
      if (ActivePage = AdvPages[ActTabIndex]) then
        SelectNextPage(True);
      NewActivePage := ActivePage;
      InvalidateTab(-1);

      if FreeOnClose then
      begin
        if Assigned(AdvPages[ActTabIndex].FCloseButton) then
        begin
          PostMessage(Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(AdvPages[ActTabIndex].FCloseButton)), 0);
          //AdvPages[ActTabIndex].FCloseButton.Free;
          AdvPages[ActTabIndex].FCloseButton := nil;
        end;

        if Assigned(AdvPages[ActTabIndex].FCloseButtonChrome) then
        begin
          PostMessage(Handle, WM_OPDESTROYCLOSEBTN, Integer(Pointer(AdvPages[ActTabIndex].FCloseButtonChrome)), 1);
          AdvPages[ActTabIndex].FCloseButtonChrome := nil;
        end;

        AdvPages[ActTabIndex].Free;

        FActivePageIndex := -1;
        ActivePage := NewActivePage;
        //SelectNextPage(True);
      end
      else if (ActTabIndex >= 0) then
      begin
        AdvPages[ActTabIndex].TabVisible := False;
        AdvPages[ActTabIndex].Visible := False;
        if CloseOnTab then
        begin
          if Assigned(AdvPages[ActTabIndex].FCloseButton) then
            AdvPages[ActTabIndex].FCloseButton.Visible := false;

          if Assigned(AdvPages[ActTabIndex].FCloseButtonChrome) then
            AdvPages[ActTabIndex].FCloseButtonChrome.Visible := false;
        end;
        UpdateClosedListButton;
      end;

      UpdateTabScroller;
      UpdateMultiLineTabs;

      if Assigned(ActivePage) then
        ActivePage.Invalidate
      else
        Invalidate;

      if Assigned(FOnClosedPage) then
        FOnClosedPage(Self, ActTabIndex);
    end;
    FIsClosing := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnCloseButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    OnCloseButtonClick(Sender);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnPageListMenuClick(Sender: TObject);
begin
  if (Sender is TMenuItem) and ((TMenuItem(Sender).Tag >= 0) and (TMenuItem(Sender).Tag < FAdvPages.Count)) then
  begin
    ActivePageIndex := TMenuItem(Sender).Tag;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnPageListButtonClick(Sender: TObject);
var
  I: Integer;
  MenuItem: TMenuItem;
  P: TPoint;
  R: TRect;
begin
  if (ButtonSettings.PageListButton) then
  begin
    if Assigned(PageListMenu) then
    begin
      PageListMenu.Items.Clear;
      PageListMenu.Images := Self.Images;
      for I := 0 to FAdvPages.Count-1 do
      begin
        if AdvPages[I].TabVisible then
        begin
          MenuItem := TMenuItem.Create(self);
          if AdvPages[I].Caption <> '' then
            MenuItem.Caption := AdvPages[I].Caption
          else
            MenuItem.Caption := AdvPages[I].WideCaption;
          MenuItem.RadioItem := True;
          MenuItem.Checked := ActivePageIndex = I;
          MenuItem.Tag := I;
          MenuItem.OnClick := OnPageListMenuClick;
          MenuItem.ImageIndex := AdvPages[I].ImageIndex;
          PageListMenu.Items.Add(MenuItem);
        end;
      end;
    end;

    if (FPageListButton <> nil) then
    begin
      case (TabPosition) of
        tpTop:
        begin
          R := GetPageListRect;
          P.X := R.Left + self.Left;
          P.Y := R.Bottom + self.Top;

          p := Parent.ClientToScreen(p);

          (*
          SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);

          if R.Bottom < (P.Y + FOptionWindow.Height + 2) then
            spt.Y := spt.Y - ((spt.Y + FOptionWindow.Height + 2) - R.Bottom);

          if (R.Right < spt.X + FOptionWindow.Width) then
          begin
            if Position = daRight then
              spt.X := ClientToScreen(Point(Left - FOptionWindow.Width, Top + Height + 1)).X
            else
              spt.X := spt.X - ((spt.X + FOptionWindow.Width) - R.Right);
          end; *)  
        end;
        tpBottom:
        begin
          R := GetPageListRect;
          P.X := R.Left + self.Left;
          P.Y := R.Bottom + self.Top;
          p := Parent.ClientToScreen(p);
          if Assigned(PageListMenu) then
          begin
            if (GetSystemMetrics(SM_CYMENU) * PageListMenu.Items.Count) + P.Y + 10 >
            {$IFDEF DELPHI6_LVL}
              Screen.MonitorFromPoint(P).Height then
            {$ELSE}
              Screen.Height then
            {$ENDIF}
            begin
              if (FPageListButton <> nil) then
                Dec(P.Y, (GetSystemMetrics(SM_CYMENU) * PageListMenu.Items.Count) + (FPageListButton.Height) + 4);
            end;
          end;
        end;
        tpLeft:
        begin
          R := GetPageListRect;
          P.X := R.Right + self.Left;
          P.Y := R.Top + self.Top;
          p := Parent.ClientToScreen(p);
        end;
        tpRight:
        begin
          R := GetPageListRect;
          P.X := R.Right + self.Left;
          P.Y := R.Top + self.Top;
          p := Parent.ClientToScreen(p);
        end;
      end;

      if Assigned(FOnPageListClick) then
        FOnPageListClick(Self, P.X, P.Y)
      else if Assigned(PageListMenu) then
        PageListMenu.Popup(P.X, P.Y);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnScrollPrevButtonClick(Sender: TObject);
begin
  ScrollLeftBtnClick;
  InitializeAndUpdateButtons;
  if assigned(FOnPrevClick) then
    FOnPrevClick(Sender);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnScrollFirstButtonClick(Sender: TObject);
begin
  if assigned(FOnFirstClick) then
    FOnFirstClick(Sender);
  ScrollFirstBtnClick;
  InitializeAndUpdateButtons;
end;

procedure TAdvOfficePager.OnScrollLastButtonClick(Sender: TObject);
begin
  if Assigned(FOnLastClick) then
    FOnLastClick(Sender);
  ScrollLastBtnClick;
  InitializeAndUpdateButtons;
end;

procedure TAdvOfficePager.OnScrollNextButtonClick(Sender: TObject);
begin
  ScrollRightBtnClick;
  InitializeAndUpdateButtons;
  if Assigned(FOnNextClick) then
    FOnNextClick(Sender);
  //if (FScrollNextButton <> nil) and not FScrollNextButton.Enabled then
    //FScrollNextButton.Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.AnyButtonVisible: Boolean;
begin
  Result := (FTabScroller.Visible or (ButtonSettings.CloseButton and not CloseOnTab) or ButtonSettings.PageListButton);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.CreateShortCutHintWin: TShortCutHintWindow;
begin
  Result := TShortCutHintWindow.Create(Self);
  Result.Parent := Self;
  FShortCutHintWinList.Add(Result);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DestroyShortCutHintWin(
  ShortCutWin: TShortCutHintWindow);
var
  i: Integer;
begin
  i := FShortCutHintWinList.IndexOf(ShortCutWin);
  if (i >= 0) then
  begin
    TShortCutHintWindow(FShortCutHintWinList.Items[i]).Free;
    FShortCutHintWinList.Items[i] := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.HideShortCutHintOfAllPages;
var
  i: Integer;
  p: TWinControl;
begin
  for I:= 0 to FAdvPages.Count-1 do
  begin
    AdvPages[I].HideShortCutHint;
  end;
  FTabShortCutHintShowing := False;

  if not (csDesigning in ComponentState) and Assigned(FFormWndProc) then
  begin
    p := self;
    repeat
      p := p.Parent;
    until (p is TForm) or (p is TActiveForm) or not Assigned(p);

    if (p <> nil) then
    begin
      p.WindowProc := FFormWndProc;
      FFormWndProc := nil;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ShowShortCutHintOfAllPages;
var
  i: Integer;
  p: TWinControl;
begin
  if FShowShortCutHints then
  begin
    for I:= 0 to FAdvPages.Count-1 do
    begin
      AdvPages[I].ShowShortCutHint;
    end;
    FTabShortCutHintShowing := True;
    FTabShortCutChars := '';


    if not (csDesigning in ComponentState) and not Assigned(FFormWndProc) then
    begin
      p := self;

      repeat
        p := p.Parent;
      until (p is TForm) or (p is TActiveForm) or not Assigned(p);

      if Assigned(p) then
      begin
        FFormWndProc := p.WindowProc;
        p.WindowProc := SubClassProc;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMKeyDown(var Message: TWMKeyDown);
var
  i, c: Integer;
  s, sub: String;
  found: Boolean;
  Ctrl: TWinControl;
begin
  case Message.CharCode of
    VK_LEFT, VK_UP:
    begin
      HideShortCutHintOfAllPages;
      SelectNextPage(False);
    end;
    VK_RIGHT, VK_DOWN:
    begin
      HideShortCutHintOfAllPages;
      SelectNextPage(True);
    end;
    {VK_DOWN:
    begin
      if Assigned(ActivePage) and Focused and (ActivePage.ControlCount > 0) then
      begin
        ATB := ActivePage.GetFirstToolBar(True);
        ATB.SetFocus;
        HideShortCutHintOfAllPages;
        ActivePage.ShowShortCutHintOfAllToolBars;
      end;
    end;}
    VK_ESCAPE:
    begin
      HideShortCutHintOfAllPages;
    end;
    VK_TAB:
    begin
      if Assigned(Self.Parent) then
      begin
        Ctrl := TProWinControl(Self.Parent).FindNextControl(Self, True, True, True);
        if Assigned(Ctrl) and Ctrl.CanFocus then
        begin
          Ctrl.SetFocus;
        end;
      end;
    end;
    else
    begin
      if FTabShortCutHintShowing then
      begin
        s := char(Message.CharCode);
        FTabShortCutChars := FTabShortCutChars + s;
        found := False;
        c := 0;
        for i := 0 to FAdvPages.Count-1 do
        begin
          if (UpperCase(AdvPages[i].ShortCutHint) = UpperCase(FTabShortCutChars)) then
          begin
            HideShortCutHintOfAllPages;
            ActivePageIndex := i;
            if Assigned(ActivePage) then
            begin
              {ATB := ActivePage.GetFirstToolBar(True);
              ATB.SetFocus;
              ActivePage.ShowShortCutHintOfAllToolBars; }
            end;
            found := True;
            Break;
          end
          else if (Length(AdvPages[i].ShortCutHint) > 0) then
          begin
            sub := Copy(AdvPages[i].ShortCutHint, 1, Length(FTabShortCutChars));
            if (UpperCase(sub) <> UpperCase(FTabShortCutChars)) then
            begin
              AdvPages[i].HideShortCutHint;
            end
            else
            begin
              found := True;
            end;
          end;

          if Assigned(AdvPages[i].FShortCutHint) and (AdvPages[i].FShortCutHint.Visible) then
          begin
            Inc(c);
          end;
        end;

        if (c = 0) then
          FTabShortCutHintShowing := False;

        if not found then
        begin
          Beep;
        end;
      end;
    end;
  end;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS + DLGC_WANTCHARS{ + DLGC_WANTTAB};
  {using DLGC_WANTTAB, disabled default Tab key functioning}
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMDialogKey(var Message: TCMDialogKey);
begin
  (*if TabStop and Assigned(ActivePage) and (Message.CharCode = 18{ALT}) then
  begin
    if not FTabShortCutHintShowing and (CanFocus) then
    begin
      if not Focused then
        Self.SetFocus;
      Message.Result := 1;
      ShowShortCutHintOfAllPages;
      Exit;
    end
    else if FTabShortCutHintShowing then
    begin
      HideShortCutHintOfAllPages;
      Message.Result := 1;
      Exit;
    end;
  end; *)
  if (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    SelectNextPage(GetKeyState(VK_SHIFT) >= 0);
    Message.Result := 1;
  end
  else
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMFocusChanged(var Message: TCMFocusChanged);
{var
  i: Integer;
  h: HWND;
  Active: Boolean;}
begin
  inherited;

 { Active := Self.Focused;
  if not Active and (Message.Sender <> Self) and (self.HandleAllocated) then
  begin
    h := GetFocus;
    i := 1;
    while (h <> 0) do
    begin
      if (h = self.Handle) then
      begin
        Active := True;
        Break;
      end;
      h := GetParent(h);
      inc(i);
      if (i > 50) then
        Break;
    end;
  end;
  }
  if (Message.Sender <> Self) and FTabShortCutHintShowing then
  begin
    HideShortCutHintOfAllPages;
  end;

  if (Message.Sender = Self) and not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
    ShowShortCutHintOfAllPages;

  InvalidateTab(-1);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;

  if FTabShortCutHintShowing then
  begin
    HideShortCutHintOfAllPages;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WndProc(var Msg: TMessage);
var
  p: TWinControl;
  cb: TAdvGlowButton;
  cbp: TAdvPagerButton;
begin
  if (Msg.Msg = WM_DESTROY) then
  begin
    // restore subclassed proc
    if not (csDesigning in ComponentState) and Assigned(FFormWndProc) then
    begin
      p := self;
      repeat
        p := p.Parent;
      until (p is TForm) or (p is TActiveForm) or not Assigned(p);

      if (p <> nil) then
      begin
        p.WindowProc := FFormWndProc;
        FFormWndProc := nil;
      end;
    end;
  end
  else if (Msg.Msg = WM_OPDESTROYCLOSEBTN) then
  begin
    if (Msg.WParam <> 0) then
    begin
      if (Msg.LParam = 0) then
      begin
        cb := TAdvGlowButton(Pointer(Msg.WParam));
        cb.Free;
      end
      else
      begin
        cbp := TAdvPagerButton(Pointer(Msg.WParam));
        cbp.Free;
      end;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SubclassProc(var Msg: TMessage);
begin
  if not Assigned(FFormWndProc) then
    Exit;
    
  FFormWndProc(Msg);

  //if (Msg.Msg = WM_CLOSE) then

  if ((Msg.Msg = WM_MOVING) or (Msg.Msg = WM_LBUTTONDOWN) or (Msg.Msg = WM_SIZE)) or
     ((Msg.Msg = WM_SYSCOMMAND) and ((Msg.WParam = SC_MAXIMIZE) or (Msg.WParam = SC_MINIMIZE))) then
  begin
    HideShortCutHintOfAllPages;
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DragDrop(Source: TObject; X, Y: Integer);
var
  CurIndex, NewIndex: Integer;
begin
  inherited;
  CurIndex := ActivePageIndex;
  NewIndex := PTOnTab(X, Y);
  if (CurIndex >= 0) and (CurIndex < AdvPageCount) and (NewIndex >= 0) and (NewIndex < AdvPageCount) and (CurIndex <> NewIndex) then
  begin
    MoveAdvPage(CurIndex, NewIndex);
    Invalidate;
    if Assigned(ActivePage) then
      ActivePage.Invalidate;
  end;

  if Assigned(FArrow) and FArrow.Visible then
    FArrow.Visible := False;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CMDockClient(var Message: TCMDockClient);
var
  IsVisible: Boolean;
  DockCtl: TControl;
  I: Integer;
begin
  Message.Result := 0;
  DockCtl := Message.DockSource.Control;
  { First, look and see if the page is already docked. If it is,
    then simply move the page index to the end }
  for I := 0 to AdvPageCount - 1 do
  begin
    if DockCtl.Parent = AdvPages[I] then
    begin
      { We did find it; just move the page to the end }
      AdvPages[I].PageIndex := AdvPageCount - 1;
      Exit;
    end;
  end;

  FNewPage := TAdvOfficePage.Create(Self);
  try
    try
      FNewPage.AdvOfficePager := Self;
      DockCtl.Dock(Self, Message.DockSource.DockRect);
    except
      FNewPage.Free;
      raise;
    end;
    IsVisible := DockCtl.Visible;
    FNewPage.TabVisible := IsVisible;
    if IsVisible then
      ActivePage := FNewPage;
    DockCtl.Align := alClient;

    if DockCtl is TCustomForm then
    begin
      FNewPage.Caption := TCustomForm(DockCtl).Caption;
    end;

  finally
    FNewPage := nil;
  end;
end;

procedure TAdvOfficePager.CMDockNotification(var Message: TCMDockNotification);
var
  I: Integer;
  S: string;
  Page: TAdvOfficePage;
begin
  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
        begin
          S := PChar(Message.NotifyRec.MsgLParam);
          { Search for first CR/LF and end string there }
          for I := 1 to Length(S) do
          {$IFDEF DELPHI_UNICODE}
            if CharInSet(S[I],[#13,#10]) then
          {$ENDIF}
          {$IFNDEF DELPHI_UNICODE}
            if S[I] in [#13, #10] then
          {$ENDIF}
            begin
              SetLength(S, I - 1);
              Break;
            end;

          if (Message.Client is TCustomForm) then
            Page.Caption := (Message.Client as TCustomForm).Caption
          else
            Page.Caption := S;
          
        end;
      CM_VISIBLECHANGED:
        Page.TabVisible := Boolean(Message.NotifyRec.MsgWParam);
    end;
  inherited;
end;

procedure TAdvOfficePager.CMUnDockClient(var Message: TCMUnDockClient);
var
  Page: TAdvOfficePage;
begin
  Message.Result := 0;

  Page := GetPageFromDockClient(Message.Client);
  if Page <> nil then
  begin
    FUndockPage := Page;
    Message.Client.Align := alNone;
  end;
end;

procedure TAdvOfficePager.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  if FNewPage <> nil then
    Client.Parent := FNewPage;
end;

procedure TAdvOfficePager.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Source.DockRect := R;
  DoDockOver(Source, X, Y, State, Accept);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockPage <> nil) and not (csDestroying in ComponentState) then
  begin
    SelectNextPage(True);
    FUndockPage.Free;
    FUndockPage := nil;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetPageFromDockClient(Client: TControl): TAdvOfficePage;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AdvPageCount - 1 do
  begin
    if (Client.Parent = AdvPages[I]) and (Client.HostDockSite = Self) then
    begin
      Result := AdvPages[I];
      Exit;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DragCanceled;
begin
  inherited;
  if Assigned(FArrow) and FArrow.Visible then
    FArrow.Visible := False;
  //ReleaseCapture;
  //Cursor := FOldCursor;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  i: Integer;
  R: TRect;
  P: TPoint;
begin
  inherited;
  if TabReorder and (Source = Self) then
  begin
    i := PTOnTab(X, Y);
    Accept := (i >= 0) and (i < AdvPageCount) and (Source = Self);

    if Accept and Assigned(FArrow) then
    begin
      R := GetTabRect(i);
      case TabPosition of
        tpTop: P := Point(R.Left + (R.Right - R.Left) div 2, R.Top - FArrow.Height);
        tpBottom: P := Point(R.Left + (R.Right - R.Left) div 2, R.Top - FArrow.Height);
        tpLeft: P := Point(R.Left - FArrow.Width, R.Top + (R.Bottom - R.Top) div 2);
        tpRight: P := Point(R.Left - FArrow.Width, R.Top + (R.Bottom - R.Top) div 2);
      end;

      P := ClientToScreen(P);
      FArrow.Left := P.X;
      FArrow.Top := P.Y;
      FArrow.Color := FTabReorderIndicatorColor;
      FArrow.Visible := True;
    end;
  end;

  if not Accept and Assigned(FArrow) and FArrow.Visible then
    FArrow.Visible := False;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('NextPictureChanged', ReadNextPicChanged, WriteNextPicChanged, True);
  Filer.DefineProperty('PrevPictureChanged', ReadPrevPicChanged, WritePrevPicChanged, True);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetNextPictureChanged: Boolean;
begin
  Result := FButtonSettings.FNextPictureChanged;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetPrevPictureChanged: Boolean;
begin
  Result := FButtonSettings.FPrevPictureChanged;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetNextPictureChanged(const Value: Boolean);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.SetPrevPictureChanged(const Value: Boolean);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ReadNextPicChanged(Reader: TReader);
begin
  FButtonSettings.FNextPictureChanged := Reader.ReadBoolean;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ReadPrevPicChanged(Reader: TReader);
begin
  FButtonSettings.FPrevPictureChanged := Reader.ReadBoolean;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WriteNextPicChanged(Writer: TWriter);
begin
  Writer.WriteBoolean(FButtonSettings.FNextPictureChanged);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WritePrevPicChanged(Writer: TWriter);
begin
  Writer.WriteBoolean(FButtonSettings.FPrevPictureChanged);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.UseOldDrawing: Boolean;
begin
  Result := (TabSettings.Shape = tsRectangle) and (TabSettings.Rounding = 1);
  if not Result and (TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight then
    Result := (TabSettings.Rounding = 1);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IsActivePageNeighbour(
  PageIndex: Integer): Integer;
var
  i: Integer;  
begin
  Result := 0;
  if (PageIndex = ActivePageIndex) or (PageIndex < 0) or (PageIndex >= AdvPageCount) then
    Exit;

  if (PageIndex < ActivePageIndex) then
  begin
    for i:= ActivePageIndex - 1 downto PageIndex do
    begin
      if AdvPages[i].TabVisible then
      begin
        if (i = PageIndex) then
          Result := -1;
        Break;
      end;
    end;
  end
  else // if (PageIndex > ActivePageIndex) then
  begin
    for i:= ActivePageIndex + 1 to PageIndex do
    begin
      if AdvPages[i].TabVisible then
      begin
        if (i = PageIndex) then
          Result := 1;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetLeftRoundingOffset: Integer;
begin
  Result := 0;
  if (TabSettings.Shape in [tsLeftRamp, tsLeftRightRamp]) and not UseOldDrawing and not ((TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight) then
    Result := TabSettings.Rounding * 2 + 5;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetRightRoundingOffset: Integer;
begin
  Result := 0;
  if (TabSettings.Shape in [tsRightRamp, tsLeftRightRamp]) and not UseOldDrawing and not ((TabPosition in [tpLeft, tpRight]) and not RotateTabLeftRight) then
    Result := TabSettings.Rounding * 2 + 5;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.GetSettings: string;
var
  i: integer;
  ns: string;
begin
  Result := 'P:' + inttostr(ActivePageIndex);

  for i := 0 to AdvPageCount - 1 do
  begin
    if AdvPages[i].TabVisible then
      ns := 'V:' + IntToStr(AdvPages[i].TabIndex)
    else
      ns := 'H:' + IntToStr(AdvPages[i].TabIndex);

    Result := Result + ',' + ns;
  end;

  for i := 0 to FloatingPageCount - 1 do
  begin
    ns := 'F:' + IntToStr(FloatingPages[i].TabIndex);

    ns := ns + '['+ IntToStr(FloatingPages[i].GetFloatingWindow.Left) + ':'
                  + IntToStr(FloatingPages[i].GetFloatingWindow.Top) + ':'
                  + IntToStr(FloatingPages[i].GetFloatingWindow.Width) + ':'
                  + IntToStr(FloatingPages[i].GetFloatingWindow.Height) + ']';

    Result := Result + ',' + ns;
  end;

end;

//------------------------------------------------------------------------------
procedure TAdvOfficePager.SetPageValidCache(Value: Boolean);
var
  i: Integer;
begin
  for i := 0 to AdvPageCount -1 do
    AdvPages[i].FValidCache := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdatePageAppearanceOfPages(PageAppearance: TVistaBackground);
var
  i: Integer;
begin
  if not Assigned(PageAppearance) then
    Exit;

  for i := 0 to AdvPageCount -1 do
  begin
    if not AdvPages[i].UsePageAppearance then
      AdvPages[i].PageAppearance.Assign(PageAppearance);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateTabAppearanceOfPages(TabAppearance: TTabAppearance);
var
  i: Integer;
begin
  if not Assigned(TabAppearance) then
    Exit;

  for i := 0 to AdvPageCount -1 do
  begin
    if not AdvPages[i].UseTabAppearance then
      AdvPages[i].TabAppearance.Assign(TabAppearance);
  end;
end;

//------------------------------------------------------------------------------

type
  TOverrideControl = class(TControl);


procedure TAdvOfficePager.WMLButtonDown(var Message: TWMLButtonDown);

var
  DockCtl: TControl;
  p: TPoint;
begin
  inherited;
  p := Point(Message.XPos, Message.YPos);
  DockCtl := GetDockClientFromMousePos(p);
  if (DockCtl <> nil) then
    if (TOverrideControl(DockCtl).DragMode = dmAutomatic) and (TOverrideControl(DockCtl).DragKind = dkDock) then
      DockCtl.BeginDrag(False);
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  Tab: integer;
  p: TPoint;
  DockCtl: TControl;

begin
  inherited;

  p := Point(Message.XPos, Message.YPos);

  DockCtl := GetDockClientFromMousePos(p);
  if DockCtl <> nil then DockCtl.ManualDock(nil, nil, alNone);

  if PtInRect(GetTabsArea, p) then
  begin
    Tab := PTOnTab(p.X, p.Y);
    if (Tab >= 0) then
    begin
      if Assigned(FOnTabDblClick) then
        FOnTabDblClick(Self, Tab);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnCloseListMenuClick(Sender: TObject);
begin
  if (Sender is TMenuItem) and ((TMenuItem(Sender).Tag >= 0) and (TMenuItem(Sender).Tag < FClosedPageList.Count)) then
  begin
    OpenClosedPage(TAdvOfficePage(FClosedPageList.Objects[TMenuItem(Sender).Tag]));
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.ClosedListButtonClick(Sender: TObject);
var
  Menu: TPopupMenu;
  MenuItem: TMenuItem;
  I: Integer;
  P: TPoint;
begin
  if (FClosedPageList.Count > 0) and not (csDesigning in ComponentState) then
  begin
    if Assigned(ClosedListMenu) then
      Menu := ClosedListMenu
    else if Assigned(FInternalClosedListMenu) then
      Menu := FInternalClosedListMenu
    else
    begin
      FInternalClosedListMenu := TPopupMenu.Create(Self);
      Menu := FInternalClosedListMenu;
    end;

    Menu.Items.Clear;
    Menu.Images := Self.Images;

    for I := 0 to FClosedPageList.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(self);
      MenuItem.Caption := TAdvOfficePage(FClosedPageList.Objects[i]).Caption;
      MenuItem.Tag := I;
      MenuItem.OnClick := OnCloseListMenuClick;
      MenuItem.ImageIndex := TAdvOfficePage(FClosedPageList.Objects[i]).ImageIndex;
      Menu.Items.Add(MenuItem);
    end;

    if (FClosedListButton <> nil) then
    begin

      case (TabPosition) of
        tpTop:
        begin
          P.X := FClosedListButton.Left;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          p := ClientToScreen(p);
        end;
        tpBottom:
        begin
          P.X := FClosedListButton.Left;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          p := ClientToScreen(p);
          if (GetSystemMetrics(SM_CYMENU) * Menu.Items.Count) + P.Y + 10 >
          {$IFDEF DELPHI6_LVL}
            Screen.MonitorFromPoint(P).Height then
          {$ELSE}
            Screen.Height then
          {$ENDIF}
          begin
            Dec(P.Y, (GetSystemMetrics(SM_CYMENU) * Menu.Items.Count) + (FClosedListButton.Height) + 4);
          end;
        end;
        tpLeft:
        begin
          P.X := FClosedListButton.Left + FClosedListButton.Width;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          p := ClientToScreen(p);
        end;
        tpRight:
        begin
          P.X := self.Left + Self.Width;
          P.Y := FClosedListButton.Top + FClosedListButton.Height;
          if Assigned(Parent) then
            p := Parent.ClientToScreen(p)
          else
            p := ClientToScreen(p);
        end;
      end;

      if Assigned(FOnClosedListClick) then
        FOnClosedListClick(Self, P.X, P.Y)
      else
        Menu.Popup(P.X, P.Y);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CreateClosedListButton;
begin
  if not Assigned(FClosedListButton) then
  begin
    FClosedListButton := TAdvGlowButton.Create(Self);
    FClosedListButton.Parent := Self;
    FClosedListButton.OnClick := ClosedListButtonClick;
  end;
  UpdateClosedListButton;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateClosedListButton;
var
  R: TRect;
begin
  if not Assigned(FClosedListButton) then
    Exit;

  FClosedListButton.Width := ButtonSettings.ButtonSize;
  FClosedListButton.Height := ButtonSettings.ButtonSize;
  FClosedListButton.Visible := FClosedPageList.Count > 0;

  if FClosedListButton.Visible then
  begin
    if Assigned(FCurrentOfficePagerStyler) then
    begin
      if Assigned(FGlowButtonAppearance) and FItones then
        SetGlowButtonColorTones(FClosedListButton)
      else
        FClosedListButton.Appearance.Assign(FCurrentOfficePagerStyler.GlowButtonAppearance);
    end;
    FClosedListButton.Picture.Assign(ButtonSettings.ClosedListButtonPicture);
    FClosedListButton.Hint := ButtonSettings.ClosedListButtonHint;
    FClosedListButton.ShowHint := True;

    R := GetClosedListButtonRect;
    FClosedListButton.Left := R.Left;
    FClosedListButton.Top := R.Top;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OpenAllClosedPages;
var
  i: integer;
begin
  for i := 0 to FClosedPageList.Count - 1 do
  begin
    TAdvOfficePage(FClosedPageList.Objects[i]).TabVisible := True;
  end;
  FClosedPageList.Clear;
  UpdateClosedListButton;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.OpenClosedPage(APage: TAdvOfficePage): Boolean;
var
  i: integer;
begin
  Result := false;
  if not Assigned(APage) then
    Exit;

  i := FClosedPageList.IndexOfObject(APage);
  if i >= 0 then
  begin
    TAdvOfficePage(FClosedPageList.Objects[i]).TabVisible := True;
    Result := true;
    if (GetVisibleTabCount = 1) then
      ActivePage := APage;

    UpdateClosedListButton;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CreateDropArrow;
var
  ad: TArrowDirection;
begin
  if not (csDesigning in ComponentState) and not Assigned(FArrow) then
  begin
    ad := arrDown;
    case TabPosition of
      tpTop, tpBottom: ad := arrDown;
      tpLeft: ad := arrLeft;
      tpRight: ad := arrLeft;
    end;

    FArrow := TArrowWindow.Init(Self, ad);
    FArrow.Color := FTabReorderIndicatorColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.CreateGlowTimer;
begin
  if not Assigned(FBrGlowTimer) then
  begin
    FBrGlowTimer := TTimer.Create(self);
    FBrGlowTimer.OnTimer := OnGlowTimerTime;
    FBrGlowTimer.Interval := GlowSpeed + 20;
    FBrGlowTimer.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.UpdateGlowTimer;
begin
  if not (csDesigning in ComponentState) and CanGlow and IsAnyTabGlowing then
  begin
    CreateGlowTimer;
    if not FBrGlowTimer.Enabled then
    begin
      FTimeInc := 20;
      FStepHover := 0;
      FBrGlowTimer.Enabled := True;
    end;
  end
  else if Assigned(FBrGlowTimer) and FBrGlowTimer.Enabled then
  begin
    FBrGlowTimer.Enabled := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficePager.OnGlowTimerTime(Sender: TObject);
var
  I: Integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    FStepHover := FStepHover + FTimeInc;
    if ((FStepHover > 140) and (FTimeInc > 0)) or ((FStepHover < 0) and (FTimeInc < 0)) then
    begin
      if (FStepHover > 140) and (FTimeInc > 0) then
      begin
        FTimeInc := -FTimeInc;
      end
      else if ((FStepHover < 0) and (FTimeInc < 0)) then
      begin
        FTimeInc := -FTimeInc;
      end;
    end;

    for I := 0 to AdvPageCount - 1 do
    begin
      if not AdvPages[I].TabVisible or not AdvPages[I].Enabled then
        Continue;

      if AdvPages[I].Glow and (AdvPages[I].GlowColor <> clNone) and ((I = ActivePageIndex) or (I = FHotPageIndex) or ShowNonSelectedTabs) then
      begin
        InvalidateTab(I);
      end;
    end;

  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IsAnyTabGlowing: Boolean;
var
  i: Integer;
begin
  Result := False;
  for I := 0 to AdvPageCount - 1 do
  begin
    if not AdvPages[I].TabVisible or not AdvPages[I].Enabled then
      Continue;

    if AdvPages[I].Glow and (AdvPages[I].GlowColor <> clNone) {and IsTabShowing(I)} then
    begin
      Result := True;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IsGlass: Boolean;
begin
  Result := TabSettings.Glass and FIsAeroVista and Transparent and not (csDesigning in ComponentState);
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IsOnGlass: Boolean;
begin
  Result := False;
  {$IFDEF DELPHI2007_LVL}
  if Assigned(Parent) and (Parent is TCustomForm) and TCustomForm(Parent).GlassFrame.Enabled then
    Result := IsGlass and (Align in [alTop, alBottom, alClient]);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function TAdvOfficePager.IsTabShowing(TabIndex: Integer): Boolean;
begin
  Result := CanShowTab(TabIndex);
  if Result then
  begin
    Result := GetTabRect(TabIndex).Left >= 0;
  end;
end;

//------------------------------------------------------------------------------

{ TPageButtonSettings }

constructor TPageButtonSettings.Create;
begin
  inherited;
  FPageListButton := False;
  FCloseButton := False;
  FClosedListButton := False;
  FCloseButtonLook := cblOffice;
  FButtonSize := PAGEBUTTON_SIZE;
  FShowInsertButton := False;

  FScrollButtonNextPicture := TGDIPPicture.Create;
  FScrollButtonNextPicture.LoadFromResourceName(hinstance,'TMSAONEXT');
  FScrollButtonNextPicture.OnChange := OnPictureChanged;
  FNextPictureChanged := False;

  FScrollButtonPrevPicture := TGDIPPicture.Create;
  FScrollButtonPrevPicture.LoadFromResourceName(hinstance,'TMSAOPREV');
  FScrollButtonPrevPicture.OnChange := OnPictureChanged;
  FPrevPictureChanged := False;

  FScrollButtonFirstPicture := TGDIPPicture.Create;
  FScrollButtonFirstPicture.LoadFromResourceName(hinstance,'TMSAOFIRST');
  FScrollButtonFirstPicture.OnChange := OnPictureChanged;
  FFirstPictureChanged := False;

  FScrollButtonLastPicture := TGDIPPicture.Create;
  FScrollButtonLastPicture.LoadFromResourceName(hinstance,'TMSAOLAST');
  FScrollButtonLastPicture.OnChange := OnPictureChanged;
  FLastPictureChanged := False;

  FPageListButtonPicture := TGDIPPicture.Create;
  FPageListButtonPicture.LoadFromResourceName(hinstance,'TMSAOLIST');
  FPageListButtonPicture.OnChange := OnPictureChanged;

  FCloseButtonPicture := TGDIPPicture.Create;
  FCloseButtonPicture.LoadFromResourceName(hinstance,'TMSAOCLOSE');
  FCloseButtonPicture.OnChange := OnPictureChanged;

  FClosedListButtonPicture := TGDIPPicture.Create;
  FClosedListButtonPicture.LoadFromResourceName(hinstance,'TMSAOLIST');
  FClosedListButtonPicture.OnChange := OnPictureChanged;

  FScrollButtonPrevHint := 'Previous';
  FScrollButtonNextHint := 'Next';
  FScrollButtonFirstHint := 'First';
  FScrollButtonLastHint := 'Last';
  FCloseButtonHint := 'Close';
  FPageListButtonHint := 'Page List';
  FClosedListButtonHint := 'Closed Pages';
  FInsertButtonHint := 'Insert new page';
  FScrollButtonsAlways := False;
  FFirstButton := false;
  FLastButton := false;

  FScrollButtonDownPicture := TGDIPPicture.Create;
  FScrollButtonDownPicture.LoadFromResourceName(hinstance,'TMSAODOWN');

  FScrollButtonUpPicture := TGDIPPicture.Create;
  FScrollButtonUpPicture.LoadFromResourceName(hinstance,'TMSAOUP');

  FScrollButtonDownLastPicture := TGDIPPicture.Create;
  FScrollButtonDownLastPicture.LoadFromResourceName(hinstance,'TMSAODOWNLAST');

  FScrollButtonUpFirstPicture := TGDIPPicture.Create;
  FScrollButtonUpFirstPicture.LoadFromResourceName(hinstance,'TMSAOUPFIRST');
end;

//------------------------------------------------------------------------------

destructor TPageButtonSettings.Destroy;
begin
  FScrollButtonNextPicture.Free;
  FScrollButtonPrevPicture.Free;
  FPageListButtonPicture.Free;
  FCloseButtonPicture.Free;
  FClosedListButtonPicture.Free;
  FScrollButtonDownPicture.Free;
  FScrollButtonUpPicture.Free;
  FScrollButtonFirstPicture.Free;
  FScrollButtonLastPicture.Free;
  FScrollButtonDownLastPicture.Free;
  FScrollButtonUpFirstPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.Assign(Source: TPersistent);
begin
  if (Source is TPageButtonSettings) then
  begin
    FPageListButton := (Source as TPageButtonSettings).FPageListButton;
    FCloseButton := (Source as TPageButtonSettings).FCloseButton;
    FClosedListButton := (Source as TPageButtonSettings).ClosedListButton;
    FScrollButtonNextPicture.Assign((Source as TPageButtonSettings).FScrollButtonNextPicture);
    FScrollButtonPrevPicture.Assign((Source as TPageButtonSettings).FScrollButtonPrevPicture);
    FPageListButtonPicture.Assign((Source as TPageButtonSettings).FPageListButtonPicture);
    FCloseButtonPicture.Assign((Source as TPageButtonSettings).FCloseButtonPicture);
    FClosedListButtonPicture.Assign((Source as TPageButtonSettings).FClosedListButtonPicture);
    FScrollButtonsAlways := (Source as TPageButtonSettings).ScrollButtonsAlways;
    FFirstButton := (source as TPageButtonSettings).FirstButton;
    FLastButton := (source as TPageButtonSettings).LastButton;
    FScrollButtonFirstPicture.Assign((Source as TPageButtonSettings).ScrollButtonFirstPicture);
    FScrollButtonLastPicture.Assign((Source as TPageButtonSettings).ScrollButtonLastPicture);
    FScrollButtonPrevHint := (Source as TPageButtonSettings).ScrollButtonPrevHint;
    FScrollButtonNextHint := (Source as TPageButtonSettings).ScrollButtonNextHint;
    FScrollButtonFirstHint := (Source as TPageButtonSettings).ScrollButtonFirstHint;
    FScrollButtonLastHint := (Source as TPageButtonSettings).ScrollButtonLastHint;
    CloseButtonHint := (Source as TPageButtonSettings).CloseButtonHint;
    InsertButtonHint := (Source as TPageButtonSettings).InsertButtonHint;
    PageListButtonHint := (Source as TPageButtonSettings).PageListButtonHint;
    ClosedListButtonHint := (Source as TPageButtonSettings).ClosedListButtonHint;
    CloseButtonLook := (Source as TPageButtonSettings).CloseButtonLook;
    ShowInsertButton := (Source as TPageButtonSettings).ShowInsertButton;
    ButtonSize := (Source as TPageButtonSettings).ButtonSize;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.OnPictureChanged(Sender: TObject);
begin
  FNextPictureChanged := FNextPictureChanged or (Sender = FScrollButtonNextPicture);
  FPrevPictureChanged := FPrevPictureChanged or (Sender = FScrollButtonPrevPicture);
  FLastPictureChanged := FLastPictureChanged or (Sender = FScrollButtonLastPicture);
  FFirstPictureChanged := FFirstPictureChanged or (Sender = FScrollButtonFirstPicture);
  
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetButtonSize(const Value: Integer);
begin
  if (FButtonSize <> Value) then
  begin
    FButtonSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetCloseButton(const Value: Boolean);
begin
  if (FCloseButton <> Value) then
  begin
    FCloseButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetCloseButtonLook(const Value: TCloseButtonLook);
begin
  if (FCloseButtonLook <> Value) then
  begin
    FCloseButtonLook := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetClosedListButton(const Value: Boolean);
begin
  if (FClosedListButton <> Value) then
  begin
    FClosedListButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetClosedListButtonPicture(
  const Value: TGDIPPicture);
begin
  FClosedListButtonPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetCloseButtonPicture(
  const Value: TGDIPPicture);
begin
  FCloseButtonPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetFirstButton(const Value: Boolean);
begin
  if FFirstButton <> value then
  begin
    FFirstButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetLastButton(const Value: Boolean);
begin
  if FLastButton <> value then
  begin
    FLastButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetPageListButton(const Value: boolean);
begin
  if (FPageListButton <> Value) then
  begin
    FPageListButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetPageListButtonPicture(
  const Value: TGDIPPicture);
begin
  FPageListButtonPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetScrollButtonFirstPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonFirstPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetScrollButtonLastPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonLastPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetScrollButtonNextPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonNextPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetScrollButtonPrevPicture(
  const Value: TGDIPPicture);
begin
  FScrollButtonPrevPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetScrollButtonsAlways(const Value: Boolean);
begin
  if (FScrollButtonsAlways <> Value) then
  begin
    FScrollButtonsAlways := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TPageButtonSettings.SetShowInsertButton(const Value: Boolean);
begin
  if (FShowInsertButton <> Value) then
  begin
    FShowInsertButton := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvPagerButton }

constructor TAdvPagerButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTransparent := True;
  FDrawClose := True;

  FIPicture := TGDIPPicture.Create;
  FIDisabledPicture := TGDIPPicture.Create;
  FIHotPicture := TGDIPPicture.Create;
  FIDownPicture := TGDIPPicture.Create;

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';
  ShowHint := False;
end;

//------------------------------------------------------------------------------

destructor TAdvPagerButton.Destroy;
begin
  FIPicture.Free;
  FIDisabledPicture.Free;
  FIHotPicture.Free;
  FIDownPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) or FMouseEnter then
    Exit;

  FMouseEnter := True;
  if Enabled then
    Invalidate;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) or not FMouseEnter then
    Exit;

  FMouseEnter := false;

  if Enabled then
    Invalidate;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button <> mbLeft) or not Enabled or (csDesigning in ComponentState) then
    Exit;

  FMouseDown := true;
  if Enabled then
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if PtInRect(ClientRect, Point(X, Y)) then
  begin
    if not FMouseEnter then
    begin
      FMouseEnter := True;
      Invalidate;
    end
  end
  else
  begin
    if FMouseEnter then
    begin
      FMouseEnter := False;
      Invalidate;
    end
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  Invalidate;

  FMouseDown := false;
  if Enabled and FMouseEnter then
    Click;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.Paint;
var
  Pic: TGDIPPicture;
  g: TGPGraphics;
  R: TRect;
  br: TGPSolidBrush;
  p: TGPPen;
  j, w, h: Integer;
begin
  R := ClientRect;

  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  if DrawClose then
  begin
    j := Width div 3;
    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    if FMouseDown and FMouseEnter then
    begin
      //--- DownPicture
      br := TGPSolidBrush.Create(ColorToARGB(RGB(67, 30, 32)));
      g.FillEllipse(br, 2, 1, W-3, H-3);
      br.Free;

      p := TGPPen.Create(MakeColor(220, 235, 235, 235), 1.6);
      g.DrawLine(p, j, j-1, W - j + 1, H - j);
      g.DrawLine(p, W - j + 1, j - 1, j, H - j);
      p.Free;
    end
    else if FMouseEnter then
    begin
      //--- HotPicture
      br := TGPSolidBrush.Create(ColorToARGB(RGB(192, 53, 53)));
      g.FillEllipse(br, 2, 1, W-3, H-3);
      br.Free;

      p := TGPPen.Create(MakeColor(220, 235, 235, 235), 1.6);
      g.DrawLine(p, j, j-1, W - j + 1, H - j);
      g.DrawLine(p, W - j + 1, j - 1, j, H - j);
      p.Free;
    end
    else    //--- Normal
    begin
      p := TGPPen.Create(MakeColor(220, 165, 165, 165), 1.6);
      g.DrawLine(p, j, j - 1, W - j + 1, H - j);
      g.DrawLine(p, W - j + 1, j - 1, j, h - j);
      p.Free;
    end;
  end
  else
  begin
    if Enabled or DisabledPicture.Empty then
    begin
      if FMouseDown and not DownPicture.Empty then
        Pic := DownPicture
      else if FMouseEnter and not HotPicture.Empty then
        Pic := HotPicture
      else
        Pic := Picture;
    end
    else
      Pic := DisabledPicture;

    DrawGDIPImage(g, R, Pic);
  end;
  g.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.SetDisabledPicture(const Value: TGDIPPicture);
begin
  FIDisabledPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.SetDownPicture(const Value: TGDIPPicture);
begin
  FIDownPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.SetHotPicture(const Value: TGDIPPicture);
begin
  FIHotPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.SetPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.Click;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvPagerButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------
{ TFloatingPagerWindow }

function TFloatingPagerWindow.AdvPager: TAdvOfficePager;
begin
  Result := FAdvPager;
end;

//------------------------------------------------------------------------------

procedure TFloatingPagerWindow.SetAdvPager(Pager: TAdvOfficePager);
begin
  FAdvPager := Pager;
end;

//------------------------------------------------------------------------------

{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}


initialization
  WM_OPDESTROYCLOSEBTN := RegisterWindowMessage('OPDESTROYCLOSEBTN');

{$IFDEF FREEWARE}
  if  (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
      (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
  begin
    MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
  end
{$ENDIF}


end.
