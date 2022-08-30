{***************************************************************************}
{ TAdvGlowButton component                                                  }
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

unit AdvGlowButton;

{$R ADVGLOWBUTTONDB.RES}

{$I TMSDEFS.INC}

{$T-}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, Menus, ImgList, AdvGDIP, GDIPicture, ActnList,
  AdvHintInfo, AdvStyleIF, ActiveX, Types, CommCtrl
  {$IFNDEF TMS_STD}
  , DB
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  DropDownSectWidth = 14;

  MAJ_VER = 2; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.0.5.1 : Fixed issue with width & height initialization
  // 1.0.5.2 : Improved fade painting
  // 1.1.0.0 : New separate dropdown button hot & down effect
  //         : Silver, Blue, Black styles added
  // 1.2.0.0 : New DropDownSplit property added
  // 1.2.0.1 : Fixed issue with ModalResult <> mrNone
  // 1.2.0.2 : Fixed issue with Action handling Checked state
  // 1.2.0.3 : Fixed issue with disabled painting
  // 1.2.0.4 : Fixed issue with key handling
  // 1.3.0.0 : Added new property FocusType
  //         : Added new ShortCutHint, ShortCutHintPos & methods ShowShortCutHint, HideShortCutHint
  // 1.3.0.1 : Fixed issue with font and aaNone
  // 1.3.0.2 : Fixed issue with hot & down border painting
  // 1.3.1.0 : New : exposed OnMouseEnter, OnMouseLeave
  //         : Fixed issue with Down property for bsCheck style
  // 1.3.1.1 : Fixed issue with Down property for buttons with GroupIndex > 0
  // 1.3.1.2 : Improved transitioning from transparent to hot
  // 1.3.1.3 : Fixed issue with actionlinks & bsCheck type
  // 1.3.2.0 : New styler interface added
  // 1.3.3.0 : New public property DroppedDown added
  // 1.3.4.0 : New TAdvCustomGlowButton.ParentFont added
  //         : TButtonLayout blGlyphLeftAdjusted and blGlyphRightAdjusted added
  // 1.3.5.0 : New borderless display possible by setting BorderStyle = bsNone
  // 1.4.0.0 : Improved : seamlessly works with TrueType & non TrueType fonts
  //         : New : Spacing property added
  //         : New : WordWrap property added
  //         : New : AutoSize property added
  //         : New : MarginVert property added
  //         : New : MarginHorz property added
  //         : New : Rounded property added
  //         : New : DropDownDirection property added
  //         : New : HotImages, HotPicture property added
  // 1.4.5.0 : New : PopupMenu property added
  //         : New : OnDrawButton event added
  //         : New : TButtonLayout blGlyphTopAdjusted and blGlyphBottomAdjusted added
  // 1.4.6.0 : New : support for Office 2007 silver style added
  // 1.4.6.1 : Fixed : issue with Win98 resource leak
  // 1.5.0.0 : New : support for Unicode text via public property WideCaption
  //         : Improved : text drawing in aaNone AntiAlias mode
  // 1.5.0.1 : Fix for use with fonts that are not installed
  // 1.6.0.0 : New : support for Trimming added
  // 1.6.0.1 : Fixed : issue with Action images
  // 1.7.0.0 : New : Repeat functionality added with repeat initial delay & frequency setting
  //         : Improved wordwrap drawing  with no text aliasing
  //         : New : support for using \n newline specifier in property inspector
  // 1.7.0.1 : Fixed : drawing issue with Delphi 2007
  // 1.7.1.0 : New : F4 key to open attached dropdown menu
  // 1.7.1.1 : Fixed : issue with DropDownSplit and OnClick event handler
  // 1.7.2.0 : New : events OnEnter, OnExit added
  // 1.7.2.1 : Improved : painting on MDI child windows
  // 1.7.2.2 : Fixed : drawing issue with Delphi 2007
  // 1.8.0.0 : New : Notes & NotesFont
  //         : New : C++Builder 2007 support
  //         : Improved : drawing down state for Transparent button
  //         : Improved : drawing speed
  // 1.8.0.1 : Fixed : runtime WideCaption assigning causes repaint
  // 1.8.1.0 : Fixed : issue with inherited forms
  // 1.8.1.1 : Fixed : issue with dbl click event
  //         : Fixed : issue with actions & groupindex
  //         : Fixed : border painting issue on checked buttons in bpMiddle, bpRight position
  // 1.8.1.2 : Fixed : issue with ShowCaption & WideCaption
  // 1.8.1.3 : Fixed : issue with using font not installed on the system
  // 1.8.1.4 : Fixed : issue with WideCaption & aaNone AntiAlias type
  // 1.8.1.5 : Fixed : issue with DblClick & OnClick event
  // 1.8.1.6 : Fixed : issue with AutoCheck action items for bsCheck button type
  // 1.8.1.7 : Fixed : issue with shortcuts on TAdvToolBar
  //         : Fixed : issue with dbl click
  //         : Improved : dropdown button position
  // 1.8.1.8 : Improved : wordwrapped text drawing for non anti aliased text
  // 1.8.1.9 : Improved : spacing for blGlyphTop, blGlyphTopAdjusted setting
  // 1.8.1.10: Improved : assigning images via action
  // 1.8.2.0 : New : shortcut hint position : shpBelowBottomCenter
  // 1.8.2.1 : Fixed : painting issue with default key handling
  // 1.8.2.2 : Fixed : issue with focus border drawing
  // 1.8.2.3 : Fixed : issue with spacing for glyph right / glyph right adjusted
  // 1.8.3.0 : New : exposed DoDropDown method
  // 1.8.3.1 : Fixed : issue with static imagelist versus actionlist imagelist use
  // 1.8.3.2 : Improved : vertical alignment of Notes text & caption with word wrap
  // 1.8.3.3 : Fixed : issue with accelerator key handling and wide captions
  // 1.8.4.0 : Improved : adaptions for use on Windows Vista style ribbon
  // 1.8.4.1 : Fixed : issue with PNG transparency drawing
  // 1.8.4.2 : Fixed : issue with dbl click and modal forms shown from button click
  // 1.8.4.3 : Fixed : text positioning for aaNone text with GlyphTop layout
  // 1.8.4.4 : Improved : text centering
  // 1.8.4.5 : Fixed : issue with maximizing form in combination with specific form/button position
  // 1.8.4.6 : Improved : painting when Rounded = false
  // 1.8.4.7 : Fixed : issue when RoundMode is set to rmUp
  // 1.9.0.0 : New : Terminal, Vista & Windows 7 styles
  // 1.9.0.1 : Fixed : issue with imagelists on data modules
  // 1.9.1.0 : Improved : Windows 7 Scenic Ribbon support
  // 1.9.1.1 : Fixed : issue with size states on ribbon
  // 1.9.1.2 : Fixed : issue with assigning action and checked actions
  // 2.0.0.0 : New : Built in support for Office 2010 colors
  // 2.0.1.0 : New : ImageListType property added to choose between actionlist imagelist or own imagelist 
  //         : Fixed : Issue with AutoSize = true during form loading
  // 2.0.1.1 : Fixed : Issue with button appearance while dropdown menu is displayed
  // 2.0.1.2 : Fixed : Issue with transparent button appearance while dropdown menu is displayed
  // 2.0.1.3 : Fixed : Issue with actions & helpcontext
  // 2.0.1.4 : Fixed : Issue with using imagelist images for TDBAdvGlowButton
  // 2.0.1.5 : Fixed : Issue with GroupIndex > 0 and Transparent = true
  // 2.0.2.0 : New : TAdvGlowButton uses TActionManager disabled images when possible
  // 2.0.2.1 : New : Exposed property OnEndDrag
  // 2.0.2.2 : Fixed : Hovered checkbox style down painting
  // 2.1.0.0 : New : Appearance.TextColorHot, Appearance.TextColorDown added
  // 2.1.0.1 : Fixed : Issue with TrueType font & Notes on WinXP fixed
  //         : Fixed : Drawing of transparent dropdown buttons in Office 2010 mode
  // 2.1.0.2 : Fixed : Issue with Wordwrap & notes on WinXP
  // 2.1.0.3 : Fixed : Issue with persistence of ShowDisabled property
  // 2.1.0.4 : Fixed : Issue with initial down state
  // 2.1.0.5 : Improved : Set DropDownMenu.PopupComponent when it is dropped down
  // 2.1.0.6 : Fixed : Issue with setting Down state on grouped button when parent handle is not allocated
  // 2.1.0.7 : Fixed : Issue with blGlyphLeftAdjusted and AntiAlias = aaNone
  // 2.1.0.8 : Fixed : bpLeft, bpMiddle, bpRight border color issue
  //         : Fixed : Issue with GroupIndex > 0 and TabStop = true
  // 2.1.1.0 : Improved : Exposed PerformResize as protected function
  // 2.1.1.1 : Fixed : Issue with button groups & actions
  // 2.1.1.2 : Improved : Accel char display
  // 2.1.1.3 : Fixed : Issue with shortcut hints in specific circumstances
  // 2.1.1.4 : Fixed : Issue with radiogroup functionality and keyboard handling
  // 2.1.1.5 : Fixed : Issue with constraints & autosize
  // 2.1.2.0 : New : Support for DropDownMenu Alignment control
  // 2.1.3.0 : New : Appearance.TextColorChecked property added
  // 2.2.0.0 : New : Windows 8, Office 2013 styles added
  // 2.2.1.0 : New : Automatic use of assigned ActionManager small & large imagelists depending on button state
  // 2.2.1.1 : Fixed : Issue with focus change on form activation
  // 2.2.1.2 : Fixed : Issue with use of action managers that have no largeimages specified
  // 2.2.2.0 : Improved : Use of MarginHorz in combination with Layout = blGlyphLeftAdjusted
  // 2.2.3.0 : Improved : Drawing of disabled images when no DisabledImages list is assigned
  // 2.2.3.1 : Fixed : Issue with autosize and use of WideCaption
  // 2.2.3.2 : Fixed : Issue with improved disabled image drawing on older operating systems
  // 2.2.3.3 : Fixed : Issue with Assign() proc
  // 2.2.3.4 : Fixed : Issue with drawing disabled imagelist images on 64bit
  // 2.2.3.5 : Fixed : Issue with message handling in 64bit
  // 2.3.0.0 : New : Enhancements for Windows 8 / Office 2013 style support on ribbon
  // 2.3.0.1 : Fixed : Use of AutoSize and runtime assignment of Notes
  // 2.3.0.2 : Fixed : Paint issue with autosize during specific circumstances
  // 2.3.0.3 : Fixed : Vertically centering of dropdown button
  // 2.3.0.4 : Improved : Small improvement in Windows 8 color style
  // 2.3.0.5 : Fixed : Issue with Alt-F4 handling
  // 2.3.1.0 : Improved : SetComponentStyle is now virtual function to allow to override it
  // 2.3.2.0 : New : AlwaysShowAccel property added
  // 2.3.2.1 : Fixed : Issue with changing font color at runtime
  // 2.3.2.2 : Fixed : Small issue with note size calculation in aaNone AntiAlias mode
  // 2.3.2.3 : Fixed : Issue with glGlyphLeftAdjusted layout and AntiAlias aaNone
  // 2.3.3.0 : Improved : Accelerator keys can be used without Alt key when used outside toolbar or ribbon
  // 2.3.4.0 : Improved : Behavior on touch screens
  // 2.3.5.0 : Improved : Behavior in Hi DPI modes
  // 2.3.5.1 : Fixed : Issue with Autosize for multiline captions in button
  // 2.4.0.0 : New : Windows 10, Office 2016 styles added
  //         : New : Property Appearance.BorderColorCheckedHot added
  // 2.4.0.1 : Fixed : Picture centering when dropdown button is visible
  // 2.4.0.2 : Fixed : Issue with Office2016 Black style font color
  // 2.4.0.3 : Improved : Drawing of imagelist images in button on QAT
  // 2.4.1.0 : Improved : High-DPI handling
  //         : Improved : Dropdown arrow drawing for multiline button captions
  // 2.4.1.1 : Improved : Shortcut handling on buttons

type
  TAdvCustomGlowButton = class;
  TAdvGlowButton = class;

  TGlowState = (gsHover, gsPush, gsNone);
  TAdvButtonStyle = (bsButton, bsCheck);
  TAdvButtonState = (absUp, absDisabled, absDown, absDropDown, absExclusive);
  TButtonLayout = (blGlyphLeft, blGlyphTop, blGlyphRight, blGlyphBottom,
                   blGlyphLeftAdjusted, blGlyphRightAdjusted,
                   blGlyphTopAdjusted, blGlyphBottomAdjusted);

  TDropDownPosition = (dpRight, dpBottom);
  TDropDownDirection = (ddDown, ddRight);
  TGDIPGradient = (ggRadial, ggVertical, ggDiagonalForward, ggDiagonalBackward);

  TFocusType = (ftBorder, ftHot, ftHotBorder, ftNone);

  TShortCutHintPos = (shpLeft, shpTop, shpRight, shpBottom, shpCenter, shpAuto,
                      shpTopLeft, shpTopRight, shpAboveTop, shpAboveTopLeft,
                      shpAboveTopRight, shpBottomLeft, shpBottomRight, shpBelowBottom,
                      shpBelowBottomLeft, shpBelowBottomRight, shpBelowBottomCenter);

  TButtonPosition = (bpStandalone, bpLeft, bpMiddle, bpRight);

  TGlowButtonState = (gsNormal, gsHot, gsDown);

  TButtonSizeState = (bsGlyph, bsLabel, bsLarge);

  TGlowButtonDrawEvent = procedure(Sender: TObject; Canvas: TCanvas; Rect: TRect; State: TGlowButtonState) of object;
  TSetButtonSizeEvent = procedure(Sender: TObject; var W, H: Integer) of object;
  TOnGetShortCutHintPos = procedure(Sender: TObject; ButtonSizeState: TButtonSizeState; var ShortCutHintPosition: TShortCutHintPos) of object;

  TImageListType = (ipActionList, ipOwnLists);//, ipToolBarLists);

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvGlowButtonActionLink = class(TControlActionLink)
  protected
    FImageIndex: Integer;
    FClient: TAdvCustomGlowButton; //TAdvGlowButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
    function IsHelpLinked: Boolean;  override;
    procedure SetHelpContext(Value: THelpContext); override;
    procedure SetHelpKeyword(const Value: string); override;
    procedure SetHelpType(Value: THelpType); override;
  end;

  TShortCutHintWindow = class(THintWindow)
  private
    FColor: TColor;
    FColorTo: TColor;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
  protected
    procedure Resize; override;
    procedure Paint; override;
    procedure CreateParams(var Params:TCreateParams);override;
  published
    property Color: TColor read FColor write FColor;
    property ColorTo: TColor read FColorTo write FColorTo;
  end;

  TGlowButtonAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderColorHot: TColor;
    FBorderColorDown: TColor;
    FBorderColorDisabled: TColor;
    FBorderColorChecked: TColor;
    FBorderColorCheckedHot: TColor;
    FBorderColorFocused: TColor;
    FColor: TColor;
    FColorTo: TColor;
    FColorDown: TColor;
    FColorDownTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorCheckedTo: TColor;
    FColorDisabled: TColor;
    FColorDisabledTo: TColor;
    FColorChecked: TColor;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FColorMirrorHot: TColor;
    FColorMirrorHotTo: TColor;
    FColorMirrorDown: TColor;
    FColorMirrorDownTo: TColor;
    FGradientDown: TGDIPGradient;
    FGradientMirror: TGDIPGradient;
    FGradientMirrorHot: TGDIPGradient;
    FGradient: TGDIPGradient;
    FGradientMirrorDown: TGDIPGradient;
    FGradientHot: TGDIPGradient;
    FColorMirrorDisabledTo: TColor;
    FColorMirrorDisabled: TColor;
    FColorMirrorCheckedTo: TColor;
    FColorMirrorChecked: TColor;
    FGradientChecked: TGDIPGradient;
    FGradientDisabled: TGDIPGradient;
    FGradientMirrorChecked: TGDIPGradient;
    FGradientMirrorDisabled: TGDIPGradient;
    FTextColorHot: TColor;
    FTextColorDown: TColor;
    FSystemFont: boolean;
    FTranspHover: boolean;
    FNoBorderDefault: boolean;
    FTextColorChecked: TColor;
    FTextColor: TColor;
    procedure SetSystemFont(const Value: boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorChecked(const Value: TColor);
    procedure SetBorderColorDisabled(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetBorderColorHot(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetColorCheckedTo(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorDisabledTo(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorChecked(const Value: TColor);
    procedure SetColorMirrorCheckedTo(const Value: TColor);
    procedure SetColorMirrorDisabled(const Value: TColor);
    procedure SetColorMirrorDisabledTo(const Value: TColor);
    procedure SetColorMirrorDown(const Value: TColor);
    procedure SetColorMirrorDownTo(const Value: TColor);
    procedure SetColorMirrorHot(const Value: TColor);
    procedure SetColorMirrorHotTo(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradient(const Value: TGDIPGradient);
    procedure SetGradientChecked(const Value: TGDIPGradient);
    procedure SetGradientDisabled(const Value: TGDIPGradient);
    procedure SetGradientDown(const Value: TGDIPGradient);
    procedure SetGradientHot(const Value: TGDIPGradient);
    procedure SetGradientMirror(const Value: TGDIPGradient);
    procedure SetGradientMirrorChecked(const Value: TGDIPGradient);
    procedure SetGradientMirrorDisabled(const Value: TGDIPGradient);
    procedure SetGradientMirrorDown(const Value: TGDIPGradient);
    procedure SetGradientMirrorHot(const Value: TGDIPGradient);
    procedure SetTextColorDown(const Value: TColor);
    procedure SetTextColorHot(const Value: TColor);
    procedure SetTextColorChecked(const Value: TColor);
    procedure SetBorderColorFocused(const Value: TColor);
    procedure SetTextColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property TranspHover: boolean read FTranspHover write FTranspHover;
    property NoBorderDefault: boolean read FNoBorderDefault write FNoBorderDefault;
    property TextColor: TColor read FTextColor write SetTextColor;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clSilver;
    property BorderColorHot: TColor read FBorderColorHot write SetBorderColorHot default clBlue;
    property BorderColorCheckedHot: TColor read FBorderColorCheckedHot write FBorderColorCheckedHot default clNone;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown default clNavy;
    property BorderColorChecked: TColor read FBorderColorChecked write SetBorderColorChecked default clBlue;
    property BorderColorDisabled: TColor read FBorderColorDisabled write SetBorderColorDisabled default clGray;
    property BorderColorFocused: TColor read FBorderColorFocused write SetBorderColorFocused default $E4AD89;
    property Color: TColor read FColor write SetColor default clWhite;
    property ColorTo: TColor read FColorTo write SetColorTo default clWhite;
    property ColorChecked: TColor read FColorChecked write SetColorChecked;
    property ColorCheckedTo: TColor read FColorCheckedTo write SetColorCheckedTo;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled;
    property ColorDisabledTo: TColor read FColorDisabledTo write SetColorDisabledTo;
    property ColorDown: TColor read FColorDown write SetColorDown;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property ColorMirror: TColor read FColorMirror write SetColorMirror default clSilver;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo default clWhite;
    property ColorMirrorHot: TColor read FColorMirrorHot write SetColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write SetColorMirrorHotTo;
    property ColorMirrorDown: TColor read FColorMirrorDown write SetColorMirrorDown;
    property ColorMirrorDownTo: TColor read FColorMirrorDownTo write SetColorMirrorDownTo;
    property ColorMirrorChecked: TColor read FColorMirrorChecked write SetColorMirrorChecked;
    property ColorMirrorCheckedTo: TColor read FColorMirrorCheckedTo write SetColorMirrorCheckedTo;
    property ColorMirrorDisabled: TColor read FColorMirrorDisabled write SetColorMirrorDisabled;
    property ColorMirrorDisabledTo: TColor read FColorMirrorDisabledTo write SetColorMirrorDisabledTo;
    property Gradient: TGDIPGradient read FGradient write SetGradient default ggVertical;
    property GradientMirror: TGDIPGradient read FGradientMirror write SetGradientMirror default ggVertical;
    property GradientHot: TGDIPGradient read FGradientHot write SetGradientHot default ggRadial;
    property GradientMirrorHot: TGDIPGradient read FGradientMirrorHot write SetGradientMirrorHot default ggRadial;
    property GradientDown: TGDIPGradient read FGradientDown write SetGradientDown default ggRadial;
    property GradientMirrorDown: TGDIPGradient read FGradientMirrorDown write SetGradientMirrorDown default ggRadial;
    property GradientChecked: TGDIPGradient read FGradientChecked write SetGradientChecked default ggRadial;
    property GradientMirrorChecked: TGDIPGradient read FGradientMirrorChecked write SetGradientMirrorChecked default ggVertical;
    property GradientDisabled: TGDIPGradient read FGradientDisabled write SetGradientDisabled default ggRadial;
    property GradientMirrorDisabled: TGDIPGradient read FGradientMirrorDisabled write SetGradientMirrorDisabled default ggRadial;
    property SystemFont: boolean read FSystemFont write SetSystemFont default true;
    property TextColorChecked: TColor read FTextColorChecked write SetTextColorChecked default clBlack;
    property TextColorDown: TColor read FTextColorDown write SetTextColorDown default clBlack;
    property TextColorHot: TColor read FTextColorHot write SetTextColorHot default clBlack;
  end;

  TAdvCustomGlowButton = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FActive: Boolean;
    FDown: Boolean;
    FLeftDown: Boolean;
    FMouseDown: Boolean;
    FTimer: TTimer;
    FStepHover: Integer;
    FStepPush: Integer;
    FTimeInc: Integer;
    FGlowState: TGlowState;
    FImages: TCustomImageList;
    FImageIndex: TImageIndex;
    FState: TAdvButtonState;
    FMouseInControl: Boolean;
    FHasMouse: boolean;
    FMouseEnter: Boolean;
    FDownChecked: Boolean;
    FInitialDown: Boolean;
    FDragging: Boolean;
    FStyle: TAdvButtonStyle;
    FGroupIndex: Integer;
    FAllowAllUp: Boolean;
    FTransparent: Boolean;
    FLayout: TButtonLayout;
    FDropDownButton: Boolean;
    FDropDownSplit: Boolean;
    FDropDownDirection: TDropDownDirection;
    FDropDownMenu: TPopupMenu;
    FOnDropDown: TNotifyEvent;
    FDropDownPosition: TDropDownPosition;
    FAppearance: TGlowButtonAppearance;
    FDisabledImages: TCustomImageList;
    FInternalImages: TCustomImageList;
    FHotImages: TCustomImageList;
    FIPicture: TGDIPPicture;
    FIDisabledPicture: TGDIPPicture;
    FIHotPicture: TGDIPPicture;
    FShowCaption: Boolean;
    FAntiAlias: TAntiAlias;
    FModalResult: TModalResult;
    FDefault: boolean;
    FCancel: Boolean;
    FInButton: Boolean;
    FBorderStyle: TBorderStyle;
    FButtonPosition: TButtonPosition;
    FOfficeHint: TAdvHintInfo;
    FCheckLinked: Boolean;
    FGroupIndexLinked: Boolean;
    FFocusType: TFocusType;
    FShortCutHint: TShortCutHintWindow;
    FShortCutHintPos: TShortCutHintPos;
    FShortCutHintText: string;
    FShowDisabled: Boolean;
    FOnInternalKeyDown: TKeyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FDroppedDown: Boolean;
    FOverlappedText: Boolean;
    FSpacing: Integer;
    FAutoSize: Boolean;
    FWordWrap: Boolean;
    FDoAutoSize: Boolean;
    FFirstPaint: Boolean;
    FMarginVert: integer;
    FMarginHorz: integer;
    FRounded: Boolean;
    FOnDrawButton: TGlowButtonDrawEvent;
    FWideCaption: widestring;
    FTrimming: TStringTrimming;
    FRepeatTimer: TTimer;
    FInitRepeatPause: Integer;
    FRepeatPause: Integer;
    FRepeatClick: Boolean;
    FPainting: Boolean;
    FOnInternalClick: TNotifyEvent;
    FButtonSizeState: TButtonSizeState;
    FMaxButtonSizeState: TButtonSizeState;
    FOnSetButtonSize: TSetButtonSizeEvent;
    FOldLayout: TButtonLayout;
    FOldDropDownPosition: TDropDownPosition;
    FMinButtonSizeState: TButtonSizeState;
    FParentForm: TCustomForm;
    FIsVista: boolean;
    FNotes: TStringList;
    FNotesFont: TFont;
    FGotButtonClick: boolean;
    FOnGetShortCutHintPos: TOnGetShortCutHintPos;
    FHasFocus: boolean;
    FStaticButton: Boolean;
    FKeepDown: Boolean;
    FImageChangeLink: TChangeLink;
    FDisabledImageChangeLink: TChangeLink;
    FHotImageChangeLink: TChangeLink;
    FImageListType: TImageListType;
    FOnInternalDropDown: TNotifyEvent;
    FInternalIsOnGlassQAT: Boolean;
    FCompStyle: TTMSStyle;
    FAlwaysShowAccel: boolean;
    {$IFDEF DELPHI2006_LVL}
    class var FStaticActionImageIndex: boolean;
    {$ENDIF}
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetButtonPosition(const Value: TButtonPosition);
    procedure SetBorderStyle(const Value: TBorderStyle);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetDefault(const Value: boolean);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetDisabledPicture(const Value: TGDIPPicture);
    procedure SetHotPicture(const Value: TGDIPPicture);
    procedure SetPicture(const Value: TGDIPPicture);
    procedure SetTransparent(const Value: Boolean);
    procedure UpdateExclusive;
    procedure UpdateTracking;
    procedure SetImageIndex(const Value: TImageIndex);
    procedure SetImages(const Value: TCustomImageList);
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMLButtonDown(var Msg:TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg:TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMLDblClk(var Msg: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure TimerProc(Sender: TObject);
    procedure OnAppearanceChanged(Sender: TObject);
    procedure SetDown(Value: Boolean);
    procedure SetGroupIndex(const Value: Integer);
    procedure SetAllowAllUp(const Value: Boolean);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetDropDownButton(const Value: Boolean);
    procedure PopupBtnDown;
    procedure SetDropDownPosition(const Value: TDropDownPosition);
    procedure SetDropDownDirection(const Value: TDropDownDirection);
    procedure SetAppearance(const Value: TGlowButtonAppearance);
    procedure SetDisabledImages(const Value: TCustomImageList);
    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure HotImageListChange(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure SetSpacing(const Value: integer);
    procedure SetAutoSizeEx(const Value: boolean);
    procedure SetShowDisabled(const Value: boolean);
    procedure SetWordWrap(const Value: boolean);
    procedure SetMarginVert(const Value: integer);
    procedure SetMarginHorz(const Value: integer);
    procedure SetRounded(const Value: boolean);
    procedure SetTrimming(const Value: TStringTrimming);
    function IsFontStored: Boolean;
    procedure SetButtonSizeState(const Value: TButtonSizeState);
    procedure SetMaxButtonSizeState(const Value: TButtonSizeState);
    procedure SetMinButtonSizeState(const Value: TButtonSizeState);
    procedure SetNotes(const Value: TStrings);
    function GetNotes: TStrings;
    procedure SetNotesFont(const Value: TFont);
    procedure SetWideCaption(const Value: widestring);
    procedure SetHotImages(const Value: TCustomImageList);
    procedure SetImageListType(const Value: TImageListType);
    function ActionHasImages: boolean;
    procedure SetAlwaysShowAccel(const Value: boolean);
//    procedure SetCaption(const Value: string);
//    function GetCaption: string;
  protected
    FHot: Boolean;
    FDefaultPicDrawing: Boolean;
    FDefaultCaptionDrawing: Boolean;
    FCustomizerCreated: Boolean;
    FCommandID: Integer;
    FIcon: TIcon;
    procedure SetStyle(const Value: TAdvButtonStyle);
    procedure TimerExpired(Sender: TObject); virtual;
    procedure DrawGlyphCaption; virtual;
    procedure GetToolImage(bmp: TBitmap); virtual;
    procedure GetToolPicture(var pic: TGDIPPicture); virtual;
    procedure SetDroppedDown(Value: Boolean);
    procedure CreateParams(var Params:TCreateParams); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property AlwaysShowAccel: boolean read FAlwaysShowAccel write SetAlwaysShowAccel default False;
    property GlowState: TGlowState read FGlowState write FGlowState;
    property Down: Boolean read FDownChecked write SetDown default False;
    property Style: TAdvButtonStyle read FStyle write SetStyle default bsButton;
    property State: TAdvButtonState read FState write FState;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property DroppedDown: Boolean read FDroppedDown;
    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton default False;
    property DropDownDirection: TDropDownDirection read FDropDownDirection write SetDropDownDirection default ddDown;
    property DropDownPosition: TDropDownPosition read FDropDownPosition write SetDropDownPosition default dpRight;
    property DropDownSplit: Boolean read FDropDownSplit write FDropDownSplit default true;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    function GetVersionNr: Integer; virtual;
    function IsMenuButton: Boolean; virtual;
    function CanDrawBorder: Boolean; virtual;
    function CanDrawFocused: Boolean; virtual;
    procedure InternalClick;
    procedure PerformResize;
    property CheckLinked: Boolean read FCheckLinked write FCheckLinked;
    property GroupIndexLinked: Boolean read FGroupIndexLinked write FGroupIndexLinked;
    property OnInternalKeyDown: TKeyEvent read FOnInternalKeyDown write FOnInternalKeyDown; // Used by AdvToolBar
    property OnInternalClick: TNotifyEvent read FOnInternalClick write FOnInternalClick; // Used by AdvToolBar
    property OnInternalDropDown: TNotifyEvent read FOnInternalDropDown write FOnInternalDropDown; // Used by AdvToolBar
    property InternalIsOnGlassQAT: Boolean read FInternalIsOnGlassQAT write FInternalIsOnGlassQAT; // Used by TAdvQuickAccessToolBar
    property OnGetShortCutHintPos: TOnGetShortCutHintPos read FOnGetShortCutHintPos write FOnGetShortCutHintPos; // Used by AdvToolBar
    property OverlappedText: boolean read FOverlappedText write FOverlappedText;
    property DoAutoSize: boolean read FDoAutoSize write FDoAutoSize;
    property ButtonSizeState: TButtonSizeState read FButtonSizeState write SetButtonSizeState; // Used by AdvToolBar
    property MaxButtonSizeState: TButtonSizeState read FMaxButtonSizeState write SetMaxButtonSizeState default bsLarge;
    property MinButtonSizeState: TButtonSizeState read FMinButtonSizeState write SetMinButtonSizeState default bsGlyph;
    property OnSetButtonSize: TSetButtonSizeEvent read FOnSetButtonSize write FOnSetButtonSize; // Used by AdvToolBar
    function GetButtonSize(BtnSizeState: TButtonSizeState): TSize;
    property KeepDown: boolean read FKeepDown write FKeepDown;
    procedure NotesChanged(Sender: TObject);
    {$IFDEF DELPHI_UNICODE}
    procedure ChangeScale(M, D: Integer); override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CreateWnd; override;
    procedure Click; override;
    procedure DoDropDown;
    function IsDropDown: Boolean; virtual;
    property Appearance: TGlowButtonAppearance read FAppearance write SetAppearance;
    procedure ShowShortCutHint;
    procedure HideShortCutHint;
    /// <summary>Sets the style of the component, make sure to include AdvStyleIF unit</summary>
    procedure SetComponentStyle(AStyle: TTMSStyle); virtual;
    procedure SetColorTones(ATones: TColorTones);
    property WideCaption: widestring read FWideCaption write SetWideCaption;
    {$IFDEF DELPHI2006_LVL}
    class property StaticActionImageIndex: boolean read FStaticActionImageIndex write FStaticActionImageIndex;
    {$ENDIF}
    property StaticButton: Boolean read FStaticButton write FStaticButton default false;
  published
    property Align;
    property Action;
    property Anchors;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property AutoSize: boolean read FAutoSize write SetAutoSizeEx default false;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Cancel: Boolean read FCancel write FCancel default False;
    property Caption;
    property Constraints;
    property Default: boolean read FDefault write SetDefault default False;
    property Font stored IsFontStored;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DisabledPicture: TGDIPPicture read FIDisabledPicture write SetDisabledPicture;
    property DragMode;
    property DragKind;
    property FocusType: TFocusType read FFocusType write FFocusType default ftBorder;
    property HotImages: TCustomImageList read FHotImages write SetHotImages;
    property HotPicture: TGDIPPicture read FIHotPicture write SetHotPicture;
    property ImageListType: TImageListType read FImageListType write SetImageListType default ipActionList;
    property MarginVert: integer read FMarginVert write SetMarginVert default 1;
    property MarginHorz: integer read FMarginHorz write SetMarginHorz default 1;
    property ModalResult: TModalResult read FModalResult write FModalResult default 0;
    property Notes: TStrings read GetNotes write SetNotes;
    property NotesFont: TFont read FNotesFont write SetNotesFont;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property ParentFont default true;
    property Picture: TGDIPPicture read FIPicture write SetPicture;
    property PopupMenu;
    property Position: TButtonPosition read FButtonPosition write SetButtonPosition default bpStandalone;
    property InitRepeatPause: Integer read FInitRepeatPause write FInitRepeatPause default 400;
    property RepeatPause: Integer read FRepeatPause write FRepeatPause default 100;
    property RepeatClick: boolean read FRepeatClick write FRepeatClick default false;
    property Rounded: Boolean read FRounded write SetRounded default true;
    property ShortCutHint: string read FShortCutHintText write FShortCutHintText;
    property ShortCutHintPos: TShortCutHintPos read FShortCutHintPos write FShortCutHintPos default shpTop;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default true;
    property ShowDisabled: Boolean read FShowDisabled write SetShowDisabled default true;
    property Spacing: Integer read FSpacing write SetSpacing default 2;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Trimming: TStringTrimming read FTrimming write SetTrimming default StringTrimmingNone;
    property Version: string read GetVersion write SetVersion stored False;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnExit;
    property OnEnter;

    property OnStartDock;
    property OnStartDrag;

    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnDrawButton: TGlowButtonDrawEvent read FOnDrawButton write FOnDrawButton;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGlowButton = class(TAdvCustomGlowButton)
  private
  protected
  public
    property State;
    property DroppedDown;
  published
    property AllowAllUp;
    property AlwaysShowAccel;
    property Appearance;
    property Down;
    property Enabled;
    property GroupIndex;
    property Layout;
    property Style;
    property MaxButtonSizeState;
    property MinButtonSizeState;
    property DropDownButton;
    property DropDownPosition;
    property DropDownDirection;
    property DropDownSplit;
    property DropDownMenu;
    property OnDropDown;
  end;

  {$IFNDEF TMS_STD}

  //---- DB aware version
  TDBGlowButtonType = (dbCustom, dbFirst, dbPrior, dbNext, dbLast, dbInsert, dbAppend,
                   dbDelete, dbEdit, dbPost, dbCancel, dbRefresh);

  TDBBDisableControl = (drBOF, drEOF, drReadonly, drNotEditing, drEditing, drEmpty, drEvent);
  TDBBDisableControls = set of TDBBDisableControl;

  TBeforeActionEvent = procedure (Sender: TObject; var DoAction: Boolean) of object;
  TAfterActionEvent = procedure (Sender: TObject; var ShowException: Boolean) of object;
  TGetConfirmEvent = procedure (Sender: TObject; var Question: string; var Buttons: TMsgDlgButtons; var HelpCtx: Longint) of object;
  TGetEnabledEvent = procedure (Sender: TObject; var Enabled: Boolean) of object;

  TDBGlowButtonDataLink = class(TDataLink)
  private
    FOnEditingChanged: TNotifyEvent;
    FOnDataSetChanged: TNotifyEvent;
    FOnActiveChanged: TNotifyEvent;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create;
    property OnEditingChanged: TNotifyEvent
      read FOnEditingChanged write FOnEditingChanged;
    property OnDataSetChanged: TNotifyEvent
      read FOnDataSetChanged write FOnDataSetChanged;
    property OnActiveChanged: TNotifyEvent
      read FOnActiveChanged write FOnActiveChanged;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBAdvGlowButton = class(TAdvCustomGlowButton)
  private
    FDataLink: TDBGlowButtonDataLink;
    FAutoDisable: Boolean;
    FDisableControls: TDBBDisableControls;
    FOnAfterAction: TAfterActionEvent;
    FOnBeforeAction: TBeforeActionEvent;
    FDBButtonType: TDBGlowButtonType;
    FOnGetConfirm: TGetConfirmEvent;
    FOnGetEnabled: TGetEnabledEvent;
    FOnEnabledChanged: TNotifyEvent;
    FConfirmAction: Boolean;
    FConfirmActionString: String;
    FInProcUpdateEnabled: Boolean;
    procedure CMEnabledChanged(var Message: TMessage);  message CM_ENABLEDCHANGED;
    procedure OnDataSetEvents(Sender: TObject);

    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetDBButtonType(const Value: TDBGlowButtonType);
    procedure SetConfirmActionString(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Loaded; override;
    procedure CalcDisableReasons;
    procedure DoBeforeAction(var DoAction: Boolean); virtual;
    procedure DoGetQuestion(var Question: string; var Buttons: TMsgDlgButtons; var HelpCtx: Longint); virtual;
    function DoConfirmAction: Boolean; virtual;
    procedure DoAction; virtual;
    procedure UpdateEnabled; virtual;
    procedure LoadGlyph; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Appearance;
    property Layout;
    property Constraints;
    property AutoDisable: Boolean read FAutoDisable write FAutoDisable;
    property ConfirmAction: Boolean read FConfirmAction write FConfirmAction;
    property ConfirmActionString: String read FConfirmActionString write SetConfirmActionString;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DBButtonType: TDBGlowButtonType read FDBButtonType write SetDBButtonType;
    property DisableControl: TDBBDisableControls read FDisableControls write FDisableControls;
    property Enabled;

    property OnBeforeAction: TBeforeActionEvent read FOnBeforeAction write FOnBeforeAction;
    property OnAfterAction: TAfterActionEvent read FOnAfterAction write FOnAfterAction;
    property OnGetConfirm: TGetConfirmEvent read FOnGetConfirm write FOnGetConfirm;
    property OnGetEnabled: TGetEnabledEvent read FOnGetEnabled write FOnGetEnabled;
    property OnEnabledChanged: TNotifyEvent read FOnEnabledChanged write FOnEnabledChanged;
  end;

  {$ENDIF}

  {$EXTERNALSYM _IMAGELISTDRAWPARAMSL}
  _IMAGELISTDRAWPARAMSL = record
    cbSize: DWORD;
    himl: HIMAGELIST;
    i: Integer;
    hdcDst: HDC;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    xBitmap: Integer;        // x offest from the upperleft of bitmap
    yBitmap: Integer;        // y offset from the upperleft of bitmap
    rgbBk: COLORREF;
    rgbFg: COLORREF;
    fStyle: UINT;
    dwRop: DWORD;
    { For IE >= 0x0501 }
    fState: DWORD;
    Frame: DWORD;
    crEffect: COLORREF;
  end;

  {$EXTERNALSYM PIMAGELISTDRAWPARAMSL}
  PImageListDrawParamsL = ^TImageListDrawParamsL;
  {$EXTERNALSYM TIMAGELISTDRAWPARAMSL}
  TImageListDrawParamsL = _IMAGELISTDRAWPARAMSL;


implementation

{$IFNDEF TMS_STD}
uses
  {$IFDEF TMSPACK}
  AdvToolBar,
  {$ENDIF}
  {$IFDEF DELPHI6_LVL}
  VDBConsts
  {$ELSE}
  DBConsts
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  , ActnMan
  {$ENDIF}
  ;
{$ENDIF}

const
  {$EXTERNALSYM ILS_SATURATE}
  ILS_SATURATE            = $00000004;


type
  TButtonDisplay = (bdNone, bdButton, bdDropDown);

//------------------------------------------------------------------------------

function HideAccelFlag(Control: TControl): Cardinal;
begin
  //ask the top level window about its UI state
  while Assigned(Control.Parent) do
  begin
    Control := Control.Parent;
  end;

  if (Control.Perform(WM_QUERYUISTATE, 0, 0) and UISF_HIDEACCEL) = UISF_HIDEACCEL then
  begin
    Result := DT_HIDEPREFIX;
  end
  else
  begin
    Result := 0;
  end;
end;

function GB_GetDPIScale: single;
begin
  {$IFDEF TMSPACK}
  Result := ADVToolBar_GetDPIScale;
  {$ELSE}
  Result := 1.0;
  {$ENDIF}
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

function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := ColorToRGB(Col);
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,Brightness)
  else
    r1 := Round( Min(100,(100 + Brightness))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,Brightness)
  else
    g1 := Round( Min(100,(100 + Brightness))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,Brightness)
  else
    b1 := Round( Min(100,(100 + Brightness))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,BR)
  else
    r1 := Round( Min(100,(100 + BR))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,BG)
  else
    g1 := Round( Min(100,(100 + BG))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,BB)
  else
    b1 := Round( Min(100,(100 + BB))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor >= 100 then
  begin
    Result := Col1;
    Exit;
  end;
  if BlendFactor <= 0 then
  begin
    Result := Col2;
    Exit;
  end;
  if Col1 = clNone then
  begin
    Result := Col2;
    Exit;
  end;
  if Col2 = clNone then
  begin
    Result := Col1;
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

  // in case someone screws up the rounding mode!
  if (r1 > 255) then r1 := 255;
  if (g1 > 255) then g1 := 255;
  if (b1 > 255) then b1 := 255;

  Result := RGB(r1,g1,b1);
end;


//------------------------------------------------------------------------------

procedure DrawOpenRoundRectMiddle(graphics: TGPGraphics; PC: TColor; X,Y,Width,Height,Radius: integer;Hot:boolean);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;

begin
  path := TGPGraphicsPath.Create;

  gppen := tgppen.Create(ColorToARGB(PC),1);
  path.AddLine(X-1, Y + height, X + width, Y + height);
  graphics.DrawPath(gppen, path);
  path.Free;

  path := TGPGraphicsPath.Create;
  path.AddLine(X-1, Y, X + width, Y);
  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;

  path := TGPGraphicsPath.Create;
  gppen := tgppen.Create(ColorToARGB(BrightnessColor(PC,-5)),1);

  if IsWin7 then
    path.AddLine(X + Width , Y + 2, X + width , Y + Height - 2)
  else
    path.AddLine(X + Width, Y, X + width, Y + Height);

  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;

  if hot then
  begin
    path := TGPGraphicsPath.Create;
    gppen := tgppen.Create(ColorToARGB(BrightnessColor(PC,-5)),1);
    path.AddLine(X  , Y, X    , Y + Height);
    graphics.DrawPath(gppen, path);
    gppen.Free;
    path.Free;
  end
  else
  begin
    path := TGPGraphicsPath.Create;
    // 3D color effect
    gppen := tgppen.Create(ColorToARGB(BrightnessColor(clwhite,-10)),1);
    path.AddLine(X, Y + 2, X, Y + Height - 2);
    graphics.DrawPath(gppen, path);
    gppen.Free;
    path.Free;
  end;
end;


//------------------------------------------------------------------------------

procedure DrawOpenRoundRectLeft(graphics: TGPGraphics; PC:TColor; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;
begin
  path := TGPGraphicsPath.Create;
  gppen := tgppen.Create(ColorToARGB(PC),1);
  path.AddLine(X + width , Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.AddLine(X + radius, Y, X + width, Y);
  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;

  path := TGPGraphicsPath.Create;
  gppen := TGPPen.Create(ColorToARGB(BrightnessColor(PC,-5)),1);

  if IsWin7 then
    path.AddLine(X + Width , Y + 2, X + width , Y + Height - 2)
  else
    path.AddLine(X + Width , Y, X + width , Y + Height);

  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;

end;

procedure DrawOpenRoundRectRight(graphics: TGPGraphics; PC: TColor; X,Y,Width,Height,Radius: integer;Hot: boolean);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;
begin
  path := TGPGraphicsPath.Create;
  gppen := tgppen.Create(ColorToARGB(PC),1);
  path.AddLine(X, Y, X + width - (radius *2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width , Y + height, X, Y + height);
  graphics.DrawPath(gppen, path);
  gppen.Free;

  path.Free;


  if hot then
  begin
    path := TGPGraphicsPath.Create;
    gppen := tgppen.Create(ColorToARGB(BrightnessColor(PC,-5)),1);
    path.AddLine(X  , Y, X    , Y + Height);
    graphics.DrawPath(gppen, path);
    gppen.Free;
    path.Free;
  end
  else
  begin
    path := TGPGraphicsPath.Create;
    // 3D color effect
    gppen := tgppen.Create(ColorToARGB(BrightnessColor(clwhite,-10)),1);
    path.AddLine(X, Y + 2, X, Y + Height - 2);
    graphics.DrawPath(gppen, path);
    gppen.Free;
    path.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawDottedRoundRect(graphics: TGPGraphics; PC: TColor; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;
begin
  path := TGPGraphicsPath.Create;
  gppen := tgppen.Create(ColorToARGB(PC),1);
  gppen.SetDashStyle(DashStyleDot);
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;
end;


//------------------------------------------------------------------------------

procedure DrawRoundRect(graphics: TGPGraphics; PC: TColor; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;
  r: integer;
begin
  gppen := tgppen.Create(ColorToARGB(PC),1);

  if radius = 0 then
  begin
    graphics.DrawRectangle(gppen, X, Y, Width, Height);
  end
  else
  begin
    r := radius * 2;
    path := TGPGraphicsPath.Create;
    //gppen := tgppen.Create(ColorToARGB(PC),1);
    path.AddLine(X + radius, Y, X + width - r, Y);
    path.AddArc(X + width - r, Y, r, r, 270, 90);
    path.AddLine(X + width, Y + radius, X + width, Y + height - r);
    path.AddArc(X + width - r, Y + height - r, r, r,0,90);
    path.AddLine(X + width - r, Y + height, X + radius, Y + height);
    path.AddArc(X, Y + height - r, r, r, 90, 90);
    path.AddLine(X, Y + height - r, X, Y + radius);
    path.AddArc(X, Y, r, r, 180, 90);
    path.CloseFigure;
    graphics.DrawPath(gppen, path);
    path.Free;
  end;
  gppen.Free;
end;

procedure DrawArrow(Canvas: TCanvas; ArP: TPoint; ArClr, ArShad: TColor; Down:boolean);
var
  i: integer;
  FArrSize: integer;
begin
  FArrSize := round(GB_GetDPIScale * 3);
  ArP.X := ArP.X - (FArrSize - 3) div 2;
  ArP.Y := ArP.Y - (FArrSize - 3) div 2;
  if Down then
  begin
    Canvas.Pen.Color := ArClr;
    for I := 0 to FArrSize - 1 do
    begin
      Canvas.MoveTo(ArP.X + I, ArP.Y + I);
      Canvas.LineTo(ArP.X + (FArrSize * 2) - I - 1, ArP.Y + I);
    end;
  end
  else
  begin
    Canvas.Pen.Color := ArClr;
    for I := 0 to FArrSize - 1 do
    begin
      Canvas.MoveTo(ArP.X + I, ArP.Y + I);
      Canvas.LineTo(ArP.X + I, ArP.Y + (FArrSize * 2) - I - 1);
    end;
  end;

  (*
  if Down then
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
    Canvas.Pixels[ArP.X, ArP.Y + 1] := ArShad;
    Canvas.Pixels[ArP.X + 4, ArP.Y + 1] := ArShad;
    Canvas.Pixels[ArP.X + 2, ArP.Y + 3] := ArShad;
    Canvas.Pixels[ArP.X + 1, ArP.Y + 2] := ArShad;
    Canvas.Pixels[ArP.X + 3, ArP.Y + 2] := ArShad;
  end
  else
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X, ArP.Y + 5);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 1, ArP.Y + 4);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
    Canvas.Pixels[ArP.X + 2, ArP.Y + 1] := ArShad;
    Canvas.Pixels[ArP.X + 1, ArP.Y + 4] := ArShad;
    Canvas.Pixels[ArP.X + 2, ArP.Y + 1] := ArShad;
    Canvas.Pixels[ArP.X + 3, ArP.Y + 2] := ArShad;
    Canvas.Pixels[ArP.X + 3, ArP.Y + 2] := ArShad;
  end;
  *)
end;

procedure DrawGDIPArrow(Canvas: TCanvas; ArP: TPoint; Clr: TColor);
var
  graphics: TGPGraphics;
  Path: TGPGraphicsPath;
  apen: TGPPen;
  rc, gc, bc: Byte;
  FArrSize: integer;
  i: integer;
begin
  FArrSize := Round(GB_GetDPIScale * 3);
  ArP.X := ArP.X - FArrSize + 3;
  ArP.Y := ArP.Y - FArrSize + 3;

  graphics := TGPGraphics.Create(Canvas.Handle);

  Path := TGPGraphicsPath.Create();

  for I := 0 to FArrSize - 1 do
    Path.AddLine(ArP.X + I, ArP.Y + I , Arp.X + FArrSize * 2 - I - 1, ArP.Y + I);

//  Path.AddLine(ArP.X, ArP.Y, Arp.X + 5, ArP.Y);
//  Path.AddLine(ArP.X + 1, ArP.Y + 1, Arp.X + 4, ArP.Y + 1);
//  Path.AddLine(ArP.X + 2, ArP.Y + 2, Arp.X + 3, ArP.Y + 2);

  Path.CloseFigure;

  rc := GetRValue(Clr);
  gc := GetGValue(Clr);
  bc := GetBValue(Clr);

  apen := TGPPen.Create(MakeColor(200, rc, gc, bc), 1.0);
  graphics.DrawPath(apen, Path);
  apen.Free;
  Path.Free;

  graphics.Free;
end;

procedure DrawButtonBackground(Canvas: TCanvas; Graphics: TGPGraphics; R: TRect; CF,CT: TColor; Gradient: TGDIPGradient; Upper: boolean);
var
  path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  linGrBrush: TGPLinearGradientBrush;
  solGrBrush: TGPSolidBrush;

  w,h,w2,h2: Integer;
  colors : array[0..0] of TGPColor;
  count: Integer;

begin
  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;
  w2 := w div 2;

  (*
  // draw background
  if Upper then
    Canvas.Brush.Color := CF
  else
    Canvas.Brush.Color := CT;
  Canvas.FillRect(rect(r.Left , r.Top, r.Right , r.Bottom));
  *)

  if CT = clNone then
    CT := CF
  else
    if CF = clNone then
      CF := CT;

  if Upper then
    solGrBrush := TGPSolidBrush.Create(ColorToARGB(CF))
  else
    solGrBrush := TGPSolidBrush.Create(ColorToARGB(CT));


  Graphics.FillRectangle(solGrBrush, MakeRect(r.Left, r.Top, w , h));

  solGrBrush.Free;

  // Create a path that consists of a single ellipse.
  path := TGPGraphicsPath.Create;

  if Upper then        // take borders in account
    path.AddEllipse(r.Left, r.Top - h2 + 2, w , h)
  else
    path.AddEllipse(r.Left, r.Top, w , h);

  pthGrBrush := nil;
  linGrBrush := nil;

  case Gradient of
  ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
  ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CF),ColorToARGB(CT), LinearGradientModeVertical);
  ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CF),ColorToARGB(CT), LinearGradientModeForwardDiagonal);
  ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CF),ColorToARGB(CT), LinearGradientModeBackwardDiagonal);
  end;

  if Gradient = ggRadial then
  begin
    if Upper then
      pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Top))
    else
      pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

    // Set the color at the center point to blue.
    if Upper then
    begin
      pthGrBrush.SetCenterColor(ColorToARGB(CT));
      colors[0] := ColorToARGB(CF);
    end
    else
    begin
      pthGrBrush.SetCenterColor(ColorToARGB(CF));
      colors[0] := ColorToARGB(CT);
    end;

    count := 1;
    pthGrBrush.SetSurroundColors(@colors, count);
    graphics.FillRectangle(pthGrBrush, r.Left, r.Top, w, h);
    pthGrBrush.Free;
  end
  else
  begin
    graphics.FillRectangle(linGrBrush, r.Left, r.Top, w, h);
    linGrBrush.Free;
  end;

  path.Free;
end;

//------------------------------------------------------------------------------

procedure DrawStretchPicture(graphics : TGPGraphics; Canvas: TCanvas; R: TRect; Pic: TGDIPPicture);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  bmp: TBitmap;
  hr: HResult;
begin
  ms := TMemoryStream.Create;
  Pic.SaveToStream(ms);

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
    pstm.Write(ms.Memory, ms.Size, @pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);
      if (Img.GetFormat = ifBMP) then
      begin // use this alternative for easy bitmap auto transparent drawing
        bmp := TBitmap.Create;
        try
          ms.Position := 0;
          bmp.LoadFromStream(ms);
          bmp.TransparentMode := tmAuto;
          bmp.Transparent := true;
          Canvas.StretchDraw(R, bmp);
        finally
          bmp.Free;
        end;
      end
      else
      begin
        graphics.DrawImageRect(Img, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
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

procedure DrawGDIPImageFromImageList(graphics: TGPGraphics; P: TPoint; Images: TCustomImageList; ImageIndex: Integer; Enable: Boolean);
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
  hr: HResult;
  Options: TImageListDrawParamsL;

begin
  if not Assigned(Images) or (ImageIndex < 0) or not Assigned(graphics) then
    Exit;

  bmp := TBitmap.Create;
  try
    bmp.Width := Images.Width;
    bmp.Height := Images.Height;
    bmp.PixelFormat := pf32bit;
    bmp.HandleType := bmDIB;
    {$IFDEF DELPHI_UNICODE}
    bmp.AlphaFormat := afDefined;
    {$ENDIF}

    if Enable then
      Images.Draw(bmp.Canvas, 0, 0, ImageIndex, Enable)
    else
    begin
      ZeroMemory(@Options, SizeOf(Options));
      Options.cbSize := SizeOf(Options);
      Options.himl := Images.Handle;
      Options.i := ImageIndex;
      Options.hdcDst := bmp.Canvas.Handle;
      Options.x := 0;
      Options.y := 0;
      Options.fState := ILS_SATURATE;
      ImageList_DrawIndirect(@Options);
    end;

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
        0, 0,               // upper-left corner of source rectangle
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
    GlobalFree(hGlobal);

  ms.Free;
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(graphics: TGPGraphics; P: TPoint; Pic: TGDIPPicture; Mw,Mh: integer);
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
  hr: HResult;

  function SmartMin(a,b: integer): integer;
  begin
    if (a > 0) then
      Result := Min(a,b)
    else
      Result := b;
  end;

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
    pstm.Write(ms.Memory, ms.Size, @pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Img.GetFormat = ifBMP then
      begin
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
          SmartMin(mw,Img.GetWidth),       // width of source rectangle
          SmartMin(mh,Img.GetHeight),      // height of source rectangle
          UnitPixel,
          ImageAttributes);

        ImageAttributes.Free;
      end
      else
        graphics.DrawImageRect(Img, p.X, p.Y, SmartMin(mw,Img.GetWidth), SmartMin(mh,Img.GetHeight));

      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);
  ms.Free;
end;


procedure DrawDisabledImage(DC: HDC; ImageList: TCustomImageList; Index, X, Y: Integer);
var
  Options: TImageListDrawParamsL;
begin
  ZeroMemory(@Options, SizeOf(Options));
  Options.cbSize := SizeOf(Options);
  Options.himl := ImageList.Handle;
  Options.i := Index;
  Options.hdcDst := DC;
  Options.x := X;
  Options.y := Y;
  Options.fState := ILS_SATURATE;
  ImageList_DrawIndirect(@Options);
end;

//------------------------------------------------------------------------------

function DrawVistaButton(Owner: TControl; Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC, PCF: TColor;
   GradientU, GradientB: TGDIPGradient; Caption:string; WideCaption: widestring; DrawCaption: Boolean; AFont: TFont;
   Images, DisabledImages: TCustomImageList; ImageIndex: Integer; EnabledImage: Boolean; Layout: TButtonLayout;
   DropDownButton: Boolean; DrawDwLine: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
   Picture: TGDIPPicture; Icon: TIcon; ForcePicSize: TSize; AntiAlias: TAntiAlias; DrawPic: Boolean; Glyph: TBitmap; ButtonDisplay: TButtonDisplay; Transparent, Hot: boolean;
   ButtonPosition: TButtonPosition; DropDownSplit, DrawBorder, OverlapText, WordWrap, AutoSize, Rounded, DropDir: Boolean; Spacing: integer;
   Trimming: TStringTrimming; Notes: TStringList; NotesFont: TFont;Checked, Down, TranspHover, GDIPArrow: boolean; MargH,MargV: integer; NoDefBorder: boolean; AlwaysAccel: boolean): TSize;
var
  graphics : TGPGraphics;
  w,h,h2,h2d: Integer;
  fontFamily,nfontFamily: TGPFontFamily;
  font,nfont: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush,nsolidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs,nfs: integer;
  sizerect: TGPRectF;
  noterect: TGPRectF;
  ImgX, ImgY, ImgW, ImgH: Integer;
  BtnR, DwR: TRect;
  BR1,BR2: TRect;
  DR1,DR2: TRect;
  AP: TPoint;
  szRect: TRect;
  tm: TTextMetric;
  ttf: boolean;
  Radius: integer;
  uformat,wwformat: Cardinal;
  tdrect: TRect;
  th, px, py: integer;
  notesrect: TRect;
  i: integer;
  ntext,ncap: string;
  FDropDownSectWidth: integer;
  slineh: single;
  mline: boolean;
begin
  BtnR := R;

  mline := false;

  if Rounded then
    Radius := 3
  else
    Radius := 0;

  FDropDownSectWidth := Round(DropDownSectWidth * GB_GetDPIScale);

  if DropDownPos = dpRight then
  begin
    DwR := Rect(BtnR.Right - FDropDownSectWidth, BtnR.Top, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Right := DwR.Left;
  end
  else // DropDownPos = doBottom
  begin
    DwR := Rect(BtnR.Left, BtnR.Bottom - FDropDownSectWidth, BtnR.Right, BtnR.Bottom);
    if DropDownButton and DropDownSplit then
      BtnR.Bottom := DwR.Top;
  end;

  if (Notes.Text <> '') then
    Layout := blGlyphLeftAdjusted;

  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;

  if (CTB = clNone) and (CFB = clNone) then
    CTB := CFU;

  if CFB = clNone then
    CFB := CFU;

  // Create GDI+ canvas
  graphics := TGPGraphics.Create(Canvas.Handle);

  if (not Transparent) or (Down) or (Checked) then
  begin
    if DropDownButton and (DrawDwLine) and DropDownSplit then
    begin
      if DropDownPos = dpRight then
      begin
//        DR1 := Rect(r.Right - 12, r.Top + h2 - 1, r.Right, r.Bottom);
//        DR2 := Rect(r.Right - 12, r.Top, r.Right, r.Bottom - h2);
//        BR1 := Rect(r.Left, r.Top + h2 - 1, r.Right - 12, r.Bottom);
//        BR2 := Rect(r.Left, r.Top, r.Right - 12, r.Bottom - h2);

        DR1 := Rect(r.Right - FDropDownSectWidth, r.Top + h2 - 1, r.Right, r.Bottom);
        DR2 := Rect(r.Right - FDropDownSectWidth, r.Top, r.Right, r.Bottom - h2);
        BR1 := Rect(r.Left, r.Top + h2 - 1, r.Right - FDropDownSectWidth, r.Bottom);
        BR2 := Rect(r.Left, r.Top, r.Right - FDropDownSectWidth, r.Bottom - h2);
      end
      else
      begin
        DR1 := Rect(r.Left, r.Bottom - 6, r.Right, r.Bottom);

//        DR2 := Rect(r.Left, r.Bottom - 12, r.Right, r.Bottom);
//        h2d := (r.Bottom - r.Top - 12) div 2;
//        BR1 := Rect(r.Left, r.Top + h2d - 1, r.Right, r.Bottom - 12);
//        BR2 := Rect(r.Left, r.Top, r.Right, r.Bottom - 12 - h2d);

        DR2 := Rect(r.Left, r.Bottom - FDropDownSectWidth, r.Right, r.Bottom);
        h2d := (r.Bottom - r.Top - FDropDownSectWidth) div 2;
        BR1 := Rect(r.Left, r.Top + h2d - 1, r.Right, r.Bottom - FDropDownSectWidth);
        BR2 := Rect(r.Left, r.Top, r.Right, r.Bottom - FDropDownSectWidth - h2d);
      end;

      if (ButtonDisplay = bdDropDown) then
      begin
        DrawButtonBackground(Canvas, Graphics, BR1, CTB, CFB, GradientB, False);
        DrawButtonBackground(Canvas, Graphics, BR2, CFU, CTU, GradientU, True);

        if not TranspHover then
        begin
          DrawButtonBackground(Canvas, Graphics, DR2, BrightnessColor(CFU,-10), BrightnessColor(CTU,-10), GradientU, True);
          if (DropDownPos = dpRight) then
            DrawButtonBackground(Canvas, Graphics, DR1, BrightnessColor(CTB,-10), BrightnessColor(CFB,-10), GradientB, False);
        end;
      end
      else
      begin
        if not TranspHover then
        begin
          DrawButtonBackground(Canvas, Graphics, BR1, BrightnessColor(CTB,-10), BrightnessColor(CFB,-10), GradientB, False);
          DrawButtonBackground(Canvas, Graphics, BR2, BrightnessColor(CFU,-10), BrightnessColor(CTU,-10), GradientU, True);
        end;

        DrawButtonBackground(Canvas, Graphics, DR2, CFU, CTU, ggRadial, True);
        if DropDownPos = dpRight then
          DrawButtonBackground(Canvas, Graphics, DR1, CTB, CFB, GradientB, False);
      end;
    end
    else
    begin
      DrawButtonBackground(Canvas, Graphics, Rect(r.Left, r.Top + h2 - 1, r.Right, r.Bottom), CTB, CFB, GradientB, False);
      DrawButtonBackground(Canvas, Graphics, Rect(r.Left, r.Top, r.Right, r.Bottom - h2), CFU, CTU, GradientU, True);
    end;
  end;

  graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  DrawBorder := DrawBorder and (not NoDefBorder or (Hot or Down));

  if ((not Transparent) or (Down) or (Checked)) and DrawBorder then
  begin
    case ButtonPosition of
    bpStandalone: DrawRoundRect(Graphics, PC, r.Left, r.Top, r.Right - 1, r.Bottom - 1, Radius);
    bpLeft: DrawOpenRoundRectLeft(Graphics, PC, r.Left, r.Top, r.Right - 1, r.Bottom - 1, Radius);
    bpRight: DrawOpenRoundRectRight(Graphics, PC, r.Left, r.Top, r.Right - 1, r.Bottom - 1, Radius, Hot or Checked);
    bpMiddle: DrawOpenRoundRectMiddle(Graphics, PC, r.Left, r.Top, r.Right - 1, r.Bottom - 1, Radius, Hot or Checked);
    end;
  end;

  if Focus then // Draw focus line
  begin
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics,PCF ,r.Left + 1,r.Top + 1, r.Right - 3, r.Bottom - 3, Radius);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawDottedRoundRect(graphics, clGray,r.Left + 2,r.Top + 2, r.Right - 5, r.Bottom - 5, Radius);
  end;

  if (Layout = blGlyphLeftAdjusted) and not AutoSize then
    r.Left := r.Left + MargH;

  ImgX := 0;
  ImgY := 0;
  ImgH := 0;
  ImgW := 0;

  fontFamily := TGPFontFamily.Create(AFont.Name);

  if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    fontFamily.Free;
    fontFamily := TGPFontFamily.Create('Arial');
  end;

  nfontFamily := TGPFontFamily.Create(NotesFont.Name);

  if (nfontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    nfontFamily.Free;
    nfontFamily := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in AFont.Style) then
    fs := fs + 1;
  if (fsItalic in AFont.Style) then
    fs := fs + 2;
  if (fsUnderline in AFont.Style) then
    fs := fs + 4;

  nfs := 0;
  if (fsBold in NotesFont.Style) then
    nfs := nfs + 1;
  if (fsItalic in NotesFont.Style) then
    nfs := nfs + 2;
  if (fsUnderline in NotesFont.Style) then
    nfs := nfs + 4;

  if Assigned(Glyph) and not Glyph.Empty and (Glyph.Width > 1) and (Glyph.Height > 1) then
  begin
    ImgW := Glyph.Width;
    ImgH := Glyph.Height;

    if (ForcePicSize.CX > 0) and (ForcePicSize.CY > 0) then
    begin
      ImgW := ForcePicSize.CX;
      ImgH := ForcePicSize.CY;
    end;
  end
  else if Assigned(Picture) and not Picture.Empty then
  begin
    Picture.GetImageSizes;
    ImgW := Picture.Width;
    ImgH := Picture.Height;
    if (ForcePicSize.CX > 0) and (ForcePicSize.CY > 0) then
    begin
      ImgW := ForcePicSize.CX;
      ImgH := ForcePicSize.CY;
    end;
  end
  else
  begin
    if (ImageIndex > -1) and Assigned(Images) then
    begin
      ImgW := Images.Width;
      ImgH := Images.Height;
    {end
    else if Assigned(ToolImage) and not (ToolImage.Empty) and (ToolImage.Width > 1) then
    begin
      ImgW := ToolImage.Width;
      ImgH := ToolImage.Height; }
    end;
  end;

  if DrawCaption and ((Caption <> '') or (WideCaption <> '')) then
  begin
    if (ImgW > 0) and (Layout in [blGlyphLeft, blGlyphLeftAdjusted, blGlyphRight, blGlyphRightAdjusted])then
      ImgW := ImgW + Spacing;

    if (ImgH > 0) and (Layout in [blGlyphTop, blGlyphTopAdjusted])then
      ImgH := ImgH + Spacing;
  end;

  Result.cx := ImgW;
  Result.cy := ImgH;

  if (Caption <> '') or (WideCaption <> '') or AutoSize then
  begin
    if pos('\n',Caption) > 0 then
    begin
      if (ForcePicSize.cx > 0) and (ForcePicSize.cy > 0) then
      begin
        Caption := StringReplace(Caption, '\n', ' ', [rfReplaceAll, rfIgnoreCase])
      end
      else
      begin
        Caption := StringReplace(Caption, '\n', #10#13, [rfReplaceAll, rfIgnoreCase]);
      end;
    end;

    ncap := Caption;

    if AutoSize and (ncap = '') and (WideCaption = '') then
      ncap := '!';

    Canvas.Font.Name := AFont.Name;

    //ttf := false;

    GetTextMetrics(Canvas.Handle, tm);

    ttf := tm.tmPitchAndFamily AND TMPF_TRUETYPE = TMPF_TRUETYPE;

    //if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
    //begin
    //  if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    //  begin
    //    ttf := true;
    //  end
    //end;

    if Screen.Fonts.IndexOf(AFont.Name) = -1 then
      ttf := false;

    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    w := BtnR.Right - BtnR.Left;
    h := BtnR.Bottom - BtnR.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    if AutoSize then
    begin
      {$IFDEF DELPHI_UNICODE}
      if Owner.Constraints.MaxWidth > 0 then
        x2 := Owner.Constraints.MaxWidth
      else
      {$ENDIF}
        x2 := 4096;

      {$IFDEF DELPHI_UNICODE}
      if Owner.Constraints.MaxHeight > 0 then
        y2 := Owner.Constraints.MaxHeight
      else
      {$ENDIF}
        y2 := 4096;
    end;

    rectf := MakeRect(x1,y1,x2,y2);

    if WordWrap then
      stringFormat := TGPStringFormat.Create(0)
    else
      stringFormat := TGPStringFormat.Create(GDIP_NOWRAP);

    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    // Center-justify each line of text.
    case Layout of
      blGlyphLeftAdjusted: stringFormat.SetAlignment(StringAlignmentNear);
      blGlyphRightAdjusted: stringFormat.SetAlignment(StringAlignmentFar);
      else
        stringFormat.SetAlignment(StringAlignmentCenter);
    end;

    // Center the block of text (top to bottom) in the rectangle.
    case Layout of
      blGlyphTopAdjusted: stringFormat.SetLineAlignment(StringAlignmentNear);
      blGlyphBottomAdjusted: stringFormat.SetLineAlignment(StringAlignmentFar);
      else
        stringFormat.SetLineAlignment(StringAlignmentCenter);
    end;

    if (HideAccelFlag(Owner) = DT_HIDEPREFIX) and not AlwaysAccel then
      stringFormat.SetHotkeyPrefix(HotkeyPrefixHide)
    else
      stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

    stringFormat.SetTrimming(Trimming);

    case AntiAlias of
    aaClearType:
      begin
        if GDIPArrow then
          graphics.SetTextRenderingHint(TextRenderingHintAntiAlias)
        else
          graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
      end;
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    if (AntiAlias = aaNone) or not ttf then
    begin
      Canvas.Font.Assign(AFont);
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;

      uformat := DT_CALCRECT or DT_LEFT or HideAccelFlag(Owner);

      if AlwaysAccel then
        uformat := uformat and not DT_HIDEPREFIX;

      if WordWrap then
      begin
        szRect.Right := szRect.Left + round(rectf.Width);
        uformat := uformat + DT_WORDBREAK
      end
      else
        uformat := uformat + DT_SINGLELINE;

      if (ncap <> '') then
        szRect.Bottom := DrawText(Canvas.Handle,PChar(ncap),Length(ncap), szrect, uformat)
      else
        szRect.Bottom := DrawTextW(Canvas.Handle,PWideChar(WideCaption),Length(WideCaption), szrect, uformat);

      //ydropd :=  (round(rectf.Height) + szRect.Bottom) div 2;

      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;

      notesRect := Rect(0,0,0,0);

      if (Notes.Text <> '') then
      begin
        Canvas.Font.Assign(NotesFont);
        notesRect.Left := round(rectf.X);
        notesRect.Top := round(rectf.Y);
        notesRect.Right := notesRect.Left + round(rectf.Width) - ImgW;

        ntext := '';
        if Notes.Count > 0 then
        begin
          {$IFDEF DELPHI2007_LVL}
          for i := 0 to Notes.Count - 2 do
            ntext := ntext + Notes[i] + Notes.LineBreak;
          {$ENDIF}
          {$IFNDEF DELPHI2007_LVL}
          for i := 0 to Notes.Count - 2 do
            ntext := ntext + Notes[i] + #13#10;
          {$ENDIF}
          ntext := ntext + Notes[Notes.Count - 1];
        end;

        notesrect.Bottom := $FFFF;
        if WordWrap then
          notesRect.Bottom := DrawText(Canvas.Handle,PChar(nText),Length(nText), notesRect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK)
        else
          notesRect.Bottom := DrawText(Canvas.Handle,PChar(nText),Length(nText), notesRect, DT_CALCRECT or DT_LEFT);
        noteRect.Width := notesRect.Right - notesRect.Left;
        noteRect.Height := notesRect.Bottom;
      end;

      case Layout of
        blGlyphLeft:
        begin
          sizeRect.X := (w - (szRect.Right - szRect.Left) - ImgW) div 2;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + Round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + Round(sizerect.Height) + Round(noteRect.Height));
        end;
        blGlyphLeftAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + Max(Round(sizerect.Width), Round(noteRect.Width));
          Result.cy := Max(ImgH + Spacing, Spacing + Round(sizerect.Height) + Round(noteRect.Height));
        end;
        blGlyphTop:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := (h - (szRect.Bottom - szRect.Top) - ImgH - 2) div 2;
          Result.cx := Max(ImgW + Spacing, Spacing + Round(sizerect.Width));
          Result.cy := ImgH + Spacing + Round(sizerect.Height);
        end;
        blGlyphTopAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
        blGlyphRight:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphRightAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphBottom:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
        blGlyphBottomAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
      end;
      //Result.cx := ImgW + Spacing + round(sizerect.Width);
      //Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
    end
    else
    begin
      graphics.MeasureString('gh', 2, font, rectf, stringFormat, sizeRect);
      slineh := sizeRect.Height;

      if ncap <> '' then
        graphics.MeasureString(ncap, Length(ncap), font, rectf, stringFormat, sizeRect)
      else
        graphics.MeasureString(WideCaption, Length(WideCaption), font, rectf, stringFormat, sizeRect);

      mline := sizeRect.Height > 1.5 * slineh;

      //ydropd := Round(sizerect.y + sizerect.height);

      noteRect := MakeRect(0,0,0,0);

      if ImgW > 0 then
        rectf.Width := rectf.Width - ImgW - Spacing;

      if Notes.Text <> '' then
      begin
        nfont := TGPFont.Create(nfontFamily, NotesFont.Size , nfs, UnitPoint);
        graphics.MeasureString(Notes.Text, Length(Notes.Text), nfont, rectf, stringFormat, noteRect);
        nfont.Free;
      end;

      case Layout of
        blGlyphLeft, blGlyphLeftAdjusted, blGlyphRight, blGlyphRightAdjusted:
        begin
          Result.cx := ImgW + Spacing + Max(Round(sizerect.Width), Round(noteRect.Width));
          Result.cy := Max(ImgH + Spacing, Spacing + Round(sizerect.Height) + Round(noteRect.Height));
        end;
        blGlyphTop, blGlyphTopAdjusted, blGlyphBottom, blGlyphBottomAdjusted:
        begin
          Result.cx := Max(ImgW + Spacing, Spacing + Round(sizerect.Width));
          Result.cy := ImgH + {Spacing +} Round(sizerect.Height);
        end;
      end;
    end;

    if not AutoSize then
    begin
      if not WordWrap then
      begin
        x2 := w;
        y2 := h;
        rectf := MakeRect(x1,y1,x2,y2);
      end;

//      if (ImgW > 0) then
      begin
        case Layout of
          blGlyphLeft:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              //x1 := sizeRect.X + ImgW;
              x1 := r.Left + 2 + ImgW;
              x2 := w - 2 - ImgW;
              ImgX := round(sizeRect.X);
            end
            else
            begin
              x1 := r.Left + 2 + ImgW;
              x2 := w - 2 - ImgW;
              ImgX := round(sizerect.X - ImgW div 2);
            end;
            if ImgX < 2 then
              ImgX := 2;
            ImgY := r.Top + Max(0, (h - ImgH) div 2);
          end;
          blGlyphLeftAdjusted:
          begin
            x1 := r.Left + 2 + ImgW;
            x2 := w - 2 - ImgW;

            ImgX := round(sizerect.X - ImgW div 2);
            if ImgX < 2 then
              ImgX := 2;
            ImgY := r.Top + Max(0, (h - ImgH) div 2);
          end;
          blGlyphTop:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              y1 := r.Top + ImgH;
 //             y1 := sizeRect.Y + ImgH;
              y2 := h - 2 - ImgH;

              ImgX := r.Left + Max(0, (w - ImgW) div 2);
//              ImgY := round(sizeRect.Y);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(y1) - ImgH + ImgY - 4;
            end
            else
            begin
              y1 := r.Top + ImgH;
              y2 := h - 2 - ImgH;
              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(y1) - ImgH + ImgY;
            end;
            if ImgY < 2 then ImgY := 2;
          end;
          blGlyphTopAdjusted:
          begin
            y1 := r.Top{ + 2} + ImgH;
            y2 := h - 2 - ImgH;

            ImgX := r.Left + Max(0, (w - ImgW) div 2);

            if Layout = blGlyphTopAdjusted then
              ImgY := 0 //force to top margin
            else
              ImgY := round(y2 - sizerect.Height);

            ImgY := Max(0, ImgY div 2);
            ImgY := round(y1) - ImgH + ImgY; //round(sizerect.Height) - ImgY - 4;
            if ImgY < 2 then ImgY := 2;
          end;
          blGlyphRight, blGlyphRightAdjusted:
          begin
            x1 := 2;
            x2 := w - 4 - ImgW;
            if Layout = blGlyphRightAdjusted then
              ImgX := w - ImgW - 2
            else
            begin
              ImgX := round(X2 - sizerect.width);
              ImgX := Max(0, ImgX div 2);
              ImgX := ImgX + round(sizerect.width) + 4;
              if ImgX > (w - ImgW) then
                ImgX := w - ImgW - 2;
            end;
            ImgY := r.Top + Max(0, (h - ImgH) div 2);
            ImgX := ImgX + spacing;
          end;
          blGlyphBottom:
          begin
            y1 := 2;
            y2 := h - 2 - ImgH;
            if (AntiAlias = aaNone) or not ttf then
            begin
              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(sizerect.Height + 5) + ImgY;
              if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
            end
            else
            begin
              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(sizerect.Height + 2) + ImgY;
              if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
            end;
          end;
          blGlyphBottomAdjusted:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              y1 := 2;
              y2 := h - 4 - ImgH;
              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := (h - ImgH - 2);
            end
            else
            begin
              y1 := 2;
              y2 := h - 2 - ImgH;

              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              if Layout = blGlyphBottomAdjusted then
                ImgY := h; //force to bottom margin

              ImgY := Max(0, ImgY div 2);
              ImgY := round(sizerect.Height + 2) + ImgY;
              if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
            end;
          end;
        end;
      end;

      if OverlapText then
        rectf := MakeRect(r.Left, r.Top, r.Right, r.Bottom)
      else
        rectf := MakeRect(x1, y1, x2, y2);

      if DrawPic and OverlapText then
      begin
        if Assigned(Glyph) and not Glyph.Empty and (Glyph.Width > 1) and (Glyph.Height > 1) then
          Canvas.Draw(ImgX, ImgY, Glyph);
      end;

      if DrawCaption then
      begin
        if (AntiAlias = aaNone) or not ttf then
        begin
          szRect.Left := round(rectf.X);
          szRect.Top := round(rectf.Y);
          szRect.Right := szRect.Left + round(rectf.Width);
          szRect.Bottom := szRect.Top + round(rectf.Height);

          Canvas.Brush.Style := bsClear;
          if WordWrap then
            wwformat := DT_WORDBREAK
          else
            wwformat := DT_SINGLELINE;

          uformat := DT_VCENTER or wwformat;

          case Layout of
            blGlyphLeft:
            begin
              uformat := DT_VCENTER or wwformat or DT_CENTER;
              szrect.Left := szrect.Left;
            end;

            blGlyphLeftAdjusted:
            begin
              uformat := DT_VCENTER or wwformat or DT_LEFT;
              szrect.Left := szrect.Left + 2;

              if Notes.Text <> '' then
              begin
                uformat := uformat AND NOT DT_VCENTER;
                szrect.Top := ((szRect.Bottom - szRect.Top) - round(sizeRect.Height) - round(noteRect.Height)) div 2;
              end;
            end;

            blGlyphTop:
            begin
              uformat := DT_TOP or wwformat or DT_CENTER;
            end;
            blGlyphTopAdjusted: uformat := DT_TOP or wwformat or DT_CENTER;
            blGlyphRight: uformat := DT_VCENTER or wwformat or DT_CENTER;
            blGlyphRightAdjusted: uformat := DT_VCENTER or wwformat or DT_RIGHT;
            blGlyphBottom: uformat := DT_VCENTER or wwformat or DT_CENTER;
            blGlyphBottomAdjusted: uformat := DT_BOTTOM or wwformat or DT_CENTER;
          end;

          tdrect := szrect;

          Canvas.Font.Assign(AFont);

          if not Enabled then
            Canvas.Font.Color := clGray;

          if WordWrap then
          begin
            if Caption <> '' then
              th := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, uformat or DT_CALCRECT)
            else
              th := DrawTextW(Canvas.Handle,PWideChar(WideCaption),Length(WideCaption), szrect, uformat or DT_CALCRECT);

            case Layout of
            blGlyphTopAdjusted:
              begin
                // do nothing
              end;
            blGlyphTop:
              begin
                tdrect.Top := ImgY + ImgH;
                //tdrect.Top := tdrect.Top + (tdrect.Bottom - tdrect.Top - th) div 2;
              end;
            blGlyphBottomAdjusted:
              begin
                tdrect.Top := tdrect.Bottom - th;
              end
            else
              begin
                tdrect.Top := (tdrect.Bottom - tdrect.Top - th) div 2;
              end;
            end;
          end;

          uformat := uformat or HideAccelFlag(Owner);

          if AlwaysAccel then
            uformat := uformat and not DT_HIDEPREFIX;

          if Caption <> '' then
            DrawText(Canvas.Handle, PChar(Caption), Length(Caption), tdrect, uformat)
          else
            DrawTextW(Canvas.Handle, PWideChar(WideCaption), Length(WideCaption), tdrect, uformat);

          if (Notes.Text <> '') then
          begin
            tdRect.Top := tdRect.Top + round(sizeRect.Height);
            tdRect.Bottom := tdRect.Top + round(noteRect.Height);

            Canvas.Font.Assign(NotesFont);
            DrawText(Canvas.Handle,PChar(Notes.Text),Length(Notes.Text), tdrect, uformat);
          end;
        end
        else
        begin
          if (Notes.Text <> '') then
          begin
            stringFormat.SetLineAlignment(StringAlignmentNear);
            rectf.Y := rectf.Y + ((rectf.Height) - round(sizeRect.Height) - round(noteRect.Height)) / 2;
          end;

          rectf.X := rectf.X - 1;
          slineh := rectf.Y + slineh + 4;

          if (Caption <> '') then
            graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush)
          else
            graphics.DrawString(WideCaption, Length(WideCaption), font, rectf, stringFormat, solidBrush);

          if (Notes.Text <> '') then
          begin
            rectf.Y := rectf.Y + round(sizeRect.Height);
            nfont := TGPFont.Create(nfontFamily, NotesFont.Size , nfs, UnitPoint);
            nsolidBrush := TGPSolidBrush.Create(ColorToARGB(NotesFont.Color));
            graphics.DrawString(Notes.Text, Length(Notes.Text), nfont, rectf, stringFormat, nsolidBrush);
            nsolidBrush.Free;
            nfont.Free;
          end
        end;
      end;
    end;

    stringformat.Free;
    solidBrush.Free;
    font.Free;
  end;

  if (Layout = blGlyphLeftAdjusted) and not AutoSize then
    imgx := imgx + MargH;

  fontFamily.Free;
  nfontFamily.Free;

  if not AutoSize then
  begin
    if DropDownButton then
    begin
      if DropDownPos = dpRight then
        w := w - 12
      else
        h := h - 8;
    end;

    if DrawPic and not OverlapText then
    begin
      if Assigned(Glyph) and not Glyph.Empty and (Glyph.Width > 1) and (Glyph.Height > 1) then
      begin
        if (ForcePicSize.CX > 0) and (ForcePicSize.CY > 0) then
        begin
          Glyph.Transparent := True;
          if (Caption = '') and (WideCaption = '') then
          begin
            px := r.Left + Max(0, (w - ImgW) div 2);
            py := r.Top + Max(0, (h - ImgH) div 2);
            Canvas.StretchDraw(Rect(px, py, px + ForcePicSize.CX, py + ForcePicSize.CY), Glyph);
          end
          else
            Canvas.StretchDraw(Rect(ImgX, ImgY, ImgX + ForcePicSize.CX, ImgY + ForcePicSize.CY), Glyph);
        end
        else
        begin
          if (Caption = '') and (WideCaption = '') then
            Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Glyph)
          else
            Canvas.Draw(ImgX, ImgY, Glyph);
        end;
      end
      else
        if Assigned(Picture) and not Picture.Empty then
        begin
          if (ForcePicSize.CX > 0) and (ForcePicSize.CY > 0) then
          begin
            if (Caption = '') and (WideCaption = '') then
            begin
              px := r.Left + Max(0, (w - ImgW) div 2);
              py := r.Top + Max(0, (h - ImgH) div 2);
              //Canvas.StretchDraw(Rect(px, py, px + ForcePicSize.CX, py + ForcePicSize.CY), Picture);
              DrawStretchPicture(graphics, Canvas, Rect(px, py, px + ForcePicSize.CX, py + ForcePicSize.CY), Picture);
            end
            else
            begin
              //Canvas.StretchDraw(Rect(ImgX, ImgY, ImgX + ForcePicSize.CX, ImgY + ForcePicSize.CY), Picture);
              DrawStretchPicture(graphics, Canvas, Rect(ImgX, ImgY, ImgX + ForcePicSize.CX, ImgY + ForcePicSize.CY), Picture);
            end;
          end
          else
          begin
            if (Caption = '') and (WideCaption = '') then
            begin
              //Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Picture)
              //DrawGDIPImage(graphics, Point(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2)), Picture);
              //DrawStretchPicture(graphics, Canvas, Rect(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Picture.Width, Picture.Height), Picture);

              if not Assigned(Icon) then
                DrawGDIPImage(graphics, Point(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2)), Picture, w, w)
              else
              begin
                DrawIconEx(Canvas.Handle, r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Icon.Handle, ImgW, ImgH, 0, 0, DI_NORMAL);
              end;
            end
            else
              //Canvas.Draw(ImgX, ImgY, Picture);
              DrawGDIPImage(graphics, Point(ImgX, ImgY), Picture, w, h);
          end;
        end
        else
          if (ImageIndex <> -1) and Assigned(Images) then
          begin
            if (Caption = '') and (WideCaption = '') then
            begin
              if Assigned(Owner) and Assigned(Owner.Parent) and (Owner.Parent.ClassName = 'TAdvQuickAccessToolBar') and (GDIPArrow or (IsGlass and IsWin7)) then
                DrawGDIPImageFromImageList(graphics, Point(r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2)), Images, ImageIndex, EnabledImage)
              else
              begin
                {$IFDEF DELPHI2007_LVL}
                if not EnabledImage and not Assigned(DisabledImages) and IsVista and TMSDISABLEWITHCOLORSATURATION then
                begin
                  DrawDisabledImage(Canvas.Handle, Images, ImageIndex, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2))
                end
                else
                {$ENDIF}
                begin
                  if not EnabledImage and Assigned(DisabledImages) then
                    DisabledImages.Draw(Canvas, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2), ImageIndex, true)
                  else
                    Images.Draw(Canvas, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2), ImageIndex, EnabledImage);
                end;
              end;
            end
            else
            begin
              if Assigned(Owner) and Assigned(Owner.Parent) and (Owner.Parent.ClassName = 'TAdvQuickAccessToolBar') and (GDIPArrow or (IsGlass and IsWin7)) then
                DrawGDIPImageFromImageList(graphics, Point(ImgX, ImgY), Images, ImageIndex, True{EnabledImage})
              else
              begin
                {$IFDEF DELPHI2007_LVL}
                if not EnabledImage and not Assigned(DisabledImages) and IsVista and TMSDISABLEWITHCOLORSATURATION then
                begin
                  DrawDisabledImage(Canvas.Handle, Images, ImageIndex, ImgX, ImgY);
                end
                else
                {$ENDIF}
                begin
                  if not EnabledImage and Assigned(DisabledImages) then
                    DisabledImages.Draw(Canvas, ImgX, ImgY, ImageIndex, true)
                  else
                    Images.Draw(Canvas, ImgX, ImgY, ImageIndex, EnabledImage);
                end;
              end;
            end;
            {end
            else if Assigned(ToolImage) and not (ToolImage.Empty) and (ToolImage.Width > 1) then
            begin
              if Caption = '' then
                Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), ToolImage)
              else
                Canvas.Draw(ImgX, ImgY, ToolImage); }
          end;
    end;

    Canvas.Brush.Style := bsClear;

    if DropDownButton then
    begin
      if DrawDwLine and DropDownSplit then
      begin
        Canvas.Pen.Color := ColorToRGB(PC);
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

//      AP.X := DwR.Left + ((DwR.Right - DwR.Left - 5) div 2);

      if (DropDir = Down) then
        AP.X := DwR.Left + ((DwR.Right - DwR.Left - 3) div 2)
      else
        AP.X := DwR.Left + ((DwR.Right - DwR.Left - 5) div 2);

      // always vertically center button in its area
//      if (DropDownPos = dpBottom) or ((Caption = '') and (WideCaption = '')) then
        AP.Y := DwR.Top + ((DwR.Bottom - DwR.Top - 3) div 2) + 1;
//      else
//        AP.Y := yDropD - 8;

      // do NOT force a multiline button to always have its dropdown on the right
      mline := false;

      if mline and not DropDownSplit and (DropDownPos = dpBottom) then
      begin
        AP.X := r.Right - FDropDownSectWidth;
        AP.Y := Round(slineh);
      end;

      if GDIPArrow or (IsGlass and IsWin7) then
      begin
        if not Enabled then
          DrawGDIPArrow(Canvas, AP, clGray)
        else
          DrawGDIPArrow(Canvas, AP, clBlack);
      end
      else
      begin
        if not Enabled then
          DrawArrow(Canvas, AP, clGray, clWhite, DropDir)
        else
          DrawArrow(Canvas, AP, clBlack, clWhite, DropDir);
      end;
    end;
  end;

  graphics.Free;
end;

//------------------------------------------------------------------------------

{TWinCtrl}

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

//------------------------------------------------------------------------------

{ TAdvGlowButton }


//------------------------------------------------------------------------------


procedure TAdvCustomGlowButton.CMMouseEnter(var Msg: TMessage);
{$IFNDEF DELPHI6_LVL}
var
  i: integer;
{$ENDIF}
begin
  inherited;

  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);

  if (csDesigning in ComponentState) then
    Exit;

  if FMouseEnter then
    Exit;

  {$IFNDEF DELPHI6_LVL}   // Workaround for VCL shortcoming in Delphi 5
  try
    if Assigned(Owner) then
    begin
      for i := Owner.ComponentCount - 1 downto 0 do
      begin
        if Owner.Components[i] is TAdvCustomGlowButton then
        begin
          if (Owner.Components[i] as TAdvCustomGlowButton).FMouseEnter then
          begin
            (Owner.Components[i] as TAdvCustomGlowButton).FMouseEnter := False;
            (Owner.Components[i] as TAdvCustomGlowButton).FHot := False;
            (Owner.Components[i] as TAdvCustomGlowButton).FLeftDown := False;
            (Owner.Components[i] as TAdvCustomGlowButton).Repaint;
          end;
        end;
      end;
    end;
  except
  end;
 {$ENDIF}

  FHot := true;

  if FLeftDown then
    FDown := true;

  if not Assigned(FTimer) and (GlowSpeed > 0) then
  begin
    FTimer := TTimer.Create(self);
    FTimer.OnTimer := TimerProc;
    FTimer.Interval := GlowSpeed;
    FTimer.Enabled := true;
  end;

  if not FDown and (GlowState <> gsPush) then
  begin
    FTimeInc := 20;
    GlowState := gsHover;
  end;

  Invalidate;

  FMouseInControl := true;
  FMouseEnter := true;
  FHasMouse := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.CMMouseLeave(var Msg: TMessage);
begin
  inherited;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);

  if (csDesigning in ComponentState) then
    Exit;

  if not FMouseEnter then
    Exit;

  FHasMouse := false;
  FMouseEnter := false;
  FMouseInControl := false;

  FHot := false;
  FInButton := false;

//  Repaint;

  // down process busy
  if FDown and FMouseDown then
  begin
    FDown := False;
    FTimeInc := -20;
    GlowState := gsHover;
    Invalidate;
    FLeftDown := true;
  end
  else
    //if not (Style = bsCheck) then
    begin
      FDown := false;
      FStepHover := 100;
      FTimeInc := -20;
      GlowState := gsHover;
      Invalidate;
    end;

  if not Assigned(FTimer) and (GlowSpeed > 0) then
  begin
    FTimer := TTimer.Create(self);
    FTimer.OnTimer := TimerProc;
    FTimer.Interval := GlowSpeed;
    FTimer.Enabled := true;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
  begin
    Click;
  end;
end;

//------------------------------------------------------------------------------

constructor TAdvCustomGlowButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csDoubleClicks];
  FStaticButton := false;
  FAlwaysShowAccel := false;
  FCompStyle := tsCustom;
  FTimer := nil;
  FImageIndex := -1;
  DoubleBuffered := true;
  FGroupIndex := 0;
  FState := absUp;
  FStyle := bsButton;
  FTransparent := False;
  FLayout := blGlyphLeft;
  FDropDownButton := False;
  FDropDownPosition := dpRight;
  FDropDownDirection := ddDown;
  FDropDownSplit := true;
  FShowCaption := true;
  FFocusType := ftBorder;
  FShortCutHint := nil;
  FShortCutHintPos := shpTop;
  FShowDisabled := true;
  FOverlappedText := false;
  FSpacing := 2;
  FWordWrap := true;
  FFirstPaint := true;
  FMarginVert := 1;
  FMarginHorz := 1;
  FRounded := true;
  FInitRepeatPause := 400;
  FRepeatPause := 100;
  FRepeatClick := false;

  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := PictureChanged;

  FIDisabledPicture := TGDIPPicture.Create;
  FIDisabledPicture.OnChange := PictureChanged;
  FIHotPicture := TGDIPPicture.Create;

  ParentFont := true;
  FAppearance := TGlowButtonAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;
  FInternalImages := nil;
  FAntiAlias := aaClearType;
  FBorderStyle := bsSingle;

  FOfficeHint := TAdvHintInfo.Create;

  Width := 100;
  Height := 41;

  FDefaultPicDrawing := True;
  FDefaultCaptionDrawing := True;
  FTrimming := StringTrimmingNone;

  FCommandID := -1;

  FButtonSizeState := bsLarge;
  FMaxButtonSizeState := bsLarge;
  FMinButtonSizeState := bsGlyph;
  FOldLayout := Layout;
  FOldDropDownPosition := DropDownPosition;

  FNotes := TStringList.Create;
  FNotes.OnChange := NotesChanged;
  FNotesFont := TFont.Create;
  FNotesFont.Name := 'Tahoma';
  FNotesFont.Size := 8;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;

  FDisabledImageChangeLink := TChangeLink.Create;
  FDisabledImageChangeLink.OnChange := DisabledImageListChange;

  FHotImageChangeLink := TChangeLink.Create;
  FHotImageChangeLink.OnChange := HotImageListChange;
  FImageListType := ipActionList;
  FIcon := nil;
end;


procedure TAdvCustomGlowButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
//  if FTransparent then
//    Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TAdvCustomGlowButton.CreateWnd;
begin
  inherited;

  FActive := FDefault;
  FParentForm := GetParentForm(Self);
  GlowState := gsNone;
  FLeftDown := false;
  FMouseInControl := false;
  FMouseEnter := false;
  FMouseDown := false;
  FHot := false;

  if not FInitialDown then
  begin
    FState := absUp;
    FDown := false;
  end;

  FDragging := false;
  FFirstPaint := true;
  FStepHover := 0;

  if MouseCapture then
    ReleaseCapture;

  MouseCapture := false;
  if Assigned(FTimer) then
    FreeAndNil(FTimer);
end;

//------------------------------------------------------------------------------

destructor TAdvCustomGlowButton.Destroy;
begin
  if Assigned(FShortCutHint) then
    FShortCutHint.Free;
  FOfficeHint.Free;
  FAppearance.Free;
  FIPicture.Free;
  FIDisabledPicture.Free;
  FIHotPicture.Free;
  FNotes.Free;
  FNotesFont.Free;
  if Assigned(FIcon) then
    FreeAndNil(FIcon);
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FDisabledImageChangeLink);
  FreeAndNil(FHotImageChangeLink);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.DisabledImageListChange(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  FHasFocus := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  FHasFocus := false;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.DoEnter;
begin
  inherited;
  Invalidate;
  FHasFocus := true;
end;

procedure TAdvCustomGlowButton.DoExit;
begin
  inherited;

  if FGroupIndex = 0 then
  begin
    FDown := false;
    FState := absUp;
  end;

  FHasFocus := false;
  Invalidate;
end;

procedure TAdvCustomGlowButton.ShowShortCutHint;
var
  pt: TPoint;
  SCHintPos: TShortCutHintPos;
  OffsetX: Integer;
begin
  if not Assigned(FShortCutHint) then
  begin
    FShortCutHint := TShortCutHintWindow.Create(Self);
    FShortCutHint.Parent := Self;
    FShortCutHint.Visible := False;
    FShortCutHint.Color := clWhite;
    FShortCutHint.ColorTo := Appearance.Color;
  end;

  FShortCutHint.Caption := FShortCutHintText;

  pt := ClientToScreen(Point(0,0));

  OffsetX := 6;
  SCHintPos := ShortCutHintPos;

  if Assigned(FOnGetShortCutHintPos) then
    FOnGetShortCutHintPos(Self, ButtonSizeState, SCHintPos);
    
  if (SCHintPos = shpAuto) then
    SCHintPos := shpTop;

  case SCHintPos of
  shpLeft:
    begin
      //FShortCutHint.Left := pt.X - (FShortCutHint.Width div 2);
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  shpTop:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  shpRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - (FShortCutHint.Width div 2);
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  shpBottom:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  shpCenter:
    begin
      FShortCutHint.Left  := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  shpTopLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  shpTopRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  shpAboveTop:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y - FShortCutHint.Height;
    end;
  shpAboveTopLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y - FShortCutHint.Height;
    end;
  shpAboveTopRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y - FShortCutHint.Height;
    end;
  shpBottomLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  shpBottomRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  shpBelowBottom:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height;
    end;
  shpBelowBottomLeft:
    begin
      FShortCutHint.Left := pt.X + OffsetX;
      FShortCutHint.Top := pt.Y + self.Height
    end;
  shpBelowBottomRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - FShortCutHint.Width + 1;
      FShortCutHint.Top := pt.Y + self.Height
    end;
  shpBelowBottomCenter:
    begin
      FShortCutHint.Left  := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height
    end;
  end;

  FShortCutHint.Visible := true;
end;

procedure TAdvCustomGlowButton.HideShortCutHint;
begin
  if Assigned(FShortCutHint) then
  begin
    FShortCutHint.Visible := false;
    //FShortCutHint.Free;
    //FShortCutHint := nil;
  end;
end;

procedure TAdvCustomGlowButton.HotImageListChange(Sender: TObject);
begin
  Invalidate;
end;

function TAdvCustomGlowButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) +
    '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomGlowButton.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCustomGlowButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key in [VK_SPACE, VK_RETURN]) then
  begin
    FDown := True;
    FState := absDown;
    Repaint;
  end;

  if (Key = VK_F4) and (Shift = []) then
    DoDropDown;

  if Assigned(FOnInternalKeyDown) then
    FOnInternalKeyDown(Self, Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMGetDlgCode(var Message: TMessage);
begin
  if Assigned(FOnInternalKeyDown) then
    Message.Result := DLGC_WANTARROWS
  else
    inherited;
end;


//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  inherited;

  if (Key = #32) or (Key = #13) then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.ModalResult := ModalResult;

    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key in [VK_SPACE, VK_RETURN]) then
  begin
    if (FGroupIndex = 0) then
    begin
      FDown := False;
      FState := absUp;
    end
    else
    begin
      FDown := not FDown;
      FDownChecked := not FDownChecked;
      if FDown then
        FState := absExclusive;
      UpdateExclusive;
    end;
  end;
  Repaint;
end;

//------------------------------------------------------------------------------


function TAdvCustomGlowButton.ActionHasImages: boolean;
begin
  Result := false;

  {$IFDEF DELPHI2006_LVL}
  if not self.StaticActionImageIndex then
    Result := true
  else
  {$ENDIF}
  if Assigned(Action) then
  begin
    if (Action.Owner is TActionList) then
      Result := Assigned((Action.Owner as TActionList).Images);
  end;
end;

procedure TAdvCustomGlowButton.Assign(Source: TPersistent);
begin
  if (Source is TAdvCustomGlowButton) then
  begin
    Align := (Source as TAdvCustomGlowButton).Align;
    Action := (Source as TAdvCustomGlowButton).Action;
    Anchors := (Source as TAdvCustomGlowButton).Anchors;
    AntiAlias := (Source as TAdvCustomGlowButton).AntiAlias;
    AutoSize := (Source as TAdvCustomGlowButton).AutoSize;
    BorderStyle := (Source as TAdvCustomGlowButton).BorderStyle;
    Cancel := (Source as TAdvCustomGlowButton).Cancel;
    Caption := (Source as TAdvCustomGlowButton).Caption;
    Constraints := (Source as TAdvCustomGlowButton).Constraints;
    Default := (Source as TAdvCustomGlowButton).Default;
    Font.Assign((Source as TAdvCustomGlowButton).Font);
    ImageIndex := (Source as TAdvCustomGlowButton).ImageIndex;
    Images  := (Source as TAdvCustomGlowButton).Images;
    DisabledImages := (Source as TAdvCustomGlowButton).DisabledImages;
    DisabledPicture.Assign((Source as TAdvCustomGlowButton).DisabledPicture);
    DragMode := (Source as TAdvCustomGlowButton).DragMode;
    DragKind := (Source as TAdvCustomGlowButton).DragKind;
    FocusType := (Source as TAdvCustomGlowButton).FocusType;
    HotImages := (Source as TAdvCustomGlowButton).HotImages;
    HotPicture.Assign((Source as TAdvCustomGlowButton).HotPicture);
    MarginVert := (Source as TAdvCustomGlowButton).MarginVert;
    MarginHorz := (Source as TAdvCustomGlowButton).MarginHorz;
    ModalResult := (Source as TAdvCustomGlowButton).ModalResult;
    Notes.Assign((Source as TAdvCustomGlowButton).Notes);
    NotesFont.Assign((Source as TAdvCustomGlowButton).NotesFont);
    OfficeHint.Assign((Source as TAdvCustomGlowButton).OfficeHint);
    ParentFont := (Source as TAdvCustomGlowButton).ParentFont;;
    Picture.Assign((Source as TAdvCustomGlowButton).Picture);
    PopupMenu := (Source as TAdvCustomGlowButton).PopupMenu;
    Position  := (Source as TAdvCustomGlowButton).Position;
    InitRepeatPause := (Source as TAdvCustomGlowButton).InitRepeatPause;
    RepeatPause := (Source as TAdvCustomGlowButton).RepeatPause;
    RepeatClick := (Source as TAdvCustomGlowButton).RepeatClick;
    Rounded := (Source as TAdvCustomGlowButton).Rounded;
    ShortCutHint := (Source as TAdvCustomGlowButton).ShortCutHint;
    ShortCutHintPos := (Source as TAdvCustomGlowButton).ShortCutHintPos;
    ShowCaption := (Source as TAdvCustomGlowButton).ShowCaption;
    ShowDisabled := (Source as TAdvCustomGlowButton).ShowDisabled;
    Spacing := (Source as TAdvCustomGlowButton).Spacing;
    Transparent := (Source as TAdvCustomGlowButton).Transparent;
    Trimming := (Source as TAdvCustomGlowButton).Trimming;
    Version := (Source as TAdvCustomGlowButton).Version;
    WordWrap := (Source as TAdvCustomGlowButton).WordWrap;
    ShowHint := (Source as TAdvCustomGlowButton).ShowHint;
    ParentShowHint := (Source as TAdvCustomGlowButton).ParentShowHint;
    TabOrder := (Source as TAdvCustomGlowButton).TabOrder;
    TabStop := (Source as TAdvCustomGlowButton).TabStop;
    Visible := (Source as TAdvCustomGlowButton).Visible;
  end;
end;

procedure TAdvCustomGlowButton.Click;
var
  Form: TCustomForm;
begin

  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;

  if Assigned(FOnInternalClick) then
    FOnInternalClick(Self);

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.Loaded;
begin
  inherited;

  if (Down <> FInitialDown) then
  begin
    Down := FInitialDown;
  end;

  FIsVista := IsVista;

  if AutoSize then
    PerformResize;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.DoDropDown;
var
  pt: TPoint;
begin
  if IsMenuButton or Assigned(FDropDownMenu) then
  begin
    {State := absDropDown;
    Invalidate;
    CheckMenuDropdown; }

    if Assigned(FDropDownMenu) then
    begin
      if Assigned(FOnInternalDropDown) then
        FOnInternalDropDown(Self);

      FDropDownMenu.PopupComponent := Self;

      //FDown := false;
      //FHot := false;
      FState := absDown;
      PopupBtnDown;
      Invalidate;

      case DropDownDirection of
      ddDown: pt := Point(Left, Top + Height);
      ddRight: pt := Point(Left + Width, Top);
      end;

      pt := Parent.ClientToScreen(pt);

      case FDropDownMenu.Alignment of
      paCenter: pt.X := pt.X + Width div 2;
      paRight: pt.X := pt.X + Width;
      end;

      FDropDownMenu.Popup(pt.X,pt.Y);

      FState := absUp;
      Repaint;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGlowButton.IsDropDown: Boolean;
begin
  Result := false;
end;

function TAdvCustomGlowButton.IsFontStored: Boolean;
begin
  Result := not ParentFont;
end;

function TAdvCustomGlowButton.IsMenuButton: Boolean;
begin
  Result := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.TimerExpired(Sender: TObject);
begin
  FRepeatTimer.Interval := RepeatPause;
  if (FDown) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);

  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  pt:TPoint;
  InBottomDrop,InRightDrop: boolean;
  InSepBtn: boolean;

begin
  inherited;

  {$IFDEF DELPHIXE_LVL}
  if (ssTouch in Shift) or (ssPen in Shift) then
    Perform(CM_MOUSEENTER, X, Y);
  {$ENDIF}

  if Button <> mbLeft then
    Exit;

  if FRepeatClick then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create(Self);

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled  := True;
  end;

  FDown := true;
  FMouseDown := true;

  if TabStop then
    SetFocus;

  if not Assigned(FTimer) and (GlowSpeed > 0) then
  begin
    FTimer := TTimer.Create(self);
    FTimer.OnTimer := TimerProc;
    FTimer.Interval := GlowSpeed;
    FTimer.Enabled := true;
  end;

  //FStepPush := 0;
  FTimeInc := +20;
  GlowState := gsPush;

  if not DropDownButton and IsMenuButton and false then
  begin
    Invalidate;
    DoDropDown;
  end;

  InBottomDrop := (DropDownPosition = dpRight) and (X > (Width - DropDownSectWidth));
  InRightDrop := (DropDownPosition = dpBottom) and (Y > (Height - DropDownSectWidth));

  InSepBtn := (InBottomDrop or InRightDrop);

  if (not FDropDownButton and IsMenuButton) or
     (FDropDownButton and InSepBtn and DropDownSplit) or
     (FDropDownButton and not DropDownSplit and (not ((Style = bsCheck) or (GroupIndex > 0))))
      then
  begin
    // FState := absUp;

    FMouseInControl := False;

    // FMouseDownInControl := False;
    PopupBtnDown;

    if Assigned(FDropDownMenu) then
    begin
      FDropDownMenu.PopupComponent := Self;
      FDown := not FDropDownSplit;
      //FHot := false;
      SetDroppedDown(True);

      // prevent that mouse leave message changes button state & painting
      FMouseEnter := false;

      Repaint;

      case DropDownDirection of
        ddDown: pt := Point(Left, Top + Height);
        ddRight: pt := Point(Left + Width, Top);
      end;

      pt := Parent.ClientToScreen(pt);

      case FDropDownMenu.Alignment of
        paRight: pt.X := pt.X + Width;
        paCenter: pt.X := pt.X + Width div 2;
      end;

      FDropDownMenu.Popup(pt.X,pt.Y);

      FDown := false;
      SetDroppedDown(False);

      GetCursorPos(pt);
      pt := ScreenToClient(pt);
      if not PtInRect(ClientRect, pt) then
      begin
        FMouseEnter := false;
        FMouseInControl := false;
        FHot := false;
        FInButton := false;
      end;
      Repaint;
    end;

    Invalidate;
  end
  else
  begin
    if (Style = bsCheck) then
    begin
      SetDown(not FDownChecked);
    end;

    if not FDownChecked then
    begin
      FState := absDown;
      Invalidate;
    end;

    if (Style = bsCheck) then
    begin
      FState := absDown;
      Repaint;
    end;

    FDragging := True;
  end;

  {$IFDEF DELPHIXE_LVL}
  if (ssTouch in Shift) or (ssPen in Shift) then  // to release the focus after Touch usage in Win8 Design
    if not Assigned(FTimer)  then
    begin
      FTimer := TTimer.Create(Self);
      FTimer.OnTimer := TimerProc;
      FTimer.Interval := 20;
      FTimer.Enabled := true;
    end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.ImageListChange(Sender: TObject);
begin
  if HandleAllocated then
  begin
    Invalidate;
  end;
end;

procedure TAdvCustomGlowButton.InternalClick;
begin
  if (not FDropDownButton and IsMenuButton) or (FDropDownButton and not ((Style = bsCheck) or (GroupIndex > 0)) and
     (not DropDownSplit)) then
  begin
    if Assigned(FDropDownMenu) then
    begin
      //PostMessage(Handle, WM_LBUTTONDOWN,0,0);
      //PostMessage(Handle, WM_LBUTTONUP,0,0);
      DoDropDown;
    end
    else
      Click;
  end
  else
  begin
    if Style = bsCheck then
    begin
      SetDown(not FDownChecked);
    end;
    (*
    if not FDownChecked then
    begin
      FState := absDown;
      Invalidate;
    end;
    *)
    if (Style = bsCheck) then
    begin
      FState := absDown;
      Repaint;
    end;

    Click;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMLDblClk(var Msg: TWMLButtonDblClk);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMPaint(var Msg: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if not FDoubleBuffered or (Msg.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Msg);
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
      Msg.DC := MemDC;
      WMPaint(Msg);
      Msg.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0, SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomGlowButton.WMLButtonDown(var Msg:TWMLButtonDown);
begin
  FGotButtonClick := true;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAdvCustomGlowButton.WMLButtonUp(var Msg:TWMLButtonDown);
var
  DoClick: Boolean;
  inht: boolean;
  frm: TCustomForm;

begin
  FTimeInc := -10;
  FStepPush := 100;
  inht := false;
  GlowState := gsPush;

  FMouseDown := false;
  FLeftDown := false;

  if not Assigned(FTimer) and (GlowSpeed > 0) then
  begin
    FTimer := TTimer.Create(self);
    FTimer.OnTimer := TimerProc;
    FTimer.Interval := GlowSpeed;
    FTimer.Enabled := true;
  end;

  if not DropDownButton and IsMenuButton then
  begin
    // do nothing
  end
  else
  if FDragging then
  begin
    FDragging := False;

    DoClick := (Msg.XPos >= 0) and (Msg.XPos < ClientWidth) and (Msg.YPos >= 0) and (Msg.YPos <= ClientHeight);

    if (FGroupIndex = 0) then
    begin
      // Redraw face in-case mouse is captured
      FState := absUp;
      FMouseInControl := False;

      if (Style = bsCheck) then
      begin
        if Assigned(Action) then
        begin
          inherited;
          inht := true;
          if (FCheckLinked or FGroupIndexLinked) then
            Exit;
          {$IFDEF DELPHI7_LVL}
          if (Action is TAction) then
            if (Action as TAction).AutoCheck then
              Exit;
          {$ENDIF}
        end;

        // ***** extension for toolbar compactbutton handling
        if not DoClick and Self.Down then
        begin
          Self.Down := not Self.Down;
        end;

        if (Style <> bsCheck) then
        begin
          SetDown(not FDownChecked);
        end;

        //FState := absUp;
        FMouseInControl := true;
        Repaint;
      end;

      if DoClick and not (FState in [absExclusive, absDown]) then
        Invalidate;
    end
    else
    begin
      if Assigned(Action) then
        if FCheckLinked or FGroupIndexLinked then
        begin
          inherited;
          Exit;
        end;

      if DoClick then
      begin
        SetDown(not FDownChecked);
        if FDownChecked then
          Repaint;
      end
      else
      begin
        if FDownChecked then
          FState := absExclusive;
        Repaint;
      end;

    end;

    UpdateTracking;
  end;

  frm := GetParentForm(Self);

  if FGotButtonClick then
  begin
    ControlState := ControlState + [csClicked];
  end
  else
  begin
    DoClick := true;  // prevent extra click on spawned form
    if Assigned(frm) and not frm.Active then
      DoClick := false;

    if Assigned(OnClick) and DoClick and FGotButtonClick then
    begin
      if Assigned(ActionLink) then
        OnClick(ActionLink.Action)
      else
        OnClick(Self);
    end;
  end;

  FGotButtonClick := false;

  if not inht and not FDroppedDown then
    inherited;

  if (Style = bsCheck) or (GroupIndex > 0) then
    Repaint;

  Invalidate;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI_UNICODE}
procedure TAdvCustomGlowButton.ChangeScale(M, D: Integer);
begin
  inherited;
  NotesFont.Size := MulDiv(NotesFont.Size,M,D);
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.NotesChanged(Sender: TObject);
begin
  if AutoSize then
  begin
    PerformResize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FDisabledImages) then
    FDisabledImages := nil;

  if (AOperation = opRemove) and (AComponent = FHotImages) then
  begin
    FHotImages := nil;
  end;

  if (AOperation = opRemove) and (AComponent = DropdownMenu) then
    DropdownMenu := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.DrawGlyphCaption;
begin
//
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.GetToolImage(bmp: TBitmap);
begin
//
end;

procedure TAdvCustomGlowButton.GetToolPicture(var pic: TGDIPPicture);
begin
//
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDroppedDown(Value: Boolean);
begin
  FDroppedDown := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.Paint;
var
  GradColor: TColor;
  GradColorTo: TColor;
  GradColorMirror: TColor;
  GradColorMirrorTo: TColor;
  PenColor: TColor;
  PenColorFocused: TColor;
  GradB, GradU: TGDIPGradient;
  DrawDwLn: Boolean;
  ImgList: TCustomImageList;
  Pic: TGDIPPicture;
  EnabledImg: Boolean;
  Rgn1, Rgn2: HRGN;
  R: TRect;
  i, w, h: Integer;
  p: TPoint;
  DCaption: string;
  DWideCaption: widestring;
  BD: TButtonDisplay;
  DrawFocused, DrawFocusedHot: boolean;
  bmp: TBitmap;
  sz: TSize;
  gs: TGlowButtonState;
  PicSize: TSize;
  AFont: TFont;
  ActnList: TCustomActionList;
  szrect,drrect: TRect;

begin
  if FPainting then
    Exit;

  FPainting := True;

  try
    if FTransparent and (Appearance.TranspHover or (not FMouseEnter or StaticButton)) then
    begin
      // TRANSPARENCY CODE
      R := ClientRect;
      rgn1 :=  CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
      SelectClipRgn(Canvas.Handle, rgn1);

      i := SaveDC(Canvas.Handle);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;
      MoveWindowOrg(Canvas.Handle, p.x, p.y);

      SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
      // transparency ?
      SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);

      if (Parent is TWinCtrl) then
       (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);

      RestoreDC(Canvas.Handle, i);

      SelectClipRgn(Canvas.Handle, 0);
      DeleteObject(rgn1);
    end;

    if not Enabled then
    begin
      FState := absDisabled;
      FDragging := False;
    end
    else
    begin
      if (FState = absDisabled) then
        if FDownChecked and (GroupIndex <> 0) then
          FState := absExclusive
        else
          FState := absUp;
    end;

    if (Style = bsCheck) and Down then
    begin
      FState := absDown;
    end;

    if FKeepDown then
    begin
      FState := absDown;
      FDown := true;
    end;

    with Appearance do
    begin
      DrawDwLn := False;
      PenColorFocused := FBorderColorFocused;

      if ((State = absDisabled) or not Enabled) and FShowDisabled then
      begin
        GradColor := FColorDisabled;
        GradColorTo := FColorDisabledTo;
        GradColorMirror := FColorMirrorDisabled;
        GradColorMirrorTo := FColorMirrorDisabledTo;
        PenColor := BorderColorDisabled;
        GradU := GradientDisabled;
        GradB := GradientMirrorDisabled;
      end
      else if (State = absDown) and not ((Style = bsCheck) and (State = absDown)) then
      begin
        GradColor := FColorDown;
        GradColorTo := FColorDownTo;
        GradColorMirror := FColorMirrorDown;
        GradColorMirrorTo := FColorMirrorDownTo;
        PenColor := BorderColorDown;
        GradU := GradientDown;
        GradB := GradientMirrorDown;
        DrawDwLn := True;
      end
      else
      if (State = absExclusive) or ((Style = bsCheck) and (State = absDown)) then
      begin
        GradColor := FColorChecked;
        GradColorTo := FColorCheckedTo;
        GradColorMirror := FColorMirrorChecked;
        GradColorMirrorTo := FColorMirrorCheckedTo;

        if FHasMouse and (BorderColorCheckedHot <> clNone) and not FMouseDown then
          PenColor := BorderColorCheckedHot
        else
          PenColor := BorderColorChecked;

        GradU := GradientChecked;
        GradB := GradientMirrorChecked;

        if Assigned(FTimer) and not (not FMouseInControl and ((Style = bsCheck) or ((GroupIndex > 0) and (State <> absDown)))) then
        begin
          GradColor := BlendColor(FColorChecked, FColorDown, FStepPush);
          GradColorTo := BlendColor(FColorCheckedTo, FColorDownTo, FStepPush);
          GradColorMirror := BlendColor(FColorMirrorChecked, FColorMirrorDown, FStepPush);
          GradColorMirrorTo := BlendColor(FColorMirrorCheckedTo, FColorMirrorDownTo, FStepPush);
        end;

        if Assigned(FTimer) and FMouseEnter and (Style = bsCheck) and (State = absDown) then
        begin
          GradColor := BlendColor(FColorChecked, FColorDown, FStepPush);
          GradColorTo := BlendColor(FColorCheckedTo, FColorDownTo, FStepPush);
          GradColorMirror := BlendColor(FColorMirrorChecked, FColorMirrorDown, FStepPush);
          GradColorMirrorTo := BlendColor(FColorMirrorCheckedTo, FColorMirrorDownTo, FStepPush);
        end;
      end
      else //if State = absUp then
      begin
        if FHot then
        begin
          GradColor := FColorHot;
          GradColorTo := FColorHotTo;
          GradColorMirror := FColorMirrorHot;
          GradColorMirrorTo := FColorMirrorHotTo;
          PenColor := BorderColorHot;
          GradU := GradientHot;
          GradB := GradientMirrorHot;
          DrawDwLn := True;
        end
        else // Normal draw
        begin
          if (1>0) {Transparent} then
          begin
            GradColor := FColor;
            GradColorTo := FColorTo;
            GradColorMirror := FColorMirror;
            GradColorMirrorTo := FColorMirrorTo;
            PenColor := BorderColor;
            GradU := Gradient;
            GradB := GradientMirror;
          end
          else
          begin
          end;
        end;
      end;

      if GradColorMirror = clNone then
        GradColorMirror := GradColor;

      if Assigned(FTimer) then
      begin
        if not FDown and not Transparent and not ((State = absExclusive) or ((Style = bsCheck) and (State = absDown)))  then
        begin
          GradColor := BlendColor(FColorHot, FColor, FStepHover);
          GradColorTo := BlendColor(FColorHotTo, FColorTo, FStepHover);
          GradColorMirror := BlendColor(FColorMirrorHot, FColorMirror, FStepHover);
          GradColorMirrorTo := BlendColor(FColorMirrorHotTo, FColorMirrorTo, FStepHover);
          PenColor := BlendColor(BorderColorHot, BorderColor, FStepHover);
        end
        else
        begin
          if (Style = bsCheck) and (State = absDown) then
          begin
            if FDown then
            begin
              if FMouseEnter then
              begin
                GradColor := BlendColor(FColorChecked, FColorDown, FStepPush);
                GradColorTo := BlendColor(FColorCheckedTo, FColorDownTo, FStepPush);
                GradColorMirror := BlendColor(FColorMirrorChecked, FColorMirrorDown, FStepPush);
                GradColorMirrorTo := BlendColor(FColorMirrorCheckedTo, FColorMirrorDownTo, FStepPush);
              end
              else
              begin
                GradColor := BlendColor(FColorDown, FColorChecked, FStepPush);
                GradColorTo := BlendColor(FColorDownTo, FColorCheckedTo, FStepPush);
                GradColorMirror := BlendColor(FColorMirrorDown, FColorMirrorChecked, FStepPush);
                GradColorMirrorTo := BlendColor(FColorMirrorDownTo, FColorMirrorCheckedTo, FStepPush);
              end;
  //            PenColor := BlendColor(BorderColorDown, BorderColorChecked, FStepPush);
            end;
          end
          else
            if FDown and (State <> absExclusive) and not FKeepDown then
            begin
              GradColor := BlendColor(FColorDown, FColorHot, FStepPush);
              GradColorTo := BlendColor(FColorDownTo, FColorHotTo, FStepPush);
              GradColorMirror := BlendColor(FColorMirrorDown, FColorMirrorHot, FStepPush);
              GradColorMirrorTo := BlendColor(FColorMirrorDownTo, FColorMirrorHotTo, FStepPush);
              PenColor := BlendColor(BorderColorDown, BorderColorHot, FStepPush);
            end;
        end;
      end;

      if Enabled or (DisabledImages = nil) then
      begin
        if FHot and (HotImages <> nil) then
          ImgList := HotImages
        else
          ImgList := Images;

        EnabledImg := Enabled;
      end
      else
      begin
        ImgList := DisabledImages;
        EnabledImg := True;
      end;

      if Enabled or DisabledPicture.Empty then
      begin
        if FHot and not HotPicture.Empty then
          Pic := HotPicture
        else
          Pic := Picture;
      end
      else
        Pic := DisabledPicture;


      if (ImgList = nil) then
      begin
        ImgList := FInternalImages;
        EnabledImg := True;
      end;

      if ShowCaption then
      begin
        DCaption := Caption;
        DWideCaption := WideCaption;
      end
      else
      begin
        DCaption := '';
        DWideCaption := '';
      end;

      if (FMouseInControl or FMouseDown) and DropDownButton then
      begin
        if FInButton then
          BD := bdButton
        else
          BD := bdDropDown;
      end
      else
        BD := bdNone;

      // do not use special border color for non standalone buttons in mouse hover/down state or checked buttons
      if ((Position <> bpStandalone) and FMouseDown) {or ((Style = bsCheck) and (FState = absDown))}  then
      begin
        if (FCompStyle in [tsOffice2003Blue, tsOffice2003Olive, tsOffice2003Classic, tsOffice2003Silver]) then
          PenColor := BorderColor;
      end;

      if ((State = absDisabled) or not Enabled) and FShowDisabled then
      begin
        GradColor := FColorDisabled;
        GradColorTo := FColorDisabledTo;
        GradColorMirror := FColorMirrorDisabled;
        GradColorMirrorTo := FColorMirrorDisabledTo;
        PenColor := BorderColorDisabled;
        GradU := GradientDisabled;
        GradB := GradientMirrorDisabled;
      end;

      if (FHasFocus and (FocusType in [ftHot, ftHotBorder])) and not FDown then
      begin
        GradColor := FColorHot;
        GradColorTo := FColorHotTo;
        GradColorMirror := FColorMirrorHot;
        GradColorMirrorTo := FColorMirrorHotTo;
        PenColor := BorderColorHot;
        GradU := GradientHot;
        GradB := GradientMirrorHot;
        DrawDwLn := True;
      end;

      DrawFocused := (FHasFocus) and (FocusType in [ftBorder, ftHotBorder]);
      DrawFocusedHot := (FHasFocus) and (FocusType in [ftHot, ftHotBorder]);

      AFont := TFont.Create;
      AFont.Assign(Font);

      if FHot then
        AFont.Color := Appearance.TextColorHot;

      if FState = absDown then
        AFont.Color := Appearance.TextColorDown;

      if (Style = bsCheck) and (FState = absDown) then
        AFont.Color := Appearance.TextColorChecked;

      if Appearance.SystemFont then
      begin
        if IsVista then
          AFont.Name := 'Segoe UI'
        else
          AFont.Name := 'Tahoma';
      end;

      bmp := TBitmap.Create;
      bmp.Width := 1;
      bmp.Height := 1;

      GetToolImage(bmp);

      if Assigned(Action) then
      begin
        begin
          if ((Action as TCustomAction).ImageIndex >= 0) {and (ImageIndex = (Action as TCustomAction).ImageIndex)} then
          begin
            ActnList := (Action as TCustomAction).ActionList;

            if Assigned(ActnList) then
            begin
              if (ActnList is TActionList) then
              begin
                if Assigned(TCustomImageList((ActnList as TActionList).Images)) then
                begin
                  if (ImgList = nil) or (FImageListType = ipActionList) then
                  begin
                    ImgList := TCustomImageList((Action as TCustomAction).ActionList.Images);
                    EnabledImg := Enabled;
                  end;
                  FImageIndex := (Action as TCustomAction).ImageIndex;
                end;
              end
              {$IFDEF DELPHI_UNICODE}
              else
                if (ActnList is TActionManager) then
                begin
                  if not Enabled then
                  begin
                    //show disabled-state
                    if (ButtonSizeState in [bsLabel, bsGlyph]) or not Assigned(TCustomImageList((ActnList as TActionManager).LargeDisabledImages)) then
                    begin
                      //use small images
                      if Assigned(TCustomImageList((ActnList as TActionManager).DisabledImages)) then
                      begin
                        if (ImgList = nil) or (FImageListType = ipActionList) then
                        begin
                          ImgList := TCustomImageList((ActnList as TActionManager).DisabledImages);
                          EnabledImg := true;
                        end;
                        FImageIndex := (Action as TCustomAction).ImageIndex;
                      end;
                    end
                    else
                    begin
                      //use large images
                      if Assigned(TCustomImageList((ActnList as TActionManager).LargeDisabledImages)) then
                      begin
                        if (ImgList = nil) or (FImageListType = ipActionList) then
                        begin
                          ImgList := TCustomImageList((ActnList as TActionManager).LargeDisabledImages);
                          EnabledImg := true;
                        end;
                        FImageIndex := (Action as TCustomAction).ImageIndex;
                      end;
                    end;
                  end;

                  if Enabled or (ImgList = nil) then
                  begin
                    //show normal-state
                    if (ButtonSizeState in [bsLabel, bsGlyph]) or not Assigned(TCustomImageList((ActnList as TActionManager).LargeImages)) then
                    begin
                      //use small images
                      if Assigned(TCustomImageList((ActnList as TActionManager).Images)) then
                      begin
                        if (ImgList = nil) or (FImageListType = ipActionList) then
                        begin
                          ImgList := TCustomImageList((ActnList as TActionManager).Images);
                          EnabledImg := Enabled;
                        end;
                        FImageIndex := (Action as TCustomAction).ImageIndex;
                      end;
                    end
                    else
                    begin
                      //use large images
                      if Assigned(TCustomImageList((ActnList as TActionManager).LargeImages)) then
                      begin
                        if (ImgList = nil) or (FImageListType = ipActionList) then
                        begin
                          ImgList := TCustomImageList((ActnList as TActionManager).LargeImages);
                          EnabledImg := Enabled;
                        end;
                        FImageIndex := (Action as TCustomAction).ImageIndex;
                      end;
                    end
                  end;
                end;
              {$ENDIF}

            end;
          end;
        end;
      end;

      GetToolPicture(Pic);

      PicSize.cx := 0;  // no stretch pic
      PicSize.cy := 0;
      if AutoSize then
      begin
        if (ButtonSizeState in [bsLabel, bsGlyph]) then
        begin
          PicSize.cx := 16;
          PicSize.cy := 16;

          {if (bmp.Width = 1) then
          begin
            bmp.Height := Pic.Height;
            bmp.Width := Pic.Width;
            bmp.Canvas.Draw(0, 0, Pic);
            Pic := nil;
          end;}

          if Assigned(ImgList) and (ImageIndex >= 0) then
          begin
            Pic := nil;
          end;
        end;

        if (ButtonSizeState = bsGlyph) then
        begin
          DCaption := '';
          DWideCaption := '';
        end;
      end;

      drrect := ClientRect;

      if DoAutoSize or (FFirstPaint and AutoSize) then
      begin
        szrect := drrect;
        szrect.Bottom := szrect.Top + 4096;

        sz := DrawVistaButton(Self, Canvas, drrect, GradColor, GradColorTo, GradColorMirror,GradColorMirrorTo,
          PenColor, PenColorFocused, GradU, GradB, DCaption, DWideCaption, FDefaultCaptionDrawing, AFont, ImgList, FDisabledImages, ImageIndex, EnabledImg, Layout, FDropDownButton {and (Style <> bsCheck)},
          DrawDwLn, Enabled, DrawFocused, DropDownPosition, Pic, FIcon, PicSize, AntiAlias, FDefaultPicDrawing, bmp, BD,
          FStaticButton or Transparent and not (FMouseEnter or DrawFocusedHot or (State = absDown)), FMouseEnter, Position, DropDownSplit, CanDrawBorder,
          FOverlappedText, FWordWrap, True, FRounded, FDropDownDirection = ddDown,
          FSpacing, FTrimming, FNotes, FNotesFont, FDownChecked, FDroppedDown, TranspHover and Transparent, InternalIsOnGlassQAT, MarginHorz, MarginVert, Appearance.NoBorderDefault, AlwaysShowAccel);

        if AutoSize then
        begin
          W := sz.cx + Spacing * 3 + 2 + 2 * MarginHorz;

          if Layout in [blGlyphTop, blGlyphBottom, blGlyphBottomAdjusted, blGlyphTopAdjusted] then
            H := sz.cy + 2 * MarginVert
          else
            H := sz.cy + Spacing * 2 + 2 * MarginVert;

          if DropDownButton then
          begin
            if (DropDownPosition = dpBottom) then
              H := H + DropDownSectWidth
            else
              W := W + DropDownSectWidth;
          end;

          if Assigned(FOnSetButtonSize) then
            FOnSetButtonSize(Self, w, h);

          if (W <> Width) then
            Width := W;
          if (H <> Height) then
            Height := H;

        end;

        FFirstPaint := false;
      end;

      drrect := ClientRect;

      // transparent border pixels

      sz := DrawVistaButton(Self, Canvas, drrect, GradColor, GradColorTo, GradColorMirror, GradColorMirrorTo,
        PenColor, PenColorFocused, GradU, GradB, DCaption, DWideCaption, FDefaultCaptionDrawing, AFont, ImgList, FDisabledImages, ImageIndex, EnabledImg, Layout, FDropDownButton {and (Style <> bsCheck)},
        DrawDwLn, Enabled, DrawFocused, DropDownPosition, Pic, FIcon, PicSize, AntiAlias, FDefaultPicDrawing, bmp, BD, FStaticButton or Transparent and not (FMouseEnter or DrawFocusedHot or (State = absDown)), FMouseEnter, Position, DropDownSplit, CanDrawBorder, FOverlappedText, FWordWrap,
        False, FRounded, FDropDownDirection = ddDown, FSpacing, FTrimming, FNotes, FNotesFont, FDownChecked, FDroppedDown, TranspHover and Transparent, InternalIsOnGlassQAT, MarginHorz,MarginVert,Appearance.NoBorderDefault, AlwaysShowAccel);

      DrawGlyphCaption;

      gs := gsNormal;

      if FMouseEnter then
        gs := gsHot;

      if State = absDown then
        gs := gsDown;

      if Assigned(OnDrawButton) then
        OnDrawButton(Self, Canvas, ClientRect, gs);

      AFont.Free;
      bmp.Free;

      if not Assigned(Parent) then
        Exit;

      if FRounded and (not FTransparent or FMouseEnter or (State = absDown) or (FHot)) then
      begin
        R := drrect;

        if Position <> bpMiddle then
        begin
          if (Position in [bpStandalone, bpLeft]) then
          begin
            rgn1 := CreateRectRgn(0, 0, 1, 1);
          end
          else
          begin
            rgn1 := CreateRectRgn(R.Right - 1, 0, R.Right, 1);
          end;

          if (Position in [bpStandalone]) then
          begin
            rgn2 := CreateRectRgn(R.Right - 1, 0, R.Right, 1);
            CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
            DeleteObject(rgn2);
          end;

          if (Position in [bpStandalone, bpLeft]) then
          begin
            rgn2 := CreateRectRgn(0, R.Bottom - 1, 1, R.Bottom);
            CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
            DeleteObject(rgn2);
          end;

          if (Position in [bpStandalone, bpRight]) then
          begin
            rgn2 := CreateRectRgn(R.Right - 1, R.Bottom - 1, R.Right, R.Bottom);
            CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
            DeleteObject(rgn2);
          end;


          SelectClipRgn(Canvas.Handle, rgn1);

          i := SaveDC(Canvas.Handle);
          p := ClientOrigin;
          Windows.ScreenToClient(Parent.Handle, p);
          p.x := -p.x;
          p.y := -p.y;
          MoveWindowOrg(Canvas.Handle, p.x, p.y);

          SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);

          // transparency ?
          SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);
          if (Parent is TWinCtrl) then
            (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);
          RestoreDC(Canvas.Handle, i);

          SelectClipRgn(Canvas.Handle, 0);
          DeleteObject(rgn1);
        end;
      end;

    end;
  finally
  	FPainting := False;
  end;
end;

procedure TAdvCustomGlowButton.PictureChanged(Sender: TObject);
begin
  PerformResize;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDown(Value: Boolean);
begin
  if (csLoading in ComponentState) or (not HandleAllocated) then
    FInitialDown := Value;

  if (FGroupIndex = 0) and (Style = bsButton) then
    Value := False;

  if (Style = bsCheck) then
  begin
    FDownChecked := Value;
    if FDownChecked then
      FState := absDown
    else
      FState := absUp;
    Repaint;
    Exit;
  end;

  if (Value <> FDownChecked) then
  begin
    if FDownChecked and (not FAllowAllUp) then
      Exit;

    FDownChecked := Value;
    if Value then
    begin
      if FState = absUp then Invalidate;
      FState := absExclusive
    end
    else
    begin
      FState := absUp;
      Repaint;
    end;

    if Value {and not FCheckLinked} then UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetGroupIndex(const Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvCustomGlowButton.SetImageIndex(const Value: TImageIndex);
begin
  FImageIndex := Value;
  PerformResize;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FImageChangeLink);

    FImages := Value;

    if FImages <> nil then
    begin
      FImages.RegisterChanges(FImageChangeLink);
      FImages.FreeNotification(Self);
    end;

    PerformResize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetImageListType(const Value: TImageListType);
begin
  if Value <> FImageListType then
  begin
    FImageListType := Value;
    PerformResize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDefault(const Value: boolean);
begin
  FDefault := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDisabledImages(const Value: TCustomImageList);
begin
  if Value <> FDisabledImages then
  begin
    if FDisabledImages <> nil then
      FDisabledImages.UnRegisterChanges(FDisabledImageChangeLink);

    FDisabledImages := Value;

    if FDisabledImages <> nil then
    begin
      FDisabledImages.RegisterChanges(FDisabledImageChangeLink);
      FDisabledImages.FreeNotification(Self);
    end;

    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvCustomGlowButton.SetWideCaption(const Value: widestring);
begin
  if (FWideCaption <> Value) then
  begin
    FWideCaption := Value;

    if AutoSize then
    begin
      DoAutoSize := true;
      if Assigned(Parent) and not (csDesigning in ComponentState) and not(csLoading in ComponentState) and HandleAllocated then
        Paint
      else
        Repaint;

      DoAutoSize := false;
    end
    else
      Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.PerformResize;
begin
  if AutoSize then
  begin
    DoAutoSize := true;

    Repaint;

    if Assigned(Parent) and not (csDesigning in ComponentState) and not(csLoading in ComponentState) and HandleAllocated then    
      Paint;

    DoAutoSize := false;
  end
  else
    Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetRounded(const Value: Boolean);
begin
  if (FRounded <> Value) then
  begin
    FRounded := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetMarginVert(const Value: Integer);
begin
  if FMarginVert <> Value then
  begin
    FMarginVert := Value;
    PerformResize;
  end;
end;

procedure TAdvCustomGlowButton.SetMarginHorz(const Value: Integer);
begin
  if FMarginHorz <> Value then
  begin
    FMarginHorz := Value;
    PerformResize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetAutoSizeEx(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    PerformResize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDisabledPicture(const Value: TGDIPPicture);
begin
  FIDisabledPicture.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetHotImages(const Value: TCustomImageList);
begin
  if Value <> FHotImages then
  begin
    if FHotImages <> nil then
      FHotImages.UnRegisterChanges(FHotImageChangeLink);

    FHotImages := Value;

    if FHotImages <> nil then
    begin
      FHotImages.RegisterChanges(FHotImageChangeLink);
      FHotImages.FreeNotification(Self);
    end;

    Invalidate;
  end;
end;

procedure TAdvCustomGlowButton.SetHotPicture(const Value: TGDIPPicture);
begin
  FIHotPicture.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetShowCaption(const Value: Boolean);
begin
  FShowCaption := Value;
  PerformResize;
  Invalidate;
end;

procedure TAdvCustomGlowButton.SetShowDisabled(const Value: boolean);
begin
  FShowDisabled := Value;
  Invalidate;
end;

procedure TAdvCustomGlowButton.SetStyle(const Value: TAdvButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    //if (Value = bsCheck) and DropDownButton then
    //  DropDownButton := false;
  end;
end;

procedure TAdvCustomGlowButton.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.TimerProc(Sender: TObject);
begin
  case GlowState of
    gsHover:
      begin
        FStepHover := FStepHover + FTimeInc;
        if ((FStepHover > 100) and (FTimeInc > 0))
           or ((FStepHover < 0) and (FTimeInc < 0)) then
        begin
          if FStepHover > 100 then
            FStepHover := 100;

          if FStepHover < 0then
            FStepHover := 0;

          GlowState := gsNone;

          FreeAndNil(FTimer);
        end
        else
          Invalidate;
      end;
    gsPush:
      begin
        FStepPush := FStepPush + FTimeInc;

        if ((FStepPush > 100) and (FTimeInc > 0))
           or ((FStepPush < 0) and (FTimeInc < 0)) then
        begin
          if FStepPush > 100 then
            FStepPush := 100;

          if FStepPush < 0 then
            FStepPush := 0;

          if (FTimeInc < 0) and (Style = bsButton) then
          begin
            FDown := false;
            FLeftDown := false;
            Perform(CM_MOUSELEAVE, 0, 0);
          end;

          GlowState := gsNone;
          FreeAndNil(FTimer);
        end
        else
          Invalidate;
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMSetText(var Message: TWMSetText);
begin
  inherited;

  if AutoSize then
  begin
    PerformResize;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
const
  delta = 3;
{
var
  DC: HDC;
  i: THandle;
//  rgn1,rgn2: THandle;
  p,op: TPoint;
  PDC : HDC;
}

begin
  // SetBkMode(Message.DC, Windows.TRANSPARENT );
  Message.Result := 1;
  Exit;

  if FTransparent then
  begin
    if Assigned(Parent) and not (FMouseDown or FMouseInControl) then
    begin
      {
      rgn1 := CreateRectRgn(0, 0, delta, delta);
      rgn2 := CreateRectRgn(ClientRect.Right-delta, 0, ClientRect.Right, delta);
      CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
      rgn2 := CreateRectRgn(0, ClientRect.Bottom - delta, delta, ClientRect.Bottom);
      CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
      rgn2 := CreateRectRgn(ClientRect.Right - delta, ClientRect.Bottom - delta, ClientRect.Right, ClientRect.Bottom);
      CombineRgn(rgn1, rgn1, rgn2, RGN_OR);
      SelectClipRgn(Message.DC, rgn1);
      }

      (*
      DC := Message.DC;
      i := SaveDC(DC);

      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.x := -p.x;
      p.y := -p.y;

//      MoveWindowOrg(DC, p.x, p.y);

//      SetMapMode(FBmp.Canvas.Handle,mm_isotropic);

      SetMapMode(FBmp.Canvas.Handle,mm_isotropic);
      SetViewPortOrgEx(FBmp.Canvas.Handle,p.x,p.y,@op);

      SendMessage(Parent.Handle, WM_ERASEBKGND, FBmp.Canvas.Handle, 0);
      SendMessage(Parent.Handle, WM_PAINT, FBmp.Canvas.Handle, 0);

//      if (Parent is TWinCtrl) then
//        (Parent as TWinCtrl).PaintCtrls(FBmp.Canvas.Handle, nil);

      SetViewPortOrgEx(FBmp.Canvas.Handle,op.x,op.y,nil);
      RestoreDC(DC, i);

      // SelectClipRgn(Message.DC, 0);
      // DeleteObject(rgn1);
    *)
    end;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.CMDialogChar(var Message: TCMDialogChar);
begin
  if (GetKeyState(VK_MENU) and $8000 = $8000) or (Parent.ClassName <> 'TAdvToolBar') then
  begin
    with Message do
    begin
      if CanFocus then
      begin

        if (Caption <> '') then
        begin
          if IsAccel(CharCode, Caption) and CanFocus then
          begin
            if IsMenuButton or (Assigned(DropDownMenu)) then
              DoDropDown
            else
              Click;
            Result := 1;
            Exit;
          end;
        end;

        if (WideCaption <> '') then
        begin
          if IsAccel(CharCode, WideCaption) and CanFocus then
          begin
            if IsMenuButton or (Assigned(DropDownMenu)) then
              DoDropDown
            else
              Click;
            Result := 1;
            Exit;
          end;
        end;

        if Hint <> '' then
        begin
          if IsAccel(CharCode, Hint) and CanFocus then
          begin
            if IsMenuButton or (Assigned(DropDownMenu)) then
              DoDropDown
            else
              Click;
            Result := 1;
            Exit;
          end;
        end;

        if OfficeHint.Title <> '' then
        begin
          if IsAccel(CharCode, OfficeHint.Title) and CanFocus then
          begin
            if IsMenuButton or (Assigned(DropDownMenu)) then
              DoDropDown
            else
              Click;
            Result := 1;
            Exit;
          end;
        end;

        inherited;

      end;
    end;
  end
  else
    inherited;
end;

procedure TAdvCustomGlowButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if
      (((CharCode = VK_RETURN) and FActive) or
      ((CharCode = VK_ESCAPE) and FCancel)) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      //Click;
      InternalClick;
      if (FGroupIndex = 0) then
        FState := absUp;
      Result := 1;
    end
    else
      inherited;
end;

procedure TAdvCustomGlowButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;


procedure TAdvCustomGlowButton.CMFocusChanged(var Message: TCMFocusChanged);
begin
  with Message do
    if Sender is TAdvCustomGlowButton then
      FActive := Sender = Self
    else
      FActive := FDefault;
  //SetButtonStyle(FActive);
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;

  // if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
   //  Appearance.SystemFont := false;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvGlowButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvGlowButton(Message.LParam);
    if (Sender <> Self) then
    begin
      if Sender.Down and FDownChecked then
      begin
        FDownChecked := False;
        FState := absUp;
        { if (Action is TCustomAction) then
           TCustomAction(Action).Checked := False; }
        Invalidate;
      end;
      //FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvCustomGlowButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LParam(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

//------------------------------------------------------------------------------


//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.UpdateTracking;
var
  P: TPoint;
  FNewMouseInControl: boolean;
begin
  //if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);

      FNewMouseInControl := not (FindDragTarget(P, True) = Self);

      if FNewMouseInControl <> FMouseInControl then
      begin
        FMouseInControl := FNewMouseInControl;
        if FMouseInControl then
          Perform(CM_MOUSELEAVE, 0, 0)
        else
          Perform(CM_MOUSEENTER, 0, 0);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetAllowAllUp(const Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetAlwaysShowAccel(const Value: boolean);
begin
  if (FAlwaysShowAccel <> Value) then
  begin
    FAlwaysShowAccel := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetAntiAlias(const Value: TAntiAlias);
begin
  if (FAntiAlias <> Value) then
  begin
    FAntiAlias := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetTrimming(const Value: TStringTrimming);
begin
  if (FTrimming <> Value) then
  begin
    FTrimming := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TAdvButtonState;
  FOldInButton: Boolean;
  FDropDownSectWidth: integer;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  FDropDownSectWidth := Round(12 * GB_GetDPIScale);

  {$IFNDEF DELPHI2006_LVL}
  UpdateTracking;
  {$ENDIF}

  FOldInButton := FInButton;
  FInButton := false;
//  FMouseInControl := true;

  if DropDownButton then
  begin
    case DropDownPosition of
    dpRight: if X >= Width - FDropDownSectWidth then FInButton := true;
    dpBottom: if Y >= Height - FDropDownSectWidth then FInButton := true;
    end;
  end;

  if (FInButton <> FOldInButton) then
  begin
    Invalidate;
  end;

  if FDragging then
  begin
    if (not FDownChecked) then NewState := absUp
    else NewState := absExclusive;

    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDownChecked then NewState := absExclusive else NewState := absDown;

    if (Style = bsCheck) and FDownChecked then
    begin
      NewState := absDown;
    end;

    if (NewState <> FState) then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else
    if not FMouseInControl then
      UpdateTracking;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetTransparent(const Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDropDownButton(const Value: Boolean);
begin
  if FDropDownButton <> Value then
  begin
    //if (Value and not (Style = bsCheck)) or not Value then
    FDropDownButton := Value;
    AdjustSize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDropDownDirection(const Value: TDropDownDirection);
begin
  if FDropDownDirection <> Value then
  begin
    //if (Value and not (Style = bsCheck)) or not Value then
    FDropDownDirection := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.PopupBtnDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetDropDownPosition(
  const Value: TDropDownPosition);
begin
  if FDropDownPosition <> Value then
  begin
    FDropDownPosition := Value;
    if FDropDownButton then
      AdjustSize;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.OnAppearanceChanged(Sender: TObject);
begin
  Invalidate;
  if Assigned(FShortCutHint) then
  begin
    FShortCutHint.Color := clWhite;
    FShortCutHint.ColorTo := Appearance.Color;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetAppearance(
  const Value: TGlowButtonAppearance);
begin
  FAppearance.Assign(Value);
  if Assigned(FShortCutHint) then
  begin
    FShortCutHint.Color := clWhite;
    FShortCutHint.ColorTo := Appearance.Color;
  end;
end;

procedure TAdvCustomGlowButton.SetBorderStyle(const Value: TBorderStyle);
begin
  FBorderStyle := Value;
  Invalidate;
end;

procedure TAdvCustomGlowButton.SetButtonPosition(const Value: TButtonPosition);
begin
  FButtonPosition := Value;
  Invalidate;
end;

procedure TAdvCustomGlowButton.SetColorTones(ATones: TColorTones);
begin
  Appearance.Color := ATones.Background.BrushColor;
  Appearance.ColorTo := clNone;
  Appearance.BorderColor := ATones.Background.BorderColor;
  Appearance.ColorMirror := ATones.Background.BrushColor;
  Appearance.ColorMirrorTo := clNone;
  Appearance.ColorHot := ATones.Hover.BrushColor;
  Appearance.ColorHotTo := clNone;
  Appearance.ColorMirrorHot := ATones.Hover.BrushColor;
  Appearance.ColorMirrorHotTo := clNone;
  Appearance.BorderColorHot := ATones.Hover.BorderColor;
  Appearance.TextColorHot := ATones.Hover.TextColor;
  Appearance.TextColorDown := ATones.Selected.TextColor;
  Appearance.ColorDown := ATones.Selected.BrushColor;
  Appearance.ColorDownTo := clNone;
  Appearance.ColorMirrorDown := ATones.Selected.BrushColor;
  Appearance.ColorMirrorDownTo := clNone;
  Appearance.BorderColorDown := ATones.Selected.BorderColor;

  Appearance.ColorDisabled := ATones.Background.BrushColor;
  Appearance.ColorDisabledTo := ATones.Background.BrushColor;
  Appearance.ColorMirrorDisabled := ATones.Background.BrushColor;
  Appearance.ColorMirrorDisabledTo := ATones.Background.BrushColor;
  Appearance.BorderColorDisabled := ATones.Disabled.TextColor;

  Appearance.ColorChecked := ATones.Selected.BrushColor;
  Appearance.ColorCheckedTo := clNone;
  Appearance.ColorMirrorChecked := clNone;
  Appearance.ColorMirrorCheckedTo := clNone;
  Appearance.BorderColorChecked := ATones.Selected.BorderColor;

  GlowSpeed := 0;
  Rounded := false;
  Font.Color := ATones.Background.TextColor;
  Font.Name := GetMetroFont;
end;

procedure TAdvCustomGlowButton.SetComponentStyle(AStyle: TTMSStyle);
begin
  Font.Color := clWindowText;
  Appearance.TextColorChecked := clBlack;
  Appearance.TextColorHot := clBlack;
  Appearance.TextColorDown := clBlack;

  Appearance.TranspHover := (AStyle in [tsOffice2010Blue, tsOffice2010Silver, tsOffice2010Black, tsOffice2013White, tsOffice2013LightGray, tsOffice2013Gray, tsOffice2016White, tsOffice2016Gray, tsOffice2016Black]);

  FCompStyle := AStyle;

  Appearance.BorderColorCheckedHot := clNone;

  Rounded := not (AStyle in [tsCustom, tsWindows8, tsOffice2013White, tsOffice2013LightGray, tsOffice2013Gray, tsWindows10,
  tsOffice2016White, tsOffice2016Gray, tsOffice2016Black]);

  if (AStyle in [tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsWhidbey]) then
  begin
    Appearance.ColorHot := $EBFDFF;
    Appearance.ColorHotTo := $ACECFF;
    Appearance.ColorMirrorHot := $59DAFF;
    Appearance.ColorMirrorHotTo := $A4E9FF;
    Appearance.BorderColorHot := $99CEDB;
    Appearance.GradientHot := ggVertical;
    Appearance.GradientMirrorHot := ggVertical;

    Appearance.ColorDown := $76AFF1;
    Appearance.ColorDownTo := $4190F3;
    Appearance.ColorMirrorDown := $0E72F1;
    Appearance.ColorMirrorDownTo := $4C9FFD;
    Appearance.BorderColorDown := $45667B;
    Appearance.GradientDown := ggVertical;
    Appearance.GradientMirrorDown := ggVertical;

    Appearance.ColorChecked := $B5DBFB;
    Appearance.ColorCheckedTo := $78C7FE;
    Appearance.ColorMirrorChecked := $9FEBFD;
    Appearance.ColorMirrorCheckedTo := $56B4FE;
    Appearance.GradientChecked := ggVertical;
    Appearance.GradientMirrorChecked := ggVertical;
  end;

  case AStyle of
    tsOffice2003Blue:
      begin
        Appearance.Color := $EEDBC8;
        Appearance.ColorTo := $F6DDC9;
        Appearance.ColorMirror := $EDD4C0;
        Appearance.ColorMirrorTo := $F7E1D0;
        Appearance.BorderColor := $E0B99B;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;
      end;
    tsOffice2003Olive:
      begin
        Appearance.Color := $CFF0EA;
        Appearance.ColorTo := $CFF0EA;
        Appearance.ColorMirror := $CFF0EA;
        Appearance.ColorMirrorTo := $8CC0B1;
        Appearance.BorderColor := $8CC0B1;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;
      end;
    tsOffice2003Silver:
      begin
        Appearance.Color := $E6E9E2; //$EDD4C0;
        Appearance.ColorTo := $00E6D8D8;
        Appearance.ColorMirror := $E6E9E2; //$EDD4C0;
        Appearance.ColorMirrorTo := $C8B2B3;
        Appearance.BorderColor := $927476;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;
      end;    
    tsOffice2003Classic:
      begin
        Appearance.Color := clWhite;
        Appearance.ColorTo := $C9D1D5;
        Appearance.ColorMirror := clWhite;
        Appearance.ColorMirrorTo := $C9D1D5;
        Appearance.BorderColor := clBlack;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2007Luna:
      begin
        Appearance.Color := $EEDBC8;
        Appearance.ColorTo := $F6DDC9;
        Appearance.ColorMirror := $EDD4C0;
        Appearance.ColorMirrorTo := $F7E1D0;
        Appearance.BorderColor := $E0B99B;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.BorderColorChecked := $45667B;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;
      end;
    tsOffice2007Obsidian:
      begin
        Appearance.Color := $DFDED6;
        Appearance.ColorTo := $E4E2DB;
        Appearance.ColorMirror := $D7D5CE;
        Appearance.ColorMirrorTo := $E7E5E0;
        Appearance.BorderColor := $C0BCB2;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.BorderColorChecked := $45667B;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

      end;
    tsOffice2007Silver:
      begin
        Appearance.Color := $F3F3F1;
        Appearance.ColorTo := $F5F5F3;
        Appearance.ColorMirror := $EEEAE7;
        Appearance.ColorMirrorTo := $F8F7F6;
        Appearance.BorderColor := $CCCAC9;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EBFDFF;
        Appearance.ColorHotTo := $ACECFF;
        Appearance.ColorMirrorHot := $59DAFF;
        Appearance.ColorMirrorHotTo := $A4E9FF;
        Appearance.BorderColorHot := $99CEDB;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $76AFF1;
        Appearance.ColorDownTo := $4190F3;
        Appearance.ColorMirrorDown := $0E72F1;
        Appearance.ColorMirrorDownTo := $4C9FFD;
        Appearance.BorderColorDown := $45667B;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B5DBFB;
        Appearance.ColorCheckedTo := $78C7FE;
        Appearance.ColorMirrorChecked := $9FEBFD;
        Appearance.ColorMirrorCheckedTo := $56B4FE;
        Appearance.BorderColorChecked := $45667B;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;
      end;
    tsWindowsXP:
      begin
        Appearance.Color := clWhite;
        Appearance.ColorTo := $B9D8DC;
        Appearance.ColorMirror := $B9D8DC;
        Appearance.ColorMirrorTo := $B9D8DC;
        Appearance.BorderColor := $B9D8DC;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $EFD3C6;
        Appearance.ColorHotTo := $EFD3C6;
        Appearance.ColorMirrorHot := $EFD3C6;
        Appearance.ColorMirrorHotTo := $EFD3C6;
        Appearance.BorderColorHot := clHighlight;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $B59284;
        Appearance.ColorDownTo := $B59284;
        Appearance.ColorMirrorDown := $B59284;
        Appearance.ColorMirrorDownTo := $B59284;
        Appearance.BorderColorDown := clHighlight;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $B9D8DC;
        Appearance.ColorCheckedTo := $B9D8DC;
        Appearance.ColorMirrorChecked := $B9D8DC;
        Appearance.ColorMirrorCheckedTo := $B9D8DC;
        Appearance.BorderColorChecked := clBlack;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;
      end;
    tsWhidbey:
      begin
        Appearance.Color := clWhite;
        Appearance.ColorTo := $DFEDF0;
        Appearance.ColorMirror := $DFEDF0;
        Appearance.ColorMirrorTo := $DFEDF0;
        Appearance.BorderColor := $99A8AC;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

      end;
      tsWindowsVista:
      begin
        Appearance.Color := $FDF8F1;
        Appearance.ColorTo := $FDF8F1;
        Appearance.ColorMirror := $FCEFD5;
        Appearance.ColorMirrorTo := $FDF8F1;
        Appearance.BorderColor := $FDDE99;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := clNone;
        Appearance.ColorHotTo := $FFFAF0;
        Appearance.ColorMirrorHot := $FFFAF0;
        Appearance.ColorMirrorHotTo := $FFFAF0;
        Appearance.BorderColorHot := $FCF2DA;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $FCEFD5;
        Appearance.ColorDownTo := $FCEFD5;
        Appearance.ColorMirrorDown := $FDF4E3;
        Appearance.ColorMirrorDownTo := $FDF4E3;
        Appearance.BorderColorDown := $FEDF9A;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $FCEFD5;
        Appearance.ColorCheckedTo := $FAE9C6;
        Appearance.ColorMirrorChecked := $F7DAA2;
        Appearance.ColorMirrorCheckedTo := $FBEDD3;
        Appearance.BorderColorChecked := $FEDF9A;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;
      end;
      tsWindows7:
      begin
        Appearance.Color := $FCEBDC;
        Appearance.ColorTo := $FCDBC1;
        Appearance.ColorMirror := $FCDBC1;
        Appearance.ColorMirrorTo := $FCDBC1;
        Appearance.BorderColor := $CEA27D;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $FDFBFA;
        Appearance.ColorHotTo := $FDF3EB;
        Appearance.ColorMirrorHot := $FDF3EB;
        Appearance.ColorMirrorHotTo := $FDFBFA;
        Appearance.BorderColorHot := $FBD6B8;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $FCEBDC;
        Appearance.ColorDownTo := $FCDBC1;
        Appearance.ColorMirrorDown := $FCDBC1;
        Appearance.ColorMirrorDownTo := $FCEBDC;
        Appearance.BorderColorDown := $CEA27D;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $FDFBFA;
        Appearance.ColorCheckedTo := $FDF3EB;
        Appearance.ColorMirrorChecked := $FCEBDC;
        Appearance.ColorMirrorCheckedTo := $FCEBDC;
        Appearance.BorderColorChecked := clHighlight;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;
      end;
      tsTerminal:
      begin
        Appearance.Color := clbtnFace;
        Appearance.ColorTo := clbtnFace;
        Appearance.ColorMirror := clbtnFace;
        Appearance.ColorMirrorTo := clbtnFace;


        Appearance.ColorHot := clSilver;
        Appearance.ColorHotTo := clSilver;
        Appearance.ColorMirrorHot := clSilver;
        Appearance.ColorMirrorHotTo := clSilver;
        Appearance.BorderColorHot := clGray;


        Appearance.ColorDown := clHighlight;
        Appearance.ColorDownTo := clHighlight;
        Appearance.ColorMirrorDown := clHighlight;
        Appearance.ColorMirrorDownTo := clHighlight;
        Appearance.BorderColorDown := clHighlight;

        Appearance.ColorChecked := clGray;
        Appearance.ColorCheckedTo := clGray;
        Appearance.ColorMirrorChecked := clGray;
        Appearance.ColorMirrorCheckedTo := clGray;
        Appearance.BorderColorChecked := clGray;
      end;
      tsOffice2010Blue:
      begin
        Appearance.Color := $FDF6EF;
        Appearance.ColorTo := $F0DAC7;
        Appearance.ColorMirror := $F0DAC7;
        Appearance.ColorMirrorTo := $F0DAC7;
        Appearance.BorderColor := $C7B29F;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $8AE3FD;
        Appearance.ColorHotTo := $D9F9FD;
        Appearance.ColorMirrorHot := $D9F9FD;
        Appearance.ColorMirrorHotTo := $8AE3FD;
        Appearance.BorderColorHot := $58CAF1;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $6CD0FF;
        Appearance.ColorDownTo := $7BEEFF;
        Appearance.ColorMirrorDown := $7BEEFF;
        Appearance.ColorMirrorDownTo := $6CD0FF;
        Appearance.BorderColorDown := $308AC2;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $D9F9FD;
        Appearance.ColorCheckedTo := $6CD0FF;
        Appearance.ColorMirrorChecked := $6CD0FF;
        Appearance.ColorMirrorCheckedTo := $6CD0FF;
        Appearance.BorderColorChecked := $308AC2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

      end;
      tsOffice2010Silver:
      begin
        Appearance.Color := $FFFFFF;
        Appearance.ColorTo := $EDE5E0;
        Appearance.ColorMirror := $EDE5E0;
        Appearance.ColorMirrorTo := $EDE5E0;
        Appearance.BorderColor := $D2CDC8;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $8AE3FD;
        Appearance.ColorHotTo := $D9F9FD;
        Appearance.ColorMirrorHot := $D9F9FD;
        Appearance.ColorMirrorHotTo := $8AE3FD;
        Appearance.BorderColorHot := $58CAF1;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $6CD0FF;
        Appearance.ColorDownTo := $7BEEFF;
        Appearance.ColorMirrorDown := $7BEEFF;
        Appearance.ColorMirrorDownTo := $6CD0FF;
        Appearance.BorderColorDown := $308AC2;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $D9F9FD;
        Appearance.ColorCheckedTo := $6CD0FF;
        Appearance.ColorMirrorChecked := $6CD0FF;
        Appearance.ColorMirrorCheckedTo := $6CD0FF;
        Appearance.BorderColorChecked := $308AC2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;
      end;
      tsOffice2010Black:
      begin
        Appearance.Color := $BFBFBF;
        Appearance.ColorTo := $919191;
        Appearance.ColorMirror := $919191;
        Appearance.ColorMirrorTo := $919191;
        Appearance.BorderColor := $6D6D6D;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $8AE3FD;
        Appearance.ColorHotTo := $D9F9FD;
        Appearance.ColorMirrorHot := $D9F9FD;
        Appearance.ColorMirrorHotTo := $8AE3FD;
        Appearance.BorderColorHot := $58CAF1;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $6CD0FF;
        Appearance.ColorDownTo := $7BEEFF;
        Appearance.ColorMirrorDown := $7BEEFF;
        Appearance.ColorMirrorDownTo := $6CD0FF;
        Appearance.BorderColorDown := $308AC2;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $D9F9FD;
        Appearance.ColorCheckedTo := $6CD0FF;
        Appearance.ColorMirrorChecked := $6CD0FF;
        Appearance.ColorMirrorCheckedTo := $6CD0FF;
        Appearance.BorderColorChecked := $308AC2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

      end;

      tsWindows8, tsWindows10:
      begin
        Appearance.Color := $F7F6F5;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := $F7F6F5;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $E4E3E2;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $F7EFE8;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $F7EFE8;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $F9CEA4;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $F7E0C9;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $F7E0C9;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E4A262;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $DAA026;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $DAA026; //$F6E8CB;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $DAA026;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $F7F7F7;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $F7F7F7;
        Appearance.ColorMirrorDisabledTo := clNone;

        {
        Appearance.ColorDisabled := Appearance.Color;
        Appearance.ColorDisabledTo := Appearance.ColorTo;
        Appearance.ColorMirrorDisabled := Appearance.ColorMirror;
        Appearance.ColorMirrorDisabledTo := Appearance.ColorMirrorTo;
        Appearance.BorderColorDisabled := Appearance.Color;
        }

        Appearance.TextColorChecked := clWhite;
      end;

      tsOffice2013White:
      begin
        Appearance.Color := clWhite;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := clWhite;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $D4D4D4;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $FCF0E4;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $FCF0E4;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $EAB47E;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $FCE2C8;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $FCE2C8;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E59D56;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $FF9933;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $FF9933;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $FF9933;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;


        Appearance.ColorDisabled := $EEEEEE;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $EEEEEE;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $ACACAC;

        Appearance.TextColorChecked := clWhite;
      end;

      tsOffice2013LightGray:
      begin
        Appearance.Color := $F6F6F6;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := $F6F6F6;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $C6C6C6;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $FCF0E4;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $FCF0E4;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $EAB47E;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $FCE2C8;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $FCE2C8;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E59D56;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $FF9933;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $FF9933;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $FF9933;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $EEEEEE;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $EEEEEE;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $ACACAC;

        Appearance.TextColorChecked := clWhite;
      end;

      tsOffice2013Gray:
      begin
        Appearance.Color := $E5E5E5;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := $E5E5E5;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $ABABAB;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $FCF0E4;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $FCF0E4;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $EAB47E;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $FCE2C8;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $FCE2C8;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E59D56;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $FF9933;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $FF9933;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $FF9933;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $EEEEEE;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $EEEEEE;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $ACACAC;

        Appearance.TextColorChecked := clWhite;
      end;

      tsOffice2016White:
      begin

        Appearance.Color := clWhite;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := clWhite;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $D4D4D4;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $F2E1D5;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $F2E1D5;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $F2E1D5;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $E3BDA3;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $E3BDA3;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E3BDA3;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $F2D5C2;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $F2D5C2;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $F2D5C2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := clWhite;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := clWhite;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $D4D4D4;

        Appearance.BorderColorCheckedHot := $E3BDA3;

        Font.Color := $505050;
        Appearance.TextColorChecked := $505050;
        Appearance.TextColorHot := $505050;
        Appearance.TextColorDown := $505050;

      end;

      tsOffice2016Gray:
      Begin
        Appearance.Color := $B2B2B2;
        Appearance.ColorTo := clNone;
        Appearance.ColorMirror := $B2B2B2;
        Appearance.ColorMirrorTo := clNone;
        Appearance.BorderColor := $444444;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $F2E1D5;
        Appearance.ColorHotTo := clNone;
        Appearance.ColorMirrorHot := $F2E1D5;
        Appearance.ColorMirrorHotTo := clNone;
        Appearance.BorderColorHot := $F2E1D5;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $E3BDA3;
        Appearance.ColorDownTo := clNone;
        Appearance.ColorMirrorDown := $E3BDA3;
        Appearance.ColorMirrorDownTo := clNone;
        Appearance.BorderColorDown := $E3BDA3;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $F2D5C2;
        Appearance.ColorCheckedTo := clNone;
        Appearance.ColorMirrorChecked := $F2D5C2;
        Appearance.ColorMirrorCheckedTo := clNone;
        Appearance.BorderColorChecked := $F2D5C2;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $B2B2B2;
        Appearance.ColorDisabledTo := clNone;
        Appearance.ColorMirrorDisabled := $B2B2B2;
        Appearance.ColorMirrorDisabledTo := clNone;
        Appearance.BorderColorDisabled := $444444;

        Appearance.BorderColorCheckedHot := $E3BDA3;

        Font.Color := $424242;
        Appearance.TextColorChecked := $424242;
        Appearance.TextColorHot := $424242;
        Appearance.TextColorDown := $424242;
      End;

      tsOffice2016Black:
      Begin

        Appearance.Color := $363636;
        Appearance.ColorTo := $363636;
        Appearance.ColorMirror := $363636;
        Appearance.ColorMirrorTo := $363636;
        Appearance.BorderColor := $444444;
        Appearance.Gradient := ggVertical;
        Appearance.GradientMirror := ggVertical;

        Appearance.ColorHot := $6A6A6A;
        Appearance.ColorHotTo := $6A6A6A;
        Appearance.ColorMirrorHot := $6A6A6A;
        Appearance.ColorMirrorHotTo := $6A6A6A;
        Appearance.BorderColorHot := $6A6A6A;
        Appearance.GradientHot := ggVertical;
        Appearance.GradientMirrorHot := ggVertical;

        Appearance.ColorDown := $444444;
        Appearance.ColorDownTo := $444444;
        Appearance.ColorMirrorDown := $444444;
        Appearance.ColorMirrorDownTo := $444444;
        Appearance.BorderColorDown := $444444;
        Appearance.GradientDown := ggVertical;
        Appearance.GradientMirrorDown := ggVertical;

        Appearance.ColorChecked := $575757;
        Appearance.ColorCheckedTo := $575757;
        Appearance.ColorMirrorChecked := $575757;
        Appearance.ColorMirrorCheckedTo := $575757;
        Appearance.BorderColorChecked := $575757;
        Appearance.GradientChecked := ggVertical;
        Appearance.GradientMirrorChecked := ggVertical;

        Appearance.ColorDisabled := $363636;
        Appearance.ColorDisabledTo := clnone;
        Appearance.ColorMirrorDisabled := $363636;
        Appearance.ColorMirrorDisabledTo := clnone;
        Appearance.BorderColorDisabled := $444444;

        Font.Color := $A6A6A6;
        Appearance.TextColorChecked := $C8C8C8;
        Appearance.TextColorHot := $A6A6A6;
        Appearance.TextColorDown := $A6A6A6;
      End;


    tsCustom:
      begin
      end;
  end;

  Invalidate;

  if Assigned(FShortCutHint) then
  begin
    FShortCutHint.Color := clWhite;
    FShortCutHint.ColorTo := Appearance.Color;
  end;
end;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      if (csDesigning in ComponentState)
      {$IFDEF DELPHI2006_LVL}
         or not self.StaticActionImageIndex
      {$ENDIF}
      then
      begin
        if ActionHasImages then
          Self.ImageIndex := ImageIndex;

        Self.Down := Checked;
      end;
    end;

  if AutoSize and Assigned(Parent) and HandleAllocated then
  begin
    DoAutoSize := true;
    Paint;
    DoAutoSize := false;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGlowButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvGlowButtonActionLink;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetButtonSizeState(
  const Value: TButtonSizeState);
begin
  if (FButtonSizeState <> Value) {and AutoSize} then
  begin
    if (FButtonSizeState = bsLarge) then
    begin
      FOldLayout := Layout;
      FOldDropDownPosition := DropDownPosition;
    end;

    FButtonSizeState := Value;
    
    if (FButtonSizeState = bsLarge) and AutoSize then
    begin
      Layout := FOldLayout;
      DropDownPosition := FOldDropDownPosition;
    end
    else if AutoSize then
    begin
      Layout := blGlyphLeft;
      DropDownPosition := dpRight;
    end;
    FFirstPaint := True;
    Paint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetMaxButtonSizeState(
  const Value: TButtonSizeState);
begin
  if (FMaxButtonSizeState <> Value) {and AutoSize} then
  begin
    FMaxButtonSizeState := Value;
    ButtonSizeState := FMaxButtonSizeState;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGlowButton.GetNotes: TStrings;
begin
  Result := TStrings(FNotes);
end;


//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetNotes(const Value: TStrings);
begin
  FNotes.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetNotesFont(const Value: TFont);
begin
  FNotesFont.Assign(Value);
  Invalidate;  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlowButton.SetMinButtonSizeState(
  const Value: TButtonSizeState);
begin
  if (FMinButtonSizeState <> Value) then
  begin
    FMinButtonSizeState := Value;
    if (FMinButtonSizeState > ButtonSizeState) then
      ButtonSizeState := FMinButtonSizeState;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGlowButton.GetButtonSize(BtnSizeState: TButtonSizeState): TSize;
var
  DCaption: string;
  DWideCaption: widestring;
  ImgList: TCustomImageList;
  Pic: TGDIPPicture;
  EnabledImg: Boolean;
  BD: TButtonDisplay;
  bmp: TBitmap;
  DrawFocused, DrawFocusedHot, DrawDwLn: boolean;
  PicSize: TSize;
  LayOt: TButtonLayout;
  DpDwPosition: TDropDownPosition;

begin
  if Enabled or (DisabledImages = nil) then
  begin
    if FHot and (HotImages <> nil) then
      ImgList := HotImages
    else
      ImgList := Images;

    EnabledImg := Enabled;
  end
  else
  begin
    ImgList := DisabledImages;
    EnabledImg := True;
  end;

  if Enabled or DisabledPicture.Empty then
  begin
    if FHot and not HotPicture.Empty then
      Pic := HotPicture
    else
      Pic := Picture;
  end
  else
    Pic := DisabledPicture;


  if (ImgList = nil) then
  begin
    ImgList := FInternalImages;
    EnabledImg := True;
  end;

  if ShowCaption then
  begin
    DCaption := Caption;
    DWideCaption := WideCaption;
  end
  else
  begin
    DCaption := '';
    DWideCaption := '';
  end;

  if (FMouseInControl or FMouseDown) and DropDownButton then
  begin
    if FInButton then
      BD := bdButton
    else
      BD := bdDropDown;
  end
  else
    BD := bdNone;

  DrawFocused := (FHasFocus) and (FocusType in [ftBorder, ftHotBorder]);
  DrawFocusedHot := (FHasFocus) and (FocusType in [ftHot, ftHotBorder]);

  bmp := TBitmap.Create;
  bmp.Width := 1;
  bmp.Height := 1;

  GetToolImage(bmp);

  if Assigned(Action) then
  begin
    begin
      if ((Action as TCustomAction).ImageIndex >= 0) and (ImageIndex = (Action as TCustomAction).ImageIndex) then
        if Assigned((Action as TCustomAction).ActionList) then
          if Assigned(TCustomImageList((Action as TCustomAction).ActionList.Images)) then
          begin
            ImgList := TCustomImageList((Action as TCustomAction).ActionList.Images);
            EnabledImg := Enabled;
          end;
    end;
  end;

  LayOt := Layout;
  DpDwPosition := DropDownPosition;

  PicSize.cx := 0;  // no stretch pic
  PicSize.cy := 0;
  if AutoSize then
  begin
    if (BtnSizeState in [bsLabel, bsGlyph]) then
    begin
      PicSize.cx := 16;
      PicSize.cy := 16;

      if (bmp.Width = 1) then
      begin
        bmp.Height := Pic.Height;
        bmp.Width := Pic.Width;
        bmp.Canvas.Draw(0, 0, Pic);
        Pic := nil;
      end;

      if Assigned(ImgList) and (ImageIndex >= 0) then
      begin
        Pic := nil;
      end;
    end;

    if (BtnSizeState = bsGlyph) then
    begin
      DCaption := '';
      DWideCaption := '';
    end;

    if (BtnSizeState = bsLarge) then
    begin
      LayOt := FOldLayout;
      DpDwPosition := FOldDropDownPosition;
    end
    else
    begin
      LayOt := blGlyphLeft;
      DpDwPosition := dpRight;
    end;
  end;

  DrawDwLn := False;

  with Appearance do
    Result := DrawVistaButton(Self, Canvas,ClientRect,FColor, FColorTo, FColorMirror, FColorMirrorTo,
        BorderColor, BorderColorFocused, Gradient, GradientMirror, DCaption, DWideCaption, FDefaultCaptionDrawing, Font, ImgList, FDisabledImages, ImageIndex, EnabledImg, LayOt, FDropDownButton,
        DrawDwLn, Enabled, DrawFocused, DpDwPosition, Pic, FIcon, PicSize, AntiAlias, FDefaultPicDrawing, bmp, BD, FStaticButton or Transparent and not (FMouseEnter or DrawFocusedHot or (State = absDown)), FMouseEnter, Position, DropDownSplit, CanDrawBorder,
        FOverlappedText, FWordWrap, True, FRounded, FDropDownDirection = ddDown, FSpacing, FTrimming, FNotes, FNotesFont, FDownChecked, FDroppedDown, TranspHover and Transparent, InternalIsOnGlassQAT,MarginHorz,MarginVert,NoBorderDefault, AlwaysShowAccel);

  Result.cx := Result.cx + Spacing * 3 + 2 + 2 * MarginHorz;
  Result.cy := Result.cy + Spacing * 2 + 2 * MarginVert;

  if DropDownButton then
  begin
    if (DpDwPosition = dpBottom) then
      Result.cy := Result.cy + DropDownSectWidth
    else
      Result.cx := Result.cx + DropDownSectWidth;
  end;
  //if Assigned(FOnSetButtonSize) then
    //FOnSetButtonSize(Self, w, h);

  bmp.Free;
end;

//------------------------------------------------------------------------------

{ TGlowButtonAppearance }

constructor TGlowButtonAppearance.Create;
begin
  inherited Create;

  Color := clWhite;
  ColorTo := clWhite;
  ColorMirror := clSilver;
  ColorMirrorTo := clWhite;

  ColorHot := $F5F0E1;
  ColorHotTo := $F9D2B2;
  ColorMirrorHot := $F5C8AD;
  ColorMirrorHotTo := $FFF8F4;

  ColorDown := BrightnessColor($F5F0E1,-10,-10,0);
  ColorDownTo := BrightnessColor($F9D2B2, -10,-10,0);
  ColorMirrorDown := BrightnessColor($F5C8AD, -10,-10,0);
  ColorMirrorDownTo := BrightnessColor($FFF8F4, -10,-10,0);

  ColorChecked := BrightnessColor($F5F0E1,-10,-10,0);
  ColorCheckedTo := BrightnessColor($F9D2B2, -10,-10,0);
  ColorMirrorChecked := BrightnessColor($F5C8AD, -10,-10,0);
  ColorMirrorCheckedTo := BrightnessColor($FFF8F4, -10,-10,0);

  ColorDisabled := BrightnessColor(clWhite,-5,-5,-5);
  ColorDisabledTo := BrightnessColor(clWhite, -5,-5,-5);
  ColorMirrorDisabled := BrightnessColor(clSilver, -5,-5,-5);
  ColorMirrorDisabledTo := BrightnessColor(clWhite, -5,-5,-5);

  BorderColor := clSilver;
  BorderColorHot := clBlue;
  BorderColorDown := clNavy;
  BorderColorChecked := clBlue;
  BorderColorDisabled := clGray;
  BorderColorFocused := $E4AD89;
  BorderColorCheckedHot := clNone;

  Gradient := ggVertical;
  GradientMirror := ggVertical;

  GradientHot := ggRadial;
  GradientMirrorHot := ggRadial;

  GradientDown := ggRadial;
  GradientMirrorDown := ggRadial;

  GradientChecked := ggRadial;
  GradientMirrorChecked := ggVertical;

  GradientDisabled := ggRadial;
  GradientMirrorDisabled := ggRadial;

  TextColor := clNone;
  TextColorHot := clBlack;
  TextColorDown := clBlack;
  TextColorChecked := clBlack;
  FSystemFont := true;
  FNoBorderDefault := false;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetSystemFont(const Value: boolean);
begin
  if (FSystemFont <> Value) then
  begin
    FSystemFont := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

procedure TGlowButtonAppearance.SetTextColorChecked(const Value: TColor);
begin
  if (FTextColorChecked <> Value) then
  begin
    FTextColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetTextColorDown(const Value: TColor);
begin
  if (FTextColorDown <> Value) then
  begin
    FTextColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetTextColorHot(const Value: TColor);
begin
  if (FTextColorHot <> Value) then
  begin
    FTextColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TGlowButtonAppearance) then
  begin
    Color := (Source as TGlowButtonAppearance).Color;
    ColorTo := (Source as TGlowButtonAppearance).ColorTo;
    ColorMirror := (Source as TGlowButtonAppearance).ColorMirror;
    ColorMirrorTo := (Source as TGlowButtonAppearance).ColorMirrorTo;

    ColorHot := (Source as TGlowButtonAppearance).ColorHot;
    ColorHotTo := (Source as TGlowButtonAppearance).ColorHotTo;
    ColorMirrorHot := (Source as TGlowButtonAppearance).ColorMirrorHot;
    ColorMirrorHotTo := (Source as TGlowButtonAppearance).ColorMirrorHotTo;

    ColorDown := (Source as TGlowButtonAppearance).ColorDown;
    ColorDownTo := (Source as TGlowButtonAppearance).ColorDownTo;
    ColorMirrorDown := (Source as TGlowButtonAppearance).ColorMirrorDown;
    ColorMirrorDownTo := (Source as TGlowButtonAppearance).ColorMirrorDownTo;

    ColorChecked := (Source as TGlowButtonAppearance).ColorChecked;
    ColorCheckedTo := (Source as TGlowButtonAppearance).ColorCheckedTo;
    ColorMirrorChecked := (Source as TGlowButtonAppearance).ColorMirrorChecked;
    ColorMirrorCheckedTo := (Source as TGlowButtonAppearance).ColorMirrorCheckedTo;

    ColorDisabled := (Source as TGlowButtonAppearance).ColorDisabled;
    ColorDisabledTo := (Source as TGlowButtonAppearance).ColorDisabledTo;
    ColorMirrorDisabled := (Source as TGlowButtonAppearance).ColorMirrorDisabled;
    ColorMirrorDisabledTo := (Source as TGlowButtonAppearance).ColorMirrorDisabledTo;

    BorderColor := (Source as TGlowButtonAppearance).BorderColor;
    BorderColorHot := (Source as TGlowButtonAppearance).BorderColorHot;
    BorderColorDown := (Source as TGlowButtonAppearance).BorderColorDown;
    BorderColorChecked := (Source as TGlowButtonAppearance).BorderColorChecked;
    BorderColorDisabled := (Source as TGlowButtonAppearance).BorderColorDisabled;
    BorderColorCheckedHot := (Source as TGlowButtonAppearance).BorderColorCheckedHot;

    Gradient := (Source as TGlowButtonAppearance).Gradient;
    GradientMirror := (Source as TGlowButtonAppearance).GradientMirror;

    GradientHot := (Source as TGlowButtonAppearance).GradientHot;
    GradientMirrorHot := (Source as TGlowButtonAppearance).GradientMirrorHot;

    GradientDown := (Source as TGlowButtonAppearance).GradientDown;
    GradientMirrorDown := (Source as TGlowButtonAppearance).GradientMirrorDown;

    GradientChecked := (Source as TGlowButtonAppearance).GradientChecked;
    GradientMirrorChecked := (Source as TGlowButtonAppearance).GradientMirrorChecked;

    GradientDisabled := (Source as TGlowButtonAppearance).GradientDisabled;
    GradientMirrorDisabled := (Source as TGlowButtonAppearance).GradientMirrorDisabled;

    TextColorHot := (Source as TGlowButtonAppearance).TextColorHot;
    TextColorDown := (Source as TGlowButtonAppearance).TextColorDown;
    TextColorChecked := (Source as TGlowButtonAppearance).TextColorChecked;

    TextColor := (Source as TGlowButtonAppearance).TextColor;

    SystemFont := (Source as TGlowButtonAppearance).SystemFont;

    TranspHover := (Source as TGlowButtonAppearance).TranspHover;
    NoBorderDefault := (Source as TGlowButtonAppearance).NoBorderDefault;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetBorderColorChecked(const Value: TColor);
begin
  if (FBorderColorChecked <> Value) then
  begin
    FBorderColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetBorderColorDisabled(
  const Value: TColor);
begin
  if (FBorderColorDisabled <> Value) then
  begin
    FBorderColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetBorderColorDown(const Value: TColor);
begin
  if (FBorderColorDown <> Value) then
  begin
    FBorderColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetBorderColorFocused(const Value: TColor);
begin
  if Value <> FBorderColorFocused then
  begin
    FBorderColorFocused := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetBorderColorHot(const Value: TColor);
begin
  if (FBorderColorHot <> Value) then
  begin
    FBorderColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;    
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorChecked(const Value: TColor);
begin
  if (FColorChecked <> Value) then
  begin
    FColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorCheckedTo(const Value: TColor);
begin
  if (FColorCheckedTo <> Value) then
  begin
    FColorCheckedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorDisabled(const Value: TColor);
begin
  if (FColorDisabled <> Value) then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorDisabledTo(const Value: TColor);
begin
  if (FColorDisabledTo <> Value) then
  begin
    FColorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorDown(const Value: TColor);
begin
  if (FColorDown <> Value) then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorDownTo(const Value: TColor);
begin
  if (FColorDownTo <> Value) then
  begin
    FColorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorHot(const Value: TColor);
begin
  if (FColorHot <> Value) then
  begin
    FColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorHotTo(const Value: TColor);
begin
  if (FColorHotTo <> Value) then
  begin
    FColorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorChecked(const Value: TColor);
begin
  if (FColorMirrorChecked <> Value) then
  begin
    FColorMirrorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorCheckedTo(
  const Value: TColor);
begin
  if (FColorMirrorCheckedTo <> Value) then
  begin
    FColorMirrorCheckedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorDisabled(
  const Value: TColor);
begin
  if (FColorMirrorDisabled <> Value) then
  begin
    FColorMirrorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorDisabledTo(
  const Value: TColor);
begin
  if (FColorMirrorDisabledTo <> Value) then
  begin
    FColorMirrorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorDown(const Value: TColor);
begin
  if (FColorMirrorDown <> Value) then
  begin
    FColorMirrorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorDownTo(const Value: TColor);
begin
  if (FColorMirrorDownTo <> Value) then
  begin
    FColorMirrorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorHot(const Value: TColor);
begin
  if (FColorMirrorHot <> Value) then
  begin
    FColorMirrorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorHotTo(const Value: TColor);
begin
  if (FColorMirrorHotTo <> Value) then
  begin
    FColorMirrorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradient(const Value: TGDIPGradient);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientChecked(
  const Value: TGDIPGradient);
begin
  if (FGradientChecked <> Value) then
  begin
    FGradientChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientDisabled(
  const Value: TGDIPGradient);
begin
  if (FGradientDisabled <> Value) then
  begin
    FGradientDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientDown(
  const Value: TGDIPGradient);
begin
  if (FGradientDown <> Value) then
  begin
    FGradientDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientHot(const Value: TGDIPGradient);
begin
  if (FGradientHot <> Value) then
  begin
    FGradientHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientMirror(
  const Value: TGDIPGradient);
begin
  if (FGradientMirror <> Value) then
  begin
    FGradientMirror := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientMirrorChecked(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorChecked <> Value) then
  begin
    FGradientMirrorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientMirrorDisabled(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorDisabled <> Value) then
  begin
    FGradientMirrorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientMirrorDown(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorDown <> Value) then
  begin
    FGradientMirrorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TGlowButtonAppearance.SetGradientMirrorHot(
  const Value: TGDIPGradient);
begin
  if (FGradientMirrorHot <> Value) then
  begin
    FGradientMirrorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{$IFNDEF TMS_STD}

{ TDBATBButtonDataLink }

constructor TDBGlowButtonDataLink.Create;
begin
  inherited Create;
  FOnEditingChanged := nil;
  FOnDataSetChanged := nil;
  FOnActiveChanged := nil;
end;

//------------------------------------------------------------------------------

procedure TDBGlowButtonDataLink.ActiveChanged;
begin
  if Assigned(FOnActiveChanged) then FOnActiveChanged(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGlowButtonDataLink.DataSetChanged;
begin
  if Assigned(FOnDataSetChanged) then FOnDataSetChanged(Self);
end;

//------------------------------------------------------------------------------

procedure TDBGlowButtonDataLink.EditingChanged;
begin
  if Assigned(FOnEditingChanged) then FOnEditingChanged(Self);
end;

//------------------------------------------------------------------------------

{ TDBAdvToolBarButton }

constructor TDBAdvGlowButton.Create(AOwner: TComponent);
begin
  inherited;
  FAutoDisable := True;
  FDBButtonType := dbCustom;
  FDisableControls := [];
  FDataLink := TDBGlowButtonDataLink.Create;
  with FDataLink do
  begin
    OnEditingChanged := OnDataSetEvents;
    OnDataSetChanged := OnDataSetEvents;
    OnActiveChanged := OnDataSetEvents;
  end;
  FConfirmActionString := '';  
end;

//------------------------------------------------------------------------------

destructor TDBAdvGlowButton.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  if (FInternalImages <> nil) then
    FInternalImages.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.CalcDisableReasons;
begin
  case FDBButtonType of
    dbPrior:    FDisableControls := [drBOF, drEditing, drEmpty];
    dbNext:     FDisableControls := [drEOF, drEditing, drEmpty];
    dbFirst:    FDisableControls := [drBOF, drEditing, drEmpty];
    dbLast:     FDisableControls := [drEOF, drEditing, drEmpty];
    dbInsert,
    dbAppend:   FDisableControls := [drReadonly, drEditing];
    dbEdit:     FDisableControls := [drReadonly, drEditing, drEmpty];
    dbCancel:   FDisableControls := [drNotEditing];
    dbPost:     FDisableControls := [drNotEditing];
    dbRefresh:  FDisableControls := [drEditing];
    dbDelete:   FDisableControls := [drReadonly, drEditing, drEmpty];
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.Click;
begin
  inherited;
  DoAction;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if (not FInProcUpdateEnabled) and
     (not (csLoading in ComponentState)) and
     (not (csDestroying in ComponentState)) then
  begin
    UpdateEnabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.DoAction;
var
  DoAction: Boolean;
  ShowException: Boolean;
begin
  if not DoConfirmAction then
    Exit;

  DoAction := (FDBButtonType <> dbCustom);
  try
    DoBeforeAction(DoAction);
    if DoAction and (DataSource <> nil) and (DataSource.State <> dsInactive) then
    begin
      with DataSource.DataSet do
      begin
        case FDBButtonType of
          dbPrior: Prior;
          dbNext: Next;
          dbFirst: First;
          dbLast: Last;
          dbInsert: Insert;
          dbAppend: Append;
          dbEdit: Edit;
          dbCancel: Cancel;
          dbPost: Post;
          dbRefresh:Refresh;
          dbDelete: Delete;
        end;
      end;
    end;
    ShowException := false;
  except
    ShowException := true;
    if Assigned(FOnAfterAction) then
      FOnAfterAction(self, ShowException);
    if ShowException then
      raise;
    ShowException := true;
  end;
  if not ShowException and DoAction and Assigned(FOnAfterAction) then
    FOnAfterAction(self, ShowException);
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.DoBeforeAction(var DoAction: Boolean);
begin
  if (not (csDesigning in ComponentState)) and Assigned(FOnBeforeAction) then
    FOnBeforeAction(self, DoAction);
end;

//------------------------------------------------------------------------------

function TDBAdvGlowButton.DoConfirmAction: Boolean;
var
  Question: string;
  QuestionButtons: TMsgDlgButtons;
  QuestionHelpCtx: Longint;
  QuestionResult: Longint;
begin
  DoGetQuestion(Question, QuestionButtons, QuestionHelpCtx);
  if (Question <> '') then
  begin
    QuestionResult := MessageDlg(Question, mtConfirmation, QuestionButtons, QuestionHelpCtx);
    Result := (QuestionResult = idOk) or (QuestionResult = idYes);
  end
  else
    Result := true;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.DoGetQuestion(var Question: string;
  var Buttons: TMsgDlgButtons; var HelpCtx: Integer);
begin
  Question := '';
  if FConfirmAction then
  begin
    Question := FConfirmActionString;
    Buttons := mbOKCancel;
    HelpCtx := 0;
    if Assigned(FOnGetConfirm) then
      FOnGetConfirm(self, Question, Buttons, HelpCtx);
  end;
end;

//------------------------------------------------------------------------------

function TDBAdvGlowButton.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if not (csLoading in ComponentState) then
    UpdateEnabled;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.SetDBButtonType(const Value: TDBGlowButtonType);
begin
  if (Value = FDBButtonType) then
    Exit;

  if (Value = dbDelete) and (FConfirmActionString = ''){and ConfirmAction} then
    FConfirmActionString := SDeleteRecordQuestion; //'Delete Record?';

  if (csReading in ComponentState) or (csLoading in ComponentState) then
  begin
    FDBButtonType := Value;
    CalcDisableReasons;
    exit;
  end;

  FDBButtonType := Value;
  LoadGlyph;
  CalcDisableReasons;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.UpdateEnabled;
var
  PossibleDisableReasons: TDBBDisableControls;
  GetEnable: Boolean;
  WasEnabled: Boolean;
begin
  if (csDesigning in ComponentState) or (csDestroying in ComponentState) or not FAutoDisable then
    Exit;

  FInProcUpdateEnabled := true;
  try
   WasEnabled := Enabled;
   if FDataLink.Active then
   begin
     PossibleDisableReasons := [];
     if FDataLink.DataSet.BOF then
       Include(PossibleDisableReasons, drBOF);
     if FDataLink.DataSet.EOF then
       Include(PossibleDisableReasons, drEOF);
     if not FDataLink.DataSet.CanModify then
       Include(PossibleDisableReasons, drReadonly);
     if FDataLink.DataSet.BOF and FDataLink.DataSet.EOF then
       Include(PossibleDisableReasons, drEmpty);
     if FDataLink.Editing then
       Include(PossibleDisableReasons, drEditing)
     else
       Include(PossibleDisableReasons, drNotEditing);

     GetEnable := ((FDisableControls - [drEvent])* PossibleDisableReasons = []);
     if (drEvent in FDisableControls) and (Assigned(FOnGetEnabled)) then
       FOnGetEnabled(Self, GetEnable);
     Enabled := GetEnable;
   end
   else
     Enabled := false;

   if (WasEnabled <> Enabled) and Assigned(FOnEnabledChanged) then
     FOnEnabledChanged(self);
  finally
    FInProcUpdateEnabled := false;
  end;
  LoadGlyph;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.Loaded;
begin
  inherited;
  //if not Assigned(Images) then
  if ImageIndex <= 0 then
    LoadGlyph;

  UpdateEnabled;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.OnDataSetEvents(Sender: TObject);
begin
  UpdateEnabled;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.LoadGlyph;
var
  Glyph: TBitMap;
begin
  if (csLoading in ComponentState) or Assigned(Images) or (not Enabled and Assigned(DisabledImages)) then
    Exit;

  if (FDBButtonType = dbCustom) then
    Exit;

  if (FInternalImages = nil) then
    FInternalImages := TCustomImageList.Create(self);

  FInternalImages.Clear;
  Glyph := TBitMap.Create;
  Glyph.Width := 16;
  Glyph.Height := 16;
  Glyph.Transparent := True;

  case FDBButtonType of
    dbPrior:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGPRIOR')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGPRIORD');
    end;
    dbNext:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGNEXT')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGNEXTD');
    end;
    dbFirst:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGFIRST')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGFIRSTD');
    end;
    dbLast:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGLAST')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGLASTD');
    end;
    dbInsert:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGINSERT')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGINSERTD');
    end;
    dbAppend:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGINSERT')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGINSERTD');
    end;
    dbEdit:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGEDIT')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGEDITD');
    end;
    dbCancel:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGCANCEL')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGCANCELD');
    end;
    dbPost:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGPOST')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGPOSTD');
    end;
    dbRefresh:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGREFRESH')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGREFRESHD');
    end;
    dbDelete:
    begin
      if Enabled then
        Glyph.LoadFromResourceName(HInstance, 'DBIMGDELETE')
      else
        Glyph.LoadFromResourceName(HInstance, 'DBIMGDELETED');
    end;
  end;

  FInternalImages.DrawingStyle := dsTransparent;
  FInternalImages.Masked := true;
  FInternalImages.AddMasked(Glyph, clFuchsia);

  FImageIndex := 0;

  Glyph.Free;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TDBAdvGlowButton.SetConfirmActionString(const Value: String);
begin
  if FConfirmActionString <> Value then
  begin
    FConfirmActionString := Value;
  end;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{ TAdvGlowButtonActionLink }

procedure TAdvGlowButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvCustomGlowButton;
end;

//------------------------------------------------------------------------------

function TAdvGlowButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked {and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp} and (FClient.Down = (Action as TCustomAction).Checked);

  FClient.CheckLinked := Result;
end;

//------------------------------------------------------------------------------

function TAdvGlowButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAdvCustomGlowButton) and
    (TAdvCustomGlowButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);

  FClient.GroupIndexLinked := Result;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
  begin
    FImageIndex := Value;
    TAdvCustomGlowButton(FClient).Invalidate;

    if (csDesigning in FClient.ComponentState)
    {$IFDEF DELPHI2006_LVL}
      or not TAdvCustomGlowButton(FClient).StaticActionImageIndex
    {$ENDIF}
    then
    begin
      if TAdvCustomGlowButton(FClient).ActionHasImages then
        TAdvCustomGlowButton(FClient).ImageIndex := Value;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvGlowButtonActionLink.IsImageIndexLinked: boolean;
begin
  Result := inherited IsImageIndexLinked and
    (FImageIndex = (Action as TCustomAction).ImageIndex);
end;

//------------------------------------------------------------------------------

function TAdvGlowButtonActionLink.IsHelpLinked: Boolean;
begin
  Result :=
    (FClient.HelpContext = (Action as TCustomAction).HelpContext) and
    (FClient.HelpKeyword = (Action as TCustomAction).HelpKeyword) and
    (FClient.HelpType = (Action as TCustomAction).HelpType);
end;

//------------------------------------------------------------------------------

procedure TAdvGlowButtonActionLink.SetHelpContext(Value: THelpContext);
begin
  if IsHelpLinked then FClient.HelpContext := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowButtonActionLink.SetHelpKeyword(const Value: string);
begin
  if IsHelpLinked then FClient.HelpKeyword := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowButtonActionLink.SetHelpType(Value: THelpType);
begin
  if IsHelpLinked then FClient.HelpType := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    TAdvCustomGlowButton(FClient).Down  := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvGlowButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then
    TAdvCustomGlowButton(FClient).GroupIndex := Value;
end;

{ TShortCutHintWindow }

procedure TShortCutHintWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  Params.Style := Params.Style and not WS_BORDER;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
     ((Win32MajorVersion > 5) or
      ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
        if Params.WindowClass.Style and CS_DROPSHADOW <> 0 then
          Params.WindowClass.Style := Params.WindowClass.Style - CS_DROPSHADOW;
  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

procedure TShortCutHintWindow.Paint;
var
  r: TRect;
begin
  r := ClientRect;
  DrawGradient(Canvas, Color, ColorTo, 16, r, false);
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(self.Font);

  DrawText(Canvas.Handle,PChar(Caption),Length(Caption),r, DT_CENTER or DT_SINGLELINE or DT_VCENTER);

  Canvas.Pen.Color := clGray;
  RoundRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom, 3,3);
end;


procedure TShortCutHintWindow.Resize;
var
  ow: integer;
begin
  inherited;
  ow := Canvas.TextWidth('O') + 8;
  if Width < ow then
    Width := ow;
end;

procedure TShortCutHintWindow.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;

function TAdvCustomGlowButton.CanDrawBorder: Boolean;
begin
  Result := (BorderStyle = bsSingle);
end;

function TAdvCustomGlowButton.CanDrawFocused: Boolean;
begin
  Result := (FHasFocus) and (FocusType in [ftBorder, ftHotBorder]);
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}




end.
