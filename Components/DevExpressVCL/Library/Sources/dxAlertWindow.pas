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
unit dxAlertWindow;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Classes, Windows, Forms, Controls, SysUtils, cxClasses, cxControls, cxGraphics,
  Graphics, cxGeometry, Messages, cxLookAndFeelPainters, Math,
  cxDrawTextUtils, ImgList, dxCoreClasses, cxLookAndFeels, Menus, dxForms;

type
  TdxAlertWindowFullAnimation = (awaNone, awaSlide, awaFade, awaMove, awaResize);
  TdxAlertWindowAnimation = awaNone..awaMove;
  TdxAlertWindowCaptionButton = (awcbDropdown, awcbPin, awcbClose);
  TdxAlertWindowCaptionButtons = set of TdxAlertWindowCaptionButton;
  TdxAlertWindowVisibilityTransition = (awvtNone, awvtShowing, awvtHiding);
  TdxAlertWindowMovingDirection = (awmdAuto, awmdLeft, awmdRight, awmdUp, awmdDown);
  TdxAlertWindowNavigationPanelVisibility = (awnpvAuto, awnpvAlways, awnpvNever);
  TdxAlertWindowPosition = (awpAuto, awpTopLeft, awpTopRight, awpBottomLeft, awpBottomRight);

const
  dxAlertWindowIndentBetweenRegions = 5;
  //OptionsAnimate
  dxAlertWindowDefaultAlphaBlendValue = 190;
  dxAlertWindowDefaultHidingAnimation = awaFade;
  dxAlertWindowDefaultHidingAnimationDirection = awmdAuto;
  dxAlertWindowDefaultHidingAnimationTime = 1000;
  dxAlertWindowDefaultHotTrack = True;
  dxAlertWindowDefaultHotTrackAlphaBlendValue = 250;
  dxAlertWindowDefaultHotTrackFadeInTime = 100;
  dxAlertWindowDefaultHotTrackFadeOutTime = 1000;
  dxAlertWindowDefaultShowingAnimation = awaFade;
  dxAlertWindowDefaultShowingAnimationDirection = awmdAuto;
  dxAlertWindowDefaultShowingAnimationTime = 100;
  //OptionsBehavior
  dxAlertWindowDefaultCloseOnRightClick = True;
  dxAlertWindowDefaultDisplayTime = 7000;
  dxAlertWindowDefaultScreenSnap = True;
  dxAlertWindowDefaultScreenSnapBuffer = 20;
  //OptionsButtons
  dxAlertWindowDefaultCaptionButtons = [awcbDropdown, awcbPin, awcbClose];
  dxAlertWindowDefaultButtonHeight = 24;
  dxAlertWindowDefaultButtonWidth = 24;
  //OptionsNavigationPanel
  dxAlertWindowDefaultNavigationPanelVisibility = awnpvAuto;
  dxAlertWindowDefaultNavigationPanelWidth = 0;
  //OptionsSize
  dxAlertWindowDefaultAutoHeight = False;
  dxAlertWindowDefaultAutoWidth = False;
  dxAlertWindowDefaultAutoSizeAdjustment = False;
  dxAlertWindowDefaultMaxHeight = 0;
  dxAlertWindowDefaultMinHeight = 100;
  dxAlertWindowDefaultMaxWidth = 0;
  dxAlertWindowDefaultMinWidth = 200;
  //OptionsText
  dxAlertWindowDefaultTextAlignHorz = taLeftJustify;
  dxAlertWindowDefaultTextAlignVert = vaTop;
  //DefaultCaptionFont
  dxAlertWindowDefaultCaptionFontSize = 10;
  dxAlertWindowDefaultCaptionFontStyle = [fsBold];

  sdxAlertWindowMessageCountMacro = '[MessageCount]';
  sdxAlertWindowMessageIndexMacro = '[MessageIndex]';

type
  TdxAlertWindow = class;
  TdxAlertWindowButtons = class;
  TdxAlertWindowCaptionTextViewInfo = class;
  TdxAlertWindowController = class;
  TdxAlertWindowContentCalculator = class;
  TdxAlertWindowCustomButtonViewInfo = class;
  TdxAlertWindowCustomContentCalculator = class;
  TdxAlertWindowImageViewInfo = class;
  TdxAlertWindowManager = class;
  TdxAlertWindowMessageList = class;
  TdxAlertWindowMessageTextViewInfo = class;
  TdxAlertWindowNavigationPanelContentCalculator = class;
  TdxAlertWindowNavigationPanelTextViewInfo = class;
  TdxAlertWindowOptionsBehavior = class;
  TdxAlertWindowOptionsButtons = class;
  TdxAlertWindowOptionsCaptionButtons = class;
  TdxAlertWindowOptionsMessage = class;
  TdxAlertWindowOptionsNavigationPanel = class;
  TdxAlertWindowViewInfo = class;
  TdxAlertWindowAnimationHelper = class;

  { IdxAlertWindow }

  IdxAlertWindow = interface
  ['{3979BEBB-2A61-4410-A0E2-C83DB48F360A}']
    procedure ButtonClick(AButtonIndex: Integer);
    procedure Close;
    function DoCaptionButtonClick(AButton: TdxAlertWindowCaptionButton): Boolean;

    function DoCustomDrawBackground(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo): Boolean;
    function DoCustomDrawButton(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo): Boolean;
    function DoCustomDrawMessageCaptionText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo): Boolean;
    function DoCustomDrawMessageImage(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo): Boolean;
    function DoCustomDrawMessageText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo): Boolean;
    function DoCustomDrawNavigationPanelText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo): Boolean;

    procedure DoMeasureMessageText(var AWidth, AHeight: Integer);
    function GetContainer: TWinControl;
    function GetController: TdxAlertWindowController;
    function GetCurrentMessageIndex: Integer;
    function GetIsPopupMenuShown: Boolean;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetMessages: TdxAlertWindowMessageList;
    function GetOptionsBehavior: TdxAlertWindowOptionsBehavior;
    function GetOptionsButtons: TdxAlertWindowOptionsButtons;
    function GetOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
    function GetOptionsMessage: TdxAlertWindowOptionsMessage;
    function GetOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
    function GetPinned: Boolean;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TdxAlertWindowViewInfo;
    procedure InvalidateRect(const ARect: TRect);
    procedure SetCurrentMessageIndex(AValue: integer);
    procedure SetIsPopupMenuShown(AValue: Boolean);
    procedure SetPinned(AValue: Boolean);

    property CurrentMessageIndex: Integer read GetCurrentMessageIndex write SetCurrentMessageIndex;
    property IsPopupMenuShown: Boolean read GetIsPopupMenuShown write SetIsPopupMenuShown;
    property Pinned: Boolean read GetPinned write SetPinned;
  end;

  { TdxAlertWindowMessage }

  TdxAlertWindowMessage = class
  private
    FCaption: string;
    FImageIndex: TcxImageIndex;
    FOwner: TdxAlertWindowMessageList;
    FText: string;

    procedure SetCaption(const AValue: string);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetText(const AValue: string);
  protected
    procedure Changed; virtual;

    property Owner: TdxAlertWindowMessageList read FOwner;
  public
    constructor Create(AOwner: TdxAlertWindowMessageList); virtual;

    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Text: string read FText write SetText;
  end;

  { TdxAlertWindowMessageList }

  TdxAlertWindowMessageList = class(TcxObjectList)
  private
    FOnChange: TNotifyEvent;

    function GetItem(AIndex: Integer): TdxAlertWindowMessage;
  protected
    procedure Changed; virtual;
    function IsValid(AIndex: Integer): Boolean;
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    function Add: TdxAlertWindowMessage; overload;
    function Add(const ACaption, AText: string; AImageIndex: TcxImageIndex = -1): TdxAlertWindowMessage; overload;

    property Items[Index: Integer]: TdxAlertWindowMessage read GetItem; default;
  end;

  { TdxAlertWindowCustomOptions }

  TdxAlertWindowCustomOptions = class(TcxOwnedPersistent)
  private
    FOnChange: TNotifyEvent;

    FFreeNotificator: TcxFreeNotificator;
  protected
    procedure Changed; virtual;
    procedure ChangeScale(M: Integer; D: Integer); virtual;
    procedure FreeNotification(Sender: TComponent); virtual;

    property FreeNotificator: TcxFreeNotificator read FFreeNotificator;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  end;

  { TdxAlertWindowOptionsAnimate }

  TdxAlertWindowOptionsAnimate = class(TdxAlertWindowCustomOptions)
  private
    FAlphaBlendValue: Byte;
    FHidingAnimation: TdxAlertWindowAnimation;
    FHidingAnimationDirection: TdxAlertWindowMovingDirection;
    FHidingAnimationTime: Cardinal;
    FHotTrack: Boolean;
    FHotTrackAlphaBlendValue: Byte;
    FHotTrackFadeInTime: Cardinal;
    FHotTrackFadeOutTime: Cardinal;
    FSizeAdjustmentAnimationTime: Cardinal;
    FShowingAnimation: TdxAlertWindowAnimation;
    FShowingAnimationDirection: TdxAlertWindowMovingDirection;
    FShowingAnimationTime: Cardinal;

    procedure SetAlphaBlendValue(AValue: Byte);
    procedure SetHidingAnimation(AValue: TdxAlertWindowAnimation);
    procedure SetHidingAnimationDirection(AValue: TdxAlertWindowMovingDirection);
    procedure SetHidingAnimationTime(AValue: Cardinal);
    procedure SetHotTrack(AValue: Boolean);
    procedure SetHotTrackAlphaBlendValue(AValue: Byte);
    procedure SetHotTrackFadeInTime(AValue: Cardinal);
    procedure SetHotTrackFadeOutTime(AValue: Cardinal);
    procedure SetSizeAdjustmentAnimationTime(AValue: Cardinal);
    procedure SetShowingAnimation(AValue: TdxAlertWindowAnimation);
    procedure SetShowingAnimationDirection(AValue: TdxAlertWindowMovingDirection);
    procedure SetShowingAnimationTime(AValue: Cardinal);
  public
    constructor Create(AOwner: TPersistent); override;

    procedure Assign(Source: TPersistent); override;
  published
    property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue default dxAlertWindowDefaultAlphaBlendValue;
    property HidingAnimation: TdxAlertWindowAnimation read FHidingAnimation write SetHidingAnimation default dxAlertWindowDefaultHidingAnimation;
    property HidingAnimationDirection: TdxAlertWindowMovingDirection
      read FHidingAnimationDirection write SetHidingAnimationDirection default dxAlertWindowDefaultHidingAnimationDirection;
    property HidingAnimationTime: Cardinal read FHidingAnimationTime write SetHidingAnimationTime default dxAlertWindowDefaultHidingAnimationTime;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default dxAlertWindowDefaultHotTrack;
    property HotTrackAlphaBlendValue: Byte read FHotTrackAlphaBlendValue write SetHotTrackAlphaBlendValue default dxAlertWindowDefaultHotTrackAlphaBlendValue;
    property HotTrackFadeInTime: Cardinal read FHotTrackFadeInTime write SetHotTrackFadeInTime default dxAlertWindowDefaultHotTrackFadeInTime;
    property HotTrackFadeOutTime: Cardinal read FHotTrackFadeOutTime write SetHotTrackFadeOutTime default dxAlertWindowDefaultHotTrackFadeOutTime;
    property SizeAdjustmentAnimationTime: Cardinal read FSizeAdjustmentAnimationTime write SetSizeAdjustmentAnimationTime default dxAlertWindowDefaultShowingAnimationTime;
    property ShowingAnimation: TdxAlertWindowAnimation read FShowingAnimation write SetShowingAnimation default dxAlertWindowDefaultShowingAnimation;
    property ShowingAnimationDirection: TdxAlertWindowMovingDirection
      read FShowingAnimationDirection write SetShowingAnimationDirection default dxAlertWindowDefaultShowingAnimationDirection;
    property ShowingAnimationTime: Cardinal read FShowingAnimationTime write SetShowingAnimationTime default dxAlertWindowDefaultShowingAnimationTime;
  end;

  { TdxAlertWindowOptionsBehavior }

  TdxAlertWindowOptionsBehavior = class(TdxAlertWindowCustomOptions)
  private
    FCloseOnRightClick: Boolean;
    FDisplayTime: Cardinal;
    FScreenSnap: Boolean;
    FScreenSnapBuffer: Integer;

    procedure SetCloseOnRightClick(AValue: Boolean);
    procedure SetDisplayTime(AValue: Cardinal);
    procedure SetScreenSnap(AValue: Boolean);
    procedure SetScreenSnapBuffer(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
  public
    constructor Create(AOwner: TPersistent); override;

    procedure Assign(Source: TPersistent); override;
  published
    property CloseOnRightClick: Boolean read FCloseOnRightClick write SetCloseOnRightClick default dxAlertWindowDefaultCloseOnRightClick;
    property DisplayTime: Cardinal read FDisplayTime write SetDisplayTime default dxAlertWindowDefaultDisplayTime;
    property ScreenSnap: Boolean read FScreenSnap write SetScreenSnap default dxAlertWindowDefaultScreenSnap;
    property ScreenSnapBuffer: Integer read FScreenSnapBuffer write SetScreenSnapBuffer default dxAlertWindowDefaultScreenSnapBuffer;
  end;

  { TdxAlertWindowOptionsText }

  TdxAlertWindowOptionsText = class(TdxAlertWindowCustomOptions)
  private
    FAlignHorz: TAlignment;
    FAlignVert: TcxAlignmentVert;
    FFont: TFont;

    procedure SetAlignHorz(AValue: TAlignment);
    procedure SetAlignVert(AValue: TcxAlignmentVert);
    procedure SetFont(AValue: TFont);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoFontChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property AlignHorz: TAlignment read FAlignHorz write SetAlignHorz default dxAlertWindowDefaultTextAlignHorz;
    property AlignVert: TcxAlignmentVert read FAlignVert write SetAlignVert default dxAlertWindowDefaultTextAlignVert;
    property Font: TFont read FFont write SetFont;
  end;

  { TdxAlertWindowOptionsMessage }

  TdxAlertWindowOptionsMessage = class(TdxAlertWindowCustomOptions)
  private
    FCaption: TdxAlertWindowOptionsText;
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FText: TdxAlertWindowOptionsText;

    procedure ImageListChange(Sender: TObject);
    procedure SetCaption(AValue: TdxAlertWindowOptionsText);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetText(AValue: TdxAlertWindowOptionsText);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoOptionTextChange(Sender: TObject); virtual;
    procedure FreeNotification(Sender: TComponent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TdxAlertWindowOptionsText read FCaption write SetCaption;
    property Images: TCustomImageList read FImages write SetImages;
    property Text: TdxAlertWindowOptionsText read FText write SetText;
  end;

  { TdxAlertWindowOptionsNavigationPanel }

  TdxAlertWindowOptionsNavigationPanel = class(TdxAlertWindowCustomOptions)
  private
    FDisplayMask: string;
    FFont: TFont;
    FVisibility: TdxAlertWindowNavigationPanelVisibility;
    FWidth: Integer;

    function IsDisplayMaskStored: Boolean;
    procedure SetDisplayMask(const AValue: string);
    procedure SetFont(AValue: TFont);
    procedure SetVisibility(AValue: TdxAlertWindowNavigationPanelVisibility);
    procedure SetWidth(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure DoFontChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property DisplayMask: string read FDisplayMask write SetDisplayMask stored IsDisplayMaskStored;
    property Font: TFont read FFont write SetFont;
    property Visibility: TdxAlertWindowNavigationPanelVisibility read FVisibility write SetVisibility default dxAlertWindowDefaultNavigationPanelVisibility;
    property Width: Integer read FWidth write SetWidth default dxAlertWindowDefaultNavigationPanelWidth;
  end;

  { TdxAlertWindowOptionsCaptionButtons }

  TdxAlertWindowOptionsCaptionButtons = class(TdxAlertWindowCustomOptions)
  private
    FCaptionButtons: TdxAlertWindowCaptionButtons;
    FPopupMenu: TComponent;

    procedure SetCaptionButtons(AValue: TdxAlertWindowCaptionButtons);
    procedure SetPopupMenu(AValue: TComponent);
  protected
    procedure FreeNotification(Sender: TComponent); override;
  public
    constructor Create(AOwner: TPersistent); override;

    procedure Assign(Source: TPersistent); override;
  published
    property CaptionButtons: TdxAlertWindowCaptionButtons read FCaptionButtons write SetCaptionButtons default dxAlertWindowDefaultCaptionButtons;
    property PopupMenu: TComponent read FPopupMenu write SetPopupMenu;
  end;

  { TdxAlertWindowOptionsSize }

  TdxAlertWindowOptionsSize = class(TdxAlertWindowCustomOptions)
  private
    FAutoHeight: Boolean;
    FAutoWidth: Boolean;
    FAutoSizeAdjustment: Boolean;
    FHeight: Integer;
    FMaxHeight: Integer;
    FMaxWidth: Integer;
    FMinHeight: Integer;
    FMinWidth: Integer;
    FWidth: Integer;

    procedure SetAutoHeight(AValue: Boolean);
    procedure SetAutoSizeAdjustment(AValue: Boolean);
    procedure SetAutoWidth(AValue: Boolean);
    procedure SetHeight(AValue: Integer);
    procedure SetMaxHeight(AValue: Integer);
    procedure SetMaxWidth(AValue: Integer);
    procedure SetMinHeight(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
  public
    constructor Create(AOwner: TPersistent); override;

    procedure Assign(Source: TPersistent); override;
  published
// #BA: Don't change properties order
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default dxAlertWindowDefaultAutoHeight;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default dxAlertWindowDefaultAutoWidth;
    property AutoSizeAdjustment: Boolean read FAutoSizeAdjustment write SetAutoSizeAdjustment default dxAlertWindowDefaultAutoSizeAdjustment;
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight default dxAlertWindowDefaultMaxHeight;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default dxAlertWindowDefaultMaxWidth;
    property MinHeight: Integer read FMinHeight write SetMinHeight default dxAlertWindowDefaultMinHeight;
    property MinWidth: Integer read FMinWidth write SetMinWidth default dxAlertWindowDefaultMinWidth;
    property Height: Integer read FHeight write SetHeight default dxAlertWindowDefaultMinHeight;
    property Width: Integer read FWidth write SetWidth default dxAlertWindowDefaultMinWidth;
  end;

  { TdxAlertWindowButton }

  TdxAlertWindowButton = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FHint: string;
    FImageIndex: TcxImageIndex;
    FVisible: Boolean;

    function GetCollection: TdxAlertWindowButtons;
    procedure SetEnabled(AValue: Boolean);
    procedure SetHint(const AValue: string);
    procedure SetImageIndex(AValue: TcxImageIndex);
    procedure SetVisible(AValue: Boolean);
  protected
    property Collection: TdxAlertWindowButtons read GetCollection;
  public
    constructor Create(Collection: TCollection); override;

    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Hint: string read FHint write SetHint;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  { TdxAlertWindowButtons }

  TdxAlertWindowButtonClass = class of TdxAlertWindowButton;

  TdxAlertWindowButtons = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TdxAlertWindowButton;
    procedure SetItem(Index: Integer; AValue: TdxAlertWindowButton);
  protected
    class function GetButtonClass: TdxAlertWindowButtonClass; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); virtual;

    function Add: TdxAlertWindowButton;

    property Items[Index: Integer]: TdxAlertWindowButton read GetItem write SetItem; default;
  end;

  { TdxAlertWindowOptionsButtons }

  TdxAlertWindowOptionsButtons = class(TdxAlertWindowCustomOptions)
  private
    FButtons: TdxAlertWindowButtons;
    FChangeLink: TChangeLink;
    FHeight: Integer;
    FImages: TCustomImageList;
    FWidth: Integer;

    procedure ImageListChange(Sender: TObject);
    procedure SetButtons(AValue: TdxAlertWindowButtons);
    procedure SetHeight(AValue: Integer);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetWidth(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    procedure FreeNotification(Sender: TComponent); override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Buttons: TdxAlertWindowButtons read FButtons write SetButtons;
    property Height: Integer read FHeight write SetHeight default dxAlertWindowDefaultButtonHeight;
    property Images: TCustomImageList read FImages write SetImages;
    property Width: Integer read FWidth write SetWidth default dxAlertWindowDefaultButtonWidth;
  end;

  { TdxAlertWindowCustomViewInfo }

  TdxAlertWindowCustomViewInfo = class
  private
    FBounds: TRect;
    FOwner: IdxAlertWindow;

    function GetController: TdxAlertWindowController;
    function GetCurrentMessage: TdxAlertWindowMessage;
    function GetMessageCount: Integer;
    function GetMessageIndex: Integer;
    function GetMessages: TdxAlertWindowMessageList;
    function GetOptionsButtons: TdxAlertWindowOptionsButtons;
    function GetOptionsMessage: TdxAlertWindowOptionsMessage;
    function GetOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    function CustomDraw(ACanvas: TcxCanvas): Boolean; virtual;
    procedure DoDraw(ACanvas: TcxCanvas); virtual; abstract;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    function GetHint: string; virtual;
    function GetScaleFactor: TdxScaleFactor;
  public
    constructor Create(AOwner: IdxAlertWindow); virtual;

    procedure Calculate(const ABounds: TRect); virtual;
    function CalculateAutoHeight(AWidth: Integer): Integer; virtual;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; virtual;
    procedure Draw(ACanvas: TcxCanvas); virtual;
    function GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo; virtual;
    procedure Invalidate;
    procedure RecreateViewInfo; virtual;

    property Bounds: TRect read FBounds;
    property Controller: TdxAlertWindowController read GetController;
    property CurrentMessage: TdxAlertWindowMessage read GetCurrentMessage;
    property Hint: string read GetHint;
    property MessageCount: Integer read GetMessageCount;
    property MessageIndex: Integer read GetMessageIndex;
    property Messages: TdxAlertWindowMessageList read GetMessages;
    property OptionsButtons: TdxAlertWindowOptionsButtons read GetOptionsButtons;
    property OptionsMessage: TdxAlertWindowOptionsMessage read GetOptionsMessage;
    property OptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel read GetOptionsNavigationPanel;
    property Owner: IdxAlertWindow read FOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  {TdxAlertWindowCustomTextViewInfo}

  TdxAlertWindowCustomTextViewInfo = class(TdxAlertWindowCustomViewInfo)
  private
    FIsTextClipped: Boolean;
  protected
    function FormatText(AAlignHorz: TAlignment; AAlignVert: TcxAlignmentVert): Cardinal;
    function GetActualAlignHorz(AAlignHorz: TAlignment): TAlignment;
    function GetDefaultTextColor: TColor; virtual;
    function GetDrawTextFlags: Cardinal; virtual; abstract;
    function GetFont: TFont; virtual; abstract;
    function GetHint: string; override;
    function GetIsTextClipped: Boolean; virtual;
    function GetText: string; virtual; abstract;

    procedure DoDraw(ACanvas: TcxCanvas); override;
    property DrawTextFlags: Cardinal read GetDrawTextFlags;
  public
    procedure Calculate(const ABounds: TRect); override;
    procedure Draw(ACanvas: TcxCanvas); override;

    property Font: TFont read GetFont;
    property IsTextClipped: Boolean read FIsTextClipped write FIsTextClipped;
    property Text: string read GetText;
  end;

  { TdxAlertWindowCustomButtonViewInfo }

  TdxAlertWindowCustomButtonViewInfoClass = class of TdxAlertWindowCustomButtonViewInfo;

  TdxAlertWindowCustomButtonViewInfo = class(TdxAlertWindowCustomViewInfo)
  protected
    procedure Click; virtual; abstract;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetActualDrawingKind: TdxAlertWindowButtonKind; virtual;
    function GetContentOffsets: TRect; virtual;
    function GetEnabled: Boolean; virtual;
    function GetGlyphSize: TSize; virtual;
    function GetKind: TdxAlertWindowButtonKind; virtual; abstract;
    function GetState: TcxButtonState; virtual;
  public
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;

    property ContentOffsets: TRect read GetContentOffsets;
    property Enabled: Boolean read GetEnabled;
    property GlyphSize: TSize read GetGlyphSize;
    property Kind: TdxAlertWindowButtonKind read GetKind;
    property State: TcxButtonState read GetState;
  end;

  { TdxAlertWindowPreviousButtonViewInfo }

  TdxAlertWindowPreviousButtonViewInfo = class(TdxAlertWindowCustomButtonViewInfo)
  protected
    procedure Click; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetKind: TdxAlertWindowButtonKind; override;
  end;

  { TdxAlertWindowNextButtonViewInfo }

  TdxAlertWindowNextButtonViewInfo = class(TdxAlertWindowCustomButtonViewInfo)
  protected
    procedure Click; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetKind: TdxAlertWindowButtonKind; override;
  end;

  { TdxAlertWindowCloseButtonViewInfo }

  TdxAlertWindowCloseButtonViewInfo = class(TdxAlertWindowCustomButtonViewInfo)
  protected
    procedure Click; override;
    function GetHint: String; override;
    function GetKind: TdxAlertWindowButtonKind; override;
  end;

  { TdxAlertWindowPinButtonViewInfo }

  TdxAlertWindowPinButtonViewInfo = class(TdxAlertWindowCustomButtonViewInfo)
  protected
    procedure Click; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetHint: String; override;
    function GetKind: TdxAlertWindowButtonKind; override;
    function GetPinned: Boolean;
  public
    property Pinned: Boolean read GetPinned;
  end;

  { TdxAlertWindowDropdownButtonViewInfo }

  TdxAlertWindowDropdownButtonViewInfo = class(TdxAlertWindowCustomButtonViewInfo)
  protected
    procedure Click; override;
    function GetEnabled: Boolean; override;
    function GetHint: String; override;
    function GetKind: TdxAlertWindowButtonKind; override;
    function GetState: TcxButtonState; override;
  end;

  { TdxAlertWindowButtonViewInfo }

  TdxAlertWindowButtonViewInfo = class(TdxAlertWindowCustomButtonViewInfo)
  private
    FButtonItem: TdxAlertWindowButton;

    function GetIsImageAssigned: Boolean;
  protected
    procedure Click; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetEnabled: Boolean; override;
    function GetGlyphSize: TSize; override;
    function GetHint: string; override;
    function GetKind: TdxAlertWindowButtonKind; override;
  public
    constructor Create(AOwner: IdxAlertWindow; AButtonItem: TdxAlertWindowButton); reintroduce; virtual;

    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;

    property ButtonItem: TdxAlertWindowButton read FButtonItem;
    property IsImageAssigned: Boolean read GetIsImageAssigned;
  end;

  { TdxAlertWindowButtonsViewInfoList }

  TdxAlertWindowButtonsViewInfoList = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TdxAlertWindowCustomButtonViewInfo;
  public
    function Add(AButton: TdxAlertWindowCustomButtonViewInfo): Integer; overload;
    function Add(AOwner: IdxAlertWindow; AButtonItem: TdxAlertWindowButton): Integer; overload;
    function Add(AOwner: IdxAlertWindow; AButtonViewInfoClass: TdxAlertWindowCustomButtonViewInfoClass): Integer; overload;

    property Items[Index: Integer]: TdxAlertWindowCustomButtonViewInfo read GetItem; default;
  end;

  { TdxAlertWindowCustomButtonsViewInfo }

  TdxAlertWindowCustomButtonsViewInfo = class(TdxAlertWindowCustomViewInfo)
  private
    FButtons: TdxAlertWindowButtonsViewInfoList;

    function GetButtonsCount: Integer;
    function GetButtonSize(ANumButton: Integer): TSize;
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
  public
    constructor Create(AOwner: IdxAlertWindow); override;
    destructor Destroy; override;

    procedure Calculate(const ABounds: TRect); override;
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;
    function GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo; override;
    procedure RecreateViewInfo; override;

    property Buttons: TdxAlertWindowButtonsViewInfoList read FButtons;
    property ButtonsCount: Integer read GetButtonsCount;
  end;

  { TdxAlertWindowButtonsViewInfo}

  TdxAlertWindowButtonsViewInfo = class(TdxAlertWindowCustomButtonsViewInfo)
  public
    procedure RecreateViewInfo; override;
  end;

  { TdxAlertWindowCaptionButtonsViewInfo}

  TdxAlertWindowCaptionButtonsViewInfo = class(TdxAlertWindowCustomButtonsViewInfo)
  public
    procedure RecreateViewInfo; override;
  end;

  { TdxAlertWindowImageViewInfo }

  TdxAlertWindowImageViewInfo = class(TdxAlertWindowCustomViewInfo)
  private
    function GetImageIndex: TcxImageIndex;
    function GetIsImageAssigned: Boolean;
  protected
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
  public
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;

    property ImageIndex: TcxImageIndex read GetImageIndex;
    property IsImageAssigned: Boolean read GetIsImageAssigned;
  end;

  { TdxAlertWindowCaptionTextViewInfo }

  TdxAlertWindowCaptionTextViewInfo = class(TdxAlertWindowCustomTextViewInfo)
  protected
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetDrawTextFlags: Cardinal; override;
    function GetFont: TFont; override;
    function GetIsTextClipped: Boolean; override;
    function GetText: string; override;
  public
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;
  end;

  { TdxAlertWindowMessageTextViewInfo }

  TdxAlertWindowMessageTextViewInfo = class(TdxAlertWindowCustomTextViewInfo)
  protected
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetDrawTextFlags: Cardinal; override;
    function GetFont: TFont; override;
    function GetIsTextClipped: Boolean; override;
    function GetText: string; override;
  public
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;
  end;

  {TdxAlertWindowNavigationPanelTextViewInfo}

  TdxAlertWindowNavigationPanelTextViewInfo = class(TdxAlertWindowCustomTextViewInfo)
  protected
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetDefaultTextColor: TColor; override;
    function GetDrawTextFlags: Cardinal; override;
    function GetFont: TFont; override;
    function GetIsTextClipped: Boolean; override;
    function GetText: string; override;
  public
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;
  end;

  { TdxAlertWindowNavigationPanelViewInfo }

  TdxAlertWindowNavigationPanelViewInfo = class(TdxAlertWindowCustomViewInfo)
  private
    FInfoPanelText: TdxAlertWindowNavigationPanelTextViewInfo;
    FNextButton: TdxAlertWindowNextButtonViewInfo;
    FPreviousButton: TdxAlertWindowPreviousButtonViewInfo;
    FContentCalculator: TdxAlertWindowNavigationPanelContentCalculator;
  protected
    procedure DoDraw(ACanvas: TcxCanvas); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function GetVisible: Boolean; virtual;

    property ContentCalculator: TdxAlertWindowNavigationPanelContentCalculator read FContentCalculator;
  public
    constructor Create(AOwner: IdxAlertWindow); override;
    destructor Destroy; override;

    procedure Calculate(const ABounds: TRect); override;
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;
    function GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo; override;

    property InfoPanelText: TdxAlertWindowNavigationPanelTextViewInfo read FInfoPanelText;
    property NextButton: TdxAlertWindowNextButtonViewInfo read FNextButton;
    property PreviousButton: TdxAlertWindowPreviousButtonViewInfo read FPreviousButton;
    property Visible: Boolean read GetVisible;
  end;

  { TdxAlertWindowViewInfo }

  TdxAlertWindowViewInfo = class(TdxAlertWindowCustomViewInfo)
  private
    FButtonsViewInfo: TdxAlertWindowButtonsViewInfo;
    FCaptionButtonsViewInfo: TdxAlertWindowCaptionButtonsViewInfo;
    FCaptionTextViewInfo: TdxAlertWindowCaptionTextViewInfo;
    FContentCalculator: TdxAlertWindowContentCalculator;
    FImageViewInfo: TdxAlertWindowImageViewInfo;
    FMessageTextViewInfo: TdxAlertWindowMessageTextViewInfo;
    FNavigationPanelViewInfo: TdxAlertWindowNavigationPanelViewInfo;
    FUseRightToLeftAlignment: Boolean;
    FUseRightToLeftReading: Boolean;
    FUseRightToLeftScrollBar: Boolean;
    FViewInfoList: TList;

    function GetItemCount: Integer;
  protected
    procedure CheckBiDiMode; virtual;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    procedure DoDraw(ACanvas: TcxCanvas); override;
    function GetViewInfoItem(Index: Integer): TdxAlertWindowCustomViewInfo; virtual;

    property ContentCalculator: TdxAlertWindowContentCalculator read FContentCalculator;
    property ItemCount: Integer read GetItemCount;
    property ViewInfoItem[Index: Integer]: TdxAlertWindowCustomViewInfo read GetViewInfoItem; default;
  public
    constructor Create(AOwner: IdxAlertWindow); override;
    destructor Destroy; override;

    procedure Calculate(const ABounds: TRect); override;
    function CalculateAutoHeight(AWidth: Integer): Integer; override;
    function CalculateAutoWidth(AHeight: Integer = -1): Integer; override;
    procedure CalculateCustomSize(AOptionsSize: TdxAlertWindowOptionsSize; var AWidth, AHeight: Integer); virtual;
    procedure Draw(ACanvas: TcxCanvas); override;
    function GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo; override;
    procedure RecreateViewInfo; override;

    property ButtonsViewInfo: TdxAlertWindowButtonsViewInfo read FButtonsViewInfo;
    property CaptionButtonsViewInfo: TdxAlertWindowCaptionButtonsViewInfo read FCaptionButtonsViewInfo;
    property CaptionTextViewInfo: TdxAlertWindowCaptionTextViewInfo read FCaptionTextViewInfo;
    property ImageViewInfo: TdxAlertWindowImageViewInfo read FImageViewInfo;
    property MessageTextViewInfo: TdxAlertWindowMessageTextViewInfo read FMessageTextViewInfo;
    property NavigationPanelViewInfo: TdxAlertWindowNavigationPanelViewInfo read FNavigationPanelViewInfo;
    property UseRightToLeftAlignment: Boolean read FUseRightToLeftAlignment;
    property UseRightToLeftReading: Boolean read FUseRightToLeftReading;
    property UseRightToLeftScrollBar: Boolean read FUseRightToLeftScrollBar;
  end;

  {TdxAlertWindowCustomElementPlace}

  TdxAlertWindowCustomElementPlace = class
  private
    FCalculatedHeight: Integer;
    FCalculatedWidth: Integer;
    FContentCalculator: TdxAlertWindowCustomContentCalculator;
    FViewInfo: TdxAlertWindowCustomViewInfo;
  protected
    function GetBounds: TRect; virtual; abstract;
    function GetCalculatedHeight: Integer; virtual;
    function GetCalculatedWidth: Integer; virtual;
    function GetCustomCalculate: Boolean; virtual;
    function GetHeightForCalculating: Integer; virtual;
    function GetScaleFactor: TdxScaleFactor; virtual;
    function GetWidthForCalculating: Integer; virtual;
  public
    constructor Create(AContentCalculator: TdxAlertWindowCustomContentCalculator; AViewInfo: TdxAlertWindowCustomViewInfo);

    property Bounds: TRect read GetBounds;
    property CalculatedHeight: Integer read GetCalculatedHeight write FCalculatedHeight;
    property CalculatedWidth: Integer read GetCalculatedWidth write FCalculatedWidth;
    property ContentCalculator: TdxAlertWindowCustomContentCalculator read FContentCalculator;
    property CustomCalculate: Boolean read GetCustomCalculate;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TdxAlertWindowCustomViewInfo read FViewInfo;
  end;

  {TdxAlertWindowImagePlace}

  TdxAlertWindowImagePlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
  end;

  {TdxAlertWindowButtonsPlace}

  TdxAlertWindowButtonsPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
  end;

  {TdxAlertWindowCaptionButtonsPlace}

  TdxAlertWindowCaptionButtonsPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
  end;

  {TdxAlertWindowNavigationPanelPlace}

  TdxAlertWindowNavigationPanelPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
    function GetContentCalculator: TdxAlertWindowContentCalculator; virtual;
  end;

  {TdxAlertWindowCaptionTextPlace}

  TdxAlertWindowCaptionTextPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
    function GetContentCalculator: TdxAlertWindowContentCalculator; virtual;
  end;

  {TdxAlertWindowMessageTextPlace}

  TdxAlertWindowMessageTextPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
    function GetContentCalculator: TdxAlertWindowContentCalculator; virtual;
    function GetHeightForCalculating: Integer; override;
    function GetWidthForCalculating: Integer; override;
  end;

  {TdxAlertWindowCustomContentCalculator}

  TdxAlertWindowCustomContentCalculator = class
  private
    FBounds: TRect;
    FCustomCalculate: Boolean;
    FViewInfo: TdxAlertWindowCustomViewInfo;

    function GetScaleFactor: TdxScaleFactor;
  protected
    function GetBounds: TRect; virtual; abstract;
    function GetContentHeight: Integer; virtual; abstract;
    function GetContentWidth: Integer; virtual; abstract;
  public
    constructor Create(AViewInfo: TdxAlertWindowCustomViewInfo); virtual;

    property Bounds: TRect read GetBounds write FBounds;
    property ContentHeight: Integer read GetContentHeight;
    property ContentWidth: Integer read GetContentWidth;
    property CustomCalculate: Boolean read FCustomCalculate write FCustomCalculate;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  end;

  {TdxAlertWindowContentCalculator}

  TdxAlertWindowContentCalculator = class(TdxAlertWindowCustomContentCalculator)
  strict private
    FButtonsPlace: TdxAlertWindowButtonsPlace;
    FCaptionButtonsPlace: TdxAlertWindowCaptionButtonsPlace;
    FCaptionTextPlace: TdxAlertWindowCaptionTextPlace;
    FImagePlace: TdxAlertWindowImagePlace;
    FMessageTextPlace: TdxAlertWindowMessageTextPlace;
    FNavigationPanelPlace: TdxAlertWindowNavigationPanelPlace;
  protected
    function GetBounds: TRect; override;
    function GetContentHeight: Integer; override;
    function GetContentWidth: Integer; override;
    function GetViewInfo: TdxAlertWindowViewInfo;
  public
    constructor Create(AViewInfo: TdxAlertWindowCustomViewInfo); override;
    destructor Destroy; override;

    property ButtonsPlace: TdxAlertWindowButtonsPlace read FButtonsPlace;
    property CaptionButtonsPlace: TdxAlertWindowCaptionButtonsPlace read FCaptionButtonsPlace;
    property CaptionTextPlace: TdxAlertWindowCaptionTextPlace read FCaptionTextPlace;
    property ImagePlace: TdxAlertWindowImagePlace read FImagePlace;
    property MessageTextPlace: TdxAlertWindowMessageTextPlace read FMessageTextPlace;
    property NavigationPanelPlace: TdxAlertWindowNavigationPanelPlace read FNavigationPanelPlace;
  end;

  {TdxAlertWindowPreviousButtonPlace}

  TdxAlertWindowPreviousButtonPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
  end;

  {TdxAlertWindowNextButtonPlace}

  TdxAlertWindowNextButtonPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
  end;

  {TdxAlertWindowInfoPanelTextPlace}

  TdxAlertWindowInfoPanelTextPlace = class(TdxAlertWindowCustomElementPlace)
  protected
    function GetBounds: TRect; override;
    function GetContentCalculator: TdxAlertWindowNavigationPanelContentCalculator; virtual;
    function GetWidthForCalculating: Integer; override;
  end;

  {TdxAlertWindowNavigationPanelContentCalculator}

  TdxAlertWindowNavigationPanelContentCalculator = class(TdxAlertWindowCustomContentCalculator)
  private
    FInfoPanelTextPlace: TdxAlertWindowInfoPanelTextPlace;
    FNextButtonPlace: TdxAlertWindowNextButtonPlace;
    FPreviousButtonPlace: TdxAlertWindowPreviousButtonPlace;
  protected
    function GetBounds: TRect; override;
    function GetContentHeight: Integer; override;
    function GetContentWidth: Integer; override;
    function GetViewInfo: TdxAlertWindowNavigationPanelViewInfo; virtual;
  public
    constructor Create(AViewInfo: TdxAlertWindowCustomViewInfo); override;
    destructor Destroy; override;

    property InfoPanelTextPlace: TdxAlertWindowInfoPanelTextPlace read FInfoPanelTextPlace;
    property NextButtonPlace: TdxAlertWindowNextButtonPlace read FNextButtonPlace;
    property PreviousButtonPlace: TdxAlertWindowPreviousButtonPlace read FPreviousButtonPlace;
  end;

  { TdxAlertWindowCustomAnimationController }

  TdxAlertWindowCustomAnimationController = class(TObject)
  private
    FTimer: TcxTimer;

    FOnAnimation: TNotifyEvent;
    FOnAnimationComplete: TNotifyEvent;

    procedure InternalStopAnimation;
    function GetActive: Boolean;
    procedure StartAnimationTimer;
    procedure StopAnimationTimer;
    procedure TimerHandler(Sender: TObject);
  protected
    FFramesCount: Integer;

    procedure DoAnimation; virtual;
    procedure DoAnimationComplete; virtual;
    function GetAnimationTimerInterval: Cardinal; virtual;
    procedure JumpToFinalState; virtual; abstract;
    procedure ProcessStep; virtual; abstract;
  public
    destructor Destroy; override;

    procedure StopAnimation; virtual;

    property Active: Boolean read GetActive;
    property OnAnimation: TNotifyEvent read FOnAnimation write FOnAnimation;
    property OnAnimationComplete: TNotifyEvent read FOnAnimationComplete write FOnAnimationComplete;
  end;

  { TdxAlertWindowMovingAnimationController }

  TdxAlertWindowMovingAnimationController = class(TdxAlertWindowCustomAnimationController)
  private
    FBaseRect: TRect;
    FFinishPoint: TPoint;
    FStartPoint: TPoint;
    FStepMovingX: Single;
    FStepMovingY: Single;
    FWorkingPointX: Single;
    FWorkingPointY: Single;

    procedure SetBaseRect(const AValue: TRect);
    procedure SetWorkingPoint(X, Y: Single);
  protected
    function GetCurrentPoint: TPoint; virtual;
    function GetCurrentWindowRect: TRect; virtual;
    procedure JumpToFinalState; override;
    procedure ProcessStep; override;
    procedure StartAnimation(const AStartPoint, AFinishPoint: TPoint; ATime: Cardinal); overload;

    property BaseRect: TRect read FBaseRect write SetBaseRect;
  public
    procedure StartAnimation(const AStartRect: TRect; const AFinishPoint: TPoint; ATime: Cardinal); overload;

    property CurrentWindowRect: TRect read GetCurrentWindowRect;
    property FinishPoint: TPoint read FFinishPoint;
    property StartPoint: TPoint read FStartPoint;
  end;

  { TdxAlertWindowResizingAnimationController }

  TdxAlertWindowResizingAnimationController = class(TdxAlertWindowMovingAnimationController)
  private
    FDirection: array[0..3] of Single;
  protected
    function GetCurrentWindowRect: TRect; override;
    procedure JumpToFinalState; override;
  public
    procedure StartAnimation(const AStartRect, AFinishRect: TRect; ATime: Cardinal); overload;
  end;

  { TdxAlertWindowSlidingAnimationController }

  TdxAlertWindowSlidingAnimationController = class(TdxAlertWindowMovingAnimationController)
  protected
    function GetCurrentContentRect: TRect; virtual;
    function GetCurrentWindowRect: TRect; override;
    procedure JumpToFinalState; override;
  public
    procedure StartAnimation(const ARect: TRect; AShowing: Boolean; ADirection: TdxAlertWindowMovingDirection; ATime: Cardinal); overload;

    property CurrentContentRect: TRect read GetCurrentContentRect;
  end;

  { TdxAlertWindowFadingAnimationController }

  TdxAlertWindowFadingAnimationController = class(TdxAlertWindowCustomAnimationController)
  private
    FAlphaStep: Single;
    FFinishAlphaValue: Byte;
    FWorkingAlphaValue: Single;

    function GetCurrentAlphaValue: Byte;
  protected
    procedure JumpToFinalState; override;
    procedure ProcessStep; override;
  public
    procedure StartAnimation(AStartAlphaValue, AFinishAlphaValue: Byte; ATime: Cardinal);

    property CurrentAlphaValue: Byte read GetCurrentAlphaValue;
  end;

  { TdxAlertWindowController }

  TdxAlertWindowController = class(TObject)
  private
    FAlertWindow: IdxAlertWindow;
    FHotButton: TdxAlertWindowCustomButtonViewInfo;
    FPressedButton: TdxAlertWindowCustomButtonViewInfo;

    function ButtonHitTest(const APoint: TPoint): TdxAlertWindowCustomButtonViewInfo;
    function GetViewInfo: TdxAlertWindowViewInfo;
    procedure SetHotButton(AValue: TdxAlertWindowCustomButtonViewInfo);
    procedure SetPressedButton(AValue: TdxAlertWindowCustomButtonViewInfo);
  protected
    procedure ButtonsClick(AButtonindex: Integer); virtual;
    procedure Close; virtual;
    procedure MouseDown(const P: TPoint); virtual;
    procedure MouseLeave; virtual;
    procedure MouseMove(const P: TPoint); virtual;
    procedure MouseUp(const P: TPoint; Button: TMouseButton); virtual;
    procedure NextMessage; virtual;
    procedure PreviousMessage; virtual;
    procedure ShowPopupMenu; virtual;
    procedure TogglePin; virtual;

    property AlertWindow: IdxAlertWindow read FAlertWindow;
    property HotButton: TdxAlertWindowCustomButtonViewInfo read FHotButton write SetHotButton;
    property PressedButton: TdxAlertWindowCustomButtonViewInfo read FPressedButton write SetPressedButton;
    property ViewInfo: TdxAlertWindowViewInfo read GetViewInfo;
  public
    constructor Create(AAlertWindow: IdxAlertWindow); virtual;
    destructor Destroy; override;
  end;

  { TdxAlertWindowCustomNextAnimation }

  TdxAlertWindowCustomNextAnimation = class
  private
    FAssigned: Boolean;
    FTime: Cardinal;
  public
    constructor Create; virtual;

    procedure Add(ATime: Integer);
    procedure Clear;

    property Assigned: Boolean read FAssigned;
    property Time: Cardinal read FTime;
  end;

  { TdxAlertWindowNextMovingAnimation }

  TdxAlertWindowNextMovingAnimation = class(TdxAlertWindowCustomNextAnimation)
  private
    FFinishPoint: TPoint;
    FInternal: Boolean;
  public
    procedure Add(const AFinishPoint: TPoint; ATime: Integer; AInternal: Boolean = True); overload;

    property FinishPoint: TPoint read FFinishPoint;
    property Internal: Boolean read FInternal;
  end;

  { TdxAlertWindowNextResizingAnimation }

  TdxAlertWindowNextResizingAnimation = class(TdxAlertWindowCustomNextAnimation)
  private
    FFinishRect: TRect;
  public
    procedure Add(const AFinishRect: TRect; ATime: Integer); overload;

    property FinishRect: TRect read FFinishRect;
  end;

  { TdxAlertWindowNextAnimationManager }

  TdxAlertWindowNextAnimationManager = class
  private
    FAnimationHelper: TdxAlertWindowAnimationHelper;
    FNextAnimation: TdxAlertWindowFullAnimation;
    FMoving: TdxAlertWindowNextMovingAnimation;
    FResizing: TdxAlertWindowNextResizingAnimation;
  public
    constructor Create(AAnimationHelper: TdxAlertWindowAnimationHelper); virtual;
    destructor Destroy; override;

    procedure AddMovingAnimation(const AFinishPoint: TPoint; ATime: Integer; AInternal: Boolean = True);
    procedure AddResizingAnimation(const AFinishRect: TRect; ATime: Integer; AInternal: Boolean = True);
    procedure Clear;
    procedure LaunchNextAnimation;
  end;

  { TdxAlertWindowAnimationHelper }

  TdxAlertWindowAnimationHelper = class
  private
    FActiveInternalAnimation: Boolean;
    FAlertWindow: TdxAlertWindow;
    FFadingAnimationController: TdxAlertWindowFadingAnimationController;
    FMovingAnimationController: TdxAlertWindowMovingAnimationController;
    FNextAnimationManager: TdxAlertWindowNextAnimationManager;
    FOptionsAnimate: TdxAlertWindowOptionsAnimate;
    FResizingAnimationController: TdxAlertWindowResizingAnimationController;
    FSlidingAnimationController: TdxAlertWindowSlidingAnimationController;
    FVisibilityAnimation: TdxAlertWindowAnimation;

    procedure AnimationComplete(AAnimation: TdxAlertWindowFullAnimation);
    procedure AnimationOptionsChanged(Sender: TObject);
    function CalculateFinishRectMoving: TRect;
    function CalculateStartRectMoving: TRect;
    procedure FadingAnimationHandler(Sender: TObject);
    procedure FadingCompleteAnimationHandler(Sender: TObject);
    function GetAlphaBlendValue: Integer;
    function GetBoundsRect: TRect;
    function GetContentRect: TRect;
    procedure MovingAnimationHandler(Sender: TObject);
    procedure MovingCompleteAnimationHandler(Sender: TObject);
    procedure ResizingAnimationHandler(Sender: TObject);
    procedure ResizingCompleteAnimationHandler(Sender: TObject);
    procedure SetAlphaBlendValue(AValue: Integer);
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetOptionsAnimate(AValue: TdxAlertWindowOptionsAnimate);
    procedure Show;
    procedure SlidingAnimationHandler(Sender: TObject);
    procedure SlidingCompleteAnimationHandler(Sender: TObject);
  protected
    function GetRealMovingDirection(AVisibilityTransition: TdxAlertWindowVisibilityTransition): TdxAlertWindowMovingDirection; virtual;
    function IsActiveAnimation: Boolean;

    property AlertWindow: TdxAlertWindow read FAlertWindow;
    property AlphaBlendValue: Integer read GetAlphaBlendValue write SetAlphaBlendValue;
    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property FadingAnimationController: TdxAlertWindowFadingAnimationController read FFadingAnimationController;
    property MovingAnimationController: TdxAlertWindowMovingAnimationController read FMovingAnimationController;
    property ResizingAnimationController: TdxAlertWindowResizingAnimationController read FResizingAnimationController;
    property SlidingAnimationController: TdxAlertWindowSlidingAnimationController read FSlidingAnimationController;
  public
    constructor Create(AOwner: TdxAlertWindow); virtual;
    destructor Destroy; override;

    procedure HideAnimation;
    procedure MoveAnimation(const AFinishPoint: TPoint; ATime: Cardinal; AInternalMove: Boolean);
    procedure ResizeAnimation(const AFinishRect: TRect; ATime: Cardinal);
    procedure ShowAnimation;
    procedure StopAnimation;
    procedure UpdateAlphaBlendValue;

    property ActiveInternalAnimation: Boolean read FActiveInternalAnimation;
    property ContentRect: TRect read GetContentRect;
    property OptionsAnimate: TdxAlertWindowOptionsAnimate read FOptionsAnimate write SetOptionsAnimate;
  end;

  {TdxAlertWindowHitTes}

  TdxAlertWindowHitTest = class
  private
    FAlertWindow: TdxAlertWindow;
    FHitPoint: TPoint;
    FViewInfo: TdxAlertWindowCustomViewInfo;

    function GetHitAtBackground: Boolean;
    function GetHitAtButton: Boolean;
    function GetHitAtCaptionButtonClose: Boolean;
    function GetHitAtCaptionButtonDropdown: Boolean;
    function GetHitAtCaptionButtonPin: Boolean;
    function GetHitAtMessageCaptionText: Boolean;
    function GetHitAtMessageImage: Boolean;
    function GetHitAtMessageText: Boolean;
    function GetHitAtNavigationPanelNextButton: Boolean;
    function GetHitAtNavigationPanelPreviousButton: Boolean;
    function GetHitAtNavigationPanelText: Boolean;
    function GetHitAtWindowArea: Boolean;

    function GetHitBackground: TdxAlertWindowViewInfo;
    function GetHitButton: TdxAlertWindowButtonViewInfo;
    function GetHitCaptionButtonClose: TdxAlertWindowCloseButtonViewInfo;
    function GetHitCaptionButtonDropdown: TdxAlertWindowDropdownButtonViewInfo;
    function GetHitCaptionButtonPin: TdxAlertWindowPinButtonViewInfo;
    function GetHitMessageCaptionText: TdxAlertWindowCaptionTextViewInfo;
    function GetHitMessageImage: TdxAlertWindowImageViewInfo;
    function GetHitMessageText: TdxAlertWindowMessageTextViewInfo;
    function GetHitNavigationPanelNextButton: TdxAlertWindowNextButtonViewInfo;
    function GetHitNavigationPanelPreviousButton: TdxAlertWindowPreviousButtonViewInfo;
    function GetHitNavigationPanelText: TdxAlertWindowNavigationPanelTextViewInfo;

    procedure SetHitPoint(const APoint: TPoint);
  public
    constructor Create(AOwner: TdxAlertWindow); virtual;

    procedure ReCalculate; overload;
    procedure ReCalculate(const APoint: TPoint); overload;

    property HitAtBackground: Boolean read GetHitAtBackground;
    property HitAtButton: Boolean read GetHitAtButton;
    property HitAtCaptionButtonClose: Boolean read GetHitAtCaptionButtonClose;
    property HitAtCaptionButtonDropdown: Boolean read GetHitAtCaptionButtonDropdown;
    property HitAtCaptionButtonPin: Boolean read GetHitAtCaptionButtonPin;
    property HitAtMessageCaptionText: Boolean read GetHitAtMessageCaptionText;
    property HitAtMessageImage: Boolean read GetHitAtMessageImage;
    property HitAtMessageText: Boolean read GetHitAtMessageText;
    property HitAtNavigationPanelNextButton: Boolean read GetHitAtNavigationPanelNextButton;
    property HitAtNavigationPanelPreviousButton: Boolean read GetHitAtNavigationPanelPreviousButton;
    property HitAtNavigationPanelText: Boolean read GetHitAtNavigationPanelText;
    property HitAtWindowArea: Boolean read GetHitAtWindowArea;

    property HitBackground: TdxAlertWindowViewInfo read GetHitBackground;
    property HitButton: TdxAlertWindowButtonViewInfo read GetHitButton;
    property HitCaptionButtonClose: TdxAlertWindowCloseButtonViewInfo read GetHitCaptionButtonClose;
    property HitCaptionButtonDropdown: TdxAlertWindowDropdownButtonViewInfo read GetHitCaptionButtonDropdown;
    property HitCaptionButtonPin: TdxAlertWindowPinButtonViewInfo read GetHitCaptionButtonPin;
    property HitMessageCaptionText: TdxAlertWindowCaptionTextViewInfo read GetHitMessageCaptionText;
    property HitMessageImage: TdxAlertWindowImageViewInfo read GetHitMessageImage;
    property HitMessageText: TdxAlertWindowMessageTextViewInfo read GetHitMessageText;
    property HitNavigationPanelNextButton: TdxAlertWindowNextButtonViewInfo read GetHitNavigationPanelNextButton;
    property HitNavigationPanelPreviousButton: TdxAlertWindowPreviousButtonViewInfo read GetHitNavigationPanelPreviousButton;
    property HitNavigationPanelText: TdxAlertWindowNavigationPanelTextViewInfo read GetHitNavigationPanelText;

    property HitPoint: TPoint read FHitPoint write SetHitPoint;
    property ViewInfo: TdxAlertWindowCustomViewInfo read FViewInfo;
  end;

  { TdxAlertWindow }

  TdxAlertWindowCustomDrawBackgroundEvent = procedure(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowCustomDrawButtonEvent = procedure(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowCustomDrawMessageCaptionTextEvent = procedure(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowCustomDrawMessageImageEvent = procedure(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowCustomDrawMessageTextEvent = procedure(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowCustomDrawNavigationPanelTextEvent = procedure(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo; var ADone: Boolean) of object;

  TdxAlertWindowButtonClickEvent = procedure(AAlertWindow: TdxAlertWindow; AButtonIndex: Integer) of object;
  TdxAlertWindowCaptionButtonClickEvent = procedure(AAlertWindow: TdxAlertWindow; AButton: TdxAlertWindowCaptionButton; var AHandled: Boolean) of object;
  TdxAlertWindowDragBeginEvent = procedure(AAlertWindow: TdxAlertWindow; var AAllow: Boolean) of object;
  TdxAlertWindowMeasureEvent = procedure(AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer) of object;
  TdxAlertWindowMouseEvent = procedure(AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer) of object;
  TdxAlertWindowMouseMoveEvent = procedure(AAlertWindow: TdxAlertWindow; AShift: TShiftState; X, Y: Integer) of object;
  TdxAlertWindowNotifyEvent = procedure(AAlertWindow: TdxAlertWindow) of object;

  TdxAlertWindowResizeEvent = procedure(AAlertWindow: TdxAlertWindow; var ANewRect: TRect) of object;

  TdxAlertWindow = class(TdxCustomForm,
    IdxAlertWindow,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2,
    IdxSkinSupport2)
  private
    FAnimationHelper: TdxAlertWindowAnimationHelper;
    FMouseHoldForDragging: Boolean;
    FController: TdxAlertWindowController;
    FCurrentMessageIndex: Integer;
    FDisplayTimer: TcxTimer;
    FDragging: Boolean;
    FDraggingPoint: TPoint;
    FDraggingScreenPoint: TPoint;
    FHitTest: TdxAlertWindowHitTest;
    FIsPopupMenuShown: Boolean;
    FLastSize: TSize;
    FLockCount: Integer;
    FLookAndFeel: TcxLookAndFeel;
    FMessageList: TdxAlertWindowMessageList;
    FMouseInControl: Boolean;
    FOptionsBehavior: TdxAlertWindowOptionsBehavior;
    FOptionsButtons: TdxAlertWindowOptionsButtons;
    FOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
    FOptionsMessage: TdxAlertWindowOptionsMessage;
    FOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
    FOptionsSize: TdxAlertWindowOptionsSize;
    FPinned: Boolean;
    FViewInfo: TdxAlertWindowViewInfo;
    FVisibilityTransition: TdxAlertWindowVisibilityTransition;
    FOnCustomDrawBackground: TdxAlertWindowCustomDrawBackgroundEvent;
    FOnCustomDrawButton: TdxAlertWindowCustomDrawButtonEvent;
    FOnCustomDrawMessageCaptionText: TdxAlertWindowCustomDrawMessageCaptionTextEvent;
    FOnCustomDrawMessageImage: TdxAlertWindowCustomDrawMessageImageEvent;
    FOnCustomDrawMessageText: TdxAlertWindowCustomDrawMessageTextEvent;
    FOnCustomDrawNavigationPanelText: TdxAlertWindowCustomDrawNavigationPanelTextEvent;

    FOnDragBegin: TdxAlertWindowDragBeginEvent;
    FOnDragEnd: TdxAlertWindowNotifyEvent;
    FOnDragMove: TdxAlertWindowNotifyEvent;

    FOnMouseDown: TdxAlertWindowMouseEvent;
    FOnMouseEnter: TdxAlertWindowNotifyEvent;
    FOnMouseLeave: TdxAlertWindowNotifyEvent;
    FOnMouseMove: TdxAlertWindowMouseMoveEvent;
    FOnMouseUp: TdxAlertWindowMouseEvent;

    FOnButtonClick: TdxAlertWindowButtonClickEvent;
    FOnCaptionButtonClick: TdxAlertWindowCaptionButtonClickEvent;
    FOnClick: TdxAlertWindowNotifyEvent;
    FOnHide: TdxAlertWindowNotifyEvent;
    FOnMeasureMessageText: TdxAlertWindowMeasureEvent;
    FOnMove: TdxAlertWindowNotifyEvent;
    FOnResize: TdxAlertWindowResizeEvent;
    FOnShow: TdxAlertWindowNotifyEvent;

    procedure DisplayTimerHandler(Sender: TObject);
    function GetOptionsAnimate: TdxAlertWindowOptionsAnimate;
    function GetVisible: Boolean;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetMessageList(AValue: TdxAlertWindowMessageList);
    procedure SetMouseInControl(AValue: Boolean);
    procedure SetNewSize(AWidth, AHeight: Integer);
    procedure SetOptionsAnimate(AValue: TdxAlertWindowOptionsAnimate);
    procedure SetOptionsBehavior(AValue: TdxAlertWindowOptionsBehavior);
    procedure SetOptionsButtons(AValue: TdxAlertWindowOptionsButtons);
    procedure SetOptionsCaptionButtons(AValue: TdxAlertWindowOptionsCaptionButtons);
    procedure SetOptionsMessage(AValue: TdxAlertWindowOptionsMessage);
    procedure SetOptionsNavigationPanel(AValue: TdxAlertWindowOptionsNavigationPanel);
    procedure SetOptionsSize(AValue: TdxAlertWindowOptionsSize);
    procedure StartDisplayTimer;
    procedure StopDisplayTimer;
    procedure StopVisibilityTransition;
    procedure UpdateDisplayTimerState;
  protected
    procedure BehaviorOptionsChanged(Sender: TObject); virtual;
    procedure ButtonsOptionsChanged(Sender: TObject); virtual;
    procedure CalculateAutoSize(var AWidth, AHeight: Integer); virtual;
    procedure CalculateViewInfo; virtual;
    procedure Click; override;
    procedure CollapseEmptySlotsMove(const ANewPosition: TPoint; ATime: Cardinal); virtual;
    function CreateHitTest: TdxAlertWindowHitTest; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateViewInfo: TdxAlertWindowViewInfo; virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure ScaleFactorChanged(M, D: Integer); override;
    procedure SetWindowRegion(ACornerRadius: Integer); virtual;
    procedure VisibilityTransitionComplete;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed; virtual;
    procedure LookAndFeelOptionsChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MessagesChanged(Sender: Tobject); virtual;
    procedure OptionsChanged(Sender: TObject); virtual;
    procedure SizeOptionsChanged(Sender: TObject); virtual;

    procedure DoButtonClick(AButtonIndex: Integer); virtual;
    procedure DoClick; virtual;
    function DoDragBegin: Boolean; virtual;
    procedure DoDragEnd; virtual;
    procedure DoDragMove; virtual;
    procedure DoHide; override;
    procedure DoMouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseEnter; virtual;
    procedure DoMouseLeave; virtual;
    procedure DoMouseMove(AShift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    procedure DoMove; virtual;
    procedure DoResize(var ANewBounds: TRect); virtual;
    procedure DoShow; override;

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure WMMove(var Msg: TWMMove); message WM_MOVE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    // IcxMouseTrackingCaller
    procedure MouseLeave;

    // IcxMouseTrackingCaller2
    function PtInCaller(const P: TPoint): Boolean;

    // IdxAlertWindow
    procedure ButtonClick(AButtonIndex: Integer);
    function DoCaptionButtonClick(AButton: TdxAlertWindowCaptionButton): Boolean; virtual;
    function DoCustomDrawBackground(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo): Boolean; virtual;
    function DoCustomDrawButton(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo): Boolean; virtual;
    function DoCustomDrawMessageCaptionText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo): Boolean; virtual;
    function DoCustomDrawMessageImage(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo): Boolean; virtual;
    function DoCustomDrawMessageText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo): Boolean; virtual;
    function DoCustomDrawNavigationPanelText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo): Boolean; virtual;
    procedure DoMeasureMessageText(var AWidth, AHeight: Integer); virtual;
    function GetContainer: TWinControl;
    function GetController: TdxAlertWindowController;
    function GetCurrentMessageIndex: Integer;
    function GetIsPopupMenuShown: Boolean;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetMessages: TdxAlertWindowMessageList;
    function GetOptionsBehavior: TdxAlertWindowOptionsBehavior;
    function GetOptionsButtons: TdxAlertWindowOptionsButtons;
    function GetOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
    function GetOptionsMessage: TdxAlertWindowOptionsMessage;
    function GetOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
    function GetOptionsSize: TdxAlertWindowOptionsSize;
    function GetPinned: Boolean;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TdxAlertWindowViewInfo;
    procedure InvalidateRect(const ARect: TRect);
    procedure SetCurrentMessageIndex(AValue: Integer);
    procedure SetIsPopupMenuShown(AValue: Boolean);
    procedure SetPinned(AValue: Boolean);

    //IdxSkinSupport2
    function IsSkinnable: Boolean;

    property AnimationHelper: TdxAlertWindowAnimationHelper read FAnimationHelper;
    property Controller: TdxAlertWindowController read GetController;
    property LockCount: Integer read FLockCount;
    property MouseInControl: Boolean read FMouseInControl write SetMouseInControl;
    property IsPopupMenuShown: Boolean read GetIsPopupMenuShown write SetIsPopupMenuShown;
    property ViewInfo: TdxAlertWindowViewInfo read GetViewInfo;

    property OnCustomDrawBackground: TdxAlertWindowCustomDrawBackgroundEvent read FOnCustomDrawBackground write FOnCustomDrawBackground;
    property OnCustomDrawButton: TdxAlertWindowCustomDrawButtonEvent read FOnCustomDrawButton write FOnCustomDrawButton;
    property OnCustomDrawMessageCaptionText: TdxAlertWindowCustomDrawMessageCaptionTextEvent read FOnCustomDrawMessageCaptionText write FOnCustomDrawMessageCaptionText;
    property OnCustomDrawMessageImage: TdxAlertWindowCustomDrawMessageImageEvent read FOnCustomDrawMessageImage write FOnCustomDrawMessageImage;
    property OnCustomDrawMessageText: TdxAlertWindowCustomDrawMessageTextEvent read FOnCustomDrawMessageText write FOnCustomDrawMessageText;
    property OnCustomDrawNavigationPanelText: TdxAlertWindowCustomDrawNavigationPanelTextEvent read FOnCustomDrawNavigationPanelText write FOnCustomDrawNavigationPanelText;

    property OnDragBegin: TdxAlertWindowDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragEnd: TdxAlertWindowNotifyEvent read FOnDragEnd write FOnDragEnd;
    property OnDragMove: TdxAlertWindowNotifyEvent read FOnDragMove write FOnDragMove;

    property OnMouseDown: TdxAlertWindowMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TdxAlertWindowNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TdxAlertWindowNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TdxAlertWindowMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TdxAlertWindowMouseEvent read FOnMouseUp write FOnMouseUp;

    property OnButtonClick: TdxAlertWindowButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnCaptionButtonClick: TdxAlertWindowCaptionButtonClickEvent read FOnCaptionButtonClick write FOnCaptionButtonClick;
    property OnClick: TdxAlertWindowNotifyEvent read FOnClick write FOnClick;
    property OnHide: TdxAlertWindowNotifyEvent read FOnHide write FOnHide;
    property OnMeasureMessageText: TdxAlertWindowMeasureEvent read FOnMeasureMessageText write FOnMeasureMessageText;
    property OnMove: TdxAlertWindowNotifyEvent read FOnMove write FOnMove;
    property OnResize: TdxAlertWindowResizeEvent read FOnResize write FOnResize;
    property OnShow: TdxAlertWindowNotifyEvent read FOnShow write FOnShow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure DeleteCurrentMessage;
    procedure EndUpdate;
    procedure Hide;
    procedure RestartDisplayTimer;
    procedure Show;

    // IdxAlertWindow
    procedure Close;

    property CurrentMessageIndex: Integer read GetCurrentMessageIndex write SetCurrentMessageIndex;
    property HitTest: TdxAlertWindowHitTest read FHitTest;
    property Left;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property MessageList: TdxAlertWindowMessageList read GetMessages write SetMessageList;
    property OptionsAnimate: TdxAlertWindowOptionsAnimate read GetOptionsAnimate write SetOptionsAnimate;
    property OptionsBehavior: TdxAlertWindowOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsButtons: TdxAlertWindowOptionsButtons read GetOptionsButtons write SetOptionsButtons;
    property OptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons read GetOptionsCaptionButtons write SetOptionsCaptionButtons;
    property OptionsMessage: TdxAlertWindowOptionsMessage read GetOptionsMessage write SetOptionsMessage;
    property OptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel read GetOptionsNavigationPanel write SetOptionsNavigationPanel;
    property OptionsSize: TdxAlertWindowOptionsSize read GetOptionsSize write SetOptionsSize;
    property Pinned: Boolean read GetPinned write SetPinned default False;
    property ShowHint;
    property Top;
    property VisibilityTransition: TdxAlertWindowVisibilityTransition read FVisibilityTransition;
    property Visible: Boolean read GetVisible;
  end;

  {TdxAlertWindowPositionInfo}

  TdxAlertWindowPositionInfo = class
  private
    FAlertWindow: TdxAlertWindow;
    FColumnIndex: Integer;
    FInitialBounds: TRect;
    FMonitorNum: Integer;
    FPosition: TdxAlertWindowPosition;
  public
    constructor Create(AAlertWindow: TdxAlertWindow; APosition: TdxAlertWindowPosition; AMonitorNum: Integer);

    function CompareAlertWindow(AAlertWindow: TdxAlertWindow): Boolean;
    function IsPositionChanged: Boolean;

    property ColumnIndex: Integer read FColumnIndex write FColumnIndex;
    property InitialBounds: TRect read FInitialBounds;
    property MonitorNum: Integer read FMonitorNum;
    property Position: TdxAlertWindowposition read FPosition;
  end;

  { TdxAlertWindowPositionInfoList }

  TdxAlertWindowPositionInfoList = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TdxAlertWindowPositionInfo;
  public
    function Add(AAlertWindow: TdxAlertWindow; APosition: TdxAlertWindowPosition; AMonitorNum: Integer): TdxAlertWindowPositionInfo; overload;

    property Items[Index: Integer]: TdxAlertWindowPositionInfo read GetItem; default;
  end;

  { TdxAlertWindowInitialLayout }

  TdxAlertWindowInitialLayout = class
  private
    FManager: TdxAlertWindowManager;
    FPositionInfoList: TdxAlertWindowPositionInfoList;

    function GetLastInitialGrid(APosition: TdxAlertWindowPosition; AMonitorNum: Integer): TdxAlertWindowPositionInfo;
    function GetNextWindowForShowing: TdxAlertWindow;
    function GetPosition: TdxAlertWindowPosition;
    procedure Recalculate(APosition: TdxAlertWindowPosition; AMonitorNum: Integer; AIndex: Integer = -1);
    function RemoveItemWithWindow(AAlertWindow: TdxAlertWindow): Integer;
  protected
    function GetActualPosition(AAlertWindow: TdxAlertWindow): TdxAlertWindowPosition;
    function GetNewPosition(const AAlertWindowRect: TRect; APreviousPositionInfo: TdxAlertWindowPositionInfo; APosition: TdxAlertWindowPosition; const AWorkArea: TRect; var AColumnIndex: Integer): TRect; virtual;

    property Manager: TdxAlertWindowManager read FManager;
    property PositionInfoList: TdxAlertWindowPositionInfoList read FPositionInfoList;
  public
    constructor Create(AManager: TdxAlertWindowManager); virtual;
    destructor Destroy; override;

    procedure AddItem;
    procedure CalculateNextPosition(AAlertWindow: TdxAlertWindow); virtual;
    procedure DeleteItem(AAlertWindow: TdxAlertWindow); virtual;
    function HasWindow(AAlertWindow: TdxAlertWindow): Integer; virtual;
    procedure ResizeWindow(AAlertWindow: TdxAlertWindow; var ANewBounds: TRect); virtual;

    property Position: TdxAlertWindowPosition read GetPosition;
  end;

  { TdxAlertWindowList }

  TdxAlertWindowClass = class of TdxAlertWindow;

  TdxAlertWindowList = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TdxAlertWindow;
  public
    function Add(AClass: TdxAlertWindowClass): TdxAlertWindow;

    property Items[Index: Integer]: TdxAlertWindow read GetItem; default;
  end;

  {TdxAlertWindowManagerOptionsAnimate}

  TdxAlertWindowManagerOptionsAnimate = class(TdxAlertWindowOptionsAnimate)
  private
    FCollapseEmptySlots: Boolean;
    FCollapseEmptySlotsAnimationTime: Cardinal;

    procedure SetCollapseEmptySlots(AValue: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;

    procedure Assign(Source: TPersistent); override;
  published
    property CollapseEmptySlots: Boolean read FCollapseEmptySlots write SetCollapseEmptySlots default False;
    property CollapseEmptySlotsAnimationTime: Cardinal read FCollapseEmptySlotsAnimationTime write FCollapseEmptySlotsAnimationTime default dxAlertWindowDefaultShowingAnimationTime;
  end;

  { TdxAlertWindowManager }

  TdxAlertWindowManagerCustomDrawBackgroundEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowManagerCustomDrawButtonEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowManagerCustomDrawMessageCaptionTextEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowManagerCustomDrawMessageImageEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowManagerCustomDrawMessageTextEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo; var ADone: Boolean) of object;
  TdxAlertWindowManagerCustomDrawNavigationPanelTextEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo; var ADone: Boolean) of object;

  TdxAlertWindowManagerButtonClickEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; AButtonIndex: Integer) of object;
  TdxAlertWindowManagerCaptionButtonClickEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; AButton: TdxAlertWindowCaptionButton; var AHandled: Boolean) of object;
  TdxAlertWindowManagerDragBeginEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; var AAllow: Boolean) of object;
  TdxAlertWindowManagerMeasureEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer) of object;
  TdxAlertWindowManagerMouseEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer) of object;
  TdxAlertWindowManagerMouseMoveEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow; AShift: TShiftState; X, Y: Integer) of object;
  TdxAlertWindowManagerNotifyEvent = procedure(Sender: TObject; AAlertWindow: TdxAlertWindow) of object;

  TdxAlertWindowManager = class(TcxScalableComponent, IdxSkinSupport, IcxLookAndFeelContainer)
  private
    FAlertWindows: TdxAlertWindowList;
    FHotWindow: TdxAlertWindow;
    FInitialLayout: TdxAlertWindowInitialLayout;
    FLookAndFeel: TcxLookAndFeel;
    FOptionsAnimate: TdxAlertWindowManagerOptionsAnimate;
    FOptionsBehavior: TdxAlertWindowOptionsBehavior;
    FOptionsButtons: TdxAlertWindowOptionsButtons;
    FOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
    FOptionsMessage: TdxAlertWindowOptionsMessage;
    FOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
    FOptionsSize: TdxAlertWindowOptionsSize;
    FWindowPosition: TdxAlertWindowPosition;
    FWindowMaxCount: Integer;

    FOnCustomDrawBackground: TdxAlertWindowManagerCustomDrawBackgroundEvent;
    FOnCustomDrawMessageImage: TdxAlertWindowManagerCustomDrawMessageImageEvent;
    FOnCustomDrawMessageText: TdxAlertWindowManagerCustomDrawMessageTextEvent;
    FOnCustomDrawMessageCaptionText: TdxAlertWindowManagerCustomDrawMessageCaptionTextEvent;
    FOnCustomDrawButton: TdxAlertWindowManagerCustomDrawButtonEvent;
    FOnCustomDrawNavigationPanelText: TdxAlertWindowManagerCustomDrawNavigationPanelTextEvent;

    FOnDragBegin: TdxAlertWindowManagerDragBeginEvent;
    FOnDragEnd: TdxAlertWindowManagerNotifyEvent;
    FOnDragMove: TdxAlertWindowManagerNotifyEvent;

    FOnMouseDown: TdxAlertWindowManagerMouseEvent;
    FOnMouseEnter: TdxAlertWindowManagerNotifyEvent;
    FOnMouseLeave: TdxAlertWindowManagerNotifyEvent;
    FOnMouseMove: TdxAlertWindowManagerMouseMoveEvent;
    FOnMouseUp: TdxAlertWindowManagerMouseEvent;

    FOnBeforeShow: TdxAlertWindowManagerNotifyEvent;
    FOnButtonClick: TdxAlertWindowManagerButtonClickEvent;
    FOnCaptionButtonClick: TdxAlertWindowManagerCaptionButtonClickEvent;
    FOnClick: TdxAlertWindowManagerNotifyEvent;
    FOnClose: TdxAlertWindowManagerNotifyEvent;
    FOnHide: TdxAlertWindowManagerNotifyEvent;
    FOnInitialize: TdxAlertWindowManagerNotifyEvent;
    FOnMeasureMessageText: TdxAlertWindowManagerMeasureEvent;
    FOnShow: TdxAlertWindowManagerNotifyEvent;

    procedure AlertWindowCustomDrawBackgroundHandler(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo; var ADone: Boolean);
    procedure AlertWindowCustomDrawButtonHandler(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo; var ADone: Boolean);
    procedure AlertWindowCustomDrawMessageCaptionTextHandler(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo; var ADone: Boolean);
    procedure AlertWindowCustomDrawMessageImageHandler(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo; var ADone: Boolean);
    procedure AlertWindowCustomDrawMessageTextHandler(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo; var ADone: Boolean);
    procedure AlertWindowCustomDrawNavigationPanelTextHandler(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo; var ADone: Boolean);

    procedure AlertWindowDragBeginHandler(AAlertWindow: TdxAlertWindow; var AAllow: Boolean);
    procedure AlertWindowDragEndHandler(AAlertWindow: TdxAlertWindow);
    procedure AlertWindowDragMoveHandler(AAlertWindow: TdxAlertWindow);

    procedure AlertWindowButtonClickHandler(AAlertWindow: TdxAlertWindow; AButtonIndex: Integer);
    procedure AlertWindowCaptionButtonClickHandler(AAlertWindow: TdxAlertWindow; AButton: TdxAlertWindowCaptionButton; var AHandled: Boolean);
    procedure AlertWindowClickHandler(AAlertWindow: TdxAlertWindow);
    procedure AlertWindowEnterHandler(AAlertWindow: TdxAlertWindow);
    procedure AlertWindowHideHandler(AAlertWindow: TdxAlertWindow);
    procedure AlertWindowLeaveHandler(AAlertWindow: TdxAlertWindow);
    procedure AlertWindowMeasureMessageTextHandler(AAlertWindow: TdxAlertWindow;  var AWidth, AHeight: Integer);
    procedure AlertWindowMouseDownHandler(AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
    procedure AlertWindowMouseMoveHandler(AAlertWindow: TdxAlertWindow; AShift: TShiftState; X, Y: Integer);
    procedure AlertWindowMouseUpHandler(AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
    procedure AlertWindowMoveHandler(AAlertWindow: TdxAlertWindow);
    procedure AlertWindowResizeHandler(AAlertWindow: TdxAlertWindow; var ANewBounds: TRect);
    procedure AlertWindowShowHandler(AAlertWindow: TdxAlertWindow);

    function GetCount: Integer;
    function GetDisplayedCount: Integer;
    function GetItem(AIndex: Integer): TdxAlertWindow;
    function GetOptionsAnimate: TdxAlertWindowManagerOptionsAnimate;
    function GetOptionsBehavior: TdxAlertWindowOptionsBehavior;
    function GetOptionsButtons: TdxAlertWindowOptionsButtons;
    function GetOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
    function GetOptionsMessage: TdxAlertWindowOptionsMessage;
    function GetOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
    function GetOptionsSize: TdxAlertWindowOptionsSize;
    function InternalAdd: TdxAlertWindow;
    function InternalRemove(AComponent: TComponent): Boolean;
    procedure SetLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetOptionsAnimate(AValue: TdxAlertWindowManagerOptionsAnimate);
    procedure SetOptionsBehavior(AValue: TdxAlertWindowOptionsBehavior);
    procedure SetOptionsButtons(AValue: TdxAlertWindowOptionsButtons);
    procedure SetOptionsCaptionButtons(AValue: TdxAlertWindowOptionsCaptionButtons);
    procedure SetOptionsMessage(AValue: TdxAlertWindowOptionsMessage);
    procedure SetOptionsNavigationPanel(AValue: TdxAlertWindowOptionsNavigationPanel);
    procedure SetOptionsSize(AValue: TdxAlertWindowOptionsSize);
    procedure SetWindowPosition(AValue: TdxAlertWindowPosition);
    procedure SetWindowMaxCount(AValue: Integer);
  protected
    procedure ChangeScale(M: Integer; D: Integer); override;
    function CreateInitialLayoutCalculator: TdxAlertWindowInitialLayout; virtual;
    function GetAlertWindowClass: TdxAlertWindowClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoBeforeShow(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoButtonClick(AAlertWindow: TdxAlertWindow; AButtonIndex: Integer); virtual;
    function DoCaptionButtonClick(AAlertWindow: TdxAlertWindow; AButton: TdxAlertWindowCaptionButton): Boolean; virtual;
    procedure DoClick(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoClose(AAlertWindow: TdxAlertWindow); virtual;

    function DoCustomDrawBackground(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo): Boolean; virtual;
    function DoCustomDrawButton(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo): Boolean; virtual;
    function DoCustomDrawMessageCaptionText(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo): Boolean; virtual;
    function DoCustomDrawMessageImage(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo): Boolean; virtual;
    function DoCustomDrawMessageText(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo): Boolean; virtual;
    function DoCustomDrawNavigationPanelText(AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo): Boolean; virtual;

    function DoDragBegin(AAlertWindow: TdxAlertWindow): Boolean; virtual;
    procedure DoDragEnd(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoDragMove(AAlertWindow: TdxAlertWindow); virtual;

    procedure DoHide(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoInitialize(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoMeasureMessageText(AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer); virtual;

    procedure DoMouseDown(AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseEnter(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoMouseLeave(AAlertWindow: TdxAlertWindow); virtual;
    procedure DoMouseMove(AAlertWindow: TdxAlertWindow; AShift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer); virtual;

    procedure DoShow(AAlertWindow: TdxAlertWindow); virtual;

    // IcxLookAndFeelContainer
    function GetLookAndFeel: TcxLookAndFeel;

    property DisplayedCount: Integer read GetDisplayedCount;
    property InitialLayout: TdxAlertWindowInitialLayout read FInitialLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Close(AAlertWindow: TdxAlertWindow);
    function IndexOf(AAlertWindow: TdxAlertWindow): Integer;
    function Show(const ACaption, AText: string; AImageIndex: TcxImageIndex = -1): TdxAlertWindow;

    property Count: Integer read GetCount;
    property HotWindow: TdxAlertWindow read FHotWindow;
    property Items[Index: Integer]: TdxAlertWindow read GetItem; default;
  published
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property OptionsAnimate: TdxAlertWindowManagerOptionsAnimate read GetOptionsAnimate write SetOptionsAnimate;
    property OptionsBehavior: TdxAlertWindowOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsButtons: TdxAlertWindowOptionsButtons read GetOptionsButtons write SetOptionsButtons;
    property OptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons read GetOptionsCaptionButtons write SetOptionsCaptionButtons;
    property OptionsMessage: TdxAlertWindowOptionsMessage read GetOptionsMessage write SetOptionsMessage;
    property OptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel read GetOptionsNavigationPanel write SetOptionsNavigationPanel;
    property OptionsSize: TdxAlertWindowOptionsSize read GetOptionsSize write SetOptionsSize;
    property WindowPosition: TdxAlertWindowPosition read FWindowPosition write SetWindowPosition default awpAuto;
    property WindowMaxCount: Integer read FWindowMaxCount write SetWindowMaxCount default 0;

    property OnCustomDrawBackground: TdxAlertWindowManagerCustomDrawBackgroundEvent read FOnCustomDrawBackground write FOnCustomDrawBackground;
    property OnCustomDrawMessageImage: TdxAlertWindowManagerCustomDrawMessageImageEvent read FOnCustomDrawMessageImage write FOnCustomDrawMessageImage;
    property OnCustomDrawMessageText: TdxAlertWindowManagerCustomDrawMessageTextEvent read FOnCustomDrawMessageText write FOnCustomDrawMessageText;
    property OnCustomDrawMessageCaptionText: TdxAlertWindowManagerCustomDrawMessageCaptionTextEvent read FOnCustomDrawMessageCaptionText write FOnCustomDrawMessageCaptionText;
    property OnCustomDrawButton: TdxAlertWindowManagerCustomDrawButtonEvent read FOnCustomDrawButton write FOnCustomDrawButton;
    property OnCustomDrawNavigationPanelText: TdxAlertWindowManagerCustomDrawNavigationPanelTextEvent read FOnCustomDrawNavigationPanelText write FOnCustomDrawNavigationPanelText;

    property OnDragBegin: TdxAlertWindowManagerDragBeginEvent read FOnDragBegin write FOnDragBegin;
    property OnDragEnd: TdxAlertWindowManagerNotifyEvent read FOnDragEnd write FOnDragEnd;
    property OnDragMove: TdxAlertWindowManagerNotifyEvent read FOnDragMove write FOnDragMove;

    property OnBeforeShow:  TdxAlertWindowManagerNotifyEvent read FOnBeforeShow write FOnBeforeShow;
    property OnButtonClick: TdxAlertWindowManagerButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnCaptionButtonClick: TdxAlertWindowManagerCaptionButtonClickEvent read FOnCaptionButtonClick write FOnCaptionButtonClick;
    property OnClick: TdxAlertWindowManagerNotifyEvent read FOnClick write FOnClick;
    property OnClose: TdxAlertWindowManagerNotifyEvent read FOnClose write FOnClose;
    property OnHide: TdxAlertWindowManagerNotifyEvent read FonHide write FonHide;
    property OnInitialize: TdxAlertWindowManagerNotifyEvent read FOnInitialize write FOnInitialize;
    property OnMeasureMessageText: TdxAlertWindowManagerMeasureEvent read FOnMeasureMessageText write FOnMeasureMessageText;
    property OnMouseDown: TdxAlertWindowManagerMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TdxAlertWindowManagerNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TdxAlertWindowManagerNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TdxAlertWindowManagerMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TdxAlertWindowManagerMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnShow: TdxAlertWindowManagerNotifyEvent read FOnShow write FOnShow;
  end;

implementation

uses
  Types, dxCore, cxEditConsts, ShellAPI, dxDPIAwareUtils;

function AdjustSizeByIndent(AValue: Integer; AScaleFactor: TdxScaleFactor; ANeedIndent: Boolean = True): Integer;
begin
  Result := AValue;
  if ANeedIndent and (Result > 0) then
    Inc(Result, AScaleFactor.Apply(dxAlertWindowIndentBetweenRegions));
end;

procedure AddSize(AElementSize: Integer; AScaleFactor: TdxScaleFactor; var ALayoutSize: Integer);
begin
  Inc(ALayoutSize, AdjustSizeByIndent(AElementSize, AScaleFactor, ALayoutSize > 0));
end;

function GetTaskBarPosition: Cardinal;
var
  AAppBarData: TAppBarData;
begin
  AAppBarData.cbSize := SizeOf(TAppBarData);
  if SHAppBarMessage(ABM_GETTASKBARPOS, AAppBarData) <> 0 then
    Result := AAppBarData.uEdge
  else
    Result := ABE_BOTTOM;
end;

function GetSuitableHeightForText(AFont: TFont; const AText: String; AWidth: Integer; AFormat: Cardinal): Integer;
var
  ARect: TRect;
  ATextRows: TcxTextRows;
  ATextParams: TcxTextParams;
begin
  if (AWidth > 0) then
  begin
    ARect := Rect(0, 0, AWidth, MAXWORD);
    cxScreenCanvas.Font := AFont;
    ATextParams := cxCalcTextParams(cxScreenCanvas.Canvas, AFormat);
    cxMakeTextRows(cxScreenCanvas.Canvas, PChar(AText), Length(AText), ARect, ATextParams, ATextRows, Result);
    Result := cxTextHeight(AFont) * Result;
    cxScreenCanvas.Dormant;
  end
  else
    Result := 0;
end;

{ TdxAlertWindowMessage }

constructor TdxAlertWindowMessage.Create(AOwner: TdxAlertWindowMessageList);
begin
  inherited Create;
  FOwner := AOwner;
  FImageIndex := -1;
end;

procedure TdxAlertWindowMessage.Changed;
begin
  if Owner <> nil then
    Owner.Changed;
end;

procedure TdxAlertWindowMessage.SetCaption(const AValue: string);
begin
  if FCaption <> AValue then
  begin
    FCaption := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowMessage.SetImageIndex(AValue: TcxImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowMessage.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Changed;
  end;
end;

{ TdxAlertWindowMessageList }

function TdxAlertWindowMessageList.Add: TdxAlertWindowMessage;
begin
  Result := TdxAlertWindowMessage.Create(Self);
  inherited Add(Result);
end;

function TdxAlertWindowMessageList.Add(const ACaption, AText: string; AImageIndex: TcxImageIndex = -1): TdxAlertWindowMessage;
begin
  Result := Add;
  Result.Caption := ACaption;
  Result.ImageIndex := AImageIndex;
  Result.Text := AText;
end;

procedure TdxAlertWindowMessageList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TdxAlertWindowMessageList.IsValid(AIndex: Integer): Boolean;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
end;

procedure TdxAlertWindowMessageList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    Changed;
end;

function TdxAlertWindowMessageList.GetItem(AIndex: Integer): TdxAlertWindowMessage;
begin
  Result := TdxAlertWindowMessage(inherited Items[AIndex]);
end;

{ TdxAlertWindowCustomOptions }

constructor TdxAlertWindowCustomOptions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotification;
end;

destructor TdxAlertWindowCustomOptions.Destroy;
begin
  FreeAndNil(FFreeNotificator);
  inherited Destroy;
end;

procedure TdxAlertWindowCustomOptions.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TdxAlertWindowCustomOptions.ChangeScale(M: Integer; D: Integer);
begin
  // do nothing
end;

procedure TdxAlertWindowCustomOptions.FreeNotification(Sender: TComponent);
begin
  // do nothing
end;

{ TdxAlertWindowOptionsAnimate }

constructor TdxAlertWindowOptionsAnimate.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FHotTrackAlphaBlendValue := dxAlertWindowDefaultHotTrackAlphaBlendValue;
  FAlphaBlendValue := dxAlertWindowDefaultAlphaBlendValue;
  FHidingAnimation := dxAlertWindowDefaultHidingAnimation;
  FHidingAnimationDirection := dxAlertWindowDefaultHidingAnimationDirection;
  FShowingAnimationDirection := dxAlertWindowDefaultShowingAnimationDirection;
  FHidingAnimationTime := dxAlertWindowDefaultHidingAnimationTime;
  FShowingAnimation := dxAlertWindowDefaultShowingAnimation;
  FShowingAnimationTime := dxAlertWindowDefaultShowingAnimationTime;
  FSizeAdjustmentAnimationTime := dxAlertWindowDefaultShowingAnimationTime;
  FHotTrack := dxAlertWindowDefaultHotTrack;
  FHotTrackFadeInTime := dxAlertWindowDefaultHotTrackFadeInTime;
  FHotTrackFadeOutTime := dxAlertWindowDefaultHotTrackFadeOutTime;
end;

procedure TdxAlertWindowOptionsAnimate.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsAnimate then
  begin
    HotTrackAlphaBlendValue := TdxAlertWindowOptionsAnimate(Source).HotTrackAlphaBlendValue;
    AlphaBlendValue := TdxAlertWindowOptionsAnimate(Source).AlphaBlendValue;
    HidingAnimation := TdxAlertWindowOptionsAnimate(Source).HidingAnimation;
    HidingAnimationTime := TdxAlertWindowOptionsAnimate(Source).HidingAnimationTime;
    ShowingAnimation := TdxAlertWindowOptionsAnimate(Source).ShowingAnimation;
    ShowingAnimationTime := TdxAlertWindowOptionsAnimate(Source).ShowingAnimationTime;
    HidingAnimationDirection := TdxAlertWindowOptionsAnimate(Source).HidingAnimationDirection;
    ShowingAnimationDirection := TdxAlertWindowOptionsAnimate(Source).ShowingAnimationDirection;
    SizeAdjustmentAnimationTime := TdxAlertWindowOptionsAnimate(Source).SizeAdjustmentAnimationTime;
    HotTrack := TdxAlertWindowOptionsAnimate(Source).HotTrack;
    HotTrackFadeInTime := TdxAlertWindowOptionsAnimate(Source).HotTrackFadeInTime;
    HotTrackFadeOutTime := TdxAlertWindowOptionsAnimate(Source).HotTrackFadeOutTime;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsAnimate.SetHotTrackAlphaBlendValue(AValue: Byte);
begin
  if FHotTrackAlphaBlendValue <> AValue then
  begin
    FHotTrackAlphaBlendValue := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetAlphaBlendValue(AValue: Byte);
begin
  if FAlphaBlendValue <> AValue then
  begin
    FAlphaBlendValue := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetHidingAnimation(AValue: TdxAlertWindowAnimation);
begin
  if FHidingAnimation <> AValue then
  begin
    FHidingAnimation := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetHidingAnimationDirection(
  AValue: TdxAlertWindowMovingDirection);
begin
  if FHidingAnimationDirection <> AValue then
  begin
    FHidingAnimationDirection := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetHidingAnimationTime(AValue: Cardinal);
begin
  if FHidingAnimationTime <> AValue then
  begin
    FHidingAnimationTime := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetHotTrack(AValue: Boolean);
begin
  if FHotTrack <> AValue then
  begin
    FHotTrack := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetHotTrackFadeInTime(AValue: Cardinal);
begin
  if FHotTrackFadeInTime <> AValue then
  begin
    FHotTrackFadeInTime := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetHotTrackFadeOutTime(AValue: Cardinal);
begin
  if FHotTrackFadeOutTime <> AValue then
  begin
    FHotTrackFadeOutTime := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetSizeAdjustmentAnimationTime(
  AValue: Cardinal);
begin
  if FSizeAdjustmentAnimationTime <> AValue then
  begin
    FSizeAdjustmentAnimationTime := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetShowingAnimation(AValue: TdxAlertWindowAnimation);
begin
  if FShowingAnimation <> AValue then
  begin
    FShowingAnimation := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetShowingAnimationDirection(
  AValue: TdxAlertWindowMovingDirection);
begin
  if FShowingAnimationDirection <> AValue then
  begin
    FShowingAnimationDirection := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsAnimate.SetShowingAnimationTime(AValue: Cardinal);
begin
  if FShowingAnimationTime <> AValue then
  begin
    FShowingAnimationTime := AValue;
    Changed;
  end;
end;

{ TdxAlertWindowOptionsBehavior }

constructor TdxAlertWindowOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCloseOnRightClick := dxAlertWindowDefaultCloseOnRightClick;
  FDisplayTime := dxAlertWindowDefaultDisplayTime;
  FScreenSnap := dxAlertWindowDefaultScreenSnap;
  FScreenSnapBuffer := dxAlertWindowDefaultScreenSnapBuffer;
end;

procedure TdxAlertWindowOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsBehavior then
  begin
    CloseOnRightClick := TdxAlertWindowOptionsBehavior(Source).CloseOnRightClick;
    DisplayTime := TdxAlertWindowOptionsBehavior(Source).DisplayTime;
    ScreenSnap := TdxAlertWindowOptionsBehavior(Source).ScreenSnap;
    ScreenSnapBuffer := TdxAlertWindowOptionsBehavior(Source).ScreenSnapBuffer;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsBehavior.ChangeScale(M: Integer; D: Integer);
begin
  FScreenSnapBuffer := MulDiv(FScreenSnapBuffer, M, D);
end;

procedure TdxAlertWindowOptionsBehavior.SetCloseOnRightClick(AValue: Boolean);
begin
  if FCloseOnRightClick <> AValue then
  begin
    FCloseOnRightClick := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsBehavior.SetDisplayTime(AValue: Cardinal);
begin
  if FDisplayTime <> AValue then
  begin
    FDisplayTime := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsBehavior.SetScreenSnap(AValue: Boolean);
begin
  if FScreenSnap <> AValue then
  begin
    FScreenSnap := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsBehavior.SetScreenSnapBuffer(AValue: Integer);
begin
  if FScreenSnapBuffer <> AValue then
  begin
    FScreenSnapBuffer := AValue;
    Changed;
  end;
end;

{ TdxAlertWindowOptionsText }

constructor TdxAlertWindowOptionsText.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.OnChange := DoFontChanged;
  FAlignHorz := dxAlertWindowDefaultTextAlignHorz;
  FAlignVert := dxAlertWindowDefaultTextAlignVert;
end;

destructor TdxAlertWindowOptionsText.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxAlertWindowOptionsText.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsText then
  begin
    Font := TdxAlertWindowOptionsText(Source).Font;
    AlignHorz := TdxAlertWindowOptionsText(Source).AlignHorz;
    AlignVert := TdxAlertWindowOptionsText(Source).AlignVert;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsText.ChangeScale(M: Integer; D: Integer);
begin
  FFont.Height := MulDiv(FFont.Height, M, D);
end;

procedure TdxAlertWindowOptionsText.DoFontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxAlertWindowOptionsText.SetAlignHorz(AValue: TAlignment);
begin
  if FAlignHorz <> AValue then
  begin
    FAlignHorz := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsText.SetAlignVert(AValue: TcxAlignmentVert);
begin
  if FAlignVert <> AValue then
  begin
    FAlignVert := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsText.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

{ TdxAlertWindowOptionsMessage }

constructor TdxAlertWindowOptionsMessage.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCaption := TdxAlertWindowOptionsText.Create(Self);
  FCaption.OnChange := DoOptionTextChange;
  FText := TdxAlertWindowOptionsText.Create(Self);
  FText.OnChange := DoOptionTextChange;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
  Caption.Font.Size := dxAlertWindowDefaultCaptionFontSize;
  Caption.Font.Style := dxAlertWindowDefaultCaptionFontStyle;
end;

destructor TdxAlertWindowOptionsMessage.Destroy;
begin
  FreeAndNil(FChangeLink);
  FreeAndNil(FText);
  FreeAndNil(FCaption);
  inherited Destroy;
end;

procedure TdxAlertWindowOptionsMessage.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsMessage then
  begin
    Caption := TdxAlertWindowOptionsMessage(Source).Caption;
    Text := TdxAlertWindowOptionsMessage(Source).Text;
    Images := TdxAlertWindowOptionsMessage(Source).Images;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsMessage.ChangeScale(M: Integer; D: Integer);
begin
  FCaption.ChangeScale(M, D);
  FText.ChangeScale(M, D);
end;

procedure TdxAlertWindowOptionsMessage.DoOptionTextChange(Sender: TObject);
begin
  Changed;
end;

procedure TdxAlertWindowOptionsMessage.FreeNotification(Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = Images then
    Images := nil;
end;

procedure TdxAlertWindowOptionsMessage.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    Changed;
end;

procedure TdxAlertWindowOptionsMessage.SetCaption(AValue: TdxAlertWindowOptionsText);
begin
  FCaption.Assign(AValue);
end;

procedure TdxAlertWindowOptionsMessage.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FChangeLink, FreeNotificator);
end;

procedure TdxAlertWindowOptionsMessage.SetText(AValue: TdxAlertWindowOptionsText);
begin
  FText.Assign(AValue);
end;

{ TdxAlertWindowOptionsNavigationPanel }

constructor TdxAlertWindowOptionsNavigationPanel.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  Font.OnChange := DoFontChanged;
  FDisplayMask := cxGetResourceString(@sdxAlertWindowNavigationPanelDefaultDisplayMask);
  FVisibility := dxAlertWindowDefaultNavigationPanelVisibility;
  FWidth := dxAlertWindowDefaultNavigationPanelWidth;
end;

destructor TdxAlertWindowOptionsNavigationPanel.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TdxAlertWindowOptionsNavigationPanel.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsNavigationPanel then
  begin
    Font := TdxAlertWindowOptionsNavigationPanel(Source).Font;
    DisplayMask := TdxAlertWindowOptionsNavigationPanel(Source).DisplayMask;
    Visibility := TdxAlertWindowOptionsNavigationPanel(Source).Visibility;
    Width := TdxAlertWindowOptionsNavigationPanel(Source).Width;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsNavigationPanel.ChangeScale(M: Integer; D: Integer);
begin
  FFont.Height := MulDiv(FFont.Height, M, D);
  FWidth := MulDiv(FWidth, M, D);
end;

procedure TdxAlertWindowOptionsNavigationPanel.DoFontChanged(Sender: TObject);
begin
  Changed;
end;

function TdxAlertWindowOptionsNavigationPanel.IsDisplayMaskStored: Boolean;
begin
  Result := cxGetResourceString(@sdxAlertWindowNavigationPanelDefaultDisplayMask) <> DisplayMask;
end;

procedure TdxAlertWindowOptionsNavigationPanel.SetDisplayMask(const AValue: string);
begin
  if FDisplayMask <> AValue then
  begin
    FDisplayMask := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsNavigationPanel.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
end;

procedure TdxAlertWindowOptionsNavigationPanel.SetVisibility(AValue: TdxAlertWindowNavigationPanelVisibility);
begin
  if FVisibility <> AValue then
  begin
    FVisibility := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsNavigationPanel.SetWidth(AValue: Integer);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Changed;
  end;
end;

{ TdxAlertWindowOptionsCaptionButtons }

constructor TdxAlertWindowOptionsCaptionButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCaptionButtons := dxAlertWindowDefaultCaptionButtons;
end;

procedure TdxAlertWindowOptionsCaptionButtons.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsCaptionButtons then
  begin
    PopupMenu := TdxAlertWindowOptionsCaptionButtons(Source).PopupMenu;
    CaptionButtons := TdxAlertWindowOptionsCaptionButtons(Source).CaptionButtons;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsCaptionButtons.FreeNotification(Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = PopupMenu then
    PopupMenu := nil;
end;

procedure TdxAlertWindowOptionsCaptionButtons.SetCaptionButtons(AValue: TdxAlertWindowCaptionButtons);
begin
  if FCaptionButtons <> AValue then
  begin
    FCaptionButtons := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsCaptionButtons.SetPopupMenu(AValue: TComponent);
begin
  if not IsPopupMenu(AValue) then
    AValue := nil;

  if FPopupMenu <> AValue then
  begin
    cxRemoveFreeNotification(FreeNotificator, FPopupMenu);
    FPopupMenu := AValue;
    cxAddFreeNotification(FreeNotificator, FPopupMenu);
    Changed;
  end;
end;

{ TdxAlertWindowOptionsSize }

constructor TdxAlertWindowOptionsSize.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FAutoHeight := dxAlertWindowDefaultAutoHeight;
  FAutoWidth := dxAlertWindowDefaultAutoWidth;
  FAutoSizeAdjustment := dxAlertWindowDefaultAutoSizeAdjustment;
  FMaxHeight := dxAlertWindowDefaultMaxHeight;
  FMaxWidth := dxAlertWindowDefaultMaxWidth;
  FMinHeight := dxAlertWindowDefaultMinHeight;
  FMinWidth := dxAlertWindowDefaultMinWidth;
  FWidth := dxAlertWindowDefaultMinWidth;
  FHeight := dxAlertWindowDefaultMinHeight;
end;

procedure TdxAlertWindowOptionsSize.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsSize then
  begin
    AutoHeight := TdxAlertWindowOptionsSize(Source).AutoHeight;
    AutoWidth := TdxAlertWindowOptionsSize(Source).AutoWidth;
    AutoSizeAdjustment := TdxAlertWindowOptionsSize(Source).AutoSizeAdjustment;
    MaxHeight := TdxAlertWindowOptionsSize(Source).MaxHeight;
    MaxWidth := TdxAlertWindowOptionsSize(Source).MaxWidth;
    MinHeight := TdxAlertWindowOptionsSize(Source).MinHeight;
    MinWidth := TdxAlertWindowOptionsSize(Source).MinWidth;
    Width := TdxAlertWindowOptionsSize(Source).Width;
    Height := TdxAlertWindowOptionsSize(Source).Height;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsSize.ChangeScale(M: Integer; D: Integer);
begin
  FHeight := MulDiv(FHeight, M, D);
  FMaxHeight := MulDiv(FMaxHeight, M, D);
  FMaxWidth := MulDiv(FMaxWidth, M, D);
  FMinHeight := MulDiv(FMinHeight, M, D);
  FMinWidth := MulDiv(FMinWidth, M, D);
  FWidth := MulDiv(FWidth, M, D);
end;

procedure TdxAlertWindowOptionsSize.SetAutoHeight(AValue: Boolean);
begin
  if FAutoHeight <> AValue then
  begin
    FAutoHeight := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetAutoSizeAdjustment(AValue: Boolean);
begin
  if FAutoSizeAdjustment <> AValue then
  begin
    FAutoSizeAdjustment := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetAutoWidth(AValue: Boolean);
begin
  if FAutoWidth <> AValue then
  begin
    FAutoWidth := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetHeight(AValue: Integer);
begin
  AValue := Max(MinHeight, AValue);
  if MaxHeight > 0 then
    AValue := Min(MaxHeight, AValue);

  if FHeight <> AValue then
  begin
    FHeight := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetMaxHeight(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FMaxHeight <> AValue then
  begin
    FMaxHeight := AValue;
    if MaxHeight > 0 then
    begin
      FMinHeight := Min(MinHeight, MaxHeight);
      FHeight := Min(Height, MaxHeight);
    end;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetMaxWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FMaxWidth <> AValue then
  begin
    FMaxWidth := AValue;
    if MaxWidth > 0 then
    begin
      FMinWidth := Min(MinWidth, MaxWidth);
      FWidth := Min(Width, MaxWidth);
    end;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetMinHeight(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FMinHeight <> AValue then
  begin
    FMinHeight := AValue;
    if MaxHeight > 0 then
      FMaxHeight := Max(MaxHeight, MinHeight);

    FHeight := Max(Height, MinHeight);
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetMinWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FMinWidth <> AValue then
  begin
    FMinWidth := AValue;
    if MaxWidth > 0 then
      FMaxWidth := Max(MaxWidth, MinWidth);

    FWidth := Max(Width, MinWidth);
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsSize.SetWidth(AValue: Integer);
begin
  AValue := Max(MinWidth, AValue);
  if MaxWidth > 0 then
    AValue := Min(MaxWidth, AValue);

  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Changed;
  end;
end;

{ TdxAlertWindowButton }

constructor TdxAlertWindowButton.Create(Collection: TCollection);
begin
  Collection.BeginUpdate;
  try
    inherited Create(Collection);
    FEnabled := True;
    FVisible := True;
    FImageIndex := -1;
  finally
    Collection.EndUpdate;
  end;
end;

procedure TdxAlertWindowButton.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowButton then
  begin
    Enabled := TdxAlertWindowButton(Source).Enabled;
    Visible := TdxAlertWindowButton(Source).Visible;
    Hint := TdxAlertWindowButton(Source).Hint;
    ImageIndex := TdxAlertWindowButton(Source).ImageIndex;
  end
  else
    inherited Assign(Source);
end;

function TdxAlertWindowButton.GetCollection: TdxAlertWindowButtons;
begin
  Result := TdxAlertWindowButtons(inherited Collection);
end;

procedure TdxAlertWindowButton.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    Changed(False);
  end;
end;

procedure TdxAlertWindowButton.SetHint(const AValue: string);
begin
  if FHint <> AValue then
  begin
    FHint := AValue;
    Changed(False);
  end;
end;

procedure TdxAlertWindowButton.SetImageIndex(AValue: TcxImageIndex);
begin
  if FImageIndex <> AValue then
  begin
    FImageIndex := AValue;
    Changed(False);
  end;
end;

procedure TdxAlertWindowButton.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(False);
  end;
end;

{ TdxAlertWindowButtons }

constructor TdxAlertWindowButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, GetButtonClass);
end;

function TdxAlertWindowButtons.Add: TdxAlertWindowButton;
begin
  Result := TdxAlertWindowButton(inherited Add);
end;

class function TdxAlertWindowButtons.GetButtonClass: TdxAlertWindowButtonClass;
begin
  Result := TdxAlertWindowButton;
end;

procedure TdxAlertWindowButtons.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  TdxAlertWindowOptionsButtons(Owner).Changed;
end;

function TdxAlertWindowButtons.GetItem(Index: Integer): TdxAlertWindowButton;
begin
  Result := TdxAlertWindowButton(inherited GetItem(Index));
end;

procedure TdxAlertWindowButtons.SetItem(Index: Integer; AValue: TdxAlertWindowButton);
begin
  inherited SetItem(Index, AValue);
end;

{ TdxAlertWindowOptionsButtons }

constructor TdxAlertWindowOptionsButtons.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := ImageListChange;
  FButtons := TdxAlertWindowButtons.Create(Self);
  FWidth := dxAlertWindowDefaultButtonWidth;
  FHeight := dxAlertWindowDefaultButtonHeight;
end;

destructor TdxAlertWindowOptionsButtons.Destroy;
begin
  FreeAndNil(FButtons);
  FreeAndNil(FChangeLink);
  inherited Destroy;
end;

procedure TdxAlertWindowOptionsButtons.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowOptionsButtons then
  begin
    Buttons := TdxAlertWindowOptionsButtons(Source).Buttons;
    Height := TdxAlertWindowOptionsButtons(Source).Height;
    Width := TdxAlertWindowOptionsButtons(Source).Width;
    Images := TdxAlertWindowOptionsButtons(Source).Images;
  end
  else
    inherited Assign(Source);
end;

procedure TdxAlertWindowOptionsButtons.ChangeScale(M: Integer; D: Integer);
begin
  FHeight := MulDiv(FHeight, M, D);
  FWidth := MulDiv(FWidth, M, D);
end;

procedure TdxAlertWindowOptionsButtons.FreeNotification(Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = Images then
    Images := nil;
end;

procedure TdxAlertWindowOptionsButtons.ImageListChange(Sender: TObject);
begin
  if Sender = Images then
    Changed;
end;

procedure TdxAlertWindowOptionsButtons.SetButtons(AValue: TdxAlertWindowButtons);
begin
  FButtons.Assign(AValue);
end;

procedure TdxAlertWindowOptionsButtons.SetHeight(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    Changed;
  end;
end;

procedure TdxAlertWindowOptionsButtons.SetImages(AValue: TCustomImageList);
begin
  cxSetImageList(AValue, FImages, FChangeLink, FreeNotificator);
end;

procedure TdxAlertWindowOptionsButtons.SetWidth(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Changed;
  end;
end;

{ TdxAlertWindowCustomViewInfo }

constructor TdxAlertWindowCustomViewInfo.Create(AOwner: IdxAlertWindow);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxAlertWindowCustomViewInfo.Calculate(const ABounds: TRect);
begin
  FBounds := ABounds;
end;

function TdxAlertWindowCustomViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  Result := 0;
end;

function TdxAlertWindowCustomViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  Result := 0;
end;

procedure TdxAlertWindowCustomViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds);
end;

procedure TdxAlertWindowCustomViewInfo.Draw(ACanvas: TcxCanvas);
begin
  if not CustomDraw(ACanvas) then
    DoDraw(ACanvas);
end;

function TdxAlertWindowCustomViewInfo.GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo;
begin
  if PtInRect(Bounds, APoint) then
    Result := Self
  else
    Result := nil;
end;

procedure TdxAlertWindowCustomViewInfo.Invalidate;
begin
  Owner.InvalidateRect(Bounds);
end;

procedure TdxAlertWindowCustomViewInfo.RecreateViewInfo;
begin
// do nothing
end;

function TdxAlertWindowCustomViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := False;
end;

function TdxAlertWindowCustomViewInfo.GetHint: string;
begin
  Result := '';
end;

function TdxAlertWindowCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.GetScaleFactor;
end;

function TdxAlertWindowCustomViewInfo.GetController: TdxAlertWindowController;
begin
  Result := Owner.GetController;
end;

function TdxAlertWindowCustomViewInfo.GetCurrentMessage: TdxAlertWindowMessage;
begin
  if (Messages <> nil) and Messages.IsValid(MessageIndex) then
    Result := Messages.Items[MessageIndex]
  else
    Result := nil;
end;

function TdxAlertWindowCustomViewInfo.GetMessageCount: Integer;
begin
  if Messages <> nil then
    Result := Messages.Count
  else
    Result := -1;
end;

function TdxAlertWindowCustomViewInfo.GetMessageIndex: Integer;
begin
  Result := Owner.CurrentMessageIndex;
end;

function TdxAlertWindowCustomViewInfo.GetMessages: TdxAlertWindowMessageList;
begin
  Result := Owner.GetMessages;
end;

function TdxAlertWindowCustomViewInfo.GetOptionsButtons: TdxAlertWindowOptionsButtons;
begin
  Result := Owner.GetOptionsButtons;
end;

function TdxAlertWindowCustomViewInfo.GetOptionsMessage: TdxAlertWindowOptionsMessage;
begin
  Result := Owner.GetOptionsMessage;
end;

function TdxAlertWindowCustomViewInfo.GetOptionsNavigationPanel:
  TdxAlertWindowOptionsNavigationPanel;
begin
  Result := Owner.GetOptionsNavigationPanel;
end;

function TdxAlertWindowCustomViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.GetLookAndFeel.Painter;
end;

{ TdxAlertWindowCustomTextViewInfo }

procedure TdxAlertWindowCustomTextViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  IsTextClipped := GetIsTextClipped;
end;

procedure TdxAlertWindowCustomTextViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  ARect: TRect;
begin
  ARect := Bounds;
  cxTextOut(ACanvas.Canvas, Text, ARect, DrawTextFlags);
end;

procedure TdxAlertWindowCustomTextViewInfo.Draw(ACanvas: TcxCanvas);
begin
  ACanvas.Font := Font;
  if ACanvas.Font.Color = clWindowText then
    ACanvas.Font.Color := GetDefaultTextColor;
  inherited Draw(ACanvas);
end;

function TdxAlertWindowCustomTextViewInfo.FormatText(AAlignHorz: TAlignment; AAlignVert: TcxAlignmentVert): Cardinal;
const
  AlignHorzMap: array[TAlignment] of Cardinal = (CXTO_LEFT, CXTO_RIGHT, CXTO_CENTER_HORIZONTALLY);
  AlignVertMap: array[TcxAlignmentVert] of Cardinal = (CXTO_TOP, CXTO_BOTTOM, CXTO_CENTER_VERTICALLY);
begin
  Result := AlignHorzMap[AAlignHorz] or AlignVertMap[AAlignVert];
end;

function TdxAlertWindowCustomTextViewInfo.GetActualAlignHorz(AAlignHorz: TAlignment): TAlignment;
begin
  Result := AAlignHorz;
  if Owner.GetViewInfo.UseRightToLeftAlignment then
    ChangeBiDiModeAlignment(Result);
end;

function TdxAlertWindowCustomTextViewInfo.GetDefaultTextColor: TColor;
begin
  Result := Painter.AlertWindowTextColor;
end;

function TdxAlertWindowCustomTextViewInfo.GetHint: String;
begin
  if IsTextClipped then
    Result := Text
  else
    Result := '';
end;

function TdxAlertWindowCustomTextViewInfo.GetIsTextClipped: Boolean;
begin
  Result := False;
end;

{ TdxAlertWindowCustomButtonViewInfo }

function TdxAlertWindowCustomButtonViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  Result := GlyphSize.cy + cxMarginsHeight(ContentOffsets);
end;

function TdxAlertWindowCustomButtonViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  Result := GlyphSize.cx + cxMarginsWidth(ContentOffsets);
end;

function TdxAlertWindowCustomButtonViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := Owner.DoCustomDrawButton(ACanvas, Self);
end;

procedure TdxAlertWindowCustomButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  Painter.DrawAlertWindowScaledButton(ACanvas, Bounds, State, GetActualDrawingKind, ScaleFactor);
end;

function TdxAlertWindowCustomButtonViewInfo.GetActualDrawingKind: TdxAlertWindowButtonKind;
begin
  Result := Kind;
  if Owner.GetViewInfo.UseRightToLeftAlignment then
  case Result of
    awbkPrevious: Result := awbkNext;
    awbkNext:     Result := awbkPrevious;
  end;
end;

function TdxAlertWindowCustomButtonViewInfo.GetContentOffsets: TRect;
begin
  Result := Painter.AlertWindowScaledButtonContentOffsets(Kind, ScaleFactor);
end;

function TdxAlertWindowCustomButtonViewInfo.GetEnabled: Boolean;
begin
  Result := True;
end;

function TdxAlertWindowCustomButtonViewInfo.GetGlyphSize: TSize;
begin
  Result := Painter.AlertWindowScaledButtonGlyphSize(Kind, ScaleFactor);
end;

function TdxAlertWindowCustomButtonViewInfo.GetState: TcxButtonState;
const
  PressedMap: array[Boolean] of TcxButtonState = (cxbsHot, cxbsPressed);
begin
  if not Enabled then
    Result := cxbsDisabled
  else
    if Controller.HotButton = Self then
      Result := PressedMap[Controller.PressedButton = Self]
    else
      Result := cxbsNormal;
end;

{ TdxAlertWindowPreviousButtonViewInfo }

procedure TdxAlertWindowPreviousButtonViewInfo.Click;
begin
  Controller.PreviousMessage;
end;

function TdxAlertWindowPreviousButtonViewInfo.GetEnabled: Boolean;
begin
  if MessageIndex > 0 then
    Result := inherited GetEnabled
  else
    Result := False;
end;

function TdxAlertWindowPreviousButtonViewInfo.GetHint: String;
begin
  Result := cxGetResourceString(@sdxAlertWindowPreviousMessage);
end;

function TdxAlertWindowPreviousButtonViewInfo.GetKind: TdxAlertWindowButtonKind;
begin
  Result := awbkPrevious;
end;

{ TdxAlertWindowNextButtonViewInfo }

procedure TdxAlertWindowNextButtonViewInfo.Click;
begin
  Controller.NextMessage;
end;

function TdxAlertWindowNextButtonViewInfo.GetEnabled: Boolean;
begin
  if MessageIndex < MessageCount - 1 then
    Result := inherited GetEnabled
  else
    Result := False;
end;

function TdxAlertWindowNextButtonViewInfo.GetHint: String;
begin
  Result := cxGetResourceString(@sdxAlertWindowNextMessage);
end;

function TdxAlertWindowNextButtonViewInfo.GetKind: TdxAlertWindowButtonKind;
begin
  Result := awbkNext;
end;

{ TdxAlertWindowCloseButtonViewInfo }

procedure TdxAlertWindowCloseButtonViewInfo.Click;
begin
  Controller.Close;
end;

function TdxAlertWindowCloseButtonViewInfo.GetHint: String;
begin
  Result := cxGetResourceString(@sdxAlertWindowClose);
end;

function TdxAlertWindowCloseButtonViewInfo.GetKind: TdxAlertWindowButtonKind;
begin
  Result := awbkClose;
end;

{ TdxAlertWindowPinButtonViewInfo }

procedure TdxAlertWindowPinButtonViewInfo.Click;
begin
  Controller.TogglePin;
end;

procedure TdxAlertWindowPinButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  Painter.DrawAlertWindowScaledButton(ACanvas, Bounds, State, Kind, ScaleFactor, Pinned);
end;

function TdxAlertWindowPinButtonViewInfo.GetHint: String;
begin
  Result := cxGetResourceString(@sdxAlertWindowPin);
end;

function TdxAlertWindowPinButtonViewInfo.GetKind: TdxAlertWindowButtonKind;
begin
  Result := awbkPin;
end;

function TdxAlertWindowPinButtonViewInfo.GetPinned: Boolean;
begin
  Result := Owner.Pinned;
end;

{ TdxAlertWindowDropdownButtonViewInfo }

procedure TdxAlertWindowDropdownButtonViewInfo.Click;
begin
  Controller.ShowPopupMenu;
end;

function TdxAlertWindowDropdownButtonViewInfo.GetEnabled: Boolean;
begin
  Result := Owner.GetOptionsCaptionButtons.PopupMenu <> nil;
end;

function TdxAlertWindowDropdownButtonViewInfo.GetHint: String;
begin
  Result := cxGetResourceString(@sdxAlertWindowDropdown);
end;

function TdxAlertWindowDropdownButtonViewInfo.GetKind: TdxAlertWindowButtonKind;
begin
  Result := awbkDropdown;
end;

function TdxAlertWindowDropdownButtonViewInfo.GetState: TcxButtonState;
begin
  if Owner.IsPopupMenuShown then
    Result := cxbsPressed
  else
    Result := inherited GetState;
end;

{ TdxAlertWindowButtonViewInfo }

constructor TdxAlertWindowButtonViewInfo.Create(AOwner: IdxAlertWindow; AButtonItem: TdxAlertWindowButton);
begin
  inherited Create(AOwner);
  FButtonItem := AButtonItem;
end;

function TdxAlertWindowButtonViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  Result := OptionsButtons.Height;
  if Result = 0 then
    Result := inherited CalculateAutoHeight(AWidth);
end;

function TdxAlertWindowButtonViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  Result := OptionsButtons.Width;
  if Result = 0 then
    Result := Max(Result, inherited CalculateAutoWidth);
end;

procedure TdxAlertWindowButtonViewInfo.Click;
begin
  Controller.ButtonsClick(ButtonItem.Index);
end;

procedure TdxAlertWindowButtonViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  inherited DoDraw(ACanvas);
  if IsImageAssigned then
    cxDrawImage(ACanvas.Handle, cxRectCenter(Bounds, GlyphSize), Bounds, nil, OptionsButtons.Images,
      ButtonItem.ImageIndex, idmNormal, False, 0, clDefault, True, Painter.AlertWindowButtonGetColorPalette(State));
end;

function TdxAlertWindowButtonViewInfo.GetEnabled: Boolean;
begin
  Result := ButtonItem.Enabled;
end;

function TdxAlertWindowButtonViewInfo.GetGlyphSize: TSize;
begin
  if IsImageAssigned then
    Result := dxGetImageSize(OptionsButtons.Images, ScaleFactor)
  else
    Result := Inherited GetGlyphSize;
end;

function TdxAlertWindowButtonViewInfo.GetHint: string;
begin
  if Enabled then
    Result := ButtonItem.Hint
  else
    Result := '';
end;

function TdxAlertWindowButtonViewInfo.GetKind: TdxAlertWindowButtonKind;
begin
  Result := awbkCustom;
end;

function TdxAlertWindowButtonViewInfo.GetIsImageAssigned: Boolean;
begin
  Result := cxGraphics.IsImageAssigned(OptionsButtons.Images, ButtonItem.ImageIndex);
end;

{ TdxAlertWindowButtonsViewInfoList }

function TdxAlertWindowButtonsViewInfoList.Add(AButton: TdxAlertWindowCustomButtonViewInfo): Integer;
begin
  Result := inherited Add(AButton);
end;

function TdxAlertWindowButtonsViewInfoList.Add(AOwner: IdxAlertWindow; AButtonItem: TdxAlertWindowButton): Integer;
begin
  Result := Add(TdxAlertWindowButtonViewInfo.Create(AOwner, AButtonItem));
end;

function TdxAlertWindowButtonsViewInfoList.Add(
  AOwner: IdxAlertWindow; AButtonViewInfoClass: TdxAlertWindowCustomButtonViewInfoClass): Integer;
begin
  Result := Add(AButtonViewInfoClass.Create(AOwner));
end;

function TdxAlertWindowButtonsViewInfoList.GetItem(AIndex: Integer): TdxAlertWindowCustomButtonViewInfo;
begin
  Result := TdxAlertWindowCustomButtonViewInfo(inherited Items[AIndex]);
end;

{ TdxAlertWindowCustomButtonsViewInfo }

constructor TdxAlertWindowCustomButtonsViewInfo.Create(AOwner: IdxAlertWindow);
begin
  inherited Create(AOwner);
  FButtons := TdxAlertWindowButtonsViewInfoList.Create;
end;

destructor TdxAlertWindowCustomButtonsViewInfo.Destroy;
begin
  FButtons.Clear;
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TdxAlertWindowCustomButtonsViewInfo.Calculate(const ABounds: TRect);
var
  I: Integer;
  R: TRect;
begin
  inherited Calculate(ABounds);
  R := ABounds;
  for I := 0 to ButtonsCount - 1 do
  begin
    R.Right := R.Left + GetButtonSize(I).cx;
    Buttons[I].Calculate(R);
    R.Left := R.Right;
  end;
end;

function TdxAlertWindowCustomButtonsViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ButtonsCount - 1 do
    Result := Max(Result, GetButtonSize(I).cy);
end;

function TdxAlertWindowCustomButtonsViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ButtonsCount - 1 do
    Result := Result + GetButtonSize(I).cx;
end;

function TdxAlertWindowCustomButtonsViewInfo.GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo;
var
  I: Integer;
begin
  Result := nil;
  if cxRectPtIn(Bounds, APoint) then
  begin
    for I := 0 to ButtonsCount - 1 do
    begin
      Result := Buttons[I].GetHitTest(APoint);
      if Result <> nil then Break;
    end;
    if Result = nil then
      Result := inherited GetHitTest(APoint);
  end;
end;

procedure TdxAlertWindowCustomButtonsViewInfo.RecreateViewInfo;
var
  I: Integer;
begin
  inherited RecreateViewInfo;
  for I := 0 to Buttons.Count - 1 do
  begin
    if Controller.HotButton = Buttons[I] then
      Controller.HotButton := nil;

    if Controller.PressedButton = Buttons[I] then
      Controller.PressedButton := nil;
  end;
  Buttons.Clear;
end;

procedure TdxAlertWindowCustomButtonsViewInfo.DoDraw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to ButtonsCount - 1 do
    Buttons.Items[I].Draw(ACanvas);
end;

procedure TdxAlertWindowCustomButtonsViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(AClientBounds);
  for I := 0 to ButtonsCount - 1 do
    Buttons.Items[I].DoRightToLeftConversion(AClientBounds);
end;

function TdxAlertWindowCustomButtonsViewInfo.GetButtonsCount: Integer;
begin
  Result := Buttons.Count;
end;

function TdxAlertWindowCustomButtonsViewInfo.GetButtonSize(ANumButton: Integer): TSize;
begin
  Result.cx := Buttons[ANumButton].CalculateAutoWidth;
  Result.cy := Buttons[ANumButton].CalculateAutoHeight(Result.cx);
end;

{ TdxAlertWindowButtonsViewInfo }

procedure TdxAlertWindowButtonsViewInfo.RecreateViewInfo;
var
  I: Integer;
begin
  inherited RecreateViewInfo;
  for I := 0 to OptionsButtons.Buttons.Count - 1 do
    if OptionsButtons.Buttons[I].Visible then
      Buttons.Add(Owner, OptionsButtons.Buttons[I]);
end;

{ TdxAlertWindowCaptionButtonsViewInfo }

procedure TdxAlertWindowCaptionButtonsViewInfo.RecreateViewInfo;
begin
  inherited RecreateViewInfo;
  if (awcbDropdown in Owner.GetOptionsCaptionButtons.CaptionButtons) then
    Buttons.Add(Owner, TdxAlertWindowDropdownButtonViewInfo);

  if (awcbPin in Owner.GetOptionsCaptionButtons.CaptionButtons) then
    Buttons.Add(Owner, TdxAlertWindowPinButtonViewInfo);

  if (awcbClose in Owner.GetOptionsCaptionButtons.CaptionButtons) then
    Buttons.Add(Owner, TdxAlertWindowCloseButtonViewInfo);
end;

{ TdxAlertWindowImageViewInfo }

function TdxAlertWindowImageViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  if IsImageAssigned then
    Result := dxGetImageSize(OptionsMessage.Images, Owner.GetScaleFactor).cy
  else
    Result := 0;
end;

function TdxAlertWindowImageViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  if IsImageAssigned then
    Result := dxGetImageSize(OptionsMessage.Images, Owner.GetScaleFactor).cx
  else
    Result := 0;
end;

function TdxAlertWindowImageViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := Owner.DoCustomDrawMessageImage(ACanvas, Self);
end;

procedure TdxAlertWindowImageViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if IsImageAssigned then
    cxDrawImage(ACanvas, Bounds, nil, OptionsMessage.Images, ImageIndex, ifmNormal, idmNormal, True, nil, Owner.GetScaleFactor);
end;

function TdxAlertWindowImageViewInfo.GetImageIndex: TcxImageIndex;
begin
  if CurrentMessage <> nil then
    Result := CurrentMessage.ImageIndex
  else
    Result := -1;
end;

function TdxAlertWindowImageViewInfo.GetIsImageAssigned: Boolean;
begin
  Result := cxGraphics.IsImageAssigned(OptionsMessage.Images, ImageIndex);
end;

{ TdxAlertWindowCaptionTextViewInfo }

function TdxAlertWindowCaptionTextViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  Result := cxTextExtent(Font, Text).cy;
end;

function TdxAlertWindowCaptionTextViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  Result := cxTextExtent(Font, Text).cx;
end;

function TdxAlertWindowCaptionTextViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := Owner.DoCustomDrawMessageCaptionText(ACanvas, Self);
end;

function TdxAlertWindowCaptionTextViewInfo.GetDrawTextFlags: Cardinal;
begin
  Result := FormatText(GetActualAlignHorz(OptionsMessage.Caption.AlignHorz), OptionsMessage.Caption.AlignVert) or
    CXTO_END_ELLIPSIS or CXTO_SINGLELINE;
  if Owner.GetViewInfo.UseRightToLeftReading then
    Result := Result or CXTO_RTLREADING;
end;

function TdxAlertWindowCaptionTextViewInfo.GetFont: TFont;
begin
  Result := OptionsMessage.Caption.Font;
end;

function TdxAlertWindowCaptionTextViewInfo.GetIsTextClipped: Boolean;
begin
  Result := cxRectWidth(Bounds) < CalculateAutoWidth;
end;

function TdxAlertWindowCaptionTextViewInfo.GetText: String;
begin
  if CurrentMessage <> nil then
    Result := CurrentMessage.Caption
  else
    Result := '';
end;

{ TdxAlertWindowMessageTextViewInfo }

function TdxAlertWindowMessageTextViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
var
  ARect: TRect;
  ATextRows: TcxTextRows;
  ATextParams: TcxTextParams;
begin
  if (AWidth > 0) then
  begin
    ARect := Rect(0, 0, AWidth, MAXWORD);
    cxScreenCanvas.Font := Font;
    ATextParams := cxCalcTextParams(cxScreenCanvas.Canvas, CXTO_WORDBREAK or CXTO_EDITCONTROL or CXTO_END_ELLIPSIS);
    cxMakeTextRows(cxScreenCanvas.Canvas, PChar(Text), Length(Text), ARect, ATextParams, ATextRows, Result);
    Result := cxTextHeight(Font) * Result;
    cxScreenCanvas.Dormant;
  end
  else
    Result := 0;
end;

function TdxAlertWindowMessageTextViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  if (AHeight = -1) or (Text = '') then
    Result := 0
  else
    Result := cxRectWidth(cxGetTextRect(Font, Text, AHeight div cxTextHeight(Font), True));
end;

function TdxAlertWindowMessageTextViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := Owner.DoCustomDrawMessageText(ACanvas, Self);
end;

function TdxAlertWindowMessageTextViewInfo.GetDrawTextFlags: Cardinal;
begin
  Result := FormatText(GetActualAlignHorz(OptionsMessage.Text.AlignHorz), OptionsMessage.Text.AlignVert);
  Result := Result or CXTO_WORDBREAK or CXTO_EDITCONTROL or CXTO_END_ELLIPSIS;
  if Owner.GetViewInfo.UseRightToLeftReading then
    Result := Result or CXTO_RTLREADING;
end;

function TdxAlertWindowMessageTextViewInfo.GetFont: TFont;
begin
  Result := OptionsMessage.Text.Font;
end;

function TdxAlertWindowMessageTextViewInfo.GetIsTextClipped: Boolean;
begin
  Result := cxRectHeight(Bounds) < CalculateAutoHeight(cxRectWidth(Bounds));
end;

function TdxAlertWindowMessageTextViewInfo.GetText: string;
begin
  if CurrentMessage <> nil then
    Result := CurrentMessage.Text
  else
    Result := '';
end;

{ TdxAlertWindowNavigationPanelTextViewInfo }

function TdxAlertWindowNavigationPanelTextViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  Result := cxTextHeight(Font);
end;

function TdxAlertWindowNavigationPanelTextViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  Result := cxTextExtent(OptionsNavigationPanel.Font, Text).cx;
end;

function TdxAlertWindowNavigationPanelTextViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := Owner.DoCustomDrawNavigationPanelText(ACanvas, Self);
end;

function TdxAlertWindowNavigationPanelTextViewInfo.GetDefaultTextColor: TColor;
begin
  Result := Painter.AlertWindowNavigationPanelTextColor;
end;

function TdxAlertWindowNavigationPanelTextViewInfo.GetDrawTextFlags: Cardinal;
begin
  Result := CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or CXTO_END_ELLIPSIS or CXTO_SINGLELINE;
  if Owner.GetViewInfo.UseRightToLeftReading then
    Result := Result or CXTO_RTLREADING;
end;

function TdxAlertWindowNavigationPanelTextViewInfo.GetFont: TFont;
begin
  Result := OptionsNavigationPanel.Font;
end;

function TdxAlertWindowNavigationPanelTextViewInfo.GetIsTextClipped: Boolean;
begin
  Result := cxRectWidth(Bounds) < CalculateAutoWidth;
end;

function TdxAlertWindowNavigationPanelTextViewInfo.GetText: String;
begin
  Result := OptionsNavigationPanel.DisplayMask;
  Result := StringReplace(Result, sdxAlertWindowMessageIndexMacro, IntToStr(MessageIndex + 1), [rfReplaceAll]);
  Result := StringReplace(Result, sdxAlertWindowMessageCountMacro, IntToStr(MessageCount), [rfReplaceAll]);
end;

{ TdxAlertWindowNavigationPanelViewInfo }

constructor TdxAlertWindowNavigationPanelViewInfo.Create(AOwner: IdxAlertWindow);
begin
  inherited Create(AOwner);
  FNextButton := TdxAlertWindowNextButtonViewInfo.Create(AOwner);
  FPreviousButton := TdxAlertWindowPreviousButtonViewInfo.Create(AOwner);
  FInfoPanelText := TdxAlertWindowNavigationPanelTextViewInfo.Create(AOwner);
  FContentCalculator := TdxAlertWindowNavigationPanelContentCalculator.Create(Self);
end;

destructor TdxAlertWindowNavigationPanelViewInfo.Destroy;
begin
  FreeAndNil(FContentCalculator);
  FreeAndNil(FInfoPanelText);
  FreeAndNil(FPreviousButton);
  FreeAndNil(FNextButton);
  inherited Destroy;
end;

procedure TdxAlertWindowNavigationPanelViewInfo.Calculate(const ABounds: TRect);
begin
  inherited Calculate(ABounds);
  if Visible then
  begin
    ContentCalculator.Bounds := ABounds;
    PreviousButton.Calculate(ContentCalculator.PreviousButtonPlace.Bounds);
    NextButton.Calculate(ContentCalculator.NextButtonPlace.Bounds);
    InfoPanelText.Calculate(ContentCalculator.InfoPanelTextPlace.Bounds);
  end;
end;

function TdxAlertWindowNavigationPanelViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  if Visible then
  begin
    ContentCalculator.Bounds := cxRectSetWidth(cxNullRect, AWidth);
    Result := ContentCalculator.ContentHeight;
  end
  else
    Result := 0;
end;

function TdxAlertWindowNavigationPanelViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  Result := 0;
  if Visible then
  begin
    if OptionsNavigationPanel.Width <= 0 then
    begin
      ContentCalculator.Bounds := cxNullRect;
      Result := ContentCalculator.ContentWidth;
    end
    else
      Result := OptionsNavigationPanel.Width;
  end;
end;

function TdxAlertWindowNavigationPanelViewInfo.GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo;
begin
  Result := nil;
  if cxRectPtIn(Bounds, APoint) then
  begin
    Result := InfoPanelText.GetHitTest(APoint);
    if Result = nil then
      Result := PreviousButton.GetHitTest(APoint);
    if Result = nil then
      Result := NextButton.GetHitTest(APoint);
    if Result = nil then
      Result := inherited GetHitTest(APoint);
  end;
end;

procedure TdxAlertWindowNavigationPanelViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  if Visible then
  begin
    Painter.DrawAlertWindowNavigationPanel(ACanvas, Bounds);
    InfoPanelText.Draw(ACanvas);
    NextButton.Draw(ACanvas);
    PreviousButton.Draw(ACanvas);
  end;
end;

procedure TdxAlertWindowNavigationPanelViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  if not Visible then Exit;
  inherited DoRightToLeftConversion(AClientBounds);
  NextButton.DoRightToLeftConversion(AClientBounds);
  PreviousButton.DoRightToLeftConversion(AClientBounds);
end;

function TdxAlertWindowNavigationPanelViewInfo.GetVisible: Boolean;
begin
  Result := (OptionsNavigationPanel.Visibility = awnpvAlways) or
    ((OptionsNavigationPanel.Visibility = awnpvAuto) and (MessageCount > 1));
end;

{ TdxAlertWindowViewInfo }

constructor TdxAlertWindowViewInfo.Create(AOwner: IdxAlertWindow);
begin
  inherited Create(AOwner);
  FViewInfoList := TcxObjectList.Create;
  FImageViewInfo := TdxAlertWindowImageViewInfo.Create(Owner);
  FViewInfoList.Add(FImageViewInfo);
  FCaptionTextViewInfo := TdxAlertWindowCaptionTextViewInfo.Create(Owner);
  FViewInfoList.Add(FCaptionTextViewInfo);
  FMessageTextViewInfo := TdxAlertWindowMessageTextViewInfo.Create(Owner);
  FViewInfoList.Add(FMessageTextViewInfo);
  FNavigationPanelViewInfo := TdxAlertWindowNavigationPanelViewInfo.Create(Owner);
  FViewInfoList.Add(FNavigationPanelViewInfo);
  FButtonsViewInfo := TdxAlertWindowButtonsViewInfo.Create(Owner);
  FViewInfoList.Add(FButtonsViewInfo);
  FCaptionButtonsViewInfo := TdxAlertWindowCaptionButtonsViewInfo.Create(Owner);
  FViewInfoList.Add(FCaptionButtonsViewInfo);
  FContentCalculator := TdxAlertWindowContentCalculator.Create(Self);
end;

destructor TdxAlertWindowViewInfo.Destroy;
begin
  FreeAndNil(FContentCalculator);
  FViewInfoList.Clear;
  FreeAndNil(FViewInfoList);
  inherited Destroy;
end;

procedure TdxAlertWindowViewInfo.Calculate(const ABounds: TRect);
begin
  RecreateViewInfo;
  FBounds := ABounds;
  ContentCalculator.Bounds := Bounds;
  ImageViewInfo.Calculate(ContentCalculator.ImagePlace.Bounds);
  CaptionButtonsViewInfo.Calculate(ContentCalculator.CaptionButtonsPlace.Bounds);
  CaptionTextViewInfo.Calculate(ContentCalculator.CaptionTextPlace.Bounds);
  ButtonsViewInfo.Calculate(ContentCalculator.ButtonsPlace.Bounds);
  NavigationPanelViewInfo.Calculate(ContentCalculator.NavigationPanelPlace.Bounds);
  MessageTextViewInfo.Calculate(ContentCalculator.MessageTextPlace.Bounds);
  CheckBiDiMode;
end;

function TdxAlertWindowViewInfo.CalculateAutoHeight(AWidth: Integer): Integer;
begin
  ContentCalculator.Bounds := cxRectSetWidth(cxNullRect, AWidth);
  Result := ContentCalculator.ContentHeight;
end;

function TdxAlertWindowViewInfo.CalculateAutoWidth(AHeight: Integer): Integer;
begin
  if AHeight <> -1 then
    ContentCalculator.Bounds := cxRectSetHeight(cxNullRect, AHeight)
  else
    ContentCalculator.Bounds := cxNullRect;

  Result := ContentCalculator.ContentWidth;
end;

procedure TdxAlertWindowViewInfo.CalculateCustomSize(AOptionsSize: TdxAlertWindowOptionsSize; var AWidth, AHeight: Integer);

  procedure MeasureMessageText;
  var
    AWidth, AHeight: Integer;
  begin
    AWidth := Max(0, cxRectWidth(ContentCalculator.MessageTextPlace.Bounds));
    AHeight := Max(0,cxRectHeight(ContentCalculator.MessageTextPlace.Bounds));
    Owner.DoMeasureMessageText(AWidth, AHeight);
    ContentCalculator.MessageTextPlace.CalculatedWidth := AWidth;
    ContentCalculator.MessageTextPlace.CalculatedHeight := AHeight;
  end;

begin
  ContentCalculator.Bounds := cxRectSetSize(cxNullRect, AWidth, AHeight);
  MeasureMessageText;
  ContentCalculator.CustomCalculate := True;
  if AOptionsSize.AutoWidth then
  begin
    if AOptionsSize.AutoHeight then
      AWidth := CalculateAutoWidth
    else
      AWidth := CalculateAutoWidth(AHeight);
  end;
  if AOptionsSize.AutoHeight then
    AHeight := CalculateAutoHeight(AWidth);

  ContentCalculator.CustomCalculate := False;
end;

procedure TdxAlertWindowViewInfo.Draw(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  inherited Draw(ACanvas);
  for I := 0 to FViewInfoList.Count - 1 do
    ViewinfoItem[I].Draw(ACanvas);
end;

function TdxAlertWindowViewInfo.GetHitTest(const APoint: TPoint): TdxAlertWindowCustomViewInfo;
var
  I: Integer;
begin
  Result := nil;
  if cxRectPtIn(Bounds, APoint) then
  begin
    for I := FViewInfoList.Count - 1 downto 0 do
    begin
      Result := ViewInfoItem[I].GetHitTest(APoint);
      if Result <> nil then
        Break;
    end;
    if Result = nil then
      Result := inherited GetHitTest(APoint);
  end;
end;

procedure TdxAlertWindowViewInfo.RecreateViewInfo;
var
  I: Integer;
begin
  inherited RecreateViewInfo;
  FUseRightToLeftAlignment := False;
  FUseRightToLeftReading := False;
  FUseRightToLeftScrollBar := False;
  for I := 0 to FViewInfoList.Count - 1 do
    ViewInfoItem[I].RecreateViewInfo;
end;

procedure TdxAlertWindowViewInfo.CheckBiDiMode;
begin
  if not (Owner is TdxAlertWindow) then Exit;
  FUseRightToLeftAlignment := TdxAlertWindow(Owner).UseRightToLeftAlignment;
  FUseRightToLeftReading := TdxAlertWindow(Owner).UseRightToLeftReading;
  FUseRightToLeftScrollBar := TdxAlertWindow(Owner).UseRightToLeftScrollBar;
  if UseRightToLeftAlignment then
  begin
    ImageViewInfo.DoRightToLeftConversion(ContentCalculator.Bounds);
    CaptionButtonsViewInfo.DoRightToLeftConversion(ContentCalculator.Bounds);
    CaptionTextViewInfo.DoRightToLeftConversion(ContentCalculator.Bounds);
    ButtonsViewInfo.DoRightToLeftConversion(ContentCalculator.Bounds);
    NavigationPanelViewInfo.DoRightToLeftConversion(ContentCalculator.Bounds);
    MessageTextViewInfo.DoRightToLeftConversion(ContentCalculator.Bounds);
  end;
end;

function TdxAlertWindowViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := Owner.DoCustomDrawBackground(ACanvas, Self);
end;

procedure TdxAlertWindowViewInfo.DoDraw(ACanvas: TcxCanvas);
begin
  Painter.DrawAlertWindowBackground(ACanvas, Bounds, ScaleFactor);
end;

function TdxAlertWindowViewInfo.GetViewInfoItem(Index: Integer): TdxAlertWindowCustomViewInfo;
begin
  Result := TdxAlertWindowCustomViewInfo(FViewInfoList[Index]);
end;

function TdxAlertWindowViewInfo.GetItemCount: Integer;
begin
  Result := FViewInfoList.Count;
end;

{ TdxAlertWindowCustomElementPlace }

constructor TdxAlertWindowCustomElementPlace.Create(
  AContentCalculator: TdxAlertWindowCustomContentCalculator; AViewInfo: TdxAlertWindowCustomViewInfo);
begin
  FContentCalculator := AContentCalculator;
  FViewInfo := AViewInfo;
end;

function TdxAlertWindowCustomElementPlace.GetCalculatedHeight: Integer;
begin
  if not CustomCalculate then
    FCalculatedHeight := ViewInfo.CalculateAutoHeight(GetWidthForCalculating);

  Result := FCalculatedHeight;
end;

function TdxAlertWindowCustomElementPlace.GetCalculatedWidth: Integer;
begin
  if not CustomCalculate then
    FCalculatedWidth := ViewInfo.CalculateAutoWidth(GetHeightForCalculating);

  Result := FCalculatedWidth;
end;

function TdxAlertWindowCustomElementPlace.GetCustomCalculate: Boolean;
begin
  Result := ContentCalculator.CustomCalculate;
end;

function TdxAlertWindowCustomElementPlace.GetHeightForCalculating: Integer;
begin
  Result := -1;
end;

function TdxAlertWindowCustomElementPlace.GetScaleFactor: TdxScaleFactor;
begin
  Result := ViewInfo.ScaleFactor;
end;

function TdxAlertWindowCustomElementPlace.GetWidthForCalculating: Integer;
begin
  Result := ViewInfo.CalculateAutoWidth;
end;

{ TdxAlertWindowImagePlace }

function TdxAlertWindowImagePlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result.Right := Result.Left + CalculatedWidth;
  Result.Bottom := Result.Top + CalculatedHeight;
end;

{ TdxAlertWindowButtonsPlace }

function TdxAlertWindowButtonsPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result.Right := Result.Left + CalculatedWidth;
  Result.Top := Result.Bottom - CalculatedHeight;
end;

{ TdxAlertWindowCaptionButtonsPlace }

function TdxAlertWindowCaptionButtonsPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result.Left := Result.Right - CalculatedWidth;
  Result.Bottom := Result.Top + CalculatedHeight;
end;

{ TdxAlertWindowNavigationPanelPlace }

function TdxAlertWindowNavigationPanelPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result := cxRectOffsetVert(Result, -AdjustSizeByIndent(GetContentCalculator.ButtonsPlace.CalculatedHeight, ScaleFactor));
  Result.Top := Result.Bottom - CalculatedHeight;
  Result := cxRectCenterHorizontally(Result, CalculatedWidth);
end;

function TdxAlertWindowNavigationPanelPlace.GetContentCalculator: TdxAlertWindowContentCalculator;
begin
  Result := ContentCalculator as TdxAlertWindowContentCalculator;
end;

{ TdxAlertWindowCaptionTextPlace }

function TdxAlertWindowCaptionTextPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Inc(Result.Left, AdjustSizeByIndent(GetContentCalculator.ImagePlace.CalculatedWidth, ScaleFactor));
  Dec(Result.Right, AdjustSizeByIndent(GetContentCalculator.CaptionButtonsPlace.CalculatedWidth, ScaleFactor));
  Result := cxRectSetHeight(Result, Max(CalculatedHeight, GetContentCalculator.CaptionButtonsPlace.CalculatedHeight));
end;

function TdxAlertWindowCaptionTextPlace.GetContentCalculator: TdxAlertWindowContentCalculator;
begin
  Result := ContentCalculator as TdxAlertWindowContentCalculator;
end;

{ TdxAlertWindowMessageTextPlace }

function TdxAlertWindowMessageTextPlace.GetBounds: TRect;

  function BottomSpace: Integer;
  begin
    Result := GetContentCalculator.ButtonsPlace.CalculatedHeight;
    AddSize(GetContentCalculator.NavigationPanelPlace.CalculatedHeight, ScaleFactor, Result);
  end;

  function CaptionHeight: Integer;
  begin
    Result := Max(GetContentCalculator.CaptionTextPlace.CalculatedHeight,
      GetContentCalculator.CaptionButtonsPlace.CalculatedHeight);
  end;

begin
  Result := ContentCalculator.Bounds;
  AddSize(GetContentCalculator.ImagePlace.CalculatedWidth, ScaleFactor, Result.Left);
  AddSize(CaptionHeight, ScaleFactor, Result.Top);
  Dec(Result.Bottom, AdjustSizeByIndent(BottomSpace, ScaleFactor));
end;

function TdxAlertWindowMessageTextPlace.GetContentCalculator: TdxAlertWindowContentCalculator;
begin
  Result := ContentCalculator as TdxAlertWindowContentCalculator;
end;

function TdxAlertWindowMessageTextPlace.GetHeightForCalculating: Integer;
begin
  Result := cxRectHeight(Bounds);
end;

function TdxAlertWindowMessageTextPlace.GetWidthForCalculating: Integer;
begin
  Result := cxRectWidth(Bounds);
end;

{ TdxAlertWindowCustomContentCalculator }

constructor TdxAlertWindowCustomContentCalculator.Create(AViewInfo: TdxAlertWindowCustomViewInfo);
begin
  FViewInfo := AViewInfo;
end;

function TdxAlertWindowCustomContentCalculator.GetScaleFactor: TdxScaleFactor;
begin
  Result := FViewInfo.Owner.GetScaleFactor;
end;

{ TdxAlertWindowContentCalculator }

constructor TdxAlertWindowContentCalculator.Create(AViewInfo: TdxAlertWindowCustomViewInfo);
begin
  inherited Create(AViewInfo);
  FImagePlace := TdxAlertWindowImagePlace.Create(Self, GetViewInfo.ImageViewInfo);
  FButtonsPlace := TdxAlertWindowButtonsPlace.Create(Self, GetViewInfo.ButtonsViewInfo);
  FCaptionButtonsPlace := TdxAlertWindowCaptionButtonsPlace.Create(Self, GetViewInfo.CaptionButtonsViewInfo);
  FNavigationPanelPlace := TdxAlertWindowNavigationPanelPlace.Create(Self, GetViewInfo.NavigationPanelViewInfo);
  FCaptionTextPlace := TdxAlertWindowCaptionTextPlace.Create(Self, GetViewInfo.CaptionTextViewInfo);
  FMessageTextPlace := TdxAlertWindowMessageTextPlace.Create(Self, GetViewInfo.MessageTextViewInfo);
end;

destructor TdxAlertWindowContentCalculator.Destroy;
begin
  FreeAndNil(FImagePlace);
  FreeAndNil(FButtonsPlace);
  FreeAndNil(FCaptionButtonsPlace);
  FreeAndNil(FNavigationPanelPlace);
  FreeAndNil(FCaptionTextPlace);
  FreeAndNil(FMessageTextPlace);
  inherited Destroy;
end;

function TdxAlertWindowContentCalculator.GetBounds: TRect;
begin
  Result := cxRectContent(FBounds, GetViewInfo.Painter.AlertWindowScaledContentOffsets(ScaleFactor));
end;

function TdxAlertWindowContentCalculator.GetContentHeight: Integer;
begin
  Result := Max(CaptionTextPlace.CalculatedHeight, CaptionButtonsPlace.CalculatedHeight);
  AddSize(MessageTextPlace.CalculatedHeight, ScaleFactor, Result);
  Result := Max(Result, ImagePlace.CalculatedHeight);
  AddSize(NavigationPanelPlace.CalculatedHeight, ScaleFactor, Result);
  AddSize(ButtonsPlace.CalculatedHeight, ScaleFactor, Result);
  Inc(Result, cxMarginsHeight(GetViewInfo.Painter.AlertWindowScaledContentOffsets(ScaleFactor)));
end;

function TdxAlertWindowContentCalculator.GetContentWidth: Integer;

  function CaptionWidth: integer;
  begin
    Result := CaptionTextPlace.CalculatedWidth;
    AddSize(CaptionButtonsPlace.CalculatedWidth, ScaleFactor, Result);
  end;

begin
  Result := MessageTextPlace.CalculatedWidth;
  Result := Max(Result, CaptionWidth);
  AddSize(ImagePlace.CalculatedWidth, ScaleFactor, Result);
  Result := Max(Result, ButtonsPlace.CalculatedWidth);
  Result := Max(Result, NavigationPanelPlace.CalculatedWidth);
  Inc(Result, cxMarginsWidth(GetViewInfo.Painter.AlertWindowScaledContentOffsets(ScaleFactor)));
end;

function TdxAlertWindowContentCalculator.GetViewInfo: TdxAlertWindowViewInfo;
begin
  Result := TdxAlertWindowViewInfo(FViewInfo);
end;

{ TdxAlertWindowPreviousButtonPlace }

function TdxAlertWindowPreviousButtonPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result := cxRectSetWidth(Result, Min(CalculatedWidth, (cxRectWidth(Result) div 2)));
end;

{ TdxAlertWindowNextButtonPlace }

function TdxAlertWindowNextButtonPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result.Left := Result.Right - Min(CalculatedWidth, (cxRectWidth(Result) div 2));
end;

{ TdxAlertWindowInfoPanelTextPlace }

function TdxAlertWindowInfoPanelTextPlace.GetBounds: TRect;
begin
  Result := ContentCalculator.Bounds;
  Result.Left := GetContentCalculator.PreviousButtonPlace.Bounds.Right;
  Result.Right := GetContentCalculator.NextButtonPlace.Bounds.Left;
end;

function TdxAlertWindowInfoPanelTextPlace.GetContentCalculator: TdxAlertWindowNavigationPanelContentCalculator;
begin
  Result := ContentCalculator as TdxAlertWindowNavigationPanelContentCalculator;
end;

function TdxAlertWindowInfoPanelTextPlace.GetWidthForCalculating: Integer;
begin
  Result := cxRectWidth(Bounds);
end;

{ TdxAlertWindowNavigationPanelContentCalculator }

constructor TdxAlertWindowNavigationPanelContentCalculator.Create(AViewInfo: TdxAlertWindowCustomViewInfo);
begin
  inherited Create(AViewInfo);
  FNextButtonPlace := TdxAlertWindowNextButtonPlace.Create(Self, GetViewInfo.NextButton);
  FPreviousButtonPlace := TdxAlertWindowPreviousButtonPlace.Create(Self, GetViewInfo.PreviousButton);
  FInfoPanelTextPlace := TdxAlertWindowInfoPanelTextPlace.Create(Self, GetViewInfo.InfoPanelText);
end;

destructor TdxAlertWindowNavigationPanelContentCalculator.Destroy;
begin
  FreeAndNil(FInfoPanelTextPlace);
  FreeAndNil(FPreviousButtonPlace);
  FreeAndNil(FNextButtonPlace);
  inherited Destroy;
end;

function TdxAlertWindowNavigationPanelContentCalculator.GetBounds: TRect;
begin
  Result := FBounds;
end;

function TdxAlertWindowNavigationPanelContentCalculator.GetContentHeight: Integer;
begin
  Result := InfoPanelTextPlace.CalculatedHeight;
  inc(Result, (cxTextSpace shl 1));
  Result := Max(PreviousButtonPlace.CalculatedHeight, Result);
  Result := Max(NextButtonPlace.CalculatedHeight, Result);
end;

function TdxAlertWindowNavigationPanelContentCalculator.GetContentWidth: Integer;
begin
  Result := InfoPanelTextPlace.CalculatedWidth;
  Inc(Result, (cxTextSpace shl 1));
  AddSize(PreviousButtonPlace.CalculatedWidth, ScaleFactor, Result);
  AddSize(NextButtonPlace.CalculatedWidth, ScaleFactor, Result);
end;

function TdxAlertWindowNavigationPanelContentCalculator.GetViewInfo: TdxAlertWindowNavigationPanelViewInfo;
begin
  Result := FViewInfo as TdxAlertWindowNavigationPanelViewInfo;
end;

{ TdxAlertWindowCustomAnimationController }

destructor TdxAlertWindowCustomAnimationController.Destroy;
begin
  StopAnimationTimer;
  inherited Destroy;
end;

procedure TdxAlertWindowCustomAnimationController.StopAnimation;
begin
  if Active then
    InternalStopAnimation;
end;

procedure TdxAlertWindowCustomAnimationController.DoAnimation;
begin
  if Assigned(FOnAnimation) then
    FOnAnimation(nil);
end;

procedure TdxAlertWindowCustomAnimationController.DoAnimationComplete;
begin
  if Assigned(FOnAnimationComplete) then
    FOnAnimationComplete(nil);
end;

function TdxAlertWindowCustomAnimationController.GetActive: Boolean;
begin
  Result := FTimer <> nil;
end;

function TdxAlertWindowCustomAnimationController.GetAnimationTimerInterval: Cardinal;
begin
  Result := 30;
end;

procedure TdxAlertWindowCustomAnimationController.InternalStopAnimation;
begin
  StopAnimationTimer;
  JumpToFinalState;
  DoAnimation;
  DoAnimationComplete;
end;

procedure TdxAlertWindowCustomAnimationController.StartAnimationTimer;
begin
  if FTimer = nil then
  begin
    FTimer := TcxTimer.Create(nil);
    FTimer.Interval := GetAnimationTimerInterval;
    FTimer.OnTimer := TimerHandler;
    FTimer.Enabled := True;
  end;
end;

procedure TdxAlertWindowCustomAnimationController.StopAnimationTimer;
begin
  FreeAndNil(FTimer);
end;

procedure TdxAlertWindowCustomAnimationController.TimerHandler(Sender: TObject);
begin
  if FFramesCount = 0 then
    InternalStopAnimation
  else
  begin
    ProcessStep;
    Dec(FFramesCount);
  end;
end;

{ TdxAlertWindowMovingAnimationController }

procedure TdxAlertWindowMovingAnimationController.StartAnimation(
  const AStartRect: TRect; const AFinishPoint: TPoint; ATime: Cardinal);
begin
  BaseRect := AStartRect;
  StartAnimation(AStartRect.TopLeft, AFinishPoint, ATime);
end;

function TdxAlertWindowMovingAnimationController.GetCurrentPoint: TPoint;
begin
  Result := Point(Trunc(FWorkingPointX), Trunc(FWorkingPointY));
end;

function TdxAlertWindowMovingAnimationController.GetCurrentWindowRect: TRect;
begin
  Result := cxRectBounds(GetCurrentPoint, cxRectWidth(FBaseRect), cxRectHeight(FBaseRect));
end;

procedure TdxAlertWindowMovingAnimationController.JumpToFinalState;
begin
  //nothing
end;

procedure TdxAlertWindowMovingAnimationController.ProcessStep;
var
  APreviosPoint: TPoint;
begin
  APreviosPoint := GetCurrentPoint;
  if FFramesCount = 1 then
    SetWorkingPoint(FFinishPoint.X, FFinishPoint.Y)
  else
    SetWorkingPoint(FWorkingPointX + FStepMovingX, FWorkingPointY + FStepMovingY);

  if not cxPointIsEqual(APreviosPoint, GetCurrentPoint) then
    DoAnimation;
end;

procedure TdxAlertWindowMovingAnimationController.StartAnimation(const AStartPoint, AFinishPoint: TPoint; ATime: Cardinal);

  function GetTrackLength(X1, Y1, X2, Y2: Single): Single;
  begin
    Result := Sqrt(Sqr(X2 - X1) + Sqr(Y2 - Y1));
  end;

  function GetTracksRatio: Single;
  begin
    if cxPointIsEqual(FStartPoint, FFinishPoint) then
      Result := 1
    else
      Result :=
        GetTrackLength(FWorkingPointX, FWorkingPointY, AFinishPoint.X, AFinishPoint.Y) /
        GetTrackLength(AStartPoint.X, AStartPoint.Y, AFinishPoint.X, AFinishPoint.Y);
  end;

var
  ARealTimeAnimation: Single;
begin
  FStartPoint := AStartPoint;
  FFinishPoint := AFinishPoint;
  if not Active then
    SetWorkingPoint(AStartPoint.X, AStartPoint.Y);

  ARealTimeAnimation := ATime * GetTracksRatio;
  if ARealTimeAnimation >= GetAnimationTimerInterval then
  begin
    FFramesCount := Trunc(ARealTimeAnimation / GetAnimationTimerInterval);
    FStepMovingX := GetAnimationTimerInterval * (AFinishPoint.X - FWorkingPointX) / ARealTimeAnimation;
    FStepMovingY := GetAnimationTimerInterval * (AFinishPoint.Y - FWorkingPointY) / ARealTimeAnimation;
    StartAnimationTimer;
  end
  else
  begin
    SetWorkingPoint(FinishPoint.X, FinishPoint.Y);
    InternalStopAnimation;
  end;
end;

procedure TdxAlertWindowMovingAnimationController.SetBaseRect(const AValue: TRect);
begin
  FBaseRect := AValue;
end;

procedure TdxAlertWindowMovingAnimationController.SetWorkingPoint(X, Y: Single);
begin
  FWorkingPointX := X;
  FWorkingPointY := Y;
end;

{ TdxAlertWindowResizingAnimationController }

procedure TdxAlertWindowResizingAnimationController.StartAnimation(const AStartRect, AFinishRect: TRect; ATime: Cardinal);

  function CalculateShift(AShift1, AShift2, ADirectionIndex1, ADirectionIndex2: Integer): Integer;
  begin
    if Abs(AShift1) > Abs(AShift2) then
    begin
      Result := AShift1;
      FDirection[ADirectionIndex1] := 1;
      if Result = 0 then
        FDirection[ADirectionIndex2] := 0
      else
        FDirection[ADirectionIndex2] := AShift2 / Result;
    end
    else
    begin
      Result := AShift2;
      FDirection[ADirectionIndex2] := 1;
      if Result = 0 then
        FDirection[ADirectionIndex1] := 0
      else
        FDirection[ADirectionIndex1] := AShift1 / Result;
    end;
  end;

  function GetHorizontalShift: Integer;
  begin
    Result := CalculateShift(AFinishRect.Left - AStartRect.Left, AFinishRect.Right - AStartRect.Right, 0, 2);
  end;

  function GetVerticalShift: Integer;
  begin
    Result := CalculateShift(AFinishRect.Top - AStartRect.Top, AFinishRect.Bottom - AStartRect.Bottom, 1, 3);
  end;

  function GetFinishPoint: TPoint;
  begin
    Result := Point(GetHorizontalShift, GetVerticalShift);
  end;

begin
  FBaseRect := AStartRect;
  StartAnimation(cxNullPoint, GetFinishPoint, ATime);
end;

function TdxAlertWindowResizingAnimationController.GetCurrentWindowRect: TRect;
var
  ACurrentPoint: TPoint;
begin
  ACurrentPoint := GetCurrentPoint;
  Result.Left := BaseRect.Left + Trunc(ACurrentPoint.X * FDirection[0]);
  Result.Top := BaseRect.Top + Trunc(ACurrentPoint.Y * FDirection[1]);
  Result.Right := BaseRect.Right + Trunc(ACurrentPoint.X * FDirection[2]);
  Result.Bottom := BaseRect.Bottom + Trunc(ACurrentPoint.Y * FDirection[3]);
end;

procedure TdxAlertWindowResizingAnimationController.JumpToFinalState;
begin
  FWorkingPointX := FFinishPoint.X;
  FWorkingPointY := FFinishPoint.Y;
end;

{ TdxAlertWindowSlidingAnimationController }

procedure TdxAlertWindowSlidingAnimationController.StartAnimation(
  const ARect: TRect; AShowing: Boolean; ADirection: TdxAlertWindowMovingDirection; ATime: Cardinal);
const
  AOffsetHor: array[TdxAlertWindowMovingDirection] of Integer = (0, 1, -1, 0, 0);
  AOffsetVert: array[TdxAlertWindowMovingDirection] of Integer = (0, 0, 0, 1, -1);
var
  AOffset: TPoint;
begin
  FBaseRect := ARect;
  AOffset := Point(AOffsetHor[ADirection] * cxRectWidth(ARect), AOffsetVert[ADirection] * cxRectHeight(ARect));
  if AShowing then
    StartAnimation(cxPointOffset(ARect.TopLeft, AOffset), ARect.TopLeft, ATime)
  else
    StartAnimation(ARect.TopLeft, cxPointOffset(ARect.TopLeft, cxPointInvert(AOffset)), ATime);
end;

function TdxAlertWindowSlidingAnimationController.GetCurrentContentRect: TRect;
var
  P: TPoint;
begin
  Result := cxRectBounds(cxNullPoint, cxRectWidth(BaseRect), cxRectHeight(BaseRect));
  P := Point(Min(0, GetCurrentPoint.X - BaseRect.Left), Min(0, GetCurrentPoint.Y - BaseRect.Top));
  Result := cxRectSetOrigin(Result, P);
end;

function TdxAlertWindowSlidingAnimationController.GetCurrentWindowRect: TRect;
var
  ARect: TRect;
begin
  ARect := inherited GetCurrentWindowRect;
  if not cxRectIntersect(Result, ARect, BaseRect) then
  begin
    Result := ARect;
    if ARect.Right <= BaseRect.Left then
      Result := cxRectSetRight(ARect, BaseRect.Left, 0);

    if ARect.Left >= BaseRect.Right then
      Result := cxRectSetLeft(ARect, BaseRect.Right, 0);

    if ARect.Bottom <= BaseRect.Top then
      Result := cxRectSetBottom(ARect, BaseRect.Top, 0);

    if ARect.Top >= BaseRect.Bottom then
      Result := cxRectSetTop(ARect, BaseRect.Bottom, 0);
  end;
end;

procedure TdxAlertWindowSlidingAnimationController.JumpToFinalState;
begin
  if not cxRectIsEmpty(GetCurrentWindowRect) then
    BaseRect := cxRectSetOrigin(BaseRect, GetCurrentPoint);
end;

{ TdxAlertWindowFadingAnimationController }

procedure TdxAlertWindowFadingAnimationController.StartAnimation(AStartALphaValue, AFinishAlphaValue: Byte; ATime: Cardinal);
var
  ARealTimeAnimation: Single;
  ASourceAlphaInterval: Byte;
  AWorkingAlphaInterval: Single;
begin
  FFinishAlphaValue := AFinishAlphaValue;
  ASourceAlphaInterval := Abs(AFinishAlphaValue - AStartAlphaValue);
  if ASourceAlphaInterval = 0 then
    InternalStopAnimation
  else
  begin
    if not Active then
      FWorkingAlphaValue := AStartAlphaValue;

    AWorkingAlphaInterval := AFinishAlphaValue - FWorkingAlphaValue;
    ARealTimeAnimation := Abs(ATime * AWorkingAlphaInterval / ASourceAlphaInterval);
    if ARealTimeAnimation < GetAnimationTimerInterval then
      InternalStopAnimation
    else
    begin
      FFramesCount := Trunc(ARealTimeAnimation / GetAnimationTimerInterval);
      FAlphaStep := GetAnimationTimerInterval * AWorkingAlphaInterval / ARealTimeAnimation;
      StartAnimationTimer;
    end;
  end;
end;

procedure TdxAlertWindowFadingAnimationController.JumpToFinalState;
begin
  FWorkingAlphaValue := FFinishAlphaValue;
end;

procedure TdxAlertWindowFadingAnimationController.ProcessStep;
var
  APreviosAlphaValue: Byte;
begin
  APreviosAlphaValue := CurrentAlphaValue;
  FWorkingAlphaValue := FWorkingAlphaValue + FAlphaStep;
  if APreviosAlphaValue <> CurrentAlphaValue then
    DoAnimation;
end;

function TdxAlertWindowFadingAnimationController.GetCurrentAlphaValue: Byte;
begin
  Result := Trunc(FWorkingAlphaValue);
end;

{ TdxAlertWindowController }

constructor TdxAlertWindowController.Create(AAlertWindow: IdxAlertWindow);
begin
  inherited Create;
  FAlertWindow := AAlertWindow;
end;

destructor TdxAlertWindowController.Destroy;
begin
  cxClearObjectLinks(Self);
  inherited Destroy;
end;

procedure TdxAlertWindowController.ButtonsClick(AButtonIndex: Integer);
begin
  AlertWindow.ButtonClick(AButtonIndex);
end;

procedure TdxAlertWindowController.Close;
begin
  if not AlertWindow.DoCaptionButtonClick(awcbClose) then
    AlertWindow.Close;
end;

procedure TdxAlertWindowController.MouseDown(const P: TPoint);
begin
  PressedButton := ButtonHitTest(P);
end;

procedure TdxAlertWindowController.MouseLeave;
begin
  HotButton := nil;
end;

procedure TdxAlertWindowController.MouseMove(const P: TPoint);
begin
  HotButton := ButtonHitTest(P);
end;

procedure TdxAlertWindowController.MouseUp(const P: TPoint; Button: TMouseButton);
var
  ALink: TcxObjectLink;
begin
  ALink := cxAddObjectLink(Self);
  try
    if (PressedButton <> nil) and (PressedButton = HotButton) and PressedButton.Enabled then
      PressedButton.Click;
    if ALink.Ref <> nil then
    begin
      PressedButton := nil;
      if (HotButton = nil) and (Button = mbRight) and
        AlertWindow.GetOptionsBehavior.CloseOnRightClick
      then
        AlertWindow.Close;
    end;
  finally
    cxRemoveObjectLink(ALink);
  end;
end;

procedure TdxAlertWindowController.NextMessage;
begin
  if AlertWindow.CurrentMessageIndex < AlertWindow.GetMessages.Count - 1 then
    AlertWindow.CurrentMessageIndex := AlertWindow.CurrentMessageIndex + 1;
end;

procedure TdxAlertWindowController.PreviousMessage;
begin
  if AlertWindow.CurrentMessageIndex > 0 then
    AlertWindow.CurrentMessageIndex := AlertWindow.CurrentMessageIndex - 1;
end;

procedure TdxAlertWindowController.ShowPopupMenu;
var
  APoint: TPoint;
begin
  AlertWindow.IsPopupMenuShown := True;
  try
    if not AlertWindow.DoCaptionButtonClick(awcbDropdown) then
    begin
      APoint := AlertWindow.GetContainer.ClientToScreen(cxRectLeftBottom(HotButton.Bounds));
      cxControls.ShowPopupMenu(AlertWindow.GetContainer, AlertWindow.GetOptionsCaptionButtons.PopupMenu, APoint.X, APoint.Y);
    end;
  finally
    AlertWindow.IsPopupMenuShown := False;
  end;
end;

procedure TdxAlertWindowController.TogglePin;
begin
  if not AlertWindow.DoCaptionButtonClick(awcbPin) then
    AlertWindow.Pinned := not AlertWindow.Pinned;
end;

function TdxAlertWindowController.ButtonHitTest(const APoint: TPoint): TdxAlertWindowCustomButtonViewInfo;
var
  AViewInfo: TdxAlertWindowCustomViewInfo;
begin
  AViewInfo := ViewInfo.GetHitTest(APoint);
  if AViewInfo is TdxAlertWindowCustomButtonViewInfo then
    Result := TdxAlertWindowCustomButtonViewInfo(AViewInfo)
  else
    Result := nil;
end;

function TdxAlertWindowController.GetViewInfo: TdxAlertWindowViewInfo;
begin
  Result := AlertWindow.GetViewInfo;
end;

procedure TdxAlertWindowController.SetHotButton(AValue: TdxAlertWindowCustomButtonViewInfo);
var
  AButton: TdxAlertWindowCustomButtonViewInfo;
begin
  if (AValue <> nil) and not AValue.Enabled then
    AValue := nil;

  if FHotButton <> AValue then
  begin
    AButton := HotButton;
    FHotButton := AValue;
    if AButton <> nil then
      AButton.Invalidate;

    if HotButton <> nil then
      HotButton.Invalidate;
  end;
end;

procedure TdxAlertWindowController.SetPressedButton(AValue: TdxAlertWindowCustomButtonViewInfo);
var
  AButton: TdxAlertWindowCustomButtonViewInfo;
begin
  if (AValue <> nil) and not AValue.Enabled then
    AValue := nil;
  if FPressedButton <> AValue then
  begin
    AButton := PressedButton;
    FPressedButton := AValue;
    if AButton <> nil then
      AButton.Invalidate;

    if PressedButton <> nil then
      PressedButton.Invalidate;
  end;
end;

{ TdxAlertWindowCustomNextAnimation }

procedure TdxAlertWindowCustomNextAnimation.Add(ATime: Integer);
begin
  FTime := Max(FTime, ATime);
  FAssigned := True;
end;

constructor TdxAlertWindowCustomNextAnimation.Create;
begin
  Clear;
end;

procedure TdxAlertWindowCustomNextAnimation.Clear;
begin
  FTime := 0;
  FAssigned := False;
end;

{ TdxAlertWindowNextMovingAnimation }

procedure TdxAlertWindowNextMovingAnimation.Add(const AFinishPoint: TPoint; ATime: Integer; AInternal: Boolean);
begin
  Add(ATime);
  FFinishPoint := AFinishPoint;
  FInternal := AInternal;
end;

{ TdxAlertWindowNextResizingAnimation }

procedure TdxAlertWindowNextResizingAnimation.Add(const AFinishRect: TRect; ATime: Integer);
begin
  Add(ATime);
  FFinishRect := AFinishRect;
end;

{ TdxAlertWindowNextAnimationManager }

constructor TdxAlertWindowNextAnimationManager.Create(AAnimationHelper: TdxAlertWindowAnimationHelper);
begin
  FAnimationHelper := AAnimationHelper;
  FNextAnimation := awaNone;
  FMoving := TdxAlertWindowNextMovingAnimation.Create;
  FResizing := TdxAlertWindowNextResizingAnimation.Create;
end;

destructor TdxAlertWindowNextAnimationManager.Destroy;
begin
  FreeAndNil(FMoving);
  FreeAndNil(FResizing);
  inherited Destroy;
end;

procedure TdxAlertWindowNextAnimationManager.AddMovingAnimation(const AFinishPoint: TPoint; ATime: Integer; AInternal: Boolean);
begin
  FMoving.Add(AFinishPoint, ATime, AInternal);
  if not FResizing.Assigned then
    FNextAnimation := awaMove
  else
    FNextAnimation := awaResize;
end;

procedure TdxAlertWindowNextAnimationManager.AddResizingAnimation(const AFinishRect: TRect; ATime: Integer; AInternal: Boolean);
begin
  FResizing.Add(AFinishRect, ATime);
  if not FMoving.Assigned then
    FNextAnimation := awaResize
  else
    FNextAnimation := awaMove;
end;

procedure TdxAlertWindowNextAnimationManager.Clear;
begin
  FNextAnimation := awaNone;
  FMoving.Clear;
  FResizing.Clear;
end;

procedure TdxAlertWindowNextAnimationManager.LaunchNextAnimation;
begin
  if FNextAnimation = awaResize then
  begin
    FAnimationHelper.ResizeAnimation(FResizing.FinishRect, FResizing.Time);
    FResizing.Clear;
    if FMoving.Assigned then
      FNextAnimation := awaMove
    else
      FNextAnimation := awaNone;
  end
  else
    if FNextAnimation = awaMove then
    begin
      FAnimationHelper.MoveAnimation(FMoving.FinishPoint, FMoving.Time, FMoving.Internal);
      FMoving.Clear;
      if FResizing.Assigned then
        FNextAnimation := awaResize
      else
        FNextAnimation := awaNone;
    end;
end;

{ TdxAlertWindowAnimationHelper }

constructor TdxAlertWindowAnimationHelper.Create(AOwner: TdxAlertWindow);
begin
  inherited Create;
  FAlertWindow := AOwner;
  FOptionsAnimate := TdxAlertWindowManagerOptionsAnimate.Create(AOwner);
  FNextAnimationManager := TdxAlertWindowNextAnimationManager.Create(Self);
  FMovingAnimationController := TdxAlertWindowMovingAnimationController.Create;
  FFadingAnimationController := TdxAlertWindowFadingAnimationController.Create;
  FResizingAnimationController := TdxAlertWindowResizingAnimationController.Create;
  FSlidingAnimationController := TdxAlertWindowSlidingAnimationController.Create;
  OptionsAnimate.OnChange := AnimationOptionsChanged;
  MovingAnimationController.OnAnimation := MovingAnimationHandler;
  MovingAnimationController.OnAnimationComplete := MovingCompleteAnimationHandler;
  ResizingAnimationController.OnAnimation := ResizingAnimationHandler;
  ResizingAnimationController.OnAnimationComplete := ResizingCompleteAnimationHandler;
  FadingAnimationController.OnAnimation := FadingAnimationHandler;
  FadingAnimationController.OnAnimationComplete := FadingCompleteAnimationHandler;
  FSlidingAnimationController.OnAnimation := SlidingAnimationHandler;
  FSlidingAnimationController.OnAnimationComplete := SlidingCompleteAnimationHandler;
end;

destructor TdxAlertWindowAnimationHelper.Destroy;
begin
  FreeAndNil(FSlidingAnimationController);
  FreeAndNil(FResizingAnimationController);
  FreeAndNil(FFadingAnimationController);
  FreeAndNil(FMovingAnimationController);
  FreeAndNil(FNextAnimationManager);
  FreeAndNil(FOptionsAnimate);
  inherited Destroy;
end;

procedure TdxAlertWindowAnimationHelper.HideAnimation;
begin
  FVisibilityAnimation := OptionsAnimate.HidingAnimation;
  case FVisibilityAnimation of
    awaFade:
      FadingAnimationController.StartAnimation(FAlertWindow.AlphaBlendValue, 0, OptionsAnimate.HidingAnimationTime);
    awaMove:
      begin
        FActiveInternalAnimation := True;
        MovingAnimationController.StartAnimation(
          BoundsRect, CalculateFinishRectMoving.TopLeft, OptionsAnimate.HidingAnimationTime);
      end;
    awaSlide:
      begin
        FActiveInternalAnimation := True;
        SlidingAnimationController.StartAnimation(
          BoundsRect, False, GetRealMovingDirection(awvtHiding), OptionsAnimate.HidingAnimationTime);
      end;
    awaNone:
      AlertWindow.VisibilityTransitionComplete;
  end;
end;

procedure TdxAlertWindowAnimationHelper.MoveAnimation(const AFinishPoint: TPoint; ATime: Cardinal; AInternalMove: Boolean);
begin
  if SlidingAnimationController.Active or MovingAnimationController.Active or ResizingAnimationController.Active then
    FNextAnimationManager.AddMovingAnimation(AFinishPoint, ATime, AInternalMove)
  else
  begin
    FActiveInternalAnimation := AInternalMove;
    MovingAnimationController.StartAnimation(BoundsRect, AFinishPoint, ATime);
  end;
end;

procedure TdxAlertWindowAnimationHelper.ResizeAnimation(const AFinishRect: TRect; ATime: Cardinal);
begin
  if SlidingAnimationController.Active or MovingAnimationController.Active or ResizingAnimationController.Active then
    FNextAnimationManager.AddResizingAnimation(AFinishRect, ATime)
  else
  begin
    FActiveInternalAnimation := True;
    ResizingAnimationController.StartAnimation(BoundsRect, AFinishRect, ATime);
  end;
end;

procedure TdxAlertWindowAnimationHelper.ShowAnimation;
begin
  FVisibilityAnimation := OptionsAnimate.ShowingAnimation;
  case FVisibilityAnimation of
    awaFade:
      begin
        AlphaBlendValue := 0;
        Show;
        FadingAnimationController.StartAnimation(0, OptionsAnimate.AlphaBlendValue, OptionsAnimate.ShowingAnimationTime)
      end;
    awaMove:
      begin
        FActiveInternalAnimation := True;
        AlphaBlendValue := OptionsAnimate.AlphaBlendValue;
        MovingAnimationController.StartAnimation(
          CalculateStartRectMoving, BoundsRect.TopLeft, OptionsAnimate.ShowingAnimationTime);
      end;
    awaSlide:
      begin
        FActiveInternalAnimation := True;
        AlphaBlendValue := OptionsAnimate.AlphaBlendValue;
        SlidingAnimationController.StartAnimation(
          BoundsRect, True, GetRealMovingDirection(awvtShowing), OptionsAnimate.ShowingAnimationTime);
      end;
    awaNone:
      begin
        AlphaBlendValue := OptionsAnimate.AlphaBlendValue;
        ShowWindow(AlertWindow.Handle, SW_SHOWNA);
        AlertWindow.VisibilityTransitionComplete;
      end;
  end;
end;

procedure TdxAlertWindowAnimationHelper.StopAnimation;
begin
  FNextAnimationManager.Clear;
  ResizingAnimationController.StopAnimation;
  MovingAnimationController.StopAnimation;
  SlidingAnimationController.StopAnimation;
  FadingAnimationController.StopAnimation;
end;

procedure TdxAlertWindowAnimationHelper.UpdateAlphaBlendValue;

  procedure DoStartFadingAnimation(AFinalValue: Byte; AAnimationTime: Integer);
  begin
    FadingAnimationController.StartAnimation(AlphaBlendValue, AFinalValue, AAnimationTime);
  end;

begin
  if AlertWindow.Visible and OptionsAnimate.HotTrack then
  begin
    if AlertWindow.MouseInControl then
      DoStartFadingAnimation(OptionsAnimate.HotTrackAlphaBlendValue, OptionsAnimate.HotTrackFadeInTime)
    else
      DoStartFadingAnimation(OptionsAnimate.AlphaBlendValue, OptionsAnimate.HotTrackFadeOutTime);
  end
  else
  begin
    FadingAnimationController.StopAnimation;
    AlphaBlendValue := OptionsAnimate.AlphaBlendValue;
  end;
end;

function TdxAlertWindowAnimationHelper.GetRealMovingDirection(
  AVisibilityTransition: TdxAlertWindowVisibilityTransition): TdxAlertWindowMovingDirection;
const
  AutoMovingDirectionMap: array[Boolean] of TdxAlertWindowMovingDirection = (awmdRight, awmdLeft);
begin
  if AVisibilityTransition = awvtShowing then
  begin
    Result := OptionsAnimate.ShowingAnimationDirection;
    if Result = awmdAuto then
      Result := AutoMovingDirectionMap[GetTaskBarPosition <> ABE_LEFT];
  end
  else
  begin
    Result := OptionsAnimate.HidingAnimationDirection;
    if Result = awmdAuto then
      Result := AutoMovingDirectionMap[GetTaskBarPosition = ABE_LEFT];
  end;
  if AlertWindow.UseRightToLeftAlignment then
    case Result of
      awmdLeft:  Result := awmdRight;
      awmdRight: Result := awmdLeft;
    end;
end;

function TdxAlertWindowAnimationHelper.IsActiveAnimation: Boolean;
begin
  Result := SlidingAnimationController.Active or MovingAnimationController.Active or ResizingAnimationController.Active;
end;

procedure TdxAlertWindowAnimationHelper.AnimationComplete(AAnimation: TdxAlertWindowFullAnimation);
begin
  if FVisibilityAnimation = AAnimation then
  begin
    AlertWindow.VisibilityTransitionComplete;
    FVisibilityAnimation := awaNone;
  end;
  if AAnimation in [awaSlide, awaMove, awaResize] then
  begin
    case FNextAnimationManager.FNextAnimation of
      awaNone:
        begin
          FActiveInternalAnimation := False;
          AlertWindow.Changed;
        end;
      else
        FNextAnimationManager.LaunchNextAnimation;
    end;
  end;
end;

procedure TdxAlertWindowAnimationHelper.AnimationOptionsChanged(Sender: TObject);
begin
  UpdateAlphaBlendValue;
end;

function TdxAlertWindowAnimationHelper.CalculateFinishRectMoving: TRect;
begin
  case GetRealMovingDirection(awvtHiding) of
    awmdLeft:
      Result := cxRectSetLeft(BoundsRect, -cxRectWidth(BoundsRect));
    awmdRight:
      Result := cxRectSetLeft(BoundsRect, Screen.Width);
    awmdUp:
      Result := cxRectSetTop(BoundsRect, -cxRectHeight(BoundsRect));
    awmdDown:
      Result := cxRectSetTop(BoundsRect, Screen.Height);
  end;
end;

function TdxAlertWindowAnimationHelper.CalculateStartRectMoving: TRect;
begin
  case GetRealMovingDirection(awvtShowing) of
    awmdLeft:
      Result := cxRectSetLeft(BoundsRect, Screen.Width);
    awmdRight:
      Result := cxRectSetLeft(BoundsRect, -cxRectWidth(BoundsRect));
    awmdUp:
      Result := cxRectSetTop(BoundsRect, Screen.Height);
    awmdDown:
      Result := cxRectSetTop(BoundsRect, -cxRectHeight(BoundsRect));
  end;
end;

procedure TdxAlertWindowAnimationHelper.FadingAnimationHandler(Sender: TObject);
begin
  AlphaBlendValue := FadingAnimationController.CurrentAlphaValue;
  AlertWindow.AlphaBlend := AlphaBlendValue < 255;
end;

procedure TdxAlertWindowAnimationHelper.FadingCompleteAnimationHandler(Sender: TObject);
begin
  FadingAnimationHandler(Sender);
  AnimationComplete(awaFade);
end;

function TdxAlertWindowAnimationHelper.GetAlphaBlendValue: Integer;
begin
  Result := AlertWindow.AlphaBlendValue;
end;

function TdxAlertWindowAnimationHelper.GetBoundsRect: TRect;
begin
  Result := AlertWindow.BoundsRect;
end;

function TdxAlertWindowAnimationHelper.GetContentRect: TRect;
begin
  if FVisibilityAnimation = awaSlide then
    Result := SlidingAnimationController.CurrentContentRect
  else
    Result := AlertWindow.ClientRect;
end;

procedure TdxAlertWindowAnimationHelper.MovingAnimationHandler(Sender: TObject);
begin
  BoundsRect := MovingAnimationController.CurrentWindowRect;
  Show;
end;

procedure TdxAlertWindowAnimationHelper.MovingCompleteAnimationHandler(Sender: TObject);
begin
  AnimationComplete(awaMove);
end;

procedure TdxAlertWindowAnimationHelper.ResizingAnimationHandler(Sender: TObject);
begin
  BoundsRect := ResizingAnimationController.CurrentWindowRect;
  Show;
end;

procedure TdxAlertWindowAnimationHelper.ResizingCompleteAnimationHandler(Sender: TObject);
begin
  AnimationComplete(awaResize);
end;

procedure TdxAlertWindowAnimationHelper.SetAlphaBlendValue(AValue: Integer);
begin
  AlertWindow.AlphaBlendValue := AValue;
end;

procedure TdxAlertWindowAnimationHelper.SetBoundsRect(const AValue: TRect);
begin
  AlertWindow.BoundsRect := AValue;
end;

procedure TdxAlertWindowAnimationHelper.SetOptionsAnimate(AValue: TdxAlertWindowOptionsAnimate);
begin
  FOptionsAnimate.Assign(AValue);
end;

procedure TdxAlertWindowAnimationHelper.Show;
begin
  if (AlertWindow.VisibilityTransition = awvtShowing) and not AlertWindow.Visible then
    ShowWindow(AlertWindow.Handle, SW_SHOWNA);
end;

procedure TdxAlertWindowAnimationHelper.SlidingAnimationHandler(Sender: TObject);
begin
  BoundsRect := SlidingAnimationController.CurrentWindowRect;
  AlertWindow.Invalidate;
  AlertWindow.Update;
  Show;
end;

procedure TdxAlertWindowAnimationHelper.SlidingCompleteAnimationHandler(Sender: TObject);
begin
  AnimationComplete(awaSlide);
end;

{ TdxAlertWindowHitTest }

constructor TdxAlertWindowHitTest.Create(AOwner: TdxAlertWindow);
begin
  FAlertWindow := AOwner;
  FHitPoint := cxInvalidPoint;
end;

procedure TdxAlertWindowHitTest.ReCalculate;
begin
  ReCalculate(FHitPoint);
end;

procedure TdxAlertWindowHitTest.ReCalculate(const APoint: TPoint);
begin
  FHitPoint := APoint;
  FViewInfo := FAlertWindow.ViewInfo.GetHitTest(FHitPoint);
end;

function TdxAlertWindowHitTest.GetHitAtBackground: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtButton: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowButtonViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtCaptionButtonClose: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowCloseButtonViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtCaptionButtonDropdown: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowDropdownButtonViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtCaptionButtonPin: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowPinButtonViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtMessageCaptionText: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowCaptionTextViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtMessageImage: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowImageViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtMessageText: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowMessageTextViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtNavigationPanelNextButton: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowNextButtonViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtNavigationPanelPreviousButton: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowPreviousButtonViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtNavigationPanelText: Boolean;
begin
  Result := ViewInfo is TdxAlertWindowNavigationPanelTextViewInfo;
end;

function TdxAlertWindowHitTest.GetHitAtWindowArea: Boolean;
begin
  Result := not (ViewInfo = nil);
end;

function TdxAlertWindowHitTest.GetHitBackground: TdxAlertWindowViewInfo;
begin
  if HitAtBackground then
    Result := ViewInfo as TdxAlertWindowViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitButton: TdxAlertWindowButtonViewInfo;
begin
  if HitAtButton then
    Result := ViewInfo as TdxAlertWindowButtonViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitCaptionButtonClose: TdxAlertWindowCloseButtonViewInfo;
begin
  if HitAtCaptionButtonClose then
    Result := ViewInfo as TdxAlertWindowCloseButtonViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitCaptionButtonDropdown: TdxAlertWindowDropdownButtonViewInfo;
begin
  if HitAtCaptionButtonDropdown then
    Result := ViewInfo as TdxAlertWindowDropdownButtonViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitCaptionButtonPin: TdxAlertWindowPinButtonViewInfo;
begin
  if HitAtCaptionButtonPin then
    Result := ViewInfo as TdxAlertWindowPinButtonViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitMessageCaptionText: TdxAlertWindowCaptionTextViewInfo;
begin
  if HitAtMessageCaptionText then
    Result := ViewInfo as TdxAlertWindowCaptionTextViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitMessageImage: TdxAlertWindowImageViewInfo;
begin
  if HitAtMessageImage then
    Result := ViewInfo as TdxAlertWindowImageViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitMessageText: TdxAlertWindowMessageTextViewInfo;
begin
  if HitAtMessageText then
    Result := ViewInfo as TdxAlertWindowMessageTextViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitNavigationPanelNextButton: TdxAlertWindowNextButtonViewInfo;
begin
  if HitAtNavigationPanelNextButton then
    Result := ViewInfo as TdxAlertWindowNextButtonViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitNavigationPanelPreviousButton: TdxAlertWindowPreviousButtonViewInfo;
begin
  if HitAtNavigationPanelPreviousButton then
    Result := ViewInfo as TdxAlertWindowPreviousButtonViewInfo
  else
    Result := nil;
end;

function TdxAlertWindowHitTest.GetHitNavigationPanelText: TdxAlertWindowNavigationPanelTextViewInfo;
begin
  if HitAtNavigationPanelText then
    Result := ViewInfo as TdxAlertWindowNavigationPanelTextViewInfo
  else
    Result := nil;
end;

procedure TdxAlertWindowHitTest.SetHitPoint(const APoint: TPoint);
begin
  ReCalculate(APoint);
end;

{ TdxAlertWindow }

constructor TdxAlertWindow.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
{$IFDEF DELPHI16}
  ControlStyle := ControlStyle + [csOverrideStylePaint];
{$ENDIF}
  FViewInfo := CreateViewInfo;
  FHitTest := CreateHitTest;
  FOptionsBehavior := TdxAlertWindowOptionsBehavior.Create(Self);
  FOptionsButtons := TdxAlertWindowOptionsButtons.Create(Self);
  FOptionsCaptionButtons := TdxAlertWindowOptionsCaptionButtons.Create(Self);
  FOptionsMessage := TdxAlertWindowOptionsMessage.Create(Self);
  FOptionsNavigationPanel := TdxAlertWindowOptionsNavigationPanel.Create(Self);
  FOptionsSize := TdxAlertWindowOptionsSize.Create(Self);
  FAnimationHelper := TdxAlertWindowAnimationHelper.Create(Self);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FMessageList := TdxAlertWindowMessageList.Create;
  FController := TdxAlertWindowController.Create(Self);
  OptionsBehavior.OnChange := BehaviorOptionsChanged;
  OptionsButtons.OnChange := ButtonsOptionsChanged;
  OptionsCaptionButtons.OnChange := ButtonsOptionsChanged;
  OptionsSize.OnChange := SizeOptionsChanged;
  OptionsMessage.OnChange := OptionsChanged;
  OptionsNavigationPanel.OnChange := OptionsChanged;
  LookAndFeel.OnChanged := LookAndFeelOptionsChanged;
  MessageList.OnChange := MessagesChanged;
  BorderStyle := bsNone;
  DoubleBuffered := True;
  SnapBuffer := OptionsBehavior.ScreenSnapBuffer;
  SetBounds(0, 0, dxAlertWindowDefaultMinWidth, dxAlertWindowDefaultMinHeight);
  AlphaBlend := True;
  ShowHint := True;
  Pinned := False;
end;

destructor TdxAlertWindow.Destroy;
begin
  StopDisplayTimer;
  EndMouseTracking(Self);
  FreeAndNil(FMessageList);
  FreeAndNil(FController);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FAnimationHelper);
  FreeAndNil(FOptionsSize);
  FreeAndNil(FOptionsNavigationPanel);
  FReeAndNil(FOptionsMessage);
  FreeAndNil(FOptionsCaptionButtons);
  FreeAndNil(FOptionsButtons);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FHitTest);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TdxAlertWindow.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxAlertWindow.DeleteCurrentMessage;
begin
  if MessageList.Count > 1 then
    MessageList.FreeAndDelete(CurrentMessageIndex)
  else
    Hide;
end;

procedure TdxAlertWindow.EndUpdate;
begin
  if LockCount > 0 then
  begin
    Dec(FLockCount);
    Changed;
  end;
end;

procedure TdxAlertWindow.Hide;
begin
  if Visible and (FVisibilityTransition <> awvtHiding) then
  begin
    StopVisibilityTransition;
    FVisibilityTransition := awvtHiding;
    AnimationHelper.HideAnimation;
  end;
end;

procedure TdxAlertWindow.RestartDisplayTimer;
begin
  if FDisplayTimer <> nil then
  begin
    StopDisplayTimer;
    StartDisplayTimer;
  end;
end;

procedure TdxAlertWindow.Show;
begin
  if (FVisibilityTransition = awvtNone) and not Visible then
  begin
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
    StopVisibilityTransition;
    FVisibilityTransition := awvtShowing;
    AnimationHelper.ShowAnimation;
  end;
end;

procedure TdxAlertWindow.Close;
begin
  Hide;
end;

procedure TdxAlertWindow.BehaviorOptionsChanged(Sender: TObject);
begin
  SnapBuffer := OptionsBehavior.ScreenSnapBuffer;
  if (FDisplayTimer <> nil) and (OptionsBehavior.DisplayTime <> FDisplayTimer.Interval) then
    FDisplayTimer.Interval := OptionsBehavior.DisplayTime;
  Changed;
end;

procedure TdxAlertWindow.ButtonsOptionsChanged(Sender: TObject);
begin
  ViewInfo.RecreateViewInfo;
  Changed;
end;

procedure TdxAlertWindow.CalculateAutoSize(var AWidth, AHeight: Integer);

  function CalculateNewWidth(AWidth: Integer): Integer;
  begin
    Result := Max(AWidth, OptionsSize.MinWidth);
    if OptionsSize.MaxWidth > 0 then
      Result := Min(Result, OptionsSize.MaxWidth);
  end;

  function CalculateNewHeight(AHeight: Integer): Integer;
  begin
    Result := Max(AHeight, OptionsSize.MinHeight);
    if OptionsSize.MaxHeight > 0 then
      Result := Min(AHeight, OptionsSize.MaxHeight);
  end;

  function CalculateAutoWidth(AHeight: Integer): Integer;
  begin
    if OptionsSize.AutoHeight then
      Result := ViewInfo.CalculateAutoWidth
    else
      Result := ViewInfo.CalculateAutoWidth(AHeight);

    Result := CalculateNewWidth(Result);
  end;

  function CalculateAutoHeight(AWidth: Integer): Integer;
  begin
    Result := CalculateNewHeight(ViewInfo.CalculateAutoHeight(AWidth));
  end;

begin
  if OptionsSize.AutoWidth then
    AWidth := CalculateAutoWidth(AHeight);
  if OptionsSize.AutoHeight then
    AHeight := CalculateAutoHeight(AWidth);
  ViewInfo.CalculateCustomSize(OptionsSize, AWidth, AHeight);
  AWidth := CalculateNewWidth(AWidth);
  AHeight := CalculateNewHeight(AHeight);
end;

procedure TdxAlertWindow.CalculateViewInfo;
begin
  if Assigned(ViewInfo) then
    ViewInfo.Calculate(AnimationHelper.ContentRect);
end;

procedure TdxAlertWindow.Click;
begin
  if (Controller.HotButton = nil) and (Controller.PressedButton = nil) and not FDragging then
    DoClick;
end;

procedure TdxAlertWindow.CollapseEmptySlotsMove(const ANewPosition: TPoint; ATime: Cardinal);
begin
  AnimationHelper.MoveAnimation(ANewPosition, ATime, True);
end;

function TdxAlertWindow.CreateHitTest: TdxAlertWindowHitTest;
begin
  Result := TdxAlertWindowHitTest.Create(Self);
end;

procedure TdxAlertWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE;
  Params.WndParent := 0;
end;

function TdxAlertWindow.CreateViewInfo: TdxAlertWindowViewInfo;
begin
  Result := TdxAlertWindowViewInfo.Create(Self);
end;

procedure TdxAlertWindow.Paint;
begin
  inherited Paint;
  cxPaintCanvas.BeginPaint(Canvas);
  try
    ViewInfo.Draw(cxPaintCanvas);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxAlertWindow.Resize;

  procedure AdjustSize(AWidth, AHeight: Integer);
  begin
    if not AnimationHelper.ActiveInternalAnimation then
    begin
      if not Visible or OptionsSize.AutoSizeAdjustment then
        CalculateAutoSize(AWidth, AHeight);

      BeginUpdate;
      try
        SetNewSize(AWidth, AHeight);
        Width := OptionsSize.Width;
        Height := OptionsSize.Height;
      finally
        EndUpdate;
      end;
    end;
  end;

begin
  inherited Resize;
  AdjustSize(Width, Height);
  Changed;
end;

procedure TdxAlertWindow.ScaleFactorChanged(M, D: Integer);
begin
  inherited ScaleFactorChanged(M, D);
  if M <> D then
  begin
    OptionsBehavior.ChangeScale(M, D);
    OptionsButtons.ChangeScale(M, D);
    OptionsMessage.ChangeScale(M, D);
    OptionsNavigationPanel.ChangeScale(M, D);
    OptionsSize.ChangeScale(M, D);
  end;
end;

procedure TdxAlertWindow.SetWindowRegion(ACornerRadius: Integer);
var
  ARegion: HRGN;
begin
  if ACornerRadius > 0 then
  begin
    ARegion := CreateRoundRectRgn(
      0, 0, cxRectWidth(BoundsRect) + 1, cxRectHeight(BoundsRect) + 1, ACornerRadius, ACornerRadius);
    SetWindowRgn(Handle, ARegion, True);
  end
  else
    SetWindowRgn(Handle, 0, True);
end;

procedure TdxAlertWindow.VisibilityTransitionComplete;
begin
  case VisibilityTransition of
    awvtHiding:
      begin
        StopVisibilityTransition;
        ShowWindow(Handle, SW_HIDE);
        FVisibilityTransition := awvtNone;
        DoHide;
      end;
    awvtShowing:
      begin
        FVisibilityTransition := awvtNone;
        DoShow;
        if not Pinned then
          StartDisplayTimer;
      end;
    awvtNone:
      FVisibilityTransition := awvtNone;
  end;
end;

procedure TdxAlertWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  MouseCapture := True;
  if (ssLeft in Shift) then
  begin
    Controller.MouseDown(Point(X, Y));
    if Controller.PressedButton = nil then
    begin
      FDraggingPoint := Point(X, Y);
      FDraggingScreenPoint := ClientToScreen(FDraggingPoint);
      AnimationHelper.StopAnimation;
      FMouseHoldForDragging := True;
      ScreenSnap := OptionsBehavior.ScreenSnap;
      DoMouseDown(Button, Shift, X, Y);
    end;
  end
  else
    DoMouseDown(Button, Shift, X, Y);
end;

procedure TdxAlertWindow.MouseMove(Shift: TShiftState; X, Y: Integer);

  function IsBeginDragging: Boolean;
  var
    ANewPoint: TPoint;
  begin
    ANewPoint := ClientToScreen(Point(X, Y));
    Result := (Abs(FDraggingScreenPoint.X - ANewPoint.X) > Mouse.DragThreshold) or
      (Abs(FDraggingScreenPoint.Y - ANewPoint.Y) > Mouse.DragThreshold);
    if Result and not DoDragBegin then
    begin
      FMouseHoldForDragging := False;
      Result := False;
    end;
  end;

begin
  inherited MouseMove(Shift, X, Y);
  if FMouseHoldForDragging then
  begin
    if not FDragging and IsBeginDragging then
      FDragging := True;
    if FDragging then
    begin
      SetBounds(Left + X - FDraggingPoint.X, Top + Y - FDraggingPoint.Y, Width, Height);
      DoDragMove;
    end;
  end
  else
  begin
    if not FMouseInControl then
    begin
      MouseInControl := True;
      DoMouseEnter;
      BeginMouseTracking(Self, ClientRect, Self);
    end;
    DoMouseMove(Shift, X, Y);
  end;
  Controller.MouseMove(Point(X, Y));
end;

procedure TdxAlertWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
    DoDragEnd;
  FDragging := False;
  FMouseHoldForDragging := False;
  MouseCapture := False;
  ScreenSnap := False;
  if Controller.PressedButton = nil then
    DoMouseUp(Button, Shift, X, Y);
  Controller.MouseUp(Point(X, Y), Button);
end;

procedure TdxAlertWindow.Changed;

  procedure AdjustSize(AWidth, AHeight: Integer);
  var
    ANewBounds: TRect;
  begin
    if VisibilityTransition = awvtNone then
    begin
      AWidth := OptionsSize.Width;
      AHeight := OptionsSize.Height;
      if not Visible or OptionsSize.AutoSizeAdjustment then
        CalculateAutoSize(AWidth, AHeight);

      if not Visible then
        SetBounds(Left, Top, AWidth, AHeight)
      else
        if (AWidth <> FLastSize.cx) or (AHeight <> FLastSize.cy) then
        begin
          ANewBounds := cxRectSetSize(BoundsRect, AWidth, AHeight);
          DoResize(ANewBounds);
          AnimationHelper.ResizeAnimation(ANewBounds, OptionsAnimate.SizeAdjustmentAnimationTime);
        end;
      SetNewSize(AWidth, AHeight);
    end;
  end;

begin
  if LockCount = 0 then
  begin
    AdjustSize(OptionsSize.Width, OptionsSize.Height);
    CalculateViewInfo;
    SetWindowRegion(LookAndFeel.Painter.AlertWindowCornerRadius);
    Invalidate;
  end;
end;

procedure TdxAlertWindow.LookAndFeelOptionsChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  Changed;
end;

procedure TdxAlertWindow.MessagesChanged(Sender: Tobject);
begin
  if (MessageList <> nil) then
  begin
    CurrentMessageIndex := Min(CurrentMessageIndex, MessageList.Count - 1);
    Changed;
  end;
end;

procedure TdxAlertWindow.OptionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxAlertWindow.SizeOptionsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TdxAlertWindow.DoButtonClick(AButtonIndex: Integer);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, AButtonIndex);
end;

procedure TdxAlertWindow.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TdxAlertWindow.DoDragBegin: Boolean;
begin
  Result := True;
  if Assigned(FOnDragBegin) then
    FOnDragBegin(Self, Result);
end;

procedure TdxAlertWindow.DoDragEnd;
begin
  if Assigned(FOnDragEnd) then
    FOnDragEnd(Self);
end;

procedure TdxAlertWindow.DoDragMove;
begin
  if Assigned(FOnDragMove) then
    FOnDragMove(Self);
end;

procedure TdxAlertWindow.DoHide;
begin
  if Assigned(FOnHide) then
    FOnHide(Self);
end;

procedure TdxAlertWindow.DoMouseDown(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, AButton, AShift, X, Y);
end;

procedure TdxAlertWindow.DoMouseEnter;
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TdxAlertWindow.DoMouseLeave;
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TdxAlertWindow.DoMouseMove(AShift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, AShift, X, Y);
end;

procedure TdxAlertWindow.DoMouseUp(AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, AButton, AShift, X, Y);
end;

procedure TdxAlertWindow.DoMove;
begin
  if Assigned(FOnMove) then
    FOnMove(Self);
end;

procedure TdxAlertWindow.DoResize(var ANewBounds: TRect);
begin
  if Assigned(FOnResize) then
    FOnResize(Self, ANewBounds);
end;

procedure TdxAlertWindow.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

procedure TdxAlertWindow.CMHintShow(var Message: TCMHintShow);
var
  AViewInfo: TdxAlertWindowCustomViewInfo;
begin
  AViewInfo := ViewInfo.GetHitTest(Message.HintInfo^.CursorPos);
  if AViewInfo.Hint <> '' then
  begin
    Message.HintInfo^.HintMaxWidth := Width;
    Message.HintInfo^.HintStr := GetShortHint(AViewInfo.Hint);
    Message.HintInfo^.CursorRect := AViewInfo.Bounds;
  end
  else
    Message.Result := -1;
end;

procedure TdxAlertWindow.WMMove(var Msg: TWMMove);
begin
  inherited;
  if Visible and not AnimationHelper.ActiveInternalAnimation then
    DoMove;
end;

procedure TdxAlertWindow.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if VisibilityTransition = awvtHiding then
    Message.Result := HTTRANSPARENT
  else
    inherited;
end;

procedure TdxAlertWindow.MouseLeave;
begin
  MouseInControl := False;
  Controller.MouseLeave;
  DoMouseLeave;
end;

function TdxAlertWindow.PtInCaller(const P: TPoint): Boolean;
begin
  Result := FMouseHoldForDragging or (WindowFromPoint(ClientToScreen(P)) = Handle);
end;

procedure TdxAlertWindow.ButtonClick(AButtonIndex: Integer);
begin
  DoButtonClick(AButtonIndex);
end;

function TdxAlertWindow.DoCaptionButtonClick(AButton: TdxAlertWindowCaptionButton): Boolean;
begin
  Result := False;
  if Assigned(FOnCaptionButtonClick) then
    FOnCaptionButtonClick(Self, AButton, Result);
end;

function TdxAlertWindow.DoCustomDrawBackground(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawBackground) then
    FOnCustomDrawBackground(Self, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindow.DoCustomDrawButton(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawButton) then
    FOnCustomDrawButton(Self, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindow.DoCustomDrawMessageCaptionText(
  ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawMessageCaptionText) then
    FOnCustomDrawMessageCaptionText(Self, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindow.DoCustomDrawMessageImage(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawMessageImage) then
    FOnCustomDrawMessageImage(Self, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindow.DoCustomDrawMessageText(ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawMessageText) then
    FOnCustomDrawMessageText(Self, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindow.DoCustomDrawNavigationPanelText(
  ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawNavigationPanelText) then
    FOnCustomDrawNavigationPanelText(Self, ACanvas, AViewInfo, Result);
end;

procedure TdxAlertWindow.DoMeasureMessageText(var AWidth, AHeight: Integer);
begin
  if Assigned(FOnMeasureMessageText) then
    FOnMeasureMessageText(Self, AWidth, AHeight);
end;

function TdxAlertWindow.GetContainer: TWinControl;
begin
  Result := Self;
end;

function TdxAlertWindow.GetController: TdxAlertWindowController;
begin
  Result := FController;
end;

function TdxAlertWindow.GetCurrentMessageIndex: Integer;
begin
  Result := FCurrentMessageIndex;
end;

function TdxAlertWindow.GetIsPopupMenuShown: Boolean;
begin
  Result := FIsPopupMenuShown;
end;

function TdxAlertWindow.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FLookAndFeel;
end;

function TdxAlertWindow.GetMessages: TdxAlertWindowMessageList;
begin
  Result := FMessageList;
end;

function TdxAlertWindow.GetOptionsBehavior: TdxAlertWindowOptionsBehavior;
begin
  Result := FOptionsBehavior;
end;

function TdxAlertWindow.GetOptionsButtons: TdxAlertWindowOptionsButtons;
begin
  Result := FOptionsButtons;
end;

function TdxAlertWindow.GetOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
begin
  Result := FOptionsCaptionButtons;
end;

function TdxAlertWindow.GetOptionsMessage: TdxAlertWindowOptionsMessage;
begin
  Result := FOptionsMessage;
end;

function TdxAlertWindow.GetOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
begin
  Result := FOptionsNavigationPanel;
end;

function TdxAlertWindow.GetOptionsSize: TdxAlertWindowOptionsSize;
begin
  Result := FOptionsSize;
end;

function TdxAlertWindow.GetPinned: Boolean;
begin
  Result := FPinned;
end;

function TdxAlertWindow.GetScaleFactor: TdxScaleFactor;
begin
  Result := ScaleFactor;
end;

function TdxAlertWindow.GetViewInfo: TdxAlertWindowViewInfo;
begin
  Result := FViewInfo;
end;

procedure TdxAlertWindow.InvalidateRect(const ARect: TRect);
begin
  if Visible then
    cxInvalidateRect(Handle, ARect);
end;

procedure TdxAlertWindow.SetCurrentMessageIndex(AValue: Integer);
begin
  if (MessageList <> nil) and (FCurrentMessageIndex <> AValue) then
  begin
    FCurrentMessageIndex := Min(AValue, MessageList.Count - 1);
    Changed;
  end;
end;

procedure TdxAlertWindow.SetIsPopupMenuShown(AValue: Boolean);
begin
  if AValue <> FIsPopupMenuShown then
  begin
    FIsPopupMenuShown := AValue;
    UpdateDisplayTimerState;
  end;
end;

procedure TdxAlertWindow.SetPinned(AValue: Boolean);
begin
  if FPinned <> AValue then
  begin
    FPinned := AValue;
    UpdateDisplayTimerState;
    Changed;
  end;
end;

function TdxAlertWindow.IsSkinnable: Boolean;
begin
  Result := False;
end;

procedure TdxAlertWindow.DisplayTimerHandler(Sender: TObject);
begin
  Close;
end;

function TdxAlertWindow.GetOptionsAnimate: TdxAlertWindowOptionsAnimate;
begin
  Result := AnimationHelper.OptionsAnimate;
end;

function TdxAlertWindow.GetVisible: Boolean;
begin
  Result := HandleAllocated and IsWindowVisible(Handle);
end;

procedure TdxAlertWindow.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(AValue);
end;

procedure TdxAlertWindow.SetMessageList(AValue: TdxAlertWindowMessageList);
begin
  FMessageList.Assign(AValue);
end;

procedure TdxAlertWindow.SetMouseInControl(AValue: Boolean);
begin
  if AValue <> FMouseInControl then
  begin
    FMouseInControl := AValue;
    UpdateDisplayTimerState;
  end;
end;

procedure TdxAlertWindow.SetNewSize(AWidth, AHeight: Integer);
begin
  FLastSize := cxSize(AWidth, AHeight);
  OptionsSize.Width := AWidth;
  OptionsSize.Height := AHeight;
end;

procedure TdxAlertWindow.SetOptionsAnimate(AValue: TdxAlertWindowOptionsAnimate);
begin
  AnimationHelper.OptionsAnimate.Assign(AValue);
end;

procedure TdxAlertWindow.SetOptionsBehavior(AValue: TdxAlertWindowOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxAlertWindow.SetOptionsButtons(AValue: TdxAlertWindowOptionsButtons);
begin
  FOptionsButtons.Assign(AValue);
end;

procedure TdxAlertWindow.SetOptionsCaptionButtons(AValue: TdxAlertWindowOptionsCaptionButtons);
begin
  FOptionsCaptionButtons.Assign(AValue);
end;

procedure TdxAlertWindow.SetOptionsMessage(AValue: TdxAlertWindowOptionsMessage);
begin
  FOptionsMessage.Assign(AValue);
end;

procedure TdxAlertWindow.SetOptionsNavigationPanel(AValue: TdxAlertWindowOptionsNavigationPanel);
begin
  FOptionsNavigationPanel.Assign(AValue);
end;

procedure TdxAlertWindow.SetOptionsSize(AValue: TdxAlertWindowOptionsSize);
begin
  FOptionsSize.Assign(AValue);
end;

procedure TdxAlertWindow.StartDisplayTimer;
begin
  FDisplayTimer := TcxTimer.Create(Self);
  FDisplayTimer.Interval := OptionsBehavior.DisplayTime;
  FDisplayTimer.OnTimer := DisplayTimerHandler;
end;

procedure TdxAlertWindow.StopDisplayTimer;
begin
  FreeAndNil(FDisplayTimer);
end;

procedure TdxAlertWindow.StopVisibilityTransition;
begin
  StopDisplayTimer;
  AnimationHelper.StopAnimation;
end;

procedure TdxAlertWindow.UpdateDisplayTimerState;
begin
  if VisibilityTransition <> awvtHiding then
  begin
    if MouseInControl or Pinned or IsPopupMenuShown then
      StopDisplayTimer
    else
      StartDisplayTimer;

    AnimationHelper.UpdateAlphaBlendValue;
  end;
end;

{ TdxAlertWindowPositionInfo }

constructor TdxAlertWindowPositionInfo.Create(AAlertWindow: TdxAlertWindow; APosition: TdxAlertWindowPosition; AMonitorNum: Integer);
begin
  FAlertWindow := AAlertWindow;
  FInitialBounds := AAlertWindow.BoundsRect;
  FPosition := APosition;
  FMonitorNum := AMonitorNum;
end;

function TdxAlertWindowPositionInfo.CompareAlertWindow(AAlertWindow: TdxAlertWindow): Boolean;
begin
  Result := (AAlertWindow = FAlertWindow);
end;

function TdxAlertWindowPositionInfo.IsPositionChanged: Boolean;
begin
  Result := not cxRectIsEqual(FAlertWindow.BoundsRect, InitialBounds);
end;

{ TdxAlertWindowPositionInfoList }

function TdxAlertWindowPositionInfoList.Add(
  AAlertWindow: TdxAlertWindow; APosition: TdxAlertWindowPosition; AMonitorNum: Integer): TdxAlertWindowPositionInfo;
begin
  Result := TdxAlertWindowPositionInfo.Create(AAlertWindow, APosition, AMonitorNum);
  inherited Add(Result);
end;

function TdxAlertWindowPositionInfoList.GetItem(AIndex: Integer): TdxAlertWindowPositionInfo;
begin
  Result := TdxAlertWindowPositionInfo(inherited Items[AIndex]);
end;

{ TdxAlertWindowInitialLayout }

constructor TdxAlertWindowInitialLayout.Create(AManager: TdxAlertWindowManager);
begin
  inherited Create;
  FManager := AManager;
  FPositionInfoList := TdxAlertWindowPositionInfoList.Create;
end;

destructor TdxAlertWindowInitialLayout.Destroy;
begin
  FreeAndNil(FPositionInfoList);
  inherited Destroy;
end;

procedure TdxAlertWindowInitialLayout.AddItem;
var
  AAlertWindow: TdxAlertWindow;
begin
  AAlertWindow := GetNextWindowForShowing;
  if (AAlertWindow <> nil) and ((Manager.WindowMaxCount = 0) or (Manager.GetDisplayedCount < Manager.WindowMaxCount)) then
  begin
    CalculateNextPosition(AAlertWindow);
    Manager.DoBeforeShow(AAlertWindow);
    AAlertWindow.Show;
    if PositionInfoList[HasWindow(AAlertWindow)].IsPositionChanged then
      RemoveItemWithWindow(AAlertWindow);

    AddItem;
  end;
end;

procedure TdxAlertWindowInitialLayout.CalculateNextPosition(AAlertWindow: TdxAlertWindow);
var
  AColumnIndex: Integer;
  AMonitor: TMonitor;
  AActualPosition: TdxAlertWindowPosition;
begin
  if (Application.MainForm <> nil) and Application.MainForm.HandleAllocated then
    AMonitor := Screen.MonitorFromWindow(Application.MainForm.Handle)
  else
    AMonitor := Screen.Monitors[0];

  AColumnIndex := 0;
  AActualPosition := GetActualPosition(AAlertWindow);
  AAlertWindow.BoundsRect := GetNewPosition(AAlertWindow.BoundsRect,
    GetLastInitialGrid(AActualPosition, AMonitor.MonitorNum), AActualPosition,
    GetMonitorWorkArea(AMonitor.Handle), AColumnIndex);
  PositionInfoList.Add(AAlertWindow, AActualPosition, AMonitor.MonitorNum).ColumnIndex := AColumnIndex;
end;

procedure TdxAlertWindowInitialLayout.DeleteItem(AAlertWindow: TdxAlertWindow);
begin
  RemoveItemWithWindow(AAlertWindow);
  AddItem;
end;

function TdxAlertWindowInitialLayout.HasWindow(AAlertWindow: TdxAlertWindow): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to PositionInfoList.Count - 1 do
  begin
    if PositionInfoList[I].CompareAlertWindow(AAlertWindow) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TdxAlertWindowInitialLayout.ResizeWindow(AAlertWindow: TdxAlertWindow; var ANewBounds: TRect);
const
  PositionToCoefficientResize: array[TdxAlertWindowPosition, 0..3] of Integer = (
  (0, 0, 0, 0), (0, 0, 1, 1), (-1, 0, 0, 1), (0, -1, 1, 0), (-1, -1, 0, 0));
var
  AIndex: Integer;
  ADifference: TSize;
begin
  AIndex := HasWindow(AAlertWIndow);
  if AIndex > -1 then
  begin
    ADifference.cx := cxRectWidth(ANewBounds) - cxRectWidth(PositionInfoList[AIndex].FInitialBounds);
    ADifference.cy := cxRectHeight(ANewBounds) - cxRectHeight(PositionInfoList[AIndex].FInitialBounds);
    Inc(PositionInfoList[AIndex].FInitialBounds.Left,
      PositionToCoefficientResize[PositionInfoList[AIndex].Position, 0] * ADifference.cx);
    Inc(PositionInfoList[AIndex].FInitialBounds.Top,
      PositionToCoefficientResize[PositionInfoList[AIndex].Position, 1] * ADifference.cy);
    Inc(PositionInfoList[AIndex].FInitialBounds.Right,
      PositionToCoefficientResize[PositionInfoList[AIndex].Position, 2] * ADifference.cx);
    Inc(PositionInfoList[AIndex].FInitialBounds.Bottom,
      PositionToCoefficientResize[PositionInfoList[AIndex].Position, 3] * ADifference.cy);
    ANewBounds := PositionInfoList[AIndex].FInitialBounds;
    if Manager.OptionsAnimate.CollapseEmptySlots then
      Recalculate(Position, PositionInfoList[AIndex].MonitorNum, AIndex);
  end;
end;

function TdxAlertWindowInitialLayout.GetActualPosition(AAlertWindow: TdxAlertWindow): TdxAlertWindowPosition;
begin
  Result := Position;
  if AAlertWindow.UseRightToLeftAlignment then
  case Result of
    awpTopLeft:     Result := awpTopRight;
    awpTopRight:    Result := awpTopLeft;
    awpBottomLeft:  Result := awpBottomRight;
    awpBottomRight: Result := awpBottomLeft;
  end;
end;

function TdxAlertWindowInitialLayout.GetNewPosition(const AAlertWindowRect: TRect;
  APreviousPositionInfo: TdxAlertWindowPositionInfo; APosition: TdxAlertWindowPosition;
  const AWorkArea: TRect; var AColumnIndex: Integer): TRect;

  procedure CalculatePlaceRect(var AInitialPlaceRect: TRect; const ALastWindowInGridRect: TRect);
  begin
    if ((APosition = awpBottomLeft) or (APosition = awpBottomRight)) and (AColumnIndex <= APreviousPositionInfo.ColumnIndex) and
      (ALastWindowInGridRect.Top - cxRectHeight(Result) >= AWorkArea.Top) then
    begin
      AInitialPlaceRect := cxRectSetTop(AInitialPlaceRect, ALastWindowinGridRect.Top - cxRectHeight(AInitialPlaceRect));
      AColumnIndex := APreviousPositionInfo.ColumnIndex;
    end
    else
      if ((APosition = awpTopLeft) or (APosition = awpTopRight)) and (AColumnIndex <= APreviousPositionInfo.ColumnIndex) and
        (ALastWindowInGridRect.Bottom + cxRectHeight(Result) <= AWorkArea.Bottom) then
      begin
          AInitialPlaceRect := cxRectSetTop(AInitialPlaceRect, ALastWindowInGridRect.Bottom);
          AColumnIndex := APreviousPositionInfo.ColumnIndex;
      end
      else
          AColumnIndex := APreviousPositionInfo.ColumnIndex + 1;
  end;

begin
  case APosition of
    awpTopLeft:
      Result := cxRectSetOrigin(AAlertWindowRect, AWorkArea.TopLeft);
    awpTopRight:
      Result := cxRectSetOrigin(AAlertWindowRect, cxPointOffset(cxRectRightTop(AWorkArea),
        -cxRectWidth(AAlertWindowRect), 0));
    awpBottomLeft:
      Result := cxRectSetOrigin(AAlertWindowRect, cxPointOffset(cxRectLeftBottom(AWorkArea), 0,
        -cxRectHeight(AAlertWindowRect)));
    awpBottomRight:
      Result := cxRectSetOrigin(AAlertWindowRect, cxPointOffset(AWorkArea.BottomRight,
        -cxRectWidth(AAlertWindowRect), -cxRectHeight(AAlertWindowRect)));
  end;
  if APreviousPositionInfo <> nil then
    CalculatePlaceRect(Result, APreviousPositionInfo.InitialBounds);
end;

function TdxAlertWindowInitialLayout.GetLastInitialGrid(APosition: TdxAlertWindowPosition;
  AMonitorNum: Integer): TdxAlertWindowPositionInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := PositionInfoList.Count - 1 downto 0 do
    if (PositionInfoList[I].Position = APosition) and (PositionInfoList[I].MonitorNum = AMonitorNum) then
    begin
      Result := PositionInfoList[I];
      Break;
    end;
end;

function TdxAlertWindowInitialLayout.GetNextWindowForShowing: TdxAlertWindow;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Manager.Count - 1 do
    if not Manager[I].Visible and (Manager[I].VisibilityTransition <> awvtShowing) and (HasWindow(Manager[I]) = -1) then
    begin
      Result := Manager[I];
      Break;
    end;
end;

function TdxAlertWindowInitialLayout.GetPosition: TdxAlertWindowPosition;
begin
  Result := Manager.WindowPosition;
  if Result = awpAuto then
    case GetTaskBarPosition of
      ABE_LEFT:
        Result := awpBottomLeft;
      ABE_TOP:
        Result := awpTopRight;
      else
        Result := awpBottomRight;
    end;
end;

procedure TdxAlertWindowInitialLayout.Recalculate(APosition: TdxAlertWindowPosition; AMonitorNum: Integer; AIndex: Integer = -1);
var
  I: Integer;
  AColumnIndex: Integer;
  APreviousInitialLayout: TdxAlertWindowPositionInfo;
  AOldPosition: TRect;
begin
  APreviousInitialLayout := nil;
  for I := AIndex downto 0 do
    if (PositionInfoList[I].Position = APosition) and (PositionInfoList[I].MonitorNum = AMonitorNum) then
    begin
      APreviousInitialLayout := PositionInfoList[I];
      Break;
    end;

  for I := AIndex + 1 to PositionInfoList.Count - 1 do
    if (PositionInfoList[I].Position = APosition) and (PositionInfoList[I].MonitorNum = AMonitorNum) then
    begin
      AColumnIndex := PositionInfoList[I].ColumnIndex;
      AOldPosition := PositionInfoList[I].FInitialBounds;
      PositionInfoList[I].FInitialBounds :=
        GetNewPosition(PositionInfoList[I].FInitialBounds, APreviousInitialLayout,
          APosition, GetMonitorWorkArea(Screen.Monitors[AMonitorNum].Handle), AColumnIndex);

      if not cxRectIsEqual(AOldPosition, PositionInfoList[I].FInitialBounds) then
      begin
        PositionInfoList[I].ColumnIndex := AColumnIndex;
        PositionInfoList[I].FAlertWindow.CollapseEmptySlotsMove(
          PositionInfoList[I].FInitialBounds.TopLeft, Manager.OptionsAnimate.CollapseEmptySlotsAnimationTime);
      end;

      APreviousInitialLayout := PositionInfoList[I];
    end;
end;

function TdxAlertWindowInitialLayout.RemoveItemWithWindow(AAlertWindow: TdxAlertWindow): Integer;
var
  APosition: TdxAlertWindowPosition;
  AMonitorNum: Integer;
begin
  Result := HasWindow(AAlertWindow);
  if Result > -1 then
  begin
    APosition := PositionInfoList[Result].Position;
    AMonitorNum := PositionInfoList[Result].MonitorNum;
    PositionInfoList.FreeAndDelete(Result);
    if Manager.OptionsAnimate.FCollapseEmptySlots then
      Recalculate(APosition, AMonitorNum);
  end;
end;

{ TdxAlertWindowList }

function TdxAlertWindowList.Add(AClass: TdxAlertWindowClass): TdxAlertWindow;
begin
  Result := AClass.Create(nil);
  inherited Add(Result);
end;

function TdxAlertWindowList.GetItem(AIndex: Integer): TdxAlertWindow;
begin
  Result := TdxAlertWindow(inherited Items[AIndex]);
end;

{ TdxAlertWindowManagerOptionsAnimate }

constructor TdxAlertWindowManagerOptionsAnimate.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FCollapseEmptySlots := False;
  FCollapseEmptySlotsAnimationTime := dxAlertWindowDefaultShowingAnimationTime;
end;

procedure TdxAlertWindowManagerOptionsAnimate.Assign(Source: TPersistent);
begin
  if Source is TdxAlertWindowManagerOptionsAnimate then
  begin
    FCollapseEmptySlots := TdxAlertWindowManagerOptionsAnimate(Source).CollapseEmptySlots;
    FCollapseEmptySlotsAnimationTime := TdxAlertWindowManagerOptionsAnimate(Source).CollapseEmptySlotsAnimationTime;
  end;
  inherited Assign(Source);
end;

procedure TdxAlertWindowManagerOptionsAnimate.SetCollapseEmptySlots(AValue: Boolean);
var
  APosition: TdxAlertWindowPosition;
  AMonitorNum: Integer;
begin
  if  AValue <> FCollapseEmptySlots then
  begin
    FCollapseEmptySlots := AValue;
    if FCollapseEmptySlots then
      for AMonitorNum := 0 to Screen.MonitorCount - 1 do
        for APosition := awpTopLeft to awpBottomRight do
          (Owner as TdxAlertWindowManager).InitialLayout.Recalculate(APosition, AMonitorNum);
  end;
end;

{ TdxAlertWindowManager }

constructor TdxAlertWindowManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionsAnimate := TdxAlertWindowManagerOptionsAnimate.Create(Self);
  FOptionsBehavior := TdxAlertWindowOptionsBehavior.Create(Self);
  FOptionsButtons := TdxAlertWindowOptionsButtons.Create(Self);
  FOptionsCaptionButtons := TdxAlertWindowOptionsCaptionButtons.Create(Self);
  FOptionsMessage := TdxAlertWindowOptionsMessage.Create(Self);
  FOptionsNavigationPanel := TdxAlertWindowOptionsNavigationPanel.Create(Self);
  FOptionsSize := TdxAlertWindowOptionsSize.Create(Self);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FInitialLayout := CreateInitialLayoutCalculator;
  FAlertWindows := TdxAlertWindowList.Create;
  FWindowPosition := awpAuto;
  FWindowMaxCount := 0;
end;

destructor TdxAlertWindowManager.Destroy;
begin
  FreeAndNil(FAlertWindows);
  FreeAndNil(FInitialLayout);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FOptionsSize);
  FreeAndNil(FOptionsNavigationPanel);
  FreeAndNil(FOptionsMessage);
  FreeAndNil(FOptionsCaptionButtons);
  FreeAndNil(FOptionsButtons);
  FreeAndNil(FOptionsBehavior);
  FreeAndNil(FOptionsAnimate);
  inherited Destroy;
end;

procedure TdxAlertWindowManager.Close(AAlertWindow: TdxAlertWindow);
begin
  if InternalRemove(AAlertWindow) then
  begin
    DoClose(AAlertWindow);
    AAlertWindow.Release;
  end;
end;

function TdxAlertWindowManager.IndexOf(AAlertWindow: TdxAlertWindow): Integer;
begin
  Result := FAlertWindows.IndexOf(AAlertWindow);
end;

function TdxAlertWindowManager.Show(const ACaption, AText: string; AImageIndex: TcxImageIndex = -1): TdxAlertWindow;
begin
  Result := InternalAdd;
  Result.BeginUpdate;
  try
    Result.OptionsAnimate := OptionsAnimate;
    Result.OptionsBehavior := OptionsBehavior;
    Result.OptionsButtons := OptionsButtons;
    Result.OptionsCaptionButtons := OptionsCaptionButtons;
    Result.OptionsMessage := OptionsMessage;
    Result.OptionsNavigationPanel := OptionsNavigationPanel;
    Result.OptionsSize := OptionsSize;
    Result.LookAndFeel.MasterLookAndFeel := LookAndFeel;
    if Owner is TCustomForm then
      Result.BiDiMode := TCustomForm(Owner).BiDiMode
    else
      Result.BiDiMode := Application.BiDiMode;
    Result.RightToLeftLayout := bFalse;
    Result.MessageList.Add(ACaption, AText, AImageIndex);
    DoInitialize(Result);
  finally
    Result.EndUpdate;
  end;
  InitialLayout.AddItem;
end;

procedure TdxAlertWindowManager.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  OptionsBehavior.ChangeScale(M, D);
  OptionsButtons.ChangeScale(M, D);
  OptionsMessage.ChangeScale(M, D);
  OptionsNavigationPanel.ChangeScale(M, D);
  OptionsSize.ChangeScale(M, D);
end;

function TdxAlertWindowManager.CreateInitialLayoutCalculator: TdxAlertWindowInitialLayout;
begin
  Result := TdxAlertWindowInitialLayout.Create(Self);
end;

function TdxAlertWindowManager.GetAlertWindowClass: TdxAlertWindowClass;
begin
  Result := TdxAlertWindow;
end;

procedure TdxAlertWindowManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    InternalRemove(AComponent);
end;

procedure TdxAlertWindowManager.DoBeforeShow(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnBeforeShow) then
    FOnBeforeShow(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoButtonClick(AAlertWindow: TdxAlertWindow; AButtonIndex: Integer);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self, AAlertWindow, AButtonIndex);
end;

function TdxAlertWindowManager.DoCaptionButtonClick(
  AAlertWindow: TdxAlertWindow; AButton: TdxAlertWindowCaptionButton): Boolean;
begin
  Result := False;
  if Assigned(FOnCaptionButtonClick) then
    FOnCaptionButtonClick(Self, AAlertWindow, AButton, Result);
end;

procedure TdxAlertWindowManager.DoClick(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoClose(AALertWindow: TdxAlertWindow);
begin
  if Assigned(FOnClose) then
    FOnClose(Self, AAlertWindow);
end;

function TdxAlertWindowManager.DoCustomDrawBackground(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawBackground) then
    FOnCustomDrawBackground(Self, AAlertWindow, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindowManager.DoCustomDrawButton(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawButton) then
    FOnCustomDrawButton(Self, AAlertWindow, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindowManager.DoCustomDrawMessageCaptionText(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawMessageCaptionText) then
    FOnCustomDrawMessageCaptionText(Self, AAlertWindow, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindowManager.DoCustomDrawMessageImage(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawMessageImage) then
    FOnCustomDrawMessageImage(Self, AAlertWindow, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindowManager.DoCustomDrawMessageText(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawMessageText) then
    FOnCustomDrawMessageText(Self, AAlertWindow, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindowManager.DoCustomDrawNavigationPanelText(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo): Boolean;
begin
  Result := False;
  if Assigned(FOnCustomDrawNavigationPanelText) then
    FOnCustomDrawNavigationPanelText(Self, AAlertWindow, ACanvas, AViewInfo, Result);
end;

function TdxAlertWindowManager.DoDragBegin(AAlertWindow: TdxAlertWindow): Boolean;
begin
  Result := True;
  if Assigned(FOnDragBegin) then
    FOnDragBegin(Self, AAlertWindow, Result);
end;

procedure TdxAlertWindowManager.DoDragEnd(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnDragEnd) then
    FOnDragEnd(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoDragMove(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnDragMove) then
    FOnDragMove(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoHide(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnHide) then
    FOnHide(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoInitialize(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnInitialize) then
    FOnInitialize(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoMeasureMessageText(AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer);
begin
  if Assigned(FOnMeasureMessageText) then
    FOnMeasureMessageText(Self, AAlertWindow, AWidth, AHeight);
end;

procedure TdxAlertWindowManager.AlertWindowButtonClickHandler(AAlertWindow: TdxAlertWindow; AButtonIndex: Integer);
begin
  DoButtonClick(AAlertWindow, AButtonIndex);
end;

procedure TdxAlertWindowManager.AlertWindowCaptionButtonClickHandler(
  AAlertWindow: TdxAlertWindow; AButton: TdxAlertWindowCaptionButton; var AHandled: Boolean);
begin
  AHandled := DoCaptionButtonClick(AAlertWindow, AButton);
end;

procedure TdxAlertWindowManager.AlertWindowClickHandler(AAlertWindow: TdxAlertWindow);
begin
  DoClick(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowCustomDrawBackgroundHandler(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowViewInfo; var ADone: Boolean);
begin
  ADone := DoCustomDrawBackground(AAlertWindow, ACanvas, AViewInfo);
end;

procedure TdxAlertWindowManager.AlertWindowCustomDrawButtonHandler(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCustomButtonViewInfo; var ADone: Boolean);
begin
  ADone := DoCustomDrawButton(AAlertWindow, ACanvas, AViewInfo);
end;

procedure TdxAlertWindowManager.AlertWindowCustomDrawMessageCaptionTextHandler(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowCaptionTextViewInfo; var ADone: Boolean);
begin
  ADone := DoCustomDrawMessageCaptionText(AAlertWindow, ACanvas, AViewInfo);
end;

procedure TdxAlertWindowManager.AlertWindowCustomDrawMessageImageHandler(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowImageViewInfo; var ADone: Boolean);
begin
  ADone := DoCustomDrawMessageImage(AAlertWindow, ACanvas, AViewInfo);
end;

procedure TdxAlertWindowManager.AlertWindowCustomDrawMessageTextHandler(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowMessageTextViewInfo; var ADone: Boolean);
begin
  ADone := DoCustomDrawMessageText(AAlertWindow, ACanvas, AViewInfo);
end;

procedure TdxAlertWindowManager.AlertWindowCustomDrawNavigationPanelTextHandler(
  AAlertWindow: TdxAlertWindow; ACanvas: TcxCanvas; AViewInfo: TdxAlertWindowNavigationPanelTextViewInfo;
  var ADone: Boolean);
begin
  ADone := DoCustomDrawNavigationPanelText(AAlertWindow, ACanvas, AViewInfo);
end;

procedure TdxAlertWindowManager.AlertWindowDragBeginHandler(AAlertWindow: TdxAlertWindow; var AAllow: Boolean);
begin
  AAllow := DoDragBegin(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowDragEndHandler(AAlertWindow: TdxAlertWindow);
begin
  DoDragEnd(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowDragMoveHandler(AAlertWindow: TdxAlertWindow);
begin
  DoDragMove(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowEnterHandler(AAlertWindow: TdxAlertWindow);
begin
  FHotWindow := AAlertWindow;
  DoMouseEnter(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowHideHandler(AAlertWindow: TdxAlertWindow);
begin
  DoHide(AAlertWindow);
  Close(AAlertWindow);
  InitialLayout.AddItem;
end;

procedure TdxAlertWindowManager.AlertWindowLeaveHandler(AAlertWindow: TdxAlertWindow);
begin
  FHotWindow := nil;
  DoMouseLeave(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowMeasureMessageTextHandler(
  AAlertWindow: TdxAlertWindow; var AWidth, AHeight: Integer);
begin
  DoMeasureMessageText(AAlertWindow, AWidth, AHeight);
end;

procedure TdxAlertWindowManager.AlertWindowMouseDownHandler(
  AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  DoMouseDown(AAlertWindow, AButton, AShift, X, Y);
end;

procedure TdxAlertWindowManager.AlertWindowMouseMoveHandler(
  AAlertWindow: TdxAlertWindow; AShift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(AAlertWindow, AShift, X, Y);
end;

procedure TdxAlertWindowManager.AlertWindowMouseUpHandler(
  AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(AAlertWindow, AButton, AShift, X, Y);
end;

procedure TdxAlertWindowManager.AlertWindowMoveHandler(AAlertWindow: TdxAlertWindow);
var
  AIndexInLayout: Integer;
begin
  AIndexInLayout := InitialLayout.HasWindow(AAlertWindow);
  if (AIndexInLayout > -1) and InitialLayout.PositionInfoList[AIndexInLayout].IsPositionChanged then
    InitialLayout.DeleteItem(AAlertWindow);
end;

procedure TdxAlertWindowManager.AlertWindowResizeHandler(AAlertWindow: TdxAlertWindow; var ANewBounds: TRect);
begin
  InitialLayout.ResizeWindow(AAlertWindow, ANewBounds);
end;

procedure TdxAlertWindowManager.AlertWindowShowHandler(AAlertWindow: TdxAlertWindow);
begin
  DoShow(AAlertWindow);
end;

procedure TdxAlertWindowManager.DoMouseDown(
  AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, AAlertWindow, AButton, AShift, X, Y);
end;

procedure TdxAlertWindowManager.DoMouseEnter(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self, AAlertWindow);
end;

procedure TdxAlertWindowManager.DoMouseMove(AAlertWindow: TdxAlertWindow; AShift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, AAlertWindow, AShift, X, Y);
end;

procedure TdxAlertWindowManager.DoMouseUp(
  AAlertWindow: TdxAlertWindow; AButton: TMouseButton; AShift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, AAlertWindow, AButton, AShift, X, Y);
end;

procedure TdxAlertWindowManager.DoShow(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnShow) then
    FOnShow(Self, AAlertWindow);
end;

function TdxAlertWindowManager.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := LookAndFeel;
end;

procedure TdxAlertWindowManager.DoMouseLeave(AAlertWindow: TdxAlertWindow);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self, AAlertWindow);
end;

function TdxAlertWindowManager.GetCount: Integer;
begin
  Result := FAlertWindows.Count;
end;

function TdxAlertWindowManager.GetDisplayedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (Items[I].Visible) or (Items[I].VisibilityTransition <> awvtNone) then
      Inc(Result);
end;

function TdxAlertWindowManager.GetItem(AIndex: Integer): TdxAlertWindow;
begin
  Result := FAlertWindows[AIndex];
end;

function TdxAlertWindowManager.GetOptionsAnimate: TdxAlertWindowManagerOptionsAnimate;
begin
  Result := FOptionsAnimate;
end;

function TdxAlertWindowManager.GetOptionsBehavior: TdxAlertWindowOptionsBehavior;
begin
  Result := FOptionsBehavior;
end;

function TdxAlertWindowManager.GetOptionsButtons: TdxAlertWindowOptionsButtons;
begin
  Result := FOptionsButtons;
end;

function TdxAlertWindowManager.GetOptionsCaptionButtons: TdxAlertWindowOptionsCaptionButtons;
begin
  Result := FOptionsCaptionButtons
end;

function TdxAlertWindowManager.GetOptionsMessage: TdxAlertWindowOptionsMessage;
begin
  Result := FOptionsMessage;
end;

function TdxAlertWindowManager.GetOptionsNavigationPanel: TdxAlertWindowOptionsNavigationPanel;
begin
  Result := FOptionsNavigationPanel;
end;

function TdxAlertWindowManager.GetOptionsSize: TdxAlertWindowOptionsSize;
begin
  Result := FOptionsSize;
end;

function TdxAlertWindowManager.InternalAdd: TdxAlertWindow;
begin
  Result := FAlertWindows.Add(GetAlertWindowClass);
  Result.ScaleForPPI(ScaleFactor.Apply(dxDefaultDPI));
  Result.FreeNotification(Self);
  Result.OnButtonClick := AlertWindowButtonClickHandler;
  Result.OnCaptionButtonClick := AlertWindowCaptionButtonClickHandler;
  Result.OnClick := AlertWindowClickHandler;
  Result.OnCustomDrawBackground := AlertWindowCustomDrawBackgroundHandler;
  Result.OnCustomDrawButton := AlertWindowCustomDrawButtonHandler;
  Result.OnCustomDrawMessageCaptionText := AlertWindowCustomDrawMessageCaptionTextHandler;
  Result.OnCustomDrawMessageImage := AlertWindowCustomDrawMessageImageHandler;
  Result.OnCustomDrawMessageText := AlertWindowCustomDrawMessageTextHandler;
  Result.OnCustomDrawNavigationPanelText := AlertWindowCustomDrawNavigationPanelTextHandler;
  Result.OnDragBegin := AlertWindowDragBeginHandler;
  Result.OnDragEnd := AlertWindowDragEndHandler;
  Result.OnDragMove := AlertWindowDragMoveHandler;
  Result.OnHide := AlertWindowHideHandler;
  Result.OnMeasureMessageText := AlertWindowMeasureMessageTextHandler;
  Result.OnMouseDown := AlertWindowMouseDownHandler;
  Result.OnMouseEnter := AlertWindowEnterHandler;
  Result.OnMouseLeave := AlertWindowLeaveHandler;
  Result.OnMouseMove := AlertWindowMouseMoveHandler;
  Result.OnMouseUp := AlertWindowMouseUpHandler;
  Result.OnShow := AlertWindowShowHandler;
  Result.OnMove := AlertWindowMoveHandler;
  Result.OnResize := AlertWindowResizeHandler;
end;

function TdxAlertWindowManager.InternalRemove(AComponent: TComponent): Boolean;
begin
  Result := (FAlertWindows <> nil) and (FAlertWindows.Remove(AComponent) > -1);
  if HotWindow = AComponent then
    FHotWindow := nil;

  if Result then
    InitialLayout.DeleteItem(TdxAlertWindow(AComponent));
end;

procedure TdxAlertWindowManager.SetLookAndFeel(AValue: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsAnimate(AValue: TdxAlertWindowManagerOptionsAnimate);
begin
  FOptionsAnimate.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsBehavior(AValue: TdxAlertWindowOptionsBehavior);
begin
  FOptionsBehavior.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsButtons(AValue: TdxAlertWindowOptionsButtons);
begin
  FOptionsButtons.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsCaptionButtons(AValue: TdxAlertWindowOptionsCaptionButtons);
begin
  FOptionsCaptionButtons.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsMessage(AValue: TdxAlertWindowOptionsMessage);
begin
  FOptionsMessage.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsNavigationPanel(AValue: TdxAlertWindowOptionsNavigationPanel);
begin
  FOptionsNavigationPanel.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetOptionsSize(AValue: TdxAlertWindowOptionsSize);
begin
  FOptionsSize.Assign(AValue);
end;

procedure TdxAlertWindowManager.SetWindowPosition(AValue: TdxAlertWindowPosition);
begin
  FWindowPosition := AValue;
end;

procedure TdxAlertWindowManager.SetWindowMaxCount(AValue: Integer);
begin
  AValue := Max(0, AValue);
  if FWindowMaxCount <> AValue then
  begin
    FWindowMaxCount := Max(0, AValue);
    InitialLayout.AddItem;
  end;
end;

end.
