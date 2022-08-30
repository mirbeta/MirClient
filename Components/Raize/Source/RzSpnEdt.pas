{===============================================================================
  RzSpnEdt Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzRapidFireButton
    TSpeedButton descendant that generates OnClick events as button is held down.

  TRzSpinButtons
    Compound component with two TRzRapidFireButtons--Up/Down or Left/Right.

  TRzSpinEdit
    Enhanced TRzEdit that embeds a TRzSpinButtons component. Used for cycling
    through numeric values (Integer and Floating-point).

  TRzSpinner
    Alternate control for cycling through integer values. Buttons are on left
    and right sides of display area.


  Modification History
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * Fixed issue where ParentColor property in TRzSpinner and TRzDBSpinner was
      not getting streamed correctly to form files.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzSpinEdit, TRzSpinButtons, and
      TRzSpinner to fully support VCL Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzSpinEdit to support 64-bit development.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzSpinner controls has been updated such that it will hide the focus
      rectangle until the user navigates on the form using the keyboard. The
      control honors the Windows system setting that affects this behavior.
    * Fixed issue in TRzSpinner where changing the Color property would have no
      effect when XP/Vista themes were enabled.
    * Fixed issue in TRzSpinner where custom images would not appear if XP/Vista
      themes were enabled.
    * Fixed display issue in TRzSpinner where the spin buttons would not appear
      correctly in applications that were not XP/Vista Theme enabled.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed positioning of TRzSpinEdit buttons between XP, Vista, and Windows 7.
    * Updated display of spin arrows in TRzSpinEdit when running under Windows
      7 and using FlatButtons.
    * Fixed display of TRzSpinner themed buttons to accomodate for the
      difference in styles between XP, Vista, and Windows 7.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Changed the TRzSpinEdit.IntValue property to be of type Int64.
    * Fixed issue where setting the TRzSpinEdit.Max property to 0 would not get
      saved to the DFM file even though the default value of Max is 100.0.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzSpinEdit and TRzSpinner controls.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new TextHint and TextHintVisibleOnFocus properties to TRzSpinEdit.
      The TextHint property allows a developer to specify a prompt that appears
      inside the edit area of the control, when the control is empty. The text
      prompt appears grayed. By default, the prompt is automatically hidden when
      the control receives focus. To keep the prompt visible until the user
      enters a value, set the TextHintVisibleOnFocus property to True.
      NOTES:
        - Please note that for the TextHint to appear, the AllowBlank property
          has to be set to True *and* the Text property has to programmatically
          be cleared (e.g. RzSpinEdit1.Text := '';).
        - Please see the comments in RzEdit.pas for more details on TextHint
          and TextHintVisibleOnFocus.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Surfaced ParentFont property in TRzSpinner.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added BeepOnInvalidKey property to TRzSpinEdit.
    * Fixed problem where buttons in the TRzSpinButtons were not positioned
      correctly in vertical orientation.
    * Fixed problem where the buttons in a TRzSpinEdit would change color when
      the control was focused.
    * Fixed problem in TRzSpinEdit that resulted in the spin button arrows
      looking distorted on Windows 98 systems.
    * Adjusted positioning and size of spin buttons in TRzSpinEdit.
    * Improved the display of the TRzSpinner control.
    * Added hot tracking support to TRzSpinner.
    * Added XP Theme support to TRzSpinner.
    * Added FrameController and FrameControllerNotifications properties to 
      TRzSpinner.
    * Added ShowFocusRect property to TRzSpinner.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surface OnMouseWheel event in TRzSpinEdit.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Surfaced ReadOnlyColorOnFocus property.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Surfaced InitialDelay and Delay properties for TRzSpinButtons.
    * Added ReadOnlyColor property to TRzSpinEdit. This color property is used
      to change the color of the control when the ReadOnly property is set to
      True.
    * When the ReadOnly property for a TRzSpinEdit is set to True, the spin
      buttons are hidden.
    * Added new FrameControllerNotifications property to TRzSpinEdit.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Changed the signature of the TRzSpinEdit.OnChanging event. The previous
      version did not include a parameter for the NewValue the spin edit would
      like to change to. Without this parameter, the OnChanging event was pretty
      much useless.  The NewValue parameter has been added to the event
      signature.

      IMPORTANT NOTE!
      If you are already handling this event in your existing code you will need
      to manually add the NewValue: Extended parameter to your event handler.
      For example, if your old event handle looks like this:

      procedure TForm1.RzSpinEdit1Changing( Sender: TObject;
                                            var AllowChange: Boolean );

      you would change it to the following:

      procedure TForm1.RzSpinEdit1Changing( Sender: TObject;
                                            NewValue: Extended;
                                            var AllowChange: Boolean );
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed issue where cursor position moved when user changed the value of
      the TRzSpinEdit.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Surfaced Align, Anchors, and Constraints properties for TRzSpinner.
    * Added ShowHexValue and HexValuePrefix properties to TRzSpinner.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem in CMFontChanged method where SetEditRect could be called
      before a window handle has been allocated for the TRzSpinEdit.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * System cursor (IDC_HAND) is now used in TRzSpinner instead of the custom
      hand cursor when running under Windows 98 or higher.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * The color of flat buttons in TRzSpinEdit are now adjusted appropriately
      when the control is disabled and re-enabled.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzSpinEdit >>
    * Fixed display problem when button clicked and focus taken away by an
      exception.
    * Fixed problem where changing the font color in the OnExit and OnEnter
      events caused the text to disappear.
    * The OnChange event is no longer fired when the TRzDBSpinEdit control
      receives the keyboard focus.
    * The Buttons are now placed on the left side of the control for
      Right-To-Left locales.
    * Fixed problem where disabling the control did not cause the buttons to
      appear disabled.

    << TRzRapidFireButton >>
    * Added ScrollStyle property, which allows the user to easily set the glyph
      of the button to a scroll arrow.

    << TRzSpinButtons >>
    * Fixed display problem when button clicked and focus taken away by an
      exception.

    << TRzSpinner >>
    * Redesigned TRzSpinner to remove embedded controls. Also can use an image
      for button images.
    * Fixed display problem when button clicked and focus taken away by an
      exception.
===============================================================================}

{$I RzComps.inc}

unit RzSpnEdt;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Messages,
  Windows,
  StdCtrls,
  ExtCtrls,
  Controls,
  SysUtils,
  Forms,
  Graphics,
  Menus,
  Mask,
  RzEdit,
  Buttons,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Classes,
  RzButton,
  RzCommon;


type
  {==========================================}
  {== TRzRapidFireButton Class Declaration ==}
  {==========================================}

  TRzRapidFireStyle = ( rfFocusRect, rfAllowTimer );
  TRzRapidFireStyles = set of TRzRapidFireStyle;

  TRzRapidFireButton = class( TSpeedButton )
  private
    FAboutInfo: TRzAboutInfo;
    FRepeatTimer: TTimer;
    FInitialDelay: Word;
    FDelay: Word;
    FStyle: TRzRapidFireStyles;
    FScrollStyle: TRzScrollStyle;

    { Internal Event Handlers }
    procedure TimerExpired( Sender: TObject );
  protected
    procedure Paint; override;
    procedure AssignArrowGlyph;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetScrollStyle( Value: TRzScrollStyle ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Style: TRzRapidFireStyles
      read FStyle
      write FStyle;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Delay: Word
      read FDelay
      write FDelay
      default 100;

    property InitialDelay: Word
      read FInitialDelay
      write FInitialDelay
      default 400;

    property ScrollStyle: TRzScrollStyle
      read FScrollStyle
      write SetScrollStyle
      default scsNone;
  end;


  {======================================}
  {== TRzSpinButtons Class Declaration ==}
  {======================================}

  TSpinDirection = ( sdUpDown, sdLeftRight );
  TSpinButtonType = ( sbUp, sbDown );
  TSpinChangingEvent = procedure( Sender: TObject;
                                  NewValue: Extended;
                                  var AllowChange: Boolean ) of object;
  TSpinButtonEvent = procedure( Sender: TObject;
                                Button: TSpinButtonType ) of object;

  TRzSpinButtons = class( TWinControl )
  private
    FAboutInfo: TRzAboutInfo;
    FDirection: TSpinDirection;
    FOrientation: TOrientation;
    FUpRightButton: TRzControlButton;
    FDownLeftButton: TRzControlButton;
    FFocusedButton: TRzControlButton;

    FFocusControl: TWinControl;
    FFlat: Boolean;
    FOnUpRightClick: TNotifyEvent;
    FOnDownLeftClick: TNotifyEvent;

    { Message Handling Methods }
    procedure WMSize( var Msg: TWMSize );  message wm_Size;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
  protected
    FCustomUpRightGlyph: Boolean;
    FCustomDownLeftGlyph: Boolean;

    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function CreateButton: TRzControlButton; virtual;
    procedure BtnClickHandler( Sender: TObject ); virtual;
    procedure BtnMouseDownHandler( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); virtual;
    procedure BtnGlyphChangeHandler( Sender: TObject ); virtual;

    procedure AdjustDimensions( var W, H: Integer ); virtual;

    { Event Dispatch Methods }
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure UpRightClick; dynamic;
    procedure DownLeftClick; dynamic;

    { Property Access Methods }
    procedure SetDirection( Value: TSpinDirection ); virtual;
    function GetAllEnabled: Boolean; virtual;
    procedure SetAllEnabled( Value: Boolean ); virtual;
    function GetColor: TColor; virtual;
    procedure SetColor( Value: TColor ); virtual;
    procedure SetFlat( Value: Boolean ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
    function GetInitialDelay: Word;
    procedure SetInitialDelay( Value: Word );
    function GetDelay: Word;
    procedure SetDelay( Value: Word );

    function GetUpRightGlyph: TBitmap; virtual;
    procedure SetUpRightGlyph( Value: TBitmap ); virtual;
    function GetDownLeftGlyph: TBitmap; virtual;
    procedure SetDownLeftGlyph( Value: TBitmap ); virtual;
    function GetUpRightNumGlyphs: TNumGlyphs; virtual;
    procedure SetUpRightNumGlyphs( Value: TNumGlyphs ); virtual;
    function GetDownLeftNumGlyphs: TNumGlyphs; virtual;
    procedure SetDownLeftNumGlyphs( Value: TNumGlyphs ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

    property UpRightButton: TRzControlButton
      read FUpRightButton;

    property DownLeftButton: TRzControlButton
      read FDownLeftButton;

    property CustomUpRightGlyph: Boolean
      read FCustomUpRightGlyph;

    property CustomDownLeftGlyph: Boolean
      read FCustomDownLeftGlyph;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Color: TColor
      read GetColor
      write SetColor
      default clBtnFace;

    property Delay: Word
      read GetDelay
      write SetDelay
      default 100;

    property Direction: TSpinDirection
      read FDirection
      write SetDirection
      default sdUpDown;

    property Enabled: Boolean
      read GetAllEnabled
      write SetAllEnabled
      default True;

    property Flat: Boolean
      read FFlat
      write SetFlat
      default False;

    property FocusControl: TWinControl
      read FFocusControl
      write FFocusControl;

    property GlyphUpRight: TBitmap
      read GetUpRightGlyph
      write SetUpRightGlyph
      stored FCustomUpRightGlyph;

    property GlyphDownLeft: TBitmap
      read GetDownLeftGlyph
      write SetDownLeftGlyph
      stored FCustomDownLeftGlyph;

    property InitialDelay: Word
      read GetInitialDelay
      write SetInitialDelay
      default 400;

    property NumGlyphsUpRight: TNumGlyphs
      read GetUpRightNumGlyphs
      write SetUpRightNumGlyphs
      stored FCustomUpRightGlyph
      default 2;

    property NumGlyphsDownLeft: TNumGlyphs
      read GetDownLeftNumGlyphs
      write SetDownLeftNumGlyphs
      stored FCustomDownLeftGlyph
      default 2;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orVertical;

    property OnDownLeftClick: TNotifyEvent
      read FOnDownLeftClick
      write FOnDownLeftClick;

    property OnUpRightClick: TNotifyEvent
      read FOnUpRightClick
      write FOnUpRightClick;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;


  {===================================}
  {== TRzSpinEdit Class Declaration ==}
  {===================================}

  TRzSpinEdit = class( TRzCustomEdit )
  private
    FAboutInfo: TRzAboutInfo;
    FAllowKeyEdit: Boolean;
    FAllowBlank: Boolean;
    FBlankValue: Extended;
    FButtons: TRzSpinButtons;
    FButtonWidth: Integer;
    FCheckRange: Boolean;
    FDecimals: Byte;
    FIncrement: Extended;
    FIntegersOnly: Boolean;
    FMin: Extended;
    FMax: Extended;
    FPageSize: Extended;
    FFlatButtonColor: TColor;

    FOnChanging: TSpinChangingEvent;
    FOnButtonClick: TSpinButtonEvent;

    { Message Handling Methods }
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure WMPaste( var Msg: TWMPaste ); message wm_Paste;
    procedure WMCut( var Msg: TWMCut ); message wm_Cut;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
  protected
    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); override;

    function IsCustomDownGlyph: Boolean;
    function IsCustomUpGlyph: Boolean;

    procedure ReadOnlyChanged; override;
    procedure ResizeButtons; virtual;
    function GetEditRect: TRect; override;
    procedure SetEditRect; virtual; //!! change to override;

    function IsValidChar( Key: Char ): Boolean; virtual;
    procedure UpRightClickHandler( Sender: TObject ); virtual;
    procedure DownLeftClickHandler( Sender: TObject ); virtual;

    { Event Dispatch Methods }
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress(var Key: Char); override;
    function CanChange( NewValue: Extended ): Boolean; dynamic;
    procedure DoButtonClick( S: TSpinButtonType ); dynamic;
    procedure IncValue( const Amount: Extended ); virtual;
    procedure DecValue( const Amount: Extended ); virtual;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    procedure SetFrameStyle( Value: TFrameStyle ); override;
    procedure SetButtonWidth( Value: Integer ); virtual;
    procedure SetDecimals( Value: Byte ); virtual;
    procedure SetIntegersOnly( Value: Boolean ); virtual;

    function GetButton( Index: Integer ): TRzControlButton; virtual;

    function GetButtonUpGlyph: TBitmap; virtual;
    procedure SetButtonUpGlyph( Value: TBitmap ); virtual;
    function GetButtonUpNumGlyphs: TNumGlyphs; virtual;
    procedure SetButtonUpNumGlyphs( Value: TNumGlyphs ); virtual;
    function GetButtonDownGlyph: TBitmap; virtual;
    procedure SetButtonDownGlyph( Value: TBitmap ); virtual;
    function GetButtonDownNumGlyphs: TNumGlyphs; virtual;
    procedure SetButtonDownNumGlyphs( Value: TNumGlyphs ); virtual;

    function GetDirection: TSpinDirection; virtual;
    procedure SetDirection( Value: TSpinDirection ); virtual;
    procedure SetFlatButtons( Value: Boolean ); override;
    function GetOrientation: TOrientation; virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;

    procedure SetCheckRange( Value: Boolean ); virtual;
    procedure SetMin( const Value: Extended ); virtual;
    procedure SetMax( const Value: Extended ); virtual;

    function GetIntValue: Int64; virtual;
    procedure SetIntValue( Value: Int64 ); virtual;
    function GetValue: Extended; virtual;
    function CheckValue( const Value: Extended ): Extended; virtual;
    procedure SetValue( const Value: Extended); virtual;

    function StoreIncrement: Boolean;
    function StorePageSize: Boolean;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;

    property Buttons: TRzSpinButtons
      read FButtons;

    property DownLeftButton: TRzControlButton
      index 1
      read GetButton;

    property UpRightButton: TRzControlButton
      index 2
      read GetButton;

    property IntValue: Int64
      read GetIntValue
      write SetIntValue;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AllowBlank: Boolean
      read FAllowBlank
      write FAllowBlank
      default False;

    property BlankValue: Extended
      read FBlankValue
      write FBlankValue;

    property AllowKeyEdit: Boolean
      read FAllowKeyEdit
      write FAllowKeyEdit
      default False;

    property ButtonDownGlyph: TBitmap
      read GetButtonDownGlyph
      write SetButtonDownGlyph
      stored IsCustomDownGlyph;

    property ButtonDownNumGlyphs: TNumGlyphs
      read GetButtonDownNumGlyphs
      write SetButtonDownNumGlyphs
      stored IsCustomDownGlyph;

    property ButtonUpGlyph: TBitmap
      read GetButtonUpGlyph
      write SetButtonUpGlyph
      stored IsCustomUpGlyph;

    property ButtonUpNumGlyphs: TNumGlyphs
      read GetButtonUpNumGlyphs
      write SetButtonUpNumGlyphs
      stored IsCustomUpGlyph;

    property ButtonWidth: Integer
      read FButtonWidth
      write SetButtonWidth
      default 17;

    property CheckRange: Boolean
      read FCheckRange
      write SetCheckRange
      default False;

    property Decimals: Byte
      read FDecimals
      write SetDecimals
      default 0;

    property Direction: TSpinDirection
      read GetDirection
      write SetDirection
      default sdUpDown;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write FFlatButtonColor
      default clBtnFace;

    property Increment: Extended
      read FIncrement
      write FIncrement
      stored StoreIncrement;

    property IntegersOnly: Boolean
      read FIntegersOnly
      write SetIntegersOnly
      default True;

    property Max: Extended
      read FMax
      write SetMax
      stored True;

    property Min: Extended
      read FMin
      write SetMin
      stored True;

    property Orientation: TOrientation
      read GetOrientation
      write SetOrientation
      default orVertical;

    property PageSize: Extended
      read FPageSize
      write FPageSize
      stored StorePageSize;

    property Value: Extended
      read GetValue
      write SetValue;

    property OnChanging: TSpinChangingEvent
      read FOnChanging
      write FOnChanging;

    property OnButtonClick: TSpinButtonEvent
      read FOnButtonClick
      write FOnButtonClick;

    { Inherited Properties & Events }
    property Align;
    property Alignment default taRightJustify;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BeepOnInvalidKey;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatButtons;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property HideSelection;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TextHint;
    property TextHintVisibleOnFocus;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  {==================================}
  {== TRzSpinner Class Declaration ==}
  {==================================}

  TRzSpinnerArea = ( saValue, saMinusButton, saPlusButton );
  TRzSpinnerEvent = procedure ( Sender: TObject; NewValue: Integer;
                                var AllowChange: Boolean ) of object;

  TRzSpinnerIncDecEvent = procedure ( Sender: TObject; Amount: Integer ) of object;

  TRzSpinner = class( TCustomControl, IRzCustomFramingNotification )
  private
    FAboutInfo: TRzAboutInfo;
    FValue: Integer;
    FMin: Integer;
    FMax: Integer;
    FIncrement: Integer;
    FPageSize: Integer;
    FCheckRange: Boolean;
    FTabOnEnter: Boolean;

    FShowHexValue: Boolean;
    FHexValuePrefix: string;

    FFrameColor: TColor;
    FButtonColor: TColor;
    FButtonWidth: Integer;
    FShowFocusRect: Boolean;
    FMinusBtnDown: Boolean;
    FPlusBtnDown: Boolean;
    FLastHotTrackArea: TRzSpinnerArea;
    FOverPlusButton: Boolean;
    FOverMinusButton: Boolean;
    FThemeAware: Boolean;

    FRepeatTimer: TTimer;
    FInitialDelay: Word;
    FDelay: Word;

    // FGlyphBitmap is used to hold old glyph data from Spinners stored in RC 2.x format
    FGlyphBitmap: TBitmap;

    FImages: TCustomImageList;
    FImageIndexes: array[ 1..2 ] of TImageIndex;
    FImagesChangeLink: TChangeLink;

    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    FOnChange: TNotifyEvent;
    FOnChanging: TRzSpinnerEvent;
    FOnIncrement: TRzSpinnerIncDecEvent;
    FOnDecrement: TRzSpinnerIncDecEvent;

    { Internal Event Handlers }
    procedure TimerExpired( Sender: TObject );
    procedure ImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message cm_DesignHitTest;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
  protected
    procedure CreateWnd; override;
    procedure Notification( AComponent : TComponent; Operation : TOperation ); override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure Paint; override;
    procedure DrawFrame; virtual;
    procedure DrawValue; virtual;
    procedure DrawButton( Area: TRzSpinnerArea; Down: Boolean; Bounds: TRect ); virtual;
    function UseCustomImages( Area: TRzSpinnerArea ): Boolean;
    procedure CalcCenterOffsets( Bounds: TRect; var L, T: Integer);
    procedure CheckMinSize;
    procedure CheckHotTracking( P: TPoint );
    procedure CustomFramingChanged; virtual;

    // Support Methods
    procedure DecValue( Amount: Integer ); virtual;
    procedure IncValue( Amount: Integer ); virtual;

    function ShowFocus: Boolean;
    function CursorPosition: TPoint;
    function HitTest( P: TPoint ): TRzSpinnerArea; overload;
    function HitTest( X, Y: Integer ): TRzSpinnerArea; overload;


    // New Event Dispatch Methods
    procedure Change; dynamic;
    function CanChange( NewValue: Integer ): Boolean; dynamic;

    // Overridden Event Dispatch Methods
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress( var Key: Char ); override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    procedure SetButtonColor( Value: TColor ); virtual;
    procedure SetButtonWidth( Value: Integer ); virtual;
    procedure SetCheckRange( Value: Boolean ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;
    function GetImageIndex( PropIndex: Integer ): TImageIndex; virtual;
    procedure SetImageIndex( PropIndex: Integer; Value: TImageIndex ); virtual;
    procedure SetMax( Value: Integer ); virtual;
    procedure SetMin( Value: Integer ); virtual;
    function CheckValue( Value: Integer ): Integer; virtual;
    procedure SetValue( Value: Integer ); virtual;
    procedure SetShowHexValue( Value: Boolean ); virtual;
    procedure SetHexValuePrefix( const Value: string ); virtual;
    function StoreHexValuePrefix: Boolean;
    procedure SetThemeAware( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ButtonColor: TColor
      read FButtonColor
      write SetButtonColor
      default clBtnFace;

    property ButtonWidth: Integer
      read FButtonWidth
      write SetButtonWidth
      default 18;

    property CheckRange: Boolean
      read FCheckRange
      write SetCheckRange
      default False;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      default clBtnShadow;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property GlyphMinus: TBitmap
      read FGlyphBitmap
      stored False;

    property GlyphPlus: TBitmap
      read FGlyphBitmap
      stored False;

    property HexValuePrefix: string
      read FHexValuePrefix
      write SetHexValuePrefix
      stored StoreHexValuePrefix;

    property Increment: Integer
      read FIncrement
      write FIncrement
      default 1;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property ImageIndexMinus: TImageIndex
      index 1
      read GetImageIndex
      write SetImageIndex
      default -1;

    property ImageIndexPlus: TImageIndex
      index 2
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Max: Integer
      read FMax
      write SetMax
      default 100;

    property Min: Integer
      read FMin
      write SetMin
      default 0;

    property PageSize: Integer
      read FPageSize
      write FPageSize
      default 10;

    property ShowHexValue: Boolean
      read FShowHexValue
      write SetShowHexValue
      default False;

    property ShowFocusRect: Boolean
      read FShowFocusRect
      write FShowFocusRect
      default True;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;

    property Value: Integer
      read FValue
      write SetValue
      default 0;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    property OnChanging: TRzSpinnerEvent
      read FOnChanging
      write FOnChanging;

    property OnIncrement: TRzSpinnerIncDecEvent
      read FOnIncrement
      write FOnIncrement;

    property OnDecrement: TRzSpinnerIncDecEvent
      read FOnDecrement
      write FOnDecrement;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height default 21;
    property HelpContext;
    property Hint;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;
    property Width default 85;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


implementation

uses
  {&RAS}
  Themes,
  UxTheme,
  RzCommonCursors;

const
  DefaultIncrement: Extended = 1.0;
  DefaultPageSize: Extended  = 10.0;
  DefaultMin: Extended       = 0.0;
  DefaultMax: Extended       = 100.0;


{&RT}
{================================}
{== TRzRapidFireButton Methods ==}
{================================}

constructor TRzRapidFireButton.Create( AOwner: TComponent );
begin
  inherited;
  FInitialDelay := 400;                                     { 400 milliseconds }
  FDelay := 100;                                            { 100 milliseconds }
  FStyle := [ rfAllowTimer ];
  {&RCI}
  {&RV}
end;

destructor TRzRapidFireButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited;
end;


procedure TRzRapidFireButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if rfAllowTimer in FStyle then
  begin
    if FRepeatTimer = nil then
    begin
      FRepeatTimer := TTimer.Create( Self );
      FRepeatTimer.OnTimer := TimerExpired;
    end;
    FRepeatTimer.Interval := FInitialDelay;
    FRepeatTimer.Enabled := True;
  end;
end;


procedure TRzRapidFireButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
end;


procedure TRzRapidFireButton.TimerExpired( Sender: TObject );
begin
  FRepeatTimer.Interval := FDelay;
  if ( FState = bsDown ) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;


procedure TRzRapidFireButton.Paint;
var
  R: TRect;
begin
  inherited;

  if rfFocusRect in FStyle then
  begin
    R := Bounds( 0, 0, Width, Height );
    InflateRect( R, -3, -3 );
    if FState = bsDown then
      OffsetRect( R, 1, 1 );
    DrawFocusRect( Canvas.Handle, R );
  end;
end;


procedure TRzRapidFireButton.AssignArrowGlyph;
var
  DestRect, SrcRect, FrameRect: TRect;
  Flags: Integer;
  ArrowGlyph, B: TBitmap;
begin
  ArrowGlyph := TBitmap.Create;
  try
    if FScrollStyle in [ scsLeft, scsRight ] then
    begin
      ArrowGlyph.Width := 9;
      ArrowGlyph.Height := 9;
    end
    else
    begin
      ArrowGlyph.Width := 9;
      ArrowGlyph.Height := 6;
    end;

    DestRect := Bounds( 0, 0, ArrowGlyph.Width, ArrowGlyph.Height );
    FrameRect := Rect( 0, 0, 17, 17 );

    B := TBitmap.Create;
    try
      B.Width := 17;
      B.Height := 17;

      Flags := 0;
      case FScrollStyle of
        scsLeft:
        begin
          Flags := dfcs_ScrollLeft;
          SrcRect := Bounds( 4, 4, ArrowGlyph.Width, ArrowGlyph.Height );
        end;

        scsUp:
        begin
          Flags := dfcs_ScrollUp;
          SrcRect := Bounds( 4, 5, ArrowGlyph.Width, ArrowGlyph.Height );
        end;

        scsRight:
        begin
          Flags := dfcs_ScrollRight;
          SrcRect := Bounds( 6, 4, ArrowGlyph.Width, ArrowGlyph.Height );
        end;

        scsDown:
        begin
          Flags := dfcs_ScrollDown;
          SrcRect := Bounds( 4, 6, ArrowGlyph.Width, ArrowGlyph.Height );
        end;
      end;

      if Down then
        Flags := Flags or dfcs_Pushed;
      if not Enabled then
        Flags := Flags or dfcs_Inactive;

      DrawFrameControl( B.Canvas.Handle, FrameRect, dfc_Scroll, Flags );

      // Copy only the arrow and not the border
      ArrowGlyph.Canvas.CopyRect( DestRect, B.Canvas, SrcRect );
    finally
      B.Free;
    end;

    Glyph.Assign( ArrowGlyph );
  finally
    ArrowGlyph.Free;
  end;
end; {= TRzRapidFireButton.AssignArrowGlyph =}



procedure TRzRapidFireButton.SetScrollStyle( Value: TRzScrollStyle );
begin
  if FScrollStyle <> Value then
  begin
    FScrollStyle := Value;

    Glyph := nil;  // Delphi 6 requires this statement

    if FScrollStyle <> scsNone then
      AssignArrowGlyph;

    Invalidate;
  end;
end;


{============================}
{== TRzSpinButtons Methods ==}
{============================}

constructor TRzSpinButtons.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csAcceptsControls, csSetCaption ];

  FOrientation := orVertical;
  FFlat := False;

  FUpRightButton := CreateButton;
  FDownLeftButton := CreateButton;
  GlyphUpRight := nil;
  GlyphDownLeft := nil;

  Width := 16;
  Height := 25;
  FCustomUpRightGlyph := False;
  FCustomDownLeftGlyph := False;

  FFocusedButton := FUpRightButton;
  {&RCI}
end;


function TRzSpinButtons.CreateButton: TRzControlButton;
begin
  Result := TRzControlButton.Create( Self );
  Result.Parent := Self;
  Result.OnClick := BtnClickHandler;
  Result.OnMouseDown := BtnMouseDownHandler;
  Result.Visible := True;
  Result.Enabled := True;
  Result.RepeatClicks := True;
  Result.Glyph.OnChange := BtnGlyphChangeHandler;
  Result.NumGlyphs := 2;
end;


procedure TRzSpinButtons.Loaded;
var
  W, H: Integer;
begin
  {&RV}
  inherited;
  W := Width;
  H := Height;
  AdjustDimensions( W, H );
  if ( W <> Width ) or ( H <> Height ) then
    inherited SetBounds( Left, Top, W, H );
end;


procedure TRzSpinButtons.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFocusControl ) then
    FFocusControl := nil;
end;

procedure TRzSpinButtons.AdjustDimensions( var W, H: Integer );
var
  Mid: Integer;
begin
  if ( FUpRightButton = nil ) or ( csLoading in ComponentState ) then
    Exit;

  if FOrientation = orVertical then
  begin
    Mid := H div 2;
    FUpRightButton.SetBounds( 0, 0, W, Mid );
    FDownLeftButton.SetBounds( 0, Mid, W, H - Mid );
  end
  else
  begin
    Mid := W div 2;
    FUpRightButton.SetBounds( Mid, 0, W - Mid, H );
    FDownLeftButton.SetBounds( 0, 0, Mid, H );
  end;
end; {= TRzSpinButtons.AdjustDimensions =}


procedure TRzSpinButtons.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustDimensions( W, H );
  inherited SetBounds( ALeft, ATop, W, H );
end;


procedure TRzSpinButtons.KeyDown( var Key: Word; Shift: TShiftState );
begin
  case Key of
    vk_Up:
      FUpRightButton.Click;

    vk_Down:
      FDownLeftButton.Click;
  end;
  inherited;
end;


procedure TRzSpinButtons.BtnMouseDownHandler( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if Button = mbLeft then
  begin
    if ( FFocusControl <> nil ) and FFocusControl.TabStop and
       FFocusControl.CanFocus and
       ( GetFocus <> FFocusControl.Handle ) then
    begin
      Windows.SetFocus( FFocusControl.Handle );
      if not FFocusControl.Focused then
      begin
        // If FFocusControl is not focused, which may happen as the result
        // of a validation exception on a data-aware control, then abort
        // the clicking process of this button.  The call to Abort will
        // prevent the button from being drawn in the down state and not
        // being repainted after the exception message is closed.
        Abort;
      end;
    end
    else if TabStop and ( GetFocus <> Handle ) and CanFocus then
    begin
      Windows.SetFocus( Handle );
      if not Focused then
        Abort;
    end;
  end;
end;


procedure TRzSpinButtons.UpRightClick;
begin
  if Assigned( FOnUpRightClick ) then
    FOnUpRightClick( Self );
end;


procedure TRzSpinButtons.DownLeftClick;
begin
  if Assigned( FOnDownLeftClick ) then
    FOnDownLeftClick( Self );
end;


procedure TRzSpinButtons.BtnClickHandler( Sender: TObject );
begin
  if Sender = FUpRightButton then
    UpRightClick
  else
    DownLeftClick;
end;


procedure TRzSpinButtons.BtnGlyphChangeHandler( Sender: TObject );
begin
  if Sender = FUpRightButton.Glyph then
  begin
    FCustomUpRightGlyph := not FUpRightButton.Glyph.Empty;
    if FCustomUpRightGlyph then
      FUpRightButton.Style := cbsNone
    else
    begin
      if FDirection = sdUpDown then
        FUpRightButton.Style := cbsUp
      else
        FUpRightButton.Style := cbsRight;
    end;
  end
  else
  begin
    FCustomDownLeftGlyph := not FDownLeftButton.Glyph.Empty;
    if FCustomDownLeftGlyph then
      FDownLeftButton.Style := cbsNone
    else
    begin
      if FDirection = sdUpDown then
        FDownLeftButton.Style := cbsDown
      else
        FDownLeftButton.Style := cbsLeft;
    end;
  end;
end;


function TRzSpinButtons.GetUpRightGlyph: TBitmap;
begin
  Result := FUpRightButton.Glyph
end;


procedure TRzSpinButtons.SetUpRightGlyph( Value: TBitmap );
begin
  if Value <> nil then
  begin
    FUpRightButton.Glyph := Value;
    FCustomUpRightGlyph := True;
    FUpRightButton.Style := cbsNone;
  end
  else
  begin
    FUpRightButton.Glyph := nil;  // Delphi 6 requires this statement

    if FDirection = sdUpDown then
      FUpRightButton.Style := cbsUp
    else
      FUpRightButton.Style := cbsRight;

    FCustomUpRightGlyph := False;
  end;
end;


function TRzSpinButtons.GetUpRightNumGlyphs: TNumGlyphs;
begin
  Result := FUpRightButton.NumGlyphs;
end;


procedure TRzSpinButtons.SetUpRightNumGlyphs( Value: TNumGlyphs );
begin
  FUpRightButton.NumGlyphs := Value;
end;


function TRzSpinButtons.GetDownLeftGlyph: TBitmap;
begin
  Result := FDownLeftButton.Glyph;
end;


procedure TRzSpinButtons.SetDownLeftGlyph( Value: TBitmap );
begin
  if Value <> nil then
  begin
    FDownLeftButton.Glyph := Value;
    FCustomDownLeftGlyph := True;
    FDownLeftButton.Style := cbsNone;
  end
  else
  begin
    FDownLeftButton.Glyph := nil;  // Delphi 6 requires this statement

    if FDirection = sdUpDown then
      FDownLeftButton.Style := cbsDown
    else
      FDownLeftButton.Style := cbsLeft;
    FCustomDownLeftGlyph := False;
  end;
end;


function TRzSpinButtons.GetDownLeftNumGlyphs: TNumGlyphs;
begin
  Result := FDownLeftButton.NumGlyphs;
end;


procedure TRzSpinButtons.SetDownLeftNumGlyphs( Value: TNumGlyphs );
begin
  FDownLeftButton.NumGlyphs := Value;
end;


procedure TRzSpinButtons.SetDirection( Value: TSpinDirection );
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    { Update the glyphs }
    SetUpRightGlyph( nil );
    SetDownLeftGlyph( nil );
    Invalidate;
  end;
end;


function TRzSpinButtons.GetAllEnabled: Boolean;
begin
  Result := inherited Enabled;
end;


procedure TRzSpinButtons.SetAllEnabled( Value: Boolean );
begin
  if inherited Enabled <> Value then
  begin
    inherited Enabled := Value;
    FUpRightButton.Enabled := Value;
    FDownLeftButton.Enabled := Value;
  end;
end;


function TRzSpinButtons.GetColor: TColor;
begin
  Result := inherited Color;
end;


procedure TRzSpinButtons.SetColor( Value: TColor );
begin
  inherited Color := Value;
  FUpRightButton.Color := Value;
  FDownLeftButton.Color := Value;
end;


function TRzSpinButtons.GetDelay: Word;
begin
  Result := FUpRightButton.Delay;
end;


procedure TRzSpinButtons.SetDelay( Value: Word );
begin
  FUpRightButton.Delay := Value;
  FDownLeftButton.Delay := Value;
end;


function TRzSpinButtons.GetInitialDelay: Word;
begin
  Result := FUpRightButton.InitialDelay;
end;


procedure TRzSpinButtons.SetInitialDelay( Value: Word );
begin
  FUpRightButton.InitialDelay := Value;
  FDownLeftButton.InitialDelay := Value;
end;


procedure TRzSpinButtons.SetFlat( Value: Boolean );
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    FUpRightButton.Flat := Value;
    FDownLeftButton.Flat := Value;
  end;
end;


procedure TRzSpinButtons.SetOrientation( Value: TOrientation );
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if not ( csLoading in ComponentState ) then
    begin
      if FOrientation = orVertical then
        Width := Width div 2
      else
        Width := 2 * Width;
    end;
    Invalidate;
  end;
end;


procedure TRzSpinButtons.WMSize( var Msg: TWMSize );
var
  W, H: Integer;
begin
  inherited;

  { Check for minimum size }
  W := Width;
  H := Height;
  AdjustDimensions( W, H );
  if ( W <> Width ) or ( H <> Height ) then
    inherited SetBounds( Left, Top, W, H );

  Msg.Result := 0;
end;


procedure TRzSpinButtons.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantArrows;
end;


{=========================}
{== TRzSpinEdit Methods ==}
{=========================}

constructor TRzSpinEdit.Create( AOwner: TComponent );
begin
  inherited;

  FButtons := TRzSpinButtons.Create( Self );
  FButtons.Parent := Self;
  FButtons.Width := 17;
  FButtons.Height := 17;
  FButtons.Visible := True;
  FButtons.FocusControl := Self;
  FButtons.OnUpRightClick := UpRightClickHandler;
  FButtons.OnDownLeftClick := DownLeftClickHandler;

  ControlStyle := ControlStyle - [ csSetCaption ];

  FFlatButtonColor := clBtnFace;

  FButtonWidth := 17;
  Width := 47;
  FIntegersOnly := True;
  FAllowBlank := False;
  FBlankValue := 0;
  FAllowKeyEdit := False;
  FIncrement := DefaultIncrement;
  FPageSize := DefaultPageSize;
  FDecimals := 0;
  FCheckRange := False;
  FMin := DefaultMin;
  FMax := DefaultMax;

  Alignment := taRightJustify;
  SetValue( 0 );
  {&RCI}
end;


procedure TRzSpinEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or ws_ClipChildren;
end;


procedure TRzSpinEdit.CreateWnd;
begin
  inherited;
  SetEditRect; //!!remove
  {&RV}
end;


procedure TRzSpinEdit.Loaded;
begin
  inherited;
  ResizeButtons;
end;


procedure TRzSpinEdit.GetChildren( Proc: TGetChildProc; Root: TComponent );
begin
end;


procedure TRzSpinEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FlatButtonParentColor was published in version 2.x
  Filer.DefineProperty( 'FlatButtonParentColor', TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzSpinEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  inherited;

  if FlatButtons then
  begin
    if ActiveStyleServicesEnabled then
    begin
      if InFocus or Focused then
        FButtons.Flat := False
      else
        FButtons.Flat := True;
      FButtons.Color := Color;
    end
    else // No Themes
    begin
      if InFocus or Focused then
        FButtons.Color := FFlatButtonColor
      else
        FButtons.Color := Color;
    end;
  end;
end;


function TRzSpinEdit.CanChange( NewValue: Extended ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, NewValue, Result );
end;


procedure TRzSpinEdit.DoButtonClick( S: TSpinButtonType );
begin
  if Assigned( FOnButtonClick ) then
    FOnButtonClick( Self, S );
end;


procedure TRzSpinEdit.IncValue( const Amount: Extended );
var
  TempValue: Extended;
begin
  if ReadOnly then
    InvalidKeyPressed
  else
  begin
    TempValue := Value + Amount;
    if CanChange( TempValue ) then
    begin
      Value := TempValue;
      DoButtonClick( sbUp );
    end;
  end;
end;


procedure TRzSpinEdit.DecValue( const Amount: Extended );
var
  TempValue: Extended;
begin
  if ReadOnly then
    InvalidKeyPressed
  else
  begin
    TempValue := Value - Amount;
    if CanChange( TempValue ) then
    begin
      Value := TempValue;
      DoButtonClick( sbDown );
    end;
  end;
end;


function TRzSpinEdit.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  inherited DoMouseWheelDown( Shift, MousePos );
  if ssCtrl in Shift then
    DecValue( FPageSize )
  else
    DecValue( FIncrement );
  Result := True;
end;


function TRzSpinEdit.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  inherited DoMouseWheelUp( Shift, MousePos );
  if ssCtrl in Shift then
    IncValue( FPageSize )
  else
    IncValue( FIncrement );
  Result := True;
end;


procedure TRzSpinEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;

  if not FAllowKeyEdit and ( Key = vk_Delete ) then
  begin
    Key := 0;
    InvalidKeyPressed;
  end;

  case Key of
    vk_Prior:
      IncValue( FPageSize );
    vk_Next:
      DecValue( FPageSize );
    vk_Up:
      IncValue( FIncrement );
    vk_Down:
      DecValue( FIncrement );
  end;

  if Key in [ vk_Prior, vk_Next, vk_Up, vk_Down ] then
    Key := 0;
end;


procedure TRzSpinEdit.KeyPress( var Key: Char );
begin
  inherited;

  if not IsValidChar( Key ) then
  begin
    Key := #0;
    InvalidKeyPressed;
  end;
  {&RV}
end;


function TRzSpinEdit.IsValidChar( Key: Char ): Boolean;
var
  ValidCharSet: TSysCharSet;
begin
  if FIntegersOnly then
    ValidCharSet := [ '+', '-', '0'..'9' ]
  else
    ValidCharSet := [ FormatSettings.DecimalSeparator, '+', '-', '0'..'9' ];

  Result := CharInSet( Key, ValidCharSet ) or ( ( Key < #32 ) and ( Key <> Chr( vk_Return ) ) );

  if Result then
  begin
    if Key = FormatSettings.DecimalSeparator then
    begin
      if SelLength = 0 then
        Result := Pos( FormatSettings.DecimalSeparator, Text ) = 0
      else
      begin
        Result := Pos( FormatSettings.DecimalSeparator, Text ) = 0;
        if not Result then
          Result := Pos( FormatSettings.DecimalSeparator, SelText ) <> 0;
      end;
    end
    else if ( Key = '+' ) or ( Key = '-' ) then
      Result := ( ( SelStart = 0 ) and
                  ( Pos( '+', Text ) = 0 ) and
                  ( Pos( '-', Text ) = 0 ) ) or
                ( SelLength = Length( Text ) );
  end;

  if not FAllowKeyEdit and Result and
     ( ( Key >= #32 ) or
       ( Key = Char( vk_Back ) ) or
       ( Key = Char( vk_Delete ) ) ) then
    Result := False;
end;


procedure TRzSpinEdit.UpRightClickHandler( Sender: TObject );
begin
  IncValue( FIncrement );
end;


procedure TRzSpinEdit.DownLeftClickHandler( Sender: TObject );
begin
  DecValue( FIncrement );
end;


procedure TRzSpinEdit.SetFrameStyle( Value: TFrameStyle );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzSpinEdit.SetButtonWidth( Value: Integer );
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    if FButtonWidth < 0 then
      FButtonWidth := 0;

    if Orientation = orVertical then
      FButtons.Width := FButtonWidth
    else
      FButtons.Width := 2 * FButtonWidth;
    ResizeButtons;
    Invalidate;
  end;
end;


procedure TRzSpinEdit.SetDecimals( Value: Byte );
begin
  if FDecimals <> Value then
  begin
    FDecimals := Value;
    SetValue( GetValue );
  end;
end;


function TRzSpinEdit.GetButton( Index: Integer ): TRzControlButton;
begin
  if Index = 1 then
    Result := FButtons.DownLeftButton
  else
    Result := FButtons.UpRightButton;
end;


function TRzSpinEdit.IsCustomUpGlyph: Boolean;
begin
  Result := FButtons.CustomUpRightGlyph;
end;

function TRzSpinEdit.GetButtonUpGlyph: TBitmap;
begin
    Result := FButtons.GlyphUpRight;
end;

procedure TRzSpinEdit.SetButtonUpGlyph( Value: TBitmap );
begin
  FButtons.GlyphUpRight := Value;
end;


function TRzSpinEdit.GetButtonUpNumGlyphs: TNumGlyphs;
begin
  Result := FButtons.NumGlyphsUpRight;
end;

procedure TRzSpinEdit.SetButtonUpNumGlyphs( Value: TNumGlyphs );
begin
  FButtons.NumGlyphsUpRight := Value;
end;


function TRzSpinEdit.IsCustomDownGlyph: Boolean;
begin
  Result := FButtons.CustomDownLeftGlyph;
end;

function TRzSpinEdit.GetButtonDownGlyph: TBitmap;
begin
  Result := FButtons.GlyphDownLeft;
end;

procedure TRzSpinEdit.SetButtonDownGlyph( Value: TBitmap );
begin
  FButtons.GlyphDownLeft := Value;
end;


function TRzSpinEdit.GetButtonDownNumGlyphs: TNumGlyphs;
begin
  Result := FButtons.NumGlyphsDownLeft;
end;

procedure TRzSpinEdit.SetButtonDownNumGlyphs( Value: TNumGlyphs );
begin
  FButtons.NumGlyphsDownLeft := Value;
end;


function TRzSpinEdit.GetDirection: TSpinDirection;
begin
  Result := FButtons.Direction;
end;

procedure TRzSpinEdit.SetDirection( Value: TSpinDirection );
begin
  FButtons.Direction := Value;
end;


procedure TRzSpinEdit.SetFlatButtons( Value: Boolean );
begin
  inherited;
  FButtons.Flat := Value;
  ResizeButtons;
end;


function TRzSpinEdit.GetOrientation: TOrientation;
begin
  Result := FButtons.Orientation;
end;

procedure TRzSpinEdit.SetOrientation( Value: TOrientation );
begin
  FButtons.Orientation := Value;
  ResizeButtons;
  Invalidate;
end;


procedure TRzSpinEdit.SetIntegersOnly( Value: Boolean );
begin
  if FIntegersOnly <> Value then
  begin
    FIntegersOnly := Value;
    if FIntegersOnly then
    begin
      Decimals := 0;
      SetValue( Round( GetValue ) );
    end;
  end;
end;


procedure TRzSpinEdit.SetCheckRange( Value: Boolean );
begin
  if FCheckRange <> Value then
  begin
    FCheckRange := Value;
    SetValue( GetValue );
  end;
end;


procedure TRzSpinEdit.SetMin( const Value: Extended );
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMax < FMin then
      FMax := FMin;
    SetValue( GetValue ); // Reapply range
    Invalidate;
  end;
end;


procedure TRzSpinEdit.SetMax( const Value: Extended );
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMin > FMax then
      FMin := FMax;
    SetValue( GetValue ); // Reapply range
    Invalidate;
  end;
end;


function TRzSpinEdit.GetIntValue: Int64;
begin
  Result := Round( GetValue );
end;


procedure TRzSpinEdit.SetIntValue( Value: Int64 );
begin
  SetValue( Value );
end;


function TRzSpinEdit.GetValue: Extended;
begin
  try
    if Text = '' then
    begin
      if FAllowBlank then
        Result := FBlankValue
      else
      begin
        Text := FloatToStr( FMin );
        Result := StrToFloat( Text );
      end;
    end
    else
    begin
      Result := StrToFloat( Text );
    end;
  except
    Result := FMin;
  end;
end;


function TRzSpinEdit.CheckValue( const Value: Extended ): Extended;
begin
  Result := Value;
  if ( FMax <> FMin ) or FCheckRange then
  begin
    if Value < FMin then
      Result := FMin
    else if Value > FMax then
      Result := FMax;
  end;
end;


procedure TRzSpinEdit.SetValue( const Value: Extended );
var
  RestoreSelStart: Boolean;
  OldSelStart, OldLength, NewLength: Integer;
begin
  if Parent = nil then // avoid EInvalidOperation when created dynamically
  begin
    // SelStart and SelLength only work when the parent of the control is valid
    RestoreSelStart := False;
    OldSelStart := 0;
    OldLength := 0;
  end
  else
  begin
    RestoreSelStart := SelLength = 0;
    OldSelStart := SelStart;
    OldLength := Length( Text );
  end;

  Text := FloatToStrF( CheckValue( Value ), ffFixed, 18, FDecimals );

  if RestoreSelStart then
  begin
    NewLength := Length( Text );
    if NewLength = OldLength then
      SelStart := OldSelStart
    else if NewLength > OldLength then
      SelStart := OldSelStart + 1   // handle change from e.g. 99 to 100
    else
      SelStart := OldSelStart - 1;  // handle change from e.g. 100 to 99
  end;
end; {= TRzSpinEdit.SetValue =}


function TRzSpinEdit.StoreIncrement: Boolean;
begin
  Result := FIncrement <> DefaultIncrement;
end;


function TRzSpinEdit.StorePageSize: Boolean;
begin
  Result := FPageSize <> DefaultPageSize;
end;


procedure TRzSpinEdit.WMPaste( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;

procedure TRzSpinEdit.WMCut( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;


procedure TRzSpinEdit.CMEnter( var Msg: TCMEnter );
begin
  // Moved inherited to beginning b/c any changes made in OnEnter event handler that recreate the window does not
  // cause the EditRect to be updated.
  inherited;
  SetEditRect; //!!remove
  if AutoSelect and not ( csLButtonDown in ControlState ) then
    SelectAll;
end;


procedure TRzSpinEdit.CMExit( var Msg: TCMExit );
var
  N: Extended;
begin
  inherited;
  SetEditRect; //!!remove

  if FAllowBlank and ( Text = '' ) then
    Exit;

  try
    N := StrToFloat( Text );
  except
    N := FMin;
  end;
  SetValue( N );
end;


function TRzSpinEdit.GetEditRect: TRect;
begin
  Result := inherited GetEditRect;
  if not ReadOnlyValue then
    Dec( Result.Right, FButtons.Width + 2 );
end;


procedure TRzSpinEdit.SetEditRect;
begin
  if ReadOnlyValue then
  begin
    SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
    SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
    Exit;
  end;

  if not ( csLoading in ComponentState ) then
  begin
    if not UseRightToLeftAlignment then
    begin
      SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
      SendMessage( Handle, em_SetMargins, ec_RightMargin, MakeLParam( 0, FButtons.Width + 2 ) );
    end
    else
    begin
      SendMessage( Handle, em_SetMargins, ec_LeftMargin, MakeLParam( FButtons.Width + 2, 0 ) );
      SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
    end;
  end;
end;


procedure TRzSpinEdit.ResizeButtons;
var
  W, MinHeight: Integer;
begin
  if not ( csLoading in ComponentState ) then
  begin
    MinHeight := GetMinFontHeight( Font );
    if Height < MinHeight then
      Height := MinHeight
    else if FButtons <> nil then
    begin
      W := FButtons.Width;

      if not UseRightToLeftAlignment then
      begin
        if not FrameVisible then
        begin
          if Ctl3D then
          begin
            if ActiveStyleServicesEnabled then
            begin
              if UsingSystemStyle then
              begin
                if RunningAtLeast( winVista ) then
                begin
                  if FButtons.Orientation = orVertical then
                    FButtons.SetBounds( Width - W - 3, 0, W, Height - 3 )
                  else
                    FButtons.SetBounds( Width - W - 3, -1, W, Height - 3 );
                end
                else
                  FButtons.SetBounds( Width - W - 3, -1, W, Height - 2 );
              end
              else // VCL Style
              begin
                FButtons.SetBounds( Width - W - 3, -1, W, Height - 2 );
              end;
            end
            else
              FButtons.SetBounds( Width - W - 4, 0, W, Height - 4 );
          end
          else
            FButtons.SetBounds( Width - W - 1, 1, W, Height - 2 );
        end
        else
        begin
          if ActiveStyleServicesEnabled then
          begin
            if RunningAtLeast( winVista ) then
            begin
              if FButtons.Orientation = orVertical then
                FButtons.SetBounds( Width - W - 3, 0, W, Height - 3 )
              else
                FButtons.SetBounds( Width - W - 3, -1, W, Height - 3 );
            end
            else
              FButtons.SetBounds( Width - W - 3, -1, W, Height - 2 );
          end
          else
            FButtons.SetBounds( Width - W - 4, 0, W, Height - 4 );
        end;
      end
      else
      begin
        FButtons.SetBounds( 0, 0, W, Height - 4 );
      end;

      SetEditRect;
    end;
  end;
end; {= TRzSpinEdit.ResizeButtons =}


procedure TRzSpinEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzSpinEdit.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect; //!!remove
  FButtons.Enabled := Enabled;
  if FlatButtons then
    FButtons.Color := Color;
end;


procedure TRzSpinEdit.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  if HandleAllocated then
    SetEditRect; //!!remove
  FButtons.Enabled := Enabled;
end;


procedure TRzSpinEdit.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if FButtons <> nil then
    FButtons.Color := Color;
end;


procedure TRzSpinEdit.ReadOnlyChanged;
begin
  inherited;
  if FButtons <> nil then
    FButtons.Visible := not ReadOnlyValue;
  ResizeButtons;
end;


{========================}
{== TRzSpinner Methods ==}
{========================}

constructor TRzSpinner.Create( AOwner: TComponent );
begin
  inherited;

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FButtonColor := clBtnFace;
  FButtonWidth := 18;
  FFrameColor := clBtnShadow;
  FShowFocusRect := True;

  FValue := 0;
  FIncrement := 1;
  FPageSize := 10;
  FMin := 0;
  FMax := 100;
  FCheckRange := False;
  FTabOnEnter := False;

  FShowHexValue := False;
  FHexValuePrefix := '$';

  FMinusBtnDown := False;
  FPlusBtnDown := False;

  FThemeAware := True;
  FOverPlusButton := False;
  FOverMinusButton := False;

  FInitialDelay := 400;   // 400 milliseconds
  FDelay := 100;          // 100 milliseconds

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;
  FImageIndexes[ 1 ] := -1;
  FImageIndexes[ 2 ] := -1;

  // Initializing inherited properties
  Width := 85;
  Height := 21;
  TabStop := True;
  ParentColor := False;

  FGlyphBitmap := TBitmap.Create;
end;


procedure TRzSpinner.CreateWnd;
begin
  inherited;

  if RunningAtLeast( win2000 ) then
    Perform( wm_ChangeUIState, MakeWParam( UIS_INITIALIZE, UISF_HIDEACCEL or UISF_HIDEFOCUS ), 0 );
end;


destructor TRzSpinner.Destroy;
begin
  FImagesChangeLink.Free;
  FGlyphBitmap.Free;
  inherited;
end;


procedure TRzSpinner.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Handle the fact that the NumGlyphsMinus, NumGlyphsPlus, and Flat properties
  // were  published in version 2.x
  Filer.DefineProperty( 'NumGlyphsMinus', TRzOldPropReader.ReadOldIntegerProp, nil, False );
  Filer.DefineProperty( 'NumGlyphsPlus', TRzOldPropReader.ReadOldIntegerProp, nil, False );
  Filer.DefineProperty( 'Flat', TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzSpinner.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      SetImages( nil )
    else if AComponent = FFrameController then
      FFrameController := nil;
  end;
end;


procedure TRzSpinner.Paint;
var
  R: TRect;
begin
  inherited;

  DrawFrame;
  DrawValue;

  if UsingSystemStyle then
  begin
    if RunningAtLeast( winVista ) and FThemeAware and ActiveStyleServicesEnabled then
    begin
      if not UseCustomImages( saMinusButton ) then
        DrawButton( saMinusButton, FMinusBtnDown, Rect( -1, -1, FButtonWidth, Height ) )
      else
        DrawButton( saMinusButton, FMinusBtnDown, Rect( 1, 1, FButtonWidth - 1, Height - 1 ) );

      if not UseCustomImages( saPlusButton ) then
        DrawButton( saPlusButton, FPlusBtnDown, Rect( Width - FButtonWidth, -1, Width + 1, Height ) )
      else
        DrawButton( saPlusButton, FPlusBtnDown, Rect( Width - FButtonWidth + 1, 1, Width - 1, Height - 1 ) );
    end
    else
    begin
      DrawButton( saMinusButton, FMinusBtnDown, Rect( 1, 1, FButtonWidth - 1, Height - 1 ) );
      DrawButton( saPlusButton, FPlusBtnDown, Rect( Width - FButtonWidth + 1, 1, Width - 1, Height - 1 ) );
    end;
  end
  else // VCL Styles
  begin
    DrawButton( saMinusButton, FMinusBtnDown, Rect( 2, 2, FButtonWidth - 2, Height - 2 ) );
    DrawButton( saPlusButton, FPlusBtnDown, Rect( Width - FButtonWidth + 1, 2, Width - 2, Height - 2 ) );
  end;

  if ShowFocus and Focused and FShowFocusRect then
  begin
    Canvas.Brush.Color := Self.Color;
    R := Rect( FButtonWidth + 1, 2, Width - FButtonWidth -1, Height - 2 );
    Canvas.DrawFocusRect( R );
  end;
end; {= TRzSpinner.Paint =}


procedure TRzSpinner.DrawFrame;
var
  R: TRect;
  //C: DWord;
  C: TColor;
  ElementDetails: TThemedElementDetails;
begin
  R := ClientRect;

  if FThemeAware and ActiveStyleServicesEnabled then
  begin
    Canvas.Brush.Color := Self.Color;
    // Eventually add DisabledColor property like TRzEdit

    if Enabled then
      ElementDetails := ActiveStyleServices.GetElementDetails( teEditTextNormal )
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( teEditTextDisabled );

    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );

    // Get Color for border to edit theme element
//    GetThemeColor( ActiveStyleServices.Theme[ teEdit ], ElementDetails.Part,
//                   ElementDetails.State, TMT_BORDERCOLOR, C );

    GetThemeColor( ActiveStyleServices.Theme[ teEdit ], ElementDetails.Part,
                   ElementDetails.State, TMT_BORDERCOLOR, TColorRef( C ) );



    Canvas.Pen.Color := C;
  end
  else
  begin
    if Enabled then
    begin
      Canvas.Brush.Color := Self.Color;
      Canvas.Pen.Color := FFrameColor;
    end
    else
    begin
      Canvas.Brush.Color := clBtnFace;
      Canvas.Pen.Color := clBtnShadow;
    end;
    Canvas.Rectangle( R );
  end;

  // Do not draw divider lines if using a Custom VCL Style
  if UsingSystemStyle then
  begin
    // Draw vertical lines between value and buttons in current pen color
    Canvas.MoveTo( FButtonWidth - 1, 1 );
    Canvas.LineTo( FButtonWidth - 1, Height - 1 );
    Canvas.MoveTo( Width - FButtonWidth, 1 );
    Canvas.LineTo( Width - FButtonWidth, Height - 1 );
  end;
end; {= RzSpinner.DrawFrame =}


procedure TRzSpinner.DrawValue;
var
  R: TRect;
  S: string;
  YOffset: Integer;
begin
  Canvas.Font := Self.Font;

  if not UsingSystemStyle then
    Canvas.Font.Color := ActiveStyleFontColor( sfEditBoxTextNormal );

  SetTextAlign( Canvas.Handle, ta_Center or ta_Top );
  R := Rect( FButtonWidth - 1, 0, Width - FButtonWidth + 1, Height );
  InflateRect( R, -1, -1 );
  S := IntToStr( FValue );
  YOffset := R.Top + ( R.Bottom - R.Top - Canvas.TextHeight( S ) ) div 2;

  if not Enabled then
    Canvas.Font.Color := clBtnShadow;

  Canvas.Brush.Style := bsClear;
  if FShowHexValue then
    Canvas.TextRect( R, Width div 2, YOffset, Format( '%s%x', [ FHexValuePrefix, FValue ] ) )
  else
    Canvas.TextRect( R, Width div 2, YOffset, S );
  Canvas.Brush.Style := bsSolid;
end;


procedure TRzSpinner.DrawButton( Area: TRzSpinnerArea; Down: Boolean; Bounds: TRect );
var
  R: TRect;
  L, T: Integer;
  Direction: TDirection;
  SpinElement: TThemedSpin;
  {$IFDEF VCL160_OR_HIGHER}
  BtnElement: TThemedScrollBar;
  {$ENDIF}
  ElementDetails: TThemedElementDetails;
begin
  if FThemeAware and ActiveStyleServicesEnabled and not UseCustomImages( Area ) then
  begin
    if UsingSystemStyle then
    begin
      if Area = saMinusButton then
      begin
        if Down and not ( csDesigning in ComponentState ) then
          SpinElement := tsDownHorzPressed
        else if not Enabled then
          SpinElement := tsDownHorzDisabled
        else if FOverMinusButton and not ( csDesigning in ComponentState ) then
          SpinElement := tsDownHorzHot
        else
          SpinElement := tsDownHorzNormal;

        ElementDetails := ActiveStyleServices.GetElementDetails( SpinElement );
        ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, Bounds );

        if not RunningAtLeast( winVista ) then
        begin
          // Erase right side of the theme element
          Canvas.Pen.Color := clWindow;
          Canvas.MoveTo( Bounds.Right - 1, Bounds.Top );
          Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom );
        end;
      end
      else // Area = saPlusButton
      begin
        if Down and not ( csDesigning in ComponentState ) then
          SpinElement := tsUpHorzPressed
        else if not Enabled then
          SpinElement := tsUpHorzDisabled
        else if FOverPlusButton and not ( csDesigning in ComponentState ) then
          SpinElement := tsUpHorzHot
        else
          SpinElement := tsUpHorzNormal;

        ElementDetails := ActiveStyleServices.GetElementDetails( SpinElement );
        ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, Bounds );

        if not RunningAtLeast( winVista ) then
        begin
          // Erase left side of the theme element
          Canvas.Pen.Color := clWindow;
          Canvas.MoveTo( Bounds.Left, Bounds.Top );
          Canvas.LineTo( Bounds.Left, Bounds.Bottom );
        end;
      end;
    end
    else // VCL Styles
    begin
      {$IFDEF VCL160_OR_HIGHER}
      if Area = saMinusButton then
      begin
        if Down and not ( csDesigning in ComponentState ) then
          BtnElement := tsArrowBtnLeftPressed
        else if not Enabled then
          BtnElement := tsArrowBtnLeftDisabled
        else if FOverMinusButton and not ( csDesigning in ComponentState ) then
          BtnElement := tsArrowBtnLeftHot
        else
          BtnElement := tsArrowBtnLeftNormal;

        ElementDetails := ActiveStyleServices.GetElementDetails( BtnElement );
        ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, Bounds );
      end
      else // Area = saPlusButton
      begin
        if Down and not ( csDesigning in ComponentState ) then
          BtnElement := tsArrowBtnRightPressed
        else if not Enabled then
          BtnElement := tsArrowBtnRightDisabled
        else if FOverMinusButton and not ( csDesigning in ComponentState ) then
          BtnElement := tsArrowBtnRightHot
        else
          BtnElement := tsArrowBtnRightNormal;

        ElementDetails := ActiveStyleServices.GetElementDetails( BtnElement );
        ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, Bounds );
      end;
      {$ENDIF}
    end;
  end
  else // No XP Themes or using custom images
  begin
    R := DrawBox( Canvas, Bounds, clWindow );

    if Area = saMinusButton then
    begin
      if Down then
        Canvas.Brush.Color := DarkerColor( FButtonColor, 20 )
      else if Enabled then
      begin
        if FOverMinusButton then
        begin
          Canvas.Brush.Color := LighterColor( FButtonColor, 10 );
          R := DrawBox( Canvas, R, DarkerColor( FButtonColor, 20 ) );
        end
        else
          Canvas.Brush.Color := FButtonColor;
      end
      else
        Canvas.Brush.Color := clBtnFace;

      Canvas.FillRect( R );

      Direction := dirLeft;

      if ( Images <> nil ) and ( ImageIndexMinus <> -1 ) then
      begin
        CalcCenterOffsets( R, L, T );
        FImages.Draw( Canvas, L, T, ImageIndexMinus, Enabled );
      end
      else
      begin
        DrawSpinArrow( Canvas, Bounds, uiWindows95, Direction, Down, Enabled );
      end;
    end
    else // Area = saPlusButton
    begin
      if Down then
        Canvas.Brush.Color := DarkerColor( FButtonColor, 20 )
      else if Enabled then
      begin
        if FOverPlusButton then
        begin
          Canvas.Brush.Color := LighterColor( FButtonColor, 10 );
          R := DrawBox( Canvas, R, DarkerColor( FButtonColor, 20 ) );
        end
        else
          Canvas.Brush.Color := FButtonColor;
      end
      else
        Canvas.Brush.Color := clBtnFace;

      Canvas.FillRect( R );

      Direction := dirRight;

      if ( Images <> nil ) and ( ImageIndexPlus <> -1 ) then
      begin
        CalcCenterOffsets( R, L, T );
        FImages.Draw( Canvas, L, T, ImageIndexPlus, Enabled );
      end
      else
      begin
        DrawSpinArrow( Canvas, Bounds, uiWindows95, Direction, Down, Enabled );
      end;

    end;
  end;

  Canvas.Pen.Color := clWindowText;
  Canvas.Brush.Color := clWindow;
end; {= TRzSpinner.DrawButton =}


function TRzSpinner.UseCustomImages( Area: TRzSpinnerArea ): Boolean;
begin
  Result := False;
  case Area of
    saMinusButton:
      Result := ( Images <> nil ) and ( ImageIndexMinus <> -1 );

    saPlusButton:
      Result := ( Images <> nil ) and ( ImageIndexPlus <> -1 );
  end;
end;


procedure TRzSpinner.CalcCenterOffsets( Bounds: TRect; var L, T: Integer );
begin
  if FImages <> nil then
  begin
    L := Bounds.Left + ( Bounds.Right - Bounds.Left ) div 2 -  ( FImages.Width div 2 );
    T := Bounds.Top + ( Bounds.Bottom - Bounds.Top ) div 2 - ( FImages.Height div 2 );
  end;
end;


procedure TRzSpinner.DoEnter;
begin
  inherited;
  // When control gets focus, update display to show focus border
  Repaint;
end;


procedure TRzSpinner.DoExit;
begin
  inherited;
  // When control loses focus, update display to remove focus border
  Repaint;
end;


function TRzSpinner.CanChange( NewValue: Integer ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, NewValue, Result );
end;


procedure TRzSpinner.Change;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TRzSpinner.DecValue( Amount: Integer );
var
  OldValue: Integer;
begin
  OldValue := Value;
  SetValue( Value - Amount );
  if ( OldValue <> Value ) and Assigned( FOnDecrement ) then
    FOnDecrement( Self, Amount );
end;


procedure TRzSpinner.IncValue( Amount: Integer );
var
  OldValue: Integer;
begin
  OldValue := Value;
  SetValue( Value + Amount );
  if ( OldValue <> Value ) and Assigned( FOnIncrement ) then
    FOnIncrement( Self, Amount );
end;


procedure TRzSpinner.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  inherited;
  Msg.Result := dlgc_WantArrows;
end;


procedure TRzSpinner.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzSpinner.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;

  case Key of
    vk_Left, vk_Down, vk_Subtract:
      DecValue( FIncrement );

    vk_Up, vk_Right, vk_Add:
      IncValue( FIncrement );

    vk_Prior:
      IncValue( FPageSize );

    vk_Next:
      DecValue( FPageSize );
  end;
end;


function TRzSpinner.ShowFocus: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEFOCUS ) = 0;
end;


function TRzSpinner.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


function TRzSpinner.HitTest( P: TPoint ): TRzSpinnerArea;
begin
  Result := HitTest( P.X, P.Y );
end;


function TRzSpinner.HitTest( X, Y: Integer ): TRzSpinnerArea;
var
  MinusRect, PlusRect: TRect;
  P: TPoint;
begin
  P := Point( X, Y );
  MinusRect := Rect( 0, 0, FButtonWidth, Height );
  PlusRect := Rect( Width - FButtonWidth, 0, Width, Height );

  if PtInRect( MinusRect, P ) then
    Result := saMinusButton
  else if PtInRect( PlusRect, P ) then
    Result := saPlusButton
  else
    Result := saValue;
end;


procedure TRzSpinner.CheckHotTracking( P: TPoint );
var
  Area: TRzSpinnerArea;
begin
  Area := HitTest( P );
  FOverPlusButton := Area = saPlusButton;
  FOverMinusButton := Area = saMinusButton;

  if Area <> FLastHotTrackArea then
  begin
    // This will avoid flickering the display on mouse move
    FLastHotTrackArea := Area;
    Repaint;
  end;
end;


procedure TRzSpinner.CMMouseEnter( var Msg: TMessage );
begin
  inherited;
  CheckHotTracking( CursorPosition );
end;


procedure TRzSpinner.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
  FOverPlusButton := False;
  FOverMinusButton := False;

  if FLastHotTrackArea <> saValue then
    Repaint;
  FLastHotTrackArea := saValue;
end;


procedure TRzSpinner.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  Area: TRzSpinnerArea;
begin
  inherited;

  if not ( csDesigning in ComponentState ) then
    SetFocus;               // Move focus to Spinner only at runtime

  Area := HitTest( X, Y );
  if ( Button = mbLeft ) and
     ( ( Area = saMinusButton ) or ( Area = saPlusButton ) ) then
  begin
    FMinusBtnDown := Area = saMinusButton;
    FPlusBtnDown := Area = saPlusButton;

    if FRepeatTimer = nil then
    begin
      FRepeatTimer := TTimer.Create( Self );
      FRepeatTimer.OnTimer := TimerExpired;
    end;
    FRepeatTimer.Interval := FInitialDelay;
    FRepeatTimer.Enabled := True;

    Repaint;
  end;
end;


procedure TRzSpinner.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  CheckHotTracking( Point( X, Y ) );
end;


procedure TRzSpinner.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  Area: TRzSpinnerArea;
begin
  inherited;

  Area := HitTest( X, Y );
  if Button = mbLeft then
  begin
    if Area = saPlusButton then
      IncValue( FIncrement )
    else if Area = saMinusButton then
      DecValue( FIncrement );

    if FRepeatTimer <> nil then
      FRepeatTimer.Enabled := False;

    FMinusBtnDown := False;
    FPlusBtnDown := False;
    Repaint;
  end;
end;


procedure TRzSpinner.TimerExpired( Sender: TObject );
var
  Area: TRzSpinnerArea;
begin
  FRepeatTimer.Interval := FDelay;

  try
    Area := HitTest( CursorPosition );
    if Area = saPlusButton then
      IncValue( FIncrement )
    else if Area = saMinusButton then
      DecValue( FIncrement );
  except
    FRepeatTimer.Enabled := False;
    raise;
  end;
end;


function TRzSpinner.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  inherited DoMouseWheelDown( Shift, MousePos );
  if ssCtrl in Shift then
    DecValue( FPageSize )
  else
    DecValue( FIncrement );
  Result := True;
end;


function TRzSpinner.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  inherited DoMouseWheelUp( Shift, MousePos );
  if ssCtrl in Shift then
    IncValue( FPageSize )
  else
    IncValue( FIncrement );
  Result := True;
end;


procedure TRzSpinner.SetButtonColor( Value: TColor );
begin
  if FButtonColor <> Value then
  begin
    FButtonColor := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.SetButtonWidth( Value: Integer );
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.SetFrameController( Value: TRzFrameController );
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FFrameController := Value;
  if Value <> nil then
  begin
    Value.AddControl( Self );
    Value.FreeNotification( Self );
  end;
end;


function TRzSpinner.GetImageIndex( PropIndex: Integer ): TImageIndex;
begin
  Result := FImageIndexes[ PropIndex ];
end;


procedure TRzSpinner.SetImageIndex( PropIndex: Integer; Value: TImageIndex );
begin
  if FImageIndexes[ PropIndex ] <> Value then
  begin
    FImageIndexes[ PropIndex ] := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.SetImages( Value: TCustomImageList );
begin
  if FImages <> nil then
    FImages.UnRegisterChanges( FImagesChangeLink );

  FImages := Value;

  if FImages <> nil then
  begin
    FImages.RegisterChanges( FImagesChangeLink );
    FImages.FreeNotification( Self );
    CheckMinSize;
  end;
  Invalidate;
end;


procedure TRzSpinner.ImagesChange( Sender: TObject );
begin
  if Sender = Images then
  begin
    CheckMinSize;
    Update; // Call Update instead of Invalidate to prevent flicker
  end;
end;


procedure TRzSpinner.CheckMinSize;
begin
  // Ensures button area will display entire image
  if FImages.Width > ButtonWidth then
    ButtonWidth := FImages.Width + 4;
  if FImages.Height > Height then
    Height := FImages.Height + 4;
end;


procedure TRzSpinner.SetCheckRange( Value: Boolean );
begin
  if FCheckRange <> Value then
  begin
    FCheckRange := Value;
    SetValue( FValue );
  end;
end;


procedure TRzSpinner.SetMax( Value: Integer );
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMin > FMax then
      FMin := FMax;
    SetValue( FValue ); // Reapply range
    Invalidate;
  end;
end;


procedure TRzSpinner.SetMin( Value: Integer );
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMax < FMin then
      FMax := FMin;
    SetValue( FValue );
    Invalidate;
  end;
end;


function TRzSpinner.CheckValue( Value: Integer ): Integer;
begin
  Result := Value;
  if ( FMax <> FMin ) or FCheckRange then
  begin
    if Value < FMin then
      Result := FMin
    else if Value > FMax then
      Result := FMax;
  end;
end;


procedure TRzSpinner.SetValue( Value: Integer );
var
  TempValue: Integer;
begin
  TempValue := CheckValue( Value );
  if FValue <> TempValue then
  begin
    if CanChange( TempValue ) then
    begin
      FValue := TempValue;
      Invalidate;

      Change;                                           { Trigger Change event }

      UpdateObjectInspector( Self );
    end;
  end;
end;


function TRzSpinner.StoreHexValuePrefix: Boolean;
begin
  Result := FHexValuePrefix <> '$';
end;


procedure TRzSpinner.SetHexValuePrefix( const Value: string );
begin
  if FHexValuePrefix <> Value then
  begin
    FHexValuePrefix := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.SetShowHexValue( Value: Boolean );
begin
  if FShowHexValue <> Value then
  begin
    FShowHexValue := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    Invalidate;
  end;
end;


procedure TRzSpinner.CMDesignHitTest( var Msg: TCMDesignHitTest );
var
  ShiftState: TShiftState;
  Area: TRzSpinnerArea;
begin
  Area := HitTest( CursorPosition );
  // Handling this component message allows the Value of the
  // spinner to be changed at design-time using the left mouse
  // button.  If the mouse is positioned over one of the buttons,
  // then set the Msg.Result value to 1. This instructs Delphi to
  // allow mouse events to "get through to" the component.

  // Do not simply test for ssShift or ssCtrl or ssAlt, must be combination
  // otherwise it will interfere with Form Designer

  ShiftState := KeysToShiftState( Msg.Keys );
  if ( ssShift in ShiftState ) and ( ssCtrl in ShiftState ) then
    Msg.Result := 0
  else if ( Area = saMinusButton ) or ( Area = saPlusButton ) then
    Msg.Result := 1
  else
    Msg.Result := 0;
end;


procedure TRzSpinner.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;

  // Repaint the component so that it reflects the state change
  Repaint;
end;


procedure TRzSpinner.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameColor in FFrameControllerNotifications then
      FFrameColor := FFrameController.FrameColor;
    if fcpFlatButtonColor in FFrameControllerNotifications then
      FButtonColor := FFrameController.FlatButtonColor;
    if fcpColor in FFrameControllerNotifications then
      Color := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;

    Invalidate;
  end;
end;


{&RUIF}
end.





