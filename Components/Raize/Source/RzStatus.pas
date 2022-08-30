{===============================================================================
  RzStatus Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzStatusPane
    Basic status pane.

  TRzFieldStatus
    Enhanced TRzStatusPane--adds ability to display a field label next to
    status caption.

  TRzGlyphStatus
    Enhanced TRzStatusPane--adds ability to display a glyph.

  TRzClockStatus
    Enhanced TRzStatusPane--displays current date/time.

  TRzKeyStatus
    Enhanced TRzStatusPane--displays toggled state of CapsLock, NumLock, or
    ScrollLock keys.

  TRzResourceStatus
    Enhanced TRzStatusPane--shows System resource usage under Win9X, shows
    Memory usage under WinNT.

  TRzMarqueeStatus
    Enhanced TRzStatusPane--display Caption is scrolls across status pane.

  TRzVersionInfo
    Nonvisual component that provides access to the Version Info block of an
    application.

  TRzVersionInfoStatus
    Enhanced TRzFieldStatus--connects to a TRzVersionInfo component to display
    version information.


  Modification History
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Fixed issue in TRzStatusPane and descendents where changing the FlatColor
      property would have no effect on the frame if the FrameStyle was set to
      fsFlatBold.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzStatusPane, TRzGlyphStatus,
      TRzClockStatus, TRzKeyStatus, TRzResourceStatus, TRzMarqueeStatus,
      TRzProgressStatus, TRzFieldStatus, and TRzVersionInfoStatus to fully
      support VCL Styles introduced in RAD Studio XE2.
    * Fixed issue where TRzResourceStatus would always display 0 KB Free
      Memory when DisplayStyle was set to dsText.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Several new properties have been added to the TRzVersionInfo component.
      Specifically, the individual elements that make up the module's version
      number are accessible with MajorVersion, MinorVersion, Release, and Build
      properties. Each value is a Word value.  The module attributes are also
      accessible in the new DebugBuild, PreRelease, PrivateBuild, and
      SpecialBuild Boolean proeprties.  And finally, the new FileDateTime
      property can be used to quickly access the module's file date/time stamp.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Added OnScrollDisplay event to TRzMarqueeDisplay. This event is fired each
      time the caption is scrolled across the display. The corresponding event
      handler is passed the current step in the process along with the total
      steps required to scroll the caption completely across the display.
    * Added OnScrollComplete event to TRzMarqueeStatus. This event is fired when
      Scrolling is True and the Caption has been completely scrolled across the
      display.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzStatusPane and descendant controls.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Added new ShowParts property to TRzProgressStatus. When this property is
      set to True and ShowPercent is set to True, the text representation of the
      percentage is displayed as a ratio of the values of PartsComplete and
      TotalParts properties.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed problem where TRzResourceStatus would report 100% memory utilization
      on systems with 4GB memory. The problem was fixed by switching to use the
      GlobalMemoryStatusEx function instead GlobalMemoryStatus. MSDN reports
      that GlobalMemoryStatus returns incorrect results.
    * Fixed problem where deleting the TImageList that was currently being
      referenced by the TRzGlyphStatus control would result in an exception
      in the Form Designer.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Fixed problem where TRzKeyStatus.InsertState was not updating correctly
      as the state of the Insert key changed.
    * Updated display of disabled text in TRzStatusPane and descendants.
    * The captions in TRzStatusPane and descendants are now drawn correctly when
      running under right-to-left (RTL) systems.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Added IncParts and IncPartsByOne methods to TRzProgressStatus.
    * Adjusted streaming code in TRzStatusPane and descendants to allow older
      projects that used the FillColor property to not use the new default style
      of being transparent.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem where extra divider line would appear in last status pane
      on a TRzStatusBar that had its VisualStyle set to vsGradient.
    * Fixed problem where TRzStatusPane and descendants would not pick up the
      correct settings when connected to a TRzFrameController in Delphi 5.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new bsGradient value to TRzResourceStatus.BarStyle enumeration. When
      selected, the progress bar and background are drawn using gradients. The
      colors of the gradients are determined by the BarColor, BarColorStop,
      BackColor, and BackColorStop properties.  The style of gradient is
      controlled by the new GradientDirection property.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzCustomStatusPane to
      account for changes introduced in Borland Developer Studio 2006.
    * All status panes (i.e. TRzCustomStatusPane and descendants) now support
      the Transparent property, which is set to True by default. This change
      addresses a number of short-comings in the previous approach. Transparency
      was also necessary to support the new vsGradient ViewStyle property value
      in the TRzStatusBar.
    * When a status pane is placed on a TRzStatusBar, the appearance of the
      status pane is determined by the TRzStatusBar. As a result, it is no
      longer necessary to customize each control in order to change the style
      of the status bar.
    * Introduced the TRzProgressStatus control. This component has the
      functionality of a progress bar, but is designed to be placed in a
      TRzStatusBar.  In previous versions, a TRzProgressBar could be used in
      a TRzStatusBar, and it's border properties modified so that it would look
      appropriately in the status bar, but with the new TRzProgressStatus, this
      is no longer necessary. In addition, the TRzProgressStatus supports
      transparency just like the other status panes, and therefore provides a
      much better user interface for progress bar status.
    * At design-time, the TRzVersionInfoStatus component now displays the
      version info field that will be displayed when the app is executed.
    * Added new FrameControllerNotifications property to TRzStatusPane and all
      status pane descendants.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
    * Fixed problem in TRzGlyphStatus that prevented a Glyph that was designed
      to be non-transparent from displaying correctly.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added new KeyOffAppearance property to TRzKeyStatus, which controls the
      appearance of the "Key Off" state.  The default is koaBlank. The other
      option is koaDimmed.
    * Fixed problem where InsertState property of TRzKeyState was not being
      recognized when the control was created dynamically.
    * Modifiying the InsertState after the TRzKeyState control has been created
      now correctly resets all internal state variables to match the newly
      selected initial state value.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Added new AutoHint property to TRzStatusPane, TRzGlyphStatus, and
      TRzFieldStatus components. When set to True, the status pane will display
      the hint of the control underneath the current position of the mouse.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed AutoSize problem in TRzCustomFieldStatus.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added GetCustomKeyValue public method to TRzVersionInfo.
    * Fixed problem where TRzClockStatus would not auto size if Format was
      changed.
    * Surfaced Color property in TRzCustomStatusPane and all descendants. Also
      added the ParentFillColor property, which when True causes the FillColor
      to match the ParentColor and Color values.
    * Frame of status pane now drawn correctly when FrameStyle set to
      fsFlatBold.
    * Created new TRzFieldStatus component, which allows a field label to be
      displayed next to status information.  This component now serves as the
      ancestor for the TRzVersionInfoStatus and TRzDBStatusPane components.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Added OnGetDateTime event to TRzClockStatus.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem where AutoSize wasn't calculating correct size when Caption
      contained an "&"
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomStatusPane and TRzStatusPane >>
    * A status pane will now properly resize itself if the font style of the
      status pane is changed. For example, if the font style changes to fsBold,
      the size of the status pane will automatically be updated to accommodate
      the entire text if the AutoSize property has been set to True.
    * Added FlatColorAdjustment property.

    << TRzGlyphStatus >>
    * Added Images, ImageIndex, DisabledIndex properties to TRzGlyphStatus.
      Therefore, the status component can display its glyph from either a bitmap
      file or from an image list.

    << TRzMarqueeStatus >>
    * Enhanced the animation effects so that scrolling of text is much smoother.

    << TRzKeyStatus >>
    * When CapsLock, ScrLock, or NumLock keys are off, no caption appears in the
      status pane.  Also fixed problem where key text would appear even if
      Visible property was set to False.
    * Added the tkInsert key type and the InsertState property. When Key is set
      to tkInsert the status pane will show the Insert or Overwrite state
      depending on the initial value of InsertState when the program starts and
      the number of times the Insert key is toggled.  The InsertState property
      can be used to determine the current insert state (isInsert or
      isOverwrite) as well as change the current intrpretation of the Insert key
      with respect to the status pane.

    << TRzClockStatus >>
    * Fixed problem where clock text still appeared even though the Visible
      property of the component was set to False.

    << TRzResourceStatus >>
    * Removed the ResourceType property.  Under Windows 95/98/ME this control
      shows the percentage of Free System resources.  Under Windows NT/2000/XP,
      this control shows the percentage of Free Memory.

    * Added TRzVersionInfo component.
    * Added TRzVersionInfoStatus component.
===============================================================================}

{$I RzComps.inc}

unit RzStatus;

interface

uses
  {&RF}
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Controls,
  Graphics,
  ExtCtrls,
  Menus,
  RzCommon,
  Buttons,
  SysUtils,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Classes,
  RzPrgres;

type
  {===========================================}
  {== TRzCustomStatusPane Class Declaration ==}
  {===========================================}

  TRzCustomStatusPane = class( TGraphicControl, IRzCustomFramingNotification )
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FAutoHint: Boolean;
    FDefaultWidth: Integer;
    FBlinkColor: TColor;
    FBlinkState: TBlinkState;
    FBlinking: Boolean;
    FFontColor: TColor;
    FBorderWidth: TBorderWidth;
    FCaption: TCaption;
    FCaptionOffset: Integer;
    FParentFillColor: Boolean;
    FFillColor: TColor;
    FFlatColor: TColor;
    FFlatColorAdjustment: Integer;
    FFrameStyle: TFrameStyle;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;
    FTransparent: Boolean;

    { Message Handling Methods }
    procedure CMGetBlinking( var Msg: TMessage ); message cm_GetBlinking;
    procedure CMBlink( var Msg: TMessage ); message cm_Blink;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMHitTest( var Msg: TMessage ); message cm_HitTest;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
  protected
    FAboutInfo: TRzAboutInfo;
    FCurrentColor: TColor;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure AdjustBounds; dynamic;
    function GetMinWidth: Integer; virtual;
    function GetCaptionRect: TRect; virtual;
    function GetFontColor: TColor;
    function GetDisabledFontColor: TColor;
    procedure DrawCaption( const CaptionStr: string ); virtual;
    procedure DrawStatusBorder; virtual;
    procedure Paint; override;

    procedure CustomFramingChanged; virtual;

    procedure Blink( State: TBlinkState ); virtual;

    { Property Access Methods }
    procedure SetAlignment( Value: TAlignment ); virtual;
    procedure SetAutoSize( Value: Boolean ); override;
    procedure SetDefaultWidth( Value: Integer ); virtual;
    procedure SetBlinking( Value: Boolean ); virtual;
    procedure SetBlinkColor( Value: TColor ); virtual;
    function GetBlinkIntervalOff: Word; virtual;
    procedure SetBlinkIntervalOff( Value: Word ); virtual;
    function GetBlinkIntervalOn: Word; virtual;
    procedure SetBlinkIntervalOn( Value: Word ); virtual;
    procedure SetBorderWidth( Value: TBorderWidth ); virtual;
    procedure SetCaption( Value: TCaption ); virtual;
    procedure SetCaptionOffset( Value: Integer ); virtual;
    function IsFillColorStored: Boolean;
    procedure SetFillColor( Value: TColor ); virtual;
    procedure SetParentFillColor( Value: Boolean ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;

    function UnderSizeGrip( R: TRect ): Boolean; virtual;

    { Property Declarations }
    property Alignment: TAlignment
      read FAlignment
      write SetAlignment
      default taLeftJustify;

    property AutoHint: Boolean
      read FAutoHint
      write FAutoHint
      default False;

    property AutoSize: Boolean
      read FAutoSize
      write SetAutoSize
      default False;

    property Blinking: Boolean
      read FBlinking
      write SetBlinking
      default False;

    property BlinkColor: TColor
      read FBlinkColor
      write SetBlinkColor
      default clHighlight;

    property BlinkState: TBlinkState
      read FBlinkState;

    property BlinkIntervalOff: Word
      read GetBlinkIntervalOff
      write SetBlinkIntervalOff
      default 500;

    property BlinkIntervalOn: Word
      read GetBlinkIntervalOn
      write SetBlinkIntervalOn
      default 500;

    property CaptionOffset: Integer
      read FCaptionOffset
      write SetCaptionOffset
      default 2;

    property Caption: TCaption
      read FCaption
      write SetCaption;

    property DefaultWidth: Integer
      read FDefaultWidth
      write SetDefaultWidth
      default 40;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function ExecuteAction( Action: TBasicAction ): Boolean; override;

    function UseThemes: Boolean;

    property Canvas;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property BorderWidth: TBorderWidth
      read FBorderWidth
      write SetBorderWidth
      default 1;

    property FillColor: TColor
      read FFillColor
      write SetFillColor
      stored IsFillColorStored
      default clBtnFace;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      default fsFlat;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property ParentFillColor: Boolean
      read FParentFillColor
      write SetParentFillColor
      default True;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default True;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height default 20;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;
    property Width default 100;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end; {== TRzCustomStatusPane Class Declaration ==}


  TRzStatusPane = class( TRzCustomStatusPane )
  published
    property Alignment;
    property AutoHint;
    property AutoSize;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property Caption;
    property CaptionOffset;
    property DefaultWidth;
  end;


  {======================================}
  {== TRzGlyphStatus Class Declaration ==}
  {======================================}

  TGlyphAlignment = ( gaLeft, gaRight );

  TRzCustomGlyphStatus = class( TRzCustomStatusPane )
  private
    FGlyph: TBitmap;
    FGlyphAlignment: TGlyphAlignment;
    FGlyphOffset: Integer;
    FNumGlyphs: TNumGlyphs;
    FShowGlyph: Boolean;

    FImageIndex: TImageIndex;
    FDisabledIndex: TImageIndex;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;

    { Internal Event Handlers }
    procedure GlyphChangedHandler( Sender: TObject );
    procedure ImagesChange( Sender: TObject );
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function GetCaptionRect: TRect; override;
    function GetMinWidth: Integer; override;
    function GetImageSize: TPoint;
    procedure Paint; override;
    procedure DrawGlyph( R: TRect ); virtual;
    procedure DrawImage( R: TRect ); virtual;

    { Property Access Methods }
    procedure SetNumGlyphs( Value: TNumGlyphs ); virtual;
    procedure SetGlyph( Value: TBitmap ); virtual;
    procedure SetGlyphAlignment( Value: TGlyphAlignment ); virtual;
    procedure SetGlyphOffset( Value: Integer ); virtual;
    procedure SetShowGlyph( Value: Boolean ); virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;
    procedure SetDisabledIndex( Value: TImageIndex ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;

    { Property Declarations }
    property Glyph: TBitmap
      read FGlyph
      write SetGlyph;

    property GlyphAlignment: TGlyphAlignment
      read FGlyphAlignment
      write SetGlyphAlignment
      default gaLeft;

    property GlyphOffset: Integer
      read FGlyphOffset
      write SetGlyphOffset
      default 2;

    property NumGlyphs: TNumGlyphs
      read FNumGlyphs
      write SetNumGlyphs
      default 1;

    property ShowGlyph: Boolean
      read FShowGlyph
      write SetShowGlyph
      default True;

    property DisabledIndex: TImageIndex
      read FDisabledIndex
      write SetDisabledIndex
      default -1;

    property ImageIndex: TImageIndex
      read FImageIndex
      write SetImageIndex
      default -1;

    property Images: TCustomImageList
      read FImages
      write SetImages;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  end; {== TRzCustomGlyphStatus Class Declaration ==}


  TRzGlyphStatus = class( TRzCustomGlyphStatus )
  published
    property Alignment;
    property AutoHint;
    property AutoSize;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property Caption;
    property CaptionOffset;
    property DefaultWidth;
    property Glyph;
    property GlyphAlignment;
    property GlyphOffset;
    property ImageIndex;
    property DisabledIndex;
    property Images;
    property NumGlyphs;
    property ShowGlyph;
  end;


  {========================================}
  {== TRzPollingStatus Class Declaration ==}
  {========================================}

  TRzPollingStatus = class( TRzCustomStatusPane )
  private
    FOnTimerExpired: TNotifyEvent;
  protected
    { Event Dispatch Methods }
    procedure TimerExpired; dynamic;

    { Property Access Methods }
    function GetActive: Boolean; virtual;
    procedure SetActive( Value: Boolean ); virtual;
    function GetInterval: Word; virtual;
    procedure SetInterval( Value: Word ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property Active: Boolean
      read GetActive
      write SetActive
      default True;

    property Interval: Word
      read GetInterval
      write SetInterval
      default 500;

    property OnTimerExpired: TNotifyEvent
      read FOnTimerExpired
      write FOnTimerExpired;
  end; {== TRzPollingStatus Class Declaration ==}


  {======================================}
  {== TRzClockStatus Class Declaration ==}
  {======================================}

  TRzGetDateTimeEvent = procedure ( Sender: TObject; var DateTime: TDateTime ) of object;

  TRzClockStatus = class( TRzPollingStatus )
  private
    FFormat: string;
    FDateTimeStr: string;

    FOnGetDateTime: TRzGetDateTimeEvent;
  protected
    procedure Loaded; override;
    procedure TimerExpired; override;
    function GetCaptionRect: TRect; override;
    procedure Paint; override;
    procedure SetCurrentDateTimeStr; virtual;

    { Property Access Methods }
    procedure SetFormat( Value: string );
  public
    constructor Create( AOwner: TComponent ); override;
  published
    property Format: string
      read FFormat
      write SetFormat;

    property OnGetDateTime: TRzGetDateTimeEvent
      read FOnGetDateTime
      write FOnGetDateTime;

    { Inherited Properties }
    property Alignment;
    property AutoSize;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property CaptionOffset;
    property DefaultWidth;
    property Width default 150;
  end; {== TRzClockStatus Class Declaration ==}


  {====================================}
  {== TRzKeyStatus Class Declaration ==}
  {====================================}

  TRzKeyStatus = class;
  TRzToggleKey = ( tkCapsLock, tkNumLock, tkScrollLock, tkInsert );
  TRzToggleState  = ( tsOn, tsOff );
  TRzInsertState = ( isInsert, isOverwrite );
  TRzKeyOffAppearance = ( koaBlank, koaDimmed );

  TRzKeyStrings = class( TPersistent )
  private
    FKeyStatus: TRzKeyStatus;
    FCapsLock: string;
    FNumLock: string;
    FScrollLock: string;
    FInsert: string;
    FOverwrite: string;
  protected
    procedure SetKeyString( Index: Integer; const Value: string );
  public
    constructor Create;
  published
    property CapsLock: string
      index 0
      read FCapsLock
      write SetKeyString;

    property NumLock: string
      index 1
      read FNumLock
      write SetKeyString;

    property ScrollLock: string
      index 2
      read FScrollLock
      write SetKeyString;

    property Insert: string
      index 3
      read FInsert
      write SetKeyString;

    property Overwrite: string
      index 4
      read FOverwrite
      write SetKeyString;
  end;


  TRzKeyStatus = class( TRzPollingStatus )
  private
    FKey: TRzToggleKey;
    FKeyStrings: TRzKeyStrings;
    FStoreKeyStrings: Boolean;
    FState: TRzToggleState;
    FInitInsertState: TRzInsertState;
    FCurrentInsertState: TRzInsertState;
    FInitInsertKeyState: Boolean;
    FKeyOffAppearance: TRzKeyOffAppearance;
  protected
    procedure Loaded; override;
    procedure TimerExpired; override;
    procedure Paint; override;
    function GetKeyString( Key: TRzToggleKey ): string;
    procedure KeyStringChanged;

    { Property Access Methods }
    procedure SetKey( Value: TRzToggleKey ); virtual;
    procedure SetKeyOffAppearance( Value: TRzKeyOffAppearance ); virtual;
    procedure SetKeyStrings( Value: TRzKeyStrings ); virtual;
    procedure SetState( Value: TRzToggleState ); virtual;
    procedure SetInitInsertState( Value: TRzInsertState ); virtual;
    procedure SetCurrentInsertState( Value: TRzInsertState ); virtual;

    function GetInsertState: TRzInsertState;
    procedure SetInsertState( Value: TRzInsertState );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    { Property Declarations }
    property State: TRzToggleState
      read FState;
  published
    property Key: TRzToggleKey
      read FKey
      write SetKey
      default tkCapsLock;

    property KeyOffAppearance: TRzKeyOffAppearance
      read FKeyOffAppearance
      write SetKeyOffAppearance
      default koaBlank;

    property KeyStrings: TRzKeyStrings
      read FKeyStrings
      write SetKeyStrings
      stored FStoreKeyStrings;

    property InsertState: TRzInsertState
      read GetInsertState
      write SetInsertState
      default isInsert;

    { Inherited Properties & Events }
    property Alignment;
    property AutoSize;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property DefaultWidth;
    property Width default 45;
  end; {== TRzKeyStatus Class Declaration ==}


  {=========================================}
  {== TRzResourceStatus Class Declaration ==}
  {=========================================}

  TRzResourceStatus = class;
  TResourceType = ( rtSystem, rtMemory );
  TDisplayStyle = ( dsBar, dsText );

  TRzResourceStrings = class( TPersistent )
  private
    FResourceStatus: TRzResourceStatus;
    FSystem: string;
    FMemory: string;
  protected
    procedure SetResourceString( Index: Integer; const Value: string );
    procedure DefineProperties( Filer: TFiler ); override;
  public
    constructor Create;
  published
    property System: string
      index 0
      read FSystem
      write SetResourceString;

    property Memory: string
      index 1
      read FMemory
      write SetResourceString;
  end;


  TRzResourceStatus = class( TRzPollingStatus )
  private
    FResourceStrings: TRzResourceStrings;
    FStoreResourceStrings: Boolean;
    FBackColor: TColor;
    FBackColorStop: TColor;
    FBarColor: TColor;
    FBarColorStop: TColor;
    FBarStyle: TBarStyle;
    FFreeMemory: Longint;
    FFreePercent: Integer;
    FGradientDirection: TGradientDirection;
    FOrientation: TOrientation;
    FNumSegments: TSegmentRange;
    FResourceType: TResourceType;
    FShowPercent: Boolean;
    FDisplayStyle: TDisplayStyle;
  protected
    procedure DefineProperties( Filer: TFiler ); override;

    procedure TimerExpired; override;
    procedure Paint; override;

    function GetResourceString( ResType: TResourceType ): string;
    procedure ResourceStringChanged;

    { Property Access Methods }
    procedure SetBackColor( Value: TColor ); virtual;
    procedure SetBackColorStop( Value: TColor );
    procedure SetBarColor( Value: TColor ); virtual;
    procedure SetBarColorStop( Value: TColor );
    procedure SetBarStyle( Value: TBarStyle ); virtual;
    procedure SetDisplayStyle( Value: TDisplayStyle ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection );
    procedure SetNumSegments( Value: TSegmentRange ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
    procedure SetResourceStrings( Value: TRzResourceStrings ); virtual;
    procedure SetShowPercent( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    { Property Declarations }
    property FreeMemory: Longint
      read FFreeMemory;

    property FreePercent: Integer
      read FFreePercent;
  published
    property BackColor: TColor
      read FBackColor
      write SetBackColor
      default clBtnFace;

    property BackColorStop: TColor
      read FBackColorStop
      write SetBackColorStop
      default clBtnFace;

    property BarColor: TColor
      read FBarColor
      write SetBarColor
      default clHighlight;

    property BarColorStop: TColor
      read FBarColorStop
      write SetBarColorStop
      default clHotLight;

    property BarStyle: TBarStyle
      read FBarStyle
      write SetBarStyle
      default bsTraditional;

    property DisplayStyle: TDisplayStyle
      read FDisplayStyle
      write SetDisplayStyle
      default dsBar;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalCenter;

    property NumSegments: TSegmentRange
      read FNumSegments
      write SetNumSegments
      default 15;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    property ResourceStrings: TRzResourceStrings
      read FResourceStrings
      write SetResourceStrings
      stored FStoreResourceStrings;

    property ShowPercent: Boolean
      read FShowPercent
      write SetShowPercent
      default False;

    { Inherited Properties & Events }
    property Alignment;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property CaptionOffset;
    property Hint stored False;
    property ShowHint default True;
  end; {== TRzResourceStatus Class Declaration ==}


  {========================================}
  {== TRzMarqueeStatus Class Declaration ==}
  {========================================}

  TRzMarqueeStatus = class( TRzStatusPane )
  private
    FScrollType: TRzScrollType;
    FScrolling: Boolean;
    FSteps: Integer;
    FScrollDelay: Word;
    FScrollSize: Word;
    FBitmap: TBitmap;
    FTimer: TTimer;
    FCurrentStep: Integer;
    FOnScrollDisplay: TRzScrollDisplayEvent;
    FOnScrollComplete: TNotifyEvent;

    { Internal Event Handlers }
    procedure TimerEventHandler( Sender: TObject );

    { Message Handling Methods }
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
  protected
    procedure DrawCaption( const CaptionStr: string ); override;

    procedure UpdateSteps;

    { Event Dispatch Methods }
    procedure ScrollDisplay( CurrentStep, TotalSteps: Integer ); dynamic;
    procedure ScrollComplete; dynamic;

    { Property Access Methods }
    procedure SetScrollType( Value: TRzScrollType );
    procedure SetScrolling( Value: Boolean ); virtual;
    procedure SetScrollSize( Value: Word ); virtual;
    procedure SetScrollDelay( Value: Word );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;
  published
    property ScrollType: TRzScrollType
      read FScrollType
      write SetScrollType
      default stRightToLeft;

    property Scrolling: Boolean
      read FScrolling
      write SetScrolling
      default True;

    property ScrollSize: Word
      read FScrollSize
      write SetScrollSize
      default 1;

    property ScrollDelay: Word
      read FScrollDelay
      write SetScrollDelay
      default 100;

    property OnScrollDisplay: TRzScrollDisplayEvent
      read FOnScrollDisplay
      write FOnScrollDisplay;

    property OnScrollComplete: TNotifyEvent
      read FOnScrollComplete
      write FOnScrollComplete;
  end;


  {============================================}
  {== TRzCustomFieldStatus Class Declaration ==}
  {============================================}

  TRzCustomFieldStatus = class( TRzCustomStatusPane )
  private
    FFieldLabel: string;
    FFieldLabelColor: TColor;
  protected
    function GetMinWidth: Integer; override;
    function GetCaptionRect: TRect; override;
    procedure DrawCaption( const CaptionStr: string ); override;

    { Property Access Methods }
    procedure SetFieldLabel( const Value: string ); virtual;
    procedure SetFieldLabelColor( Value: TColor ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
  published
    property FieldLabel: string
      read FFieldLabel
      write SetFieldLabel;

    property FieldLabelColor: TColor
      read FFieldLabelColor
      write SetFieldLabelColor
      default clHighlight;

    { Inherited Properties }
    property Alignment;                                    // No Need to surface Caption property b/c the
    property AutoSize;                                     // TRzVersionInfo will supply the display string
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property CaptionOffset;
  end;


  {======================================}
  {== TRzFieldStatus Class Declaration ==}
  {======================================}

  TRzFieldStatus = class( TRzCustomFieldStatus )
  private
    FAboutInfo: TRzAboutInfo;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AutoHint;
    property Caption;
  end;


  {======================================}
  {== TRzVersionInfo Class Declaration ==}
  {======================================}

  TRzVersionInfoStatus = class;

  TRzVersionInfoField = ( vifCompanyName, vifFileDescription, vifFileVersion, vifInternalName, vifCopyright,
                          vifTrademarks, vifOriginalFilename, vifProductName, vifProductVersion, vifComments );

  TRzVersionInfo = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FFilePath: string;
    FVersionInfo: TStringList;
    FVersionInfoAvailable: Boolean;
    FStatusList: TList;
    FVersionNumbers: array[ 1..4 ] of Word;
    FModuleAttributes: array[ 1..4 ] of Boolean;
  protected
    procedure Loaded; override;
    procedure GetVersionInfo;
    function GetField( Index: Integer ): string;
    function GetVerField( Index: TRzVersionInfoField ): string;
    function GetVersionNumber( Index: Integer ): Word;
    function GetModuleAttribute( Index: Integer ): Boolean;
    function GetFileDateTime: TDateTime;

    procedure UpdateStatusControls;

    procedure SetFilePath( const Value: string ); virtual;

    property Fields[ Index: TRzVersionInfoField ]: string
      read GetVerField; default;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure AddStatus( S: TRzVersionInfoStatus );
    procedure RemoveStatus( S: TRzVersionInfoStatus );

    function GetCustomKeyValue( const Key: string ): string;

    property VersionInfoAvailable: Boolean
      read FVersionInfoAvailable;

    property CompanyName: string
      index 0 // vifCompanyName
      read GetField;

    property FileDescription: string
      index 1 // vifFileDescription
      read GetField;

    property FileVersion: string
      index 2 // vifFileVersion
      read GetField;

    property InternalName: string
      index 3 // vifInternalName
      read GetField;

    property Copyright: string
      index 4 // vifCopyright
      read GetField;

    property Trademarks: string
      index 5 // vifTrademarks
      read GetField;

    property OriginalFilename: string
      index 6 // vifOriginalFilename
      read GetField;

    property ProductName: string
      index 7 // vifProductName
      read GetField;

    property ProductVersion: string
      index 8 // vifProductVersion
      read GetField;

    property Comments: string
      index 9 // vifComments
      read GetField;

    // Version Numbers

    property MajorVersion: Word
      index 1
      read GetVersionNumber;

    property MinorVersion: Word
      index 2
      read GetVersionNumber;

    property Release: Word
      index 3
      read GetVersionNumber;

    property Build: Word
      index 4
      read GetVersionNumber;

    // Module Attributes

    property DebugBuild: Boolean
      index 1
      read GetModuleAttribute;

    property PreRelease: Boolean
      index 2
      read GetModuleAttribute;

    property PrivateBuild: Boolean
      index 3
      read GetModuleAttribute;

    property SpecialBuild: Boolean
      index 4
      read GetModuleAttribute;

    property FileDateTime: TDateTime
      read GetFileDateTime;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property FilePath: string
      read FFilePath
      write SetFilePath;
  end;


  {============================================}
  {== TRzVersionInfoStatus Class Declaration ==}
  {============================================}

  TRzVersionInfoStatus = class( TRzCustomFieldStatus )
  private
    FVersionInfo: TRzVersionInfo;
    FField: TRzVersionInfoField;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure UpdateCaption;

    { Property Access Methods }
    procedure SetVersionInfo( Value: TRzVersionInfo ); virtual;
    procedure SetField( Value: TRzVersionInfoField ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
  published
    property Field: TRzVersionInfoField
      read FField
      write SetField;

    property VersionInfo: TRzVersionInfo
      read FVersionInfo
      write SetVersionInfo;

    { Inherited Properties }
    property Alignment;                                    // No Need to surface Caption property b/c the
    property AutoSize;                                     // TRzVersionInfo will supply the display string
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property CaptionOffset;
  end;


  TRzProgressStatus = class( TRzCustomStatusPane )
  private
    FBackColor: TColor;
    FBackColorStop: TColor;
    FBarColor: TColor;
    FBarColorStop: TColor;
    FBarStyle: TBarStyle;
    FGradientDirection: TGradientDirection;
    FNumSegments: TSegmentRange;
    FPartsComplete: TUnsignedLongint;
    FPercent: Word;
    FShowPercent: Boolean;
    FShowParts: Boolean;
    FTotalParts: TUnsignedLongint;
  protected
    procedure Paint; override;

    { Property Access Methods }
    procedure SetBackColor( Value: TColor ); virtual;
    procedure SetBackColorStop( Value: TColor );
    procedure SetBarColor( Value: TColor ); virtual;
    procedure SetBarColorStop( Value: TColor );
    procedure SetBarStyle( Value: TBarStyle ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection );
    procedure SetNumSegments( Value: TSegmentRange ); virtual;
    procedure SetPartsComplete( Value: TUnsignedLongint ); virtual;
    procedure SetPercent( Value: Word ); virtual;
    procedure SetShowPercent( Value: Boolean ); virtual;
    procedure SetShowParts( Value: Boolean ); virtual;
    procedure SetTotalParts( Value: TUnsignedLongint ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;

    procedure IncPartsByOne;
    procedure IncParts( N: TUnsignedLongint );
  published
    property BackColor: TColor
      read FBackColor
      write SetBackColor
      default clBtnFace;

    property BackColorStop: TColor
      read FBackColorStop
      write SetBackColorStop
      default clBtnFace;

    property BarColor: TColor
      read FBarColor
      write SetBarColor
      default clHighlight;

    property BarColorStop: TColor
      read FBarColorStop
      write SetBarColorStop
      default clHotLight;

    property BarStyle: TBarStyle
      read FBarStyle
      write SetBarStyle
      default bsTraditional;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalCenter;

    property NumSegments: TSegmentRange
      read FNumSegments
      write SetNumSegments
      default 15;

    property PartsComplete: TUnsignedLongint
      read FPartsComplete
      write SetPartsComplete;

    property Percent: Word
      read FPercent
      write SetPercent;

    property ShowPercent: Boolean
      read FShowPercent
      write SetShowPercent
      default False;

    property ShowParts: Boolean
      read FShowParts
      write SetShowParts
      default False;

    property TotalParts: TUnsignedLongint
      read FTotalParts
      write SetTotalParts;

    { Inherited Properties & Events }
    property Alignment;
    property Hint stored False;
    property ShowHint default True;
  end; {== TRzResourceStatus Class Declaration ==}


implementation


uses
  {&RAS}
  Types,
  UxTheme,
  Themes,
  RzPanel,
  RzGrafx,
  RzSysRes,
  StdActns;


{=====================}
{== Support Methods ==}
{=====================}

procedure ChangeBiDiModeGlyphAlignment( var GlyphAlignment: TGlyphAlignment );
begin
  case GlyphAlignment of
    gaLeft:  GlyphAlignment := gaRight;
    gaRight: GlyphAlignment := gaLeft;
  end;
end;


{===============================================================================
  TRzPollingList Class

  Implementation specific class designed to wrap a timer component and
  maintain a list of polling controls.  When the internal timer fires,
  the TimerExpired method of each polling control in the list is called.
===============================================================================}

type
  TRzPollingList = class
  private
    FControls: TList;
    FTimer: TTimer;
  protected
    function GetCount: Integer; virtual;
    function GetEnabled: Boolean; virtual;
    procedure SetEnabled( Value: Boolean ); virtual;
    function GetInterval: Word; virtual;
    procedure SetInterval( Value: Word ); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add( Control: TRzPollingStatus );
    procedure Remove( Control: TRzPollingStatus );
    procedure TimerFired( Sender: TObject );

    property Count: Integer
      read GetCount;

    property Enabled: Boolean
      read GetEnabled
      write SetEnabled;

    property Interval: Word
      read GetInterval
      write SetInterval;
  end;



var
  VKCodes: array[ TRzToggleKey ] of Byte = ( vk_Capital, vk_NumLock, vk_Scroll, vk_Insert );
  InitKeyStrings: array[ TRzToggleKey ] of string = ( 'CAPS', 'NUM', 'SCR', 'Insert' );
  InitResourceStrings: array[ TResourceType ] of string = ( 'System', 'Memory' );
  PollingControls: TRzPollingList = nil;

{&RT}
{=================================}
{== TRzCustomStatusPane Methods ==}
{=================================}

constructor TRzCustomStatusPane.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle + [ csNoDesignVisible ] - [ csOpaque ];

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;
  
  FAutoSize := False;
  FDefaultWidth := 40;
  FBlinkColor := clHighlight;
  FFrameStyle := fsFlat;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;
  FCaptionOffset := 2;
  FBorderWidth := 1;
  Width := 100;
  Height := 20;
  ParentColor := True;
  FParentFillColor := True;
  FFillColor := inherited Color;
  FTransparent := True;


  { If the BlinkingControls object has not already been created, then create it }
  if BlinkingControls = nil then
    BlinkingControls := TRzBlinkingControlsList.Create;
  BlinkingControls.Add( Self );
  {&RCI}
end;


destructor TRzCustomStatusPane.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  BlinkingControls.Remove( Self );
  FreeBlinkingControlsListIfEmpty;
  inherited;
end;


procedure TRzCustomStatusPane.Loaded;
begin
  {&RV}
  inherited;
  
  if ParentColor = False then
  begin
    // if ParentColor is False, assume that FillColor has been changed by user
    // and thus Transparent should be set to False.
    FTransparent := False;
  end;

  FFontColor := Font.Color;
  FCurrentColor := GetFontColor;
  AdjustBounds;
end;


procedure TRzCustomStatusPane.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzCustomStatusPane.AdjustBounds;
var
  DC: HDC;
  W: Integer;
begin
  if not ( csReading in ComponentState ) and FAutoSize then
  begin
    DC := GetDC( 0 );
    Canvas.Handle := DC;
    W := GetMinWidth;
    Canvas.Handle := 0;
    ReleaseDC( 0, DC );
    Width := W;
  end;
end;


function TRzCustomStatusPane.GetMinWidth: Integer;
var
  R: TRect;
begin
  if Caption <> '' then
  begin
    R := ClientRect;
    Canvas.Font := Self.Font;
    DrawString( Canvas, Caption, R, dt_ExpandTabs or dt_NoPrefix or dt_CalcRect );
    Result := R.Right + ( 2 * FBorderWidth ) + FCaptionOffset + 6;
  end
  else
    Result := FDefaultWidth;
end;


procedure TRzCustomStatusPane.SetAlignment( Value: TAlignment );
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;


procedure TRzCustomStatusPane.SetAutoSize( Value: Boolean );
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;


procedure TRzCustomStatusPane.SetDefaultWidth( Value: Integer );
begin
  if FDefaultWidth <> Value then
  begin
    FDefaultWidth := Value;
    AdjustBounds;
  end;
end;


procedure TRzCustomStatusPane.SetBlinking( Value: Boolean );
begin
  if FBlinking <> Value then
  begin
    FBlinking := Value;
    if not FBlinking then
    begin
      FBlinkState := bsOff;
      FCurrentColor := GetFontColor;
      Repaint;
    end;
  end;
end;


procedure TRzCustomStatusPane.SetBlinkColor( Value: TColor );
begin
  if FBlinkColor <> Value then
  begin
    FBlinkColor := Value;
    Invalidate;
  end;
end;


function TRzCustomStatusPane.GetBlinkIntervalOff: Word;
begin
  Result := BlinkingControls.IntervalOff;
end;

procedure TRzCustomStatusPane.SetBlinkIntervalOff( Value: Word );
begin
  BlinkingControls.IntervalOff := Value;
end;


function TRzCustomStatusPane.GetBlinkIntervalOn: Word;
begin
  Result := BlinkingControls.IntervalOn;
end;

procedure TRzCustomStatusPane.SetBlinkIntervalOn( Value: Word );
begin
  BlinkingControls.IntervalOn := Value;
end;


procedure TRzCustomStatusPane.Blink( State: TBlinkState );
begin
  if State = bsOn then
  begin
    FBlinkState := bsOff;
    FCurrentColor := GetFontColor;
  end
  else
  begin
    FBlinkState := bsOn;
    FCurrentColor := FBlinkColor;
  end;
  Refresh;
end;



procedure TRzCustomStatusPane.SetBorderWidth( Value: TBorderWidth );
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Invalidate;                       { Invalidate b/c Border position changes }
  end;
end;


procedure TRzCustomStatusPane.SetCaption( Value: TCaption );
begin
  {&RV}
  if FCaption <> Value then
  begin
    FCaption := Value;
    Perform( cm_TextChanged, 0, 0 );
  end;
end;


procedure TRzCustomStatusPane.SetCaptionOffset( Value: Integer );
begin
  if FCaptionOffset <> Value then
  begin
    FCaptionOffset := Value;
    Invalidate;
    AdjustBounds;
  end;
end;


function TRzCustomStatusPane.IsFillColorStored: Boolean;
begin
  Result := not FParentFillColor;
end;


procedure TRzCustomStatusPane.SetFillColor( Value: TColor );
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    FParentFillColor := False;
    Invalidate;
  end;
end;


procedure TRzCustomStatusPane.SetParentFillColor( Value: Boolean );
begin
  if FParentFillColor <> Value then
  begin
    FParentFillColor := Value;
    if ( Parent <> nil ) and not ( csReading in ComponentState ) then
      Perform( cm_ParentColorChanged, 0, 0 );
  end;
end;


procedure TRzCustomStatusPane.SetTransparent( Value: Boolean );
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [ csOpaque ]
    else
      ControlStyle := ControlStyle + [ csOpaque ];
    Invalidate;
  end;
end;


procedure TRzCustomStatusPane.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomStatusPane.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzCustomStatusPane.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomStatusPane.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomStatusPane.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameStyle in FFrameControllerNotifications then
      FrameStyle := FFrameController.FrameStyle;
    if fcpFrameColor in FFrameControllerNotifications then
    begin
      FFlatColor := FFrameController.FrameColor;
      FFlatColorAdjustment := 0;
    end;
    if fcpColor in FFrameControllerNotifications then
      FillColor := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;
    Invalidate;
  end;
end;



procedure TRzCustomStatusPane.CMGetBlinking( var Msg: TMessage );
begin
  inherited;
  if FBlinking then
    Msg.Result := 1
  else
    Msg.Result := 0;
end;

procedure TRzCustomStatusPane.CMBlink( var Msg: TMessage );
begin
  inherited;
  if FBlinking then
    Blink( TBlinkState( Msg.WParam ) );
end;


procedure TRzCustomStatusPane.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  if csLoading in ComponentState then
    Exit;

  FFontColor := Font.Color;
  if not FBlinking then
    FCurrentColor := GetFontColor;

  AdjustBounds;
end;


procedure TRzCustomStatusPane.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
  AdjustBounds;
end;


procedure TRzCustomStatusPane.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if FParentFillColor then
    FFillColor := Color;
  Invalidate;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzCustomStatusPane.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  FCurrentColor := GetFontColor;
  Invalidate;
end;

{$ENDIF}


procedure TRzCustomStatusPane.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;
  if FParentFillColor then
    FFillColor := Color;
  Invalidate;
end;


function TRzCustomStatusPane.UnderSizeGrip( R: TRect ): Boolean;
begin
  Result := ( Parent is TRzStatusBar ) and ( TRzStatusBar( Parent ).ShowSizeGrip ) and
            ( ClientToScreen( R.BottomRight ).X >=
              Parent.ClientToScreen( Parent.ClientRect.BottomRight).X );
end;


function TRzCustomStatusPane.UseThemes: Boolean;
begin
  Result := ( Parent <> nil ) and ( Parent is TRzStatusBar ) and
            ( TRzStatusBar( Parent ).VisualStyle = vsWinXP ) and
            ActiveStyleServicesEnabled;
end;


{===============================================================================
  TRzCustomStatusPane.CMHitTest

  Description
    This message is handled in status components so that when they are used
    in an RzStatusBar component, the size grip can force the mouse message
    all the way down to the form.
===============================================================================}

procedure TRzCustomStatusPane.CMHitTest( var Msg: TMessage );
var
  R: TRect;
  P: TPoint;
begin
  inherited;

  R := ClientRect;

  if UnderSizeGrip( R ) then
  begin
    { Calculate coordinates of Size Grip rectangle }
    P := Parent.ClientToScreen( Parent.ClientRect.BottomRight );
    R.Right := ScreenToClient( P ).X;
    R.Left := R.Right - 12;

    if PtInRect( R, Point( Msg.LParamLo, Msg.LParamHi ) ) then
      Msg.Result := Parent.Perform( wm_NCHitTest, Msg.WParam, Msg.LParam );
  end;
end;



function TRzCustomStatusPane.GetCaptionRect: TRect;
var
  Offset: Integer;
begin
  Result := ClientRect;

  if FFrameStyle in [ fsFlat, fsStatus, fsPopup ] then
    Offset := FBorderWidth + 1
  else if FFrameStyle in [ fsGroove..fsButtonUp, fsFlatBold ] then
    Offset := FBorderWidth + 2
  else
    Offset := FBorderWidth;

  InflateRect( Result, -Offset, -Offset );
end;


function TRzCustomStatusPane.GetFontColor: TColor;
begin
  if UsingSystemStyle then
    Result := FFontColor
  else
    Result := ActiveStyleFontColor( sfStatusPanelTextNormal );
end;


function TRzCustomStatusPane.GetDisabledFontColor: TColor;
begin
  if UsingSystemStyle then
    Result := clGrayText
  else
    Result := ActiveStyleFontColor( sfStatusPanelTextDisabled );
end;


procedure TRzCustomStatusPane.DrawCaption( const CaptionStr: string );
var
  X, Y: Integer;
  R, ThemeRect, SrcRect: TRect;
  ElementDetails: TThemedElementDetails;
  Bmp: TBitmap;
  TempAlignment: TAlignment;
begin
  R := GetCaptionRect;

  TempAlignment := FAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment( TempAlignment );

  case TempAlignment of
    taLeftJustify:  X := R.Left + FCaptionOffset + 1;
    taRightJustify: X := R.Right - FCaptionOffset - 1;
    taCenter:       X := R.Left + ( R.Right - R.Left ) div 2;
  else
    X := 0;
  end;
  Y := R.Top + ( R.Bottom - R.Top - Canvas.TextHeight( 'Pp' ) ) div 2;

  // Reset the Brush.Color to set clNone because this will force the Canvas
  // to reset the brush settings, which is necessary for property display
  // when using VCL Styles.
  Canvas.Brush.Color := clNone;
  Canvas.Brush.Style := bsClear;

  if UseThemes then
  begin
    // If using themes, then we need to draw the background first
    Bmp := TBitmap.Create;
    try
      Bmp.Width := TRzStatusBar( Parent ).Width;
      Bmp.Height := TRzStatusBar( Parent ).Height;

      ElementDetails := ActiveStyleServices.GetElementDetails( tsStatusRoot );
      ThemeRect := Rect( 0, 0, Bmp.Width, Bmp.Height );
      ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, ElementDetails, ThemeRect );

      SrcRect := Rect( BoundsRect.Left + R.Left, BoundsRect.Top + R.Top,
                       BoundsRect.Left + R.Right, BoundsRect.Top + R.Bottom );
      Canvas.CopyRect( R, Bmp.Canvas, SrcRect );
    finally
      Bmp.Free;
    end;
  end
  else // No themes
  begin
    // Set brush color so that old text in caption area gets erased
    if not FTransparent then
    begin
      Canvas.Brush.Style := bsSolid;
      if UsingSystemStyle then
        Canvas.Brush.Color := FFillColor
      else
        Canvas.Brush.Color := ActiveStyleSystemColor( clBtnFace );
    end;
  end;

  // Draw the Text;

  SetTextAlign( Canvas.Handle, SetTextAlignments[ TempAlignment ] );

  Canvas.Font := Font;
  if Enabled then
    Canvas.Font.Color := FCurrentColor
  else
    Canvas.Font.Color := GetDisabledFontColor;

  Canvas.TextRect( R, X, Y, CaptionStr );
end; {= TRzCustomStatusPane.DrawCaption =}


procedure TRzCustomStatusPane.DrawStatusBorder;
var
  R: TRect;
  StartColor, StopColor, DividerColor, C: TColor;
  ElementDetails: TThemedElementDetails;
  StatusBar: TRzStatusBar;
  Offset: Integer;
begin
  if UseThemes then
  begin
    R := ClientRect;
    Inc( R.Top, 2 );
    ElementDetails := ActiveStyleServices.GetElementDetails( tsPane );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end
  else
  begin
    R := Rect( 0, 0, Width, Height );

    if not FTransparent then
    begin
      if UsingSystemStyle then
        Canvas.Brush.Color := Color
      else
        Canvas.Brush.Color := ActiveStyleSystemColor( clBtnFace );
      Canvas.FillRect( R );
    end;

    Canvas.Pen.Width := 1;
    InflateRect( R, -FBorderWidth, -FBorderWidth );

    if ( Parent <> nil ) and ( Parent is TRzStatusBar ) and
       ( TRzStatusBar( Parent ).VisualStyle = vsGradient ) then
    begin
      StatusBar := TRzStatusBar( Parent );

      Offset := StatusBar.BorderWidth;
      Offset := Offset + StatusBar.Padding.Left;

      if Left > Offset then
      begin
        // Not first pane, so okay to draw divider
        GetGradientStatusBarColors( StatusBar.GradientColorStyle, StartColor,
                                    StopColor, DividerColor );
        Canvas.Pen.Color := DividerColor;

        // Left side only
        Canvas.MoveTo( R.Left, R.Top + 1 );
        Canvas.LineTo( R.Left, R.Bottom - 1 );
      end;
    end
    else
    begin
      if FFrameStyle = fsFlat then
      begin
        if UsingSystemStyle then
          C := AdjustColor( FFlatColor, FFlatColorAdjustment )
        else
          C := ActiveStyleFontColor( sfStatusPanelTextDisabled );
        Canvas.Pen.Color := C;
        // Left side
        Canvas.MoveTo( R.Left, R.Top + 1 );
        Canvas.LineTo( R.Left, R.Bottom - 1 );
        // Top side
        Canvas.MoveTo( R.Left + 1, R.Top );
        Canvas.LineTo( R.Right - 1, R.Top );
        // Right side
        Canvas.MoveTo( R.Right - 1, R.Top + 1 );
        Canvas.LineTo( R.Right - 1, R.Bottom - 1 );
        // Bottom side
        Canvas.MoveTo( R.Left + 1, R.Bottom - 1 );
        Canvas.LineTo( R.Right - 1, R.Bottom - 1 );
      end
      else if FFrameStyle = fsFlatBold then
      begin
        if UsingSystemStyle then
          C := AdjustColor( FFlatColor, FFlatColorAdjustment )
        else
          C := ActiveStyleFontColor( sfStatusPanelTextDisabled );
        DrawBevel( Canvas, R, C, C, 2, sdAllSides );
      end
      else
        DrawBorder( Canvas, R, FFrameStyle );
    end;
  end;
end; {= TRzCustomStatusPane.DrawStatusBorder =}


procedure TRzCustomStatusPane.Paint;
begin
  DrawStatusBorder;
  DrawCaption( Caption );
end;


procedure TRzCustomStatusPane.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;
end;


procedure TRzCustomStatusPane.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
end;


function TRzCustomStatusPane.ExecuteAction( Action: TBasicAction ): Boolean;
begin
  if AutoHint and ( Action is THintAction ) then
  begin
    Caption := THintAction( Action ).Hint;
    Result := True;
  end
  else
    Result := inherited ExecuteAction( Action );
end;


{==================================}
{== TRzCustomGlyphStatus Methods ==}
{==================================}

constructor TRzCustomGlyphStatus.Create( AOwner: TComponent );
begin
  inherited;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChangedHandler;
  FGlyphOffset := 2;
  FNumGlyphs := 1;
  FGlyphAlignment := gaLeft;
  FShowGlyph := True;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;
  FImageIndex := -1;
  FDisabledIndex := -1;
  {&RCI}
end;


destructor TRzCustomGlyphStatus.Destroy;
begin
  FGlyph.Free;
  FImagesChangeLink.Free;
  inherited;
end;


procedure TRzCustomGlyphStatus.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FImages ) then
    SetImages( nil )  // Call access method so connections to link object can be cleared
end;


procedure TRzCustomGlyphStatus.DrawGlyph( R: TRect );
var
  DestRct, SrcRct: TRect;
  DestBmp: TBitmap;
  W, H: Integer;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  DestRct := Rect( 0, 0, W, H );
  if ( FNumGlyphs > 1 ) and not Enabled then
    SrcRct := Rect( W, 0, W + W, H )
  else
    SrcRct := Rect( 0, 0, W, H );

  // The DestBmp holds the desired region of the FGlyph bitmap

  DestBmp := TBitmap.Create;
  try
    // Don't Forget to Set the Width and Height of Destination Bitmap
    DestBmp.Width := W;
    DestBmp.Height := H;
    DestBmp.Canvas.Brush.Color := FFillColor;

    DestBmp.Canvas.CopyRect( DestRct, Canvas, R );
    DrawFullTransparentBitmap( DestBmp.Canvas, FGlyph, DestRct, SrcRct,
                               FGlyph.TransparentColor );
    Canvas.Draw( R.Left, R.Top, DestBmp );
  finally
    DestBmp.Free;
  end;
end; {= TRzCustomGlyphStatus.DrawGlyph =}


procedure TRzCustomGlyphStatus.DrawImage( R: TRect );
begin
  if FImages <> nil then
  begin
    if FDisabledIndex <> -1 then
    begin
      if Enabled then
      begin
        if FImageIndex <> -1 then
          FImages.Draw( Canvas, R.Left, R.Top, FImageIndex );
      end
      else
        FImages.Draw( Canvas, R.Left, R.Top, FDisabledIndex );
    end
    else if FImageIndex <> -1 then
      FImages.Draw( Canvas, R.Left, R.Top, FImageIndex, Enabled );
  end;
end; {= TRzCustomGlyphStatus.DrawImage =}


function TRzCustomGlyphStatus.GetImageSize: TPoint;
begin
  if ( FImages <> nil ) and ( FImageIndex <> -1 ) then
    Result := Point( FImages.Width, FImages.Height )
  else if FGlyph <> nil then
    Result := Point( FGlyph.Width div FNumGlyphs, FGlyph.Height )
  else
    Result := Point( 0, 0 );
end;



procedure TRzCustomGlyphStatus.Paint;
var
  R, GlyphRct, CaptionRct: TRect;
  W, H, X, TopOffset: Integer;
  TempGlyphAlignment: TGlyphAlignment;
begin
  inherited;

  if not FShowGlyph then
    Exit;

  Canvas.Font := Font;
  CaptionRct := GetCaptionRect;

  W := GetImageSize.X;
  if ( CaptionRct.Bottom - CaptionRct.Top ) < GetImageSize.Y then
    H := CaptionRct.Bottom - CaptionRct.Top
  else
    H := GetImageSize.Y;

  TempGlyphAlignment := FGlyphAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeGlyphAlignment( TempGlyphAlignment );

  if TempGlyphAlignment = gaLeft then
    X := FGlyphOffset + BorderWidth
  else
    X := Width - ( W + FGlyphOffset + BorderWidth + 2 );

  if FFrameStyle in [ fsGroove..fsButtonUp, fsFlatBold ] then
    Inc( X, 2 )
  else if FFrameStyle in [ fsFlat, fsStatus, fsPopup ] then
    Inc( X );

  R := Rect( X, 0, X + W, H );
  TopOffset := Abs( ( Height - H - BorderWidth - 1) ) div 2;
  OffsetRect( R, 0, BorderWidth + TopOffset );

  if not UseThemes and not Transparent then
  begin
    // Fill in area behind the glyph.  Necessary in case user changes the FillColor property.
    Canvas.Brush.Color := FFillColor;
    GlyphRct := inherited GetCaptionRect;
    GlyphRct.Right := CaptionRct.Left;
    Canvas.FillRect( GlyphRct );
  end;

  if ( FImages <> nil ) and ( FImageIndex <> -1 ) then
    DrawImage( R )
  else if not FGlyph.Empty then
    DrawGlyph( R );

end; {= TRzCustomGlyphStatus.Paint =}


function TRzCustomGlyphStatus.GetMinWidth: Integer;
begin
  Result := inherited GetMinWidth + FGlyphOffset;
  if FShowGlyph then
  begin
    if ( FImages <> nil ) and ( FImageIndex <> -1 ) then
      Result := Result + GetImageSize.X
    else
      Result := Result + FGlyph.Width div FNumGlyphs;
  end;
end;


procedure TRzCustomGlyphStatus.SetGlyphAlignment( Value: TGlyphAlignment );
begin
  if FGlyphAlignment <> Value then
  begin
    FGlyphAlignment := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphStatus.SetGlyphOffset( Value: Integer );
begin
  if FGlyphOffset <> Value then
  begin
    FGlyphOffset := Value;
    Invalidate;
    AdjustBounds;
  end;
end;


procedure TRzCustomGlyphStatus.SetNumGlyphs( Value: TNumGlyphs );
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphStatus.SetShowGlyph( Value: Boolean );
begin
  if FShowGlyph <> Value then
  begin
    FShowGlyph := Value;
    Invalidate;
    AdjustBounds;
  end;
end;


procedure TRzCustomGlyphStatus.GlyphChangedHandler( Sender: TObject );
var
  N: Integer;
begin
  if ( FGlyph.Height <> 0 ) and ( FGlyph.Width mod FGlyph.Height = 0 ) then
  begin
    N := FGlyph.Width div FGlyph.Height;
    if N > 4 then
      N := 1;
    SetNumGlyphs( N );
  end;
  Invalidate;
  AdjustBounds;
end;


procedure TRzCustomGlyphStatus.SetGlyph( Value: TBitmap );
begin
  {&RV}
  FGlyph.Assign( Value );
end;


function TRzCustomGlyphStatus.GetCaptionRect: TRect;
var
  TempGlyphAlignment: TGlyphAlignment;
begin
  Result := inherited GetCaptionRect;

  TempGlyphAlignment := FGlyphAlignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeGlyphAlignment( TempGlyphAlignment );

  if FShowGlyph then
  begin
    if ( FImages <> nil ) and ( FImageIndex <> -1 ) then
    begin
      if TempGlyphAlignment = gaLeft then
        Inc( Result.Left, FGlyphOffset + GetImageSize.X )
      else
        Dec( Result.Right, FGlyphOffset  + GetImageSize.X );
    end
    else
    begin
      if TempGlyphAlignment = gaLeft then
        Inc( Result.Left, FGlyphOffset + FGlyph.Width div FNumGlyphs )
      else
        Dec( Result.Right, FGlyphOffset  + FGlyph.Width div FNumGlyphs );
    end;
  end;
end;


procedure TRzCustomGlyphStatus.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphStatus.SetDisabledIndex( Value: TImageIndex );
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphStatus.SetImages( Value: TCustomImageList );
begin
  if FImages <> nil then
    FImages.UnRegisterChanges( FImagesChangeLink );

  FImages := Value;

  if FImages <> nil then
  begin
    FImages.RegisterChanges( FImagesChangeLink );
    FImages.FreeNotification( Self );
  end;
  Invalidate;
end;


procedure TRzCustomGlyphStatus.ImagesChange( Sender: TObject );
begin
  if Sender = Images then
  begin
    Update;         // Call Update instead of Invalidate to prevent flicker
  end;
end;





procedure FreePollingListIfEmpty;
begin
  if PollingControls.Count = 0 then
  begin
    { If no more polling controls are left, destroy PollingControls }
    PollingControls.Free;
    PollingControls := nil;
  end;
end;


{============================}
{== TRzPollingList Methods ==}
{============================}

constructor TRzPollingList.Create;
begin
  inherited Create;
  FTimer := TTimer.Create( nil );
  FTimer.Enabled := True;
  FTimer.OnTimer := TimerFired;
  FTimer.Interval := 500;

  FControls := TList.Create;
end;


destructor TRzPollingList.Destroy;
begin
  FTimer.Free;
  FControls.Free;
  inherited;
end;


procedure TRzPollingList.Add( Control: TRzPollingStatus );
begin
  FControls.Add( Control );
end;


procedure TRzPollingList.Remove( Control: TRzPollingStatus );
begin
  with FControls do
    Delete( IndexOf( Control ) );
end;

procedure TRzPollingList.TimerFired( Sender: TObject );
var
  I: Integer;
begin
  for I := 0 to FControls.Count - 1 do
  begin
    if TRzPollingStatus( FControls.Items[ I ] ).Visible then
      TRzPollingStatus( FControls.Items[ I ] ).TimerExpired;
  end;
end;

function TRzPollingList.GetCount: Integer;
begin
  Result := FControls.Count;
end;


function TRzPollingList.GetEnabled: Boolean;
begin
  Result := FTimer.Enabled;
end;

procedure TRzPollingList.SetEnabled( Value: Boolean );
begin
  if FTimer.Enabled <> Value then
    FTimer.Enabled := Value;
end;


function TRzPollingList.GetInterval: Word;
begin
  Result := FTimer.Interval;
end;

procedure TRzPollingList.SetInterval( Value: Word );
begin
  if FTimer.Interval <> Value then
    FTimer.Interval := Value;
end;


{==============================}
{== TRzPollingStatus Methods ==}
{==============================}

constructor TRzPollingStatus.Create( AOwner: TComponent );
begin
  inherited;

  { If the PollingControls object has not already been created, then create it }
  if PollingControls = nil then
    PollingControls := TRzPollingList.Create;
  PollingControls.Add( Self );
end;


destructor TRzPollingStatus.Destroy;
begin
  { Delete current Polling Control from PollingControls list }
  PollingControls.Remove( Self );
  FreePollingListIfEmpty;
  inherited;
end;


function TRzPollingStatus.GetActive: Boolean;
begin
  Result := PollingControls.Enabled;
end;


procedure TRzPollingStatus.SetActive( Value: Boolean );
begin
  PollingControls.Enabled := Value;
end;


function TRzPollingStatus.GetInterval: Word;
begin
  Result := PollingControls.Interval;
end;


procedure TRzPollingStatus.SetInterval( Value: Word );
begin
  PollingControls.Interval := Value;
end;


procedure TRzPollingStatus.TimerExpired;
begin
  if Assigned( FOnTimerExpired ) then
    FOnTimerExpired( Self );
end;


{============================}
{== TRzClockStatus Methods ==}
{============================}

constructor TRzClockStatus.Create( AOwner: TComponent );
begin
  inherited;
  Width := 150;
  FFormat := '';
  FDateTimeStr := '';
  {&RCI}
end;


procedure TRzClockStatus.Loaded;
begin
  inherited;
  SetCurrentDateTimeStr;
  FCaption := FDateTimeStr;
  AdjustBounds;
end;

procedure TRzClockStatus.SetCurrentDateTimeStr;
var
  S: string;
  DT: TDateTime;
begin
  DT := Now;
  if Assigned( FOnGetDateTime ) then
    FOnGetDateTime( Self, DT );

  if FFormat <> '' then
    S := FormatDateTime( FFormat, DT )
  else
    S := DateTimeToStr( DT );
  if FDateTimeStr <> S then
  begin
    FDateTimeStr := S;
    FCaption := FDateTimeStr;
    AdjustBounds;
    Invalidate;
  end;
end;


procedure TRzClockStatus.TimerExpired;
begin
  SetCurrentDateTimeStr;
  inherited;
end;


function TRzClockStatus.GetCaptionRect: TRect;
begin
  Result := inherited GetCaptionRect;

  if UnderSizeGrip( ClientRect ) then
    Dec( Result.Right, 12 );
end;


{===============================================================================
  TRzClockStatus.Paint

  Description
    Avoid using the Caption property to prevent extra wm_Paint messages from
    being generated when the clock needs to be updated.
===============================================================================}

procedure TRzClockStatus.Paint;
begin
  FCaption := FDateTimeStr;
  inherited;
end;


procedure TRzClockStatus.SetFormat( Value: string );
begin
  {&RV}
  if FFormat <> Value then
  begin
    FFormat := Value;
    SetCurrentDateTimeStr;
    Invalidate;
  end;
end;


{===========================}
{== TRzKeyStrings Methods ==}
{===========================}

constructor TRzKeyStrings.Create;
begin
  inherited Create;
  FCapsLock := InitKeyStrings[ tkCapsLock ];
  FNumLock := InitKeyStrings[ tkNumLock ];
  FScrollLock := InitKeyStrings[ tkScrollLock ];
  FInsert := InitKeyStrings[ tkInsert ];
  FOverwrite := 'Overwrite';
end;

procedure TRzKeyStrings.SetKeyString( Index: Integer; const Value: string );
begin
  case Index of
    0: // tkCapsLock
    begin
      if FCapsLock <> Value then
      begin
        FCapsLock := Value;
        FKeyStatus.KeyStringChanged;
      end;
    end;

    1: // tkNumLock
    begin
      if FNumLock <> Value then
      begin
        FNumLock := Value;
        FKeyStatus.KeyStringChanged;
      end;
    end;

    2: // tkScrollLock
    begin
      if FScrollLock <> Value then
      begin
        FScrollLock := Value;
        FKeyStatus.KeyStringChanged;
      end;
    end;

    3: // tkInsert
    begin
      if FInsert <> Value then
      begin
        FInsert := Value;
        FKeyStatus.KeyStringChanged;
      end;
    end;

    4: // Overwrite
    begin
      if FOverwrite <> Value then
      begin
        FOverwrite := Value;
        FKeyStatus.KeyStringChanged;
      end;
    end;
  end;
end;


{==========================}
{== TRzKeyStatus Methods ==}
{==========================}

constructor TRzKeyStatus.Create( AOwner: TComponent );
begin
  inherited;
  Width := 45;
  Alignment := taCenter;
  FState := tsOff;
  FKeyOffAppearance := koaBlank;
  FKeyStrings := TRzKeyStrings.Create;
  FKeyStrings.FKeyStatus := Self;
  FStoreKeyStrings := False;

  FInitInsertState := isInsert;
  FCurrentInsertState := FInitInsertState;
  FInitInsertKeyState := ( GetKeyState( VKCodes[ tkInsert ] ) and $1 ) = $1;
  {&RCI}
end;


destructor TRzKeyStatus.Destroy;
begin
  FKeyStrings.Free;
  inherited;
end;


procedure TRzKeyStatus.Loaded;
begin
  inherited;
//  FInitInsertState := FInsertState;
end;


procedure TRzKeyStatus.SetKey( Value: TRzToggleKey );
begin
  if FKey <> Value then
  begin
    FKey := Value;
    Invalidate;
  end;
  {&RV}
end;


procedure TRzKeyStatus.SetKeyOffAppearance( Value: TRzKeyOffAppearance );
begin
  if FKeyOffAppearance <> Value then
  begin
    FKeyOffAppearance := Value;
    Invalidate;
  end;
end;


function TRzKeyStatus.GetKeyString( Key: TRzToggleKey ): string;
begin
  case Key of
    tkCapsLock:
      Result := FKeyStrings.CapsLock;
    tkNumLock:
      Result := FKeyStrings.NumLock;
    tkScrollLock:
      Result := FKeyStrings.ScrollLock;
    tkInsert:
      Result := FKeyStrings.Insert;
  end;
end;


procedure TRzKeyStatus.KeyStringChanged;
begin
  FStoreKeyStrings := True;
  Invalidate;
end;


procedure TRzKeyStatus.SetKeyStrings( Value: TRzKeyStrings );
begin
  FKeyStrings.Assign( Value );
end;


procedure TRzKeyStatus.SetState( Value: TRzToggleState );
begin
  if FState <> Value then
  begin
    FState := Value;
    Invalidate;
  end;
end;


procedure TRzKeyStatus.SetInitInsertState( Value: TRzInsertState );
begin
  // Reset the other state variables whenever the InsertState published property
  // (i.e. FInitInsertState) is changed.
  FInitInsertState := Value;
  FCurrentInsertState := Value;
  FInitInsertKeyState := ( GetKeyState( VKCodes[ tkInsert ] ) and $1 ) = $1;
  Invalidate;
end;


procedure TRzKeyStatus.SetCurrentInsertState( Value: TRzInsertState );
begin
  if FCurrentInsertState <> Value then
  begin
    FCurrentInsertState := Value;
    Invalidate;
  end;
end;


function TRzKeyStatus.GetInsertState: TRzInsertState;
begin
  if csDesigning in ComponentState then
    Result := FInitInsertState
  else
    Result := FCurrentInsertState;
end;


procedure TRzKeyStatus.SetInsertState( Value: TRzInsertState );
begin
  SetInitInsertState( Value );
  if not ( csDesigning in ComponentState ) then
    SetCurrentInsertState( Value );
end;


procedure TRzKeyStatus.TimerExpired;
var
  CurrentInsertKeyState: Boolean;
begin
  if FKey = tkInsert then
  begin
    if not ( csDesigning in ComponentState ) then
    begin
      // If the low-order bit is 1, the key is active (or On)
      CurrentInsertKeyState := ( GetKeyState( VKCodes[ FKey ] ) and $1 ) = $1;

      if CurrentInsertKeyState = FInitInsertKeyState then
        SetCurrentInsertState( FInitInsertState )
      else
      begin
        if FInitInsertState = isInsert then
          SetCurrentInsertState( isOverwrite )
        else
          SetCurrentInsertState( isInsert );
      end;
    end;
  end
  else
  begin
    // If the low-order bit is 1, the key is active (or On)
    if ( GetKeyState( VKCodes[ FKey ] ) and $1 ) = $1 then
      SetState( tsOn )
    else
      SetState( tsOff );
  end;
  inherited;
end;


procedure TRzKeyStatus.Paint;
begin
  if FKey = tkInsert then
  begin
    if FCurrentInsertState = isInsert then
      FCaption  := GetKeyString( tkInsert )
    else
      FCaption := FKeyStrings.Overwrite;
  end
  else
  begin
    if FKeyOffAppearance = koaBlank then
    begin
      FCurrentColor := GetFontColor;
      if FState = tsOn then
        FCaption  := GetKeyString( FKey )
      else
        FCaption := '';
    end
    else { koaDimmed }
    begin
      FCaption  := GetKeyString( FKey );
      if FState = tsOn then
        FCurrentColor := GetFontColor
      else
        FCurrentColor := GetDisabledFontColor;
    end;
  end;
  inherited;
end;


{================================}
{== TRzResourceStrings Methods ==}
{================================}

constructor TRzResourceStrings.Create;
begin
  inherited Create;
  FSystem := InitResourceStrings[ rtSystem ];
  FMemory := InitResourceStrings[ rtMemory ];
end;


procedure TRzResourceStrings.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the GDI and User properties were published in version 2.x
  Filer.DefineProperty( 'User', TRzOldPropReader.ReadOldStringProp, nil, False );
  Filer.DefineProperty( 'GDI', TRzOldPropReader.ReadOldStringProp, nil, False );
end;


procedure TRzResourceStrings.SetResourceString( Index: Integer; const Value: string );
begin
  case TResourceType( Index ) of
    rtSystem:
    begin
      if FSystem <> Value then
      begin
        FSystem := Value;
        FResourceStatus.ResourceStringChanged;
      end;
    end;

    rtMemory:
    begin
      if FMemory <> Value then
      begin
        FMemory := Value;
        FResourceStatus.ResourceStringChanged;
      end;
    end;
  end;
end; {= TRzResourceStrings.SetResourceString =}


{===============================}
{== TRzResourceStatus Methods ==}
{===============================}

constructor TRzResourceStatus.Create( AOwner: TComponent );
begin
  inherited;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
    FResourceType := rtMemory
  else
    FResourceType := rtSystem;

  FBackColor := clBtnFace;
  FBackColorStop := clBtnFace;
  FBarColor := clHighlight;
  FBarColorStop := clHotLight;
  FFreeMemory := 0;
  FFreePercent := 0;
  FGradientDirection := gdHorizontalCenter;
  FOrientation := orHorizontal;
  FNumSegments := 15;
  FShowPercent := False;
  ShowHint := True;

  FResourceStrings := TRzResourceStrings.Create;
  FResourceStrings.FResourceStatus := Self;
  FStoreResourceStrings := False;
end;


destructor TRzResourceStatus.Destroy;
begin
  FResourceStrings.Free;
  inherited;
end;


procedure TRzResourceStatus.SetBackColor( Value: TColor );
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetBackColorStop( Value: TColor );
begin
  if FBackColorStop <> Value then
  begin
    FBackColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetBarColor( Value: TColor );
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetBarColorStop( Value: TColor );
begin
  if FBarColorStop <> Value then
  begin
    FBarColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetBarStyle( Value: TBarStyle );
begin
  if FBarStyle <> Value then
  begin
    FBarStyle := Value;
    FShowPercent := False;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetDisplayStyle( Value: TDisplayStyle );
begin
  if FDisplayStyle <> Value then
  begin
    FDisplayStyle := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetNumSegments( Value: TSegmentRange );
begin
  if FNumSegments <> Value then
  begin
    FNumSegments := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.SetOrientation( Value: TOrientation );
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Invalidate;
  end;
end;


function TRzResourceStatus.GetResourceString( ResType: TResourceType ): string;
begin
  case ResType of
    rtSystem:
      Result := FResourceStrings.System;
    rtMemory:
      Result := FResourceStrings.Memory;
  end;
end;

procedure TRzResourceStatus.ResourceStringChanged;
begin
  FStoreResourceStrings := True;
  Hint := Format('%s: %d%%', [ GetResourceString( FResourceType ), FFreePercent]);
  Invalidate;
end;

procedure TRzResourceStatus.SetResourceStrings( Value: TRzResourceStrings );
begin
  FResourceStrings.Assign( Value );
end;


procedure TRzResourceStatus.SetShowPercent( Value: Boolean );
begin
  if FShowPercent <> Value then
  begin
    FShowPercent := Value;
    Invalidate;
  end;
end;


procedure TRzResourceStatus.TimerExpired;
var
  MemInfoEx: TMemoryStatusEx;
  OldPercent: Integer;
begin
  FFreeMemory := 0;
  OldPercent := FFreePercent;

  case FResourceType of
    rtSystem:
      FFreePercent := GetFreeSystemResources( gfsr_SystemResources );

    rtMemory:
    begin
      MemInfoEx.dwLength := SizeOf( MemInfoEx );
      GlobalMemoryStatusEx( MemInfoEx );
      FFreePercent := MemInfoEx.dwMemoryLoad;
      FFreeMemory := MemInfoEx.ullAvailVirtual div 1048576;  // Convert to MB (i.e. 1024 * 1024)
    end;
  end; { case }

  if FFreePercent <> OldPercent then
  begin
    Hint := Format('%s: %d%%', [ GetResourceString( FResourceType ), FFreePercent]);
    Invalidate;
  end;
  inherited;
end; {= TRzResourceStatus.TimerExpired =}


procedure TRzResourceStatus.Paint;
var
  R: TRect;
  BarColor, BarColorStop, BackColor, BackColorStop: TColor;
begin
  if FDisplayStyle = dsText then
  begin
    if FResourceType = rtMemory then
      FCaption := FloatToStrF( FFreeMemory, ffNumber, 10, 0 ) + ' MB'
    else
      FCaption := Format( '%u%%', [ FFreePercent ] );
    inherited;
  end
  else
  begin
    DrawStatusBorder;
    Canvas.Font := Font;
    R := GetCaptionRect;
    InflateRect( R, 0, -1 );

    if UsingSystemStyle then
    begin
      BarColor := FBarColor;
      BarColorStop := FBarColorStop;
      BackColor := FBackColor;
      BackColorStop := FBackColorStop;
    end
    else // VCL Styles
    begin
      BarColor := ActiveStyleSystemColor( clHighlight );
      BarColorStop := DarkerColor( BarColor, 20 );
      BackColor := clNone;
      BackColorStop := clNone;
    end;


    // Reset the Brush.Color to set clNone because this will force the Canvas
    // to reset the brush settings, which is necessary for property display
    // when using VCL Styles.
    Canvas.Brush.Color := clNone;

    case FBarStyle of
      bsTraditional:
      begin
        DrawPercentBar( Canvas, R, FOrientation, BarColor, BackColor,
                        FFreePercent, FShowPercent, True );
      end;

      bsLED:
      begin
        DrawLEDBar( Canvas, R, FOrientation, BarColor, BackColor, FNumSegments,
                    FFreePercent, False, True );
      end;

      bsGradient:
      begin
        DrawGradientPercentBar( Canvas, R, FOrientation, BarColor, BarColorStop,
                                BackColor, BackColorStop, FGradientDirection,
                                FFreePercent, FShowPercent, True );
      end;
    end;
  end;
end; {= TRzResourceStatus.Paint =}


procedure TRzResourceStatus.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the ResourceType property was published in version 2.x
  Filer.DefineProperty( 'ResourceType', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;



{==============================}
{== TRzMarqueeStatus Methods ==}
{==============================}

constructor TRzMarqueeStatus.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle - [ csOpaque ];
  FScrolling := True;
  FScrollSize := 1;
  FSteps := Width;
  FCurrentStep := 0;
  FScrollType := stRightToLeft;

  FBitmap := TBitmap.Create;

  FScrollDelay := 100;
  FTimer := TTimer.Create( Self );
  FTimer.Enabled := FScrolling;
  FTimer.OnTimer := TimerEventHandler;
  FTimer.Interval := FScrollDelay;
end;


destructor TRzMarqueeStatus.Destroy;
begin
  FBitmap.Free;
  FTimer.Free;
  inherited;
end;


procedure TRzMarqueeStatus.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
  inherited;
  UpdateSteps;
end;


procedure TRzMarqueeStatus.DrawCaption( const CaptionStr: string );
var
  R, BmpRect, SrcRect, DestRect: TRect;
  X, Y: Integer;
  ElementDetails: TThemedElementDetails;
  Bmp: TBitmap;
  StatusBar: TRzStatusBar;
  StartColor, StopColor, C: TColor;
begin
  if FScrollType = stNone then
    inherited
  else
  begin
    R := GetCaptionRect;
    FBitmap.Width := R.Right - R.Left;
    FBitmap.Height := R.Bottom - R.Top;

    FBitmap.Canvas.Font := Self.Font;
    FBitmap.Canvas.Font.Color := FCurrentColor;
    if UseThemes then
    begin
      // UseThemes is only True if Parent is a TRzStatusBar
      StatusBar := TRzStatusBar( Parent );
      Bmp := TBitmap.Create;
      try
        Bmp.Width := StatusBar.Width;
        Bmp.Height := StatusBar.Height;
        BmpRect := Rect( 0, 0, Bmp.Width, Bmp.Height );

        ElementDetails := ActiveStyleServices.GetElementDetails( tsStatusRoot );
        ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, ElementDetails, BmpRect );
        SrcRect := Rect( BoundsRect.Left + R.Left, BoundsRect.Top + R.Top,
                         BoundsRect.Left + R.Right, BoundsRect.Top + R.Bottom );
        DestRect := R;
        OffsetRect( DestRect, -R.Left, -R.Top );
        FBitmap.Canvas.CopyRect( DestRect, Bmp.Canvas, SrcRect );
      finally
        Bmp.Free;
      end;
      FBitmap.Canvas.Brush.Style := bsClear;
    end
    else
    begin
      if Transparent and ( Parent <> nil ) and ( Parent is TRzStatusBar ) then
      begin
        StatusBar := TRzStatusBar( Parent );
        FBitmap.Canvas.Brush.Style := bsClear;

        // Fill background of FBitmap.Canvas just like status bar
        Bmp := TBitmap.Create;
        try
          Bmp.Width := StatusBar.Width;
          Bmp.Height := StatusBar.Height;
          BmpRect := Rect( 0, 0, Bmp.Width, Bmp.Height );

          case StatusBar.VisualStyle of
            vsClassic, vsWinXP:
            begin
              Bmp.Canvas.Brush.Color := Color;
              Bmp.Canvas.FillRect( BmpRect );
            end;

            vsGradient:
            begin
              if StatusBar.GradientColorStyle <> gcsCustom then
              begin
                GetGradientStatusBarColors( StatusBar.GradientColorStyle,
                                            StartColor, StopColor, C );
              end
              else
              begin
                StartColor := StatusBar.GradientColorStop;
                StopColor := StatusBar.GradientColorStart;
              end;

              PaintGradient( Bmp.Canvas, BmpRect, StatusBar.GradientDirection,
                             StartColor, StopColor );
            end;
          end;

          SrcRect := Rect( BoundsRect.Left + R.Left, BoundsRect.Top + R.Top,
                           BoundsRect.Left + R.Right, BoundsRect.Top + R.Bottom );
          DestRect := R;
          OffsetRect( DestRect, -R.Left, -R.Top );
          FBitmap.Canvas.CopyRect( DestRect, Bmp.Canvas, SrcRect );
        finally
          Bmp.Free;
        end;
      end
      else
      begin
        FBitmap.Canvas.Brush.Color := FillColor;
        FBitmap.Canvas.Brush.Style := bsSolid;
        FBitmap.Canvas.Rectangle( 0, 0, FBitmap.Width + 1, FBitmap.Height + 1 );
        FBitmap.Canvas.FillRect( FBitmap.Canvas.ClipRect );
      end;
    end;

    Y := R.Top + ( R.Bottom - R.Top - FBitmap.Canvas.TextHeight( 'Pp' ) ) div 2 - BorderWidth - 1;

    if FScrollType = stRightToLeft then
      X := Width - FCurrentStep
    else
      X := - FBitmap.Canvas.TextWidth( Caption ) + FCurrentStep;

    FBitmap.Canvas.TextRect( R, X, Y, CaptionStr );
    Canvas.Draw( BorderWidth + 1, BorderWidth + 1, FBitmap );
  end;
end; {= TRzMarqueeStatus.DrawCaption =}



procedure TRzMarqueeStatus.SetScrollType( Value: TRzScrollType );
begin
  if FScrollType <> Value then
  begin
    FScrollType := Value;
    FTimer.Enabled := FScrollType <> stNone;
    Invalidate;
  end;
end;


procedure TRzMarqueeStatus.SetScrolling( Value: Boolean );
begin
  if FScrolling <> Value then
  begin
    FScrolling := Value;
    FTimer.Enabled := FScrolling;
    if not FScrolling then
      FCurrentStep := 0;
    Invalidate;
  end;
end;


procedure TRzMarqueeStatus.SetScrollSize( Value: Word );
begin
  if FScrollSize <> Value then
  begin
    if Value < 1 then
      Value := 1;
    FScrollSize := Value;
  end;
end;


procedure TRzMarqueeStatus.SetScrollDelay( Value: Word );
begin
  if FScrollDelay <> Value then
  begin
    FScrollDelay := Value;
    if FTimer <> nil then
      FTimer.Interval := FScrollDelay;
  end;
end;



procedure TRzMarqueeStatus.TimerEventHandler( Sender: TObject );
begin
  if not FTimer.Enabled or not Visible then
    Exit;

  Inc( FCurrentStep, FScrollSize );

  DrawCaption( Caption );

  if FCurrentStep >= FSteps then
  begin
    FCurrentStep := 0;
    ScrollComplete;
  end
  else
  begin
    ScrollDisplay( FCurrentStep, FSteps );
  end;
end;


procedure TRzMarqueeStatus.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  UpdateSteps;
  Invalidate;
end;


procedure TRzMarqueeStatus.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  UpdateSteps;
  Invalidate;
end;


procedure TRzMarqueeStatus.UpdateSteps;
begin
  if FBitmap <> nil then
  begin
    FBitmap.Canvas.Font := Self.Font;
    if FScrollSize <> 0 then
      FSteps := FBitmap.Canvas.TextWidth( Caption ) + Width;
  end;
end;


procedure TRzMarqueeStatus.ScrollDisplay( CurrentStep, TotalSteps: Integer );
begin
  if Assigned( FOnScrollDisplay ) then
    FOnScrollDisplay( Self, CurrentStep, TotalSteps );
end;


procedure TRzMarqueeStatus.ScrollComplete;
begin
  if Assigned( FOnScrollComplete ) then
    FOnScrollComplete( Self );
end;


{==================================}
{== TRzCustomFieldStatus Methods ==}
{==================================}

constructor TRzCustomFieldStatus.Create( AOwner: TComponent );
begin
  inherited;
  FFieldLabel := '';
  FFieldLabelColor := clHighlight;
  {&RCI}
end;


procedure TRzCustomFieldStatus.SetFieldLabel( const Value: string );
begin
  if Value <> FFieldLabel then
  begin
    FFieldLabel := Value;
    Invalidate;
  end;
end;


procedure TRzCustomFieldStatus.SetFieldLabelColor( Value: TColor );
begin
  if FFieldLabelColor <> Value then
  begin
    FFieldLabelColor := Value;
    Invalidate;
  end;
end;


function TRzCustomFieldStatus.GetMinWidth: Integer;
begin
  Canvas.Font := Self.Font;
  Result := inherited GetMinWidth + Canvas.TextWidth( FFieldLabel ) + 2 * CaptionOffset;
end;


function TRzCustomFieldStatus.GetCaptionRect: TRect;
var
  W: Integer;
begin
  Canvas.Font := Font;
  Result := inherited GetCaptionRect;

  if FFieldLabel <> '' then
  begin
    W := Canvas.TextWidth( FFieldLabel );

    if not UseRightToLeftAlignment then
     Result.Left := Result.Left + W + 2 * CaptionOffset
    else
     Result.Right := Result.Right - W - 2 * CaptionOffset;
  end;
end;


procedure TRzCustomFieldStatus.DrawCaption( const CaptionStr: string );
var
  X, Y, W: Integer;
  R: TRect;
begin
  inherited;

  if FFieldLabel <> '' then
  begin
    Canvas.Font := Font;
    R := inherited GetCaptionRect;

    W := Canvas.TextWidth( FFieldLabel );

    if not UseRightToLeftAlignment then
      R.Right := R.Left + W + CaptionOffset + 2
    else
      R.Left := R.Right - W - CaptionOffset - 2;


    if not Transparent then
    begin
      // Set brush color so that old text in caption area gets erased
      Canvas.Brush.Color := FillColor;
      Canvas.Brush.Style := bsSolid;
    end
    else
      Canvas.Brush.Style := bsClear;;

    Y := R.Top + ( R.Bottom - R.Top - Canvas.TextHeight( 'Pp' ) ) div 2;

    if not UseRightToLeftAlignment then
    begin
      X := R.Left + CaptionOffset + 1;
      SetTextAlign( Canvas.Handle, SetTextAlignments[ taLeftJustify ] );
    end
    else
    begin
      X := R.Right - CaptionOffset - 1;
      SetTextAlign( Canvas.Handle, SetTextAlignments[ taRightJustify ] );
    end;

    if UsingSystemStyle then
    begin
      if Enabled then
        Canvas.Font.Color := FFieldLabelColor
      else
        Canvas.Font.Color := clGrayText;
    end
    else // VCL Styles
    begin
      // Disabled font color provides best appearance with variety of styles.
      // Using highlight color or button color results in label being too light
      // with the light colored styles (e.g. Aqua Light Slate)
      Canvas.Font.Color := ActiveStyleFontColor( sfStatusPanelTextDisabled );
    end;
    Canvas.TextRect( R, X, Y, FFieldLabel );
  end;
end; {= TRzFieldStatus.DrawCaption =}


{============================}
{== TRzVersionInfo Methods ==}
{============================}

constructor TRzVersionInfo.Create( AOwner: TComponent );
begin
  inherited;

  FFilePath := '';
  FVersionInfo := TStringList.Create;
  if not ( csDesigning in ComponentState ) then
    GetVersionInfo;
  {&RCI}
end;


destructor TRzVersionInfo.Destroy;
begin
  FVersionInfo.Free;

  if FStatusList <> nil then
  begin
    FStatusList.Free;
    FStatusList := nil;
  end;

  inherited;
end;


procedure TRzVersionInfo.Loaded;
begin
  inherited;
  if not ( csDesigning in ComponentState ) then
  begin
    GetVersionInfo;
    UpdateStatusControls;
  end;
end;


procedure TRzVersionInfo.GetVersionInfo;
const
  SNotAvailable = 'Value Not Available';
var
  LanguageID: string;
  CodePage: string;
  TranslationLength: Cardinal;
  TranslationTable: Pointer;
  InfoSize, Temp, Len: DWord;
  InfoBuf: Pointer;
  CompanyName, FileDescription, FileVersion, InternalName, LegalCopyright: string;
  LegalTradeMarks, OriginalFilename, ProductName, ProductVersion, Comments: string;
  MajorVersion, MinorVersion, Release, Build: Word;
  DebugBuild, PreRelease, PrivateBuild, SpecialBuild: Boolean;
  Value: PChar;
  FileInfo: PVSFixedFileInfo;
  LookupString: string;
  PathStz: array[ 0..MAX_PATH ] of Char;
begin
  {&RV}
  // Get File Version Information
  if FFilePath = '' then
  begin
    GetModuleFileName( HInstance, PathStz, SizeOf( PathStz ) );
    FFilePath := PathStz;
  end;

  MajorVersion := 0;
  MinorVersion := 0;
  Release := 0;
  Build := 0;

  DebugBuild := False;
  PreRelease := False;
  PrivateBuild := False;
  SpecialBuild := False;
  
  InfoSize := GetFileVersionInfoSize( PChar( FFilePath ), Temp );
  FVersionInfoAvailable := InfoSize > 0;
  if FVersionInfoAvailable then
  begin
    InfoBuf := AllocMem( InfoSize );
    try
      GetFileVersionInfo( PChar( FFilePath ), 0, InfoSize, InfoBuf );


      if VerQueryValue( InfoBuf, '\VarFileInfo\Translation', TranslationTable, TranslationLength ) then
      begin
        CodePage := Format( '%.4x', [ HiWord( PLongInt( TranslationTable )^ ) ] );
        LanguageID := Format( '%.4x', [ LoWord( PLongInt( TranslationTable )^ ) ] );
      end;

      LookupString := 'StringFileInfo\' + LanguageID + CodePage + '\';

      if VerQueryValue( InfoBuf, PChar( LookupString + 'CompanyName' ), Pointer( Value ), Len ) then
        CompanyName := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'FileDescription' ), Pointer( Value ), Len ) then
        FileDescription := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'FileVersion' ), Pointer( Value ), Len ) then
        FileVersion := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'InternalName' ), Pointer( Value ), Len ) then
        InternalName := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'LegalCopyright' ), Pointer( Value ), Len ) then
        LegalCopyright := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'LegalTrademarks' ), Pointer( Value ), Len ) then
        LegalTradeMarks := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'OriginalFilename' ), Pointer( Value ), Len ) then
        OriginalFilename := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'ProductName' ), Pointer( Value ), Len ) then
        ProductName := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'ProductVersion' ), Pointer( Value ), Len ) then
        ProductVersion := Value;
      if VerQueryValue( InfoBuf, PChar( LookupString + 'Comments' ), Pointer( Value ), Len ) then
        Comments := Value;

      // Get File Info: Version Numbers and Module Attributes
      if VerQueryValue( InfoBuf, '\', Pointer( FileInfo ), Len ) then
      begin
        MajorVersion := FileInfo^.dwFileVersionMS shr 16;
        MinorVersion := FileInfo^.dwFileVersionMS and $FFFF;
        Release      := FileInfo^.dwFileVersionLS shr 16;
        Build        := FileInfo^.dwFileVersionLS and $FFFF;

        DebugBuild   := (FileInfo^.dwFileFlags and VS_FF_DEBUG) <> 0;
        PreRelease   := (FileInfo^.dwFileFlags and VS_FF_PRERELEASE) <> 0;
        PrivateBuild := (FileInfo^.dwFileFlags and VS_FF_PRIVATEBUILD) <> 0;
        SpecialBuild := (FileInfo^.dwFileFlags and VS_FF_SPECIALBUILD) <> 0;
      end;

    finally
      FreeMem( InfoBuf, InfoSize );
    end;
  end
  else
  begin
    CompanyName := SNotAvailable;
    FileDescription := SNotAvailable;
    FileVersion := SNotAvailable;
    InternalName := SNotAvailable;
    LegalCopyright := SNotAvailable;
    LegalTrademarks := SNotAvailable;
    OriginalFilename := SNotAvailable;
    ProductName := SNotAvailable;
    ProductVersion := SNotAvailable;
    Comments := SNotAvailable;
  end;

  FVersionInfo.Clear;
  FVersionInfo.Add( CompanyName );
  FVersionInfo.Add( FileDescription );
  FVersionInfo.Add( FileVersion );
  FVersionInfo.Add( InternalName );
  FVersionInfo.Add( LegalCopyright );
  FVersionInfo.Add( LegalTrademarks );
  FVersionInfo.Add( OriginalFilename );
  FVersionInfo.Add( ProductName );
  FVersionInfo.Add( ProductVersion );
  FVersionInfo.Add( Comments );

  FVersionNumbers[ 1 ] := MajorVersion;
  FVersionNumbers[ 2 ] := MinorVersion;
  FVersionNumbers[ 3 ] := Release;
  FVersionNumbers[ 4 ] := Build;

  FModuleAttributes[ 1 ] := DebugBuild;
  FModuleAttributes[ 2 ] := PreRelease;
  FModuleAttributes[ 3 ] := PrivateBuild;
  FModuleAttributes[ 4 ] := SpecialBuild;
end;


function TRzVersionInfo.GetCustomKeyValue( const Key: string ): string;
const
  SNotAvailable = 'Value Not Available';
var
  LanguageID: string;
  CodePage: string;
  TranslationLength: Cardinal;
  TranslationTable: Pointer;
  InfoSize, Temp, Len: DWord;
  InfoBuf: Pointer;
  Value: PChar;
  LookupString: string;
  PathStz: array[ 0..MAX_PATH ] of Char;
begin
  // Get File Version Information
  if FFilePath = '' then
  begin
    GetModuleFileName( HInstance, PathStz, SizeOf( PathStz ) );
    FFilePath := PathStz;
  end;
  InfoSize := GetFileVersionInfoSize( PChar( FFilePath ), Temp );
  FVersionInfoAvailable := InfoSize > 0;
  if FVersionInfoAvailable then
  begin
    InfoBuf := AllocMem( InfoSize );
    try
      GetFileVersionInfo( PChar( FFilePath ), 0, InfoSize, InfoBuf );


      if VerQueryValue( InfoBuf, '\VarFileInfo\Translation', TranslationTable, TranslationLength ) then
      begin
        CodePage := Format( '%.4x', [ HiWord( PLongInt( TranslationTable )^ ) ] );
        LanguageID := Format( '%.4x', [ LoWord( PLongInt( TranslationTable )^ ) ] );
      end;

      LookupString := 'StringFileInfo\' + LanguageID + CodePage + '\';

      if VerQueryValue( InfoBuf, PChar( LookupString + Key ), Pointer( Value ), Len ) then
        Result := Value
      else
        Result := SNotAvailable;
    finally
      FreeMem( InfoBuf, InfoSize );
    end;
  end
  else
    Result := SNotAvailable;
end; {= TRzVersionInfo.GetCustomKeyValue =}


function TRzVersionInfo.GetField( Index: Integer ): string;
begin
  Result := FVersionInfo[ Index ]
end;

function TRzVersionInfo.GetVerField( Index: TRzVersionInfoField ): string;
begin
  Result := GetField( Ord( Index ) );
end;


function TRzVersionInfo.GetVersionNumber( Index: Integer ): Word;
begin
  Result := FVersionNumbers[ Index ];
end;


function TRzVersionInfo.GetModuleAttribute( Index: Integer ): Boolean;
begin
  Result := FModuleAttributes[ Index ];
end;


function TRzVersionInfo.GetFileDateTime: TDateTime;
var
  DT: TDateTime;
begin
  if FileAge( ParamStr( 0 ), DT ) then
    Result := DT
  else
    Result := 0;
end;


procedure TRzVersionInfo.UpdateStatusControls;
var
  I: Integer;
begin
  if FStatusList <> nil then
  begin
    for I := 0 to FStatusList.Count - 1 do
      TRzVersionInfoStatus( FStatusList[ I ] ).UpdateCaption;
  end;
end;


procedure TRzVersionInfo.AddStatus( S: TRzVersionInfoStatus );
begin
  if not Assigned( FStatusList ) then
    FStatusList := TList.Create;

  if FStatusList.IndexOf( S ) < 0 then
  begin
    FStatusList.Add( S );
  end;
end;


procedure TRzVersionInfo.RemoveStatus( S: TRzVersionInfoStatus );
begin
  if FStatusList <> nil then
  begin
    FStatusList.Remove( S );
    if FStatusList.Count = 0 then
    begin
      FStatusList.Free;
      FStatusList := nil;
    end;
  end;
end;


procedure TRzVersionInfo.SetFilePath( const Value: string );
begin
  if FFilePath <> Value then
  begin
    FFilePath := Value;
    if not ( csDesigning in ComponentState ) and not ( csLoading in ComponentState ) then
    begin
      GetVersionInfo;
      UpdateStatusControls;
    end;
  end;
end;



{==================================}
{== TRzVersionInfoStatus Methods ==}
{==================================}

constructor TRzVersionInfoStatus.Create( AOwner: TComponent );
begin
  inherited;
  FField := vifProductVersion;
  if csDesigning in ComponentState then
    UpdateCaption;
  {&RCI}
end;


procedure TRzVersionInfoStatus.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FVersionInfo ) then
    SetVersionInfo( nil );
end;


procedure TRzVersionInfoStatus.UpdateCaption;
begin
  if csDesigning in ComponentState then
  begin
    case FField of
      vifCompanyName:       Caption := '{CompanyName}';
      vifFileDescription:   Caption := '{FileDescription}';
      vifFileVersion:       Caption := '{FileVersion}';
      vifInternalName:      Caption := '{InternalName}';
      vifCopyright:         Caption := '{Copyright}';
      vifTrademarks:        Caption := '{Trademarks}';
      vifOriginalFilename:  Caption := '{OriginalFilename}';
      vifProductName:       Caption := '{ProductName}';
      vifProductVersion:    Caption := '{ProductVersion}';
      vifComments:          Caption := '{Comments}';
    end;
  end
  else if ( FVersionInfo <> nil ) then
    Caption := FVersionInfo[ FField ];
end;


procedure TRzVersionInfoStatus.SetVersionInfo( Value: TRzVersionInfo );
begin
  if FVersionInfo <> nil then
    FVersionInfo.RemoveStatus( Self );
  FVersionInfo := Value;
  if Value <> nil then
  begin
    Value.AddStatus( Self );
    Value.FreeNotification( Self );
    UpdateCaption;
  end;
end;


procedure TRzVersionInfoStatus.SetField( Value: TRzVersionInfoField );
begin
  FField := Value;
  UpdateCaption;
end;


{===============================}
{== TRzProgressStatus Methods ==}
{===============================}

constructor TRzProgressStatus.Create( AOwner: TComponent );
begin
  inherited;

  FBackColor := clBtnFace;
  FBackColorStop := clBtnFace;
  FBarColor := clHighlight;
  FBarColorStop := clHotLight;
  FGradientDirection := gdHorizontalCenter;
  FNumSegments := 15;
  FPercent := 0;
  FShowPercent := False;
  FShowParts := False;
  ShowHint := True;
end;


procedure TRzProgressStatus.SetBackColor( Value: TColor );
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetBackColorStop( Value: TColor );
begin
  if FBackColorStop <> Value then
  begin
    FBackColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetBarColor( Value: TColor );
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetBarColorStop( Value: TColor );
begin
  if FBarColorStop <> Value then
  begin
    FBarColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetBarStyle( Value: TBarStyle );
begin
  if FBarStyle <> Value then
  begin
    FBarStyle := Value;
    FShowPercent := False;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetNumSegments( Value: TSegmentRange );
begin
  if FNumSegments <> Value then
  begin
    FNumSegments := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetPartsComplete( Value: TUnsignedLongint );
begin
  if FPartsComplete <> Value then
  begin
    if Value > FTotalParts then
      FPartsComplete := FTotalParts
    else
      FPartsComplete := Value;

    // Setting the Percent property causes the SetPercent method to get called
    // which will force a repaint
    if not ( csLoading in ComponentState ) and ( FTotalParts <> 0 ) then
      Percent := Round( FPartsComplete / FTotalParts * 100 );
  end;
end;


procedure TRzProgressStatus.SetPercent( Value: Word );
begin
  {&RV}
  if FPercent <> Value then
  begin
    FPercent := Value;

    // Call Repaint rather than Repaint so that the view of the component
    // does not get erased.  This prevents flicker.
    Repaint;
  end;
end;


procedure TRzProgressStatus.SetShowPercent( Value: Boolean );
begin
  if FShowPercent <> Value then
  begin
    FShowPercent := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetShowParts( Value: Boolean );
begin
  if FShowParts <> Value then
  begin
    FShowParts := Value;
    Invalidate;
  end;
end;


procedure TRzProgressStatus.SetTotalParts( Value: TUnsignedLongint );
begin
  if FTotalParts <> Value then
  begin
    FTotalParts := Value;
    if not ( csLoading in ComponentState ) then
    begin
      FPartsComplete := 0;
      Percent := 0;
    end;
  end;
end;


procedure TRzProgressStatus.Paint;
var
  R: TRect;
  BarColor, BarColorStop, BackColor, BackColorStop: TColor;
begin
  DrawStatusBorder;
  Canvas.Font := Font;
  R := GetCaptionRect;
  InflateRect( R, 0, -1 );

  if UsingSystemStyle then
  begin
    BarColor := FBarColor;
    BarColorStop := FBarColorStop;
    BackColor := FBackColor;
    BackColorStop := FBackColorStop;
  end
  else // VCL Styles
  begin
    BarColor := ActiveStyleSystemColor( clHighlight );
    BarColorStop := DarkerColor( BarColor, 20 );
    BackColor := clNone;
    BackColorStop := clNone;
  end;

  // Reset the Brush.Color to set clNone because this will force the Canvas
  // to reset the brush settings, which is necessary for property display
  // when using VCL Styles.
  Canvas.Brush.Color := clNone;

  case FBarStyle of
    bsTraditional:
    begin
      DrawPercentBar( Canvas, R, orHorizontal, BarColor, BackColor,
                      FPercent, FShowPercent, True, FShowParts,
                      FPartsComplete, FTotalParts );
    end;

    bsLED:
    begin
      DrawLEDBar( Canvas, R, orHorizontal, BarColor, BackColor, FNumSegments,
                  FPercent, False, True );
    end;

    bsGradient:
    begin
      DrawGradientPercentBar( Canvas, R, orHorizontal, BarColor, BarColorStop,
                              BackColor, BackColorStop, FGradientDirection,
                              FPercent, FShowPercent, True, FShowParts,
                              FPartsComplete, FTotalParts );
    end;
  end;
end; {= TRzProgressStatus.Paint =}


procedure TRzProgressStatus.IncPartsByOne;
begin
  IncParts( 1 );
end;


procedure TRzProgressStatus.IncParts( N: TUnsignedLongint );
begin
  PartsComplete := PartsComplete + N;
end;


{&RUIF}
end.

