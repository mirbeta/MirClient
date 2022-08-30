{===============================================================================
  RzSplit Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzSplitter
    Container component with two panes.  The size of each pane is controlled by
    the splitter bar.

  TRzSizePanel
    Enhanced panel that can be resized at runtime using a sizing bar.


  Modification History
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed display issue in TRzSizePanel where the HotSpot button would not
      get drawn correctly when the SizePanel was aligned alTop or alBottom.
    * Fixed issue in TRzSizePanel that could result in a range check error
      under certain situations.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzSplitter and TRzSizePanel to fully
      support VCL Styles introduced in RAD Studio XE2. Including automatic color
      adjustment of HotSpot bar when VCL Styles are used.
    * Made necessary modifications to TRzSplitter to support 64-bit development.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Surfaced the HotSpotRect as a public property for both TRzSplitter and
      TRzSizePanel.
    * Fixed issue where the TRzSizePanel.MarginMin property would get reset to
      0 if the sum of the MarginMin and MarginMax were greater than 302, which
      is the default client width of a TForm. The true client size of the form
      is not established until after the child controls are read from the DFM
      file. As a result, the TRzSizePanel now delays setting the MarginMin
      property until the true size of the form has been established.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed issue where a closed hot spot in a TRzSizePanel would not appear
      active the first time the user moved the mouse over the hot spot. This
      particular issue only occurred in BDS 2006 and RAD Studio 2007 because of
      the changes made to the VCL regarding cm_MouseEnter and cm_MouseLeave.
    * Fixed issue where the HotSpot area would not remain highlighted after
      dragging the splitter bar in a TRzSplitter or the sizing bar in a
      TRzSizePanel (and the mouse was still positioned over the HotSpot area).
    * Fixed issue where splitter bar mask in TRzSplitter would not get drawn in
      the correct location if the splitter's parent was offset from the edge of
      the form *and* the user was running under Vista *and* using Delphi 7 or
      earlier. Actual cause of problem was in the TControl.ClientToParent
      method.
  ------------------------------------------------------------------------------
  4.3.1  (21 Sep 2007)
    * Fixed issue where resizing a TRzSplitter that had its HotSpotVisible
      property set to True and FullRepaint set to False would result in display
      artifacts on the splitter bar.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Fixed display issue in TRzSplitter where the splitter mask would appear in
      the wrong location if the control was aligned to alRight, alBottom, or
      alNone.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Fixed display issue in TRzSplitter and TRzSizePanel that caused slow
      updating of sizing bar when running under Windows Vista with Aero Glass
      interface.
    * Fixed issue where hot spot in TRzSizePanel would not appear active the
      first time the user moved the mouse over the hot spot.
    * Fixed issue where calling CloseHotSpot would not honor the
      HotSpotIgnoreMargins property value.
  ------------------------------------------------------------------------------
  4.1.1  (12 Jan 2007)
    * Fixed issue in TRzSplitter where splitter bar would not get repositioned
      if closed to the maximium side and the splitter control was resized
      larger.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Surfaced GradientColorStart, GradientColorStop, and GradientDirection
      properties in TRzSizePanel.
    * Added HotSpotColor, HotSpotDotColor, HotSpotFrameColor properties to
      TRzSplitter and TRzSizePanel. These properties allow the appearance of the
      HotSpot area of the splitter bar (or size bar) to be customized.
    * Surfaced Padding property (introduced in Borland Developer Studio 2006)
      in LowerRight and UpperLeft panes of TRzSplitter, and TRzSizePanel.
    * Both the TRzSplitter and TRzSizePanel inherit the new VisualStyle and
      GradientColorStyle properties that have been added to TRzCustomPanel. 
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Remove UseGradients code in TRzSplitter and TRzSizePanel. This property
      has been moved up to TRzCustomPanel.
    * Fixed problem where specifying BorderInner/BorderOuter in TRzSizePanel to
      something other than fsNone caused the HotSpot button to disappear.
    * Added new Side property to TRzSizePanel that is used when the Align
      property is set to alNone or alCustom to indicate which side the panel
      will close to.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem where panes of TRzSplitter would not pickup ParentColor
      changes.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * When RealTimeDrag is False, pressing the Escape key will now cancel the
      resizing operation for both the TRzSplitter and the TRzSizePanel.
    * Surfaced Transparent property for TRzSplitter.
    * Added FlatColorAdjustment to TRzSplitter, and TRzSplitter.LowerRight and
      TRzSplitter.UpperLeft.
    * Added new ssGroupBar splitter bar style. This style is useful when the
      TRzSizePanel or a pane of the TRzSplitter contains a TRzGroupBar in
      gbsCategoryView mode. The splitter bar is drawn to match the background
      shading of the group bar. In order to support this, the following
      properties were also added: GradientColorAdjustment, GradientPath,
      ThemeAware, and UseGradients.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem where sizing bar was not displayed correctly when
      DoubleBuffered was set to True in a TRzSizePanel.
    * Fixed problem where nested splitters were causing form to be "modified" on
      load in IDE.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzSplitter >>
    * Fixed problem where fixed pane position was not restored properly when
      FixedPane was set to fpLowerRight, the HotSpot was closed, and the
      splitter was resized.
    * The HotSpot now works when the LockBar property is True. Therefore, the
      user can quickly close a pane of the splitter but is not able to resize
      the pane.
    * Fixed problem where the right and bottom sides of the splitter's outside
      border were not drawn when DoubleBuffered was set to True.
    * Fixed property where an Access Violation occurred in the splitter when
      ExpressBars components (from Developer Express) were used on the same form
      as the splitter.
    * The HotSpotDirection property can now be set to hsdBoth, which splits the
      HotSpot button into 2 areas: the upper-left area causes the splitter to go
      to the minimum side; the lower-right area causes the splitter to go to the
      maximum side.

    << TRzSizePanel >>
    * The HotSpot now works when the LockBar property is True. Therefore, the
      user can quickly close the size panel but is not able to resize it.
===============================================================================}

{$I RzComps.inc}

unit RzSplit;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  SysUtils,
  Windows,
  Messages,
  Menus,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  RzPanel,
  RzCommon;

type
  TRzSplitter = class;

  TRzSplitterPane = class( TRzCustomPanel )
  private
    FSplitter: TRzSplitter;

    { Message Handling Methods }
    procedure WMNCHitTest( var Msg: TWMNCHitTest ); message wm_NCHitTest;
  protected
    procedure Paint; override;
    procedure ReadState( Reader: TReader ); override;

    { Event Dispatch Methods }
    procedure DoEnter; override;
    procedure DoExit; override;

    { Property Access Methods }
    function GetVisible: Boolean;
    procedure SetVisible( Value: Boolean );
  public
    constructor Create( AOwner: TComponent ); override;
  published
    { Property Declarations }
    property Visible: Boolean
      read GetVisible
      write SetVisible
      default True;

    { Inherited Properties & Events }
    property Color;
    property BorderColor;
    property BorderOuter default fsNone;
    property BorderInner;
    property BorderWidth;
    property FlatColor;
    property FlatColorAdjustment;
    property TabStop default False;
    property ShowDockClientCaptions;
    property Padding;

    property OnContextPopup;
  end;


  TRzPaneData = class( TPersistent )
  private
    FPane: TRzSplitterPane;
  protected
    { Property Access Methods }
    function GetBorderColor: TColor;
    procedure SetBorderColor( Value: TColor );
    function GetBorderInner: TFrameStyleEx;
    procedure SetBorderInner( Value: TFrameStyleEx );
    function GetBorderOuter: TFrameStyleEx;
    procedure SetBorderOuter( Value: TFrameStyleEx );
    function GetBorderWidth: TBorderWidth;
    procedure SetBorderWidth( Value: TBorderWidth );
    function GetColor: TColor;
    procedure SetColor( Value: TColor );
    function GetDockSite: Boolean;
    procedure SetDockSite( Value: Boolean );
    function GetFlatColor: TColor;
    procedure SetFlatColor( Value: TColor );
    function GetFlatColorAdjustment: Integer;
    procedure SetFlatColorAdjustment( Value: Integer );
    function GetShowDockClientCaptions: Boolean;
    procedure SetShowDockClientCaptions( Value: Boolean );
    function GetVisible: Boolean;
    procedure SetVisible( Value: Boolean );
    function GetPadding: TPadding;
    procedure SetPadding( Value: TPadding );
  public
    { Property Declarations }
    property Pane: TRzSplitterPane
      read FPane;
  published
    { Property Declarations }
    property BorderColor: TColor
      read GetBorderColor
      write SetBorderColor
      default clBtnFace;

    property BorderInner: TFrameStyleEx
      read GetBorderInner
      write SetBorderInner
      default fsNone;

    property BorderOuter: TFrameStyleEx
      read GetBorderOuter
      write SetBorderOuter
      default fsNone;

    property BorderWidth: TBorderWidth
      read GetBorderWidth
      write SetBorderWidth
      default 0;

    property Color: TColor
      read GetColor
      write SetColor
      default clBtnFace;

    property FlatColor: TColor
      read GetFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read GetFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property DockSite: Boolean
      read GetDockSite
      write SetDockSite
      default False;

    property ShowDockClientCaptions: Boolean
      read GetShowDockClientCaptions
      write SetShowDockClientCaptions
      default True;

    property Visible: Boolean
      read GetVisible
      write SetVisible
      default True;

    property Padding: TPadding
      read GetPadding
      write SetPadding;
  end;


  TSelectedPane = ( spUpperLeft, spLowerRight );
  TSplitterStyle = ( ssStandard, ssGroove, ssBump, ssGroupBar );
  TFixedPane = ( fpUpperLeft, fpLowerRight );
  THotSpotDirection = ( hsdMin, hsdMax, hsdBoth );
  THotSpotPosition = ( hspOpen, hspClosed );

  TRzSplitter = class( TRzCustomPanel )
  private
    FOrientation: TOrientation;
    FPaneData: array[ 1..2 ] of TRzPaneData;
    FPanes: array[ 1..2 ] of TRzSplitterPane;
    FPanesControlList: array[ 1..2 ] of TStringList;
    FSelectedPane: TSelectedPane;

    FPosition: Integer;
    FMarginMin: Integer;
    FMarginMax: Integer;

    FUsePercent: Boolean;
    FPercent: Integer;
    FPercentMin: Integer;
    FPercentMax: Integer;

    FFixedPane: TFixedPane;
    FFixedSize: Integer;

    FResizing: Boolean;
    FSplitterWidth: Word;
    FSplitterStyle: TSplitterStyle;
    FRealTimeDrag: Boolean;

    FLockBar: Boolean;
    FBarRect: TRect;
    FOrigPos: Integer;
    FHorzCursor: HCursor;
    FHorzCursorHotSpot: HCursor;
    FVertCursor: HCursor;
    FVertCursorHotSpot: HCursor;
    FSliding: Boolean;
    FCenterOffset: Integer;
    FManualPosition: Integer;
    FLastPos: Integer;
    FBrush: TBrush;
    FPrevBrush: HBrush;
    FMaskDC: HDC;

    FOutsideHotSpot: Boolean;
    FHotSpotVisible: Boolean;
    FHotSpotHighlight: TColor;
    FHotSpotColor: TColor;
    FHotSpotDotColor: TColor;
    FHotSpotFrameColor: TColor;
    FHotSpotRect: TRect;
    FHotSpotDirection: THotSpotDirection;
    FHotSpotClosed: Boolean;
    FHotSpotClosedToMin: Boolean;
    FHotSpotClosedToMax: Boolean;
    FHotSpotting: Boolean;
    FHotSpotPosition: Integer;
    FHotSpotIgnoreMargins: Boolean;
    FHotSpotSizePercent: Integer;
    FRefreshHotSpot: Boolean;

    FGradientColorAdjustment: Integer;
    FGradientPath: TRzGroupBarGradientPath;

    FOnChange: TNotifyEvent;
    FOnChanging: TPositionChangingEvent;
    FOnHotSpotClick: TNotifyEvent;

    FOnULGetSiteInfo: TGetSiteInfoEvent;
    FOnULDockOver: TDockOverEvent;
    FOnULDockDrop: TDockDropEvent;
    FOnULUnDock: TUnDockEvent;
    FOnLRGetSiteInfo: TGetSiteInfoEvent;
    FOnLRDockOver: TDockOverEvent;
    FOnLRDockDrop: TDockDropEvent;
    FOnLRUnDock: TUnDockEvent;

    procedure ReadBarSize( Reader: TReader );
    procedure WriteBarSize( Writer: TWriter );
    procedure ReadHotSpotClosed( Reader: TReader );
    procedure WriteHotSpotClosed( Writer: TWriter );
    procedure ReadHotSpotClosedToMin( Reader: TReader );
    procedure WriteHotSpotClosedToMin( Writer: TWriter );
    procedure ReadHotSpotClosedToMax( Reader: TReader );
    procedure WriteHotSpotClosedToMax( Writer: TWriter );
    procedure ReadHotSpotPosition( Reader: TReader );
    procedure WriteHotSpotPosition( Writer: TWriter );
    procedure ReadULControls( Reader: TReader );
    procedure WriteULControls( Writer: TWriter );
    procedure ReadLRControls( Reader: TReader );
    procedure WriteLRControls( Writer: TWriter );

    procedure AllocateMaskDC;
    procedure ReleaseMaskDC;


    { Internal Event Handlers }
    procedure ULGetSiteInfoHandler( Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
                                    var CanDock: Boolean );
    procedure ULDockOverHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState;
                                 var Accept: Boolean );
    procedure ULDockDropHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer );
    procedure ULUnDockHandler( Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean );

    procedure LRGetSiteInfoHandler( Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
                                    var CanDock: Boolean );
    procedure LRDockOverHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState;
                                 var Accept: Boolean );
    procedure LRDockDropHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer );
    procedure LRUnDockHandler( Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean );

    { Message Handling Methods }
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message cm_DesignHitTest;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
    procedure WMShowWindow( var Msg: TWMShowWindow ); message wm_ShowWindow;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;

    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;

    function GetClientRect: TRect; override;
    procedure ShowControl( AControl: TControl ); override;

    procedure Resize; override;
    procedure UpdateFixedSize; virtual;
    function GetBorderOffset: Integer; virtual;
    procedure DrawMask( NewPos: Integer ); virtual;
    procedure UpdateHotSpotRect; virtual;
    procedure UpdateHotSpotHighlight; virtual;
    procedure DrawHotSpot( Highlight: Boolean; P: TPoint ); virtual;
    procedure DrawSplitterBar; virtual;
    procedure Paint; override;

    procedure CheckPosition( var Value: Integer ); virtual;
    procedure UpdateManualPosition( P: Integer ); virtual;

    { Event Dispatch Methods }
    procedure Change; dynamic;
    function CanChange( NewPos: Integer ): Boolean; dynamic;

    procedure MouseDownSetFocus; virtual;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

    procedure HotSpotClick; dynamic;

    { Property Access Methods }
    procedure SetOrientation( Value: TOrientation ); virtual;
    procedure SetSelectedPane( Value: TSelectedPane ); virtual;
    function GetSelectedPaneControl: TRzSplitterPane; virtual;
    procedure SetFixedPane( Value: TFixedPane ); virtual;
    function GetPaneData( Index: Integer ): TRzPaneData; virtual;
    procedure SetPaneData( Index: Integer; Value: TRzPaneData ); virtual;
    procedure SetPosition( Value: Integer ); virtual;
    procedure SetLockedPosition( Value: Integer ); virtual;
    procedure SetMarginMax( Value: Integer ); virtual;
    procedure SetMarginMin( Value: Integer ); virtual;
    function GetParentColor: Boolean; virtual;
    procedure SetParentColor( Value: Boolean ); virtual;
    procedure SetPercent( Value: Integer ); virtual;
    procedure SetUsePercent( Value: Boolean ); virtual;
    procedure SetPercentMax( Value: Integer ); virtual;
    procedure SetPercentMin( Value: Integer ); virtual;
    procedure SetSplitterStyle( Value: TSplitterStyle ); virtual;
    procedure SetSplitterWidth( Value: Word ); virtual;
    procedure SetTransparent( Value: Boolean ); override;

    procedure SetGradientColorAdjustment( Value: Integer ); virtual;
    procedure SetGradientPath( Value: TRzGroupBarGradientPath ); virtual;

    function GetBorderWidth: TBorderWidth;
    procedure SetBorderWidth( Value: TBorderWidth );
    procedure SetBorderInner( Value: TFrameStyleEx ); override;
    procedure SetBorderOuter( Value: TFrameStyleEx ); override;
    procedure SetBorderSides( Value: TSides ); override;

    procedure SetHotSpotVisible( Value: Boolean ); virtual;
    procedure SetHotSpotColor( Value: TColor );
    procedure SetHotSpotDotColor( Value: TColor );
    procedure SetHotSpotFrameColor( Value: TColor );
    procedure SetHotSpotHighlight( Value: TColor ); virtual;
    procedure SetHotSpotDirection( Value: THotSpotDirection ); virtual;
    procedure SetHotSpotSizePercent( Value: Integer ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure GetChildren( Proc: TGetChildProc; Root: TComponent); override;

    procedure ResizePanes; virtual;
    procedure ResetHotSpot; virtual;
    procedure RestoreHotSpot;
    procedure CloseHotSpot; overload;
    procedure CloseHotSpot( ToMin: Boolean ); overload;

    property HotSpotClosed: Boolean
      read FHotSpotClosed;

    property HotSpotClosedToMin: Boolean
      read FHotSpotClosedToMin;

    property HotSpotClosedToMax: Boolean
      read FHotSpotClosedToMax;

    property HotSpotPosition: Integer
      read FHotSpotPosition;

    property HotSpotRect: TRect
      read FHotSpotRect;

    property LockedPosition: Integer
      read FPosition
      write SetLockedPosition;

    property SelectedPaneControl: TRzSplitterPane
      read GetSelectedPaneControl;
  published
    { Property Declarations }
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property BorderWidth: TBorderWidth
      read GetBorderWidth
      write SetBorderWidth
      default 0;

    property GradientColorAdjustment: Integer
      read FGradientColorAdjustment
      write SetGradientColorAdjustment
      default 30;

    property FixedPane: TFixedPane
      read FFixedPane
      write SetFixedPane
      default fpUpperLeft;

    property GradientPath: TRzGroupBarGradientPath
      read FGradientPath
      write SetGradientPath
      default gpTopToBottom;

    property LockBar: Boolean
      read FLockBar
      write FLockBar
      default False;

    property MarginMax: Integer
      read FMarginMax
      write SetMarginMax
      default 0;

    property MarginMin: Integer
      read FMarginMin
      write SetMarginMin
      default 0;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    property ParentColor: Boolean
      read GetParentColor
      write SetParentColor
      default False;

    property Position: Integer
      read FPosition
      write SetPosition;

    property Percent: Integer
      read FPercent
      write SetPercent
      default 50;

    property PercentMax: Integer
      read FPercentMax
      write SetPercentMax
      default 100;

    property PercentMin: Integer
      read FPercentMin
      write SetPercentMin
      default 0;

    property UsePercent: Boolean
      read FUsePercent
      write SetUsePercent
      default False;

    property RealTimeDrag: Boolean
      read FRealTimeDrag
      write FRealTimeDrag
      default False;

    property UpperLeft: TRzPaneData
      index 1
      read GetPaneData
      write SetPaneData
      stored True;

    property LowerRight: TRzPaneData
      index 2
      read GetPaneData
      write SetPaneData
      stored True;

    property HotSpotVisible: Boolean
      read FHotSpotVisible
      write SetHotSpotVisible
      default False;

    property HotSpotColor: TColor
      read FHotSpotColor
      write SetHotSpotColor
      default clBtnFace;

    property HotSpotDotColor: TColor
      read FHotSpotDotColor
      write SetHotSpotDotColor
      default clHighlight;

    property HotSpotFrameColor: TColor
      read FHotSpotFrameColor
      write SetHotSpotFrameColor
      default clBtnShadow;

    property HotSpotHighlight: TColor
      read FHotSpotHighlight
      write SetHotSpotHighlight
      default clWindow;

    property HotSpotIgnoreMargins: Boolean
      read FHotSpotIgnoreMargins
      write FHotSpotIgnoreMargins
      default True;

    property HotSpotDirection: THotSpotDirection
      read FHotSpotDirection
      write SetHotSpotDirection
      default hsdMin;

    property HotSpotSizePercent: Integer
      read FHotSpotSizePercent
      write SetHotSpotSizePercent
      default 40;

    property SelectedPane: TSelectedPane
      read FSelectedPane
      write SetSelectedPane
      stored False;

    property SplitterStyle: TSplitterStyle
      read FSplitterStyle
      write SetSplitterStyle
      default ssStandard;

    property SplitterWidth: Word
      read FSplitterWidth
      write SetSplitterWidth
      stored True
      default 4;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    property OnChanging: TPositionChangingEvent
      read FOnChanging
      write FOnChanging;

    property OnHotSpotClick: TNotifyEvent
      read FOnHotSpotClick
      write FOnHotSpotClick;

    property OnULGetSiteInfo: TGetSiteInfoEvent
      read FOnULGetSiteInfo
      write FOnULGetSiteInfo;

    property OnULDockOver: TDockOverEvent
      read FOnULDockOver
      write FOnULDockOver;

    property OnULDockDrop: TDockDropEvent
      read FOnULDockDrop
      write FOnULDockDrop;

    property OnULUnDock: TUnDockEvent
      read FOnULUnDock
      write FOnULUnDock;

    property OnLRGetSiteInfo: TGetSiteInfoEvent
      read FOnLRGetSiteInfo
      write FOnLRGetSiteInfo;

    property OnLRDockOver: TDockOverEvent
      read FOnLRDockOver
      write FOnLRDockOver;

    property OnLRDockDrop: TDockDropEvent
      read FOnLRDockDrop
      write FOnLRDockDrop;

    property OnLRUnDock: TUnDockEvent
      read FOnLRUnDock
      write FOnLRUnDock;


    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property BevelWidth;
    property BorderColor;
    property BorderHighlight;
    property BorderInner;
    property BorderOuter default fsNone;
    property BorderShadow;
    property BorderSides;
    property Color;
    property Constraints;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property FullRepaint;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property Height default 100;
    property Locked;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property VisualStyle;
    property Width default 200;

    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;


  {===========================================}
  {== TRzCustomSizePanel Class Declarations ==}
  {===========================================}

  TRzCustomSizePanel = class;

  { Need to create a new dock manager class the knows how to paint a
    TRzSizePanel as a Dock Site. The problem is that the default Dock Manager
    (i.e. TDockTree) does not handle painting the area occupied by the SizeBar }

  TRzSizePanelDockManager = class( TRzPanelDockManager )
  private
    FSizePanel: TRzCustomSizePanel;
  protected
    procedure PositionDockRect( Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect ); override;
    procedure PaintDockFrame( Canvas: TCanvas; Control: TControl; const ARect: TRect ); override;
  public
    constructor Create( DockSite: TWinControl ); override; 
    procedure PaintSite( DC: HDC ); override;
  end;

  TSizeBarWidth = 1..MaxInt;

  TRzCustomSizePanel = class( TRzCustomPanel )
  private
    FMarginMin: Integer;
    FMarginMax: Integer;
    FMarginOffset: Integer;
    FHorzCursor: HCursor;
    FHorzCursorHotSpot: HCursor;
    FVertCursor: HCursor;
    FVertCursorHotSpot: HCursor;
    FLockBar: Boolean;
    FSizeBarWidth: TSizeBarWidth;
    FSizeBarStyle: TSplitterStyle;
    FResizing: Boolean;

    FOutsideHotSpot: Boolean;
    FHotSpotVisible: Boolean;
    FHotSpotHighlight: TColor;
    FHotSpotColor: TColor;
    FHotSpotDotColor: TColor;
    FHotSpotFrameColor: TColor;
    FHotSpotRect: TRect;
    FHotSpotClosed: Boolean;
    FHotSpotting: Boolean;
    FHotSpotPosition: Integer;
    FSide: TSide;
    FOnHotSpotClick: TNotifyEvent;
    FHotSpotIgnoreMargins: Boolean;
    FHotSpotSizePercent: Integer;
    FRefreshHotSpot: Boolean;

    FRealTimeDrag: Boolean;
    FOrigPos: Integer;
    FOrigWidth: Integer;
    FOrigHeight: Integer;
    FCenterOffset: Integer;
    FLastPos: Integer;
    FBrush: TBrush;
    FPrevBrush: HBrush;
    FMaskDC: HDC;

    FGradientColorAdjustment: Integer;
    FGradientPath: TRzGroupBarGradientPath;

    procedure ReadHotSpotClosed( Reader: TReader );
    procedure WriteHotSpotClosed( Writer: TWriter );
    procedure ReadHotSpotPosition( Reader: TReader );
    procedure WriteHotSpotPosition( Writer: TWriter );

    procedure AllocateMaskDC;
    procedure ReleaseMaskDC;

    { Message Handling Methods }
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message wm_WindowPosChanged;
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message cm_DesignHitTest;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;

    procedure FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean ); override;
    function GetClientRect: TRect; override;
    function GetControlRect: TRect; override;
    procedure DrawMask( NewPos: Integer ); virtual;
    procedure UpdateHotSpotRect( BarRect: TRect ); virtual;
    procedure UpdateHotSpotHighlight; virtual;
    procedure DrawHotSpot( Highlight: Boolean ); virtual;
    procedure DrawSizeBar; virtual;
    procedure Paint; override;

    function GetParentWorkingRect: TRect; virtual;
    procedure CheckPosition( var Value: Integer ); virtual;
    function HotSpotTopBottom: Boolean; virtual;

    function CreateDockManager: IDockManager; override;

    { Event Dispatch Methods }
    procedure MouseDownSetFocus; virtual;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;

    procedure HotSpotClick; dynamic;

    { Property Access Methods }
    function GetAlign: TAlign; virtual;
    procedure SetAlign( Value: TAlign ); virtual;
    procedure SetSizeBarWidth( Value: TSizeBarWidth ); virtual;
    procedure SetSizeBarStyle( Value: TSplitterStyle ); virtual;
    function  GetSizeBarRect: TRect; virtual;
    function GetMarginExtent: Integer; virtual;
    procedure SetMarginMax( Value: Integer ); virtual;
    procedure SetMarginMin( Value: Integer ); virtual;
    procedure SetHotSpotVisible( Value: Boolean ); virtual;
    procedure SetHotSpotColor( Value: TColor );
    procedure SetHotSpotDotColor( Value: TColor );
    procedure SetHotSpotFrameColor( Value: TColor );
    procedure SetHotSpotHighlight( Value: TColor ); virtual;
    procedure SetHotSpotSizePercent( Value: Integer ); virtual;
    procedure SetSide( Value: TSide ); virtual;

    procedure SetGradientColorAdjustment( Value: Integer ); virtual;
    procedure SetGradientPath( Value: TRzGroupBarGradientPath ); virtual;

    { Property Declarations }
    property Align: TAlign
      read GetAlign
      write SetAlign
      default alLeft;

    property BorderOuter default fsNone;

    property GradientColorAdjustment: Integer
      read FGradientColorAdjustment
      write SetGradientColorAdjustment
      default 30;

    property GradientPath: TRzGroupBarGradientPath
      read FGradientPath
      write SetGradientPath
      default gpTopToBottom;

    property SizeBarWidth: TSizeBarWidth
      read FSizeBarWidth
      write SetSizeBarWidth
      default 4;

    property SizeBarStyle: TSplitterStyle
      read FSizeBarStyle
      write SetSizeBarStyle
      default ssStandard;

    property MarginMin: Integer
      read FMarginMin
      write SetMarginMin
      default 0;

    property MarginMax: Integer
      read FMarginMax
      write SetMarginMax
      default 0;

    property LockBar: Boolean
      read FLockBar
      write FLockBar
      default False;

    property HotSpotClosed: Boolean
      read FHotSpotClosed;

    property HotSpotPosition: Integer
      read FHotSpotPosition;

    property HotSpotRect: TRect
      read FHotSpotRect;

    property HotSpotVisible: Boolean
      read FHotSpotVisible
      write SetHotSpotVisible
      default False;

    property HotSpotColor: TColor
      read FHotSpotColor
      write SetHotSpotColor
      default clBtnFace;

    property HotSpotDotColor: TColor
      read FHotSpotDotColor
      write SetHotSpotDotColor
      default clHighlight;

    property HotSpotFrameColor: TColor
      read FHotSpotFrameColor
      write SetHotSpotFrameColor
      default clBtnShadow;

    property HotSpotHighlight: TColor
      read FHotSpotHighlight
      write SetHotSpotHighlight
      default clWindow;

    property HotSpotIgnoreMargins: Boolean
      read FHotSpotIgnoreMargins
      write FHotSpotIgnoreMargins
      default False;

    property HotSpotSizePercent: Integer
      read FHotSpotSizePercent
      write SetHotSpotSizePercent
      default 40;

    property Side: TSide
      read FSide
      write SetSide
      default sdLeft;

    property RealTimeDrag: Boolean
      read FRealTimeDrag
      write FRealTimeDrag
      default False;

    property OnHotSpotClick: TNotifyEvent
      read FOnHotSpotClick
      write FOnHotSpotClick;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ResetHotSpot; virtual;
    procedure RestoreHotSpot;
    procedure CloseHotSpot;
  end;


  TRzSizePanel = class( TRzCustomSizePanel )
  public
    property HotSpotClosed;
    property HotSpotPosition;
    property HotSpotRect;
    property DockManager;
  published
    { Property Declarations }
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property AlignmentVertical;
    property Anchors;
    property AutoSize;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderHighlight;
    property BorderInner;
    property BorderOuter;
    property BorderShadow;
    property BorderSides;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FullRepaint;
    property GradientColorAdjustment;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GradientPath;
    property HotSpotColor;
    property HotSpotDotColor;
    property HotSpotFrameColor;
    property HotSpotHighlight;
    property HotSpotIgnoreMargins;
    property HotSpotSizePercent;
    property HotSpotVisible;
    property Side;
    property LockBar;
    property Locked;
    property MarginMax;
    property MarginMin;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RealTimeDrag;
    property ShowDockClientCaptions;
    property ShowHint;
    property SizeBarStyle;
    property SizeBarWidth;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property VisualStyle;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnHotSpotClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


implementation

// Link in Cursors for Splitter Bar
{$R RzSplit.res}

uses
  {&RAS}
  Themes,
  Consts;
  
var
  Registered: Boolean = False;
  FLoadCount: Integer = 0;



{&RT}
{=============================}
{== TRzSplitterPane Methods ==}
{=============================}

constructor TRzSplitterPane.Create( AOwner: TComponent );
begin
  inherited;
  BorderOuter := fsNone;
  ControlStyle := ControlStyle + [ csAcceptsControls, csDisplayDragImage ];

  if AOwner is TRzSplitter then
    FSplitter := AOwner as TRzSplitter
  else
  begin
    { The pane is read in from old version }
    { AOwner is the form }
  end;

  TabStop := False;
end;


procedure TRzSplitterPane.ReadState( Reader: TReader );
var
  OldOwner: TComponent;
begin
  OldOwner := Reader.Owner;
  if Reader.Parent is TRzSplitter then
  begin
    FSplitter := Reader.Parent as TRzSplitter;
    Reader.Owner := Reader.Root;
  end;
  try
    inherited;
  finally
    Reader.Owner := OldOwner;
  end;
end;


procedure TRzSplitterPane.Paint;
var
  OldPenStyle: TPenStyle;
  OldBrushStyle: TBrushStyle;
begin
  inherited;

  if ( csDesigning in ComponentState ) and
     ( BorderOuter = fsNone ) and
     ( BorderInner = fsNone ) then
  begin
    with Canvas do
    begin
      OldPenStyle := Pen.Style;
      OldBrushStyle := Brush.Style;
      Pen.Style := psClear;
      Brush.Color := clGray;

      if FSplitter.Orientation = orVertical then
        Brush.Style := bsFDiagonal
      else
        Brush.Style := bsBDiagonal;

      with ClientRect do
        Rectangle( Left, Top, Right, Bottom );

      if Self = FSplitter.Controls[ 0 ] then
      begin
        Pen.Style := psDot;
        Brush.Style := bsClear;
        with ClientRect do
          Rectangle( Left, Top, Right, Bottom );
      end;
      Brush.Style := OldBrushStyle;
      Pen.Style := OldPenStyle;
    end;
  end;
end;


procedure TRzSplitterPane.DoEnter;
begin
  inherited;
  Invalidate;
end;


procedure TRzSplitterPane.DoExit;
begin
  inherited;
  Invalidate;
end;


function TRzSplitterPane.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;


procedure TRzSplitterPane.SetVisible( Value: Boolean );
begin
  inherited Visible := Value;
  FSplitter.ResizePanes;
end;


procedure TRzSplitterPane.WMNCHitTest( var Msg: TWMNCHitTest );
begin
  if not ( csDesigning in ComponentState ) and not DockSite then
    Msg.Result := htTransparent
  else
    inherited;
end;


{=========================}
{== TRzPaneData Methods ==}
{=========================}

function TRzPaneData.GetBorderColor: TColor;
begin
  Result := FPane.BorderColor;
end;

procedure TRzPaneData.SetBorderColor( Value: TColor );
begin
  FPane.BorderColor := Value;
end;


function TRzPaneData.GetBorderInner: TFrameStyleEx;
begin
  Result := FPane.BorderInner;
end;

procedure TRzPaneData.SetBorderInner( Value: TFrameStyleEx );
begin
  FPane.BorderInner := Value;
end;


function TRzPaneData.GetBorderOuter: TFrameStyleEx;
begin
  Result := FPane.BorderOuter;
end;

procedure TRzPaneData.SetBorderOuter( Value: TFrameStyleEx );
begin
  FPane.BorderOuter := Value;
end;


function TRzPaneData.GetBorderWidth: TBorderWidth;
begin
  Result := FPane.BorderWidth;
end;

procedure TRzPaneData.SetBorderWidth( Value: TBorderWidth );
begin
  FPane.BorderWidth := Value;
end;


function TRzPaneData.GetColor: TColor;
begin
  Result := FPane.Color;
end;

procedure TRzPaneData.SetColor( Value: TColor );
begin
  FPane.Color := Value;
end;


function TRzPaneData.GetFlatColor: TColor;
begin
  Result := FPane.FlatColor;
end;

procedure TRzPaneData.SetFlatColor( Value: TColor );
begin
  FPane.FlatColor := Value;
end;


function TRzPaneData.GetFlatColorAdjustment: Integer;
begin
  Result := FPane.FlatColorAdjustment;
end;

procedure TRzPaneData.SetFlatColorAdjustment( Value: Integer );
begin
  FPane.FlatColorAdjustment := Value;
end;


function TRzPaneData.GetDockSite: Boolean;
begin
  Result := FPane.DockSite;
end;


procedure TRzPaneData.SetDockSite( Value: Boolean );
begin
  FPane.DockSite := Value;
end;


function TRzPaneData.GetShowDockClientCaptions: Boolean;
begin
  Result := FPane.ShowDockClientCaptions;
end;


procedure TRzPaneData.SetShowDockClientCaptions( Value: Boolean );
begin
  FPane.ShowDockClientCaptions := Value;
end;


function TRzPaneData.GetVisible: Boolean;
begin
  Result := FPane.Visible;
end;

procedure TRzPaneData.SetVisible( Value: Boolean );
begin
  FPane.Visible := Value;
end;


function TRzPaneData.GetPadding: TPadding;
begin
  Result := FPane.Padding;
end;


procedure TRzPaneData.SetPadding( Value: TPadding );
begin
  FPane.Padding := Value;
end;



{=========================}
{== TRzSplitter Methods ==}
{=========================}

constructor TRzSplitter.Create( AOwner: TComponent );
var
  I: Integer;
begin
  inherited;
  ControlStyle := [ csOpaque, csCaptureMouse, csDisplayDragImage ];

  inherited SetBorderOuter( fsNone );
  Width := 200;
  Height := 100;
  FPosition := 100;
  FMarginMin := 0;
  FMarginMax := 0;
  FUsePercent := False;
  FPercent := 50;
  FPercentMin := 0;
  FPercentMax := 100;
  FFixedPane := fpUpperLeft;
  FOrientation := orHorizontal;
  FResizing := False;

  FSelectedPane := spUpperLeft;
  FSplitterStyle := ssStandard;
  FSplitterWidth := 4;
  FRealTimeDrag := False;
  {&RCI}
  for I := 1 to 2 do
  begin
    FPanes[ I ] := TRzSplitterPane.Create( Self );
    FPanes[ I ].Parent := Self;
    FPanesControlList[ I ] := TStringList.Create;
  end;
  FPaneData[ 1 ] := TRzPaneData.Create;
  FPaneData[ 1 ].FPane := FPanes[ 1 ];
  FPaneData[ 2 ] := TRzPaneData.Create;
  FPaneData[ 2 ].FPane := FPanes[ 2 ];

  FPanes[ 1 ].OnGetSiteInfo := ULGetSiteInfoHandler;
  FPanes[ 1 ].OnDockOver := ULDockOverHandler;
  FPanes[ 1 ].OnDockDrop := ULDockDropHandler;
  FPanes[ 1 ].OnUnDock := ULUnDockHandler;

  FPanes[ 2 ].OnGetSiteInfo := LRGetSiteInfoHandler;
  FPanes[ 2 ].OnDockOver := LRDockOverHandler;
  FPanes[ 2 ].OnDockDrop := LRDockDropHandler;
  FPanes[ 2 ].OnUnDock := LRUnDockHandler;

  { Setup Splitter Bar }
  FSliding := False;
  FLockBar := False;

  FHotSpotVisible := False;
  FHotSpotColor := clBtnFace;
  FHotSpotDotColor := clHighlight;
  FHotSpotFrameColor := clBtnShadow;
  FHotSpotHighlight := clWindow;
  FHotSpotDirection := hsdMin;
  FHotSpotIgnoreMargins := True;
  FHotSpotClosed := False;
  FHotSpotClosedToMin := False;
  FHotSpotClosedToMax := False;
  FHotSpotting := False;
  FHotSpotPosition := 100;
  FHotSpotSizePercent := 40;

  FHorzCursor := LoadCursor( HInstance, 'RZSPLIT_HORZ' );
  FHorzCursorHotSpot := LoadCursor( HInstance, 'RZSPLIT_HORZHOTSPOT' );
  FVertCursor := LoadCursor( HInstance, 'RZSPLIT_VERT' );
  FVertCursorHotSpot := LoadCursor( HInstance, 'RZSPLIT_VERTHOTSPOT' );

  FGradientColorAdjustment := 30;

  if not Registered then
  begin
    Classes.RegisterClasses( [ TRzSplitterPane ] );
    Registered := True;
  end;
end; {= TRzSplitter.Create =}


procedure TRzSplitter.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or ws_ClipChildren;
  {&RV}
end;


procedure TRzSplitter.CreateWnd;
begin
  inherited;
end;


destructor TRzSplitter.Destroy;
var
  I: Integer;
begin
  DestroyCursor( FHorzCursor );
  DestroyCursor( FHorzCursorHotSpot );
  DestroyCursor( FVertCursor );
  DestroyCursor( FVertCursorHotSpot );
  FBrush.Free;

  for I := 1 to 2 do
    FPaneData[ I ].Free;

  for I := 1 to 2 do
    FPanesControlList[ I ].Free;

  inherited;
end;




procedure TRzSplitter.AlignControls( AControl: TControl; var Rect: TRect );
begin
  { Don't align controls if loading this splitter or other splitters }
  if ( FLoadCount <> 0 ) or ( csLoading in ComponentState ) then
    Exit;

  inherited;
end;


{==================================================================
  TRzSplitter.Loaded

  This method is responsible for "correcting" the parent hierarchy
  within the compound component. That is, when the component is
  written to a stream, the Parent of all controls dropped onto the
  drop area are changed to the main component rather than the drop
  area. This is necessary in order to support form inheritance.
  This method, corrects the Parent hierarchy by iterating through
  the FPanesControlList.
==================================================================}

procedure TRzSplitter.Loaded;
var
  I, P: Integer;
  C: TComponent;
  SaveActiveControl: TWinControl;

  function GetCurrentActiveControl: TWinControl;
  begin
    Result := nil;
    if Owner <> nil then
    begin
      if Owner is TCustomForm then
        Result := TCustomForm( Owner ).ActiveControl;
    end;
  end;

  procedure SetCurrentActiveControl( C: TWinControl );
  begin
    if Owner <> nil then
    begin
      if Owner is TCustomForm then
        TCustomForm( Owner ).ActiveControl := C;
    end;
  end;

begin {= TRzSplitter.Loaded =}
  {&RV}
  Inc( FLoadCount );
  try
    inherited;

    SaveActiveControl := GetCurrentActiveControl;

    UpdateManualPosition( FPosition );
    { Resize panes before changing parents to prevent Anchors bug }
    ResizePanes;

    DisableAlign;
    try
      if Owner <> nil then
      begin
        for P := 1 to 2 do
        begin
          for I := 0 to FPanesControlList[ P ].Count - 1 do
          begin
            C := Owner.FindComponent( FPanesControlList[ P ][ I ] );
            if C <> nil then
              TControl( C ).Parent := FPanes[ P ];
          end;

          { Fixup Tab order of controls placed on each pane }
          FPanes[ P ].FixupTabList;
        end;
      end;
    finally
      EnableAlign;
    end;

    SetCurrentActiveControl( SaveActiveControl );
  finally
    Dec( FLoadCount );
  end;
end; {= TRzSplitter.Loaded =}



{==================================================================
  TRzSplitter.GetChildren

  This method is responsible for changing the parent hierarchy when
  the component is written to a stream. This method is called by
  Delphi to determine which components are children of the main
  component. We override this method to tell Delphi to treat all of
  the components dropped into the panes as children of the main
  component rather than children of the embedded panes. The parent
  hierarchy is corrected in the Loaded method.
==================================================================}

procedure TRzSplitter.GetChildren( Proc: TGetChildProc; Root: TComponent );
var
  I, P: Integer;
  Control: TControl;
begin
  inherited;

  for P := 1 to 2 do
  begin
    for I := 0 to FPanes[ P ].ControlCount - 1 do
    begin                        { For each control in the drop area }
      Control := FPanes[ P ].Controls[ I ];
      { Only call Proc if the control's Owner is the same as the Owner of the splitter. }
      { This will not be true when form inheritance is being used. }
      if Control.Owner = Owner then
        Proc( Control );
    end;
  end;
end; {= TRzSplitter.GetChildren =}


procedure TRzSplitter.ShowControl( AControl: TControl );
var
  I: Integer;
  ParentForm: TCustomForm;
begin
  for I := 1 to 2 do
  begin
    if FPanes[ I ] = AControl then
    begin
      if csLoading in ComponentState then
        Exit;

      if not FPanes[ I ].Visible then
      begin
        ParentForm := GetParentForm(Self);
        if ParentForm <> nil then
          if ContainsControl(ParentForm.ActiveControl) then
            ParentForm.ActiveControl := Self;

        FPanes[ I ].Visible := True;
      end;
      Exit;
    end;
  end;
  inherited;
end;


procedure TRzSplitter.DefineProperties( Filer: TFiler );
begin
  inherited;

  Filer.DefineProperty( 'BarSize', ReadBarSize, WriteBarSize, Filer.Ancestor = nil );
  Filer.DefineProperty( 'HotSpotClosed', ReadHotSpotClosed, WriteHotSpotClosed, FHotSpotClosed );
  Filer.DefineProperty( 'HotSpotClosedToMin', ReadHotSpotClosedToMin, WriteHotSpotClosedToMin, FHotSpotClosed );
  Filer.DefineProperty( 'HotSpotClosedToMax', ReadHotSpotClosedToMax, WriteHotSpotClosedToMax, FHotSpotClosed );
  Filer.DefineProperty( 'HotSpotPosition', ReadHotSpotPosition, WriteHotSpotPosition, FHotSpotClosed );

  // In order to correct the parent hierarchy, a list of all controls that were dropped into the drop area must be
  // stored in the stream along with the component. To do this, we define a new custom property using this method.
  Filer.DefineProperty( 'UpperLeftControls', ReadULControls, WriteULControls, True );
  Filer.DefineProperty( 'LowerRightControls', ReadLRControls, WriteLRControls, True );

  // Handle the fact that the PositionMin, PositionMax, PercentMin, and
  // PercentMax properties were published in version 1.6
  Filer.DefineProperty( 'PositionMin', TRzOldPropReader.ReadOldIntegerProp, nil, False );
  Filer.DefineProperty( 'PositionMax', TRzOldPropReader.ReadOldIntegerProp, nil, False );
end;


procedure TRzSplitter.ReadBarSize( Reader: TReader );
begin
  Reader.ReadListBegin;
  FBarRect.Left := Reader.ReadInteger;
  FBarRect.Top := Reader.ReadInteger;
  FBarRect.Right := Reader.ReadInteger;
  FBarRect.Bottom := Reader.ReadInteger;
  UpdateHotSpotRect;
  Reader.ReadListEnd;
end;


procedure TRzSplitter.WriteBarSize( Writer: TWriter );
begin
  Writer.WriteListBegin;
  Writer.WriteInteger( FBarRect.Left );
  Writer.WriteInteger( FBarRect.Top );
  Writer.WriteInteger( FBarRect.Right );
  Writer.WriteInteger( FBarRect.Bottom );
  Writer.WriteListEnd;
end;


procedure TRzSplitter.ReadHotSpotClosed( Reader: TReader );
begin
  FHotSpotClosed := Reader.ReadBoolean;
end;

procedure TRzSplitter.WriteHotSpotClosed( Writer: TWriter );
begin
  Writer.WriteBoolean( FHotSpotClosed );
end;


procedure TRzSplitter.ReadHotSpotClosedToMin( Reader: TReader );
begin
  FHotSpotClosedToMin := Reader.ReadBoolean;
end;

procedure TRzSplitter.WriteHotSpotClosedToMin( Writer: TWriter );
begin
  Writer.WriteBoolean( FHotSpotClosedToMin );
end;


procedure TRzSplitter.ReadHotSpotClosedToMax( Reader: TReader );
begin
  FHotSpotClosedToMax := Reader.ReadBoolean;
end;

procedure TRzSplitter.WriteHotSpotClosedToMax( Writer: TWriter );
begin
  Writer.WriteBoolean( FHotSpotClosedToMax );
end;


procedure TRzSplitter.ReadHotSpotPosition( Reader: TReader );
begin
  FHotSpotPosition := Reader.ReadInteger;
end;

procedure TRzSplitter.WriteHotSpotPosition( Writer: TWriter );
begin
  Writer.WriteInteger( FHotSpotPosition );
end;

{==================================================================
  TRzSplitter.ReadULControls

  This method is called whenever the UpperLeftControls custom
  property needs to be read from a stream.  Each component
  identifier that was stored in the stream is read and saved into
  the FPanesControlList string list.
==================================================================}

procedure TRzSplitter.ReadULControls( Reader: TReader );
begin
  FPanesControlList[ 1 ].Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    FPanesControlList[ 1 ].Add( Reader.ReadIdent );
  Reader.ReadListEnd;
end;

{==================================================================
  TRzSplitter.WriteULControls

  This method is called whenever the UpperLeftControls custom
  property needs to be written to a stream.  The identifier of
  each control dropped onto the drop area is stored in the stream.
==================================================================}

procedure TRzSplitter.WriteULControls( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FPanes[ 1 ].ControlCount - 1 do
    Writer.WriteIdent( FPanes[ 1 ].Controls[ I ].Name );
  Writer.WriteListEnd;
end;


{==================================================================
  TRzSplitter.ReadLRControls

  This method is called whenever the LowerRightControls custom
  property needs to be read from a stream.  Each component
  identifier that was stored in the stream is read and saved into
  the FPanesControlList string list.
==================================================================}

procedure TRzSplitter.ReadLRControls( Reader: TReader );
begin
  FPanesControlList[ 2 ].Clear;
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    FPanesControlList[ 2 ].Add( Reader.ReadIdent );
  Reader.ReadListEnd;
end;

{==================================================================
  TRzSplitter.WriteLRControls

  This method is called whenever the LowerRightControls custom
  property needs to be written to a stream.  The identifier of
  each control dropped onto the drop area is stored in the stream.
==================================================================}

procedure TRzSplitter.WriteLRControls( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to FPanes[ 2 ].ControlCount - 1 do
    Writer.WriteIdent( FPanes[ 2 ].Controls[ I ].Name );
  Writer.WriteListEnd;
end;


function TRzSplitter.GetPaneData( Index: Integer ): TRzPaneData;
begin
  Result := FPaneData[ Index ];
end;


procedure TRzSplitter.SetPaneData( Index: Integer; Value: TRzPaneData );
begin
  FPaneData[ Index ].Assign( Value );
end;


procedure TRzSplitter.UpdateHotSpotRect;
var
  Mid, S: Integer;
begin
  with FBarRect do
  begin
    if FOrientation = orVertical then
    begin
      Mid := Left + ( Right - Left ) div 2;
      S := Round ( FHotSpotSizePercent / 100 * ( Right - Left ) ) div 2;
      FHotSpotRect := Rect( Mid - S, Top, Mid + S, Bottom  );
    end
    else
    begin
      Mid := Top + ( Bottom - Top ) div 2;
      S := Round ( FHotSpotSizePercent / 100 * ( Bottom - Top ) ) div 2;
      FHotSpotRect := Rect( Left, Mid - S, Right, Mid + S );
    end;
  end;
end;


function TRzSplitter.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if not DockSite then
    FixClientRect( Result, True );
end;


procedure TRzSplitter.ResizePanes;
var
  R: TRect;
  H, W, I: Integer;
begin
  if not ( csLoading in ComponentState ) then
  begin
    R := GetClientRect;
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;

    DisableAlign;
    try
      if FOrientation = orVertical then
      begin
        { Upper Pane }
        if UpperLeft.Pane.Visible and not LowerRight.Pane.Visible then
          UpperLeft.Pane.SetBounds( R.Left, R.Top, W, H )
        else if UpperLeft.Pane.Visible then
          UpperLeft.Pane.SetBounds( R.Left, R.Top, W, FPosition )
        else
          UpperLeft.Pane.SetBounds( R.Left, R.Top, W, 0 );

        { Splitter Bar }
        if UpperLeft.Pane.Visible and LowerRight.Pane.Visible then
          FBarRect := Bounds( R.Left, R.Top + FPosition, W, FSplitterWidth )
        else
          FBarRect := Bounds( R.Left, R.Top, W, 0 );
        UpdateHotSpotRect;

        { Lower Pane }
        if LowerRight.Pane.Visible and not UpperLeft.Pane.Visible then
          LowerRight.Pane.SetBounds( R.Left, R.Top, W, H )
        else if LowerRight.Pane.Visible then
          LowerRight.Pane.SetBounds( R.Left, R.Top + FPosition + FSplitterWidth,
                                     W, H - FPosition - FSplitterWidth )
        else
          LowerRight.Pane.SetBounds( R.Left, R.Top, W, 0 );
      end
      else { Orientation = orHorizontal }
      begin
        { Left Pane }
        if UpperLeft.Pane.Visible and not LowerRight.Pane.Visible then
          UpperLeft.Pane.SetBounds( R.Left, R.Top, W, H )
        else if UpperLeft.Pane.Visible then
          UpperLeft.Pane.SetBounds( R.Left, R.Top, FPosition, H )
        else
          UpperLeft.Pane.SetBounds( R.Left, R.Top, 0, H );

        { Splitter Bar }
        if UpperLeft.Pane.Visible and LowerRight.Pane.Visible then
          FBarRect := Bounds( R.Left + FPosition, R.Top, FSplitterWidth, H )
        else
          FBarRect := Bounds( R.Left, R.Top, 0, H );
        UpdateHotSpotRect;

        { Right Pane }
        if LowerRight.Pane.Visible and not UpperLeft.Pane.Visible then
          LowerRight.Pane.SetBounds( R.Left, R.Top, W, H )
        else if LowerRight.Pane.Visible then
          LowerRight.Pane.SetBounds( R.Left + FPosition + FSplitterWidth, R.Top,
                                     W - FPosition - FSplitterWidth, H )
        else
          LowerRight.Pane.SetBounds( R.Left, R.Top, 0, H );
      end;

      { Resize the panes of any nested splitters }

      for I := 0 to UpperLeft.Pane.ControlCount - 1 do
      begin
        if UpperLeft.Pane.Controls[ I ] is TRzSplitter then
          TRzSplitter( UpperLeft.Pane.Controls[ I ] ).ResizePanes;
      end;

      for I := 0 to LowerRight.Pane.ControlCount - 1 do
      begin
        if LowerRight.Pane.Controls[ I ] is TRzSplitter then
          TRzSplitter( LowerRight.Pane.Controls[ I ] ).ResizePanes;
      end;
    finally
      EnableAlign;
    end;

    if FRealTimeDrag then
    begin
      UpperLeft.Pane.Update;
      LowerRight.Pane.Update;
    end;
    DrawSplitterBar;        { Added to fix problem of drawing hot spot on short moves }
  end;
end; {= TRzSplitter.ResizePanes =}


procedure TRzSplitter.Resize;
var
  Size: Integer;
begin
  inherited;

  if not ( csWriting in ComponentState ) and
     not ( csDestroying in ComponentState ) then
  begin
    FResizing := True;
    try
      if FOrientation = orHorizontal then
        Size := Width
      else
        Size := Height;

      if FFixedPane = fpUpperLeft then
      begin
        if FUsePercent then
        begin
          FMarginMin := Round( FPercentMin / 100 * ( Size - FSplitterWidth div 2 ) );
          FMarginMax := ( Size - FSplitterWidth div 2 ) - Round( FPercentMax / 100 * ( Size - FSplitterWidth div 2 ) );
          if FHotSpotClosed then
          begin
            if FHotSpotClosedToMin then
              SetPosition( 0 )
            else
              SetPosition( Round( Size - FSplitterWidth div 2 ) );
          end
          else
            SetPosition( Round( FPercent / 100 * ( Size - FSplitterWidth div 2 ) ) );
        end
        else
        begin
          if FHotSpotClosed then
          begin
            if FHotSpotClosedToMin then
              SetPosition( 0 )
            else
              SetPosition( MaxInt );
          end
          else
            SetPosition( Position );   // Ensures that splitter bar remains in view
          if ( Position < FManualPosition ) and not FHotSpotClosed then
            SetPosition( FManualPosition );
        end;
      end
      else
      begin
        if not FUsePercent then
        begin
          SetPosition( Size - FFixedSize );   { Ensures that splitter bar remains in view }
          if ( Position < FManualPosition ) and not FHotSpotClosed then
            SetPosition( Size - FManualPosition );
          if FHotSpotClosed then
            FHotSpotPosition := Size - FManualPosition;
        end;
      end;
    finally
      FResizing := False;
    end;
  end;
end; {= TRzSplitter.Resize =}


procedure TRzSplitter.UpdateFixedSize;
begin
  if FFixedPane = fpUpperLeft then
    FFixedSize := FPosition
  else
  begin
    if FOrientation = orHorizontal then
      FFixedSize := Width - FPosition
    else
      FFixedSize := Height - FPosition;
  end;
end;

procedure TRzSplitter.SetFixedPane( Value: TFixedPane );
begin
  if FFixedPane <> Value then
  begin
    FFixedPane := Value;

    if FFixedPane = fpLowerRight then
      FUsePercent := False;

    { Update FManualPosition value based on which pane is fixed }
    UpdateManualPosition( Position );
    UpdateFixedSize;
  end;
end;


procedure TRzSplitter.SetOrientation( Value: TOrientation );
var
  Size: Integer;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if FUsePercent then
    begin
      if FOrientation = orHorizontal then
        Size := Width
      else
        Size := Height;

      SetPosition( Round( FPercent / 100 * ( Size - FSplitterWidth div 2 ) ) );
      FMarginMin := Round( FPercentMin / 100 * ( Size - FSplitterWidth div 2 ) );
      FMarginMax := ( Size - FSplitterWidth div 2 ) - Round( FPercentMax / 100 * ( Size - FSplitterWidth div 2 ) );
    end
    else
      ResizePanes;
    DrawSplitterBar;
  end;
end;


procedure TRzSplitter.SetSelectedPane( Value: TSelectedPane );
var
  ParentForm: TCustomForm;
begin
  if not ( csDesigning in ComponentState ) or ( csLoading in ComponentState ) then
  begin
    FSelectedPane := Value;
    Exit;
  end;

  if FSelectedPane <> Value then
  begin
    ParentForm := GetParentForm( Self );
    if ParentForm <> nil then
    begin
      if ContainsControl( ParentForm.ActiveControl ) then
        ParentForm.ActiveControl := Self;
    end;

    FSelectedPane := Value;
    { This little trick causes the panes to get reordered in the Splitter's    }
    { Controls list.  When a paste occurs, the first control in the list gets  }
    { the new components.                                                      }

    if Value = spUpperLeft then
      FPanes[ 2 ].BringToFront
    else
      FPanes[ 1 ].BringToFront;

    FPanes[ 1 ].Invalidate;
    FPanes[ 2 ].Invalidate;

    if ParentForm <> nil then
    begin
      if ParentForm.ActiveControl = Self then
        SelectFirst;
    end;

    ResizePanes;
  end;
end;


function TRzSplitter.GetSelectedPaneControl: TRzSplitterPane;
begin
  if FSelectedPane = spUpperLeft then
    Result := FPanes[ 1 ]
  else
    Result := FPanes[ 2 ];
end;


procedure TRzSplitter.HotSpotClick;
begin
  if Assigned( FOnHotSpotClick ) then
    FOnHotSpotClick( Self );
end;

procedure TRzSplitter.Change;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


function TRzSplitter.CanChange( NewPos: Integer ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, NewPos, Result );
end;


procedure TRzSplitter.CheckPosition( var Value: Integer );
var
  Size, BorderOffset, MinValue, MaxValue: Integer;
begin
  if FOrientation = orVertical then
    Size := Height
  else
    Size := Width;

  if FPanes[ 1 ].Visible and FPanes[ 2 ].Visible then
  begin
    if FHotSpotClosed and FHotSpotIgnoreMargins then
    begin
      MinValue := 0;
      MaxValue := 0;
    end
    else
    begin
      MinValue := FMarginMin;
      MaxValue := FMarginMax;
    end;

    if Value < MinValue then
      Value := MinValue;

    BorderOffset := GetBorderOffset;
    if Value > ( Size - MaxValue - FSplitterWidth - 2 * BorderOffset ) then
      Value := Size - MaxValue - FSplitterWidth - 2 * BorderOffset;
  end;
end; {= TRzSplitter.CheckPosition =}


procedure TRzSplitter.SetPosition( Value: Integer );
var
  OldPosition: Integer;
begin
  if not FResizing and ( FPosition = Value ) then
    Exit;

  { Important NOT to check FPosition against Value b/c on a Resize
    the range checking needs to still occur. }

  CheckPosition( Value );

  if CanChange( Value ) then
  begin
    OldPosition := FPosition;
    FPosition := Value;

    if FOrientation = orHorizontal then
    begin
      if ( Width - FSplitterWidth div 2 ) = 0 then
        FPercent := 0
      else
        FPercent := Round( FPosition / ( Width - FSplitterWidth div 2 ) * 100 );
    end
    else
    begin
      if ( Height - FSplitterWidth div 2 ) = 0 then
        FPercent := 0
      else
        FPercent := Round( FPosition / ( Height - FSplitterWidth div 2 ) * 100 );
    end;
    ResizePanes;
    UpdateFixedSize;

    Change;                                             { Trigger Change event }

    if ( csDesigning in ComponentState ) and ( FLoadCount = 0 ) and ( FPosition <> OldPosition ) then
    begin
      UpdateObjectInspector( Self );
    end;
  end;
end; {= TRzSplitter.SetPosition =}


procedure TRzSplitter.SetMarginMax( Value: Integer );
var
  Size: Integer;
begin
  FMarginMax := Value;

  if FMarginMax < 0 then
    FMarginMax := 0;

  if FOrientation = orHorizontal then
    Size := Width
  else
    Size := Height;

  if ( FMarginMax + FMarginMin ) > Size then
    FMarginMax := Size - FMarginMin;
end;

procedure TRzSplitter.SetMarginMin( Value: Integer );
var
  Size: Integer;
begin
  FMarginMin := Value;

  if FMarginMin < 0 then
    FMarginMin := 0;

  if FOrientation = orHorizontal then
    Size := Width
  else
    Size := Height;

  if ( FMarginMin + FMarginMax ) > Size then
    FMarginMin := Size - FMarginMax;
end;



function TRzSplitter.GetParentColor: Boolean;
begin
  Result := inherited ParentColor;
end;


procedure TRzSplitter.SetParentColor( Value: Boolean );
begin
  inherited ParentColor := Value;
  FPanes[ 1 ].ParentColor := Value;
  FPanes[ 2 ].ParentColor := Value;
end;


procedure TRzSplitter.SetUsePercent( Value: Boolean );
begin
  if FUsePercent <> Value then
  begin
    FUsePercent := Value;
    if FUsePercent then
    begin
      FFixedPane := fpUpperLeft;
      SetPercentMin( FPercentMin );
      SetPercentMax( FPercentMax );
    end;
    SetPosition( FPosition );
  end;
end;


procedure TRzSplitter.SetPercent( Value: Integer );
begin
  if FPercent <> Value then
  begin
    FPercent := Value;
    if not ( csLoading in ComponentState ) then
    begin
      FUsePercent := True;

      if FOrientation = orHorizontal then
        SetPosition( Round( FPercent / 100 * ( Width - FSplitterWidth div 2 ) ) )
      else
        SetPosition( Round( FPercent / 100 * ( Height - FSplitterWidth div 2 ) ) )
    end;
  end;
end;


procedure TRzSplitter.SetPercentMax( Value: Integer );
var
  Size: Integer;
begin
  FPercentMax := Value;
  if FPercentMax > 100 then
    FPercentMax := 100;

  if not ( csLoading in ComponentState ) and FUsePercent then
  begin
    if FOrientation = orHorizontal then
      Size := Width
    else
      Size := Height;

    FMarginMax := ( Size - FSplitterWidth div 2 ) - 
                  Round( FPercentMax / 100 * ( Size - FSplitterWidth div 2 ) );
  end;
end; {= TRzSplitter.SetPercentMax =}


procedure TRzSplitter.SetPercentMin( Value: Integer );
var
  Size: Integer;
begin
  FPercentMin := Value;
  if FPercentMin < 0 then
    FPercentMin := 0;

  if not ( csLoading in ComponentState ) and FUsePercent then
  begin
    if FOrientation = orHorizontal then
      Size := Width
    else
      Size := Height;

    FMarginMin := Round( FPercentMin / 100 * ( Size - FSplitterWidth div 2 ) );
  end;
end; {= TRzSplitter.SetPercentMin =}


procedure TRzSplitter.ResetHotSpot;
begin
  FHotSpotClosed := False;
  FHotSpotClosedToMin := False;
  FHotSpotClosedToMax := False;
end;


procedure TRzSplitter.RestoreHotSpot;
begin
  if FHotSpotClosed then
  begin
    FHotSpotClosed := False;
    FHotSpotClosedToMin := False;
    FHotSpotClosedToMax := False;
    Position := FHotSpotPosition;
    UpdateManualPosition( FPosition );
  end;
end;


procedure TRzSplitter.CloseHotSpot;
begin
  CloseHotSpot( FHotSpotDirection = hsdMin );
end;


procedure TRzSplitter.CloseHotSpot( ToMin: Boolean );
begin
  if not FHotSpotClosed then
  begin
    FHotSpotClosed := True;
    FHotSpotPosition := FPosition;
    if ToMin then
    begin
      FHotSpotClosedToMin := True;
      Position := 0;
    end
    else
    begin
      FHotSpotClosedToMax := True;
      Position := MaxInt;
    end;
  end;
end;


procedure TRzSplitter.SetHotSpotVisible( Value: Boolean );
begin
  if FHotSpotVisible <> Value then
  begin
    FHotSpotVisible := Value;
    if FHotSpotVisible then
    begin
      if FSplitterWidth < 7 then
        SplitterWidth := 7;
    end;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetHotSpotColor( Value: TColor );
begin
  if FHotSpotColor <> Value then
  begin
    FHotSpotColor := Value;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetHotSpotDotColor( Value: TColor );
begin
  if FHotSpotDotColor <> Value then
  begin
    FHotSpotDotColor := Value;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetHotSpotFrameColor( Value: TColor );
begin
  if FHotSpotFrameColor <> Value then
  begin
    FHotSpotFrameColor := Value;
    Invalidate;
  end;
end;



procedure TRzSplitter.SetHotSpotHighlight( Value: TColor );
begin
  if FHotSpotHighlight <> Value then
  begin
    FHotSpotHighlight := Value;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetHotSpotDirection( Value: THotSpotDirection );
begin
  if FHotSpotDirection <> Value then
  begin
    FHotSpotDirection := Value;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetHotSpotSizePercent( Value: Integer );
begin
  if FHotSpotSizePercent <> Value then
  begin
    FHotSpotSizePercent := Value;
    UpdateHotSpotRect;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetSplitterStyle( Value: TSplitterStyle );
begin
  if FSplitterStyle <> Value then
  begin
    FSplitterStyle := Value;
    if ( FSplitterStyle <> ssStandard ) and ( FSplitterWidth < 6 ) then
      SplitterWidth := 6;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetSplitterWidth( Value: Word );
begin
  if FSplitterWidth <> Value then
  begin
    FSplitterWidth := Value;
    if FOrientation = orVertical then
      FBarRect.Bottom := FBarRect.Top + FSplitterWidth
    else
      FBarRect.Right := FBarRect.Left + FSplitterWidth;
    ResizePanes;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetTransparent( Value: Boolean );
begin
  inherited;
  FPanes[ 1 ].Transparent := Value;
  FPanes[ 2 ].Transparent := Value;
end;


procedure TRzSplitter.SetGradientColorAdjustment( Value: Integer );
begin
  if FGradientColorAdjustment <> Value then
  begin
    FGradientColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetGradientPath( Value: TRzGroupBarGradientPath );
begin
  if FGradientPath <> Value then
  begin
    FGradientPath := Value;
    Invalidate;
  end;
end;


procedure TRzSplitter.SetBorderInner( Value: TFrameStyleEx );
begin
  if BorderInner <> Value then
  begin
    inherited;
    ResizePanes;
  end;
end;


procedure TRzSplitter.SetBorderOuter( Value: TFrameStyleEx );
begin
  if BorderOuter <> Value then
  begin
    inherited;
    ResizePanes;
  end;
end;


function TRzSplitter.GetBorderWidth: TBorderWidth;
begin
  Result := inherited BorderWidth;
end;

procedure TRzSplitter.SetBorderWidth( Value: TBorderWidth );
begin
  if inherited BorderWidth <> Value then
  begin
    inherited BorderWidth := Value;
    ResizePanes;
  end;
end;


procedure TRzSplitter.SetBorderSides( Value: TSides );
begin
  if BorderSides <> Value then
  begin
    inherited;
    ResizePanes;
  end;
end;


procedure TRzSplitter.ULGetSiteInfoHandler( Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                                            MousePos: TPoint; var CanDock: Boolean );
begin
  if Assigned( FOnULGetSiteInfo ) then
    FOnULGetSiteInfo( Self, DockClient, InfluenceRect, MousePos, CanDock );
end;


procedure TRzSplitter.ULDockOverHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState;
                                         var Accept: Boolean );
begin
  if Assigned( FOnULDockOver ) then
    FOnULDockOver( Self, Source, X, Y, State, Accept );
end;


procedure TRzSplitter.ULDockDropHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer );
begin
  if Assigned( FOnULDockDrop ) then
    FOnULDockDrop( Self, Source, X, Y );
end;


procedure TRzSplitter.ULUnDockHandler( Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean );
begin
  if Assigned( FOnULUnDock ) then
    FOnULUnDock( Self, Client, NewTarget, Allow );
end;


procedure TRzSplitter.LRGetSiteInfoHandler( Sender: TObject; DockClient: TControl; var InfluenceRect: TRect;
                                            MousePos: TPoint; var CanDock: Boolean );
begin
  if Assigned( FOnLRGetSiteInfo ) then
    FOnLRGetSiteInfo( Self, DockClient, InfluenceRect, MousePos, CanDock );
end;


procedure TRzSplitter.LRDockOverHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState;
                                         var Accept: Boolean );
begin
  if Assigned( FOnLRDockOver ) then
    FOnLRDockOver( Self, Source, X, Y, State, Accept );
end;


procedure TRzSplitter.LRDockDropHandler( Sender: TObject; Source: TDragDockObject; X, Y: Integer );
begin
  if Assigned( FOnLRDockDrop ) then
    FOnLRDockDrop( Self, Source, X, Y );
end;


procedure TRzSplitter.LRUnDockHandler( Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean );
begin
  if Assigned( FOnLRUnDock ) then
    FOnLRUnDock( Self, Client, NewTarget, Allow );
end;


procedure TRzSplitter.CMDesignHitTest( var Msg: TCMDesignHitTest );
var
  Shift: TShiftState;
begin
  if FSliding or PtInRect( FBarRect, CursorPosition ) then
  begin
    Shift := KeysToShiftState( Msg.Keys );
    if ( ssShift in Shift ) and ( ssCtrl in Shift ) then
      Msg.Result := 0
    else
      Msg.Result := 1         { Allow splitter to be moved w/ LMB at design-time }
  end
  else
    Msg.Result := 0;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzSplitter.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  if UsingSystemStyle then
    Color := clBtnFace
  else
    Color := ActiveStyleColor( scPanel );
  FPanes[ 1 ].Color := Color;
  FPanes[ 2 ].Color := Color;
end;

{$ENDIF}


procedure TRzSplitter.UpdateHotSpotHighlight;
var
  P: TPoint;
begin
  if FHotSpotVisible then
  begin
    P := CursorPosition;
    // Only draw HotSpot when a change takes place
    if PtInRect( FHotSpotRect, P ) then
    begin
      if FOutsideHotSpot or ( FHotSpotDirection = hsdBoth ) then
        DrawHotSpot( True, P );
      FOutsideHotSpot := False;
    end
    else
    begin
      if not FOutsideHotSpot then
        DrawHotSpot( False, P );
      FOutsideHotSpot := True;
    end;
  end;
end;


procedure TRzSplitter.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
  UpdateHotSpotHighlight;
end;


procedure TRzSplitter.WMSetCursor( var Msg: TWMSetCursor );
begin
  UpdateHotSpotHighlight;

  if PtInRect( FBarRect, CursorPosition ) and not FLockBar then
  begin
    if FHotSpotVisible and PtInRect( FHotSpotRect, CursorPosition ) then
    begin
      if FOrientation = orVertical then
        SetCursor( FVertCursorHotSpot )
      else
        SetCursor( FHorzCursorHotSpot );
      Exit;
    end
    else
    begin
      if FOrientation = orVertical then
        SetCursor( FVertCursor )
      else
        SetCursor( FHorzCursor );
      Exit;
    end;
  end;
  inherited;
end;


procedure TRzSplitter.UpdateManualPosition( P: Integer );
begin
  if FFixedPane = fpUpperLeft then
    FManualPosition := P
  else
  begin
    if FOrientation = orVertical then
      FManualPosition := Height - P
    else
      FManualPosition := Width - P;
  end;
end;

procedure TRzSplitter.SetLockedPosition( Value: Integer );
begin
  if FPosition <> Value then
  begin
    Position := Value;
    UpdateManualPosition( FPosition );
  end;
end;


procedure TRzSplitter.MouseDownSetFocus;
begin
  SetFocus;
end;


procedure TRzSplitter.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if not PtInRect( FBarRect, CursorPosition ) or ( Button <> mbLeft ) then
    Exit;

  if ( not FHotSpotVisible or not PtInRect( FHotSpotRect, CursorPosition ) ) and FLockBar then
    Exit;

  // Change the focus to the splitter itself, so that it can trap the Escape
  // key to get out of dragging mode.  This is not necessary when RealTimeDrag
  // is True, because there is nothing to cancel.
  if not ( csDesigning in ComponentState ) and not FRealTimeDrag then
    MouseDownSetFocus;


  SetCapture( Handle );
  FSliding := True;

  if FHotSpotVisible and PtInRect( FHotSpotRect, CursorPosition ) then
    FHotSpotting := True;

  if FOrientation = orVertical then
    FCenterOffset := Y - ( Height div 2 )
  else
    FCenterOffset := X - ( Width div 2 );

  AllocateMaskDC;
  FOrigPos := FPosition;
  DrawMask( FPosition );
  FLastPos := FPosition;
end;


procedure TRzSplitter.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  P, Size, MousePos: Integer;
begin
  inherited;

  if FHotSpotting and FLockBar then
    Exit;

  if FSliding then
  begin
    if FOrientation = orVertical then
    begin
      Size := Height;
      MousePos := Y;
    end
    else
    begin
      Size := Width;
      MousePos := X;
    end;

    P := FOrigPos + MousePos - ( Size div 2 ) - FCenterOffset;

    CheckPosition( P );

    if CanChange( P ) then
    begin
      if P <> FLastPos then
      begin
        FHotSpotClosed := False;
        FHotSpotClosedToMin := False;
        FHotSpotClosedToMax := False;
        FHotSpotting := False;

        DrawMask( FLastPos );
        DrawMask( P );

        if FRealTimeDrag then
        begin
          Position := P;
          if FFixedPane = fpUpperLeft then
            FManualPosition := P
          else
            FManualPosition := Size - P;
        end;
      end;
      FLastPos := P;
    end;
  end;
end; {= TRzSplitter.MouseMove =}


procedure TRzSplitter.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  ToMin: Boolean;

  function ClickedUpperLeftOnHotSpot: Boolean;
  begin
    if FOrientation = orHorizontal then
      Result := Y < ( FHotSpotRect.Top + ( FHotSpotRect.Bottom - FHotSpotRect.Top ) div 2 )
    else
      Result := X < ( FHotSpotRect.Left + ( FHotSpotRect.Right - FHotSpotRect.Left ) div 2 );
  end;

begin
  inherited;

  if ( FSliding and ( Button = mbLeft ) ) or FHotSpotting then
  begin
    FSliding := False;
    ReleaseCapture;

    if not FRealTimeDrag then
    begin
      DrawMask( FLastPos );
      Position := FLastPos;
      UpdateManualPosition( FLastPos );
    end;

    if FHotSpotting then
    begin
      FHotSpotting := False;
      if PtInRect( FHotSpotRect, Point( X, Y ) ) then
      begin
        FHotSpotClosed := not FHotSpotClosed;
        if FHotSpotClosed then
        begin
          FHotSpotPosition := FPosition;

          // Check which side of the hot spot was clicked
          ToMin := ( FHotSpotDirection = hsdMin ) or
                   ( ( FHotSpotDirection = hsdBoth ) and ClickedUpperLeftOnHotSpot );

          if ToMin then
          begin
            FHotSpotClosedToMin := True;
            Position := 0;
          end
          else
          begin
            FHotSpotClosedToMax := True;
            Position := MaxInt;
          end;
        end
        else
        begin
          FHotSpotClosedToMin := False;
          FHotSpotClosedToMax := False;
          Position := FHotSpotPosition;
          UpdateManualPosition( FPosition );
        end;
        HotSpotClick;
      end;
    end;
    ReleaseMaskDC;
    FRefreshHotSpot := True;
  end;
end; {= TRzSplitter.MouseUp =}


procedure TRzSplitter.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if ( Key = vk_Escape ) and FSliding then
  begin
    FSliding := False;
    ReleaseCapture;
    if not FRealTimeDrag then
    begin
      DrawMask( FLastPos );
      UpdateManualPosition( Position );
    end;
  end;
end;


function TRzSplitter.GetBorderOffset: Integer;

  procedure AdjustForSides( var Offset: Integer; N: Integer );
  begin
    if sdLeft in BorderSides then
      Inc( Offset, N );
    if sdTop in BorderSides then
      Inc( Offset, N );
  end;

begin
  Result := BorderWidth;

  if BorderOuter = fsFlat then
    AdjustForSides( Result, 1 )
  else if BorderOuter in [ fsStatus, fsPopup ] then
    AdjustForSides( Result, BevelWidth )
  else if BorderOuter in [ fsGroove..fsButtonUp ] then
    AdjustForSides( Result, 2 );

  if BorderInner = fsFlat then
    AdjustForSides( Result, 1 )
  else if BorderInner in [ fsStatus, fsPopup ] then
    AdjustForSides( Result, BevelWidth )
  else if BorderInner in [ fsGroove..fsButtonUp ] then
    AdjustForSides( Result, 2 );
end;


procedure TRzSplitter.AllocateMaskDC;
begin
  if RunningAtLeast( WinVista ) then
    FMaskDC := GetDCEx( Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE )
  else
    FMaskDC := GetDC( 0 );

  if not FRealTimeDrag then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap( clBlack, clWhite );
    end;
    FPrevBrush := SelectObject( FMaskDC, FBrush.Handle );
  end;
end;


procedure TRzSplitter.DrawMask( NewPos: Integer );
var
  P: TPoint;
begin
  if FRealTimeDrag then
    Exit;

  P.X := GetBorderOffset;
  P.Y := P.X;

  if FOrientation = orVertical then
    P.Y := P.Y + NewPos
  else
    P.X := P.X + NewPos;

  if RunningAtLeast( WinVista ) then
    P := ClientToParent( P )
  else
    P := ClientToScreen( P );

  PatBlt( FMaskDC, P.X, P.Y,
          FBarRect.Right - FBarRect.Left,
          FBarRect.Bottom - FBarRect.Top, patInvert );
end; {= TRzSplitter.DrawMask =}



procedure TRzSplitter.ReleaseMaskDC;
begin
  if FPrevBrush <> 0 then
    SelectObject( FMaskDC, FPrevBrush );

  if RunningAtLeast( WinVista ) then
    ReleaseDC( Parent.Handle, FMaskDC )
  else
    ReleaseDC( 0, FMaskDC );

  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;


procedure TRzSplitter.DrawHotSpot( Highlight: Boolean; P: TPoint );
var
  R, R2: TRect;
  MidX, MidY, I: Integer;
  HSColor, HSDotColor, HSFrameColor, HSHighlight: TColor;

  procedure DrawDownArrow( X, Y: Integer );
  begin
    Canvas.Polygon( [ Point( X - 2, Y - 1 ),
                      Point( X,     Y + 1 ),
                      Point( X + 2, Y - 1 ) ] );
  end;

  procedure DrawUpArrow( X, Y: Integer );
  begin
    Canvas.Polygon( [ Point( X - 2, Y + 1 ),
                      Point( X,     Y - 1 ),
                      Point( X + 2, Y + 1 ) ] );
  end;

  procedure DrawLeftArrow( X, Y: Integer );
  begin
    Canvas.Polygon( [ Point( X + 1, Y - 2 ),
                      Point( X - 1, Y ),
                      Point( X + 1, Y + 2 ) ] );
  end;

  procedure DrawRightArrow( X, Y: Integer );
  begin
    Canvas.Polygon( [ Point( X - 1, Y - 2 ),
                      Point( X + 1, Y ),
                      Point( X - 1, Y + 2 ) ] );
  end;

begin
  if UsingSystemStyle then
  begin
    HSColor := FHotSpotColor;
    HSDotColor := FHotSpotDotColor;
    HSFrameColor := FHotSpotFrameColor;
    HSHighlight := FHotSpotHighlight;
  end
  else // VCL Styles
  begin
    HSColor := ActiveStyleColor( scPanel );
    HSDotColor := ActiveStyleSystemColor( clHighlight );
    HSFrameColor := ActiveStyleSystemColor( clBtnShadow );
    HSHighlight := BlendColors( clWhite, HSColor, 40 );
  end;


  R := DrawSides( Canvas, FHotSpotRect, HSFrameColor, HSFrameColor, sdAllSides );
  MidX := FHotSpotRect.Left + ( FHotSpotRect.Right - FHotSpotRect.Left ) div 2;
  MidY := FHotSpotRect.Top + ( FHotSpotRect.Bottom - FHotSpotRect.Top ) div 2;

  if FOrientation = orVertical then
  begin
    if FHotSpotDirection = hsdBoth then
    begin
      Canvas.Brush.Color := HSColor;
      Canvas.FillRect( R );
      if Highlight then
      begin
        R2 := R;
        if not FHotSpotClosed then
        begin
          if P.X < MidX then
            R2.Right := MidX
          else
            R2.Left := MidX;
        end;
        Canvas.Brush.Color := HSHighlight;
        Canvas.FillRect( R2 );
      end;
    end
    else
    begin
      if Highlight then
        Canvas.Brush.Color := HSHighlight
      else
        Canvas.Brush.Color := HSColor;
      Canvas.FillRect( R );
    end;

    if ( FHotSpotDirection = hsdBoth ) and not FHotSpotClosed then
    begin
      Canvas.Pen.Color := HSFrameColor;
      Canvas.MoveTo( MidX, FHotSpotRect.Top );
      Canvas.LineTo( MidX, FHotSpotRect.Bottom );
    end;

    Canvas.Pen.Color := HSDotColor;
    Canvas.Brush.Color := HSDotColor;

    case FHotSpotDirection of
      hsdMin:
      begin
        if FHotSpotClosed then
        begin
          DrawDownArrow( FHotSpotRect.Left + 8, MidY );
          DrawDownArrow( FHotSpotRect.Right - 8, MidY );
        end
        else
        begin
          DrawUpArrow( FHotSpotRect.Left + 8, MidY );
          DrawUpArrow( FHotSpotRect.Right - 8, MidY );
        end;
      end;

      hsdMax:
      begin
        if FHotSpotClosed then
        begin
          DrawUpArrow( FHotSpotRect.Left + 8, MidY );
          DrawUpArrow( FHotSpotRect.Right - 8, MidY );
        end
        else
        begin
          DrawDownArrow( FHotSpotRect.Left + 8, MidY );
          DrawDownArrow( FHotSpotRect.Right - 8, MidY );
        end;
      end;

      hsdBoth:
      begin
        if FHotSpotClosedToMin then
        begin
          DrawDownArrow( FHotSpotRect.Left + 8, MidY );
          DrawDownArrow( FHotSpotRect.Right - 8, MidY );
        end
        else if FHotSpotClosedToMax then
        begin
          DrawUpArrow( FHotSpotRect.Left + 8, MidY );
          DrawUpArrow( FHotSpotRect.Right - 8, MidY );
        end
        else
        begin
          DrawUpArrow( FHotSpotRect.Left + 8, MidY );
          DrawDownArrow( FHotSpotRect.Right - 8, MidY );
        end;
      end;
    end;

    { Draw Dots }
    for I := 0 to ( FHotSpotRect.Right - FHotSpotRect.Left - 32 ) div 3  do
    begin
      if ( FHotSpotDirection <> hsdBoth ) or FHotSpotClosed or
         ( ( ( FHotSpotDirection = hsdBoth ) and ( Abs( FHotSpotRect.Left + 15 + I * 3 - MidX ) > 3 ) ) ) then  // Skip the middle
      begin
        Canvas.Pixels[ FHotSpotRect.Left + 15 + I * 3, MidY ] := HSDotColor;
      end;
    end;
  end
  else // Orientation = orHorizontal
  begin
    if FHotSpotDirection = hsdBoth then
    begin
      Canvas.Brush.Color := HSColor;
      Canvas.FillRect( R );
      if Highlight then
      begin
        R2 := R;
        if not FHotSpotClosed then
        begin
          if P.Y < MidY then
            R2.Bottom := MidY
          else
            R2.Top := MidY;
        end;
        Canvas.Brush.Color := HSHighlight;
        Canvas.FillRect( R2 );
      end;
    end
    else
    begin
      if Highlight then
        Canvas.Brush.Color := HSHighlight
      else
        Canvas.Brush.Color := HSColor;
      Canvas.FillRect( R );
    end;

    if ( FHotSpotDirection = hsdBoth ) and not FHotSpotClosed then
    begin
      Canvas.Pen.Color := HSFrameColor;
      Canvas.MoveTo( FHotSpotRect.Left, MidY );
      Canvas.LineTo( FHotSpotRect.Right, MidY );
    end;

    Canvas.Pen.Color := HSDotColor;
    Canvas.Brush.Color := HSDotColor;

    case FHotSpotDirection of
      hsdMin:
      begin
        if FHotSpotClosed then
        begin
          DrawRightArrow( MidX, FHotSpotRect.Top + 8 );
          DrawRightArrow( MidX, FHotSpotRect.Bottom - 8 );
        end
        else
        begin
          DrawLeftArrow( MidX, FHotSpotRect.Top + 8 );
          DrawLeftArrow( MidX, FHotSpotRect.Bottom - 8 );
        end;
      end;

      hsdMax:
      begin
        if FHotSpotClosed then
        begin
          DrawLeftArrow( MidX, FHotSpotRect.Top + 8 );
          DrawLeftArrow( MidX, FHotSpotRect.Bottom - 8 );
        end
        else
        begin
          DrawRightArrow( MidX, FHotSpotRect.Top + 8 );
          DrawRightArrow( MidX, FHotSpotRect.Bottom - 8 );
        end;
      end;

      hsdBoth:
      begin
        if FHotSpotClosedToMin then
        begin
          DrawRightArrow( MidX, FHotSpotRect.Top + 8 );
          DrawRightArrow( MidX, FHotSpotRect.Bottom - 8 );
        end
        else if FHotSpotClosedToMax then
        begin
          DrawLeftArrow( MidX, FHotSpotRect.Top + 8 );
          DrawLeftArrow( MidX, FHotSpotRect.Bottom - 8 );
        end
        else
        begin
          DrawLeftArrow( MidX, FHotSpotRect.Top + 8 );
          DrawRightArrow( MidX, FHotSpotRect.Bottom - 8 );
        end;
      end;
    end;

    { Draw Dots }
    for I := 0 to ( FHotSpotRect.Bottom - FHotSpotRect.Top - 32 ) div 3  do
    begin
      if ( FHotSpotDirection <> hsdBoth ) or FHotSpotClosed or
         ( ( FHotSpotDirection = hsdBoth ) and ( Abs( FHotSpotRect.Top + 15 + I * 3 - MidY ) > 3 ) ) then  // Skip the middle
      begin
        Canvas.Pixels[ MidX, FHotSpotRect.Top + 15 + I * 3 ] := HSDotColor;
      end;
    end;
  end;
end; {= TRzSplitter.DrawHotSpot =}


procedure TRzSplitter.DrawSplitterBar;
var
  R1, R2: TRect;
  P: TPoint;
begin
  if FSplitterStyle in [ ssGroove, ssBump ] then
  begin
    Canvas.Pen.Style := psClear;
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.Rectangle( FBarRect.Left, FBarRect.Top, FBarRect.Right + 1, FBarRect.Bottom + 1 );
    Canvas.Pen.Style := psSolid;

    R1 := FBarRect;
    if FOrientation = orVertical then
    begin
      R1.Top := FBarRect.Top + ( FBarRect.Bottom - FBarRect.Top ) div 2 - 3;
      R1.Bottom := R1.Top + 3;
      R2 := R1;
      OffsetRect( R2, 0, 3 );
    end
    else
    begin
      R1.Left := FBarRect.Left + ( FBarRect.Right - FBarRect.Left ) div 2 - 3;
      R1.Right := R1.Left + 3;
      R2 := R1;
      OffsetRect( R2, 3, 0 );
    end;

    if FSplitterStyle = ssBump then
    begin
      DrawBorder( Canvas, R1, fsPopup );
      DrawBorder( Canvas, R2, fsPopup );
    end
    else
    begin
      DrawBorder( Canvas, R1, fsStatus );
      DrawBorder( Canvas, R2, fsStatus );
    end;
  end
  else if ( VisualStyle <> vsClassic ) and ( FSplitterStyle = ssGroupBar ) then
  begin
    DrawGroupBarBackground( Canvas, FBarRect, VisualStyle,
                            GradientColorStyle, FGradientPath,
                            GradientColorStart, GradientColorStop );
  end
  else if not FullRepaint then
  begin
    // Fill background of splitter bar
    Canvas.Brush.Color := Color;
    Canvas.FillRect( FBarRect );
  end;

  if FHotSpotVisible then
  begin
    if FRefreshHotSpot then
    begin
      P := CursorPosition;
      // Only draw HotSpot when a change takes place
      if PtInRect( FHotSpotRect, P ) then
        DrawHotSpot( True, P )
      else
        DrawHotSpot( False, P );
      FRefreshHotSpot := False;
    end
    else
      DrawHotSpot( False, Point( 0, 0 ) );
  end;
end; {= TRzSplitter.DrawSplitterBar =}


procedure TRzSplitter.Paint;
begin
  inherited;
  DrawSplitterBar;
end;


// WMPaint needs to be overridden here in order to handle the case
// where DoubleBuffer is set to True.  The original TWinControl.WMPaint
// method uses ClientRect to create the compatible bitmap and this is not
// big enough b/c the splitter adjusts the ClientRect to its interior.
// Instead, the following code uses the GetControlRect inherited method.

procedure TRzSplitter.WMPaint( var Msg: TWMPaint );
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R: TRect;
begin
  if not FDoubleBuffered or ( Msg.DC <> 0 ) then
  begin
    if not ( csCustomPaint in ControlState ) and ( ControlCount = 0 ) then
      inherited
    else
      PaintHandler( Msg );
  end
  else
  begin
    DC := GetDC( 0 );
    R := GetControlRect;
    MemBitmap := CreateCompatibleBitmap( DC, R.Right, R.Bottom );
    ReleaseDC( 0, DC );
    MemDC := CreateCompatibleDC( 0 );
    OldBitmap := SelectObject( MemDC, MemBitmap );
    try
      DC := BeginPaint( Handle, PS );
      Perform( wm_EraseBkgnd, WPARAM( MemDC ), LPARAM( MemDC ) );
      Msg.DC := MemDC;
      WMPaint( Msg );
      Msg.DC := 0;
      BitBlt( DC, 0, 0, R.Right, R.Bottom, MemDC, 0, 0, SRCCOPY );
      EndPaint( Handle, PS );
    finally
      SelectObject( MemDC, OldBitmap );
      DeleteDC( MemDC );
      DeleteObject( MemBitmap );
    end;
  end;
end;


procedure TRzSplitter.WMShowWindow( var Msg: TWMShowWindow);
begin
  inherited;
  if not ( csDestroying in ComponentState ) then
  begin
    LowerRight.Pane.Invalidate;
    UpperLeft.Pane.Invalidate;
  end;
end;



{=====================================}
{== TRzSizePanelDockManager Methods ==}
{=====================================}

constructor TRzSizePanelDockManager.Create( DockSite: TWinControl );
begin
  inherited;
  FSizePanel := DockSite as TRzCustomSizePanel;
end;


procedure TRzSizePanelDockManager.PositionDockRect( Client, DropCtl: TControl; DropAlign: TAlign; var DockRect: TRect );
begin
  inherited;

  if FSizePanel.Align = alRight then
    Inc( DockRect.Left, FSizePanel.SizeBarWidth )
  else if FSizePanel.Align = alBottom then
    Inc( DockRect.Top, FSizePanel.SizeBarWidth );
end;


procedure TRzSizePanelDockManager.PaintSite( DC: HDC );
var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
begin
  if FSizePanel.HotSpotClosed then
    Exit;
  Canvas := TControlCanvas.Create;
  try
    Canvas.Control := FSizePanel;
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        FSizePanel.DrawSizeBar;

        for I := 0 to FSizePanel.ControlCount - 1 do
        begin
          Control := FSizePanel.Controls[ I ];
          if Control.Visible then
          begin
            R := Control.BoundsRect;
            AdjustDockRect( Control, R );

            Dec( R.Left, 2 * ( R.Left - Control.Left ) );
            Dec( R.Top, 2 * ( R.Top - Control.Top ) );
            Dec( R.Right, 2 * ( Control.Width - ( R.Right - R.Left ) ) );
            Dec( R.Bottom, 2 * ( Control.Height - ( R.Bottom - R.Top ) ) );

            { To compensate for problem in TDockZone }
            if FSizePanel.Align = alRight then
            begin
              Inc( R.Right, FSizePanel.SizeBarWidth );
              Dec( R.Left, FSizePanel.SizeBarWidth );
            end
            else if FSizePanel.Align = alBottom then
            begin
              Inc( R.Bottom, FSizePanel.SizeBarWidth );
              Dec( R.Top, FSizePanel.SizeBarWidth );
            end;

            PaintDockFrame( Canvas, Control, R );
          end;
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    Canvas.Free;
  end;
end;


type
  TTextControl = class( TControl )
  end;

procedure TRzSizePanelDockManager.PaintDockFrame( Canvas: TCanvas; Control: TControl; const ARect: TRect );
var
  R: TRect;
  S: string;
  W: Integer;
begin
  if not FSizePanel.ShowDockClientCaptions then
    inherited
  else
  begin
    S := TTextControl( Control ).Text;
    W := FSizePanel.SizeBarWidth;
    Canvas.Font := FFont;

    if Control is TWinControl then
    begin
      if TWinControl( Control ).Focused then
      begin
        Canvas.Brush.Color := clActiveCaption;
        Canvas.Font.Color := clCaptionText;
      end
      else
      begin
        Canvas.Brush.Color := clInactiveCaption;
        Canvas.Font.Color := clInactiveCaptionText;
      end;
    end
    else
    begin
      Canvas.Brush.Color := clActiveCaption;
      Canvas.Font.Color := clCaptionText;
    end;

    case FSizePanel.Align of
      alLeft:
      begin
        R := Rect( ARect.Left, ARect.Top, ARect.Right, ARect.Top + FGrabberSize );
        Canvas.TextRect( R, R.Left + 2, R.Top, S );

        // Draw the Close X
        Canvas.Font.Name := FCloseFont.Name;
        R := Rect( ARect.Right - FGrabberSize - 1, ARect.Top + 1, ARect.Right - 1, ARect.Top + 12 );
        Canvas.TextRect( R, R.Left, R.Top, 'r' );
      end;

      alTop:
      begin
        R := Rect( ARect.Left, ARect.Top, ARect.Left + FGrabberSize, ARect.Bottom );
        DrawVertTitle( Canvas, S, R );

        // Draw the Close X
        Canvas.Font.Name := FCloseFont.Name;
        R := Rect( ARect.Left + 1, ARect.Top + 1, ARect.Left + FGrabberSize - 1, ARect.Top + 13 );
        Canvas.TextRect( R, R.Left, R.Top, 'r' );
      end;

      alRight:
      begin
        R := Rect( ARect.Left + W, ARect.Top, ARect.Right - W, ARect.Top + FGrabberSize );
        Canvas.TextRect( R, R.Left + 2, R.Top, S );

        // Draw the Close X
        Canvas.Font.Name := FCloseFont.Name;
        R := Rect( ARect.Right - FGrabberSize - W - 1, ARect.Top + 1, ARect.Right - W - 1, ARect.Top + 12 );
        Canvas.TextRect( R, R.Left, R.Top, 'r' );
      end;

      alBottom:
      begin
        R := Rect( ARect.Left, ARect.Top + W, ARect.Left + FGrabberSize, ARect.Bottom - W );
        DrawVertTitle( Canvas, S, R );

        // Draw the Close X
        Canvas.Font.Name := FCloseFont.Name;
        R := Rect( ARect.Left + 1, ARect.Top + W + 1, ARect.Left + FGrabberSize - 1, ARect.Top + W + 13 );
        Canvas.TextRect( R, R.Left, R.Top, 'r' );
      end;
    end;
  end;
end; {= TRzSizePanelDockManager.PaintDockFrame =}



{================================}
{== TRzCustomSizePanel Methods ==}
{================================}

constructor TRzCustomSizePanel.Create( AOwner: TComponent );
begin
  inherited;

  BorderOuter := fsNone;

  FSizeBarWidth := 4;
  FSizeBarStyle := ssStandard;
  FMarginMin := 0;
  FMarginMax := 0;

  FMarginOffset := 1;

  { Setup Sizing Bar }
  FRealTimeDrag := False;
  FResizing := False;
  FLockBar := False;

  FHotSpotVisible := False;
  FHotSpotHighlight := clWindow;
  FHotSpotColor := clBtnFace;
  FHotSpotDotColor := clHighlight;
  FHotSpotFrameColor := clBtnShadow;
  FHotSpotClosed := False;
  FHotSpotting := False;
  FHotSpotPosition := 100;
  FHotSpotSizePercent := 40;
  FHotSpotIgnoreMargins := False;
  FOutsideHotSpot := True;
  FSide := sdLeft;

  FHorzCursor := LoadCursor( HInstance, 'RZSPLIT_HORZ' );
  FHorzCursorHotSpot := LoadCursor( HInstance, 'RZSPLIT_HORZHOTSPOT' );
  FVertCursor := LoadCursor( HInstance, 'RZSPLIT_VERT' );
  FVertCursorHotSpot := LoadCursor( HInstance, 'RZSPLIT_VERTHOTSPOT' );

  FGradientColorAdjustment := 30;

  inherited Align := alLeft;
  {&RCI}
end;


destructor TRzCustomSizePanel.Destroy;
begin
  DestroyCursor( FHorzCursor );
  DestroyCursor( FHorzCursorHotSpot );
  DestroyCursor( FVertCursor );
  DestroyCursor( FVertCursorHotSpot );
  FBrush.Free;

  inherited;
end;


procedure TRzCustomSizePanel.Loaded;
begin
  inherited;
  SetMarginMin( FMarginMin );
end;


procedure TRzCustomSizePanel.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineProperty( 'HotSpotClosed', ReadHotSpotClosed, WriteHotSpotClosed, FHotSpotClosed );
  Filer.DefineProperty( 'HotSpotPosition', ReadHotSpotPosition, WriteHotSpotPosition, FHotSpotClosed );
end;


procedure TRzCustomSizePanel.ReadHotSpotClosed( Reader: TReader );
begin
  FHotSpotClosed := Reader.ReadBoolean;
end;

procedure TRzCustomSizePanel.WriteHotSpotClosed( Writer: TWriter );
begin
  Writer.WriteBoolean( FHotSpotClosed );
end;


procedure TRzCustomSizePanel.ReadHotSpotPosition( Reader: TReader );
begin
  FHotSpotPosition := Reader.ReadInteger;
end;

procedure TRzCustomSizePanel.WriteHotSpotPosition( Writer: TWriter );
begin
  Writer.WriteInteger( FHotSpotPosition );
end;


function TRzCustomSizePanel.HotSpotTopBottom: Boolean;
begin
  Result := ( Align in [ alTop, alBottom ] ) or
            ( ( Align in [ alNone, alCustom ] ) and
              ( FSide in [ sdTop, sdBottom ] ) );
end;


procedure TRzCustomSizePanel.UpdateHotSpotRect( BarRect: TRect );
var
  Mid, S: Integer;
begin
  with BarRect do
  begin
    if HotSpotTopBottom then
    begin
      Mid := Left + ( Right - Left ) div 2;
      S := Round ( FHotSpotSizePercent / 100 * ( Right - Left ) ) div 2;
      FHotSpotRect := Rect( Mid - S, Top, Mid + S, Bottom  );
    end
    else
    begin
      Mid := Top + ( Bottom - Top ) div 2;
      S := Round ( FHotSpotSizePercent / 100 * ( Bottom - Top ) ) div 2;
      FHotSpotRect := Rect( Left, Mid - S, Right, Mid + S );
    end;
  end;
end;


function TRzCustomSizePanel.GetSizeBarRect: TRect;
var
  Offset: Integer;
begin
  if DockSite then
    Offset := 1
  else
    Offset := 0;

  case Align of
    alTop:
      Result := Rect( 0, Height - SizeBarWidth - Offset, Width, Height );

    alBottom:
      Result := Rect( 0, 0, Width, SizeBarWidth + Offset );

    alLeft:
      Result := Rect( Width - SizeBarWidth - Offset, 0, Width, Height );

    alRight:
      Result := Rect( 0, 0, SizeBarWidth + Offset, Height );

    alNone, alCustom:
    begin
      case FSide of
        sdTop:
          Result := Rect( 0, Height - SizeBarWidth - Offset, Width, Height );

        sdBottom:
          Result := Rect( 0, 0, Width, SizeBarWidth + Offset );

        sdLeft:
          Result := Rect( Width - SizeBarWidth - Offset, 0, Width, Height );

        sdRight:
          Result := Rect( 0, 0, SizeBarWidth + Offset, Height );
      end;
    end;

    else
      Result := Rect( 0, 0, 0, 0 );
  end;
  UpdateHotSpotRect( Result );
end;


procedure TRzCustomSizePanel.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
var
  Rect: TRect;
begin
  if FullRepaint or ( Caption <> '' ) then
    Invalidate
  else
  begin
    if BorderWidth > 0 then
    begin
      Rect.Right := Width;
      Rect.Bottom := Height;
      if Msg.WindowPos^.cx <> Rect.Right then
      begin
        Rect.Top := 0;
        Rect.Left := Rect.Right - BorderWidth - 1;
        InvalidateRect( Handle, @Rect, True );
      end;
      if Msg.WindowPos^.cy <> Rect.Bottom then
      begin
        Rect.Left := 0;
        Rect.Top := Rect.Bottom - BorderWidth - 1;
        InvalidateRect( Handle, @Rect, True );
      end;
    end;
  end;

  inherited;

  if not ( csLoading in ComponentState ) then
    Resize;
end; {= TRzCustomSizePanel.WMWindowPosChanged =}


procedure TRzCustomSizePanel.FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean );
begin
  // If the hot spot is closed, then do not adjust the client rect b/c it will
  // cause the contents of the size panel to cover the hot spot.
  if not FHotSpotClosed then
    inherited;
end;



function TRzCustomSizePanel.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;

  { The (+1) is important--it allows the size panel to completely
    hide its contents when the size panel is closed. }

  case Align of
    alTop:
      Dec( Result.Bottom, FSizeBarWidth + 1 );

    alBottom:
      Inc( Result.Top, FSizeBarWidth + 1 );

    alLeft:
      Dec( Result.Right, FSizeBarWidth + 1 );

    alRight:
      Inc( Result.Left, FSizeBarWidth + 1 );

    alNone, alCustom:
    begin
      case FSide of
        sdTop:
          Dec( Result.Bottom, FSizeBarWidth + 1 );

        sdBottom:
          Inc( Result.Top, FSizeBarWidth + 1 );

        sdLeft:
          Dec( Result.Right, FSizeBarWidth + 1 );

        sdRight:
          Inc( Result.Left, FSizeBarWidth + 1 );
      end;
    end;
  end;
end;


function TRzCustomSizePanel.GetControlRect: TRect;
begin
  Result := inherited GetControlRect;
  case Align of
    alTop:
      Dec( Result.Bottom, FSizeBarWidth );

    alBottom:
      Inc( Result.Top, FSizeBarWidth );

    alLeft:
      Dec( Result.Right, FSizeBarWidth );

    alRight:
      Inc( Result.Left, FSizeBarWidth );

    alNone, alCustom:
    begin
      case FSide of
        sdTop:
          Dec( Result.Bottom, FSizeBarWidth );

        sdBottom:
          Inc( Result.Top, FSizeBarWidth );

        sdLeft:
          Dec( Result.Right, FSizeBarWidth );

        sdRight:
          Inc( Result.Left, FSizeBarWidth );
      end;
    end;
  end;
end;


function TRzCustomSizePanel.CreateDockManager: IDockManager;
begin
  if ( DockManager = nil ) and DockSite and UseDockManager then
    Result := TRzSizePanelDockManager.Create( Self )
  else
    Result := DockManager;
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;


procedure TRzCustomSizePanel.CMDesignHitTest( var Msg: TCMDesignHitTest );
begin
  if FResizing or PtInRect( GetSizeBarRect, CursorPosition ) then
    Msg.Result := 1         { Allow splitter to be moved w/ LMB at design-time }
  else
  Msg.Result := 0;
end;


procedure TRzCustomSizePanel.UpdateHotSpotHighlight;
begin
  if FHotSpotVisible then
  begin
    // Only Draw HotSpot when a change takes place
    if PtInRect( FHotSpotRect, CursorPosition ) then
    begin
      if FOutsideHotSpot then
        DrawHotSpot( True );
      FOutsideHotSpot := False;
    end
    else
    begin
      if not FOutsideHotSpot then
        DrawHotSpot( False );
      FOutsideHotSpot := True;
    end;
  end;
end;


procedure TRzCustomSizePanel.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  // HotSpotting is allowed to work when LockBar is True.  However, we must
  // prevent the HotSpot from being redrawn if the user tries to drage the
  // HotSpot.

  if FHotSpotting and FLockBar then
    Exit;

  UpdateHotSpotHighlight;
end;


procedure TRzCustomSizePanel.WMSetCursor( var Msg: TWMSetCursor );
begin
  UpdateHotSpotHighlight;

  if PtInRect( GetSizeBarRect, CursorPosition ) and not FLockBar then
  begin
    if FHotSpotVisible and PtInRect( FHotSpotRect, CursorPosition ) then
    begin
      if HotSpotTopBottom then
        SetCursor( FVertCursorHotSpot )
      else
        SetCursor( FHorzCursorHotSpot );
      Exit;
    end
    else
    begin
      if HotSpotTopBottom then
        SetCursor( FVertCursor )
      else
        SetCursor( FHorzCursor );
      Exit;
    end;
  end;

  inherited;
end;


procedure TRzCustomSizePanel.MouseDownSetFocus;
begin
  SetFocus;
end;


procedure TRzCustomSizePanel.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  FPosition: Integer;
begin
  inherited;

  if not PtInRect( GetSizeBarRect, CursorPosition ) or ( Button <> mbLeft ) then
    Exit;

  if ( not FHotSpotVisible or not PtInRect( FHotSpotRect, CursorPosition ) ) and FLockBar then
    Exit;

  // Change the focus to the size panel itself, so that it can trap the Escape
  // key to get out of dragging mode.  This is not necessary when RealTimeDrag
  // is True, because there is nothing to cancel.
  if not ( csDesigning in ComponentState ) and not FRealTimeDrag then
    MouseDownSetFocus;

  SetCapture( Handle );
  FResizing := True;

  if FHotSpotVisible and PtInRect( FHotSpotRect, CursorPosition ) then
    FHotSpotting := True;

  if HotSpotTopBottom then
  begin
    FCenterOffset := Y - ( Height div 2 );
    if ( Align = alTop ) or ( ( Align in [ alNone, alCustom ] ) and ( FSide = sdTop ) ) then
      FPosition := Height
    else
      FPosition := 0;
  end
  else
  begin
    FCenterOffset := X - ( Width div 2 );
    if ( Align = alLeft ) or ( ( Align in [ alNone, alCustom ] ) and ( FSide = sdLeft ) ) then
      FPosition := Width
    else
      FPosition := 0;
  end;

  AllocateMaskDC;
  FOrigPos := FPosition;
  FOrigWidth := Width;
  FOrigHeight := Height;
  DrawMask( FPosition );
  FLastPos := FPosition;
end; {= TRzCustomSizePanel.MouseDown =}


function TRzCustomSizePanel.GetParentWorkingRect: TRect;
var
  I: Integer;
begin
  if Parent <> nil then
  begin
    Result := Parent.ClientRect;
    for I := 0 to Parent.ControlCount - 1 do
    begin
      case Parent.Controls[ I ].Align of
        alTop:
          Inc( Result.Top, Parent.Controls[ I ].Height );

        alBottom:
          Dec( Result.Bottom, Parent.Controls[ I ].Height );

        alLeft:
          Inc( Result.Left, Parent.Controls[ I ].Width );

        alRight:
          Dec( Result.Right, Parent.Controls[ I ].Width );

        alNone, alCustom:
        begin
          case FSide of
            sdTop:
              Inc( Result.Top, Parent.Controls[ I ].Height );

            sdBottom:
              Dec( Result.Bottom, Parent.Controls[ I ].Height );

            sdLeft:
              Inc( Result.Left, Parent.Controls[ I ].Width );

            sdRight:
              Dec( Result.Right, Parent.Controls[ I ].Width );
          end;
        end;
      end;
    end;
  end
  else
    raise Exception.Create( 'No parent specified' );
end; {= TRzCustomSizePanel.GetParentWorkingRect =}


procedure TRzCustomSizePanel.CheckPosition( var Value: Integer );
var
  R: TRect;
begin
  R := GetParentWorkingRect;

  case Align of
    alTop:
    begin
      if Value < FMarginMin + FSizeBarWidth + 1 then
        Value := FMarginMin + FSizeBarWidth + 1;

      if Value > R.Bottom - FMarginMax - Top then
        Value := R.Bottom - FMarginMax - Top;
    end;

    alBottom:
    begin
      if Value < R.Top + FMarginMax - Top then
        Value := R.Top + FMarginMax - Top;

      if Value > Height - FMarginMin - FSizeBarWidth - 1 then
        Value := Height - FMarginMin - FSizeBarWidth - 1;
    end;

    alLeft:
    begin
      if Value < FMarginMin + FSizeBarWidth + 1 then
        Value := FMarginMin + FSizeBarWidth + 1;

      if Value > R.Right - FMarginMax - Left then
        Value := R.Right - FMarginMax - Left;
    end;

    alRight:
    begin
      if Value < R.Left + FMarginMax - Left then
        Value := R.Left + FMarginMax - Left;

      if Value > Width - FMarginMin - FSizeBarWidth - 1 then
        Value := Width - FMarginMin - FSizeBarWidth - 1;
    end;

    alNone, alCustom:
    begin
      case FSide of
        sdTop:
        begin
          if Value < FMarginMin + FSizeBarWidth + 1 then
            Value := FMarginMin + FSizeBarWidth + 1;

          if Value > R.Bottom - FMarginMax - Top then
            Value := R.Bottom - FMarginMax - Top;
        end;

        sdBottom:
        begin
          if Value < R.Top + FMarginMax - Top then
            Value := R.Top + FMarginMax - Top;

          if Value > Height - FMarginMin - FSizeBarWidth - 1 then
            Value := Height - FMarginMin - FSizeBarWidth - 1;
        end;

        sdLeft:
        begin
          if Value < FMarginMin + FSizeBarWidth + 1 then
            Value := FMarginMin + FSizeBarWidth + 1;

          if Value > R.Right - FMarginMax - Left then
            Value := R.Right - FMarginMax - Left;
        end;

        sdRight:
        begin
          if Value < R.Left + FMarginMax - Left then
            Value := R.Left + FMarginMax - Left;

          if Value > Width - FMarginMin - FSizeBarWidth - 1 then
            Value := Width - FMarginMin - FSizeBarWidth - 1;
        end;
      end;
    end;

  end; { case }
end; {= TRzCustomSizePanel.CheckPosition =}



procedure TRzCustomSizePanel.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  P: Integer;
begin
  inherited;

  if FHotSpotting and FLockBar then
    Exit;

  if FResizing then
  begin
    if HotSpotTopBottom then
      P := FOrigPos + Y - ( FOrigHeight div 2 ) - FCenterOffset
    else
      P := FOrigPos + X - ( FOrigWidth div 2 ) - FCenterOffset;

    CheckPosition( P );

    if P <> FLastPos then
    begin
      case Align of
        alTop, alLeft:
        begin
          if P > FLastPos then
            FHotSpotClosed := False;
          FHotSpotting := False;
        end;

        alBottom, alRight:
        begin
          if P < FLastPos then
            FHotSpotClosed := False;
          FHotSpotting := False;
        end;

        alNone, alCustom:
        begin
          case FSide of
            sdTop, sdLeft:
            begin
              if P > FLastPos then
                FHotSpotClosed := False;
              FHotSpotting := False;
            end;

            sdBottom, sdRight:
            begin
              if P < FLastPos then
                FHotSpotClosed := False;
              FHotSpotting := False;
            end;
          end;
        end;
      end;

      DrawMask( FLastPos );
      DrawMask( P );

      if FRealTimeDrag then
      begin
        case Align of
          alTop:
            Height := P + FMarginOffset;

          alBottom:
            Height := FOrigPos - P - FMarginOffset + Height;

          alLeft:
            Width := P + FMarginOffset;

          alRight:
            Width := FOrigPos - P - FMarginOffset + Width;

          alNone, alCustom:
          begin
            case FSide of
              sdTop:
                Height := P + FMarginOffset;

              sdBottom:
                Height := FOrigPos - P - FMarginOffset + Height;

              sdLeft:
                Width := P + FMarginOffset;

              sdRight:
                Width := FOrigPos - P - FMarginOffset + Width;
            end;
          end;
        end;

        if Align in [ alTop, alBottom, alLeft, alRight ] then
          UpdateObjectInspector( Self );
      end;
    end;
    FLastPos := P;
  end;
end; {= TRzCustomSizePanel.MouseMove =}


procedure TRzCustomSizePanel.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  {&RV}
  inherited;

  if ( FResizing and ( Button = mbLeft ) ) or FHotSpotting then
  begin
    FResizing := False;
    ReleaseCapture;

    if not FRealTimeDrag then
    begin
      DrawMask( FLastPos );
      case Align of
        alTop:
          Height := FLastPos + FMarginOffset - 1;

        alBottom:
          Height := FOrigPos - FLastPos + Height;

        alLeft:
          Width := FLastPos + FMarginOffset - 1;

        alRight:
          Width := FOrigPos - FLastPos + Width;

        alNone, alCustom:
        begin
          case FSide of
            sdTop:
              Height := FLastPos + FMarginOffset - 1;

            sdBottom:
              Height := FOrigPos - FLastPos + Height;

            sdLeft:
              Width := FLastPos + FMarginOffset - 1;

            sdRight:
              Width := FOrigPos - FLastPos + Width;
          end;
        end;
      end;

      UpdateObjectInspector( Self );
    end;


    if FHotSpotting then
    begin
      FHotSpotting := False;
      if PtInRect( FHotSpotRect, Point( X, Y ) ) then
      begin
        FHotSpotClosed := not FHotSpotClosed;
        if FHotSpotClosed then
        begin
          if HotSpotTopBottom then
          begin
            FHotSpotPosition := Height;
            if FHotSpotIgnoreMargins then
              Height := FSizeBarWidth + FMarginOffset
            else
              Height := FMarginMin + FSizeBarWidth + FMarginOffset;
          end
          else
          begin
            FHotSpotPosition := Width;
            if FHotSpotIgnoreMargins then
              Width := FSizeBarWidth + FMarginOffset
            else
              Width := FMarginMin + FSizeBarWidth + FMarginOffset;
          end;
        end
        else
        begin
          if Align in [ alNone, alCustom ] then
          begin
            if FSide in [ sdLeft, sdTop ] then
              CheckPosition( FHotSpotPosition );
          end
          else if Align in [ alLeft, alTop ] then
            CheckPosition( FHotSpotPosition );

          if Align in [ alNone, alCustom ] then
          begin
            if FSide in [ sdTop, sdBottom ] then
              Height := FHotSpotPosition
            else
              Width := FHotSpotPosition;
          end
          else if Align in [ alTop, alBottom ] then
            Height := FHotSpotPosition
          else
            Width := FHotSpotPosition;
        end;
        HotSpotClick;
      end;
    end;
    ReleaseMaskDC;
    FRefreshHotSpot := True;
  end;
end; {= TRzCustomSizePanel.MouseUp =}


procedure TRzCustomSizePanel.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if ( Key = vk_Escape ) and FResizing then
  begin
    FResizing := False;
    ReleaseCapture;
    if not FRealTimeDrag then
      DrawMask( FLastPos );
  end;
end;


procedure TRzCustomSizePanel.HotSpotClick;
begin
  if Assigned( FOnHotSpotClick ) then
    FOnHotSpotClick( Self );
end;


procedure TRzCustomSizePanel.AllocateMaskDC;
begin
  if RunningAtLeast( WinVista ) then
    FMaskDC := GetDCEx( Parent.Handle, 0, DCX_CACHE or DCX_CLIPSIBLINGS or DCX_LOCKWINDOWUPDATE )
  else
    FMaskDC := GetDC( 0 );

  if not FRealTimeDrag then
  begin
    if FBrush = nil then
    begin
      FBrush := TBrush.Create;
      FBrush.Bitmap := AllocPatternBitmap( clBlack, clWhite );
    end;
    FPrevBrush := SelectObject( FMaskDC, FBrush.Handle );
  end;
end;


procedure TRzCustomSizePanel.DrawMask( NewPos: Integer );
var
  P: TPoint;
  R: TRect;
begin
  if FRealTimeDrag then
    Exit;

  if RunningAtLeast( WinVista ) then
  begin
    P.X := Left;
    P.Y := Top;
  end
  else
  begin
    P.X := 0;
    P.Y := 0;
  end;

  case Align of
    alTop:
      P.Y := P.Y + NewPos - FSizeBarWidth;

    alBottom:
      P.Y := P.Y + NewPos + 1;

    alLeft:
      P.X := P.X + NewPos - FSizeBarWidth;

    alRight:
      P.X := P.X + NewPos + 1;

    alNone, alCustom:
    begin
      case FSide of
        sdTop:
          P.Y := P.Y + NewPos - FSizeBarWidth;

        sdBottom:
          P.Y := P.Y + NewPos + 1;

        sdLeft:
          P.X := P.X + NewPos - FSizeBarWidth;

        sdRight:
          P.X := P.X + NewPos + 1;
      end;
    end;
  end;

  if not RunningAtLeast( WinVista ) then
    P := ClientToScreen( P );

  R := GetSizeBarRect;
  PatBlt( FMaskDC, P.X, P.Y, R.Right - R.Left, R.Bottom - R.Top, patInvert );
end; {= TRzCustomSizePanel.DrawMask =}


procedure TRzCustomSizePanel.ReleaseMaskDC;
begin
  if FPrevBrush <> 0 then
    SelectObject( FMaskDC, FPrevBrush );

  if RunningAtLeast( WinVista ) then
    ReleaseDC( Parent.Handle, FMaskDC )
  else
    ReleaseDC( 0, FMaskDC );

  if FBrush <> nil then
  begin
    FBrush.Free;
    FBrush := nil;
  end;
end;


procedure TRzCustomSizePanel.DrawHotSpot( Highlight: Boolean );
var
  R: TRect;
  Mid, I: Integer;
  HSColor, HSDotColor, HSFrameColor, HSHighlight: TColor;
begin
  if UsingSystemStyle then
  begin
    HSColor := FHotSpotColor;
    HSDotColor := FHotSpotDotColor;
    HSFrameColor := FHotSpotFrameColor;
    HSHighlight := FHotSpotHighlight;
  end
  else // VCL Styles
  begin
    HSColor := ActiveStyleColor( scPanel );
    HSDotColor := ActiveStyleSystemColor( clHighlight );
    HSFrameColor := ActiveStyleSystemColor( clBtnShadow );
    HSHighlight := BlendColors( clWhite, HSColor, 40 );
  end;


  R := DrawSides( Canvas, FHotSpotRect, HSFrameColor, HSFrameColor, sdAllSides );
  if Highlight then
    Canvas.Brush.Color := HSHighlight
  else
    Canvas.Brush.Color := HSColor;
  Canvas.FillRect( R );

  Canvas.Pen.Color := HSDotColor;
  Canvas.Brush.Color := HSDotColor;

  if HotSpotTopBottom then
  begin
    Mid := FHotSpotRect.Top + ( FHotSpotRect.Bottom - FHotSpotRect.Top ) div 2;
    if ( ( Align = alTop ) and not FHotSpotClosed ) or
       ( ( Align = alBottom ) and FHotSpotClosed ) or
       ( ( Align in [ alNone, alCustom ] ) and
         ( ( ( FSide = sdTop ) and not FHotSpotClosed ) or
           ( ( FSide = sdBottom ) and FHotSpotClosed ) ) ) then
    begin
      { Draw /\ Arrows }
      Canvas.Polygon( [ Point( FHotSpotRect.Left + 6, Mid + 1 ),
                 Point( FHotSpotRect.Left + 8, Mid - 1 ),
                 Point( FHotSpotRect.Left + 10, Mid + 1 ) ] );
      Canvas.Polygon( [ Point( FHotSpotRect.Right - 6, Mid + 1 ),
                 Point( FHotSpotRect.Right - 8, Mid - 1 ),
                 Point( FHotSpotRect.Right - 10, Mid + 1 ) ] );
    end
    else
    begin
      { Draw \/ Arrows }
      Canvas.Polygon( [ Point( FHotSpotRect.Left + 6, Mid - 1 ),
                 Point( FHotSpotRect.Left + 8, Mid + 1 ),
                 Point( FHotSpotRect.Left + 10, Mid - 1 ) ] );
      Canvas.Polygon( [ Point( FHotSpotRect.Right - 6, Mid - 1 ),
                 Point( FHotSpotRect.Right - 8, Mid + 1 ),
                 Point( FHotSpotRect.Right - 10, Mid - 1 ) ] );
    end;

    { Draw Dots }
    for I := 0 to ( FHotSpotRect.Right - FHotSpotRect.Left - 32 ) div 3  do
    begin
      Canvas.Pixels[ FHotSpotRect.Left + 15 + I * 3, Mid ] := HSDotColor;
    end;
  end
  else
  begin
    Mid := FHotSpotRect.Left + ( FHotSpotRect.Right - FHotSpotRect.Left ) div 2;
    if ( ( Align = alLeft ) and not FHotSpotClosed ) or
       ( ( Align = alRight ) and FHotSpotClosed ) or
       ( ( Align in [ alNone, alCustom ] ) and
         ( ( ( FSide = sdLeft ) and not FHotSpotClosed ) or
           ( ( FSide = sdRight ) and FHotSpotClosed ) ) ) then
    begin
      { Draw < Arrows }
      Canvas.Polygon( [ Point( Mid + 1, FHotSpotRect.Top + 6 ),
                 Point( Mid - 1, FHotSpotRect.Top + 8 ),
                 Point( Mid + 1, FHotSpotRect.Top + 10 ) ] );
      Canvas.Polygon( [ Point( Mid + 1, FHotSpotRect.Bottom - 6 ),
                 Point( Mid - 1, FHotSpotRect.Bottom - 8 ),
                 Point( Mid + 1, FHotSpotRect.Bottom - 10 ) ] );
    end
    else
    begin
      { Draw > Arrows }
      Canvas.Polygon( [ Point( Mid - 1, FHotSpotRect.Top + 6 ),
                 Point( Mid + 1, FHotSpotRect.Top + 8 ),
                 Point( Mid - 1, FHotSpotRect.Top + 10 ) ] );
      Canvas.Polygon( [ Point( Mid - 1, FHotSpotRect.Bottom - 6 ),
                 Point( Mid + 1, FHotSpotRect.Bottom - 8 ),
                 Point( Mid - 1, FHotSpotRect.Bottom - 10 ) ] );
    end;

    { Draw Dots }
    for I := 0 to ( FHotSpotRect.Bottom - FHotSpotRect.Top - 32 ) div 3  do
    begin
      Canvas.Pixels[ Mid, FHotSpotRect.Top + 15 + I * 3 ] := HSDotColor;
    end;
  end;
end; {= TRzCustomPanel.DrawHotSpot =}


procedure TRzCustomSizePanel.DrawSizeBar;
var
  BarRect, R1, R2: TRect;
  FillColor: TColor;
begin
  BarRect := GetSizeBarRect;

  if UsingSystemStyle then
    FillColor := Color
  else
    FillColor := ActiveStyleColor( scPanel );

  if ( VisualStyle <> vsClassic ) and ( FSizeBarStyle = ssGroupBar ) then
  begin
    case Align of
      alLeft:
        Dec( BarRect.Left );

      alRight:
        Inc( BarRect.Right );

      alBottom:
      begin
        Inc( BarRect.Bottom );
        FillColor := LighterColor( Color, GradientColorAdjustment );
      end;

      alNone, alCustom:
      begin
        case FSide of
          sdLeft:
            Dec( BarRect.Left );

          sdRight:
            Inc( BarRect.Right );

          sdBottom:
          begin
            Inc( BarRect.Bottom );
            FillColor := LighterColor( Color, GradientColorAdjustment );
          end;
        end;
      end;
    end;
  end;

  Canvas.Pen.Style := psClear;
  Canvas.Brush.Color := FillColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.Rectangle( BarRect.Left, BarRect.Top, BarRect.Right + 1, BarRect.Bottom + 1 );
  Canvas.Pen.Style := psSolid;

  if FSizeBarStyle in [ ssGroove, ssBump ] then
  begin
    R1 := BarRect;
    if HotSpotTopBottom then
    begin
      R1.Top := BarRect.Top + ( BarRect.Bottom - BarRect.Top ) div 2 - 3;
      R1.Bottom := R1.Top + 3;
      R2 := R1;
      OffsetRect( R2, 0, 3 );
    end
    else
    begin
      R1.Left := BarRect.Left + ( BarRect.Right - BarRect.Left ) div 2 - 3;
      R1.Right := R1.Left + 3;
      R2 := R1;
      OffsetRect( R2, 3, 0 );
    end;

    if FSizeBarStyle = ssBump then
    begin
      DrawBorder( Canvas, R1, fsPopup );
      DrawBorder( Canvas, R2, fsPopup );
    end
    else
    begin
      DrawBorder( Canvas, R1, fsStatus );
      DrawBorder( Canvas, R2, fsStatus );
    end;
  end
  else if ( FSizeBarStyle = ssGroupBar ) and
          ( ( Align in [ alLeft, alRight ] ) or
            ( ( Align in [ alNone, alCustom ] ) and ( FSide in [ sdLeft, sdRight ] ) ) ) then
  begin
    DrawGroupBarBackground( Canvas, BarRect, VisualStyle,
                            GradientColorStyle, FGradientPath,
                            GradientColorStart, GradientColorStop );
  end;

  if FHotSpotVisible then
  begin
    if FRefreshHotSpot then
    begin
      if PtInRect( FHotSpotRect, CursorPosition ) then
        DrawHotSpot( True )
      else
        DrawHotSpot( False );
      FRefreshHotSpot := False;
    end
    else
      DrawHotSpot( False );
    FOutsideHotSpot := True;
  end;

end; {= TRzCustomSizePanel.DrawSizeBar =}


procedure TRzCustomSizePanel.Paint;
begin
  inherited;
  DrawSizeBar;
end;


// WMPaint needs to be overridden here in order to handle the case
// where DoubleBuffer is set to True.  The original TWinControl.WMPaint
// method uses ClientRect to create the compatible bitmap and this is not
// big enough b/c the size panel adjusts the ClientRect to its interior.
// Instead, the following code uses the BoundsRect property.

procedure TRzCustomSizePanel.WMPaint( var Msg: TWMPaint );
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  R: TRect;
begin
  if not FDoubleBuffered or ( Msg.DC <> 0 ) then
  begin
    if not ( csCustomPaint in ControlState ) and ( ControlCount = 0 ) then
      inherited
    else
      PaintHandler( Msg );
  end
  else
  begin
    DC := GetDC( 0 );
    R := BoundsRect;
    MemBitmap := CreateCompatibleBitmap( DC, R.Right, R.Bottom );
    ReleaseDC( 0, DC );
    MemDC := CreateCompatibleDC( 0 );
    OldBitmap := SelectObject( MemDC, MemBitmap );
    try
      DC := BeginPaint( Handle, PS );
      Perform( wm_EraseBkgnd, WPARAM( MemDC ), LPARAM( MemDC ) );
      Msg.DC := MemDC;
      WMPaint( Msg );
      Msg.DC := 0;
      BitBlt( DC, 0, 0, R.Right, R.Bottom, MemDC, 0, 0, SRCCOPY );
      EndPaint( Handle, PS );
    finally
      SelectObject( MemDC, OldBitmap );
      DeleteDC( MemDC );
      DeleteObject( MemBitmap );
    end;
  end;
end;


function TRzCustomSizePanel.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TRzCustomSizePanel.SetAlign( Value: TAlign );
begin
  inherited Align := Value;
  Realign;
end;


procedure TRzCustomSizePanel.SetSizeBarWidth( Value: TSizeBarWidth );
begin
  if FSizeBarWidth <> Value then
  begin
    FSizeBarWidth := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TRzCustomSizePanel.SetSizeBarStyle( Value: TSplitterStyle );
begin
  if FSizeBarStyle <> Value then
  begin
    FSizeBarStyle := Value;
    if ( FSizeBarStyle <> ssStandard ) and ( FSizeBarWidth < 6 ) then
      SizeBarWidth := 6;
    Invalidate;
  end;
end;


function TRzCustomSizePanel.GetMarginExtent: Integer;
var
  R: TRect;
begin
  Result := 0;
  R := GetParentWorkingRect;
  case Align of
    alTop:
      Result := R.Bottom - Top;

    alBottom:
      Result := R.Bottom + Height - R.Top;

    alLeft:
      Result := R.Right - Left;

    alRight:
      Result := R.Right + Width - R.Left;

    alNone, alCustom:
    begin
      case FSide of
        sdTop:
          Result := R.Bottom - Top;

        sdBottom:
          Result := R.Bottom + Height - R.Top;

        sdLeft:
          Result := R.Right - Left;

        sdRight:
          Result := R.Right + Width - R.Left;
      end;
    end;
  end; { case }
end;


procedure TRzCustomSizePanel.SetMarginMax( Value: Integer );
var
  Size: Integer;
begin
  FMarginMax := Value;

  if FMarginMax < 0 then
    FMarginMax := 0;

  if not ( csReading in ComponentState ) then
  begin
    Size := GetMarginExtent;

    if ( FMarginMax + FMarginMin ) > Size then
      FMarginMax := Size - FMarginMin;
  end;
end;


procedure TRzCustomSizePanel.SetMarginMin( Value: Integer );
var
  Size: Integer;
begin
  FMarginMin := Value;

  if FMarginMin < 0 then
    FMarginMin := 0;

  if not ( csReading in ComponentState ) then
  begin
    Size := GetMarginExtent;

    if ( FMarginMin + FMarginMax ) > Size then
      FMarginMin := Size - FMarginMax;
  end;
end;


procedure TRzCustomSizePanel.ResetHotSpot;
begin
  FHotSpotClosed := False;
end;


procedure TRzCustomSizePanel.RestoreHotSpot;
begin
  if FHotSpotClosed then
  begin
    FHotSpotClosed := False;

    if Align in [ alNone, alCustom ] then
    begin
      if FSide in [ sdLeft, sdTop ] then
        CheckPosition( FHotSpotPosition );
    end
    else if Align in [ alLeft, alTop ] then
      CheckPosition( FHotSpotPosition );

    if HotSpotTopBottom then
      Height := FHotSpotPosition
    else
      Width := FHotSpotPosition;
  end;
end;


procedure TRzCustomSizePanel.CloseHotSpot;
begin
  if not FHotSpotClosed then
  begin
    FHotSpotClosed := True;
    if HotSpotTopBottom then
    begin
      FHotSpotPosition := Height;

      if FHotSpotIgnoreMargins then
        Height := FSizeBarWidth + FMarginOffset
      else
        Height := FMarginMin + FSizeBarWidth + FMarginOffset;
    end
    else
    begin
      FHotSpotPosition := Width;

      if FHotSpotIgnoreMargins then
        Width := FSizeBarWidth + FMarginOffset
      else
        Width := FMarginMin + FSizeBarWidth + FMarginOffset;
    end;
  end;
end;


procedure TRzCustomSizePanel.SetHotSpotVisible( Value: Boolean );
begin
  if FHotSpotVisible <> Value then
  begin
    FHotSpotVisible := Value;
    if FHotSpotVisible then
    begin
      if FSizeBarWidth < 7 then
        SizeBarWidth := 7;
    end
    else
      FHotSpotClosed := False;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetHotSpotColor( Value: TColor );
begin
  if FHotSpotColor <> Value then
  begin
    FHotSpotColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetHotSpotDotColor( Value: TColor );
begin
  if FHotSpotDotColor <> Value then
  begin
    FHotSpotDotColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetHotSpotFrameColor( Value: TColor );
begin
  if FHotSpotFrameColor <> Value then
  begin
    FHotSpotFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetHotSpotHighlight( Value: TColor );
begin
  if FHotSpotHighlight <> Value then
  begin
    FHotSpotHighlight := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetHotSpotSizePercent( Value: Integer );
begin
  if FHotSpotSizePercent <> Value then
  begin
    FHotSpotSizePercent := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetSide( Value: TSide );
begin
  if FSide <> Value then
  begin
    FSide := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetGradientColorAdjustment( Value: Integer );
begin
  if FGradientColorAdjustment <> Value then
  begin
    FGradientColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzCustomSizePanel.SetGradientPath( Value: TRzGroupBarGradientPath );
begin
  if FGradientPath <> Value then
  begin
    FGradientPath := Value;
    Invalidate;
  end;
end;



{&RUIF}
end.




