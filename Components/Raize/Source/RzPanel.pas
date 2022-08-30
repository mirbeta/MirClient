{===============================================================================
  RzPanel Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzPanel
    Enhanced panel component with many more display options.

  TRzGroupBox
    Custom group box component with more display options and functionality.

  TRzSpacer
    Separator control designed to be used on a TRzToolbar.

  TRzToolbar
    Custom TRzPanel control designed to function as a toolbar.

  TRzStatusBar
    Custom TRzPanel control designed to functiona as a status bar.


  Modification History
  ------------------------------------------------------------------------------
  6.1.4  (29 May 2013)
    * Fixed issue where transparent TRzPanel instances and descendants would not
      be painted correctly if the parent's DoubleBuffered property was True.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * TRzToolbar.RestoreLayout now handles previously stored layouts that
      reference controls that are no longer used by the form. This controls are
      simply ignored when the layout is restored.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed issue in TRzGroupBox and descendants where under certain GroupStyle
      values, the caption would not be displayed correctly when using VCL
      Styles.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed issue in TRzPanel and descendants that resulted in the client area
      of the panel to be calculated too early in the alignment process. The net
      effect of this issue was that controls placed on the panel with akRight or
      akBottom included in the control's Anchors property would get resized
      smaller as the client area was re-adjusted.
    * Fixed issue in TRzToolbar when setting the Align property to something
      other than alTop or alBottom and AutoSize to True would cause the drop-
      down menu button to appear and overlap the last button in the display.
    * Added new DisabledImages property to the TRzToolbar component. This
      property allows a user to associate a dedicated image list to the control
      that only contains disabled images.  The benefit of this is that the same
      ImageIndex value can be used for both the Images and DisabledImages lists.
      The TRzToolButtons that have images will use the DisabledImages list if
      it is not nil *and* the button's DisabledIndex property is -1. The
      ImageIndex property will be used to index into the DisabledImages list.
    * Made necessary modifications to TRzPanel, TRzGroupBox, TRzToolbar,
      TRzStatusBar, TRzFlowPanel, and TRzGridPanel to fully support VCL Styles
      introduced in RAD Studio XE2. Including automatic color adjustment of
      frame/border styles.
    * Made necessary modifications to TRzToolbar to support 64-bit development.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzGroupBox component now has a ShowCheckBox property. When this
      this property is set to True, a check box is displayed next to the Caption
      of the group box. There is also a new Checked property, which indicates
      the current state of the check box. When the check box is clicked and the
      state changes, the OnCheckBoxClick event is raised.  The new
      EnableControlsOnCheck property (True by default) controls whether or not
      any child controls of the group box are automatically enabled/disabled
      as the check box state changes. When EnableControlsOnCheck is True, and
      the check box is checked, then the child controls are enabled. Likewise,
      when the check box is unchecked, the controls are disabled. The
      TRzGroupBox remembers the current enabled/disabled states of the child
      controls before disabling. Therefore, if a control was disabled before
      un-checking the check box, the control remains disabled when re-checking
      the check box.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue where the TRzToolbar would display the popup button (to access
      additional buttons displayed outside the bounds of the toolbar) even
      though all remaining buttons on the toolbar had their Visible property set
      to False.
    * Fixed issue where TRzGroupBox caption would not get scaled appropriately
      when running on a system using a higher DPI setting.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed issue in TRzGroupBox where captions that included accelerator
      characters (&) would get truncated when the application was compiled with
      XP/Vista themes.
    * Added new CaptionFont property to TRzGroupBox and descendants. This
      property allows the font used to display the group box's Caption to be
      different from the component's Font property, which controls the font
      used by any controls placed on the group box. The CaptionFont property
      only takes effect when the VisualStyle of the group box is vsClassic or
      vsGradient. When the VisualStyle is set to vwWinXP, the caption is
      displayed using the current XP/Vista Theme settings.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed display issue in TRzToolbar when running on Windows XP with Themes
      enabled and ShowDivider set to True. The fix now allows custom border
      settings for the toolbar to be visible when XP/Vista Themes are enabled.
    * Fixed issue in TRzStatusBar where a SimpleCaption that contained
      ampersands (&) would result in the next character being underlined unless
      doubled-up. This is no longer necessary.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzPanel, TRzGroupBox, TRzToolbar, TRzStatusBar, TRzGridPanel, and
      TRzFlowPanel controls.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Fixed issue with TRzPanel and descendants where setting AutoSize to True
      would not take into account the borders of the panel.
    * Fixed a separate but related issue to the above where changing the size of
      a control on a TRzPanel with AutoSize set to True would cause the panel to
      adjust its own size incorrectly.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed issue where resizing a TRzStatusBar so that its Width became zero
      and AutoScalePanes was set to True would result in an exception.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new BannerHeight property to TRzGroupBox, which is used to control
      the height of the banner area when the gsBanner GroupStyle is used. By
      default, this property is 0, which instructs the control to determine the
      height of the Banner based on the font size. If BannerHeight is set to a
      non-zero value, the height of the banner is sized accordingly.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Updated display of disabled text in TRzPanel, TRzGroupBox, and
      descendants.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Fixed display problem in the Caption of a TRzGroupBox that would occur
      when using a non-standard XP theme.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Fixed display problems under Delphi 5 and 6 in which TRzCustomPanel
      descendants would not honor the Transparent property.
  ------------------------------------------------------------------------------
  4.1.1  (12 Jan 2007)
    * Fixed issue with TRzPanel, TRzGroupBar, and descendants that would cause
      transparent controls to paint incorrectly under XP themes under Delphi 5
      or Delphi 6.
    * Added TextMarginVertical property, which allows the vertical text margin
      to be set differently from the horizontal margin (via TextMargin).
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added a new PaintClientArea property to TRzCustomPanel. This property is
      set to True by default. Set this property to False, when you are using
      another control to completely fill the client area of the panel.
      Therefore, the panel does not need to paint the client area as this will
      be painted by the other control (such as a TRzBackground). This will
      eliminate flicker when resizing the panel.
    * Fixed flicker issue with TRzPanel, TRzGroupBox, and descendants.
    * The TRzToolbar now correctly auto-sizes when WrapControls is False, and
      when Align is set to alNone.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Fixed problem where grid lines in TRzGridPanel were not drawn.
    * The Caption of a TRzGroupBox using the GroupStyle of gsBanner or
      gsUnderline is not correctly positioned based on the Alignment property.
    * Modified TRzCustomPanel such that when the background appearance of the
      container is modified (e.g. GradientColorStart) any child controls in the
      container that support Transparency and invalidated in order to "pick up"
      the new appearance of the container.
    * Fixed appearance of TRzStatusBar when SimpleStatus is true and VisualStyle
      is set to vsGradient.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Surfaced OnPaint event in TRzStatusBar.
    * Surface Padding property in TRzStatusBar (only in BDS 2006).
    * When using the gsBanner GroupStyle in TRzGroupBox and setting
      GradientColorStyle to gcsCustom, the banner area is now filled using the
      color values defined in GradientColorStart and GradientColorStop.
    * Fixed problem where TRzPanel and descendants would not pick up the correct
      settings when connected to a TRzFrameController in Delphi 5.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * When TRzPanel.Transparent is set to True, the parent's background is
      usually drawn in the client area.  Starting with this version, the
      parent's background image is only drawn when Transparent is True and the
      FullRepaint property is also True.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzCustomPanel to
      account for changes introduced in Borland Developer Studio 2006.
    * Surfaced Padding property (introduced in Borland Developer Studio 2006)
      in TRzPanel and TRzGroupBox.
    * Modified control positioning code in TRzToolbar to take into account the
      Margins of controls dropped on the toolbar. A control's Margins settings
      are only used when the control's AlignWithMargins property is set to True.
    * Introduced TRzFlowPanel and TRzGridPanel. These two components are custom
      descendants of the TFlowPanel and TGridPanel introduced in Borland
      Developer Studio 2006, and provide additional features not present in the
      ones that come with the VCL.  For example, the TRzFlowPanel and
      TRzGridPanel have the same display capabilities as the TRzPanel component
      including the way contained controls are enabled/disabled as the panel
      is enabled/disabled. In addition, context menu design editors have also
      been created for the TRzFlowPanel and TRzGridPanel providing quick access
      to common proeprties and tasks associated with the controls.
    * Several changes were made to TRzCustomPanel and descendant controls
      regarding the user interface. Specifically, the ThemeAware and
      UseGradients properties have been removed. They have been replaced with
      the new VisualStyle property, which can be set to vsClassic, vsWinXP, or
      vsGradient.  The default is vsWinXP, which results in similar behavior to
      version 3.x.  For the basic TRzPanel, there is no difference between
      vsWinXP and vsClassic.  However, for controls such as the TRzToolbar,
      TRzGroupBox, and TRzStatusBar, this default value means that if XP Themes
      are used in an application, then the control will use the appropriate
      XP theme style.  When vsGradient is selected, the coloring of the control
      is dependent on the new GradientColorStyle property, which can be set to
      gcsSystem, gcsMSOffice, or gcsCustom.  The gcsSystem value is the default
      which results in colors based on the current system colors. When the
      gcsMSOffice value is specified and the XP Themes are present, the control
      colors match those used by Microsoft Office products. When XP Themes are
      not present, the selection defaults to gcsSystem. The end result is that
      when choosing gcsSystem or gcsMSOffice, you will get an appropriate
      appearance that blends in naturally with the current user's color scheme.
      Also note that if you load a form (created in an earlier verison of RC),
      with a panel control that has UseGradients set to True, the component will
      automatically set ViewStyle to vsGradient.
    * The TRzGroupBox control now picks up XP theme colors for the Caption for
      all GroupStyle values and not just gsStandard and gsFlat.
    * The TRzGroupBox now offers two new styles: gsBanner and gsUnderline. These
      styles determine how the group's caption is displayed.
    * The TRzStatusBar control now surfaces GradientColorStart and
      GradientColorStop properties, as well as the new GradientColorStyle and
      ViewStyle properties described above. When ViewStyle is set to vsGradient
      and GradientColorStyle is set to gcsCustom, these new Start/Stop color
      property values determine the appearance of the status bar.
    * Added new FrameControllerNotifications property to TRzPanel, TRzFlowPanel,
      and TRzGridPanel.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where RowHeight of TRzToolbar was not being adjusted when
      the control was scaled.
    * Fixed problem where leading ampersands (&) placed at the beginning of
      a TRzGroupBox caption would cause extra spaces to appear at the end of
      the caption.
    * Added new TextMargin property to TRzPanel, which controls the amount of
      space between the inside border of the control (or edge of control if no
      borders are visible) and the Caption's display area.
    * Added new WordWrap property to TRzPanel, which is only active when a
      Caption is visible.
    * Added two new GroupStyle values to the TRzGroupBox and descendants:
      gsBanner and gsUnderline.  The gsBanner style displays the group's caption
      in a gradient filled banner at the top of the group. The gsUnderline style
      displays a line underneath caption across the entire group's width.
    * TRzGroupBox and descendants now correctly take into account differing
      client areas that result from changes in GroupStyle when aligning
      controls.
  ------------------------------------------------------------------------------
  3.0.12 (15 Dec 2004)
    * Added back the code in the TRzCustomPanel.SetBounds method override that
      repositions panels aligned to the right or bottom when their size changes.
      This code was commented out in 3.0.11 in an attempt to address an issue
      where the TRzStatusBar was getting its Top property changed during loading
      if the status bar's Align property was set to alNone.

      However, commenting out this code causes a more significant problem that
      can occur where a panel-based control is aligned alRight or alBottom and
      there is another control also aligned alRight or alBottom adjacent to the
      panel-based control. If the size of the panel is changed the order of all
      the controls aligned in the same direction get re-ordered.  This situation
      is deemed more significant than the issue with an alNone aligned status
      bar and thus the code has been reverted.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Added UseGradients, GradientColorStart, GradientColorStop, and
      GradientDirection properties to TRzPanel and TRzToolbar.
    * Fixed problem where using &'s in TRzGroupBox.Caption would cause spaces
      in the border to the right of the caption.
    * Fixed problem where Top property of TRzStatusBar would get altered during
      loading when Align property set to alNone.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Surface the OnPaint event and Canvas property in TRzToolbar.
    * Added FrameController property to TRzGroupBox. This associate
      FrameController can be used to change the appearance of the group box when
      the GroupStyle property is set to gsCustom.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Surfaced the OnPaint event in TRzGroupBox. This is useful when GroupStyle
      is set to gsCustom.
    * Surfaced Align, Anchors, and Constraints properties in TRzSpacer.
    * Added fsFlatRounded to inner and outer border styles.
    * Refactored inner and outer border painting to common DrawInnerOuterBorders
      function.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Changing TRzGroupBox.GroupStyle to gsTopLine or gsCustom causes ThemeAware
      to be changed to False.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem where background of TRzGroupBox was not getting drawn when
      using XP Themes.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Modified alignment code of TRzStatusBar so that panes appear positioned
      more appropriately when running under Windows XP.
    * Added ShowTextOptions parameter to TRzToolbar.Customize method.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomPanel and TRzPanel >>
    * All panel controls now keep track of Enabled/Disabled children and then
      restore these values when the panel is re-enabled.
    * Added FlatColorAdjustment property.
    * The TRzPanel now supports displaying a grid on its canvas. The ShowGrid,
      GridStyle, GridXSize, GridYSize, and GridColor properties control how the
      grid is displayed.
    * The custom docking manager has been updated to show focus changes.
    * The FrameSides property has been removed from the TRzCustomPanel
      component.

    << TRzGroupBox >>
    * Added the gsFlat GroupStyle for all Raize group boxes.
    * When BorderWidth is larger than the height of the Caption, then the client
      area of the group box is no longer adjusted by the height of the caption.
    * Added XP visual styles support.

    << TRzToolbar >>
    * The TRzToolbar has received a number of enhancements including support for
      an ImageList, runtime customizations, etc.
    * Added XP visual styles support.

    << TRzStatusBar >>
    * Updated display of size grip to match Windows XP style.  The size grip now
      automatically detects the state of the parent form to determine if the
      size grip is valid.  For example, the size grip is not valid for a form
      with a BorderStyle of bsDialog.  The size grip is also not valid for a
      maximized form.
    * Added SimpleFrameStyle property.
    * Added XP visual styles support.
===============================================================================}

{$I RzComps.inc}

unit RzPanel;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  SysUtils,
  Messages,
  Windows,
  Classes,
  Graphics,
  Controls,
  Buttons,
  StdCtrls,
  ExtCtrls,
  ImgList,
  Menus,
  Forms,
  Dialogs,
  RzGrafx,
  RzCommon,
  RzButton;

type
  {======================================}
  {== TRzCustomPanel Class Declaration ==}
  {======================================}

  TRzCustomPanel = class;

  { Need to create a new dock manager class the knows how to paint a
    TRzSizePanel as a Dock Site. The problem is that the default Dock Manager
    (i.e. TDockTree) does not handle painting the area occupied by the SizeBar }

  TRzPanelDockManager = class( TDockTree )
  private
    FPanel: TRzCustomPanel;
    FOldWndProc: TWndMethod;
    procedure WindowProcHook( var Msg: TMessage );
  protected
    FFont: TFont;
    FCloseFont: TFont;
    FGrabberSize: Integer;
    procedure AdjustDockRect( Control: TControl; var ARect: TRect ); override;
    procedure DrawVertTitle( Canvas: TCanvas; const Caption: string; Bounds: TRect );
    procedure PaintDockFrame( Canvas: TCanvas; Control: TControl; const ARect: TRect ); override;
  public
    constructor Create( DockSite: TWinControl ); override;
    destructor Destroy; override;
    procedure PaintSite( DC: HDC ); override;
  end;

  TRzGridStyle = ( gsDots, gsDottedLines, gsSolidLines );

  TRzCustomPanel = class( TCustomPanel, IRzCustomFramingNotification )
  private
    FInAlignControls: Boolean;
    FAlignmentVertical: TAlignmentVertical;
    FBorderInner: TFrameStyleEx;
    FBorderOuter: TFrameStyleEx;
    FBorderSides: TSides;
    FBorderColor: TColor;
    FBorderHighlight: TColor;
    FBorderShadow: TColor;
    FFlatColor: TColor;
    FFlatColorAdjustment: Integer;
    FPaintClientArea: Boolean;
    FPanelColor: TColor;

    FVisualStyle: TRzVisualStyle;
    FGradientColorStyle: TRzGradientColorStyle;
    FGradientColorStart: TColor;
    FGradientColorStop: TColor;
    FGradientDirection: TGradientDirection;

    FGridColor: TColor;
    FGridStyle: TRzGridStyle;
    FGridXSize: Word;
    FGridYSize: Word;
    FShowGrid: Boolean;
    FTransparent: Boolean;
    FTextMargin: Integer;
    FTextMarginVertical: Integer;
    FWordWrap: Boolean;

    FShowDockClientCaptions: Boolean;
    FEnabledList: TStringList;
    FOnPaint: TNotifyEvent;

    procedure ReadOldUseGradientsProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message wm_WindowPosChanged;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
    procedure WMThemeChanged( var Msg: TMessage ); message wm_ThemeChanged;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    procedure UpdatePanelColor;
    {$ENDIF}
  protected
    FAboutInfo: TRzAboutInfo;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;

    function CursorPosition: TPoint; virtual;
    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;
    procedure FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean ); virtual;
    procedure AdjustClientRect( var Rect: TRect ); override;
    function GetClientRect: TRect; override;
    function GetControlRect: TRect; virtual;
    function InvalidateMarginSize: TPoint; virtual;
    procedure EnableChildControls( Enabled: Boolean ); virtual;

    procedure GetGradientColors( var StartColor, StopColor: TColor ); virtual;
    procedure GetGradientFrameColor( var FrameColor: TColor;
                                     var FrameColorAdjustment: Integer ); virtual;

    procedure DrawCaption( Rect: TRect ); virtual;
    procedure DrawGrid( Rect: TRect ); virtual;
    procedure Paint; override;
    procedure CustomFramingChanged; virtual;

    function CreateDockManager: IDockManager; override;

    { Property Access Methods }
    procedure SetAlignmentVertical( Value: TAlignmentVertical ); virtual;
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderInner( Value: TFrameStyleEx ); virtual;
    procedure SetBorderOuter( Value: TFrameStyleEx ); virtual;
    procedure SetBorderSides( Value: TSides ); virtual;
    procedure SetBorderHighlight( Value: TColor ); virtual;
    procedure SetBorderShadow( Value: TColor ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetGradientColorStyle( Value: TRzGradientColorStyle ); virtual;
    procedure SetGradientColorStart( Value: TColor ); virtual;
    procedure SetGradientColorStop( Value: TColor ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection ); virtual;
    procedure SetGridColor( Value: TColor ); virtual;
    procedure SetGridStyle( Value: TRzGridStyle ); virtual;
    procedure SetGridXSize( Value: Word ); virtual;
    procedure SetGridYSize( Value: Word ); virtual;
    procedure SetPaintClientArea( Value: Boolean ); virtual;
    procedure SetShowGrid( Value: Boolean ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;
    procedure SetTextMargin( Value: Integer ); virtual;
    procedure SetTextMarginVertical( Value: Integer ); virtual;
    procedure SetWordWrap( Value: Boolean ); virtual;
    procedure SetVisualStyle( Value: TRzVisualStyle ); virtual;

    { Property Declarations }
    property AlignmentVertical: TAlignmentVertical
      read FAlignmentVertical
      write SetAlignmentVertical
      default avCenter;

    property BorderInner: TFrameStyleEx
      read FBorderInner
      write SetBorderInner
      default fsNone;

    property BorderOuter: TFrameStyleEx
      read FBorderOuter
      write SetBorderOuter
      default fsRaised;

    property BorderSides: TSides
      read FBorderSides
      write SetBorderSides
      default [ sdLeft, sdTop, sdRight, sdBottom ];

    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      default clBtnFace;

    property BorderHighlight: TColor
      read FBorderHighlight
      write SetBorderHighlight
      default clBtnHighlight;

    property BorderShadow: TColor
      read FBorderShadow
      write SetBorderShadow
      default clBtnShadow;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;
      
    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property GradientColorStyle: TRzGradientColorStyle
      read FGradientColorStyle
      write SetGradientColorStyle
      default gcsSystem;

    property GradientColorStart: TColor
      read FGradientColorStart
      write SetGradientColorStart
      default clWhite;

    property GradientColorStop: TColor
      read FGradientColorStop
      write SetGradientColorStop
      default clBtnFace;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalEnd;

    property GridColor: TColor
      read FGridColor
      write SetGridColor
      default clBtnShadow;

    property GridStyle: TRzGridStyle
      read FGridStyle
      write SetGridStyle
      default gsDots;

    property GridXSize: Word
      read FGridXSize
      write SetGridXSize
      default 8;

    property GridYSize: Word
      read FGridYSize
      write SetGridYSize
      default 8;

    property PaintClientArea: Boolean
      read FPaintClientArea
      write SetPaintClientArea
      default True;

    property TextMargin: Integer
      read FTextMargin
      write SetTextMargin
      default 0;      
      
    property TextMarginVertical: Integer
      read FTextMarginVertical
      write SetTextMarginVertical
      default 0;

    property ShowGrid: Boolean
      read FShowGrid
      write SetShowGrid
      default False;

    property ShowDockClientCaptions: Boolean
      read FShowDockClientCaptions
      write FShowDockClientCaptions
      default True;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default False;

    property VisualStyle: TRzVisualStyle
      read FVisualStyle
      write SetVisualStyle
      default vsWinXP;

    property WordWrap: Boolean
      read FWordWrap
      write SetWordWrap
      default True;

    property OnPaint: TNotifyEvent
      read FOnPaint
      write FOnPaint;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;
    procedure SetGridSize( XSize, YSize: Integer );
  end;


  {================================}
  {== TRzPanel Class Declaration ==}
  {================================}

  TRzPanel = class( TRzCustomPanel )
  public
    property Canvas;
    property DockManager;
  published
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
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderColor;
    property BorderHighlight;
    property BorderShadow;
    property BorderWidth;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property FullRepaint;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GridColor;
    property GridStyle;
    property GridXSize;
    property GridYSize;
    property Locked;
    property PaintClientArea;
    property TextMargin;
    property TextMarginVertical;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowDockClientCaptions;
    property ShowGrid;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Transparent;
    property UseDockManager default True;
    property Visible;
    property VisualStyle;
    property WordWrap;

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
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  {=========================================}
  {== TRzCustomGroupBox Class Declaration ==}
  {=========================================}

  TRzGroupBoxStyle = ( gsStandard, gsCustom, gsTopLine, gsFlat,
                       gsBanner, gsUnderline );

  TRzCustomGroupBox = class( TRzCustomPanel )
  private
    FGroupStyle: TRzGroupBoxStyle;
    FBannerHeight: Integer;
    FCaptionFont: TFont;
    FCaptionFontChanged: Boolean;
    FCaptionRect: TRect;
    FEnableControlsOnCheck: Boolean;
    FShowCheckBox: Boolean;
    FCheckBoxSize: Integer;
    FDragging: Boolean;
    FMouseOverCheckBox: Boolean;
    FKeyToggle: Boolean;
    FShowDownVersion: Boolean;
    FOnCheckBoxClick: TNotifyEvent;
    FChecked: Boolean;

    // Internal Event Handlers
    procedure CaptionFontChangeHandler( Sender: TObject );

    // Message Handling Methods
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSetFocus( var Msg: TWMSetFocus ); message wm_SetFocus;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
  protected
    procedure CreateWnd; override;
    procedure CustomFramingChanged; override;

    procedure Paint; override;
    procedure DrawThemedCheckBox;
    procedure DrawNonThemedCheckBox;
    function AdjustCaptionRectForCheckBox: TRect;
    procedure AdjustClientRect( var Rect: TRect ); override;
    procedure ChangeScale( M, D: Integer ); override;

    function ShowAccel: Boolean;
    function ShowFocus: Boolean;

    procedure ChangeState; virtual;

    // Event Dispatch Methods
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure CheckBoxClick; dynamic;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;

    // Property Access Methods
    procedure SetBannerHeight( Value: Integer ); virtual;
    procedure SetGroupBoxStyle( Value: TRzGroupBoxStyle ); virtual;
    function IsCaptionFontStored: Boolean;
    procedure SetCaptionFont( Value: TFont ); virtual;
    procedure SetChecked( Value: Boolean ); virtual;
    procedure SetEnableControlsOnCheck( Value: Boolean ); virtual;
    procedure SetShowCheckBox( Value: Boolean ); virtual;

    property Checked: Boolean
      read FChecked
      write SetChecked
      default True;

    property EnableControlsOnCheck: Boolean
      read FEnableControlsOnCheck
      write SetEnableControlsOnCheck
      default True;

    property ShowCheckBox: Boolean
      read FShowCheckBox
      write SetShowCheckBox
      default False;

    property OnCheckBoxClick: TNotifyEvent
      read FOnCheckBoxClick
      write FOnCheckBoxClick;


    // Inherited Properties & Events
    property Alignment default taLeftJustify;
    property AlignmentVertical default avTop;
    property BorderOuter default fsNone;
    property BorderInner default fsNone;
    property Height default 105;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    { Property Declarations }
    property BannerHeight: Integer
      read FBannerHeight
      write SetBannerHeight
      default 0;

    property GroupStyle: TRzGroupBoxStyle
      read FGroupStyle
      write SetGroupBoxStyle
      default gsFlat;

    property CaptionFont: TFont
      read FCaptionFont
      write SetCaptionFont
      stored IsCaptionFontStored;
  end;


  {===================================}
  {== TRzGroupBox Class Declaration ==}
  {===================================}

  TRzGroupBox = class( TRzCustomGroupBox )
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property BannerHeight;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderWidth;
    property Caption;
    property CaptionFont;
    property Checked;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableControlsOnCheck;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GroupStyle;
    property Height;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCheckBox;
    property ShowDockClientCaptions;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Transparent;
    property Visible;
    property VisualStyle;

    property OnCheckBoxClick;
    property OnClick;
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
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  {=================================}
  {== TRzSpacer Class Declaration ==}
  {=================================}

  TRzSpacer = class( TGraphicControl )
  private
    FAboutInfo: TRzAboutInfo;
    FGrooved: Boolean;
    FOrientation: TOrientation;
  protected
    procedure Paint; override;

    { Property Access Methods }
    procedure SetGrooved( Value: Boolean ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Grooved: Boolean
      read FGrooved
      write SetGrooved
      default False;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Constraints;
    property Height default 25;
    property Width default 8;
    property Visible;
  end;


  {=========================================}
  {== TRzToolbarControl Class Declaration ==}
  {=========================================}

  TRzToolbarControl = class( TObject )
  protected
    FControl: TControl;
    FControlName: string;
    procedure AssignElements( Control: TRzToolbarControl ); virtual;
  public
    property Control: TControl
      read FControl;

    property ControlName: string
      read FControlName;
  end;


  {=============================================}
  {== TRzToolbarControlList Class Declaration ==}
  {=============================================}

  TRzToolbar = class;
  TRzToolbarControlList = class;

  TRzToolbarControlList = class( TList )
  protected
    FToolbar: TRzToolbar;
    FIndexOfLastControlRead: Integer; { Valid only during call to ReadControl }
    FTempControlList: TRzToolbarControlList;
    function Get( Index: Integer ): TRzToolbarControl;
    procedure Put( Index: Integer; Value: TRzToolbarControl );

    procedure ControlsAreLoaded( OwnerComp: TComponent );

    procedure ReadControl( Reader: TReader ); virtual;
    procedure WriteControl( Index: Integer; Writer: TWriter ); virtual;
    function CreateToolBarControl: TRzToolBarControl;	virtual;
  public
    constructor Create( AToolbar: TRzToolbar );
    destructor Destroy; override;

    procedure ReadControls( Reader: TReader );
    procedure WriteControls( Writer: TWriter );
    function IndexOf( Control: TControl ): Integer;
    function IndexOfName( const ControlName: string ): Integer;
    function AddControl( Control: TControl ): Integer;
    function AddControlName( const ControlName: string ): Integer;
    function RemoveControl( Control: TControl): Integer;
    function RemoveControlName( const ControlName: string ): Integer;

    property Items[ Index: Integer ]: TRzToolbarControl
      read Get
      write Put; default;

    property Toolbar: TRzToolbar
      read FToolbar;
  end;


  {============================================}
  {== TRzCustomizeCaptions Class Declaration ==}
  {============================================}

  TRzCustomizeCaptions = class( TPersistent )
  private
    FStoreCaptions: Boolean;
    FTitle: string;
    FHint: string;
    FClose: string;
    FMoveUp: string;
    FMoveDown: string;
    FTextOptions: string;
    FNoTextLabels: string;
    FShowTextLabels: string;
    FSelectiveTextOnRight: string;
  protected
    function GetCaption( Index: Integer ): string;
    procedure SetCaption( Index: Integer; const Value: string );
  public
    constructor Create;
  published
    property Title: string
      index 1
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property Hint: string
      index 2
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property Close: string
      index 3
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property MoveUp: string
      index 4
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property MoveDown: string
      index 5
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property TextOptions: string
      index 6
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property NoTextLabels: string
      index 7
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property ShowTextLabels: string
      index 8
      read GetCaption
      write SetCaption
      stored FStoreCaptions;

    property SelectiveTextOnRight: string
      index 9
      read GetCaption
      write SetCaption
      stored FStoreCaptions;
  end;


  {=============================================}
  {== TRzToolbarPopupButton Class Declaration ==}
  {=============================================}

  TRzToolbarPopupButton = class( TCustomControl )
  private
    FFrameColor: TColor;
    FToolbar: TRzToolbar;
    FMenu: TPopupMenu;
    FSelectedButton: TRzToolButton;

    FPressed: Boolean;
    FDown: Boolean;

    { Message Handling Methods }
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
  protected
    procedure DrawBackground( Bounds: TRect; Down: Boolean ); virtual;
    procedure DrawChevron( Bounds: TRect ); virtual;
    procedure Paint; override;

    { Event Dispatch Methods }
    procedure DisplayButtons;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetFrameColor( Value: TColor ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      default clBtnShadow;
  end;


  {==================================}
  {== TRzToolbar Class Declaration ==}
  {==================================}

  TRzToolbarTextOptions = ( ttoNoTextLabels, ttoShowTextLabels, ttoSelectiveTextOnRight, ttoCustom );

  TRzToolbar = class( TRzCustomPanel )
  private
    FAutoResize: Boolean;
    FAutoStyle: Boolean;
    FBackground: TBitmap;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    FDisabledImagesChangeLink: TChangeLink;
    FOrientation: TOrientation;
    FRowHeight: Integer;
    FShowDivider: Boolean;
    FShowButtonCaptions: Boolean;
    FButtonLayout: TButtonLayout;
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FTextOptions: TRzToolbarTextOptions;
    FUpdatingTextOptions: Boolean;
    FWrapControls: Boolean;
    FToolbarPopupButton: TRzToolbarPopupButton;
    FShowToolbarPopupButton: Boolean;

    FCustomizeCaptions: TRzCustomizeCaptions;
    FRegIniFile: TRzRegIniFile;

    FOnVisibleChanged: TNotifyEvent;

    { Internal Event Handlers }
    procedure BackgroundChangedHandler( Sender: TObject );
    procedure ImagesChange( Sender: TObject );
    procedure DisabledImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure CMVisibleChanged( var Msg: TMessage ); message cm_VisibleChanged;
  protected
    FToolbarControls: TRzToolbarControlList;
    FMargin: Integer;
    FTopMargin: Integer;
    FCalculatedRowHeight: Integer;

    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure DrawCaption( Rect: TRect ); override;
    procedure Paint; override;

    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure CreateToolbarPopupButton;
    procedure AlignToolbarPopupButton;
    procedure SetShowToolbarPopupButton( Value: Boolean );
    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;
    procedure ChangeScale( M, D: Integer ); override;

    function CalculateRowHeight( Row: Integer ): Integer; virtual;
    function InvalidateMarginSize: TPoint; override;
    procedure PositionControl( Index, Row: Integer; var Offset: Integer ); virtual;
    function CreateToolbarControlList: TRzToolbarControlList; virtual;
    procedure AdjustStyle( Value: TAlign ); virtual;
    function CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean; override;


    { Event Dispatch Methods }
    procedure VisibleChanged; dynamic;

    { Property Access Methods }
    function GetAlign: TAlign; virtual;
    procedure SetAlign( Value: TAlign ); virtual;
    procedure SetAutoResize( Value: Boolean ); virtual;
    procedure SetBackground( Value: TBitmap ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;
    procedure SetDisabledImages( Value: TCustomImageList ); virtual;
    procedure SetMargin( Value: Integer ); virtual;
    procedure SetTopMargin( Value: Integer ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
    procedure SetRowHeight( Value: Integer ); virtual;
    procedure SetShowButtonCaptions( Value: Boolean ); virtual;
    procedure SetButtonLayout( Value: TButtonLayout ); virtual;
    procedure SetButtonWidth( Value: Integer ); virtual;
    procedure SetButtonHeight( Value: Integer ); virtual;
    procedure SetShowDivider( Value: Boolean ); virtual;
    procedure SetTextOptions( Value: TRzToolbarTextOptions ); virtual;
    procedure SetWrapControls( Value: Boolean ); virtual;

    procedure SetCustomizeCaptions( Value: TRzCustomizeCaptions ); virtual;
    procedure SetRegIniFile( Value: TRzRegIniFile ); virtual;

    procedure SetBorderInner( Value: TFrameStyleEx ); override;
    procedure SetBorderOuter( Value: TFrameStyleEx ); override;
    procedure SetBorderSides( Value: TSides ); override;

    procedure CheckAutoResize( var Value: Boolean ); virtual;
    procedure SetParent( AParent: TWinControl ); override;
    procedure SetBiDiMode( Value: TBiDiMode ); override;

    procedure SetGradientColorStyle( Value: TRzGradientColorStyle ); override;
    procedure SetVisualStyle( Value: TRzVisualStyle ); override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure PositionControls; virtual; { Made virtual after 2.1 }
    procedure UpdateButtonSize( NewWidth, NewHeight: Integer; ShowCaptions: Boolean ); virtual;

    procedure Customize( ShowTextOptions: Boolean = True );
    procedure RestoreLayout;
    procedure SaveLayout;

    { Property Declarations }
    property ToolbarControls: TRzToolbarControlList
      read FToolbarControls;

    property Canvas;
    property DockManager;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Align: TAlign
      read GetAlign
      write SetAlign
      default alTop;

    property AutoResize: Boolean
      read FAutoResize
      write SetAutoResize
      default True;

    property AutoStyle: Boolean
      read FAutoStyle
      write FAutoStyle
      default True;

    property Background: TBitmap
      read FBackground
      write SetBackground;

    property CustomizeCaptions: TRzCustomizeCaptions
      read FCustomizeCaptions
      write SetCustomizeCaptions;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property DisabledImages: TCustomImageList
      read FDisabledImages
      write SetDisabledImages;

    property Margin: Integer
      read FMargin
      write SetMargin
      default 4;

    property TopMargin: Integer
      read FTopMargin
      write SetTopMargin
      default 2;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    property RowHeight: Integer
      read FRowHeight
      write SetRowHeight
      default 25;

    property ButtonLayout: TButtonLayout
      read FButtonLayout
      write SetButtonLayout
      default blGlyphLeft;

    property ButtonWidth: Integer
      read FButtonWidth
      write SetButtonWidth
      default 25;

    property ButtonHeight: Integer
      read FButtonHeight
      write SetButtonHeight
      default 25;

    property RegIniFile: TRzRegIniFile
      read FRegIniFile
      write SetRegIniFile;

    property ShowButtonCaptions: Boolean
      read FShowButtonCaptions
      write SetShowButtonCaptions
      default False;

    property ShowDivider: Boolean
      read FShowDivider
      write SetShowDivider
      default True;

    property TextOptions: TRzToolbarTextOptions
      read FTextOptions
      write SetTextOptions
      default ttoNoTextLabels;

    property WrapControls: Boolean
      read FWrapControls
      write SetWrapControls
      default True;

    property OnVisibleChanged: TNotifyEvent
      read FOnVisibleChanged
      write FOnVisibleChanged;

    { Inherited Properties & Events }
    property Anchors;
    property AutoSize;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner nodefault;
    property BorderOuter nodefault;
    property BorderSides nodefault;
    property BorderWidth nodefault;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FullRepaint default False;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property Height default 32;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowDockClientCaptions default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Transparent;
    property UseDockManager default True;
    property Visible;
    property VisualStyle;
    property Width default 32;

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
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  {====================================}
  {== TRzStatusBar Class Declaration ==}
  {====================================}

  TRzStatusBar = class( TRzCustomPanel )
  private
    FAutoStyle: Boolean;
    FSizeGripCanvas: TCanvas;
    FSimpleStatus: Boolean;
    FSimpleCaption: TCaption;
    FSimpleFrameStyle: TFrameStyle;
    FSizeGripValid: Boolean;
    FShowSizeGrip: Boolean;
    FAutoScalePanes: Boolean;
    FFirst: Boolean;
    FDelta: Integer;
    FLastWidth: Integer;

    { Message Handling Methods }
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure WndProc( var Msg: TMessage ); override;
    procedure Resize; override;
    function GetClientRect: TRect; override;
    procedure PaintSizeGrip( R: TRect );
    procedure DrawSimpleStatusBorder( R: TRect ); virtual;

    procedure GetGradientColors( var StartColor, StopColor: TColor ); override;
    procedure Paint; override;

    procedure ValidateSizeGrip;
    function SizeGripRect: TRect;
    procedure AdjustStyle; virtual;

    { Property Access Methods }
    procedure SetShowSizeGrip( Value: Boolean ); virtual;
    procedure SetSimpleCaption( Value: TCaption ); virtual;
    procedure SetSimpleStatus( Value: Boolean ); virtual;
    procedure SetSimpleFrameStyle( Value: TFrameStyle ); virtual;

    procedure SetBorderInner( Value: TFrameStyleEx ); override;
    procedure SetBorderOuter( Value: TFrameStyleEx ); override;
    procedure SetBorderSides( Value: TSides ); override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    { Property Declarations }
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AutoScalePanes: Boolean
      read FAutoScalePanes
      write FAutoScalePanes
      default False;

    property AutoStyle: Boolean
      read FAutoStyle
      write FAutoStyle
      default True;

    property ShowSizeGrip: Boolean
      read FShowSizeGrip
      write SetShowSizeGrip
      default True;

    property SimpleCaption: TCaption
      read FSimpleCaption
      write SetSimpleCaption;

    property SimpleFrameStyle: TFrameStyle
      read FSimpleFrameStyle
      write SetSimpleFrameStyle
      default fsFlat;

    property SimpleStatus: Boolean
      read FSimpleStatus
      write SetSimpleStatus
      default False;

    { Inherited Properties & Events }
    property Align default alBottom;
    property Anchors;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner nodefault;
    property BorderOuter nodefault;
    property BorderSides nodefault;
    property BorderWidth nodefault;
    property Color;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FullRepaint;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property Height;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;
    property VisualStyle;

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
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  {====================================}
  {== TRzFlowPanel Class Declaration ==}
  {====================================}

  TRzFlowPanel = class( TCustomFlowPanel, IRzCustomFramingNotification )
  private
    FInAlignControls: Boolean;
    FBorderInner: TFrameStyleEx;
    FBorderOuter: TFrameStyleEx;
    FBorderSides: TSides;
    FBorderColor: TColor;
    FBorderHighlight: TColor;
    FBorderShadow: TColor;
    FFlatColor: TColor;
    FFlatColorAdjustment: Integer;

    FVisualStyle: TRzVisualStyle;
    FGradientColorStyle: TRzGradientColorStyle;
    FGradientColorStart: TColor;
    FGradientColorStop: TColor;
    FGradientDirection: TGradientDirection;

    FTransparent: Boolean;

    FEnabledList: TStringList;
    FOnPaint: TNotifyEvent;

    { Message Handling Methods }
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message wm_WindowPosChanged;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
    procedure WMThemeChanged( var Msg: TMessage ); message wm_ThemeChanged;
  protected
    FAboutInfo: TRzAboutInfo;
    FThemeAware: Boolean;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function CursorPosition: TPoint; virtual;
    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;
    procedure FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean ); virtual;
    procedure AdjustClientRect( var Rect: TRect ); override;
    function GetControlRect: TRect; virtual;

    procedure GetGradientColors( var StartColor, StopColor: TColor ); virtual;
    procedure Paint; override;
    procedure CustomFramingChanged; virtual;

    { Property Access Methods }
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderInner( Value: TFrameStyleEx ); virtual;
    procedure SetBorderOuter( Value: TFrameStyleEx ); virtual;
    procedure SetBorderSides( Value: TSides ); virtual;
    procedure SetBorderHighlight( Value: TColor ); virtual;
    procedure SetBorderShadow( Value: TColor ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetGradientColorStyle( Value: TRzGradientColorStyle ); virtual;
    procedure SetGradientColorStart( Value: TColor ); virtual;
    procedure SetGradientColorStop( Value: TColor ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;
    procedure SetVisualStyle( Value: TRzVisualStyle ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

    property Canvas;
    property DockManager;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Property Declarations }
    property BorderInner: TFrameStyleEx
      read FBorderInner
      write SetBorderInner
      default fsNone;

    property BorderOuter: TFrameStyleEx
      read FBorderOuter
      write SetBorderOuter
      default fsRaised;

    property BorderSides: TSides
      read FBorderSides
      write SetBorderSides
      default [ sdLeft, sdTop, sdRight, sdBottom ];

    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      default clBtnFace;

    property BorderHighlight: TColor
      read FBorderHighlight
      write SetBorderHighlight
      default clBtnHighlight;

    property BorderShadow: TColor
      read FBorderShadow
      write SetBorderShadow
      default clBtnShadow;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property GradientColorStyle: TRzGradientColorStyle
      read FGradientColorStyle
      write SetGradientColorStyle
      default gcsSystem;

    property GradientColorStart: TColor
      read FGradientColorStart
      write SetGradientColorStart
      default clWhite;

    property GradientColorStop: TColor
      read FGradientColorStop
      write SetGradientColorStop
      default clBtnFace;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalEnd;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default False;

    property VisualStyle: TRzVisualStyle
      read FVisualStyle
      write SetVisualStyle
      default vsWinXP;

    property OnPaint: TNotifyEvent
      read FOnPaint
      write FOnPaint;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoSize;
    property AutoWrap;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlowStyle;
    property Font;
    property FullRepaint;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;


  {====================================}
  {== TRzGridPanel Class Declaration ==}
  {====================================}

  TRzGridPanel = class( TCustomGridPanel, IRzCustomFramingNotification )
  private
    FInAlignControls: Boolean;
    FBorderInner: TFrameStyleEx;
    FBorderOuter: TFrameStyleEx;
    FBorderSides: TSides;
    FBorderColor: TColor;
    FBorderHighlight: TColor;
    FBorderShadow: TColor;
    FFlatColor: TColor;
    FFlatColorAdjustment: Integer;

    FVisualStyle: TRzVisualStyle;
    FGradientColorStyle: TRzGradientColorStyle;
    FGradientColorStart: TColor;
    FGradientColorStop: TColor;
    FGradientDirection: TGradientDirection;

    FTransparent: Boolean;

    FEnabledList: TStringList;
    FOnPaint: TNotifyEvent;

    { Message Handling Methods }
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message wm_WindowPosChanged;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
    procedure WMThemeChanged( var Msg: TMessage ); message wm_ThemeChanged;
  protected
    FAboutInfo: TRzAboutInfo;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function CursorPosition: TPoint; virtual;
    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;
    procedure FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean ); virtual;
    procedure AdjustClientRect( var Rect: TRect ); override;
    function GetControlRect: TRect; virtual;

    procedure GetGradientColors( var StartColor, StopColor: TColor ); virtual;
    procedure DrawGridLines;
    procedure Paint; override;
    procedure CustomFramingChanged; virtual;

    { Property Access Methods }
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderInner( Value: TFrameStyleEx ); virtual;
    procedure SetBorderOuter( Value: TFrameStyleEx ); virtual;
    procedure SetBorderSides( Value: TSides ); virtual;
    procedure SetBorderHighlight( Value: TColor ); virtual;
    procedure SetBorderShadow( Value: TColor ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetGradientColorStyle( Value: TRzGradientColorStyle ); virtual;
    procedure SetGradientColorStart( Value: TColor ); virtual;
    procedure SetGradientColorStop( Value: TColor ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;
    procedure SetVisualStyle( Value: TRzVisualStyle ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

    property Canvas;
    property DockManager;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Property Declarations }
    property BorderInner: TFrameStyleEx
      read FBorderInner
      write SetBorderInner
      default fsNone;

    property BorderOuter: TFrameStyleEx
      read FBorderOuter
      write SetBorderOuter
      default fsRaised;

    property BorderSides: TSides
      read FBorderSides
      write SetBorderSides
      default [ sdLeft, sdTop, sdRight, sdBottom ];

    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      default clBtnFace;

    property BorderHighlight: TColor
      read FBorderHighlight
      write SetBorderHighlight
      default clBtnHighlight;

    property BorderShadow: TColor
      read FBorderShadow
      write SetBorderShadow
      default clBtnShadow;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property GradientColorStyle: TRzGradientColorStyle
      read FGradientColorStyle
      write SetGradientColorStyle
      default gcsSystem;

    property GradientColorStart: TColor
      read FGradientColorStart
      write SetGradientColorStart
      default clWhite;

    property GradientColorStop: TColor
      read FGradientColorStop
      write SetGradientColorStop
      default clBtnFace;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalEnd;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default False;

    property VisualStyle: TRzVisualStyle
      read FVisualStyle
      write SetVisualStyle
      default vsWinXP;

    property OnPaint: TNotifyEvent
      read FOnPaint
      write FOnPaint;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoSize;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Color;
    property ColumnCollection;
    property Constraints;
    property ControlCollection;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExpandStyle;
    property Font;
    property FullRepaint;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RowCollection;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;


implementation

uses
  {&RAS}
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Themes,
  UxTheme,
  RzRadChk,
  RzPopups,
  RzToolbarForm;

resourcestring
  sRzCustomizeTitle = 'Customize Toolbar';
  sRzHint = 'Uncheck to hide control';
  sRzMoveUp = 'Move Up';
  sRzMoveDown = 'Move Down';
  sRzClose = 'Close';
  sRzTextOptions = 'Text Options';
  sRzNoTextLabels = 'No text labels';
  sRzShowTextLabels = 'Show text labels';
  sRzSelectiveTextOnRight = 'Selective text on right';

const
  ToolbarPopupButtonWidth = 14;


{=================================}
{== TRzPanelDockManager Methods ==}
{=================================}

type
  TTextControl = class( TControl )
  end;

constructor TRzPanelDockManager.Create( DockSite: TWinControl );
begin
  inherited;
  FPanel := DockSite as TRzCustomPanel;
  FGrabberSize := 14;

  FFont := TFont.Create;
  FFont.Name := 'Verdana';
  FFont.Size := 8;

  FCloseFont := TFont.Create;
  FCloseFont.Name := 'Marlett';
  FCloseFont.Size := 8;

  if not (csDesigning in DockSite.ComponentState) then
  begin
    FOldWndProc := DockSite.WindowProc;
    DockSite.WindowProc := WindowProcHook;
  end;

end;


destructor TRzPanelDockManager.Destroy;
begin
  if @FOldWndProc <> nil then
  begin
    DockSite.WindowProc := FOldWndProc;
    FOldWndProc := nil;
  end;

  FFont.Free;
  FCloseFont.Free;
  inherited;
end;


procedure TRzPanelDockManager.WindowProcHook( var Msg: TMessage );
begin
  // This allows us to change the color of the caption bar on focus changes.
  if ( Msg.Msg = wm_Command ) or ( Msg.Msg = wm_MouseActivate ) then
    DockSite.Invalidate;

  if Assigned( FOldWndProc ) then
    FOldWndProc( Msg );
end;



procedure TRzPanelDockManager.AdjustDockRect( Control: TControl; var ARect: TRect );
begin
  // Allocate room for the caption on the left if docksite is horizontally
  // oriented, otherwise allocate room for the caption on the top.
  if FPanel.Align in [ alTop, alBottom ] then
    Inc( ARect.Left, FGrabberSize )
  else
    Inc( ARect.Top, FGrabberSize );
end;


procedure TRzPanelDockManager.DrawVertTitle( Canvas: TCanvas; const Caption: string; Bounds: TRect );
var
  R, TempRct: TRect;
  Center: TPoint;
  Flags: Word;
  OldTextAlign: Integer;

  function TextAligned( A: Integer ): Boolean;
  begin
    Result := ( Flags and A ) = A;
  end;

begin
  with Canvas do
  begin
    OldTextAlign := GetTextAlign( Canvas.Handle );
    R := Bounds;

    Flags := dt_ExpandTabs or DrawTextAlignments[ taLeftJustify ];

    Center.X := R.Right - 2;
    Center.Y := R.Bottom - 3;
    SetTextAlign( Canvas.Handle, ta_Left or ta_Baseline );

    Font.Handle := RotateFont( FFont, 90 );
    Canvas.Font.Color := clCaptionText;

    TempRct := R;
    TextRect( TempRct, Center.X, Center.Y, Caption );

    SetTextAlign( Canvas.Handle, OldTextAlign );
  end;
end; {= TRzPanelDockManager.DrawTitle =}


procedure TRzPanelDockManager.PaintDockFrame( Canvas: TCanvas; Control: TControl; const ARect: TRect );
var
  R: TRect;
  S: string;
begin
  if not FPanel.ShowDockClientCaptions then
    inherited
  else
  begin
    S := TTextControl( Control ).Text;
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

    if FPanel.Align in [ alTop, alBottom ] then
    begin
      R := Rect( ARect.Left, ARect.Top, ARect.Left + FGrabberSize, ARect.Bottom );
      DrawVertTitle( Canvas, S, R );

      // Draw the Close X
      Canvas.Font.Name := FCloseFont.Name;
      R := Rect( ARect.Left + 1, ARect.Top + 1, ARect.Left + FGrabberSize - 2, ARect.Top + 12 );
      Canvas.TextRect( R, R.Left, R.Top, 'r' );
    end
    else
    begin
      R := Rect( ARect.Left, ARect.Top, ARect.Right, ARect.Top + FGrabberSize );
      Canvas.TextRect( R, R.Left + 2, R.Top, S );

      // Draw the Close X
      Canvas.Font.Name := FCloseFont.Name;
      R := Rect( ARect.Right - FGrabberSize - 1, ARect.Top + 1, ARect.Right - 2, ARect.Top + 12 );
      Canvas.TextRect( R, R.Left, R.Top, 'r' );
    end;
  end;
end; {= TRzPanelDockManager.PaintDockFrame =}


procedure TRzPanelDockManager.PaintSite( DC: HDC );
begin
  inherited;
end;



{&RT}
{============================}
{== TRzCustomPanel Methods ==}
{============================}

constructor TRzCustomPanel.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
                              { Prevent Caption from being set to default name }
  ControlStyle := ControlStyle - [ csSetCaption ];

  // Delphi 7 sets the csParentBackground style and removes the csOpaque style when Themes are available, which causes
  // all kinds of other problems, so we restore these.
  ControlStyle := ControlStyle - [ csParentBackground ] + [ csOpaque ];

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FPaintClientArea := True;
  FBorderSides := [ sdLeft, sdTop, sdRight, sdBottom ];
  FBorderColor := clBtnFace;
  FBorderHighlight := clBtnHighlight;
  FBorderShadow := clBtnShadow;
  FBorderOuter := fsRaised;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;
  BevelOuter := bvNone;
  FAlignmentVertical := avCenter;
  FInAlignControls := False;

  FShowGrid := False;
  FGridColor := clBtnShadow;
  FGridStyle := gsDots;
  FGridXSize := 8;
  FGridYSize := 8;

  FTextMargin := 0;
  FTextMarginVertical := 0;
  FWordWrap := True;

  FVisualStyle := vsWinXP;
  FGradientColorStyle := gcsSystem;
  FGradientColorStart := clWhite;
  FGradientColorStop := clBtnFace;
  FGradientDirection := gdHorizontalEnd;

  FShowDockClientCaptions := True;

  FEnabledList := TStringList.Create;

  FPanelColor := clBtnFace;
  {&RV}
end;


destructor TRzCustomPanel.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  FEnabledList.Free;
  inherited;
end;


procedure TRzCustomPanel.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Handle the fact that the FrameSides property was published in version 2.x
  Filer.DefineProperty( 'FrameSides', TRzOldPropReader.ReadOldSetProp, nil, False );


  // Handle the fact that the ThemeAware property was published in verison 3.x
  Filer.DefineProperty( 'ThemeAware',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );

  // Handle the fact that the UseGradients property was replaced with the
  // VisualStyle property
  Filer.DefineProperty( 'UseGradients', ReadOldUseGradientsProp, nil, False );
end;


procedure TRzCustomPanel.ReadOldUseGradientsProp( Reader: TReader );
var
  UseGradients: Boolean;
begin
  UseGradients := Reader.ReadBoolean;
  if UseGradients then
    VisualStyle := vsGradient;
end;


procedure TRzCustomPanel.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzCustomPanel.Loaded;
begin
  inherited;
  {$IFDEF VCL160_OR_HIGHER}
  UpdatePanelColor;
  {$ENDIF}
end;


function TRzCustomPanel.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzCustomPanel.AlignControls( AControl: TControl; var Rect: TRect );
begin
  FixClientRect( Rect, False );
  inherited;
end;


procedure TRzCustomPanel.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
  // The following code handles situations where the panel is aligned alRight
  // or alBottom and there is another control also aligned alRight or alBottom
  // adjacent to this control.  If the size of this panel is changed the order
  // of all the controls aligned in the same direction will get re-ordered.

  if Align = alBottom then
  begin
    if AHeight <> Height then
      ATop := Top - ( AHeight - Height );
  end
  else if Align = alRight then
  begin
    if AWidth <> Width then
      ALeft := Left - ( AWidth - Width );
  end;

  inherited;
end;


procedure TRzCustomPanel.FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean );

  procedure AdjustRect( var R: TRect; Sides: TSides; N: Integer );
  begin
    if sdLeft in Sides then
      Inc( R.Left, N );
    if sdTop in Sides then
      Inc( R.Top, N );
    if sdRight in Sides then
      Dec( R.Right, N );
    if sdBottom in Sides then
      Dec( R.Bottom, N );
  end;

begin
  if ShrinkByBorder then
    InflateRect( Rect, -BorderWidth, -BorderWidth );

  if FBorderOuter = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderOuter in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderOuter in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );

  if FBorderInner = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderInner in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderInner in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );
end;



procedure TRzCustomPanel.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  if DockSite then
    FixClientRect( Rect, False );
end;


function TRzCustomPanel.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
end;


procedure TRzCustomPanel.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
var
  R, CR: TRect;
begin
  if FullRepaint or ( Caption <> '' ) then
    Invalidate
  else
  begin

    R := Rect( 0, 0, Width, Height );
    CR := R;
    FixClientRect( CR, True );

    if Msg.WindowPos^.cx <> R.Right then
    begin
      R.Left := CR.Right - InvalidateMarginSize.X;
      R.Top := 0;
      InvalidateRect( Handle, @R, True );
    end;

    if Msg.WindowPos^.cy <> R.Bottom then
    begin
      R.Left := 0;
      R.Top := CR.Bottom - InvalidateMarginSize.Y;
      InvalidateRect( Handle, @R, True );
    end;
  end;
  inherited;
end;


procedure TRzCustomPanel.GetGradientColors( var StartColor, StopColor: TColor );
begin
  if FGradientColorStyle <> gcsCustom then
  begin
    GetGradientPanelColors( FGradientColorStyle, StartColor, StopColor );
  end
  else
  begin
    StartColor := FGradientColorStart;
    StopColor := FGradientColorStop;
  end;
end;


procedure TRzCustomPanel.GetGradientFrameColor( var FrameColor: TColor;
                                                var FrameColorAdjustment: Integer );
begin
  if ( FVisualStyle = vsGradient ) and ( FGradientColorStyle <> gcsCustom ) then
  begin
    FrameColor := GetGradientPanelFrameColor( FGradientColorStyle );
    FrameColorAdjustment := 0;
  end
  else
  begin
    FrameColor := FFlatColor;
    FrameColorAdjustment := FFlatColorAdjustment;
  end;
end;


procedure TRzCustomPanel.DrawCaption( Rect: TRect );
var
  TextRct: TRect;
  H: Integer;
  Flags: Longint;
  {$IFDEF VCL160_OR_HIGHER}
  C: TColor;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  if Caption <> '' then
  begin
    Canvas.Font := Self.Font;

    InflateRect( Rect, -FTextMargin, -FTextMarginVertical );

    TextRct := Rect;
    Flags := dt_CalcRect or dt_ExpandTabs or dt_VCenter or
             DrawTextWordWrap[ WordWrap ] or DrawTextAlignments[ Alignment ];

    if UsingSystemStyle then
    begin
      H := DrawString( Canvas, Caption, TextRct, Flags );
    end
    else // VCL Styles
    begin
      {$IFDEF VCL160_OR_HIGHER}
      Details := StyleServices.GetElementDetails( tpPanelBackground );
      if not StyleServices.GetElementColor( Details, ecTextColor, C ) or ( C = clNone ) then
        C := Font.Color;
      StyleServices.DrawText( Canvas.Handle, Details, Caption, TextRct, TTextFormatFlags( Flags ), C );
      H := TextRct.Bottom - TextRct.Top;

      {$ELSE}

      // To eliminate warnings in earlier versions of Delphi -- this code will not be called
      H := 0;
      {$ENDIF}
    end;

    case FAlignmentVertical of
      avTop:
      begin
        if Rect.Top + H < Rect.Bottom then
          Rect.Bottom := Rect.Top + H;
      end;

      avCenter:
      begin
        Rect.Top := ( ( Rect.Bottom + Rect.Top ) - H ) div 2;
        if Rect.Top + H < Rect.Bottom then
          Rect.Bottom := Rect.Top + H;
      end;

      avBottom:
      begin
        if Rect.Bottom - H - 1 > Rect.Top then
          Rect.Top := Rect.Bottom - H - 1;
      end;
    end;

    Canvas.Brush.Style := bsClear;

    Flags := dt_ExpandTabs or dt_VCenter or
             DrawTextWordWrap[ WordWrap ] or DrawTextAlignments[ Alignment ];


    if UsingSystemStyle then
    begin
      if not Enabled then
        Canvas.Font.Color := clGrayText;

      DrawString( Canvas, Caption, Rect, Flags );
    end
    else // VCL Styles
    begin
      {$IFDEF VCL160_OR_HIGHER}
      if not StyleServices.GetElementColor( Details, ecTextColor, C ) or ( C = clNone ) then
        C := Font.Color;
      if not Enabled then
        C := StyleServices.GetSystemColor( clGrayText );
      StyleServices.DrawText( Canvas.Handle, Details, Caption, Rect, TTextFormatFlags( Flags ), C );
      {$ENDIF}
    end;
  end;
end; {= TRzCustomPanel.DrawCaption =}


function TRzCustomPanel.GetControlRect: TRect;
begin
  Result := Rect( 0, 0, Width, Height );
end;


function TRzCustomPanel.InvalidateMarginSize: TPoint;
begin
  Result := Point( 1, 1 );
end;


procedure TRzCustomPanel.DrawGrid( Rect: TRect );
var
  X, Y, XCount, YCount: Integer;
  GC: TColor;

  procedure DrawHorzLine( X1, X2, Y: Integer );
  var
    X: Integer;
  begin
    if FGridStyle = gsSolidLines then
    begin
      Canvas.MoveTo( X1, Y );
      Canvas.LineTo( X2, Y );
    end
    else
    begin
      X := X1 + 1;
      Canvas.MoveTo( X, Y );
      while X < X2 do
      begin
        Canvas.Pixels[ X, Y ] := GC;
        Inc( X, 2 );
      end;
    end;
  end;

  procedure DrawVertLine( X, Y1, Y2: Integer );
  var
    Y: Integer;
  begin
    if FGridStyle = gsSolidLines then
    begin
      Canvas.MoveTo( X, Y1 );
      Canvas.LineTo( X, Y2 );
    end
    else
    begin
      Y := Y1 + 1;
      Canvas.MoveTo( X, Y );
      while Y < Y2 do
      begin
        Canvas.Pixels[ X, Y ] := GC;
        Inc( Y, 2 );
      end;

    end;
  end;

begin {= TRzCustomPanel.DrawGrid =}
  if FGridXSize > 0 then
    XCount := ( Rect.Right - Rect.Left ) div FGridXSize
  else
    XCount := 0;
  if FGridYSize > 0 then
    YCount := ( Rect.Bottom - Rect.Top ) div FGridYSize
  else
    YCount := 0;

  GC := ActiveStyleSystemColor( FGridColor );

  Canvas.Pen.Color := GC;
  case FGridStyle of
    gsDots:
    begin
      for X := 1 to XCount do
      begin
        for Y := 1 to YCount do
          Canvas.Pixels[ Rect.Left - 1 + X * FGridXSize, Rect.Top - 1 + Y * FGridYSize ] := GC;
      end;
    end;

    gsDottedLines, gsSolidLines:
    begin
      for X := 1 to XCount do
        DrawVertLine( Rect.Left - 1 + X * FGridXSize, Rect.Top, Rect.Bottom );

      for Y := 1 to YCount do
        DrawHorzLine( Rect.Left, Rect.Right, Rect.Top - 1 + Y * FGridYSize );
    end;
  end;
end; {= TRzCustomPanel.DrawGrid =}


procedure TRzCustomPanel.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
begin
  if FTransparent and PaintClientArea then
  begin
    if ( Parent <> nil ) and Parent.DoubleBuffered then
      PerformEraseBackground( Self, Msg.DC );
    DrawParentImage( Self, Msg.DC, True );

    // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from
    // erasing background. Set Msg.Result to 1 to indicate background is painted
    // by the control.
    Msg.Result := 1;
  end
  else
    inherited;
end;



type
  TWinControlAccess = class( TWinControl );


procedure TRzCustomPanel.Paint;
var
  R, SaveRect: TRect;
  StartColor, StopColor, PanelColor, FrameColor, ParentColor, BdrColor, BdrHighlight, BdrShadow: TColor;
  FrameColorAdjustment: Integer;
  RoundCorners: Boolean;
  {$IFDEF VCL160_OR_HIGHER}
  C: TColor;
  Style: TCustomStyleServices;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  R := GetControlRect;


  if UsingSystemStyle or ( VisualStyle = vsClassic ) then
  begin
    GetGradientFrameColor( FrameColor, FrameColorAdjustment );
    if Parent <> nil then
      ParentColor := TWinControlAccess( Parent ).Color
    else
      ParentColor := clBtnFace;
    BdrColor := FBorderColor;
    BdrHighlight := FBorderHighlight;
    BdrShadow := FBorderShadow;
    PanelColor := Color;
  end
  else // VCL Styles
  begin
    {$IFDEF VCL160_OR_HIGHER}
    Style := StyleServices;

    FrameColor := Style.GetSystemColor( FFlatColor );
    FrameColorAdjustment := FFlatColorAdjustment;

    Details := Style.GetElementDetails( tpPanelBackground );
    if Style.GetElementColor( Details, ecFillColor, C ) and ( C <> clNone ) then
      PanelColor := C
    else
      PanelColor := Color;
    ParentColor := PanelColor;

    Details := Style.GetElementDetails( tpPanelBevel );
    if Style.GetElementColor( Details, ecEdgeHighLightColor, C ) and ( C <> clNone ) then
      BdrHighlight := C
    else
      BdrHighlight := FBorderHighlight;

    if Style.GetElementColor( Details, ecEdgeShadowColor, C ) and ( C <> clNone ) then
      BdrShadow := C
    else
      BdrShadow := FBorderShadow;

    BdrColor := Style.GetSystemColor( FBorderColor );

    {$ELSE}

    // To eliminate warnings in earlier versions of Delphi -- this code will not be called
    FrameColor := clNone;
    FrameColorAdjustment := 0;
    PanelColor := clNone;
    ParentColor := clNone;
    BdrColor := clNone;
    BdrHighlight := clNone;
    BdrShadow := clNone;
    {$ENDIF}
  end;



  R := DrawInnerOuterBorders( Canvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, BevelWidth,
                              BdrColor, BdrHighlight, BdrShadow,
                              FrameColor, FrameColorAdjustment,
                              PanelColor, ParentColor, FTransparent );

  if not FTransparent and PaintClientArea then
  begin
    // Adjust inside border rectangle if round border
//    RoundCorners := ( BorderOuter = fsFlatRounded ) and ( BorderInner in [ fsNone, fsFlatRounded ] );
    RoundCorners := ( ( BorderOuter = fsFlatRounded ) and ( BorderInner = fsNone ) and ( BorderWidth = 0 ) ) or
                    ( ( BorderInner = fsFlatRounded ) );

    if RoundCorners then
    begin
      SaveRect := R;
      if sdLeft in BorderSides then
        Dec( R.Left );
      if sdTop in BorderSides then
        Dec( R.Top );
      if sdRight in BorderSides then
        Inc( R.Right );
      if sdBottom in BorderSides then
        Inc( R.Bottom );
    end;

    case FVisualStyle of
      vsClassic, vsWinXP:
      begin
        Canvas.Brush.Color := PanelColor;
        Canvas.FillRect( R );
      end;

      vsGradient:
      begin
        GetGradientColors( StartColor, StopColor );
        PaintGradient( Canvas, R, FGradientDirection, StartColor, StopColor );
      end;
    end;

    // Restore round corners by setting the corner color pixel back to flat color
    if RoundCorners then
    begin
      ParentColor := AdjustColor( FrameColor, FrameColorAdjustment );
      if ( sdLeft in BorderSides ) and ( sdTop in BorderSides ) then
        Canvas.Pixels[ R.Left, R.Top ] := ParentColor;
      if ( sdTop in BorderSides ) and ( sdRight in BorderSides ) then
        Canvas.Pixels[ R.Right - 1, R.Top ] := ParentColor;
      if ( sdRight in BorderSides ) and ( sdBottom in BorderSides ) then
        Canvas.Pixels[ R.Right - 1, R.Bottom - 1 ] := ParentColor;
      if ( sdLeft in BorderSides ) and ( sdBottom in BorderSides ) then
        Canvas.Pixels[ R.Left, R.Bottom - 1 ] := ParentColor;
      R := SaveRect;
    end;
  end;

  if FShowGrid then
    DrawGrid( R );

  Canvas.Brush.Style := bsClear;
  DrawCaption( R );
  Canvas.Brush.Style := bsSolid;

  if Assigned( FOnPaint ) then
    FOnPaint( Self );

end; {= TRzCustomPanel.Paint =}


procedure TRzCustomPanel.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameStyle in FFrameControllerNotifications then
      FBorderOuter := FFrameController.FrameStyle;
    if fcpFrameSides in FFrameControllerNotifications then
      FBorderSides := FFrameController.FrameSides;
    if fcpFrameColor in FFrameControllerNotifications then
    begin
      FFlatColor := FFrameController.FrameColor;
      FFlatColorAdjustment := 0;
    end;
    if fcpColor in FFrameControllerNotifications then
      Color := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;

    if fcpColor in FFrameControllerNotifications then
    begin
      FBorderHighlight := LighterColor( Color, 100 );
      FBorderShadow := DarkerColor( Color, 50 );
    end;

    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetAlignmentVertical( Value: TAlignmentVertical );
begin
  if FAlignmentVertical <> Value then
  begin
    FAlignmentVertical := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetBorderSides( Value: TSides );
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TRzCustomPanel.SetBorderHighlight( Value: TColor );
begin
  if FBorderHighlight <> Value then
  begin
    FBorderHighlight := Value;
    Invalidate;
  end;
end;

procedure TRzCustomPanel.SetBorderShadow( Value: TColor );
begin
  if FBorderShadow <> Value then
  begin
    FBorderShadow := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetBorderInner( Value: TFrameStyleEx );
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetBorderOuter( Value: TFrameStyleEx );
begin
  if FBorderOuter <> Value then
  begin
    FBorderOuter := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TRzCustomPanel.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomPanel.SetGradientColorStyle( Value: TRzGradientColorStyle );
begin
  if FGradientColorStyle <> Value then
  begin
    FGradientColorStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetGradientColorStart( Value: TColor );
begin
  if FGradientColorStart <> Value then
  begin
    FGradientColorStart := Value;
    Invalidate;
    InvalidateControls( Self );
  end;
end;


procedure TRzCustomPanel.SetGradientColorStop( Value: TColor );
begin
  if FGradientColorStop <> Value then
  begin
    FGradientColorStop := Value;
    Invalidate;
    InvalidateControls( Self );
  end;
end;


procedure TRzCustomPanel.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Invalidate;
    InvalidateControls( Self );
  end;
end;


procedure TRzCustomPanel.SetGridColor( Value: TColor );
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetGridStyle( Value: TRzGridStyle );
begin
  if FGridStyle <> Value then
  begin
    FGridStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetGridXSize( Value: Word );
begin
  if FGridXSize <> Value then
  begin
    FGridXSize := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetGridYSize( Value: Word );
begin
  if FGridYSize <> Value then
  begin
    FGridYSize := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetGridSize( XSize, YSize: Integer );
begin
  SetGridXSize( XSize );
  SetGridYSize( YSize );
end;


procedure TRzCustomPanel.SetPaintClientArea( Value: Boolean );
begin
  if FPaintClientArea <> Value then
  begin
    FPaintClientArea := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetTextMargin( Value: Integer );
begin
  if FTextMargin <> Value then
  begin
    FTextMargin := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetTextMarginVertical( Value: Integer );
begin
  if FTextMarginVertical <> Value then
  begin
    FTextMarginVertical := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetShowGrid( Value: Boolean );
begin
  if FShowGrid <> Value then
  begin
    FShowGrid := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetTransparent( Value: Boolean );
var
  I: Integer;
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [ csOpaque ]
    else
      ControlStyle := ControlStyle + [ csOpaque ];
    Invalidate;
    for I := 0 to ControlCount - 1 do
      Controls[ I ].Invalidate;
  end;
end;


procedure TRzCustomPanel.SetVisualStyle( Value: TRzVisualStyle );
begin
  if FVisualStyle <> Value then
  begin
    FVisualStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.SetWordWrap( Value: Boolean );
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Invalidate;
  end;
end;


procedure TRzCustomPanel.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;
end;


procedure TRzCustomPanel.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
end;


procedure TRzCustomPanel.EnableChildControls( Enabled: Boolean );
var
  I, Idx: Integer;
begin
  if not Enabled then
  begin
    FEnabledList.Clear;

    for I := 0 to ControlCount - 1 do
    begin
      if Controls[ I ].Enabled then
        FEnabledList.AddObject( '1', Controls[ I ] )
      else
        FEnabledList.AddObject( '0', Controls[ I ] );
    end;

    for I := 0 to ControlCount - 1 do
      Controls[ I ].Enabled := False;
  end
  else
  begin
    for I := 0 to ControlCount - 1 do
    begin
      Idx := FEnabledList.IndexOfObject( Controls[ I ] );
      if Idx <> -1 then
      begin
        if FEnabledList[ Idx ] = '1' then
          Controls[ I ].Enabled := True;
      end
      else
        Controls[ I ].Enabled := True;
    end;
  end;
end;


procedure TRzCustomPanel.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  Repaint;

  EnableChildControls( Enabled );
end;


function TRzCustomPanel.CreateDockManager: IDockManager;
begin
  if ( DockManager = nil ) and DockSite and UseDockManager then
    Result := TRzPanelDockManager.Create( Self )
  else
    Result := DockManager;
  DoubleBuffered := DoubleBuffered or (Result <> nil);
end;


procedure TRzCustomPanel.WMThemeChanged( var Msg: TMessage );
begin
  inherited;
  // Update CurrentXPColorScheme global variable
  CurrentXPColorScheme := GetXPColorScheme;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzCustomPanel.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if UsingSystemStyle then
    FPanelColor := Color;
end;


procedure TRzCustomPanel.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  UpdatePanelColor;
end;


procedure TRzCustomPanel.UpdatePanelColor;
begin
  if UsingSystemStyle then
    Color := FPanelColor
  else
    Color := ActiveStyleSystemColor( clBtnFace );
end;

{$ENDIF}


{===============================}
{== TRzCustomGroupBox Methods ==}
{===============================}

constructor TRzCustomGroupBox.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle + [ csSetCaption, csDoubleClicks ];
  BorderOuter := fsNone;
  BorderInner := fsNone;
  Alignment := taLeftJustify;
  AlignmentVertical := avTop;
  Height := 105;

  FCaptionFont := TFont.Create;
  FCaptionFont.Assign( Self.Font );
  FCaptionFontChanged := False;
  FCaptionFont.OnChange := CaptionFontChangeHandler;

  FChecked := True;
  FEnableControlsOnCheck := True;
  FShowCheckBox := False;
  FCheckBoxSize := DefaultGlyphWidth;
  FDragging := False;
  FMouseOverCheckBox := False;

  { Initializing GroupStyle must occur after BorderOuter/BorderInner settings
    b/c SetBorderOuter/SetBorderInner change GroupStyle to gsCustom }
  FGroupStyle := gsFlat;
  FBannerHeight := 0;
  {&RCI}
end;


procedure TRzCustomGroupBox.CreateWnd;
begin
  inherited;
  if RunningAtLeast( win2000 ) then
    Perform( wm_ChangeUIState, MakeWParam( UIS_INITIALIZE, UISF_HIDEACCEL or UISF_HIDEFOCUS ), 0 );
end;


destructor TRzCustomGroupBox.Destroy;
begin
  FCaptionFont.Free;
  inherited;
end;


procedure TRzCustomGroupBox.AdjustClientRect( var Rect: TRect );
var
  H: Integer;
begin
  inherited;

  Canvas.Font := Font;
  H := Canvas.TextHeight( 'Yy' );

  case FGroupStyle of
    gsStandard, gsFlat:
    begin
      if BorderWidth > H div 2 - 1 then
        Inc( Rect.Top, H div 2 - 1 )
      else
        Inc( Rect.Top, H - BorderWidth );
      if FGroupStyle = gsStandard then
        InflateRect( Rect, -2, -2 )
      else
        InflateRect( Rect, -1, -1 );
    end;

    gsCustom:
    begin
      if Caption <> '' then
        Inc( Rect.Top, H );
      InflateRect( Rect, -2, -2 );
    end;

    gsTopLine:
    begin
      Inc( Rect.Top, H );
    end;

    gsBanner:
    begin
      if FBannerHeight = 0 then
        Inc( Rect.Top, H + 6 + 2 )    // Height of banner based on font
      else
        Inc( Rect.Top, FBannerHeight );
    end;

    gsUnderline:
    begin
      Inc( Rect.Top, H + 2 + 2 );
    end;
  end;
end;


procedure TRzCustomGroupBox.ChangeScale( M, D: Integer );
begin
  inherited;
  if FCaptionFontChanged and ( FCaptionFont <> nil ) then
    FCaptionFont.Height := MulDiv( FCaptionFont.Height, M, D );
end;


procedure TRzCustomGroupBox.CMDialogChar( var Msg: TCMDialogChar );
begin
  with Msg do
  begin
    if IsAccel( CharCode, Caption ) and CanFocus then
    begin
      SelectFirst;
      Result := 1;
    end
    else
      inherited;
  end;
end;


procedure TRzCustomGroupBox.CMFontChanged( var Msg: TMessage );

begin
  inherited;
  if not FCaptionFontChanged and ( FCaptionFont <> nil ) then
  begin
    FCaptionFont.Assign( Self.Font );
    FCaptionFontChanged := False;
  end;
  Invalidate;
end;


procedure TRzCustomGroupBox.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
  if FShowCheckBox then
  begin
    FMouseOverCheckBox := False;
    Invalidate;
  end;
end;


procedure TRzCustomGroupBox.CustomFramingChanged;
begin
  if FFrameController.FrameVisible and ( FGroupStyle = gsCustom ) then
    inherited;
end;


procedure TRzCustomGroupBox.Paint;
var
  H: Integer;
  R, ExtentRect: TRect;
  C, StartColor, StopColor, DividerColor: TColor;
  S: string;
  CaptionSize: TSize;
  GroupBoxDetails: TThemedElementDetails;
  TempAlignment: TAlignment;


  function GetTextExtent( DC: HDC; Details: TThemedElementDetails; const S: WideString;
                          const Flags: Cardinal ): TRect;
  begin
    Result := Rect( 0, 0, 0, 0 );
    GetThemeTextExtent( ActiveStyleServices.Theme[ teButton ], DC, Details.Part, Details.State,
                        PWideChar( S ), Length( S ), Flags, nil, Result );
  end;


begin {= TRzCustomGroupBox.Paint =}
  Canvas.Font := FCaptionFont;
  H := Canvas.TextHeight( 'Pp' );


  // Calculate FCaptionRect
  if ( Caption <> '' ) and ( FGroupStyle <> gsCustom ) then
  begin

    if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled and UsingSystemStyle and
       ( FGroupStyle in [ gsStandard, gsFlat ] ) then
    begin
      GroupBoxDetails := ActiveStyleServices.GetElementDetails( tbGroupBoxNormal );

      ExtentRect := GetTextExtent( Canvas.Handle, GroupBoxDetails, Caption, 0 );
      CaptionSize.CX := ExtentRect.Right - ExtentRect.Left;
      CaptionSize.CY := ExtentRect.Bottom - ExtentRect.Top;
    end
    else
    begin
      S := RemoveAccelerators( Caption );
      GetTextExtentPoint32( Canvas.Handle, PChar( S ), Length( S ), CaptionSize );
    end;

    if FShowCheckBox then
    begin
      // Adjust CaptionSize to take into account the size of the check box
      Inc( CaptionSize.CX, FCheckBoxSize + 4 );
    end;

    case FGroupStyle of
      gsStandard, gsFlat:
      begin
        if not UseRightToLeftAlignment then
          FCaptionRect := Rect( 8, 0, CaptionSize.CX + 8, CaptionSize.CY )
        else
          FCaptionRect := Rect( Width - CaptionSize.CX - 8, 0, Width - 8, CaptionSize.CY );
      end;

      gsTopLine:
      begin
        if not UseRightToLeftAlignment then
          FCaptionRect := Rect( 0, 0, CaptionSize.CX + 4, CaptionSize.CY )
        else
          FCaptionRect := Rect( Width - CaptionSize.CX - 4, 0, Width, CaptionSize.CY );
      end;

      gsBanner:
      begin
        if FBannerHeight = 0 then
          FCaptionRect := Rect( 0, 0, Width, CaptionSize.CY + 6 )
        else
          FCaptionRect := Rect( 0, 0, Width, FBannerHeight );
      end;

      gsUnderline:
      begin
        FCaptionRect := Rect( 0, 0, Width, CaptionSize.CY + 2 );
      end;
    end;

  end
  else
    FCaptionRect := Rect( 0, 0, 0, 0 );


  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled and
     ( FGroupStyle in [ gsStandard, gsFlat ] ) then
  begin
    if not FTransparent then
    begin
      // When group box is not transparent, the control has it's csOpaque style
      // set. This prevents the wm_EraseBkgnd message from being sent to the
      // control. Therefore, the background needs to be painted here.
      DrawParentImage( Self, Canvas.Handle, True );
    end;

    ExcludeClipRect( Canvas.Handle, FCaptionRect.Left, FCaptionRect.Top, FCaptionRect.Right, FCaptionRect.Bottom );
    R := Rect( 0, H div 2 - 1, Width, Height );

    if Enabled then
      GroupBoxDetails := ActiveStyleServices.GetElementDetails( tbGroupBoxNormal )
    else
      GroupBoxDetails := ActiveStyleServices.GetElementDetails( tbGroupBoxDisabled );

    ActiveStyleServices.DrawElement( Canvas.Handle, GroupBoxDetails, R );

    SelectClipRgn( Canvas.Handle, 0 );
    if Caption <> '' then
    begin
      if FShowCheckBox then
      begin
        DrawThemedCheckBox;
        R := AdjustCaptionRectForCheckBox;
      end
      else
        R := FCaptionRect;

      Canvas.Brush.Style := bsClear;
      ActiveStyleServicesDrawText( Canvas.Handle, GroupBoxDetails, Caption, R, DrawTextAlignments[ Alignment ] );

      if FShowCheckBox and ShowFocus and Focused then
      begin
        if UsingSystemStyle then
          DrawFocusBorder( Canvas, R )
        else
          DrawFocusBorder( Canvas, R, ActiveStyleFontColor( sfGroupBoxTextNormal ) );
      end;
    end;
  end
  else // No Themes
  begin
    if FGroupStyle <> gsCustom then
    begin
      R := Rect( 0, 0, Width, Height );

      if FTransparent then
      begin
        if FGroupStyle <> gsBanner then
        begin
          ExcludeClipRect( Canvas.Handle, FCaptionRect.Left, FCaptionRect.Top,
                           FCaptionRect.Right, FCaptionRect.Bottom );
        end;
      end
      else
      begin
        Canvas.Brush.Color := ActiveStyleSystemColor( Color );
        Canvas.FillRect( R );
      end;

      R := Rect( 0, H div 2 - 1, Width, Height );

      case FGroupStyle of
        gsStandard:
        begin
          if BorderOuter = fsNone then
            DrawBorderSides( Canvas, R, fsGroove, sdAllSides )
          else if BorderOuter in [ fsFlat, fsFlatBold ] then
          begin
            if UsingSystemStyle then
              C := AdjustColor( FFlatColor, FFlatColorAdjustment )
            else
              C := ActiveStyleSystemColor( FFlatColor );

            if BorderOuter = fsFlat then
              DrawBox( Canvas, R, C )
            else
              DrawBevel( Canvas, R, C, C, 2, sdAllSides );
          end
          else
            DrawBorderSides( Canvas, R, BorderOuter, sdAllSides );
        end;

        gsTopLine:
        begin
          if BorderOuter = fsNone then
            DrawBorderSides( Canvas, R, fsGroove, [ sdTop ] )
          else if BorderOuter in [ fsFlat, fsFlatBold ] then
          begin
            if UsingSystemStyle then
              C := AdjustColor( FFlatColor, FFlatColorAdjustment )
            else
              C := ActiveStyleSystemColor( FFlatColor );
            Canvas.Pen.Color := C;
            Canvas.MoveTo( R.Left + 2, R.Top );
            Canvas.LineTo( R.Right - 2, R.Top );
            if BorderOuter = fsFlatBold then
            begin
              Canvas.MoveTo( R.Left + 2, R.Top + 1 );
              Canvas.LineTo( R.Right - 2, R.Top + 1 );
            end;
          end
          else
            DrawBorderSides( Canvas, R, BorderOuter, [ sdTop ] );
        end;

        gsFlat:
        begin
          if UsingSystemStyle then
            C := AdjustColor( FFlatColor, FFlatColorAdjustment )
          else
            C := ActiveStyleSystemColor( FFlatColor );

          Canvas.Pen.Color := C;
          // Left side
          Canvas.MoveTo( R.Left, R.Top + 2 );
          Canvas.LineTo( R.Left, R.Bottom - 2 );
          // Top side
          Canvas.MoveTo( R.Left + 2, R.Top );
          Canvas.LineTo( R.Right - 2, R.Top );
          // Right side
          Canvas.MoveTo( R.Right - 1, R.Top + 2 );
          Canvas.LineTo( R.Right - 1, R.Bottom - 2 );
          // Bottom side
          Canvas.MoveTo( R.Left + 2, R.Bottom - 1 );
          Canvas.LineTo( R.Right - 2, R.Bottom - 1 );

          Canvas.Pixels[ R.Left + 1, R.Top + 1 ] := C;
          Canvas.Pixels[ R.Right - 2, R.Top + 1 ] := C;
          Canvas.Pixels[ R.Right - 2, R.Bottom - 2 ] := C;
          Canvas.Pixels[ R.Left + 1, R.Bottom - 2 ] := C;
        end;

        gsBanner:
        begin
          if GradientColorStyle <> gcsCustom then
          begin
            GetGradientStatusBarColors( GradientColorStyle, StopColor,
                                        StartColor, DividerColor );
            Canvas.Pen.Color := DividerColor;
          end
          else
          begin
            StartColor := FGradientColorStart;
            StopColor := FGradientColorStop;
            C := AdjustColor( FFlatColor, FFlatColorAdjustment );
            Canvas.Pen.Color := C;
          end;

          // Left side
          Canvas.MoveTo( FCaptionRect.Left, FCaptionRect.Top + 1 );
          Canvas.LineTo( FCaptionRect.Left, FCaptionRect.Bottom - 1 );
          // Top side
          Canvas.MoveTo( FCaptionRect.Left + 1, FCaptionRect.Top );
          Canvas.LineTo( FCaptionRect.Right - 1, FCaptionRect.Top );
          // Right side
          Canvas.MoveTo( FCaptionRect.Right - 1, FCaptionRect.Top + 1 );
          Canvas.LineTo( FCaptionRect.Right - 1, FCaptionRect.Bottom - 1 );
          // Bottom side
          Canvas.MoveTo( FCaptionRect.Left + 1, FCaptionRect.Bottom - 1 );
          Canvas.LineTo( FCaptionRect.Right - 1, FCaptionRect.Bottom - 1 );

          InflateRect( FCaptionRect, -1, -1 );
          PaintGradient( Canvas, FCaptionRect, gdHorizontalEnd,
                         StartColor, StopColor );
        end;

        gsUnderline:
        begin
          if UsingSystemStyle then
            C := AdjustColor( FFlatColor, FFlatColorAdjustment )
          else
            C := ActiveStyleSystemColor( FFlatColor );
          Canvas.Pen.Color := C;
          Canvas.MoveTo( FCaptionRect.Left, FCaptionRect.Bottom );
          Canvas.LineTo( FCaptionRect.Right, FCaptionRect.Bottom );
        end;
      end;
    end
    else { FGroupStyle = gsCustom }
    begin
      inherited; // Draw standard panel borders
    end;

    if ( Caption <> '' ) and ( FGroupStyle <> gsCustom ) then
    begin
      if not FTransparent and ( FGroupStyle <> gsBanner ) then
        Canvas.FillRect( FCaptionRect );

      if FGroupStyle = gsBanner then
      begin
        InflateRect( FCaptionRect, -4, -1 );
      end;

      Canvas.Brush.Style := bsClear;

      SelectClipRgn( Canvas.Handle, 0 );

      if FShowCheckBox then
      begin
        if ActiveStyleServicesEnabled then
          DrawThemedCheckBox
        else
          DrawNonThemedCheckBox;
        R := AdjustCaptionRectForCheckBox;
      end
      else
        R := FCaptionRect;

      if UsingSystemStyle then
      begin
        if Enabled and ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
        begin
          Canvas.Font.Color := GetXPThemeColor( xptcGroupBoxFont );
        end
        else if not Enabled then
        begin
          Canvas.Font.Color := clGrayText;
        end;
      end
      else // VCL Styles
      begin
        // When using VCL Styles and showing the check box, the Canvas can get messed up
        // and the Brush.Style loses transparency, so we need to reset it.
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Style := bsClear;

        if Enabled then
          Canvas.Font.Color := ActiveStyleFontColor( sfPanelTextNormal )
        else
          Canvas.Font.Color := ActiveStyleFontColor( sfPanelTextDisabled );
      end;


      TempAlignment := Alignment;
      if UseRightToLeftAlignment then
        ChangeBiDiModeAlignment( TempAlignment );

      DrawString( Canvas, Caption, R,
                  DrawTextAlignments[ TempAlignment ] or dt_VCenter or dt_SingleLine );
    end;
  end;

  Canvas.Font := Self.Font;
end; {= TRzCustomGroupBox.Paint =}


procedure TRzCustomGroupBox.DrawThemedCheckBox;
var
  X, Y: Integer;
  R: TRect;
  CheckBoxDetails: TThemedElementDetails;
begin
  Y := FCaptionRect.Top + ( ( FCaptionRect.Bottom - FCaptionRect.Top ) - FCheckBoxSize ) div 2;
  if not UseRightToLeftAlignment then
    X := FCaptionRect.Left
  else
    X := FCaptionRect.Right - FCheckBoxSize;

  R := Rect( X, Y, X + FCheckBoxSize, Y + FCheckBoxSize );


  if FChecked then
  begin
    if Enabled then
    begin
      if FShowDownVersion then
        CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedPressed )
      else if FMouseOverCheckBox then
        CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedHot )
      else
      CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedNormal );
    end
    else
    begin
      CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedDisabled );
    end;
  end
  else // Unchecked
  begin
    if Enabled then
    begin
      if FShowDownVersion then
        CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedPressed )
      else if FMouseOverCheckBox then
        CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedHot )
      else
      CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedNormal );
    end
    else
    begin
      CheckBoxDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedDisabled );
    end;
  end;

  ActiveStyleServices.DrawElement( Canvas.Handle, CheckBoxDetails, R );
end;


procedure TRzCustomGroupBox.DrawNonThemedCheckBox;
var
  X, Y, Flags: Integer;
  R: TRect;
begin
  Y := FCaptionRect.Top + ( ( FCaptionRect.Bottom - FCaptionRect.Top ) - FCheckBoxSize ) div 2;
  if not UseRightToLeftAlignment then
    X := FCaptionRect.Left
  else
    X := FCaptionRect.Right - FCheckBoxSize;

  R := Rect( X, Y, X + FCheckBoxSize, Y + FCheckBoxSize );

  if FChecked then
    Flags := dfcs_ButtonCheck or dfcs_Checked
  else
    Flags := dfcs_ButtonCheck;
  if FShowDownVersion then
    Flags := Flags or dfcs_Pushed;
  if not Enabled then
    Flags := Flags or dfcs_Inactive;

  DrawFrameControl( Canvas.Handle, R, dfc_Button, Flags );
end;


function TRzCustomGroupBox.AdjustCaptionRectForCheckBox: TRect;
begin
  Result := FCaptionRect;
  if not UseRightToLeftAlignment then
    Inc( Result.Left, FCheckBoxSize + 4 )
  else
    Dec( Result.Right, FCheckBoxSize + 4 );
end;


function TRzCustomGroupBox.ShowAccel: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEACCEL ) = 0;
end;


function TRzCustomGroupBox.ShowFocus: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEFOCUS ) = 0;
end;


procedure TRzCustomGroupBox.ChangeState;
begin
  SetChecked( not FChecked );
end;


procedure TRzCustomGroupBox.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if not FShowCheckBox then
    Exit;

  if ( Button = mbLeft ) and Enabled and FMouseOverCheckBox then
  begin
    // Cannot call SetFocus method b/c if the control is active, it will not change the focus back to this control.
    // This can happen if a dialog is displayed as a result of the clicking the button and the button is disabled.
    Windows.SetFocus( Handle );

    if Focused then
    begin
      FShowDownVersion := True;
      Invalidate;
      FDragging := True;
    end;
  end;

end;


procedure TRzCustomGroupBox.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  NewState, OldMouseOverCheckBox: Boolean;
begin
  inherited;

  if not FShowCheckBox then
    Exit;

  OldMouseOverCheckBox := FMouseOverCheckBox;
  FMouseOverCheckBox := PtInRect( FCaptionRect, Point( X, Y ) );
  if FMouseOverCheckBox <> OldMouseOverCheckBox then
    Invalidate;

  if FDragging then
  begin
    NewState := ( X >= 0 ) and ( X < ClientWidth ) and ( Y >= 0 ) and ( Y <= ClientHeight );

    if NewState <> FShowDownVersion then
    begin
      FShowDownVersion := NewState;
      Invalidate;
    end;
  end;
end;


procedure TRzCustomGroupBox.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  if not FShowCheckBox then
    Exit;

  if FDragging then
  begin
    FDragging := False;
    FShowDownVersion := False;
    if FMouseOverCheckBox then
      ChangeState;
    Invalidate;
  end;

end;


procedure TRzCustomGroupBox.WMSetFocus( var Msg: TWMSetFocus );
begin
  inherited;
  Invalidate;
end;


procedure TRzCustomGroupBox.WMKillFocus( var Msg: TWMKillFocus );
begin
  inherited;
  Invalidate;
end;


procedure TRzCustomGroupBox.CheckBoxClick;
begin
  if Assigned( FOnCheckBoxClick ) then
    FOnCheckBoxClick( Self );
end;


procedure TRzCustomGroupBox.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if not FShowCheckBox {or FReadOnly} then
    Exit;

  if Key = vk_Space then
  begin
    FKeyToggle := True;
    FShowDownVersion := True;
    Invalidate;
  end;
end;


procedure TRzCustomGroupBox.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if not FShowCheckBox {or FReadOnly} then
    Exit;

  if Key = vk_Space then
  begin
    FShowDownVersion := False;
    if FKeyToggle then
      ChangeState;
    Invalidate;
  end;
end;


procedure TRzCustomGroupBox.SetBannerHeight( Value: Integer );
begin
  if FBannerHeight <> Value then
  begin
    FBannerHeight := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzCustomGroupBox.SetGroupBoxStyle( Value: TRzGroupBoxStyle );
begin
  {&RV}
  if FGroupStyle <> Value then
  begin
    FGroupStyle := Value;
    Realign;
    Invalidate;
  end;
end;


function TRzCustomGroupBox.IsCaptionFontStored: Boolean;
begin
  Result := FCaptionFontChanged;
end;


procedure TRzCustomGroupBox.SetCaptionFont( Value: TFont );
begin
  FCaptionFont.Assign( Value );
end;


procedure TRzCustomGroupBox.CaptionFontChangeHandler( Sender: TObject );
begin
  FCaptionFontChanged := True;
  Invalidate;
end;


procedure TRzCustomGroupBox.SetChecked( Value: Boolean );
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    if FShowCheckBox and FEnableControlsOnCheck then
      EnableChildControls( FChecked );
    Invalidate;
    CheckBoxClick;
  end;
end;


procedure TRzCustomGroupBox.SetEnableControlsOnCheck( Value: Boolean );
begin
  if FEnableControlsOnCheck <> Value then
  begin
    FEnableControlsOnCheck := Value;
    if FEnableControlsOnCheck and FShowCheckBox then
      EnableChildControls( FChecked );
  end;
end;


procedure TRzCustomGroupBox.SetShowCheckBox( Value: Boolean );
begin
  if FShowCheckBox <> Value then
  begin
    FShowCheckBox := Value;
    TabStop := FShowCheckBox;
    if FShowCheckBox and FEnableControlsOnCheck and not FChecked then
      EnableChildControls( FChecked );
    Invalidate;
  end;
end;


{=======================}
{== TRzSpacer Methods ==}
{=======================}

constructor TRzSpacer.Create( AOwner: TComponent );
begin
  inherited;
  FGrooved := False;
  Width := 8;
  Height := 25;
  FOrientation := orHorizontal;
  {&RCI}
end;


procedure TRzSpacer.Paint;
var
  X1, Y1, X2, Y2: Integer;
  ElementDetails: TThemedElementDetails;
  R: TRect;
begin
  inherited;

  if ( csDesigning in ComponentState ) and not FGrooved then
  begin
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := clBlack;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle( ClientRect );
  end
  else if FGrooved then
  begin
    if ActiveStyleServicesEnabled then
    begin
      if Width > Height then
        ElementDetails := ActiveStyleServices.GetElementDetails( ttbSeparatorVertNormal )
      else
        ElementDetails := ActiveStyleServices.GetElementDetails( ttbSeparatorNormal );
      R := ClientRect;
      ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
    end
    else
    begin
      if Width > Height then
      begin
        X1 := ClientRect.Left + 1;
        Y1 := ClientRect.Top + ( ClientRect.Bottom - ClientRect.Top ) div 2 - 1;
        X2 := ClientRect.Right;
        Y2 := Y1;
      end
      else
      begin
        X1 := ClientRect.Left + ( ClientRect.Right - ClientRect.Left ) div 2 - 1;
        Y1 := ClientRect.Top + 1;
        X2 := X1;
        Y2 := ClientRect.Bottom;
      end;

      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo( X1, Y1 );
      Canvas.LineTo( X2, Y2 );

      if Width > Height then
      begin
        Inc( Y1 );
        Inc( Y2 );
      end
      else
      begin
        Inc( X1 );
        Inc( X2 );
      end;
      Canvas.Pen.Color := clBtnHighlight;
      Canvas.MoveTo( X1, Y1 );
      Canvas.LineTo( X2, Y2 );
    end;
  end;
end;


procedure TRzSpacer.SetGrooved( Value: Boolean );
begin
  {&RV}
  if FGrooved <> Value then
  begin
    FGrooved := Value;
    Invalidate;
  end;
end;


procedure TRzSpacer.SetOrientation( Value: TOrientation );
var
  W: Integer;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if not ( csLoading in ComponentState ) then
    begin
      { Swap Width and Height }
      W := Width;
      Width := Height;
      Height := W;
      Invalidate;
    end;
  end;
end;


{===============================}
{== TRzToolbarControl Methods ==}
{===============================}

{===============================================================================
  TRzToolbarControl.AssignElements

  Descendant classes of TRzToolbarControl that implement additional properties
  must override this method, calling inherited and then transfering any
  additional properties from referenceToolbarControl to Self.
===============================================================================}

procedure TRzToolbarControl.AssignElements( Control: TRzToolbarControl );
begin
  { Do nothing. Let descendants of TRzToolbarControl implement functionality. }
end;



{===================================}
{== TRzToolbarControlList Methods ==}
{===================================}

constructor TRzToolbarControlList.Create( AToolbar: TRzToolbar );
begin
  inherited Create;
  FToolbar := AToolbar;
end;


destructor TRzToolbarControlList.Destroy;
var
  I: Integer;
begin
  if Assigned( FTempControlList ) then
  begin
    FTempControlList.Free;
    FTempControlList := nil;
  end;

  { Free all TRzToolbarControls managed by the list }
  for I := 0 to Count - 1 do
    Items[ I ].Free;

  inherited;
end;


function TRzToolbarControlList.Get( Index: Integer ): TRzToolbarControl;
begin
  Result := TRzToolbarControl( inherited Get( Index ) );
end;

procedure TRzToolbarControlList.Put( Index: Integer; Value: TRzToolbarControl );
begin
  inherited Put( Index, Value );
end;


{===============================================================================
  TRzToolbarControlList.ReadControl

  Descendant classes can override and use FIndexOfLastControlRead to access the
  Control list, adding the necessary elements as they are streamed in.
===============================================================================}

procedure TRzToolbarControlList.ReadControl( Reader: TReader );
begin
  FIndexOfLastControlRead := FTempControlList.AddControlName( Reader.ReadIdent );
end;

procedure TRzToolbarControlList.ReadControls( Reader: TReader );
var
  I: Integer;
begin
  if not Assigned( FTempControlList ) then
    FTempControlList := TRzToolbarControlList.Create( FToolbar );

  for I := 0 to FTempControlList.Count - 1 do
    FTempControlList.Items[ I ].Free;
  FTempControlList.Clear;

  Reader.ReadListBegin;
  while not Reader.EndOfList do
    ReadControl( Reader );
  Reader.ReadListEnd;
end;


procedure TRzToolbarControlList.WriteControl( Index: Integer; Writer: TWriter );
begin
  Writer.WriteIdent( Items[ Index ].Control.Name );
end;

procedure TRzToolbarControlList.WriteControls( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count - 1 do
    WriteControl( I, Writer );
  Writer.WriteListEnd;
end;



procedure TRzToolbarControlList.ControlsAreLoaded( OwnerComp: TComponent );
var
  I: Integer;
  C: TComponent;
  Idx: Integer;
  FInACopy: Boolean;
begin
  if FTempControlList <> nil then
  begin
    FInACopy := False;
    for I := 0 to FTempControlList.Count - 1 do
    begin
      C := OwnerComp.FindComponent( FTempControlList.Items[ I ].ControlName );
      if ( C <> nil ) and ( C is TControl ) then
      begin
        if TControl( C ).Parent = FToolbar then
        begin
          Idx := AddControl( TControl( C ) );
          if Idx <> -1 then
            Items[ Idx ].AssignElements( FTempControlList.Items[ I ] );
        end
        else
        begin
          { Control is not parented by the Toolbar. This will occur
            if the toolbar was copied to the clipboard and pasted into
            the same form.  Therefore, remove the control from the list.}
          RemoveControlName( FTempControlList.Items[ I ].ControlName );
          FInACopy := True;
        end;
      end;
    end;

    if FInACopy then
    begin
      if FToolbar <> nil then
      begin
        for I := 0 to FToolbar.ControlCount - 1 do
          AddControl( FToolbar.Controls[ I ] );
      end;
    end;


    FTempControlList.Free;
    FTempControlList := nil;
  end
  else
  begin
    {===========================================================================
      If FTempControlList is empty, then the form file being loaded does not
      have a ToolbarControls property specfied. This will occur if the
      toolbar has no controls on it, or if the form was built using version
      1.01 or earlier of TRzToolbar.  In either case, iterate through all
      controls on the toolbar and add them to the list.
    ===========================================================================}
    if FToolbar <> nil then
    begin
      for I := 0 to FToolbar.ControlCount - 1 do
        AddControl( FToolbar.Controls[ I ] );
    end;
  end;
end;


function TRzToolbarControlList.IndexOfName( const ControlName: string ): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if CompareText( Items[ I ].ControlName, ControlName ) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;


function TRzToolbarControlList.IndexOf( Control: TControl ): Integer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if Items[ I ].Control = Control then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;


function TRzToolBarControlList.CreateToolBarControl: TRzToolBarControl;
begin
  Result := TRzToolBarControl.Create;
end;


{===============================================================================
  TRzToolbarControlList.AddControl

  Adds someControl to the list, if it's not already in the list.
  Returns the index to the new item.
===============================================================================}

function TRzToolbarControlList.AddControl( Control: TControl ): Integer;
var
  Idx: Integer;
  Item: TRzToolbarControl;
begin
  Idx := IndexOf( Control );
  if Idx = -1 then
  begin
    Item := CreateToolbarControl;
    Item.FControl := Control;
    Item.FControlName := Control.Name;
    Result := inherited Add( Item );
  end
  else
    Result := Idx;
end;


function TRzToolbarControlList.AddControlName( const ControlName: string ): Integer;
var
  Idx: Integer;
  Item: TRzToolbarControl;
begin
  Idx := IndexOfName( ControlName );
  if Idx = -1 then
  begin
    Item := CreateToolbarControl;
    Item.FControlName := ControlName;
    Result := inherited Add( Item );
  end
  else
    Result := Idx;
end;


{===============================================================================
  TRzToolbarControlList.RemoveControl

  Returns old index of component that was removed.
  Returns -1 if component is not in list.
===============================================================================}

function TRzToolbarControlList.RemoveControl( Control: TControl ): Integer;
var
  Idx: Integer;
  Item: TRzToolbarControl;
begin
  Idx := IndexOf( Control );
  Result := Idx;
  if Idx = -1 then
  begin
    Exit;
  end
  else
  begin
    Item := Items[ Idx ];
    Result := inherited Remove( Item );
    Item.Free;
  end;
end;


function TRzToolbarControlList.RemoveControlName( const ControlName: string ): Integer;
var
  Idx: Integer;
  Item: TRzToolbarControl;
begin
  Idx := IndexOfName( ControlName );
  Result := Idx;
  if Idx = -1 then
  begin
    Exit;
  end
  else
  begin
    Item := Items[ Idx ];
    Result := inherited Remove( Item );
    Item.Free;
  end;
end;



{==================================}
{== TRzCustomizeCaptions Methods ==}
{==================================}

constructor TRzCustomizeCaptions.Create;
begin
  inherited Create;
  FStoreCaptions := False;

  FTitle := sRzCustomizeTitle;
  FHint:= sRzHint;
  FClose := sRzClose;
  FMoveUp := sRzMoveUp;
  FMoveDown := sRzMoveDown;
  FTextOptions := sRzTextOptions;
  FNoTextLabels := sRzNoTextLabels;
  FShowTextLabels := sRzShowTextLabels;
  FSelectiveTextOnRight := sRzSelectiveTextOnRight;
end;


function TRzCustomizeCaptions.GetCaption( Index: Integer ): string;
begin
  case Index of
    1: Result := FTitle;
    2: Result := FHint;
    3: Result := FClose;
    4: Result := FMoveUp;
    5: Result := FMoveDown;
    6: Result := FTextOptions;
    7: Result := FNoTextLabels;
    8: Result := FShowTextLabels;
    9: Result := FSelectiveTextOnRight;
  end;
end;


procedure TRzCustomizeCaptions.SetCaption( Index: Integer; const Value: string );
begin
  case Index of
    1: FTitle := Value;
    2: FHint := Value;
    3: FClose := Value;
    4: FMoveUp := Value;
    5: FMoveDown := Value;
    6: FTextOptions := Value;
    7: FNoTextLabels := Value;
    8: FShowTextLabels := Value;
    9: FSelectiveTextOnRight := Value;
  end;
  FStoreCaptions := True;
end;


type
  TRzPopupToolButton = class( TRzToolButton )
  private
    FToolbar: TRzToolbar;
    FMappedButton: TRzToolButton;
    FToolbarPopupButton: TRzToolbarPopupButton;
  protected
    procedure Assign( Source: TPersistent ); override;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure Click; override;
    procedure SetToolbarPopupButton( Value: TRzToolbarPopupButton );
  end;

constructor TRzPopupToolButton.Create( AOwner: TComponent );
begin
  inherited;
  FToolbar := AOwner as TRzToolbar;
end;


procedure TRzPopupToolButton.Assign( Source: TPersistent );
begin
  if Source is TRzToolButton then
  begin
    FMappedButton := TRzToolButton( Source );

    Alignment := FMappedButton.Alignment;
    AllowAllUp := FMappedButton.AllowAllUp;
    BiDiMode := FMappedButton.BiDiMode;
    Caption := FMappedButton.Caption;
    Color := FMappedButton.Color;
    DisabledIndex := FMappedButton.DisabledIndex;
    Down := FMappedButton.Down;
    DownIndex := FMappedButton.DownIndex;
    DropDownMenu := FMappedButton.DropDownMenu;
    Enabled := FMappedButton.Enabled;
    Flat := FMappedButton.Flat;
    Font := FMappedButton.Font;
    GradientColorStyle := FMappedButton.GradientColorStyle;
    GroupIndex := FMappedButton.GroupIndex;
    Height := FMappedButton.Height;
    Hint := FMappedButton.Hint;
    HotIndex := FMappedButton.HotIndex;
    ImageIndex := FMappedButton.ImageIndex;
    Images := FMappedButton.Images;
    DisabledImages := FMappedButton.DisabledImages;
    Layout := FMappedButton.Layout;
    ParentBiDiMode := FMappedButton.ParentBiDiMode;
    ParentFont := FMappedButton.ParentFont;
    ParentShowHint := FMappedButton.ParentShowHint;
    SelectionColorStart := FMappedButton.SelectionColorStart;
    SelectionColorStop := FMappedButton.SelectionColorStop;
    SelectionFrameColor := FMappedButton.SelectionFrameColor;
    ShowCaption := FMappedButton.ShowCaption;
    ShowHint := FMappedButton.ShowHint;
    Tag := FMappedButton.Tag;
    ToolStyle := FMappedButton.ToolStyle;
    Transparent := FMappedButton.Transparent;
    UseToolbarButtonLayout := FMappedButton.UseToolbarButtonLayout;
    UseToolbarButtonSize := FMappedButton.UseToolbarButtonSize;
    UseToolbarShowCaption := FMappedButton.UseToolbarShowCaption;
    UseToolbarVisualStyle := FMappedButton.UseToolbarVisualStyle;
    Visible := FMappedButton.Visible;
    VisualStyle := FMappedButton.VisualStyle;
    Width := FMappedButton.Width;
  end
  else
    inherited;
end;


procedure TRzPopupToolButton.SetToolbarPopupButton( Value: TRzToolbarPopupButton );
begin
  FToolbarPopupButton := Value;
end;


procedure TRzPopupToolButton.Click;
begin
  FToolbarPopupButton.FSelectedButton := FMappedButton;
  FToolbar.Click;
//  inherited;
end;



{===================================}
{== TRzToolbarPopupButton Methods ==}
{===================================}

constructor TRzToolbarPopupButton.Create( AOwner: TComponent );
begin
  inherited;
  FToolbar := TRzToolbar( AOwner );
  FFrameColor := clBtnShadow;
  FMenu := TPopupMenu.Create( Self );
end;


destructor TRzToolbarPopupButton.Destroy;
begin
  FMenu.Free;
  inherited;
end;


procedure TRzToolbarPopupButton.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
begin
  // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from
  // erasing background. Set Msg.Result to 1 to indicate background is painted
  // by the control.
  Msg.Result := 1;
end;


procedure TRzToolbarPopupButton.DrawBackground( Bounds: TRect; Down: Boolean );
var
  R: TRect;
  StartColor, StopColor: TColor;
  ElementDetails: TThemedElementDetails;

  procedure GetGradientColors( var StartColor, StopColor: TColor );
  begin
    if FToolbar.GradientColorStyle <> gcsCustom then
    begin
      GetGradientPanelColors( FToolbar.GradientColorStyle, StartColor, StopColor );
    end
    else
    begin
      StartColor := FToolbar.GradientColorStart;
      StopColor := FToolbar.GradientColorStop;
    end;
  end;

begin {= TRzToolbarPopupButton.DrawBackground =}
  R := Bounds;

  case FToolbar.VisualStyle of
    vsClassic, vsWinXP:
    begin
      if ( FToolbar.VisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
      begin
        ElementDetails := ActiveStyleServices.GetElementDetails( trRebarRoot );
        ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );

        if Down then
          ElementDetails := ActiveStyleServices.GetElementDetails( trChevronPressed )
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( trChevronNormal );
        ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
      end
      else
      begin
        R := DrawBox( Canvas, R, FrameColor );
        if Down then
          Canvas.Brush.Color := DarkerColor( FToolbar.Color, 20 )
        else
          Canvas.Brush.Color := FToolbar.Color;
        Canvas.FillRect( R );
      end;
    end;

    vsGradient:
    begin
      R := DrawBox( Canvas, R, FrameColor );

      GetGradientColors( StartColor, StopColor );
      if Down then
        PaintGradient( Canvas, R, FToolbar.GradientDirection, StopColor, StartColor )
      else
        PaintGradient( Canvas, R, FToolbar.GradientDirection, StartColor, StopColor );
    end;
  end;
end; {= TRzToolbarPopupButton.DrawBackground =}


procedure TRzToolbarPopupButton.DrawChevron( Bounds: TRect );
var
  X, Y: Integer;
  C, OldBrushColor: TColor;
  ElementDetails: TThemedElementDetails;
  R: TRect;
  TempBmp: TBitmap;
begin
  OldBrushColor := Canvas.Brush.Color;
  if ActiveStyleServicesEnabled then
  begin
    ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonNormal );
    TempBmp := TBitmap.Create;
    try
      R := Rect( 0, 0, 16, 20 );
      TempBmp.Width := 17;
      TempBmp.Height := 21;
      ActiveStyleServices.DrawElement( TempBmp.Canvas.Handle, ElementDetails, R );
      C := TempBmp.Canvas.Pixels[ 8, 10 ];
      if ColorsTooClose( C, clWindow ) then
        C := cl3DDkShadow; // This is needed for Olive Windows XP color scheme
    finally
      TempBmp.Free;
    end;
    Canvas.Brush.Color := C;
  end
  else
    Canvas.Brush.Color := clBlack;

  Canvas.Pen.Style := psClear;
  X := Bounds.Left + ( Bounds.Right - Bounds.Left ) div 2;
  Y := Bounds.Bottom - 4;

  if FDown then
    Inc( Y );

  if ActiveStyleServicesEnabled then
  begin
    R := Rect( X - 3, Y - 6, X + 4, Y - 5 );
    Canvas.FillRect( R );
    Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 4 ), Point( X, Y - 4 ), Point( X, Y - 3 ),
                      Point( X + 1, Y - 4 ), Point( X + 4, Y - 4 ) ] );
  end
  else
  begin
    R := Rect( X - 3, Y - 5, X + 4, Y - 4 );
    Canvas.FillRect( R );

    Canvas.Pixels[ X + 0, Y + 0 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X - 1, Y - 1 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 0, Y - 1 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 1, Y - 1 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X - 2, Y - 2 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X - 1, Y - 2 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 0, Y - 2 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 1, Y - 2 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 2, Y - 2 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X - 3, Y - 3 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X - 2, Y - 3 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X - 1, Y - 3 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 0, Y - 3 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 1, Y - 3 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 2, Y - 3 ] := Canvas.Brush.Color;
    Canvas.Pixels[ X + 3, Y - 3 ] := Canvas.Brush.Color;
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := OldBrushColor;
end;


procedure TRzToolbarPopupButton.Paint;
var
  R: TRect;
begin
  R := ClientRect;

  DrawBackground( R, FDown );

  if not ( ( FToolbar.VisualStyle = vsWinXP ) and ActiveStyleServicesEnabled ) then
    DrawChevron( R );
end; {= TRzToolbarPopupButton.Paint =}


procedure TRzToolbarPopupButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  FPressed := True;
  FDown := True;
  Invalidate;
  SetCapture( Handle );
end;


procedure TRzToolbarPopupButton.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  P: TPoint;
begin
  inherited;

  if FPressed then
  begin
    P := Point( X, Y );

    if PtInRect( ClientRect, P ) <> FDown then
    begin
      FDown := not FDown;
      Invalidate;
    end;
  end;
end;


procedure TRzToolbarPopupButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  ReleaseCapture;
  FPressed := False;

  if FDown then
  begin
    FDown := False;
    DisplayButtons;
    Invalidate;
  end;
end;



procedure TRzToolbarPopupButton.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzToolbarPopupButton.DisplayButtons;
var
  PopupPanel: TRzPopupPanel;
  Toolbar: TRzToolbar;
  F: TCustomForm;
  I: Integer;
  Control: TControl;
  Btn: TRzPopupToolButton;
begin
  F := GetParentForm( Self );
  if F <> nil then
  begin
    PopupPanel := TRzPopupPanel.Create( Self );
    try
      Toolbar := TRzToolbar.Create( PopupPanel );
      Toolbar.Parent := PopupPanel;
      Toolbar.BorderOuter := fsFlat;
      Toolbar.BorderSides := sdAllSides;
      Toolbar.Align := alNone;
      Toolbar.Width := FToolbar.ButtonWidth * 3 + 8;

      Toolbar.Background := FToolbar.Background;
      Toolbar.BiDiMode := FToolbar.BiDiMode;
      Toolbar.ButtonHeight := FToolbar.ButtonHeight;
      Toolbar.ButtonLayout := FToolbar.ButtonLayout;
      Toolbar.ButtonWidth := FToolbar.ButtonWidth;
      if FToolbar.VisualStyle <> vsGradient then
        Toolbar.Color := FToolbar.Color
      else
        Toolbar.Color := clWindow;
      Toolbar.GradientColorStart := FToolbar.GradientColorStart;
      Toolbar.GradientColorStop := FToolbar.GradientColorStop;
      Toolbar.GradientColorStyle := FToolbar.GradientColorStyle;
      Toolbar.GradientDirection := FToolbar.GradientDirection;
      Toolbar.Images := FToolbar.Images;
      Toolbar.DisabledImages := FToolbar.DisabledImages;
      Toolbar.RowHeight := FToolbar.RowHeight;
      Toolbar.ShowButtonCaptions := FToolbar.ShowButtonCaptions;
      Toolbar.ShowHint := FToolbar.ShowHint;
      Toolbar.TextOptions := FToolbar.TextOptions;

      Toolbar.VisualStyle := FToolbar.VisualStyle;

      // Add buttons for clipped buttons
      for I := 0 to FToolbar.ToolbarControls.Count - 1 do
      begin
        Control := FToolbar.ToolbarControls[ I ].Control;
        if ( Control is TRzToolButton ) and ( Control.Visible ) and
           ( Control.Left + Control.Width > FToolbar.Width - FToolbar.Margin ) then
        begin
          Btn := TRzPopupToolButton.Create( Toolbar );
          Btn.SetToolbarPopupButton( Self );

          Btn.Assign( TRzToolButton( Control ) );
          Btn.OnClick := TRzToolButton( Control ).OnClick;

          Btn.Parent := Toolbar;
          Toolbar.ToolbarControls.AddControl( Btn );
        end;
      end;

      PopupPanel.Parent := F;
      PopupPanel.Font.Name := Font.Name;
      PopupPanel.Font.Color := Font.Color;

      Toolbar.Handle;
      Toolbar.Visible := True;
      Toolbar.OnClick := PopupPanel.Close;

      FSelectedButton := nil;
      PopupPanel.Popup( Self );
      if FSelectedButton <> nil then
        FSelectedButton.Click;
    finally
      PopupPanel.Free;
    end;
  end;
end; {= TRzToolbarPopupButton.DisplayButtons =}





{========================}
{== TRzToolbar Methods ==}
{========================}

constructor TRzToolbar.Create( AOwner: TComponent );
begin
  inherited;

  FShowDivider := True;
  FBorderInner := fsNone;
  FBorderOuter := fsGroove;
  FBorderSides := [ sdTop ];

  BorderWidth := 0;
  Align := alTop;
  Height := 32;
  Width := 32;
  ShowDockClientCaptions := False;

  FullRepaint := False;
  {&RCI}
  FToolbarControls := CreateToolbarControlList;
  FRowHeight := 25;
  FMargin := 4;
  FTopMargin := 2;
  FAutoResize := True;
  FAutoStyle := True;
  FWrapControls := True;
  FOrientation := orHorizontal;
  FBackground := TBitmap.Create;
  FBackground.OnChange := BackgroundChangedHandler;

  FShowButtonCaptions := False;
  FButtonLayout := blGlyphLeft;
  FButtonWidth := 25;
  FButtonHeight := 25;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;

  FDisabledImagesChangeLink := TChangeLink.Create;
  FDisabledImagesChangeLink.OnChange := DisabledImagesChange;

  FTextOptions := ttoNoTextLabels;
  FCustomizeCaptions := TRzCustomizeCaptions.Create;
end; {= TRzToolbar.Create =}


procedure TRzToolbar.CreateToolbarPopupButton;
begin
  FToolbarPopupButton := TRzToolbarPopupButton.Create( Self );
  FToolbarPopupButton.Parent := Self;
  FToolbarPopupButton.ControlStyle := FToolbarPopupButton.ControlStyle + [ csNoDesignVisible ];
  FToolbarPopupButton.FrameColor := AdjustColor( FFlatColor, FFlatColorAdjustment );
end;


destructor TRzToolbar.Destroy;
begin
  FBackground.Free;
  FToolbarControls.Free;
  FImagesChangeLink.Free;
  FDisabledImagesChangeLink.Free;
  FCustomizeCaptions.Free;
  inherited;
end;


procedure TRzToolbar.SetParent( AParent: TWinControl );
begin
  inherited;

  // The TPageScroller automatically resizes whatever is dropped onto it.
  //  This will cause an infinit loop if the AutoResize property is True.
  CheckAutoResize( FAutoResize );
end;


{===============================================================================
  TRzToolbar.CreateToolbarControlList

  Descendant classes that need to also use a descendant of
  TRzToolbarControlList can override this and return custom class.
  (IMPORTANT: don't call inherited when overriding this proc)
===============================================================================}

function TRzToolbar.CreateToolbarControlList: TRzToolbarControlList;
begin
  Result := TRzToolbarControlList.Create( Self );
end;


procedure TRzToolbar.Loaded;
begin
  inherited;

  if Owner <> nil then
    FToolbarControls.ControlsAreLoaded( Owner );

  PositionControls;
  if FAutoStyle then
    AdjustStyle( Align );
  {&RV}
end;


procedure TRzToolbar.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineProperty( 'ToolbarControls', FToolbarControls.ReadControls, FToolbarControls.WriteControls,
                        FToolbarControls.Count > 0 );
end;


procedure TRzToolbar.AdjustStyle( Value: TAlign );
begin
  if Value in [ alLeft, alRight ] then
  begin
    FBorderInner := fsNone;
    if FShowDivider then
      FBorderOuter := fsGroove
    else
      FBorderOuter := fsNone;
    if Value = alLeft then
      FBorderSides := [ sdLeft ]
    else
      FBorderSides := [ sdRight ];
  end
  else
  begin
    FBorderInner := fsNone;
    FBorderOuter := fsGroove;

    if FShowDivider then
      FBorderOuter := fsGroove
    else
      FBorderOuter := fsNone;
    if Value = alBottom then
      FBorderSides := [ sdBottom ]
    else
      FBorderSides := [ sdTop ];
  end;
  Invalidate;
end;


function TRzToolbar.CanAutoSize( var NewWidth, NewHeight: Integer ): Boolean;
begin
  Result := inherited CanAutoSize( NewWidth, NewHeight );
  if Result then
    NewWidth := NewWidth + FMargin;
end;


procedure TRzToolbar.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if not ( csDestroying in ComponentState ) then
  begin
    if ( Operation = opRemove ) and ( AComponent is TControl ) and
       ( FToolbarControls <> nil ) and ( AComponent <> Self ) and
       ( FToolbarControls.RemoveControl( TControl( AComponent ) ) <> -1 ) then
    begin
      PositionControls;
    end;
  end;

  if ( Operation = opRemove ) and ( AComponent = FImages ) then
    SetImages( nil )  // Call access method so connections to link object can be cleared
  else if ( Operation = opRemove ) and ( AComponent = FDisabledImages ) then
    SetDisabledImages( nil )
  else if ( Operation = opRemove ) and ( AComponent = FRegIniFile ) then
    FRegIniFile := nil;
end;


procedure TRzToolbar.AlignToolbarPopupButton;
var
  L, T, W, H: Integer;
begin
  if not ( ( csLoading in ComponentState ) or ( csReading in ComponentState ) ) then
  begin
    if not FShowToolbarPopupButton or FWrapControls or ( FOrientation = orVertical ) then
      Exit;

    L := Width - ToolbarPopupButtonWidth;
    T := 0;
    H := Height;
    W := ToolbarPopupButtonWidth;

    FToolbarPopupButton.SetBounds( L, T, W, H );
    FToolbarPopupButton.Visible := True;
  end;
end; {= TRzToolbar.AlignToolbarPopupButton =}


procedure TRzToolbar.AlignControls( AControl: TControl; var Rect: TRect );
begin
  if not ( csLoading in ComponentState ) then
  begin
    if ( AControl <> nil ) and ( AControl.Parent = Self ) and
       ( AControl <> FToolbarPopupButton ) then
    begin
      FToolbarControls.AddControl( AControl );
    end;
    PositionControls;
  end;
  AlignToolbarPopupButton;
end;


procedure TRzToolbar.ChangeScale( M, D: Integer );
begin
  inherited;
  RowHeight := MulDiv( FRowHeight, M, D );
end;



procedure TRzToolbar.PositionControl( Index, Row: Integer; var Offset: Integer );
var
  Control: TControl;
  YOffset, XOffset: Integer;
begin
  Control := FToolbarControls.Items[ Index ].Control;
  if not Assigned( Control ) or ( csDestroying in Control.ComponentState ) then
    Exit;

  with Control do
  begin
    if FOrientation = orHorizontal then
    begin
      if AlignWithMargins then
        Offset := Offset + Margins.Left;

      YOffset := FTopMargin + FCalculatedRowHeight * ( Row - 1 ) + ( FRowHeight div 2 ) - ( Height div 2 );
      if not UseRightToLeftAlignment then
        SetBounds( Offset, YOffset, Width, Height )
      else
        SetBounds( Self.Width - Width - Offset, YOffset, Width, Height );
      Offset := Offset + Width;

      if AlignWithMargins then
        Offset := Offset + Margins.Right;
    end
    else
    begin
      if AlignWithMargins then
        Offset := Offset + Margins.Top;

      XOffset := FMargin + FCalculatedRowHeight * ( Row - 1 ) +
                 ( FRowHeight div 2 ) - ( Width div 2 );
      SetBounds( XOffset, Offset, Width, Height );
      Offset := Offset + Height;

      if AlignWithMargins then
        Offset := Offset + Margins.Bottom;
    end;
  end;
end; {= TRzToolbar.PositionControl =}


function TRzToolbar.CalculateRowHeight( Row: Integer): Integer;
begin
  Result := FRowHeight;
end;


function TRzToolbar.InvalidateMarginSize: TPoint;
begin
  if ( FOrientation = orHorizontal ) and ( not FWrapControls ) and FShowToolbarPopupButton then
  begin
    Result := Point( ToolbarPopupButtonWidth + 1, 1 );
  end
  else
    Result := inherited InvalidateMarginSize;
end;


procedure TRzToolbar.PositionControls;
var
  I, Row, Col, MaxWidth, MaxHeight, XOffset, YOffset: Integer;
  Control: TControl;

  procedure ResizeParent;
  var
    State: Integer;
  begin
    if ( Parent <> nil ) and ( Parent.Visible ) and not ( Parent is TControlBar ) then
    begin
      if IsZoomed( Parent.Handle ) then
        State := SIZE_MAXIMIZED
      else if IsIconic( Parent.Handle ) then
        State := SIZE_MINIMIZED
      else
        State := SIZE_RESTORED;

      PostMessage( Parent.Handle, wm_Size, State,
                   LPARAM( MakeLong( Parent.Width, Parent.Height ) ) );
    end;
  end;

begin
  if FOrientation = orHorizontal then
  begin
    if ( not FWrapControls ) and ( FToolbarControls.Count > 0 ) then
    begin
      I := FToolbarControls.Count - 1;
      Control := FToolbarControls.Items[ I ].Control;
      while not Control.Visible and ( I > 0 ) do
      begin
        Dec( I );
        Control := FToolbarControls.Items[ I ].Control;
      end;
      if Control.Visible and not AutoSize then
        SetShowToolbarPopupButton( Control.Left + Control.Width > Width - FMargin )
      else
        SetShowToolbarPopupButton( False );
    end;

    XOffset := FMargin;
    MaxWidth := 0;
    Row := 1;
    FCalculatedRowHeight := CalculateRowHeight( Row );

    for I := 0 to FToolbarControls.Count - 1 do
    begin
      Control := FToolbarControls.Items[ I ].Control;
      if Control.Visible or ( csDesigning in ComponentState ) then
      begin
        if ( I > 0 ) and FWrapControls and
           ( XOffset + Control.Width > Width - FMargin ) then
        begin
          // Wrap to next line
          XOffset := FMargin;
          Inc( Row );
          FCalculatedRowHeight := CalculateRowHeight( Row );
        end;

        PositionControl( I, Row, XOffset );

        if XOffset > MaxWidth then
          MaxWidth := XOffset;
      end;
    end; { for }


    if FAutoResize then
    begin
      if ( Align in [ alLeft, alRight ] ) and ( MaxWidth <> 0 ) then
      begin
        if Width <> MaxWidth + FMargin then
          Invalidate;
        Width := MaxWidth + FMargin;
        ResizeParent;
      end
      else if Align in [ alNone, alTop, alBottom ] then
      begin
        if Height <> Row * FRowHeight + 2 * FTopMargin then
          Invalidate;
        Height := Row * FRowHeight + 2 * FTopMargin;
        ResizeParent;
      end;
    end;
  end
  else { FOrientation = orVertical }
  begin
    YOffset := FMargin;
    MaxHeight := 0;
    Col := 1;
    FCalculatedRowHeight := CalculateRowHeight( Col );
    for I := 0 to FToolbarControls.Count - 1 do
    begin
      Control := FToolbarControls.Items[ I ].Control;
      if Control.Visible or ( csDesigning in ComponentState ) then
      begin
        if FWrapControls and ( I > 0 ) and
           ( YOffset + Control.Height > Height - FMargin ) then
        begin
          // Wrap to next line
          YOffset := FMargin;
          Inc( Col );
          FCalculatedRowHeight := CalculateRowHeight( Col );
        end;

        PositionControl( I, Col, YOffset );

        if YOffset > MaxHeight then
          MaxHeight := YOffset;
      end;
    end; { for }

    if FAutoResize then
    begin
      if Align in [ alLeft, alRight ] then
      begin
        if Width <> Col * FRowHeight + 2 + FTopMargin then
          Invalidate;
        Width := Col * FRowHeight + 2 + FTopMargin;
        ResizeParent;
      end
      else if ( Align in [ alNone, alTop, alBottom ] ) and ( MaxHeight <> 0 ) then
      begin
        if Height <> MaxHeight + FMargin then
          Invalidate;
        Height := MaxHeight + FMargin;
        ResizeParent;
      end;
    end;

  end;
end; {= TRzToolbar.PositionControls =}



procedure TRzToolbar.BackgroundChangedHandler( Sender: TObject );
begin
  Invalidate;
end;


function TRzToolbar.GetAlign: TAlign;
begin
  Result := inherited Align;
end;


procedure TRzToolbar.SetAlign( Value: TAlign );
begin
  if FAutoStyle then
    AdjustStyle( Value );
  inherited Align := Value;

  if FAutoResize then
  begin
    if Value in [ alLeft, alRight ] then
      Width := 32
    else
      Height := 32;
  end;
end; {= TRzToolbar.SetAlign =}


procedure TRzToolbar.SetImages( Value: TCustomImageList );
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


procedure TRzToolbar.ImagesChange( Sender: TObject );
begin
  if Sender = Images then
    Update;         // Call Update instead of Invalidate to prevent flicker
end;


procedure TRzToolbar.SetDisabledImages( Value: TCustomImageList );
begin
  if FDisabledImages <> nil then
    FDisabledImages.UnRegisterChanges( FDisabledImagesChangeLink );

  FDisabledImages := Value;

  if FDisabledImages <> nil then
  begin
    FDisabledImages.RegisterChanges( FDisabledImagesChangeLink );
    FDisabledImages.FreeNotification( Self );
  end;
  Invalidate;
end;


procedure TRzToolbar.DisabledImagesChange( Sender: TObject );
begin
  if Sender = DisabledImages then
    Update;         // Call Update instead of Invalidate to prevent flicker
end;


procedure TRzToolbar.SetButtonLayout( Value: TButtonLayout );
begin
  if FButtonLayout <> Value then
  begin
    FButtonLayout := Value;
    NotifyControls( cm_ToolbarButtonLayoutChanged );
    if not FUpdatingTextOptions then
      FTextOptions := ttoCustom;
  end;
end;


procedure TRzToolbar.SetButtonWidth( Value: Integer );
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    NotifyControls( cm_ToolbarButtonSizeChanged );
    if not FUpdatingTextOptions then
      FTextOptions := ttoCustom;
  end;
end;


procedure TRzToolbar.SetButtonHeight( Value: Integer );
begin
  if FButtonHeight <> Value then
  begin
    FButtonHeight := Value;
    NotifyControls( cm_ToolbarButtonSizeChanged );
    if not FUpdatingTextOptions then
      FTextOptions := ttoCustom;
  end;
end;


procedure TRzToolbar.SetShowButtonCaptions( Value: Boolean );
begin
  if FShowButtonCaptions <> Value then
  begin
    FShowButtonCaptions := Value;
    NotifyControls( cm_ToolbarShowCaptionChanged );
    if not FUpdatingTextOptions then
      FTextOptions := ttoCustom;
  end;
end;


procedure TRzToolbar.SetTextOptions( Value: TRzToolbarTextOptions );
var
  W, H: Integer;
  ShowCaptions: Boolean;
  Layout: TButtonLayout;
begin
  if FTextOptions <> Value then
  begin
    FUpdatingTextOptions := True;
    try
      FTextOptions := Value;

      case FTextOptions of
        ttoShowTextLabels:
        begin
          W := 60;
          H := 40;
          ShowCaptions := True;
          Layout := blGlyphTop;
        end;

        ttoSelectiveTextOnRight:
        begin
          W := 60;
          H := 25;
          ShowCaptions := True;
          Layout := blGlyphLeft;
        end;

        else // ttNoTextLabels
        begin
          W := 25;
          H := 25;
          ShowCaptions := False;
          Layout := blGlyphLeft;
        end;
      end;

      if FTextOptions <> ttoCustom then
      begin
        SetButtonLayout( Layout );
        UpdateButtonSize( W, H, ShowCaptions );

        if Orientation = orHorizontal then
          SetRowHeight( H )
        else
          SetRowHeight( W );
      end;
    finally
      FUpdatingTextOptions := False;
    end;
  end;
end; {= TRzToolbar.SetTextOptions =}


procedure TRzToolbar.SetCustomizeCaptions( Value: TRzCustomizeCaptions );
begin
  FCustomizeCaptions.Assign( Value );
end;


procedure TRzToolbar.SetRegIniFile( Value: TRzRegIniFile );
begin
  if FRegIniFile <> Value then
  begin
    FRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzToolbar.SetShowDivider( Value: Boolean );
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    if FShowDivider then
      FBorderOuter := fsGroove
    else
      FBorderOuter := fsNone;
    Invalidate;
  end;
end;


procedure TRzToolbar.CheckAutoResize( var Value: Boolean );
begin
  { We use ClassNameIs to avoid linking in the ComCtrls unit. }
  if Value and ( Parent <> nil ) then
  begin
    if Parent.ClassNameIs( 'TPageScroller' ) or
       Parent.ClassNameIs( 'TToolbar' ) then
    begin
      Value := False;
    end;
  end;
end;



procedure TRzToolbar.SetAutoResize( Value: Boolean );
begin
  CheckAutoResize( Value );
  if FAutoResize <> Value then
  begin
    FAutoResize := Value;
    PositionControls;
  end;
end;


procedure TRzToolbar.SetBackground( Value: TBitmap );
begin
  FBackground.Assign( Value );
end;


procedure TRzToolbar.UpdateButtonSize( NewWidth, NewHeight: Integer; ShowCaptions: Boolean );
var
  I: Integer;
  Btn: TRzToolbarButton;
begin
  ShowButtonCaptions := ShowCaptions;
  ButtonWidth := NewWidth; 
  ButtonHeight := NewHeight;

  for I := 0 to ControlCount - 1 do
  begin
    if Controls[ I ] is TRzToolbarButton then
    begin
      Btn := TRzToolbarButton( Controls[ I ] );
      Btn.Width := NewWidth;
      Btn.Height := NewHeight;
      Btn.ShowCaption := ShowCaptions;
      if ShowCaptions then
      begin
        if NewHeight <= 30 then
          Btn.Layout := blGlyphLeft
        else
          Btn.Layout := blGlyphTop;
      end
      else
        Btn.Layout := blGlyphLeft;
    end;
  end;
  if FOrientation = orHorizontal then
    RowHeight := NewHeight
  else
    RowHeight := NewWidth;
end;


procedure TRzToolbar.SetMargin( Value: Integer );
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    PositionControls;
  end;
end;

procedure TRzToolbar.SetTopMargin( Value: Integer );
begin
  if FTopMargin <> Value then
  begin
    FTopMargin := Value;
    PositionControls;
  end;
end;


procedure TRzToolbar.SetOrientation( Value: TOrientation );
var
  I: Integer;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    for I := 0 to ControlCount - 1 do
    begin
      if Controls[ I ] is TRzSpacer then
        TRzSpacer( Controls[ I ] ).Orientation := FOrientation;
    end;
    PositionControls;
  end;
end;


procedure TRzToolbar.SetRowHeight( Value: Integer );
begin
  if FRowHeight <> Value then
  begin
    FRowHeight := Value;
    PositionControls;
  end;
end;


procedure TRzToolbar.SetWrapControls( Value: Boolean );
begin
  if FWrapControls <> Value then
  begin
    FWrapControls := Value;
    PositionControls;
  end;
end;

procedure TRzToolbar.SetBorderInner( Value: TFrameStyleEx );
begin
  if BorderInner <> Value then
  begin
    inherited;
    FAutoStyle := False;
  end;
end;

procedure TRzToolbar.SetBorderOuter( Value: TFrameStyleEx );
begin
  if BorderOuter <> Value then
  begin
    inherited;
    FAutoStyle := False;
  end;
end;

procedure TRzToolbar.SetBorderSides( Value: TSides );
begin
  if BorderSides <> Value then
  begin
    inherited;
    FAutoStyle := False;
  end;
end;


procedure TRzToolbar.SetBiDiMode( Value: TBiDiMode );
begin
  if BiDiMode <> Value then
  begin
    inherited;
    PositionControls;
  end;
end;


procedure TRzToolbar.DrawCaption( Rect: TRect );
begin
  { Do not draw caption for a toolbar }
end;

procedure TRzToolbar.Paint;
var
  Col, Row, X, Y, XTiles, YTiles, FCA: Integer;
  R: TRect;
  ElementDetails: TThemedElementDetails;
  FC: TColor;
begin
  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    R := GetControlRect;

    GetGradientFrameColor( FC, FCA );

    R := DrawInnerOuterBorders( Canvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, BevelWidth,
                                FBorderColor, FBorderHighlight, FBorderShadow,
                                FC, FCA, Color, TWinControlAccess( Parent ).Color, FTransparent );

    ElementDetails := ActiveStyleServices.GetElementDetails( trRebarRoot );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, ClientRect );
  end
  else
  begin
    inherited;

    { Tile the background, if there is one }
    if ( FBackground.Width > 0 ) and ( FBackground.Height > 0 ) then
    begin
      R := ClientRect;
      FixClientRect( R, True );

      XTiles := ClientWidth div FBackground.Width;
      if ClientWidth mod FBackground.Width > 0 then
        Inc( XTiles );
      YTiles := ClientHeight div FBackground.Height;
      if ClientHeight mod FBackground.Height > 0 then
        Inc( YTiles );

      IntersectClipRect( Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom );
      try
        for Row := 0 to Pred( YTiles ) do
        begin
          Y := R.Top + Row * FBackground.Height;
          for Col := 0 to Pred( XTiles ) do
          begin
            X := R.Left + Col * FBackground.Width;
            Canvas.Draw( X, Y, FBackground );
          end;
        end;
      finally
        SelectClipRgn( Canvas.Handle, 0 );
      end;
    end;
  end;

  if FShowToolbarPopupButton then
    FToolbarPopupButton.Invalidate;

end; {= TRzToolbar.Paint =}


procedure TRzToolbar.VisibleChanged;
begin
  if Assigned( FOnVisibleChanged ) then
    FOnVisibleChanged( Self );
end;


procedure TRzToolbar.CMVisibleChanged( var Msg: TMessage );
begin
  inherited;
  VisibleChanged;
end;


procedure TRzToolbar.SetShowToolbarPopupButton( Value: Boolean );
begin
  if FShowToolbarPopupButton <> Value then
  begin
    FShowToolbarPopupButton := Value;
    if FToolbarPopupButton = nil then
      CreateToolbarPopupButton;
    FToolbarPopupButton.Visible := FShowToolbarPopupButton;
    Invalidate;
  end;
end;



procedure TRzToolbar.Customize( ShowTextOptions: Boolean = True );
var
  F: TRzFrmCustomizeToolbar;
begin
  F := TRzFrmCustomizeToolbar.Create( Application );
  try
    F.CompOwner := Owner;
    F.Toolbar := Self;
    F.Reposition;
    F.UpdateControls( FCustomizeCaptions, ShowTextOptions );
    F.ShowModal;
  finally
    F.Free;
  end;
end;


procedure TRzToolbar.RestoreLayout;
var
  I, Count, OldIdx, Options: Integer;
  CN: string;
  MakeVisible: Boolean;
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzNoRegIniFile );

  Count := FRegIniFile.ReadInteger( Name, 'Count', 0 );
  if ( Count > 0 ) and ( Count = ToolbarControls.Count ) then
  begin
    for I := 0 to Count - 1 do
    begin
      MakeVisible := True;
      CN := FRegIniFile.ReadString( Name, 'Control' + IntToStr( I ), '' );
      if CN <> '' then
      begin
        if Pos( '##', CN ) = 1 then
        begin
          MakeVisible := False;
          Delete( CN, 1, 2 );
        end;
        OldIdx := ToolbarControls.IndexOfName( CN );
        if OldIdx <> -1 then
        begin
          ToolbarControls.Move( OldIdx, I );
          ToolbarControls[ I ].Control.Visible := MakeVisible;
        end;
      end;
    end;
    PositionControls;
  end;

  Options := FRegIniFile.ReadInteger( Name, 'TextOptions', -1 );
  if Options <> -1 then
    SetTextOptions( TRzToolbarTextOptions( Options ) );
end;


procedure TRzToolbar.SaveLayout;
var
  I: Integer;
  C: TControl;
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzNoRegIniFile );

  // Assume that all Controls have a valid Name

  FRegIniFile.WriteInteger( Name, 'TextOptions', Ord( FTextOptions ) );
  FRegIniFile.WriteInteger( Name, 'Count', FToolbarControls.Count );

  for I := 0 to FToolbarControls.Count - 1 do
  begin
    C := FToolbarControls.Items[ I ].Control;
    if C.Visible then
      FRegIniFile.WriteString( Name, 'Control' + IntToStr( I ), C.Name )
    else
      FRegIniFile.WriteString( Name, 'Control' + IntToStr( I ), '##' + C.Name );
  end;
end;


procedure TRzToolbar.SetGradientColorStyle( Value: TRzGradientColorStyle );
begin
  if GradientColorStyle <> Value then
  begin
    inherited;
    NotifyControls( cm_ToolbarVisualStyleChanged );
  end;
end;


procedure TRzToolbar.SetVisualStyle( Value: TRzVisualStyle );
begin
  if VisualStyle <> Value then
  begin
    inherited;
    NotifyControls( cm_ToolbarVisualStyleChanged );
  end;
end;



{==========================}
{== TRzStatusBar Methods ==}
{==========================}

constructor TRzStatusBar.Create( AOwner: TComponent );
begin
  inherited;

  FSizeGripCanvas := TControlCanvas.Create;
  TControlCanvas( FSizeGripCanvas ).Control := Self;

  Align := alBottom;
  BorderInner := fsNone;
  BorderOuter := fsNone;
  BorderSides := sdAllSides;
  Height := 19;
  BorderWidth := 0;
  FAutoStyle := True;

  FShowSizeGrip := True;
  FSimpleStatus := False;
  FSimpleFrameStyle := fsFlat;
  FSimpleCaption := '';
  FFirst := True;
  {&RCI}
end;


destructor TRzStatusBar.Destroy;
begin
  FSizeGripCanvas.Free;
  inherited;
end;


procedure TRzStatusBar.CreateWnd;
begin
  ValidateSizeGrip;
  inherited;
end;


procedure TRzStatusBar.Loaded;
begin
  inherited;
  if FAutoStyle then
    AdjustStyle;
  {&RV}
end;


procedure TRzStatusBar.WndProc( var Msg: TMessage );
const
  sc_DragSize = 61448;
var
  X, Y: Integer;
begin
  // No longer allow resizing from size grip at design-time b/c embedded
  // form designer in Delphi 2005 and BDS 2006 causes the IDE window to resize.
  if FSizeGripValid and ( Msg.Msg = wm_LButtonDown ) and
    not ( csDesigning in ComponentState ) then
  begin
    X := Msg.LParamLo;
    Y := Msg.LParamHi;
    if FShowSizeGrip and PtInRect( SizeGripRect, Point( X, Y ) ) then
    begin
      ReleaseCapture;
      GetParentForm( Self ).Perform( wm_SysCommand, sc_DragSize, 0 );
      Exit;
    end;
  end;

  inherited;
end;


procedure TRzStatusBar.SetShowSizeGrip( Value: Boolean );
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;
    ValidateSizeGrip;
    Invalidate;
  end;
end;

function TRzStatusBar.SizeGripRect: TRect;
begin
  Result := Rect( Width - 13, 0, Width, Height );
end;


procedure TRzStatusBar.ValidateSizeGrip;
var
  F: TCustomForm;
begin
  F := GetParentForm( Self );
  FSizeGripValid := ( F <> nil ) and ( F.BorderStyle in [ bsSizeable, bsSizeToolWin ] ) and
                    ( F.WindowState <> wsMaximized );
end;


procedure TRzStatusBar.WMSetCursor( var Msg: TWMSetCursor );
begin
  if FShowSizeGrip and FSizeGripValid and
     PtInRect( SizeGripRect, CursorPosition ) and
     not ( csDesigning in ComponentState ) then
  begin
    SetCursor( Screen.Cursors[ crSizeNWSE ] )
  end
  else
    inherited;
end;


procedure TRzStatusBar.Resize;
var
  I, Offset: Integer;
  Percent: Single;
begin
  inherited;

  if Width < 50 then
    Exit;

  if FFirst then
  begin
    FDelta := 0;
    FLastWidth := Width;
    FFirst := False;
  end
  else
  begin
    FDelta := Width - FLastWidth;
  end;

  { Adjust Size of all Status Panes }
  if FAutoScalePanes and ( ControlCount > 0 ) and ( Abs( FDelta ) > 0 ) then
  begin
    DisableAlign;
    try
      for I := 0 to ControlCount - 1 do
      begin
        if FLastWidth > 0 then
          Percent := Controls[ I ].Width / FLastWidth
        else
          Percent := 1 / ControlCount;

        Offset := Round( FDelta * Percent );
        Controls[ I ].Width := Controls[ I ].Width + Offset;
      end;
    finally
      EnableAlign;
    end;
  end;
  FLastWidth := Width;
end;


function TRzStatusBar.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;

  if ActiveStyleServicesAvailable then
  begin
    if FSimpleStatus then
    begin
      Inc( Result.Left );
      Inc( Result.Top );
      Dec( Result.Right );
    end;
  end
  else
  begin
    Inc( Result.Bottom, 1 );  // Removes bottom border of status panes in Win95
    if not FSimpleStatus then
    begin
      Result.Top := 1;
      Dec( Result.Left );
      Inc( Result.Right );
    end
    else
      Result.Top := 2;
  end;
end;


procedure TRzStatusBar.SetSimpleCaption( Value: TCaption );
begin
  if FSimpleCaption <> Value then
  begin
    FSimpleCaption := Value;
    Invalidate;
  end;
end;


procedure TRzStatusBar.SetSimpleFrameStyle( Value: TFrameStyle );
begin
  if FSimpleFrameStyle <> Value then
  begin
    FSimpleFrameStyle := Value;
    Invalidate;
  end;
end;


procedure TRzStatusBar.SetSimpleStatus( Value: Boolean );
var
  I: Integer;
begin
  if FSimpleStatus <> Value then
  begin
    FSimpleStatus := Value;

    DisableAlign;
    try
      for I := 0 to ControlCount - 1 do
      begin
        { Add csNoDesignVisible so TWinControls do not appear at design time }
        if csDesigning in ComponentState then
          Controls[ I ].ControlStyle := Controls[ I ].ControlStyle + [ csNoDesignVisible ];
        Controls[ I ].Visible := not FSimpleStatus;
      end;
    finally
      EnableAlign;
    end;
    Invalidate;
  end;
end;


procedure TRzStatusBar.AdjustStyle;
var
  OldAutoStyle: Boolean;
begin
  OldAutoStyle := FAutoStyle;
  try
    BorderInner := fsNone;
    BorderOuter := fsNone;
    BorderSides := sdAllSides;
    Height := 19;
    BorderWidth := 0;
  finally
    FAutoStyle := OldAutoStyle;
  end;
end;


procedure TRzStatusBar.PaintSizeGrip( R: TRect );
var
  X, Y: Integer;
  ElementDetails: TThemedElementDetails;
  HighlightColor, ShadowColor, StartColor, StopColor, DividerColor: TColor;
begin
  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    ElementDetails := ActiveStyleServices.GetElementDetails( tsGripper );
    R.Left := R.Right - 20;
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end
  else
  begin
    if FVisualStyle = vsGradient then
    begin
      GetGradientStatusBarColors( FGradientColorStyle, StartColor,
                                  StopColor, DividerColor );
      HighlightColor := StopColor;
      ShadowColor := DividerColor;
    end
    else
    begin
      HighlightColor := clBtnHighlight;
      ShadowColor := clBtnShadow;
    end;

    FSizeGripCanvas.Brush.Color := HighlightColor;
    FSizeGripCanvas.Pen.Color := HighlightColor;
    X := R.Right - 1;
    Y := R.Bottom - 0;
    FSizeGripCanvas.Rectangle( X - 4,  Y - 12, X - 2,  Y - 10 );
    FSizeGripCanvas.Rectangle( X - 4,  Y - 8,  X - 2,  Y - 6 );
    FSizeGripCanvas.Rectangle( X - 4,  Y - 4,  X - 2,  Y - 2 );
    FSizeGripCanvas.Rectangle( X - 8,  Y - 8,  X - 6,  Y - 6 );
    FSizeGripCanvas.Rectangle( X - 8,  Y - 4,  X - 6,  Y - 2 );
    FSizeGripCanvas.Rectangle( X - 12, Y - 4,  X - 10, Y - 2 );

    FSizeGripCanvas.Brush.Color := ShadowColor;
    FSizeGripCanvas.Pen.Color := ShadowColor;
    X := R.Right - 2;
    Y := R.Bottom - 1;
    FSizeGripCanvas.Rectangle( X - 4,  Y - 12, X - 2,  Y - 10 );
    FSizeGripCanvas.Rectangle( X - 4,  Y - 8,  X - 2,  Y - 6 );
    FSizeGripCanvas.Rectangle( X - 4,  Y - 4,  X - 2,  Y - 2 );
    FSizeGripCanvas.Rectangle( X - 8,  Y - 8,  X - 6,  Y - 6 );
    FSizeGripCanvas.Rectangle( X - 8,  Y - 4,  X - 6,  Y - 2 );
    FSizeGripCanvas.Rectangle( X - 12, Y - 4,  X - 10, Y - 2 );
  end;
end;


procedure TRzStatusBar.DrawSimpleStatusBorder( R: TRect );
var
  C: TColor;
  ElementDetails: TThemedElementDetails;
begin
  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    ElementDetails := ActiveStyleServices.GetElementDetails( tsPane );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end
  else if FVisualStyle = vsClassic then
  begin
    Canvas.Pen.Width := 1;

    if FSimpleFrameStyle = fsFlat then
    begin
      C := AdjustColor( FFlatColor, FFlatColorAdjustment );
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
    else
      DrawBorder( Canvas, R, FSimpleFrameStyle );
  end;
end; {= TRzStatusBar.DrawSimpleStatusBorder =}


procedure TRzStatusBar.GetGradientColors( var StartColor, StopColor: TColor );
var
  C: TColor;
begin
  if GradientColorStyle <> gcsCustom then
  begin
    GetGradientStatusBarColors( GradientColorStyle, StartColor, StopColor, C );
  end
  else
  begin
    StartColor := FGradientColorStop;
    StopColor := FGradientColorStart;
  end;
end;


procedure TRzStatusBar.Paint;
var
  X, Y: Integer;
  R: TRect;
  ElementDetails: TThemedElementDetails;
begin
  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    ElementDetails := ActiveStyleServices.GetElementDetails( tsStatusRoot );
    R := Rect( 0, 0, Width, Height );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );

    if FSimpleStatus then
    begin
      if FSimpleCaption <> '' then
        ActiveStyleServicesDrawText( Canvas.Handle, ElementDetails, WideString( FSimpleCaption ), ClientRect,
                                     dt_SingleLine or dt_NoPrefix or dt_VCenter );
    end;

  end
  else
  begin
    inherited;

    R := ClientRect;
    Dec( R.Bottom );

    if FSimpleStatus then
    begin
      DrawSimpleStatusBorder( R );

      InflateRect( R, -1, -1 );
      if FSimpleCaption <> '' then
      begin
        Canvas.Font := Font; 
        Canvas.Brush.Style := bsClear;

        X := R.Left + 2;
        Y := R.Top + ( R.Bottom - R.Top - Canvas.TextHeight( 'Pp' ) ) div 2;

        Canvas.TextRect( R, X, Y, FSimpleCaption );
        Canvas.Brush.Style := bsSolid;
      end;
    end;
  end;
end;


procedure TRzStatusBar.WMPaint( var Msg: TWMPaint );
var
  R: TRect;
begin
  inherited;

  R := ClientRect;
  Dec( R.Bottom );

  ValidateSizeGrip;
  if FShowSizeGrip and FSizeGripValid then
    PaintSizeGrip( R );
end;


procedure TRzStatusBar.SetBorderInner( Value: TFrameStyleEx );
begin
  if BorderInner <> Value then
  begin
    inherited;
    FAutoStyle := False;
  end;
end;

procedure TRzStatusBar.SetBorderOuter( Value: TFrameStyleEx );
begin
  if BorderOuter <> Value then
  begin
    inherited;
    FAutoStyle := False;
  end;
end;

procedure TRzStatusBar.SetBorderSides( Value: TSides );
begin
  if BorderSides <> Value then
  begin
    inherited;
    FAutoStyle := False;
  end;
end;



{==========================}
{== TRzFlowPanel Methods ==}
{==========================}

constructor TRzFlowPanel.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
                              { Prevent Caption from being set to default name }
  ControlStyle := ControlStyle - [ csSetCaption ];

  // Delphi 7 sets the csParentBackground style and removes the csOpaque style when Themes are available, which causes
  // all kinds of other problems, so we restore these.
  ControlStyle := ControlStyle - [ csParentBackground ] + [ csOpaque ];

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FBorderSides := [ sdLeft, sdTop, sdRight, sdBottom ];
  FBorderColor := clBtnFace;
  FBorderHighlight := clBtnHighlight;
  FBorderShadow := clBtnShadow;
  FBorderOuter := fsRaised;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;
  BevelOuter := bvNone;
  FInAlignControls := False;

  FVisualStyle := vsWinXP;
  FGradientColorStyle := gcsSystem;
  FGradientColorStart := clWhite;
  FGradientColorStop := clBtnFace;
  FGradientDirection := gdHorizontalEnd;

  FEnabledList := TStringList.Create;
  {&RV}
end;


destructor TRzFlowPanel.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  FEnabledList.Free;
  inherited;
end;


procedure TRzFlowPanel.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


function TRzFlowPanel.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzFlowPanel.AlignControls( AControl: TControl; var Rect: TRect );
begin
  // This was added to ensure that the border is drawn correctly when
  // DoubleBuffered is set to True
  FixClientRect( Rect, False );

  inherited;
end;


procedure TRzFlowPanel.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
  // The following code handles situations where the panel is aligned alRight
  // or alBottom and there is another control also aligned alRight or alBottom
  // adjacent to this control.  If the size of this panel is changed the order
  // of all the controls aligned in the same direction will get re-ordered.

  if Align = alBottom then
  begin
    if AHeight <> Height then
      ATop := Top - ( AHeight - Height );
  end
  else if Align = alRight then
  begin
    if AWidth <> Width then
      ALeft := Left - ( AWidth - Width );
  end;

  inherited;
end;


procedure TRzFlowPanel.FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean );

  procedure AdjustRect( var R: TRect; Sides: TSides; N: Integer );
  begin
    if sdLeft in Sides then
      Inc( R.Left, N );
    if sdTop in Sides then
      Inc( R.Top, N );
    if sdRight in Sides then
      Dec( R.Right, N );
    if sdBottom in Sides then
      Dec( R.Bottom, N );
  end;

begin
  if ShrinkByBorder then
    InflateRect( Rect, -BorderWidth, -BorderWidth );

  if FBorderOuter = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderOuter in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderOuter in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );

  if FBorderInner = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderInner in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderInner in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );
end;



procedure TRzFlowPanel.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  if DockSite then
    FixClientRect( Rect, False );
end;


procedure TRzFlowPanel.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
var
  R, CR: TRect;
begin
  if FullRepaint or ( Caption <> '' ) then
    Invalidate
  else
  begin

    R := Rect( 0, 0, Width, Height );
    CR := R;
    FixClientRect( CR, True );

    if Msg.WindowPos^.cx <> R.Right then
    begin
      R.Left := CR.Right - 1;
      R.Top := 0;
      InvalidateRect( Handle, @R, True );
    end;

    if Msg.WindowPos^.cy <> R.Bottom then
    begin
      R.Left := 0;
      R.Top := CR.Bottom - 1;
      InvalidateRect( Handle, @R, True );
    end;
  end;
  inherited;
end;


function TRzFlowPanel.GetControlRect: TRect;
begin
  Result := Rect( 0, 0, Width, Height );
end;


procedure TRzFlowPanel.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
begin
  if FTransparent then
    DrawParentImage( Self, Msg.DC, True );

  // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from
  // erasing background. Set Msg.Result to 1 to indicate background is painted
  // by the control.
  Msg.Result := 1;
end;


procedure TRzFlowPanel.GetGradientColors( var StartColor, StopColor: TColor );
begin
  if FGradientColorStyle <> gcsCustom then
  begin
    GetGradientPanelColors( FGradientColorStyle, StartColor, StopColor );
  end
  else
  begin
    StartColor := FGradientColorStart;
    StopColor := FGradientColorStop;
  end;
end;


procedure TRzFlowPanel.Paint;
var
  R: TRect;
  StartColor, StopColor: TColor;
begin
  R := GetControlRect;

  R := DrawInnerOuterBorders( Canvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, BevelWidth,
                              FBorderColor, FBorderHighlight, FBorderShadow,
                              FlatColor, FlatColorAdjustment, Color, TWinControlAccess( Parent ).Color, FTransparent );

  if not FTransparent then
  begin
    case FVisualStyle of
      vsClassic, vsWinXP:
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect( R );
      end;

      vsGradient:
      begin
        GetGradientColors( StartColor, StopColor );
        PaintGradient( Canvas, R, FGradientDirection, StartColor, StopColor );
      end;
    end;
  end;

  if Assigned( FOnPaint ) then
    FOnPaint( Self );

end; {= TRzFlowPanel.Paint =}


procedure TRzFlowPanel.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameStyle in FFrameControllerNotifications then
      FBorderOuter := FFrameController.FrameStyle;
    if fcpFrameSides in FFrameControllerNotifications then
      FBorderSides := FFrameController.FrameSides;
    if fcpFrameColor in FFrameControllerNotifications then
    begin
      FFlatColor := FFrameController.FrameColor;
      FFlatColorAdjustment := 0;
    end;
    if fcpColor in FFrameControllerNotifications then
      Color := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;

    if fcpColor in FFrameControllerNotifications then
    begin
      FBorderHighlight := LighterColor( Color, 100 );
      FBorderShadow := DarkerColor( Color, 50 );
    end;

    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetBorderSides( Value: TSides );
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TRzFlowPanel.SetBorderHighlight( Value: TColor );
begin
  if FBorderHighlight <> Value then
  begin
    FBorderHighlight := Value;
    Invalidate;
  end;
end;

procedure TRzFlowPanel.SetBorderShadow( Value: TColor );
begin
  if FBorderShadow <> Value then
  begin
    FBorderShadow := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetBorderInner( Value: TFrameStyleEx );
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetBorderOuter( Value: TFrameStyleEx );
begin
  if FBorderOuter <> Value then
  begin
    FBorderOuter := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TRzFlowPanel.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetFrameController( Value: TRzFrameController );
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


procedure TRzFlowPanel.SetGradientColorStyle( Value: TRzGradientColorStyle );
begin
  if FGradientColorStyle <> Value then
  begin
    FGradientColorStyle := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetGradientColorStart( Value: TColor );
begin
  if FGradientColorStart <> Value then
  begin
    FGradientColorStart := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetGradientColorStop( Value: TColor );
begin
  if FGradientColorStop <> Value then
  begin
    FGradientColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.SetTransparent( Value: Boolean );
var
  I: Integer;
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [ csOpaque ]
    else
      ControlStyle := ControlStyle + [ csOpaque ];
    Invalidate;
    for I := 0 to ControlCount - 1 do
      Controls[ I ].Invalidate;
  end;
end;


procedure TRzFlowPanel.SetVisualStyle( Value: TRzVisualStyle );
begin
  if FVisualStyle <> Value then
  begin
    FVisualStyle := Value;
    Invalidate;
  end;
end;


procedure TRzFlowPanel.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;
end;


procedure TRzFlowPanel.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
end;


procedure TRzFlowPanel.CMEnabledChanged( var Msg: TMessage );
var
  I, Idx: Integer;
begin
  inherited;
  Repaint;

  if not Enabled then
  begin
    FEnabledList.Clear;

    for I := 0 to ControlCount - 1 do
    begin
      if Controls[ I ].Enabled then
        FEnabledList.AddObject( '1', Controls[ I ] )
      else
        FEnabledList.AddObject( '0', Controls[ I ] );
    end;

    for I := 0 to ControlCount - 1 do
      Controls[ I ].Enabled := False;
  end
  else
  begin
    for I := 0 to ControlCount - 1 do
    begin
      Idx := FEnabledList.IndexOfObject( Controls[ I ] );
      if Idx <> -1 then
      begin
        if FEnabledList[ Idx ] = '1' then
          Controls[ I ].Enabled := True;
      end
      else
        Controls[ I ].Enabled := True;
    end;
  end;
end;


procedure TRzFlowPanel.WMThemeChanged( var Msg: TMessage );
begin
  inherited;
  // Update CurrentXPColorScheme global variable
  CurrentXPColorScheme := GetXPColorScheme;
end;


{==========================}
{== TRzGridPanel Methods ==}
{==========================}

constructor TRzGridPanel.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
                              { Prevent Caption from being set to default name }
  ControlStyle := ControlStyle - [ csSetCaption ];

  // Delphi 7 sets the csParentBackground style and removes the csOpaque style when Themes are available, which causes
  // all kinds of other problems, so we restore these.
  ControlStyle := ControlStyle - [ csParentBackground ] + [ csOpaque ];

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FBorderSides := [ sdLeft, sdTop, sdRight, sdBottom ];
  FBorderColor := clBtnFace;
  FBorderHighlight := clBtnHighlight;
  FBorderShadow := clBtnShadow;
  FBorderOuter := fsRaised;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;
  BevelOuter := bvNone;
  FInAlignControls := False;

  FVisualStyle := vsWinXP;
  FGradientColorStyle := gcsSystem;
  FGradientColorStart := clWhite;
  FGradientColorStop := clBtnFace;
  FGradientDirection := gdHorizontalEnd;

  FEnabledList := TStringList.Create;
  {&RV}
end;


destructor TRzGridPanel.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  FEnabledList.Free;
  inherited;
end;


procedure TRzGridPanel.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


function TRzGridPanel.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzGridPanel.AlignControls( AControl: TControl; var Rect: TRect );
begin
  // This was added to ensure that the border is drawn correctly when
  // DoubleBuffered is set to True
  FixClientRect( Rect, False );

  inherited;
end;


procedure TRzGridPanel.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
  // The following code handles situations where the panel is aligned alRight
  // or alBottom and there is another control also aligned alRight or alBottom
  // adjacent to this control.  If the size of this panel is changed the order
  // of all the controls aligned in the same direction will get re-ordered.

  if Align = alBottom then
  begin
    if AHeight <> Height then
      ATop := Top - ( AHeight - Height );
  end
  else if Align = alRight then
  begin
    if AWidth <> Width then
      ALeft := Left - ( AWidth - Width );
  end;

  inherited;
end;


procedure TRzGridPanel.FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean );

  procedure AdjustRect( var R: TRect; Sides: TSides; N: Integer );
  begin
    if sdLeft in Sides then
      Inc( R.Left, N );
    if sdTop in Sides then
      Inc( R.Top, N );
    if sdRight in Sides then
      Dec( R.Right, N );
    if sdBottom in Sides then
      Dec( R.Bottom, N );
  end;

begin
  if ShrinkByBorder then
    InflateRect( Rect, -BorderWidth, -BorderWidth );

  if FBorderOuter = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderOuter in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderOuter in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );

  if FBorderInner = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderInner in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderInner in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );
end;



procedure TRzGridPanel.AdjustClientRect( var Rect: TRect );
begin
  inherited;
  if DockSite then
    FixClientRect( Rect, False );
end;


procedure TRzGridPanel.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
var
  R, CR: TRect;
begin
  if FullRepaint or ( Caption <> '' ) then
    Invalidate
  else
  begin

    R := Rect( 0, 0, Width, Height );
    CR := R;
    FixClientRect( CR, True );

    if Msg.WindowPos^.cx <> R.Right then
    begin
      R.Left := CR.Right - 1;
      R.Top := 0;
      InvalidateRect( Handle, @R, True );
    end;

    if Msg.WindowPos^.cy <> R.Bottom then
    begin
      R.Left := 0;
      R.Top := CR.Bottom - 1;
      InvalidateRect( Handle, @R, True );
    end;
  end;
  inherited;
end;


function TRzGridPanel.GetControlRect: TRect;
begin
  Result := Rect( 0, 0, Width, Height );
end;


procedure TRzGridPanel.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
begin
  if FTransparent then
    DrawParentImage( Self, Msg.DC, True );

  // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from
  // erasing background. Set Msg.Result to 1 to indicate background is painted
  // by the control.
  Msg.Result := 1;
end;


procedure TRzGridPanel.GetGradientColors( var StartColor, StopColor: TColor );
begin
  if FGradientColorStyle <> gcsCustom then
  begin
    GetGradientPanelColors( FGradientColorStyle, StartColor, StopColor );
  end
  else
  begin
    StartColor := FGradientColorStart;
    StopColor := FGradientColorStop;
  end;
end;


type
  TAccessCellItem = class( TCellItem )
  end;

procedure TRzGridPanel.DrawGridLines;
var
  I: Integer;
  LinePos, Size: Integer;
  ClientRect: TRect;
begin
  LinePos := 0;
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Color := clBlack;
  ClientRect := GetClientRect;
  for I := 0 to ColumnCollection.Count - 2 do
  begin
    Size := TAccessCellItem( ColumnCollection[ I ] ).Size;
    Canvas.MoveTo( LinePos + Size, ClientRect.Top );
    Canvas.LineTo( LinePos + Size, ClientRect.Bottom );
    Inc( LinePos, Size );
  end;
  LinePos := 0;
  for I := 0 to RowCollection.Count - 2 do
  begin
    Size := TAccessCellItem( RowCollection[ I ] ).Size;
    Canvas.MoveTo( ClientRect.Left, LinePos + Size );
    Canvas.LineTo( ClientRect.Right, LinePos + Size );
    Inc( LinePos, Size );
  end;
end;


procedure TRzGridPanel.Paint;
var
  R: TRect;
  StartColor, StopColor: TColor;
begin
  R := GetControlRect;

  R := DrawInnerOuterBorders( Canvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, BevelWidth,
                              FBorderColor, FBorderHighlight, FBorderShadow,
                              FlatColor, FlatColorAdjustment, Color, TWinControlAccess( Parent ).Color, FTransparent );

  if not FTransparent then
  begin
    case FVisualStyle of
      vsClassic, vsWinXP:
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect( R );
      end;

      vsGradient:
      begin
        GetGradientColors( StartColor, StopColor );
        PaintGradient( Canvas, R, FGradientDirection, StartColor, StopColor );
      end;
    end;
  end;

  if csDesigning in ComponentState then
    DrawGridLines;

  if Assigned( FOnPaint ) then
    FOnPaint( Self );

end; {= TRzGridPanel.Paint =}


procedure TRzGridPanel.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameStyle in FFrameControllerNotifications then
      FBorderOuter := FFrameController.FrameStyle;
    if fcpFrameSides in FFrameControllerNotifications then
      FBorderSides := FFrameController.FrameSides;
    if fcpFrameColor in FFrameControllerNotifications then
    begin
      FFlatColor := FFrameController.FrameColor;
      FFlatColorAdjustment := 0;
    end;
    if fcpColor in FFrameControllerNotifications then
      Color := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;

    if fcpColor in FFrameControllerNotifications then
    begin
      FBorderHighlight := LighterColor( Color, 100 );
      FBorderShadow := DarkerColor( Color, 50 );
    end;

    Invalidate;
  end;
end;


procedure TRzGridPanel.SetBorderSides( Value: TSides );
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TRzGridPanel.SetBorderHighlight( Value: TColor );
begin
  if FBorderHighlight <> Value then
  begin
    FBorderHighlight := Value;
    Invalidate;
  end;
end;

procedure TRzGridPanel.SetBorderShadow( Value: TColor );
begin
  if FBorderShadow <> Value then
  begin
    FBorderShadow := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetBorderInner( Value: TFrameStyleEx );
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    Realign;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetBorderOuter( Value: TFrameStyleEx );
begin
  if FBorderOuter <> Value then
  begin
    FBorderOuter := Value;
    Realign;
    Invalidate;
  end;
end;

procedure TRzGridPanel.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetFrameController( Value: TRzFrameController );
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


procedure TRzGridPanel.SetGradientColorStyle( Value: TRzGradientColorStyle );
begin
  if FGradientColorStyle <> Value then
  begin
    FGradientColorStyle := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetGradientColorStart( Value: TColor );
begin
  if FGradientColorStart <> Value then
  begin
    FGradientColorStart := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetGradientColorStop( Value: TColor );
begin
  if FGradientColorStop <> Value then
  begin
    FGradientColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.SetTransparent( Value: Boolean );
var
  I: Integer;
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if FTransparent then
      ControlStyle := ControlStyle - [ csOpaque ]
    else
      ControlStyle := ControlStyle + [ csOpaque ];
    Invalidate;
    for I := 0 to ControlCount - 1 do
      Controls[ I ].Invalidate;
  end;
end;


procedure TRzGridPanel.SetVisualStyle( Value: TRzVisualStyle );
begin
  if FVisualStyle <> Value then
  begin
    FVisualStyle := Value;
    Invalidate;
  end;
end;


procedure TRzGridPanel.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;
end;


procedure TRzGridPanel.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
end;


procedure TRzGridPanel.CMEnabledChanged( var Msg: TMessage );
var
  I, Idx: Integer;
begin
  inherited;
  Repaint;

  if not Enabled then
  begin
    FEnabledList.Clear;

    for I := 0 to ControlCount - 1 do
    begin
      if Controls[ I ].Enabled then
        FEnabledList.AddObject( '1', Controls[ I ] )
      else
        FEnabledList.AddObject( '0', Controls[ I ] );
    end;

    for I := 0 to ControlCount - 1 do
      Controls[ I ].Enabled := False;
  end
  else
  begin
    for I := 0 to ControlCount - 1 do
    begin
      Idx := FEnabledList.IndexOfObject( Controls[ I ] );
      if Idx <> -1 then
      begin
        if FEnabledList[ Idx ] = '1' then
          Controls[ I ].Enabled := True;
      end
      else
        Controls[ I ].Enabled := True;
    end;
  end;
end;


procedure TRzGridPanel.WMThemeChanged( var Msg: TMessage );
begin
  inherited;
  // Update CurrentXPColorScheme global variable
  CurrentXPColorScheme := GetXPColorScheme;
end;


{&RUIF}
end.
