{===============================================================================
  RzRadChk Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzRadioButton
    Custom radio button control--supports multi-line captions, 3D text styles,
    custom glyphs

  TRzCheckBox
    Custom check box control--supports multi-line captions, 3D text styles,
    custom glyphs


  Modification History
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Redesigned the painting code for TRzCheckBox and TRzRadioButton to
      address transparency display issues.
  ------------------------------------------------------------------------------
  6.1.4  (29 May 2013)
    * Fixed issue where background of TRzCheckBox and TRzRadioButton would not
      be painted correctly if the parent's DoubleBuffered property was True.
  ------------------------------------------------------------------------------
  6.1.3  (01 May 2013)
    * Fixed potential issue in TRzCheckBox and TRzRadioButton where successive
      scale changes could produce an exception.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Updated code in TRzCheckBox and TRzRadioButton to utilize fixed VCL
      functions to get the size of the checkbox and radio button of a custom
      VCL Style.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed issue where incorrect custom glyphs was displayed for a TRzCheckBox
      using the Grayed state and has the HotTrack property set to False;
    * Fixed issue in TRzCheckBox and TRzRadioButton where the WordWrap property
      would get automatically set to True when CustomGlyphs were used.
    * Made necessary modifications to TRzCheckBox and TRzRadioButton to fully
      support VCL Styles introduced in RAD Studio XE2. Including support for
      custom glyphs even when VCL Styles are used.
  ------------------------------------------------------------------------------
  5.5.1  (31 Mar 2011)
    * Fixed display issue in a transparent TRzRadioButton and TRzCheckBox that
      would occur if the parent control modified its background.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzCheckBox and TRzRadioButton controls have been updated such that
      the controls will hide any accelerator characters in the caption until the
      user presses the Alt key. Likewise, the controls will hide the focus
      rectangle until the user navigates on the form using the keyboard. The
      controls honor the Windows system setting that affects this behavior.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * The TRzRadioButton and TRzCheckBox now scale the glyph image when using
      themes and the system is running at a higher DPI settings.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Added new WordWrap property to TRzRadioButton and TRzCheckBox. As a result
      the controls no longer resize themselves and wrap the caption if the
      caption changes to be longer than 100 pixels. Instead, the Caption will
      only wrap when the WordWrap property is set to True.  This makes the
      behavior of the TRzRadioButton and TRzCheckBox to be consistent with
      the TRzLabel with respect to auto-sizing and word wrapping.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new AutoSize property to TRzRadioButton and TRzCheckBox. When this
      property is True (the default), the bounds of the control are
      automatically adjusted so that the entire caption is visible. This also
      allows the focus rectangle to surround just the caption of the control
      rather than just the entire client area.
    * Added new CustomGlyphImages property to TRzRadioButton and TRzCheckBox.
      This property is used to reference an ImageList that contains the glyphs
      to be used for the various states of the control. This new property should
      be used instead of the deprecated CustomGlyphs property, which is still
      available strictly for backward compatibility. By referencing an ImageList
      that holds the custom glyphs rather than an embedded bitmap, the actual
      glyph images are stored only once in the application instead of inside
      each instance of the control. When populating a TImageList for use with
      CustomGlyphImages, each index in the ImageList represents a different
      state.  The following tables describe the mapping:

      TRzRadioButton CustomGlyphImages Index Mapping
        Index  State
          0    Unchecked
          1    Checked
          2    Unchecked - Pressed
          3    Checked   - Pressed
          4    Unchecked - Disabled
          5    Checked   - Disabled
          6    Unchecked - Hot       (Optional)
          7    Checked   - Hot       (Optional)

      TRzCheckBox CustomGlyphImages Index Mapping
        Index  State
          0    Unchecked
          1    Checked
          2    Grayed
          3    Unchecked - Pressed
          4    Checked   - Pressed
          5    Grayed    - Pressed
          6    Unchecked - Disabled
          7    Checked   - Disabled
          8    Grayed    - Disabled
          9    Unchecked - Hot       (Optional)
          10   Checked   - Hot       (Optional)
          11   Grayed    - Hot       (Optional)

    * As noted in the above table, with the new CustomGlyphImages property, it
      is now possible to specify "hot" glyphs. That is, separate images to be
      displayed for a given state when the mouse is positioned over the control.
    * Fixed issue where focus rectangle would not completely surround the
      caption of a TRzRadioButton or TRzCheckBox that contained tab characters.
    * Modified the header file (RzRadChk.hpp) that gets generated by the Delphi
      compiler when compiling components for use in C++Builder. The new
      modifications allow C++Builder developers to create custom controls that
      descend from the TRzCustomGlyphButton class (or from other classes that
      descend from this class such as TRzCheckBox or TRzRadioButton) without
      resulting in linker errors because of differences in how the HDC type is
      defined in Delphi and C++.
      NOTE:
        - When using C++Builder 2009 or later, the above modifications are not
          necessary because of changes made to the Delphi compiler. However, the
          changes are dependent on the STRICT conditional symbol being defined.
          However C++Builder projects define the NO_STRICT symbol by default.
          Therefore, in order to compile and link descendant controls in
          C++Builder 2009 or later, the NO_STRICT symbol must be replaced with
          the STRICT symbol.
    * Registered custom property editor for the Caption property of TRzCheckBox
      and TRzRadioButton. The editor allows the developer to display a dialog
      box that can be used to enter multi-line captions at design-time.
    * Added new ThemeAware property to TRzCheckBox and TRzRadioButton. When this
      property is set to True (the default) the control will use XP/Vista themes
      for display (if available).  When set to False, the control will be drawn
      using the visual property setttings of the controls even if themes are
      available.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Fixed issue where the background color of a TRzRadioButton or TRzCheckBox
      would not get updated when changing the Color property at runtime.
    * Fixed problem in TRzRadioButton where the border would have artifacts if
      the control was placed on a gradient background and was transparent and
      running under XP/Vista themes.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Fixed problem where TRzCheckBox would display slowly under certain
      conditions under Windows Vista.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where pressing an accelerator character for a TRzCheckBox
      would cause the state to change even if the check box was ReadOnly.
    * TRzCheckBox and TRzRadioButton now handle the BM_GETCHECK window message
      to return the current state of the control. This message is used by
      accessibility applications such as Windows Eyes.
    * Fixed flicker issue with TRzCheckBox and TRzRadioButton.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Added ReadOnly, ReadOnlyColor, and ReadOnlyColorOnFocus proeprties to
      TRzCheckBox and TRzRadioButton.  When either control is ReadOnly, the
      state of the control cannot be changed by the user (i.e. via mouse or
      keyboard). The state can stil be changed programatically.  Also, when a
      TRzRadioButton is made ReadOnly, all of the other sibling radio buttons
      (i.e within the same container) are also made ReadOnly.
    * Fixed problem where the check mark displayed in a disabled TRzCheckBox
      would appear in the normal color instead of the disabled color.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem where TRzRadioButton and TRzCheckBox would not pick up the
      correct settings when connected to a TRzFrameController in Delphi 5.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Added FillColor and FocusColor properties to TRzRadioButton and
      TRzCheckbox. These properties affect the interior of the radio button
      and the check box glyph. When the control is focused, the interior is
      filled with the FocusColor otherwise the FillColor is used. These
      properties can also be controlled through a connected TRzFrameController.
    * Added HotTrackStyle property to TRzCheckBox and TRzRadioButton, which
      determines the appearance of hot track highlighting when the HotTrack
      property is set to True. The default of htsInterior is identical to
      previous versions where the interior is highlighted.  When set to htsFrame
      the frame of the box (or circle) is highlighted to be thicker--the same
      appearance as setting FrameHotStyle to fsFlatBold in TRzEdit and others.
      The TRzFrameController can also be used to manage the this new property
      and apperance.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * The display methods for TRzRadioButton and TRzCheckBox has been completely
      redesigned in this version. The old approach utilized a bitmap resource
      and the control would replace colors in the resource to match the
      desired colors of the control. This was an adequate approach, but it did
      not allow for a smooth display (especially in radio buttons). Also, hot
      tracking was very rough looking as only two colors were used.  In this
      new version, the glyph images for the TRzRadioButton and TRzCheckBox are
      drawn using the new DrawRadioButton and DrawCheckBox procedures that were
      added to the RzCommon unit.  The new style display provides a very
      effective and polished appearance for both controls.  This is especially
      true with the circular radio button and the use of gradients for hot
      tracking.
    * Fixed problem where bottom portion of TRzCheckBox or TRzRadioButton
      caption would get cut off when using Large Fonts and using default size
      of the control.
    * Fixed display issues in TRzRadioButton and TRzCheckBox when running under
      RTL systems.
    * Surfaced Align property in TRzRadioButton and TRzCheckBox.
    * Added new FrameControllerNotifications property to TRzRadioButton and
      TRzCheckBox.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed issue where change the Checked property of an Action that is 
      connected to a TRzCheckBox caused the check box to have painting issues.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed display problems when running under Right-To-Left locales.
    * Fixed display problems when running under 256 and 16-bit color depths.
    * Fixed problem of disappearing check marks and radio button circles when
      Highlight color is clTeal as is used in the Dessert color scheme.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added GetHotTrackRect override.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added HotTrack property.
    * Fixed problem with OnExit event not firing.
===============================================================================}

{$I RzComps.inc}

unit RzRadChk;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  RzCommon,
  RzButton,
  ActnList,
  ImgList,
  Menus;

const
  DefaultGlyphWidth  = 13;
  DefaultGlyphHeight = 13;

  DefaultNumGlyphs_RadioButton = 6;
  DefaultNumGlyphs_CheckBox = 9;


type
  {============================================}
  {== TRzCustomGlyphButton Class Declaration ==}
  {============================================}

  TRzCustomGlyphButton = class;

  TRzCheckedActionLink = class( TWinControlActionLink )
  protected
    FClient: TRzCustomGlyphButton;
    procedure AssignClient( AClient: TObject ); override;
    function IsCheckedLinked: Boolean; override;
    procedure SetChecked( Value: Boolean ); override;
  end;

  TRzCheckedActionLinkClass = class of TRzCheckedActionLink;

  TRzCustomGlyphButton = class( TRzCustomButton, IRzCustomFramingNotification )
  private
    FAlignment: TLeftRight;
    FAutoSize: Boolean;
    FWordWrap: Boolean;
    FFrameColor: TColor;
    FNumGlyphs: Integer;
    FCustomGlyphs: TBitmap;
    FCustomGlyphImages: TCustomImageList;
    FCustomGlyphImagesChangeLink: TChangeLink;
    FUseCustomGlyphs: Boolean;
    FTransparentColor: TColor;
    FWinMaskColor: TColor;
    FFillColor: TColor;
    FFocusColor: TColor;
    FDisabledColor: TColor;
    FReadOnly: Boolean;
    FReadOnlyColorOnFocus: Boolean;
    FReadOnlyColor: TColor;
    FGlyphWidth: Integer;
    FGlyphHeight: Integer;
    FThemedGlyphWidth: Integer;
    FThemedGlyphHeight: Integer;
    FTabOnEnter: Boolean;
    FHotTrackStyle: TRzButtonHotTrackStyle;

    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    procedure ReadOldFrameFlatProp( Reader: TReader );

    function IsCheckedStored: Boolean;

    { Internal Event Handlers }
    procedure CustomGlyphsChanged( Sender: TObject );
    procedure CustomGlyphImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
  protected
    FClicksDisabled: Boolean;
    FBackgroundBmp: TBitmap;
    FUsingMouse: Boolean;

    procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;
    procedure AdjustBounds; dynamic;
    procedure ChangeScale( M, D: Integer ); override;

    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); override;
    function GetActionLinkClass: TControlActionLinkClass; override;

    procedure CustomFramingChanged; virtual;

    procedure GetGlyphPosition( var X, Y: Integer ); virtual;
    procedure UpdateGlyphDimensions; virtual; abstract;
    procedure SelectGlyph( Glyph: TBitmap ); virtual; abstract;

    procedure UpdateDisplay; override;
    procedure RepaintDisplay; override;
    function GetHotTrackRect: TRect; override;

    procedure BlendButtonFrame( Glyph: TBitmap ); virtual;
    procedure DrawGlyph( ACanvas: TCanvas ); virtual;

    procedure UpdateUseCustomGlyphs;

    procedure Paint; override;

    procedure ExtractGlyph( Index: Integer; Bitmap, Source: TBitmap; W, H: Integer ); virtual;

    { Event Dispatch Methods }
    procedure KeyPress( var Key: Char ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;


    { Property Access Methods }
    procedure SetAlignment( Value: TLeftRight ); virtual;
    procedure SetAutoSize( Value: Boolean ); override;
    function GetChecked: Boolean; virtual;
    procedure SetChecked( Value: Boolean ); virtual;
    procedure SetCustomGlyphs( Value: TBitmap ); virtual;
    procedure SetCustomGlyphImages( Value: TCustomImageList ); virtual;
    procedure SetFillColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetUseCustomGlyphs( Value: Boolean ); virtual;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetReadOnly( Value: Boolean ); virtual;
    procedure SetReadOnlyColor( Value: TColor ); virtual;
    procedure SetTransparentColor( Value: TColor ); virtual;
    procedure SetWinMaskColor( Value: TColor ); virtual;
    procedure SetWordWrap( Value: Boolean ); virtual;

    { Property Declarations }
    property Alignment: TLeftRight
      read FAlignment
      write SetAlignment
      default taRightJustify;

    property AutoSize: Boolean
      read FAutoSize
      write SetAutoSize
      default True;

    property Checked: Boolean
      read GetChecked
      write SetChecked
      stored IsCheckedStored
      default False;

    property CustomGlyphs: TBitmap
      read FCustomGlyphs
      write SetCustomGlyphs;

    property CustomGlyphImages: TCustomImageList
      read FCustomGlyphImages
      write SetCustomGlyphImages;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      default clBtnFace;

    property FillColor: TColor
      read FFillColor
      write SetFillColor
      default clWindow;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      default clWindow;

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

    property HotTrackStyle: TRzButtonHotTrackStyle
      read FHotTrackStyle
      write FHotTrackStyle
      default htsInterior;

    property ReadOnly: Boolean
      read FReadOnly
      write SetReadOnly
      default False;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write FReadOnlyColorOnFocus
      default False;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      default clInfoBk;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property TransparentColor: TColor
      read FTransparentColor
      write SetTransparentColor
      default clOlive;

    property UseCustomGlyphs: Boolean
      read FUseCustomGlyphs
      write SetUseCustomGlyphs
      default False;

    property WinMaskColor: TColor
      read FWinMaskColor
      write SetWinMaskColor
      default clLime;

    property WordWrap: Boolean
      read FWordWrap
      write SetWordWrap
      default False;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function GetControlsAlignment: TAlignment; override;
  end;


  {======================================}
  {== TRzRadioButton Class Declaration ==}
  {======================================}

  TRzRadioButton = class( TRzCustomGlyphButton )
  private
    FAboutInfo: TRzAboutInfo;
    FChecked: Boolean;

    { Message Handling Methods }
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMDialogKey( var Msg: TCMDialogKey ); message cm_DialogKey;
    procedure WMSetFocus( var Msg: TWMSetFocus ); message wm_SetFocus;
    procedure WMLButtonDblClk( var Msg: TWMLButtonDblClk ); message wm_LButtonDblClk;
    procedure BMGetCheck( var Msg: TMessage ); message bm_GetCheck;
  protected
    procedure ChangeState; override;
    procedure UpdateGlyphDimensions; override;
    procedure SelectGlyph( Glyph: TBitmap ); override;
    function GetStyleFontColor( Enabled: Boolean ): TColor; override;
    procedure BlendButtonFrame( Glyph: TBitmap ); override;

    { Property Access Methods }
    function GetChecked: Boolean; override;
    procedure SetChecked( Value: Boolean ); override;
    procedure SetReadOnly( Value: Boolean ); override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Action;
    property Align;
    property Alignment;
    property AlignmentVertical default avTop;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property CustomGlyphs;
    property CustomGlyphImages;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FillColor;
    property FocusColor;
    property FrameColor;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property Height;
    property HelpContext;
    property HighlightColor;
    property Hint;
    property HotTrack;
    property HotTrackColor;
    property HotTrackColorType;
    property HotTrackStyle;
    property LightTextStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property TextHighlightColor;
    property TextShadowColor;
    property TextShadowDepth;
    property ShowHint;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TextStyle;
    property ThemeAware;
    property Transparent;
    property TransparentColor;
    property UseCustomGlyphs;
    property Visible;
    property Width;
    property WinMaskColor;
    property WordWrap;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  {=========================================}
  {== TRzCustomCheckBox Class Declaration ==}
  {=========================================}

  TRzCustomCheckBox = class( TRzCustomGlyphButton )
  private
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    FKeyToggle: Boolean;

    { Message Handling Methods }
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure BMGetCheck( var Msg: TMessage ); message bm_GetCheck;
  protected
    procedure ChangeState; override;
    procedure UpdateGlyphDimensions; override;
    procedure SelectGlyph( Glyph: TBitmap ); override;
    function GetStyleFontColor( Enabled: Boolean ): TColor; override;

    { Event Dispatch Methods }
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
    procedure DoExit; override;

    { Property Access Methods }
    function GetChecked: Boolean; override;
    procedure SetChecked( Value: Boolean ); override;
    procedure SetState( Value: TCheckBoxState ); virtual;

    { Property Declarations }
    property AllowGrayed: Boolean
      read FAllowGrayed
      write FAllowGrayed
      default False;

    property State: TCheckBoxState
      read FState
      write SetState;

  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure InitState( Value: TCheckBoxState );
  end;


  {== TRzCheckBox Class Declaration ==}

  TRzCheckBox = class( TRzCustomCheckBox )
  private
    FAboutInfo: TRzAboutInfo;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Action;
    property Align;
    property Alignment;
    property AlignmentVertical default avTop;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property CustomGlyphs;
    property CustomGlyphImages;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FillColor;
    property FocusColor;
    property FrameColor;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property Height;
    property HelpContext;
    property HighlightColor;
    property Hint;
    property HotTrack;
    property HotTrackColor;
    property HotTrackColorType;
    property HotTrackStyle;
    property LightTextStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property TextHighlightColor;
    property TextShadowColor;
    property TextShadowDepth;
    property ShowHint;
    property State;
    property TabOnEnter;
    property TabOrder;
    property TabStop default True;
    property TextStyle;
    property ThemeAware;
    property Transparent;
    property TransparentColor;
    property UseCustomGlyphs;
    property Visible;
    property Width;
    property WinMaskColor;
    property WordWrap;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  {&RAS}
  Themes,
  RzCommonBitmaps,
  RzGrafx;


{==================================}
{== TRzCheckedActionLink Methods ==}
{==================================}

procedure TRzCheckedActionLink.AssignClient( AClient: TObject );
begin
  inherited;
  FClient := AClient as TRzCustomGlyphButton;
end;

function TRzCheckedActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and
    ( FClient.Checked = ( Action as TCustomAction ).Checked );
end;

procedure TRzCheckedActionLink.SetChecked( Value: Boolean );
begin
  if IsCheckedLinked then
  begin
    FClient.FClicksDisabled := True;
    try
      FClient.Checked := Value;
    finally
      FClient.FClicksDisabled := False;
    end;
  end;
end;


{&RT}
{==================================}
{== TRzCustomGlyphButton Methods ==}
{==================================}

constructor TRzCustomGlyphButton.Create( AOwner: TComponent );
begin
  inherited;
  FNumGlyphs := 2;

  FAutoSize := True;
  WordWrap := False;

  FGlyphWidth := DefaultGlyphWidth;
  FGlyphHeight := DefaultGlyphHeight;
  FThemedGlyphWidth := FGlyphWidth;
  FThemedGlyphHeight := FGlyphHeight;

  FCustomGlyphs := TBitmap.Create;
  FCustomGlyphs.OnChange := CustomGlyphsChanged;
  FCustomGlyphImagesChangeLink := TChangeLink.Create;
  FCustomGlyphImagesChangeLink.OnChange := CustomGlyphImagesChange;
  FUseCustomGlyphs := False;

  FTabOnEnter := False;

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FFrameColor := clBtnShadow;
  FDisabledColor := clBtnFace;
  FTransparentColor := clOlive;
  FWinMaskColor := clLime;
  FFocusColor := clWindow;
  FFillColor := clWindow;
  FReadOnly := False;
  FReadOnlyColor := clInfoBk;
  FReadOnlyColorOnFocus := False;

  FBackgroundBmp := TBitmap.Create;
  FUsingMouse := False;

  FAlignment := taRightJustify;
  ControlStyle := ControlStyle + [ csSetCaption, csReplicatable ];
end;


procedure TRzCustomGlyphButton.CreateWnd;
begin
  inherited;
  AdjustBounds;
end;


destructor TRzCustomGlyphButton.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  FCustomGlyphs.Free;
  FCustomGlyphImagesChangeLink.Free;
  FBackgroundBmp.Free;
  inherited;
end;


procedure TRzCustomGlyphButton.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the Flat property was renamed to HotTrack
  Filer.DefineProperty( 'Flat', ReadOldFrameFlatProp, nil, False );
end;


procedure TRzCustomGlyphButton.ReadOldFrameFlatProp( Reader: TReader );
begin
  HotTrack := Reader.ReadBoolean;
end;


procedure TRzCustomGlyphButton.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FFrameController then
      FFrameController := nil
    else if AComponent = FCustomGlyphImages then
      SetCustomGlyphImages( nil );  // Call access method so connections to link object can be cleared
  end;
end;


procedure TRzCustomGlyphButton.Loaded;
begin
  inherited;
  AdjustBounds;
end;


procedure TRzCustomGlyphButton.AdjustBounds;
var
  DC: HDC;
  X, W, H: Integer;
  R: TRect;
begin
  if HandleAllocated and not ( csReading in ComponentState ) and FAutoSize then
  begin
    R := ClientRect;
    InflateRect( R, -1, -1 );
    Dec( R.Right, FGlyphWidth + 4 );

    if R.Right - R.Left < 2 then
      R.Right := R.Left + 2;

    DC := GetDC( 0 );
    try
      Canvas.Handle := DC;
      Canvas.Font := Self.Font;
      if FWordWrap then
        DrawString( Canvas, Caption, R, dt_CalcRect or dt_WordBreak or dt_ExpandTabs )
      else
        DrawString( Canvas, Caption, R, dt_CalcRect or dt_ExpandTabs );
      Canvas.Handle := 0;
    finally
      ReleaseDC( 0, DC );
    end;

    W := R.Right - R.Left + 2;  // +2 to counter InflateRect above
    H := Max( FGlyphHeight + 2, R.Bottom - R.Top + 2 );
    if ( FAlignment = taRightJustify ) xor UseRightToLeftAlignment then
      X := Left
    else
      X := BoundsRect.Right - W - FGlyphWidth - 4;

    SetBounds( X, Top, W + FGlyphWidth + 4, H );
  end;
end;


procedure TRzCustomGlyphButton.ChangeScale( M, D: Integer );
begin
  inherited;
  if UseThemes and ( M <> D ) then
  begin
    FGlyphWidth := MulDiv( FGlyphWidth, M, D ) - 1;
    if FGlyphWidth <= 0 then
      FGlyphWidth := 1;

    FGlyphHeight := MulDiv( FGlyphHeight, M, D ) - 1;
    if FGlyphHeight <= 0 then
      FGlyphHeight := 1;

    FThemedGlyphWidth := FGlyphWidth;
    FThemedGlyphHeight := FGlyphHeight;
  end;
end;


procedure TRzCustomGlyphButton.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    FHotTrack := True;

    if fcpColor in FFrameControllerNotifications then
      FFillColor := FFrameController.Color;
    if fcpFrameColor in FFrameControllerNotifications then
      FFrameColor := FFrameController.FrameColor;
    if fcpFocusColor in FFrameControllerNotifications then
      FFocusColor := FFrameController.FocusColor;
    if fcpDisabledColor in FFrameControllerNotifications then
      FDisabledColor := FFrameController.DisabledColor;
    if fcpReadOnlyColor in FFrameControllerNotifications then
      FReadOnlyColor := FFrameController.ReadOnlyColor;
    if fcpReadOnlyColorOnFocus in FFrameControllerNotifications then
      FReadOnlyColorOnFocus := FFrameController.ReadOnlyColorOnFocus;
    if fcpFrameHotTrack in FFrameControllerNotifications then
    begin
      if FFrameController.FrameHotTrack then
        FHotTrackStyle := htsFrame
      else
        FHotTrackStyle := htsInterior;
    end;
    if fcpFrameHotColor in FFrameControllerNotifications then
    begin
      if FHotTrackStyle = htsFrame then
        HotTrackColor := FFrameController.FrameHotColor;
    end;
    Invalidate;
  end;
end;


function TRzCustomGlyphButton.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := taRightJustify
  else if FAlignment = taRightJustify then
    Result := taLeftJustify
  else
    Result := taRightJustify;
end;


procedure TRzCustomGlyphButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited;

  if Sender is TCustomAction then
  begin
    with TCustomAction( Sender ) do
    begin
      if not CheckDefaults or ( Self.Checked = False ) then
        Self.Checked := Checked;
    end;
  end;
end;


function TRzCustomGlyphButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TRzCheckedActionLink;
end;


function TRzCustomGlyphButton.IsCheckedStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not TRzCheckedActionLink( ActionLink ).IsCheckedLinked;
end;


function TRzCustomGlyphButton.GetChecked: Boolean;
begin
  Result := False;
end;

procedure TRzCustomGlyphButton.SetChecked( Value: Boolean );
begin
end;


procedure TRzCustomGlyphButton.ExtractGlyph( Index: Integer; Bitmap, Source: TBitmap; W, H: Integer );
var
  DestRct: TRect;
begin
  DestRct := Rect( 0, 0, W, H );

  Bitmap.Width := W;
  Bitmap.Height := H;
  Bitmap.Canvas.CopyRect( DestRct, Source.Canvas, Rect( Index * W, 0, (Index + 1 ) * W, H ) );
end;


procedure TRzCustomGlyphButton.SetAlignment( Value: TLeftRight );
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetAutoSize( Value: Boolean );
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;


procedure TRzCustomGlyphButton.SetCustomGlyphs( Value: TBitmap );
begin
  FCustomGlyphs.Assign( Value );
end;


procedure TRzCustomGlyphButton.CustomGlyphsChanged( Sender: TObject );
begin
  UpdateUseCustomGlyphs;
  Invalidate;
end;


procedure TRzCustomGlyphButton.SetCustomGlyphImages( Value: TCustomImageList );
begin
  if FCustomGlyphImages <> nil then
    FCustomGlyphImages.UnRegisterChanges( FCustomGlyphImagesChangeLink );

  FCustomGlyphImages := Value;

  if FCustomGlyphImages <> nil then
  begin
    FCustomGlyphImages.RegisterChanges( FCustomGlyphImagesChangeLink );
    FCustomGlyphImages.FreeNotification( Self );
  end;
  UpdateUseCustomGlyphs;
  Invalidate;
end;


procedure TRzCustomGlyphButton.CustomGlyphImagesChange( Sender: TObject );
begin
  if Sender = CustomGlyphImages then
  begin
    UpdateUseCustomGlyphs;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetFillColor( Value: TColor );
begin
  if FFillColor <> Value then
  begin
    FFillColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetFocusColor( Value: TColor );
begin
  if FFocusColor <> Value then
  begin
    FFocusColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomGlyphButton.UpdateUseCustomGlyphs;
begin
  // Look at the settings of FCustomGlyphImages and FCustomGlyphs to determine
  // if UseCustomGlyphs should be set to True or False.

  if ( FCustomGlyphImages <> nil ) and
     ( FCustomGlyphImages.Width > 0 ) and ( FCustomGlyphImages.Height > 0 ) and
     ( FCustomGlyphImages.Count > 0 ) then
  begin
    UseCustomGlyphs := True;
  end
  else if not FCustomGlyphs.Empty then
    UseCustomGlyphs := True
  else
    UseCustomGlyphs := False;
end;


procedure TRzCustomGlyphButton.SetUseCustomGlyphs( Value: Boolean );
begin
  // Do not check against previous value b/c assigning an image list will
  // force UseCustomGlyphs to be true and we need the FGlyphWidth and
  // FGlyphHeight values to be updated appropriately.

  FUseCustomGlyphs := Value;
  if FUseCustomGlyphs then
  begin
    if FCustomGlyphImages <> nil then
    begin
      FGlyphWidth := FCustomGlyphImages.Width;
      FGlyphHeight := FCustomGlyphImages.Height;
    end
    else
    begin
      FGlyphWidth := FCustomGlyphs.Width div FNumGlyphs;
      FGlyphHeight := FCustomGlyphs.Height;
    end;
  end
  else
  begin
    if UseThemes then
    begin
      FGlyphWidth := FThemedGlyphWidth;
      FGlyphHeight := FThemedGlyphHeight;
    end
    else
    begin
      FGlyphWidth := DefaultGlyphWidth;
      FGlyphHeight := DefaultGlyphHeight;
    end;
  end;
  if not ( csLoading in ComponentState ) then
  begin
    AutoSize := not AutoSize;
    AutoSize := not AutoSize;
  end;
  Invalidate;
end;


procedure TRzCustomGlyphButton.SetDisabledColor( Value: TColor );
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetReadOnly( Value: Boolean );
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetReadOnlyColor( Value: TColor );
begin
  if FReadOnlyColor <> Value then
  begin
    FReadOnlyColor := Value;
    Invalidate;
  end;
end;



procedure TRzCustomGlyphButton.SetTransparentColor( Value: TColor );
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetWinMaskColor( Value: TColor );
begin
  if FWinMaskColor <> Value then
  begin
    FWinMaskColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomGlyphButton.SetWordWrap( Value: Boolean );
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustBounds;
  end;
end;


procedure TRzCustomGlyphButton.RepaintDisplay;
begin
  Repaint;
end;


procedure TRzCustomGlyphButton.UpdateDisplay;
begin
  Paint;
end;


function TRzCustomGlyphButton.GetHotTrackRect: TRect;
var
  X, Y: Integer;
begin
  GetGlyphPosition( X, Y );
  Result := Rect( X, Y, X + FGlyphWidth, Y + FGlyphHeight );
end;


procedure TRzCustomGlyphButton.BlendButtonFrame( Glyph: TBitmap );
begin
  // Do nothing in base class -- override as needed in descendants
end;


procedure TRzCustomGlyphButton.GetGlyphPosition( var X, Y: Integer );
begin
  if ( FAlignment = taRightJustify ) xor UseRightToLeftAlignment then
    X := 0
  else
    X := Width - FGlyphWidth;

  Y := 0;
  case AlignmentVertical of
    avTop:
      Y := 2;

    avCenter:
      Y := ( Height - FGlyphHeight ) div 2;

    avBottom:
      Y := Height - FGlyphHeight - 2;
  end;
end;


procedure TRzCustomGlyphButton.DrawGlyph( ACanvas: TCanvas );
var
  R, SrcRect: TRect;
  FGlyph, Phase1Bmp, Phase2Bmp: TBitmap;
  X, Y: Integer;
begin
  if ACanvas = nil then
    ACanvas := Canvas;

  UpdateGlyphDimensions;

  FGlyph := TBitmap.Create;
  FGlyph.Width := FGlyphWidth;
  FGlyph.Height := FGlyphHeight;

  Phase1Bmp := TBitmap.Create;
  Phase1Bmp.Width := FGlyphWidth;
  Phase1Bmp.Height := FGlyphHeight;

  Phase2Bmp := TBitmap.Create;
  Phase2Bmp.Width := FGlyphWidth;
  Phase2Bmp.Height := FGlyphHeight;
  try
    GetGlyphPosition( X, Y );

    R := Rect( 0, 0, FGlyphWidth, FGlyphHeight );

    if FTransparent or UseThemes then
    begin
      SrcRect := Bounds( X, Y, FGlyphWidth, FGlyphHeight );
      if UseThemes and not FUseCustomGlyphs then
      begin
        FGlyph.Canvas.CopyMode := cmSrcCopy;
        FGlyph.Canvas.CopyRect( R, ACanvas, SrcRect );
        SelectGlyph( FGlyph );
        Phase1Bmp.Assign( FGlyph );
      end
      else
      begin
        Phase1Bmp.Canvas.CopyMode := cmSrcCopy;
        Phase1Bmp.Canvas.CopyRect( R, ACanvas, SrcRect );
        SelectGlyph( FGlyph );
        DrawFullTransparentBitmap( Phase1Bmp.Canvas, FGlyph, R, R, FTransparentColor );
      end;
    end
    else
    begin
      SelectGlyph( FGlyph );
      Phase1Bmp.Canvas.Brush.Color := ActiveStyleSystemColor( Color );
      Phase1Bmp.Canvas.BrushCopy( R, FGlyph, R, FTransparentColor );
    end;

    if FUseCustomGlyphs or Transparent or UseThemes then
    begin
      if FUseCustomGlyphs then
      begin
        // Replace WinMaskColor with clWindow color value
        Phase2Bmp.Canvas.Brush.Color := ActiveStyleSystemColor( clWindow );
        Phase2Bmp.Canvas.BrushCopy( R, Phase1Bmp, R, FWinMaskColor );
      end
      else
      begin
        Phase2Bmp.Assign( Phase1Bmp );
      end;

      if not UseThemes and not FUseCustomGlyphs and Transparent then
        BlendButtonFrame( Phase2Bmp );
    end
    else  // Normal, non-transparent image
    begin
      Phase2Bmp.Assign( Phase1Bmp );
    end;

    ACanvas.Draw( X, Y, Phase2Bmp );
  finally
    Phase2Bmp.Free;
    Phase1Bmp.Free;
    FGlyph.Free;
  end;
end; {= TRzCustomGlyphButton.DrawGlyph =}



procedure TRzCustomGlyphButton.Paint;
var
  R: TRect;
  W: Integer;
  MemImage: TBitmap;
  Flags: DWord;
begin
  if csDestroying in ComponentState then
    Exit;

  MemImage := TBitmap.Create;
  try
    // Make memory Bitmap same size as client rect
    MemImage.Height := Height;
    MemImage.Width := Width;

    MemImage.Canvas.Font := Font;

    // If would could access the Font settings in a VCL Style, then would
    // could change those here. They would the be used by the Draw3DText method
    // call below.


    // Paint the background

    if FTransparent then
    begin
      if not DoubleBuffered then
      begin
        FBackgroundBmp.Width := Width;
        FBackgroundBmp.Height := Height;
        DrawParentImage( Self, FBackgroundBmp.Canvas );
      end;

      MemImage.Canvas.CopyRect( ClientRect, FBackgroundBmp.Canvas, ClientRect );
    end
    else
    begin
      MemImage.Canvas.Brush.Color := ActiveStyleSystemColor( Color );
      MemImage.Canvas.FillRect( ClientRect );
    end;

    // Paint the Glyph in the correct state

    DrawGlyph( MemImage.Canvas );

    // Paint the Caption and Focus Rect if necessary

    R := ClientRect;
    InflateRect( R, -1, -1 );
    if (FAlignment = taRightJustify) xor UseRightToLeftAlignment then
      Inc( R.Left, FGlyphWidth + 4 )
    else
      Dec( R.Right, FGlyphWidth {+ 4} );

    if UseRightToLeftAlignment then
      Flags := dt_Right
    else
      Flags := 0;

    if FWordWrap then
      Flags := Flags or dt_WordBreak or dt_ExpandTabs
    else
      Flags := Flags or dt_ExpandTabs;

    Draw3DText( MemImage.Canvas, R, Flags );

    InflateRect( R, 1, 1 );
    if ShowFocus and Focused and ( Caption <> '' ) then
    begin
      W := R.Right - R.Left;

      if not UseRightToLeftAlignment then
        R.Right := R.Left + W
      else
        R.Left := R.Right - W;

      if UsingSystemStyle then
      begin
        MemImage.Canvas.DrawFocusRect( R );
      end
      else // VCL Styles
      begin
        DrawFocusBorder( MemImage.Canvas, R, ActiveStyleSystemColor( clWindowText ) );
      end;
    end;

    Canvas.CopyMode := cmSrcCopy;

    Canvas.Draw( 0, 0, MemImage );

  finally
    MemImage.Free;
  end;
end; {= TRzCustomGlyphButton.Paint =}


procedure TRzCustomGlyphButton.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;


procedure TRzCustomGlyphButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  FUsingMouse := True;
  inherited;
end;


procedure TRzCustomGlyphButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  FUsingMouse := False;
end;


procedure TRzCustomGlyphButton.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
var
  W, H: Integer;
  R: TRect;
begin
  if ( Parent <> nil ) and Parent.DoubleBuffered then
    PerformEraseBackground( Self, Msg.DC );
  DrawParentImage( Self, Msg.DC, True );

  // PaintBackground is called from ancestor class in response to wm_EraseBkgnd message.
  // This message occurs before the Paint method and therefore is a good point to get a
  // the background image. Otherwise, the background image will have artifacts from
  // previous paints.

  GetWindowRect( Handle, R );
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;
  FBackgroundBmp.Width := W;
  FBackgroundBmp.Height := H;
  BitBlt( FBackgroundBmp.Canvas.Handle, 0, 0, W, H, Msg.DC, 0, 0, cmSrcCopy );

  Msg.Result := 1;
end;


procedure TRzCustomGlyphButton.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzCustomGlyphButton.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzCustomGlyphButton.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzCustomGlyphButton.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  AdjustBounds;
end;


procedure TRzCustomGlyphButton.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  AdjustBounds;
end;



{============================}
{== TRzRadioButton Methods ==}
{============================}

constructor TRzRadioButton.Create( AOwner: TComponent );
begin
  inherited;
  AlignmentVertical := avTop;
  FChecked := False;
  FNumGlyphs := DefaultNumGlyphs_RadioButton;
  TabStop := False;
  {&RCI}
end;


destructor TRzRadioButton.Destroy;
begin
  inherited;
end;


function TRzRadioButton.GetChecked: Boolean;
begin
  Result := FChecked;
end;

procedure TRzRadioButton.SetChecked( Value: Boolean );

  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
      begin
        for I := 0 to ControlCount - 1 do
        begin
          Sibling := Controls[ I ];
          if ( Sibling <> Self ) and ( Sibling is TRzRadioButton ) then
            TRzRadioButton( Sibling ).SetChecked( False );
        end;
      end;
  end;

begin
  {&RV}
  if FChecked <> Value then
  begin
    FChecked := Value;
    TabStop := Value;
    UpdateDisplay;
    if Value then
    begin
      TurnSiblingsOff;
      if not FClicksDisabled then
        Click;
    end;
  end;
end;


procedure TRzRadioButton.SetReadOnly( Value: Boolean );

  procedure UpdateSiblings( ReadOnly: Boolean );
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
      begin
        for I := 0 to ControlCount - 1 do
        begin
          Sibling := Controls[ I ];
          if ( Sibling <> Self ) and ( Sibling is TRzRadioButton ) then
            TRzRadioButton( Sibling ).ReadOnly := ReadOnly;
        end;
      end;
  end;

begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    UpdateDisplay;
    UpdateSiblings( FReadOnly );
  end;
end;


procedure TRzRadioButton.ChangeState;
begin
  if not FChecked and not FReadOnly then
    SetChecked( True );
end;


procedure TRzRadioButton.BlendButtonFrame( Glyph: TBitmap );
begin
  Glyph.Canvas.Pixels[  4,  0 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  4,  0 ], 128 );
  Glyph.Canvas.Pixels[  8,  0 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  8,  0 ], 128 );
  Glyph.Canvas.Pixels[  2,  1 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  2,  1 ], 128 );
  Glyph.Canvas.Pixels[ 10,  1 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 10,  1 ], 128 );
  Glyph.Canvas.Pixels[  1,  2 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  1,  2 ], 128 );
  Glyph.Canvas.Pixels[ 11,  2 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 11,  2 ], 128 );
  Glyph.Canvas.Pixels[  0,  4 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  0,  4 ], 128 );
  Glyph.Canvas.Pixels[ 12,  4 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 12,  4 ], 128 );
  Glyph.Canvas.Pixels[  0,  8 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  0,  8 ], 128 );
  Glyph.Canvas.Pixels[ 12,  8 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 12,  8 ], 128 );
  Glyph.Canvas.Pixels[  1, 10 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  1, 10 ], 128 );
  Glyph.Canvas.Pixels[ 11, 10 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 11, 10 ], 128 );
  Glyph.Canvas.Pixels[  2, 11 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  2, 11 ], 128 );
  Glyph.Canvas.Pixels[ 10, 11 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 10, 11 ], 128 );
  Glyph.Canvas.Pixels[  4, 12 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  4, 12 ], 128 );
  Glyph.Canvas.Pixels[  8, 12 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  8, 12 ], 128 );

  Glyph.Canvas.Pixels[  3,  0 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  3,  0 ], 40 );
  Glyph.Canvas.Pixels[  9,  0 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  9,  0 ], 40 );
  Glyph.Canvas.Pixels[  0,  3 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  0,  3 ], 40 );
  Glyph.Canvas.Pixels[ 12,  3 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 12,  3 ], 40 );
  Glyph.Canvas.Pixels[  0,  9 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  0,  9 ], 40 );
  Glyph.Canvas.Pixels[ 12,  9 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[ 12,  9 ], 40 );
  Glyph.Canvas.Pixels[  3, 12 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  3, 12 ], 40 );
  Glyph.Canvas.Pixels[  9, 12 ] := BlendColors( FrameColor, Glyph.Canvas.Pixels[  9, 12 ], 40 );
end;



procedure TRzRadioButton.UpdateGlyphDimensions;
{$IFDEF VCL160_OR_HIGHER}
var
  Details: TThemedElementDetails;
  Size: TSize;
{$ENDIF}
begin
  {$IFDEF VCL160_OR_HIGHER}
  if not UsingSystemStyle and not UseCustomGlyphs then
  begin
    // GetElementSize does not return the correct value for a radio button.
    // In fact, Size is set to (0,0).  Therefore, if custom styles are used
    // we use the tbCheckBoxUncheckedNormal value instead, which does work

    //Details := StyleServices.GetElementDetails( tbRadioButtonUncheckedNormal );
    Details := StyleServices.GetElementDetails( tbCheckBoxUncheckedNormal );
    StyleServices.GetElementSize( Canvas.Handle, Details, esActual, Size );
    if ( FGlyphWidth <> Size.cx ) or ( FGlyphHeight <> Size.cy ) then
    begin
      FGlyphWidth := Size.cx;
      FGlyphHeight := Size.cy;
      AdjustBounds;
    end;

  end;
  {$ENDIF}
end;


procedure TRzRadioButton.SelectGlyph( Glyph: TBitmap );
var
  R: TRect;
  Flags: Integer;
  DestBmp, SourceBmp: TBitmap;
  HotTrackLightColor, HotTrackDarkColor: TColor;
  ElementDetails: TThemedElementDetails;
  DisplayState: TRzButtonDisplayState;
begin
  R := Rect( 0, 0, FGlyphWidth, FGlyphHeight );

  if not FUseCustomGlyphs then
  begin
    // Test for XP/Vista themes first...
    if UseThemes then
    begin
      if FChecked then
      begin
        if Enabled then
        begin
          if FShowDownVersion then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonCheckedPressed )
          else if FMouseOverButton then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonCheckedHot )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonCheckedNormal );
        end
        else
        begin
          ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonCheckedDisabled );
        end;
      end
      else // Unchecked
      begin
        if Enabled then
        begin
          if FShowDownVersion then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonUncheckedPressed )
          else if FMouseOverButton then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonUncheckedHot )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonUncheckedNormal );
        end
        else
        begin
          ElementDetails := ActiveStyleServices.GetElementDetails( tbRadioButtonUncheckedDisabled );
        end;
      end;

      ActiveStyleServices.DrawElement( Glyph.Canvas.Handle, ElementDetails, R );
    end
    else if HotTrack then // and No XP Themes
    begin
      if HotTrackColorType = htctComplement then
      begin
        HotTrackLightColor := ComplementaryColor( HotTrackColor, 180 );
        HotTrackDarkColor := DarkerColor( HotTrackLightColor, 30 );
      end
      else
      begin
        HotTrackDarkColor := HotTrackColor;
        HotTrackLightColor := BlendColors( clWhite, HotTrackDarkColor, 190 );
      end;

      if Enabled then
      begin
        if FShowDownVersion then
          DisplayState := bdsDown
        else if FMouseOverButton then
          DisplayState := bdsHot
        else
          DisplayState := bdsNormal;
      end
      else
        DisplayState := bdsDisabled;

      DrawRadioButton( Glyph.Canvas, R, FChecked, DisplayState, Focused, FHotTrackStyle,
                       FFrameColor, HighlightColor, FFillColor, FFocusColor, FDisabledColor,
                       HotTrackLightColor, HotTrackDarkColor,
                       Color, Transparent, clOlive, FReadOnly, FReadOnlyColorOnFocus,
                       FReadOnlyColor );
    end
    else // Default OS appearance
    begin
      if FChecked then
        Flags := dfcs_ButtonRadio or dfcs_Checked
      else
        Flags := dfcs_ButtonRadio;

      if FShowDownVersion then
        Flags := Flags or dfcs_Pushed;
      if not Enabled then
        Flags := Flags or dfcs_Inactive;

      Flags := Flags or dfcs_Transparent;

      Glyph.Canvas.Brush.Color := FTransparentColor;
      Glyph.Canvas.FillRect( R );
      DrawFrameControl( Glyph.Canvas.Handle, R, dfc_Button, Flags );
    end;
  end
  else // Using Custom Glyphs
  begin
    DestBmp := TBitmap.Create;
    try
      if FCustomGlyphImages <> nil then
      begin
        DestBmp.Width := FGlyphWidth;
        DestBmp.Height := FGlyphHeight;

        DestBmp.Canvas.Brush.Color := FTransparentColor;
        DestBmp.Canvas.FillRect( Rect( 0, 0, FGlyphWidth, FGlyphHeight ) );

        if Enabled then
        begin
          // If FCustomGlyphImages.Count has more than 6 images, then assume
          // image list contains "hot" images as well.
          if FChecked then
            if FShowDownVersion then
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 3 )
            else if FMouseOverButton and FHotTrack and ( FCustomGlyphImages.Count > 6 ) then
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 7 )
            else
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 1 )
          else
            if FShowDownVersion then
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 2 )
            else if FMouseOverButton and FHotTrack and ( FCustomGlyphImages.Count > 6 ) then
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 6 )
            else
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 0 )
        end
        else
        begin
          if FChecked then
            FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 5 )
          else
            FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 4 );
        end;
      end
      else // Use FCustomGlyphs bitmap
      begin
        SourceBmp := FCustomGlyphs;

        if Enabled then
        begin
          if FChecked then
            if FShowDownVersion then
              ExtractGlyph( 3, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
            else
              ExtractGlyph( 1, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
          else
            if FShowDownVersion then
              ExtractGlyph( 2, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
            else
              ExtractGlyph( 0, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );
        end
        else
        begin
          if FChecked then
            ExtractGlyph( 5, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
          else
            ExtractGlyph( 4, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );
        end;
      end;
      Glyph.Assign( DestBmp );
    finally
      DestBmp.Free;
    end;
  end;
end; {= TRzRadioButton.SelectGlyph =}


function TRzRadioButton.GetStyleFontColor( Enabled: Boolean ): TColor;
begin
  if Enabled then
    Result := ActiveStyleFontColor( sfRadioButtonTextNormal )
  else
    Result := ActiveStyleFontColor( sfRadioButtonTextDisabled );
end;


procedure TRzRadioButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  with Msg do
  begin
    if IsAccel( CharCode, Caption ) and CanFocus then
    begin
      SetFocus;
      Result := 1;
    end
    else
      inherited;
  end;
end;


procedure TRzRadioButton.CMDialogKey( var Msg: TCMDialogKey );
begin
  with Msg do
  begin
    if ( CharCode = vk_Down ) and ( KeyDataToShiftState( KeyData ) = [] ) and CanFocus then
    begin
      if not FClicksDisabled then
        Click;
      Result := 1;
    end
    else
      inherited;
  end;
end;


procedure TRzRadioButton.WMSetFocus( var Msg: TWMSetFocus );
begin
  inherited;
  if not FUsingMouse and not FReadOnly then
    SetChecked( True );
end;


procedure TRzRadioButton.WMLButtonDblClk( var Msg: TWMLButtonDblClk );
begin
  inherited;
  DblClick;
end;


procedure TRzRadioButton.BMGetCheck( var Msg: TMessage );
begin
  inherited;
  if FChecked then
    Msg.Result := BST_CHECKED
  else
    Msg.Result := BST_UNCHECKED;
end;



{===============================}
{== TRzCustomCheckBox Methods ==}
{===============================}

constructor TRzCustomCheckBox.Create( AOwner: TComponent );
begin
  inherited;
  AlignmentVertical := avTop;
  FNumGlyphs := DefaultNumGlyphs_CheckBox;
  FState := cbUnchecked;
  FAllowGrayed := False;
  {&RCI}
end;


destructor TRzCustomCheckBox.Destroy;
begin
  inherited;
end;


procedure TRzCustomCheckBox.ChangeState;
begin
  {&RV}
  if FUsingMouse and FReadOnly then
    Exit;
  case State of
    cbUnchecked:
      if FAllowGrayed then
        State := cbGrayed
      else
        State := cbChecked;

    cbChecked:
      State := cbUnchecked;

    cbGrayed:
      State := cbChecked;
  end;
end;


function TRzCustomCheckBox.GetChecked: Boolean;
begin
  Result := FState = cbChecked;
end;


procedure TRzCustomCheckBox.SetChecked( Value: Boolean );
begin
  if Value then
    State := cbChecked
  else
    State := cbUnchecked;
end;


procedure TRzCustomCheckBox.SetState( Value: TCheckBoxState );
begin
  if FState <> Value then
  begin
    FState := Value;
    UpdateDisplay;
    if not FClicksDisabled then
      Click;
  end;
end;


procedure TRzCustomCheckBox.InitState( Value: TCheckBoxState );
begin
  if FState <> Value then
  begin
    FState := Value;
    UpdateDisplay;
    { This method does not generate the OnClick event }
  end;
end;


procedure TRzCustomCheckBox.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if FReadOnly then
    Exit;
  if Key = vk_Escape then
  begin
    FKeyToggle := False;
    FShowDownVersion := False;
    UpdateDisplay;
  end
  else if Key = vk_Space then
  begin
    FKeyToggle := True;
    FShowDownVersion := True;
    UpdateDisplay;
  end;
end;


procedure TRzCustomCheckBox.KeyUp( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if FReadOnly then
    Exit;
  if Key = vk_Space then
  begin
    FShowDownVersion := False;
    if FKeyToggle then
      ChangeState;
    UpdateDisplay;
  end;
end;


procedure TRzCustomCheckBox.DoExit;
begin
  inherited;
  FShowDownVersion := False;
  UpdateDisplay;
end;


procedure TRzCustomCheckBox.UpdateGlyphDimensions;
{$IFDEF VCL160_OR_HIGHER}
var
  Details: TThemedElementDetails;
  Size: TSize;
{$ENDIF}
begin
  {$IFDEF VCL160_OR_HIGHER}
  if not UsingSystemStyle and not UseCustomGlyphs then
  begin
    Details := StyleServices.GetElementDetails( tbCheckBoxUncheckedNormal );
    StyleServices.GetElementSize( Canvas.Handle, Details, esActual, Size );
    if ( FGlyphWidth <> Size.cx ) or ( FGlyphHeight <> Size.cy ) then
    begin
      FGlyphWidth := Size.cx;
      FGlyphHeight := Size.cy;
      AdjustBounds;
    end;
  end;
  {$ENDIF}
end;


procedure TRzCustomCheckBox.SelectGlyph( Glyph: TBitmap );
var
  R: TRect;
  Flags: Integer;
  DestBmp, SourceBmp: TBitmap;
  HotTrackLightColor, HotTrackDarkColor: TColor;
  ElementDetails: TThemedElementDetails;
  DisplayState: TRzButtonDisplayState;
begin
  R := Rect( 0, 0, FGlyphWidth, FGlyphHeight );

  if not FUseCustomGlyphs then
  begin
    // Test for XP/Vista Themes first...
    if UseThemes then
    begin
      case FState of
        cbUnchecked:
        begin
          if Enabled then
          begin
            if FShowDownVersion then
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedPressed )
            else if FMouseOverButton then
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedNormal );
          end
          else
          begin
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedDisabled );
          end;
        end;

        cbChecked:
        begin
          if Enabled then
          begin
            if FShowDownVersion then
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedPressed )
            else if FMouseOverButton then
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedNormal );
          end
          else
          begin
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedDisabled );
          end;
        end;

        cbGrayed:
        begin
          if Enabled then
          begin
            if FShowDownVersion then
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedPressed )
            else if FMouseOverButton then
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedNormal );
          end
          else
          begin
            ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedDisabled );
          end;
        end;
      end;

      ActiveStyleServices.DrawElement( Glyph.Canvas.Handle, ElementDetails, R );
    end
    else if HotTrack then
    begin
      if HotTrackColorType = htctComplement then
      begin
        HotTrackLightColor := ComplementaryColor( HotTrackColor, 180 );
        HotTrackDarkColor := DarkerColor( HotTrackLightColor, 30 );
      end
      else
      begin
        HotTrackDarkColor := HotTrackColor;
        HotTrackLightColor := BlendColors( clWhite, HotTrackDarkColor, 190 );
      end;

      if Enabled then
      begin
        if FShowDownVersion then
          DisplayState := bdsDown
        else if FMouseOverButton then
          DisplayState := bdsHot
        else
          DisplayState := bdsNormal;
      end
      else
        DisplayState := bdsDisabled;

      DrawCheckBox( Glyph.Canvas, R, FState, DisplayState, Focused, FHotTrackStyle,
                    FFrameColor, HighlightColor, FFillColor, FFocusColor, FDisabledColor,
                    HotTrackLightColor, HotTrackDarkColor, FReadOnly,
                    FReadOnlyColorOnFocus, FReadOnlyColor );
    end
    else // Default OS appearance
    begin
      case FState of
        cbUnchecked: Flags := dfcs_ButtonCheck;
        cbChecked: Flags := dfcs_ButtonCheck or dfcs_Checked;
        cbGrayed: Flags := dfcs_Button3State or dfcs_Checked;
      else
        Flags := 0;
      end;
      if FShowDownVersion then
        Flags := Flags or dfcs_Pushed;
      if not Enabled then
        Flags := Flags or dfcs_Inactive;

      DrawFrameControl( Glyph.Canvas.Handle, R, dfc_Button, Flags );
    end;
  end
  else // Use Custom Glyphs
  begin
    DestBmp := TBitmap.Create;
    try
      if FCustomGlyphImages <> nil then
      begin
        DestBmp.Width := FGlyphWidth;
        DestBmp.Height := FGlyphHeight;

        DestBmp.Canvas.Brush.Color := FTransparentColor;
        DestBmp.Canvas.FillRect( Rect( 0, 0, FGlyphWidth, FGlyphHeight ) );

        if Enabled then
        begin
          case FState of
            cbUnchecked:
              if FShowDownVersion then
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 3 )
              else if FMouseOverButton and FHotTrack and ( FCustomGlyphImages.Count > 9 ) then
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 9 )
              else
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 0 );

            cbChecked:
              if FShowDownVersion then
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 4 )
              else if FMouseOverButton and FHotTrack and ( FCustomGlyphImages.Count > 9 ) then
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 10 )
              else
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 1 );

            cbGrayed:
              if FShowDownVersion then
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 5 )
              else if FMouseOverButton and FHotTrack and ( FCustomGlyphImages.Count > 9 ) then
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 11 )
              else
                FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 2 );
          end;
        end
        else
        begin
          case FState of
            cbUnchecked:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 6 );

            cbChecked:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 7 );

            cbGrayed:
              FCustomGlyphImages.Draw( DestBmp.Canvas, 0, 0, 8 );
          end;
        end;

      end
      else // Use FCustomGlyphs bitmap
      begin
        SourceBmp := FCustomGlyphs;

        if Enabled then
        begin
          case FState of
            cbUnchecked:
              if FShowDownVersion then
                ExtractGlyph( 3, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
              else
                ExtractGlyph( 0, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbChecked:
              if FShowDownVersion then
                ExtractGlyph( 4, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
              else
                ExtractGlyph( 1, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbGrayed:
              if FShowDownVersion then
                ExtractGlyph( 5, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight )
              else
                ExtractGlyph( 2, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );
          end;
        end
        else
        begin
          case FState of
            cbUnchecked:
              ExtractGlyph( 6, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbChecked:
              ExtractGlyph( 7, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );

            cbGrayed:
              ExtractGlyph( 8, DestBmp, SourceBmp, FGlyphWidth, FGlyphHeight );
          end;
        end;
      end;

      Glyph.Assign( DestBmp );

    finally
      DestBmp.Free;
    end;
  end;
end; {= TRzCustomCheckBox.SelectGlyph =}


function TRzCustomCheckBox.GetStyleFontColor( Enabled: Boolean ): TColor;
begin
  if Enabled then
    Result := ActiveStyleFontColor( sfCheckBoxTextNormal )
  else
    Result := ActiveStyleFontColor( sfCheckBoxTextDisabled );
end;


procedure TRzCustomCheckBox.CMDialogChar( var Msg: TCMDialogChar );
begin
  with Msg do
  begin
    if IsAccel( CharCode, Caption ) and CanFocus then
    begin
      Windows.SetFocus( Handle );
      if Focused and not FReadOnly then
      begin
        ChangeState;
        UpdateDisplay;
      end;
      Result := 1;
    end
    else
      inherited;
  end;
end;


procedure TRzCustomCheckBox.BMGetCheck( var Msg: TMessage );
begin
  case State of
    cbUnchecked: Msg.Result := BST_UNCHECKED;
    cbChecked:   Msg.Result := BST_CHECKED;
    cbGrayed:    Msg.Result := BST_INDETERMINATE;
  end;
end;


{&RUIF}
end.
