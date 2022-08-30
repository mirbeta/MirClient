{===============================================================================
  RzButton Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzCustomButton
    Custom ancestor for all Raize button components

  TRzButton
    Standard push button with custom colors, multi-lines captions, 3D text
    styles

  TRzBitBtn
    TRzButton descendant--adds ability to display a glyph

  TRzMenuButton
    Descendant of TRzBitBtn--adds ability to display a dropdown menu

  TRzToolbarButton
    Custom TSpeedButton that adds features such as HotGlyph, ShowCaption, etc.

  TRzMenuToolbarButton
    TRzToolbarButton descendant--adds ability to display a dropdown menu

  TRzToolButton
    New improved tool button that is purely custom and combines functionality of
    TRzToolbarButton and TRzMenuToolbarButton and is designed to work with
    Actions and ImageLists.

  TRzShapeButton
    This component automatically shapes a specified bitmap image into a button
    by masking out the transparent areas.


  Modification History
  ------------------------------------------------------------------------------
  6.2    (16 Jul 2015)
    * Fixed issue in TRzToolButton where caption would not be displayed in the
      correct color under certain VCL Styles.
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * Fixed issue in TRzToolButton where drop down menu would be displayed on
      the adjacent monitor under certainly situations.
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Fixed issue in TRzToolButton where the button would still show "hot" state
      even after a menu item was selected from an associated DropDownMenu, but
      only if the button's Flat property was set to False.
    * Fixed issue in TRzToolButton where the associated drop-down menu would not
      be displayed when using touch interface on a tablet.
    * Fixed issue with WPARAM and LPARAM values in TRzButton and TRzToolButton
      methods.
  ------------------------------------------------------------------------------
  6.1.5  (02 Oct 2013)
    * Fixed issue in TRzToolButton where setting Alignment to taLeftJustify or
      taRightJustify and Layout to glGlyphRight would cause the caption not to
      appear correctly.
  ------------------------------------------------------------------------------
  6.1.3  (01 May 2013)
    * Fixed hot-tracking display issue in TRzToolButton when Flat is False.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Added new Spacing property to TRzToolButton which can be used to control
  	  the amount of space between the button's glyph and caption.
    * The TRzToolButton now uses Font.Color to display drop-down arrow when
      a custom gradient is used.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed issue in TRzButton class that resulted in an EInvalidOperation
      exception if a descendant of the class was used in a component control.
    * Fixed display issue in TRzToolButton where the button would be drawn flat
      (i.e. without a border) even if the Flat property was False and the
      application was compiled without Themes.
    * Fixed display issue in TRzToolButton where the button would not appear
      pressed when a drop-down menu was displayed by the button.
    * Fixed issue in TRzToolButton where the associated drop-down menu would not
      be displayed when using pen interface on a tablet.
    * Added new DisabledImages property to the TRzToolButton component. This
      property allows a user to associate a dedicated image list to the control
      that only contains disabled images.  The benefit of this is that the same
      ImageIndex value can be used for both the Images and DisabledImages lists.
      The TRzToolButtons that have images will use the DisabledImages list if
      it is not nil *and* the button's DisabledIndex property is -1. The
      ImageIndex property will be used to index into the DisabledImages list.
    * Made necessary modifications to TRzButton, TRzBitBtn, TRzToolButton, and
      TRzControlButton to fully support VCL Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzButton and descendants to support 
      64-bit development.
  ------------------------------------------------------------------------------
  5.5.1  (31 Mar 2011)
    * Fixed issue in TRzButton and descendants that prevented '&&' in captions
      from appearing as a single '&' character.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzButton and descendant controls (e.g. TRzBitBtn and TRzMenuButton)
      have been updated such that the controls will hide any accelerator
      characters in the caption until the user presses the Alt key. Likewise,
      the controls will hide the focus rectangle until the user navigates on
      the form using the keyboard. The controls honor the Windows system setting
      that affects this behavior.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed issue where calling Free on a TRzButton while it has the focus would
      result in an access violation.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * The TRzToolButton now takes into account the Alignment property of a
      TPopupMenu that is connected to the button via the DropDownMenu property.
      That is, the menu will be aligned to the appropriate side of the button
      depending on the Alignment property.  For example, when the Alignment is
      set to paLeft, the menu is displayed along the left edge of the button.
    * The positioning of the DropDownMenu in TRzToolButton has also been updated
      to better handle displaying popup menus that are close to the edge of the
      active monitor.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed border display problem in TRzButton and descendants when the button
      is disabled at runtime.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed issue in TRzButton and descendants (include TRadioButton and
      TRzCheckBox) where the text of the control would be unreadable if the
      color of the control was close to the font color used when the control
      was disabled.
    * Modified the header file (RzButton.hpp) that gets generated by the Delphi
      compiler when compiling components for use in C++Builder. The new
      modifications allow C++Builder developers to create custom controls that
      descend from the TRzCustomButton, TRzButton, and TRzShapeButton classes
      (or from other classes that descend from these) without resulting in
      linker errors because of differences in how the HDC type is defined in
      Delphi and C++.
      NOTE:
        - When using C++Builder 2009 or later, the above modifications are not
          necessary because of changes made to the Delphi compiler. However, the
          changes are dependent on the STRICT conditional symbol being defined.
          However, C++Builder projects define the NO_STRICT symbol by default.
          Therefore, in order to compile and link descendant controls in
          C++Builder 2009 or later, the NO_STRICT symbol must be replaced with
          the STRICT symbol.
    * Registered custom property editor for the Caption property of TRzButton,
      TRzBitBtn, TRzMenuButton, and TRzToolButton.  The editor allows the
      developer to display a dialog box that can be used to enter multi-line
      captions at design-time.
    * Updated image position in TRzBitBtn and TRzMenuButton when running under
      right-to-left (RTL) systems. If the Layout property is set to blGlyphLeft
      (the default) and BiDiMode is set to bdRightToLeft, the image is
      positioned on the right side as if layout was blGlyphRight.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Updated display of disabled text in TRzButton and descendants.
    * Update the TRzControlButton (which is used by TRzButtonEdit, TRzSpinEdit,
      and other controls) such that when the button's symbol is drawn (e.g.
      drop-down arrow), if the background color is close to the symbol color,
      the symbol color is altered so that it remains visible.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Fixed display problems under Delphi 5 and 6 in which TRzCustomButton 
      descendants (i.e. TRzButton, TRzCheckBox) would not honor the Transparent
      property.
  ------------------------------------------------------------------------------
  4.1.1  (12 Jan 2007)
    * Fixed issue where transparent TRzCheckBox or TRzRadioButton controls 
      would not properly erase the previous Caption when changed in the Delphi
      2005 and earlier IDEs.
    * Fixed issue where Anchors were not being correctly applied during form
      loading which could occur when TRzToolButton instances needed to query
      their parent toolbar for style and size settings.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where TRzToolButton was not honoring design-time setting of
      Layout property. This only occurred when setting TRzToolButton.Layout
      property to blGlyphLeft (the default), and the button's
      UseToolbarButtonLayout property to False, *and* the toolbar's ButtonLayout
      property was set to something other than blGlyphLeft.
    * Added new Alignment property to TRzToolButton.
    * Fixed problem where TRzToolButton would display an offset Caption if the
      button was placed on a TRzToolbar that was connected to an ImageList, but
      the button's ImageIndex property was set to -1.
    * Fixed problem where TRzBitBtn would not show the down or exclusive glyphs
      in a bitmap assigned to the Glyph property.
    * Fixed flicker issue with TRzButton and TRzBitBtn.
    * Fixed problem where TRzToolButton.OnDropDown event would not get fired if
      the associated DropDownMenu did not have any menu items. The OnDropDown
      event can now be used to dynamically populate/update the drop down menu.
    * Remove csOpaque component style from TRzShapeButton, which was causing
      display issues under BDS 2006 SP2.
    * Fixed problem in TRzToolButton where incorrect gradient would be used to
      draw the down arrow portion of the button when pressed.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Fixed problem where setting a glyph in a TRzBitBtn may result in an range
      error.
    * Fixed problem where TRzToolButton would lose its selection when an
      assoicated drop-down menu was invoked.
    * Fixed problem where long text that word-wrapped to multiple lines in a
      TRzButton and descendants would become invisible if the client area got
      too small.
    * On systems that do not support True or High Color displays, TRzButton
      instances (and descendants) are no longer drawn with gradients.
    * Redesigned the way in which images and text are positioned on a
      TRzToolButton control. In previous versions, the image would be positioned
      at the edge of the button depending on the value of the Layout property.
      The text would then be centered in the remaining space. This approach
      sometimes led to the unwanted effect of a large gap between the text and
      the image. Starting with this version, the image and text are positioned
      next to each other and both are centered within the button's client area.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem in TRzMenuButton where hot track frame would be still
      visible if the user clicked outside the button or menu to cancel the menu.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Fixed problem where TRzToolButton components could not be set to have
      VisualStyle := vsWinXP while at the same time having the TRzToolbar that
      parented those buttons from being a different VisualStyle (e.g.vsClassic).
    * Fixed problem in TRzShapeButton where setting PreciseClick to False would
      not register mouse clicks unless the user clicked over the bitmap mask.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem in TRzButton and descendants where the pressing the space
      bar down and holding it down, and then pressing one of the arrow keys
      would change the focus but would not reset the original button to the
      up state.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzButton and
      TRzToolButton to account for changes introduced in Borland Developer
      Studio 2006.
    * Several changes were made to TRzToolButton regarding the control's user
      interface. Specifically, the ThemeAware and UseGradients properties have
      been removed. They have been replaced with the new VisualStyle property,
      which can be set to vsClassic, vsWinXP, or vsGradient.  The default is
      vsWinXP, which results in similar behavior to version 3.x.  That is, if
      XP Themes are used in an application, then the button will pick up the
      theme.  When vsGradient is selected, the coloring of the selection is
      dependent on the new GradientColorStyle property, which can be set to
      gcsSystem, gcsMSOffice, or gcsCustom.  The gcsSystem value is the default
      which results in the selection bar colors being based on the current
      system colors. When the gcsMSOffice value is specified and the XP Themes
      are present, the selection colors match those used by Microsoft Office
      products. When XP Themes are not present, the selection defaults to
      gcsSystem. The end result is that when choosing gcsSystem or gcsMSOffice,
      you will get an appropriate appearance that blends in naturally with the
      current user's color scheme.  If you load a form (created in an earlier
      verison of RC), with a button that has UseGradients set to True, the
      component will automatically set ViewStyle to vsGradient.
    * Along with the changes noted above, the TRzToolButton has a new property,
      UseToolbarVisualStyle, which greatly simplifies setting up the UI for
      the toolbar and all its buttons. This property is set to True by default,
      which means that the buttons will get their appearance settings from the
      toolbar.
    * The painting method for TRzButton has been significantly redesigned to
      provide a much smoother and cleaner look when the HotTrack style is
      specified. The new appearance has also been specifically designed to
      provide an appropriate look regardless of the Windows color scheme
      selected by the user. In particular, the hot track coloring and default
      button indicator coloring utilizes gradient colors to provide a more
      polished appearance.
    * Added new FrameColor property to TRzButton and descendants. This property
      is used when the HotTrack style is specified. The FrameColor controls the
      color of the solid frame that surrounds the button.
    * Fixed problem in TRzBitBtn that prevented a Glyph that was designed to
      be non-transparent from displaying correctly.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added code to allow accelerator characters to drop down menus of
      TRzToolButton when no OnClick event handler or Action is assigned to the
      control.
    * Published Align, Anchors, and Constraints properties in TRzToolButton.
    * When no Caption is specified for a TRzToolButton, the specified image is
      displayed in the center of the button regardless of the setting of the
      Layout property.
  ------------------------------------------------------------------------------    
  3.0.11 (12 Dec 2004)
    * Added UseGradients and ThemeAware properties to TRzToolButton.
    * Fixed alignment and position issues of DropDown arrows and menus when
      running under right-to-left RTL systems.
    * Published Align property.
    * Fixed problem where the TRzButton and descendants would keep the input
      focus when the control was disabled.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where the TRzToolButton would not display the separator line
      when in tsDropDown mode and a Standard Action was assigned to the button.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Added DropDownOnEnter property to TRzMenuButton. When True, pressing Enter
      while the button has the focus will cause the associated drop down menu to
      be displayed.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added TRzToolButton.CMDialogChar method to handle accelerator characters
      in TRzToolButton captions.
    * Added ThemeAware property to TRzButton and descendants. This allows a
      developer to use a custom colored button in an application running with
      XP themes.
    * Fixed problem in TRzToolButton where clicking on an already down
      (i.e. exclusive) button would cause an associated action to fire thus
      changing the Checked state of the action.
    * Surfaced TRzToolButton.Font and TRzToolButton.ParentFont.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where TRzToolButtons would not appear correctly in exclusive
      state while running under Windows XP Themes.
    * Added dt_RtlReading flag to DrawText function calls when
      UseRightToLeftAlignment = True.
    * Added BiDiMode and ParentBiDiMode properties to TRzToolButton.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem where TRzButton classes would always pickup parent's color.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Draw3DText now takes into account Right-To-Left locales.
    * Fixed display of TRzToolButton when in the exclusive down state.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added GetHotTrackRect virtual method to TRzCustomButton. This function is
      called when control needs to be updated b/c HotTrack = True.  The
      TRzRadioButton and TRzCheckBox override this method so that only the image
      is updated, not the caption.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomButton and TRzButton >>
    * Fixed problem where drop-down menus where not getting aligned correctly
      when control was positioned close to right edge of screen.
    * Fixed display problem when button clicked and focus taken away by an
      exception.
    * Renamed FrameFlat property to HotTrack.
    * When HotTrack is True, the button has a new appearance--the new
      HotTrackColor and HighlightColor property values are used.
    * Added XP visual style support.

    << TRzMenuButton >>
    * The drop down arrow now correctly reflects the Enabled property. That is,
      when disabled, the arrows is drawn in clBtnShadow.
    * Fixed problem where drop-down menus where not getting aligned correctly
      when control was positioned close to right edge of screen.

    << TRzMenuToolbarButton >>
    * Fixed problem where drop-down menus where not getting aligned correctly
      when control was positioned close to right edge of screen.

    << TRzToolButton >>
    * Initial release.

    << TRzShapeButton >>
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzButton;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Windows,
  SysUtils,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  ExtCtrls,
  Buttons,
  ActnList,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Consts,
  Classes,
  RzCommon;


const
  BitBtnCaptions: array[ TBitBtnKind ] of string = (
    '', SOKButton, SCancelButton, SHelpButton, SYesButton, SNoButton,
    SCloseButton, SAbortButton, SRetryButton, SIgnoreButton, SAllButton );

const
  cm_RzButtonPressed = wm_User + $2021;

type
  {=======================================}
  {== TRzCustomButton Class Declaration ==}
  {=======================================}

  TRzCustomButton = class( TCustomControl )
  private
    FAlignmentVertical: TAlignmentVertical;

    FHotTrackColor: TColor;
    FHotTrackColorType: TRzHotTrackColorType;
    FHighlightColor: TColor;

    FLightTextStyle: Boolean;
    FTextStyle: TTextStyle;
    FTextHighlightColor: TColor;
    FTextShadowColor: TColor;
    FTextShadowDepth: Integer;

    { Message Handling Methods }
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSetFocus( var Msg: TWMSetFocus ); message wm_SetFocus;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
  protected
    FAboutInfo: TRzAboutInfo;
    FDragging: Boolean;
    FHotTrack: Boolean;
    FMouseOverButton: Boolean;
    FShowDownVersion: Boolean;
    FTransparent: Boolean;
    FThemeAware: Boolean;

    procedure CreateWnd; override;

    procedure UpdateDisplay; virtual;
    procedure RepaintDisplay; virtual;
    function GetHotTrackRect: TRect; virtual;
    procedure RemoveFocus( Removing: Boolean );

    function ShowAccel: Boolean;
    function ShowFocus: Boolean;
    function UseThemes: Boolean; virtual;

    function GetStyleFontColor( Enabled: Boolean ): TColor; virtual;
    procedure Draw3DText( Canvas: TCanvas; R: TRect; Flags: DWord ); virtual;

    { Event Dispatch Methods }
    procedure Click; override;
    procedure ChangeState; virtual; abstract;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetAlignmentVertical( Value: TAlignmentVertical ); virtual;

    procedure SetHotTrack( Value: Boolean ); virtual;
    procedure SetHighlightColor( Value: TColor ); virtual;

    procedure SetLightTextStyle( Value: Boolean ); virtual;
    procedure SetTextStyle( Value: TTextStyle ); virtual;
    procedure SetTextHighlightColor( Value: TColor ); virtual;
    procedure SetTextShadowColor( Value: TColor ); virtual;
    procedure SetTextShadowDepth( Value: Integer ); virtual;
    procedure SetThemeAware( Value: Boolean ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;

    { Property Declarations }
    property AlignmentVertical: TAlignmentVertical
      read FAlignmentVertical
      write SetAlignmentVertical
      default avCenter;

    property HotTrack: Boolean
      read FHotTrack
      write SetHotTrack
      default False;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property HotTrackColor: TColor
      read FHotTrackColor
      write FHotTrackColor
      default xpHotTrackColor;

    property HotTrackColorType: TRzHotTrackColorType
      read FHotTrackColorType
      write FHotTrackColorType
      default htctActual;

    property TextHighlightColor: TColor
      read FTextHighlightColor
      write SetTextHighlightColor
      default clBtnHighlight;

    property LightTextStyle: Boolean
      read FLightTextStyle
      write SetLightTextStyle
      default False;

    property TextShadowColor: TColor
      read FTextShadowColor
      write SetTextShadowColor
      default clBtnShadow;

    property TextShadowDepth: Integer
      read FTextShadowDepth
      write SetTextShadowDepth
      default 2;

    property TextStyle: TTextStyle
      read FTextStyle
      write SetTextStyle
      default tsNormal;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default False;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;
  public
    constructor Create( AOwner: TComponent ); override;
  end;


  {=================================}
  {== TRzButton Class Declaration ==}
  {=================================}

  TRzButton = class( TRzCustomButton )
  private
    FAlignment: TAlignment;
    FClicking: Boolean;
    FDefault: Boolean;
    FCancel: Boolean;
    FActive: Boolean;
    FModalResult: TModalResult;
    FKeyToggle: Boolean;
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FGroupIndex: Integer;
    FShowDownPattern: Boolean;
    FShowFocusRect: Boolean;
    FDropDownOnEnter: Boolean;
    FFrameColor: TColor;

    procedure ReadOldFrameFlatProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMDialogKey( var Msg: TCMDialogKey ); message cm_DialogKey;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMFocusChanged( var Msg: TCMFocusChanged ); message cm_FocusChanged;
    procedure CMRzButtonPressed( var Msg: TMessage ); message cm_RzButtonPressed;
    procedure WMKillFocus( var Msg: TMessage ); message wm_KillFocus;
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
  protected
    FState: TButtonState;
    FKeyWasPressed: Boolean;

    procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure UpdateDisplay; override;
    procedure ChangeState; override;
    function GetCaptionRect: TRect; virtual;
    procedure DrawButtonContent; virtual;
    procedure CreateBrushPattern( PatternBmp: TBitmap ); virtual;
    procedure Paint; override;

    procedure UpdateExclusive;

    { Event Dispatch Methods }
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;

    { Property Access Methods }
    procedure SetAlignment( Value: TAlignment ); virtual;
    procedure SetDefault( Value: Boolean ); virtual;
    procedure SetAllowAllUp( Value: Boolean ); virtual;
    procedure SetDown( Value: Boolean ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetGroupIndex( Value: Integer ); virtual;
    procedure SetHotTrack( Value: Boolean ); override;
    procedure SetShowDownPattern( Value: Boolean ); virtual;
    procedure SetShowFocusRect( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;

    procedure Click; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Alignment: TAlignment
      read FAlignment
      write SetAlignment
      default taCenter;

    property AllowAllUp: Boolean
      read FAllowAllUp
      write SetAllowAllUp
      default False;

    property Cancel: Boolean
      read FCancel
      write FCancel
      default False;

    property Default: Boolean
      read FDefault
      write SetDefault
      default False;

    property DropDownOnEnter: Boolean
      read FDropDownOnEnter
      write FDropDownOnEnter
      default True;

    property GroupIndex: Integer
      read FGroupIndex
      write SetGroupIndex
      default 0;

    { Ensure group index is declared before Down }
    property Down: Boolean
      read FDown
      write SetDown
      default False;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      default cl3DDkShadow;

    property ModalResult: TModalResult
      read FModalResult
      write FModalResult
      default mrNone;

    property ShowDownPattern: Boolean
      read FShowDownPattern
      write SetShowDownPattern
      default True;

    property ShowFocusRect: Boolean
      read FShowFocusRect
      write SetShowFocusRect
      default True;

    { Inherited Properties & Events }
    property Action;
    property Align;
    property AlignmentVertical;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color default clBtnFace;
    property Constraints;
    property DoubleBuffered;
    property DragKind;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property HelpContext;
    property Height default 25;
    property HighlightColor;
    property Hint;
    property HotTrack;
    property HotTrackColor;
    property HotTrackColorType;
    property LightTextStyle;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TextHighlightColor;
    property TextShadowColor;
    property TextShadowDepth;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property TextStyle;
    property ThemeAware;
    property Visible;
    property Width default 75;

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


  {=================================}
  {== TRzBitBtn Class Declaration ==}
  {=================================}

  TRzBitBtn = class( TRzButton )
  private
    FGlyph: TBitmap;
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FMargin: Integer;
    FNumGlyphs: TNumGlyphs;
    FSpacing: Integer;
    FModifiedGlyph: Boolean;

    FImageIndex: TImageIndex;
    FDisabledIndex: TImageIndex;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;

    // Internal Event Handlers
    procedure GlyphChangedHandler( Sender: TObject );
    procedure ImagesChange( Sender: TObject );
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); override;

    function GetPalette: HPALETTE; override;
    function GetImageSize: TPoint; virtual;
    function GetCaptionRect: TRect; override;
    function GetGlyphRect: TRect; virtual;
    procedure DrawImage( R: TRect ); virtual;
    procedure DrawGlyph( R: TRect ); virtual;
    procedure DrawButtonContent; override;
    procedure ChangeScale( M, D: Integer ); override;

    function IsCustom: Boolean;
    function IsCustomCaption: Boolean;

    { Property Access Methods }
    procedure SetGlyph( Value: TBitmap ); virtual;
    function GetKind: TBitBtnKind; virtual;
    procedure SetKind( Value: TBitBtnKind ); virtual;
    procedure SetLayout( Value: TButtonLayout ); virtual;
    procedure SetMargin( Value: Integer ); virtual;
    procedure SetNumGlyphs( Value: TNumGlyphs ); virtual;
    procedure SetSpacing( Value: Integer ); virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;
    procedure SetDisabledIndex( Value: TImageIndex ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property Cancel
      stored IsCustom;

    property Caption
      stored IsCustomCaption;

    property Default
      stored IsCustom;

    property Glyph: TBitmap
      read FGlyph
      write SetGlyph
      stored IsCustom;

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

    property Kind: TBitBtnKind
      read GetKind
      write SetKind
      default bkCustom;

    property Layout: TButtonLayout
      read FLayout
      write SetLayout
      default blGlyphLeft;

    property Margin: Integer
      read FMargin
      write SetMargin
      default 2;

    property ModalResult
      stored IsCustom;

    property NumGlyphs: TNumGlyphs
      read FNumGlyphs
      write SetNumGlyphs
      stored IsCustom
      default 1;

    property Spacing: Integer
      read FSpacing
      write SetSpacing
      default 4;
  end;


  {=====================================}
  {== TRzMenuButton Class Declaration ==}
  {=====================================}

  TRzMenuButton = class( TRzBitBtn )
  private
    FDropped: Boolean;
    FDropDownMenu: TPopupMenu;
    FDropTime: DWord;
    FSkipNextClick: Boolean;
    FShowArrow: Boolean;
    FOnDropDown: TNotifyEvent;

    procedure WMKeyDown( var Msg: TWMKeyDown ); message wm_KeyDown;
  protected
    function GetCaptionRect: TRect; override;
    function GetGlyphRect: TRect; override;
    procedure Paint; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure DoDropDown; virtual;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure DropDown; dynamic;

    { Property Access Methods }
    procedure SetDropDownMenu( Value: TPopupMenu );
    procedure SetShowArrow( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure Click; override;
  published
    property DropDownMenu: TPopupMenu
      read FDropDownMenu
      write SetDropDownMenu;

    property ShowArrow: Boolean
      read FShowArrow
      write SetShowArrow
      default True;

    property OnDropDown: TNotifyEvent
      read FOnDropDown
      write FOnDropDown;

    { Inherited Properties & Events }
    property Width default 110;
  end;


  {========================================}
  {== TRzToolbarButton Class Declaration ==}
  {========================================}

  TRzToolbarButton = class;

  TRzToolbarButtonActionLink = class( TSpeedButtonActionLink )
  protected
    function IsCaptionLinked: Boolean; override;
  end;

  TRzToolbarButtonActionLinkClass = class of TRzToolbarButtonActionLink;

  TRzToolbarButton = class( TSpeedButton )
  private
    FAboutInfo: TRzAboutInfo;
    FChangingGlyph: Boolean;
    FHotGlyph: TBitmap;
    FHotNumGlyphs: TNumGlyphs;
    FUseHotGlyph: Boolean;
    FStdGlyph: TBitmap;
    FStdNumGlyphs: TNumGlyphs;
    FIgnoreActionCaption: Boolean;
    FSaveCaption: TCaption;
    FShowCaption: Boolean;

    procedure ReadSaveCaption( Reader: TReader );
    procedure WriteSaveCaption( Writer: TWriter );

    { Message Handling Methods }
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;

    { Internal Event Handlers }
    procedure HotGlyphChangedHandler( Sender: TObject );
  protected
    FMouseOverControl: Boolean;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); override;
    function GetActionLinkClass: TControlActionLinkClass; override;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetHotNumGlyphs( Value: TNumGlyphs ); virtual;
    procedure SetHotGlyph( Value: TBitmap ); virtual;
    function GetCaption: TCaption; virtual;
    procedure SetCaption( const Value: TCaption ); virtual;
    procedure SetShowCaption( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property HotGlyph: TBitmap
      read FHotGlyph
      write SetHotGlyph;

    property HotNumGlyphs: TNumGlyphs
      read FHotNumGlyphs
      write SetHotNumGlyphs
      default 1;

    property IgnoreActionCaption: Boolean
      read FIgnoreActionCaption
      write FIgnoreActionCaption
      default False;

    property Caption: TCaption
      read GetCaption
      write SetCaption;

    property ShowCaption: Boolean
      read FShowCaption
      write SetShowCaption
      default True;

    property UseHotGlyph: Boolean
      read FUseHotGlyph
      write FUseHotGlyph
      default False;

    property Flat default True;
  end;


  {============================================}
  {== TRzMenuToolbarButton Class Declaration ==}
  {============================================}

  TRzMenuToolbarButton = class( TRzToolbarButton )
  private
    FDropped: Boolean;
    FDropDownMenu: TPopupMenu;
    FDropTime: DWord;
    FSkipNextClick: Boolean;
    FShowArrow: Boolean;
    FOnDropDown: TNotifyEvent;
  protected
    procedure Paint; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure DropDown; dynamic;

    { Property Access Methods }
    procedure SetDropDownMenu( Value: TPopupMenu );
    procedure SetShowArrow( Value: Boolean );
  public
    constructor Create( AOwner: TComponent ); override;
    procedure Click; override;
    procedure DoDropDown; virtual;
  published
    property DropDownMenu: TPopupMenu
      read FDropDownMenu
      write SetDropDownMenu;

    property ShowArrow: Boolean
      read FShowArrow
      write SetShowArrow
      default True;

    property OnDropDown: TNotifyEvent
      read FOnDropDown
      write FOnDropDown;

    { Inherited Properties & Events }
    property DragMode default dmManual;
    property Margin default 2;
    property Width default 40;
  end;


  {===============================================}
  {== TRzToolButtonActionLink Class Declaration ==}
  {===============================================}

  TRzToolButton = class;

  TRzToolButtonActionLink = class( TControlActionLink )
  protected
    FClient: TRzToolButton;
    procedure AssignClient( AClient: TObject ); override;
    function IsCheckedLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked( Value: Boolean ); override;
    procedure SetImageIndex( Value: Integer ); override;
  end;

  TRzToolButtonActionLinkClass = class of TRzToolButtonActionLink;

  TRzToolButtonState = ( tbsUp, tbsDisabled, tbsDown, tbsExclusive, tbsDropDown );
  TRzToolStyle = ( tsButton, tsDropDown );

  {=====================================}
  {== TRzToolButton Class Declaration ==}
  {=====================================}

  TRzToolButton = class( TGraphicControl )
  private
    FAboutInfo: TRzAboutInfo;
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FDragging: Boolean;
    FFlat: Boolean;
    FGroupIndex: Integer;
    FImageIndex: TImageIndex;
    FDownIndex: TImageIndex;
    FDisabledIndex: TImageIndex;
    FHotIndex: TImageIndex;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    FDisabledImagesChangeLink: TChangeLink;
    FLayout: TButtonLayout;
    FMouseOverButton: Boolean;
    FDropDownMenu: TPopupMenu;
    FDropTime: DWord;
    FTreatAsNormal: Boolean;
    FToolStyle: TRzToolStyle;
    FShowCaption: Boolean;
    FSpacing: Integer;
    FTransparent: Boolean;
    FInDoDropDown: Boolean;
    FAlignment: TAlignment;

    FVisualStyle: TRzVisualStyle;
    FGradientColorStyle: TRzGradientColorStyle;
    FSelectionColorStart: TColor;
    FSelectionColorStop: TColor;
    FSelectionFrameColor: TColor;

    FUseToolbarButtonLayout: Boolean;
    FUseToolbarButtonSize: Boolean;
    FUseToolbarShowCaption: Boolean;
    FUseToolbarVisualStyle: Boolean;

    FOnDropDown: TNotifyEvent;

    function IsCheckedStored: Boolean;
    function IsImageIndexStored: Boolean;
    procedure UpdateExclusive;
    procedure UpdateTracking;

    procedure ReadOldGradientColorStartProp( Reader: TReader );
    procedure ReadOldGradientColorStopProp( Reader: TReader );
    procedure ReadOldFrameColorProp( Reader: TReader );
    procedure ReadOldUseGradientsProp( Reader: TReader );

    // Internal Event Handlers
    procedure ImagesChange( Sender: TObject );
    procedure DisabledImagesChange( Sender: TObject );

    // Message Handling Methods
    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMButtonPressed( var Msg: TMessage ); message cm_ButtonPressed;
    procedure CMToolbarShowCaptionChanged( var Msg: TMessage ); message cm_ToolbarShowCaptionChanged;
    procedure CMToolbarButtonLayoutChanged( var Msg: TMessage ); message cm_ToolbarButtonLayoutChanged;
    procedure CMToolbarButtonSizeChanged( var Msg: TMessage ); message cm_ToolbarButtonSizeChanged;
    procedure CMToolbarVisualStyleChanged( var Msg: TMessage ); message cm_ToolbarVisualStyleChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
  protected
    FState: TRzToolButtonState;

    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure PickupToolbarStyles; virtual;
    procedure Notification( AComponent : TComponent; Operation : TOperation ); override;
    procedure DrawBtnBorder( var R: TRect ); virtual;
    procedure DrawImage( R: TRect ); virtual;
    procedure DrawArrow; virtual;
    procedure DrawCaption( R: TRect ); virtual;
    procedure Paint; override;

    procedure DoDropDown; virtual;

    function GetImageSize: TPoint; virtual;
    procedure GetImageAndCaptionRects( var ImageRect, CaptionRect: TRect ); virtual;
    procedure CheckMinSize;
    function CursorPosition: TPoint;

    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); override;
    function GetActionLinkClass: TControlActionLinkClass; override;

    procedure AssignTo( Dest: TPersistent ); override;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure DropDown; dynamic;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetAllowAllUp( Value: Boolean ); virtual;
    procedure SetAlignment( Value: TAlignment ); virtual;
    procedure SetGradientColorStyle( Value: TRzGradientColorStyle ); virtual;
    procedure SetSelectionColorStart( Value: TColor ); virtual;
    procedure SetSelectionColorStop( Value: TColor ); virtual;
    procedure SetSelectionFrameColor( Value: TColor ); virtual;
    procedure SetDown( Value: Boolean ); virtual;
    procedure SetDropDownMenu( Value: TPopupMenu ); virtual;
    procedure SetFlat( Value: Boolean ); virtual;
    procedure SetGroupIndex( Value: Integer ); virtual;
    procedure SetHotIndex( Value: TImageIndex ); virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;
    procedure SetDownIndex( Value: TImageIndex ); virtual;
    procedure SetDisabledIndex( Value: TImageIndex ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;
    procedure SetDisabledImages( Value: TCustomImageList ); virtual;
    function IsLayoutStored: Boolean;
    procedure SetUseToolbarButtonLayout( Value: Boolean ); virtual;
    procedure SetLayout( Value: TButtonLayout ); virtual;
    procedure SetParent( Value: TWinControl ); override;
    function IsSizeStored: Boolean;
    procedure SetUseToolbarButtonSize( Value: Boolean ); virtual;
    function GetWidth: Integer; virtual;
    procedure SetWidth( Value: Integer ); virtual;
    function GetHeight: Integer; virtual;
    procedure SetHeight( Value: Integer ); virtual;
    function IsShowCaptionStored: Boolean;
    procedure SetUseToolbarShowCaption( Value: Boolean ); virtual;
    procedure SetShowCaption( Value: Boolean ); virtual;
    procedure SetSpacing( Value: Integer ); virtual;
    procedure SetToolStyle( Value: TRzToolStyle ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;
    function IsVisualStyleStored: Boolean;
    procedure SetUseToolbarVisualStyle( Value: Boolean ); virtual;
    procedure SetVisualStyle( Value: TRzVisualStyle ); virtual;

    property MouseOverButton: Boolean
      read FMouseOverButton;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function ImageList: TCustomImageList;
    function DisabledImageList: TCustomImageList;

    procedure Click; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AllowAllUp: Boolean
      read FAllowAllUp
      write SetAllowAllUp
      default False;

    property Alignment: TAlignment
      read FAlignment
      write SetAlignment
      default taCenter;

    property DisabledIndex: TImageIndex
      read FDisabledIndex
      write SetDisabledIndex
      default -1;

    property GradientColorStyle: TRzGradientColorStyle
      read FGradientColorStyle
      write SetGradientColorStyle
      stored IsVisualStyleStored;

    property SelectionColorStart: TColor
      read FSelectionColorStart
      write SetSelectionColorStart
      default clBtnFace;

    property SelectionColorStop: TColor
      read FSelectionColorStop
      write SetSelectionColorStop
      default clBtnShadow;

    property SelectionFrameColor: TColor
      read FSelectionFrameColor
      write SetSelectionFrameColor
      default cl3DDkShadow;


    // GroupIndex must be before Down
    property GroupIndex: Integer
      read FGroupIndex
      write SetGroupIndex
      default 0;

    property Down: Boolean
      read FDown
      write SetDown
      stored IsCheckedStored
      default False;

    property DownIndex: TImageIndex
      read FDownIndex
      write SetDownIndex
      default -1;

    property DropDownMenu: TPopupMenu
      read FDropDownMenu
      write SetDropDownMenu;

    property Flat: Boolean
      read FFlat
      write SetFlat
      default True;

    property Height: Integer
      read GetHeight
      write SetHeight
      stored IsSizeStored
      default 25;

    property HotIndex: TImageIndex
      read FHotIndex
      write SetHotIndex
      default -1;

    property ImageIndex: TImageIndex
      read FImageIndex
      write SetImageIndex
      stored IsImageIndexStored
      default -1;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property DisabledImages: TCustomImageList
      read FDisabledImages
      write SetDisabledImages;

    property Layout: TButtonLayout
      read FLayout
      write SetLayout
      default blGlyphLeft;

    property ShowCaption: Boolean
      read FShowCaption
      write SetShowCaption
      stored IsShowCaptionStored;

    property Spacing: Integer
      read FSpacing
      write SetSpacing
      default 4;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default True;

    property UseToolbarButtonLayout: Boolean
      read FUseToolbarButtonLayout
      write SetUseToolbarButtonLayout
      default True;

    property UseToolbarButtonSize: Boolean
      read FUseToolbarButtonSize
      write SetUseToolbarButtonSize
      default True;

    property UseToolbarShowCaption: Boolean
      read FUseToolbarShowCaption
      write SetUseToolbarShowCaption
      default True;

    property UseToolbarVisualStyle: Boolean
      read FUseToolbarVisualStyle
      write SetUseToolbarVisualStyle
      default True;

    property ToolStyle: TRzToolStyle
      read FToolStyle
      write SetToolStyle
      default tsButton;

    property VisualStyle: TRzVisualStyle
      read FVisualStyle
      write SetVisualStyle
      stored IsVisualStyleStored;

    property Width: Integer
      read GetWidth
      write SetWidth
      stored IsSizeStored
      default 25;

    property OnDropDown: TNotifyEvent
      read FOnDropDown
      write FOnDropDown;

    { Inherited Properties & Events }
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color default clBtnFace;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode default dmManual;
    property Enabled;
    property Font;
    property Hint;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  {========================================}
  {== TRzControlButton Class Declaration ==}
  {========================================}

  TRzControlButtonStyle = ( cbsNone, cbsLeft, cbsUp, cbsRight, cbsDown, cbsDropDown );

  TRzControlButton = class( TGraphicControl )
  private
    FDown: Boolean;
    FDragging: Boolean;
    FFlat: Boolean;
    FMouseOverButton: Boolean;
    FGlyph: TBitmap;
    FNumGlyphs: TNumGlyphs;

    FRepeatClicks: Boolean;
    FRepeatTimer: TTimer;
    FInitialDelay: Word;
    FDelay: Word;
    FStyle: TRzControlButtonStyle;

    procedure UpdateTracking;

    { Internal Event Handlers }
    procedure GlyphChangedHandler( Sender: TObject );
    procedure TimerExpired( Sender: TObject );

    { Message Handling Methods }
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
  protected
    procedure DrawBtnFace( var R: TRect ); virtual;
    procedure DrawSpinButton( var R: TRect ); virtual;
    procedure DrawDropDownButton( var R: TRect ); virtual;
    procedure DrawGlyph( R: TRect ); virtual;
    procedure Paint; override;

    function GetImageSize: TPoint; virtual;
    function GetPalette: HPALETTE; override;

    function CursorPosition: TPoint;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetFlat( Value: Boolean ); virtual;
    procedure SetGlyph( Value: TBitmap ); virtual;
    procedure SetNumGlyphs( Value: TNumGlyphs ); virtual;
    procedure SetStyle( Value: TRzControlButtonStyle ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Click; override;
  published
    property Flat: Boolean
      read FFlat
      write SetFlat
      default False;

    property Glyph: TBitmap
      read FGlyph
      write SetGlyph;

    property NumGlyphs: TNumGlyphs
      read FNumGlyphs
      write SetNumGlyphs
      default 1;

    property Delay: Word
      read FDelay
      write FDelay
      default 100;

    property InitialDelay: Word
      read FInitialDelay
      write FInitialDelay
      default 400;

    property RepeatClicks: Boolean
      read FRepeatClicks
      write FRepeatClicks
      default False;

    property Style: TRzControlButtonStyle
      read FStyle
      write SetStyle
      default cbsNone;

    { Inherited Properties & Events }
    property Action;
    property Caption;
    property Color default clBtnFace;
    property DragCursor;
    property DragKind;
    property DragMode default dmManual;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


  {======================================}
  {== TRzShapeButton Class Declaration ==}
  {======================================}

  TRzBevelWidth = 0..2;
  TRzCaptionPosition = ( cpCentered, cpXY );

  TRzShapeButton = class( TGraphicControl )
  private
    FAboutInfo: TRzAboutInfo;
    FAutoSize: Boolean;
    FBevelWidth: TRzBevelWidth;
    FBevelHighlightColor: TColor;
    FBevelShadowColor: TColor;
    FBitmap: TBitmap;
    FBitmapUp: TBitmap;
    FBitmapDown: TBitmap;
    FBorderStyle: TBorderStyle;
    FBorderColor: TColor;
    FDragging: Boolean;
    FHitTestMask: TBitmap;
    FPrevCursorSaved: Boolean;
    FPrevCursor: TCursor;
    FPrevShowHintSaved: Boolean;
    FPrevShowHint: Boolean;
    FPrevParentShowHint: Boolean;
    FPreciseClick: Boolean;
    FPreciseShowHint: Boolean;
    FCaptionPosition: TRzCaptionPosition;
    FCaptionX: Integer;
    FCaptionY: Integer;

    procedure AdjustBounds;
    procedure AdjustButtonSize( var W, H: Integer );
    function BevelColor( const AState: TButtonState; const TopLeft: Boolean ): TColor;
    procedure BitmapChanged( Sender: TObject );
    procedure Create3DBitmap( Source: TBitmap; const AState: TButtonState; Target: TBitmap );

    // BCB Header Translation Required for HDC
    procedure InitPalette( DC: HDC );

    procedure CMDialogChar( var Msg: TCMDialogChar ); message cm_DialogChar;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMSysColorChange( var Msg: TMessage ); message cm_SysColorChange;
    procedure CMHitTest( var Msg: TCMHitTest ); message cm_HitTest;
  protected
    FState: TButtonState;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure DrawButtonText( Canvas: TCanvas; const Caption: string;
                              TextBounds: TRect; State: TButtonState ); virtual;
    function GetPalette: HPALETTE; override;
    function GetCaptionRect( Canvas: TCanvas; const Caption: string ): TRect; virtual;

    procedure Loaded; override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure Paint; override;

    procedure ReadBitmapUpData( Stream: TStream ); virtual;
    procedure WriteBitmapUpData( Stream: TStream ); virtual;
    procedure ReadBitmapDownData( Stream: TStream ); virtual;
    procedure WriteBitmapDownData( Stream: TStream ); virtual;

    { Property Access Methods }
    procedure SetAutoSize( Value: Boolean ); override;
    procedure SetBevelHighlightColor( Value: TColor ); virtual;
    procedure SetBevelShadowColor( Value: TColor ); virtual;
    procedure SetBevelWidth( Value: TRzBevelWidth ); virtual;
    procedure SetBitmap( Value: TBitmap ); virtual;
    procedure SetBitmapDown( Value: TBitmap ); virtual;
    procedure SetBitmapUp( Value: TBitmap ); virtual;
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderStyle( Value: TBorderStyle ); virtual;
    procedure SetCaptionPosition( Value: TRzCaptionPosition ); virtual;
    procedure SetCaptionX( Value: Integer ); virtual;
    procedure SetCaptionY( Value: Integer ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Click; override;
    procedure Invalidate; override;
    function PtInMask( const X, Y: Integer ): Boolean; virtual;
    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;
    procedure SetCaptionXY( const X, Y: Integer ); virtual;

    property BitmapUp: TBitmap
      read FBitmapUp;

    property BitmapDown: TBitmap
      read FBitmapDown;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AutoSize: Boolean
      read FAutoSize
      write SetAutoSize
      default True;

    property BevelHighlightColor: TColor
      read FBevelHighlightColor
      write SetBevelHighlightColor
      default clBtnHighlight;

    property BevelShadowColor: TColor
      read FBevelShadowColor
      write SetBevelShadowColor
      default clBtnShadow;

    property BevelWidth: TRzBevelWidth
      read FBevelWidth
      write SetBevelWidth
      default 2;

    property Bitmap: TBitmap
      read FBitmap
      write SetBitmap;

    // BorderStyle should be bsSingle to get the best effect between
    // the button's up and down 3D images.  However, it can be changed
    // without any negative side-effects.
    property BorderStyle: TBorderStyle
      read FBorderStyle
      write SetBorderStyle
      default bsSingle;

    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      default cl3DDkShadow;

    property PreciseClick: Boolean
      read FPreciseClick
      write FPreciseClick
      default True;

    property PreciseShowHint: Boolean
      read FPreciseShowHint
      write FPreciseShowHint
      default True;

    property CaptionPosition: TRzCaptionPosition
      read FCaptionPosition
      write SetCaptionPosition
      default cpCentered;

    property CaptionX: Integer
      read FCaptionX
      write SetCaptionX
      default 0;

    property CaptionY: Integer
      read FCaptionY
      write SetCaptionY
      default 0;

    { Inherited Properties & Events }
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;



function GetBitBtnGlyph( Kind: TBitBtnKind ): TBitmap;

implementation

uses
  {&RAS}
  Themes,
  RzPanel,
  RzGrafx,
  RzCommonBitmaps,
  RzSpnEdt,
  CommCtrl;

const
  ArrowRegionWidth = 14;
  MinDelay = 100;
  { This value represents an upper limit for the amount of time it takes
    for the MouseDown method to finish (after displaying the drop down menu)
    and the time it takes for the next MouseDown method to start executing.
    Unfortunately, this value will be processor dependent.  That is, the
    faster the processor, the less this delay needs to be. But, we'll just
    set this to a upper limit. }

const
  BitBtnResNames: array[ TBitBtnKind ] of PChar = ( nil, 'RZCOMMON_OK', 'RZCOMMON_CANCEL', 'RZCOMMON_HELP',
                                                    'RZCOMMON_YES', 'RZCOMMON_NO', 'RZCOMMON_CLOSE', 'RZCOMMON_ABORT',
                                                    'RZCOMMON_RETRY', 'RZCOMMON_IGNORE', 'RZCOMMON_ALL');

  BitBtnModalResults: array[ TBitBtnKind ] of TModalResult = ( 0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort,
                                                               mrRetry, mrIgnore, mrAll );

var
  BitBtnGlyphs: array[ TBitBtnKind ] of TBitmap;


function GetBitBtnGlyph( Kind: TBitBtnKind ): TBitmap;
begin
  if BitBtnGlyphs[ Kind ] = nil then
  begin
    BitBtnGlyphs[ Kind ] := TBitmap.Create;
    BitBtnGlyphs[ Kind ].LoadFromResourceName( HInstance, BitBtnResNames[ Kind ] );
  end;
  Result := BitBtnGlyphs[ Kind ];
end;


function GetMenuWidth( Control: TControl; DropDownMenu: TPopupMenu ): Integer;
var
  Canvas: TControlCanvas;
  W, I: Integer;
begin
  Canvas := TControlCanvas.Create;
  Canvas.Control := Control;
  try
    Canvas.Font := Screen.MenuFont;
    Result := 0;
    for I := 0 to DropDownMenu.Items.Count - 1 do
    begin
      W := Canvas.TextWidth( DropDownMenu.Items[ I ].Caption );
      if W > Result then
        Result := W;
    end;
    Result := Result + 56;
  finally
    Canvas.Free;
  end;
end;



{&RT}
{=============================}
{== TRzCustomButton Methods ==}
{=============================}

constructor TRzCustomButton.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := [ csCaptureMouse, csSetCaption, csDoubleClicks, csOpaque,
                    csReplicatable, csReflector ];

  FTransparent := False;

  Height := 17;
  Width := 115;
  TabStop := True;

  FAlignmentVertical := avCenter;

  FDragging := False;
  FMouseOverButton := False;

  FHotTrack := False;
  FHighlightColor := clHighlight;
  FHotTrackColor := xpHotTrackColor;
  FHotTrackColorType := htctActual;

  FLightTextStyle := False;
  FTextStyle := tsNormal;
  FTextShadowDepth := 2;
  FTextShadowColor := clBtnShadow;
  FTextHighlightColor := clBtnHighlight;

  FThemeAware := True;
  {&RCI}
end;


procedure TRzCustomButton.CreateWnd;
begin
  inherited;
  if RunningAtLeast( win2000 ) then
    Perform( wm_ChangeUIState, MakeWParam( UIS_INITIALIZE, UISF_HIDEACCEL or UISF_HIDEFOCUS ), 0 );
end;


procedure TRzCustomButton.SetAlignmentVertical( Value: TAlignmentVertical );
begin
  if FAlignmentVertical <> Value then
  begin
    FAlignmentVertical := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetHotTrack( Value: Boolean );
begin
  if FHotTrack <> Value then
  begin
    FHotTrack := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetTextHighlightColor( Value: TColor );
begin
  if FTextHighlightColor <> Value then
  begin
    FTextHighlightColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetTextShadowColor( Value: TColor );
begin
  if FTextShadowColor <> Value then
  begin
    FTextShadowColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetTextShadowDepth( Value: Integer );
begin
  if FTextShadowDepth <> Value then
  begin
    FTextShadowDepth := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetLightTextStyle( Value: Boolean );
begin
  if FLightTextStyle <> Value then
  begin
    FLightTextStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetTextStyle( Value: TTextStyle );
begin
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    Invalidate;
  end;
end;


function TRzCustomButton.ShowAccel: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEACCEL ) = 0;
end;


function TRzCustomButton.ShowFocus: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEFOCUS ) = 0;
end;


function TRzCustomButton.UseThemes: Boolean;
begin
  Result := FThemeAware and ActiveStyleServicesEnabled;
end;


procedure TRzCustomButton.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.SetTransparent( Value: Boolean );
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;


procedure TRzCustomButton.Click;
begin
  inherited;
  {&RV}
end;


// Process wm_SetFocus and wm_KillFocus messages instead of overriding DoEnter
// and DoExit because the window messages are correctly sent when the form is
// activated and deactivated

procedure TRzCustomButton.WMSetFocus( var Msg: TWMSetFocus );
begin
  inherited;
  // When control gets focus, update display to show focus border
  RepaintDisplay;
end;

procedure TRzCustomButton.WMKillFocus( var Msg: TWMKillFocus );
begin
  inherited;
  // When control loses focus, update display to remove focus border
  RepaintDisplay;
end;


procedure TRzCustomButton.RepaintDisplay;
begin
  Repaint;
end;


procedure TRzCustomButton.UpdateDisplay;
begin
  // Overridden in descendant classes
end;


function TRzCustomButton.GetHotTrackRect: TRect;
begin
  Result := ClientRect;
end;


function TRzCustomButton.GetStyleFontColor( Enabled: Boolean ): TColor;
begin
  if Enabled then
    Result := ActiveStyleFontColor( sfButtonTextNormal )
  else
    Result := ActiveStyleFontColor( sfButtonTextDisabled );
end;


procedure TRzCustomButton.Draw3DText( Canvas: TCanvas; R: TRect; Flags: DWord );
var
  TempRct: TRect;
  ULColor, LRColor: TColor;
  H: Integer;
  S: string;
begin
  Canvas.Brush.Style := bsClear;

  // For some reason, under WinXP with Visual Styles, if a button is placed on
  // a panel, the font always appears as the System font. Although the
  // Canvas.Font object says that it is not.  We change the name of the font
  // here to and then reset the font back to the control's font.
  Canvas.Font.Name := 'System';
  Canvas.Font := Self.Font;
  TempRct := R;

  if ShowAccel then
    S := Caption
  else
  begin
    S := RemoveAccelerators( Caption );
    Flags := Flags or dt_NoPrefix;
  end;


  H := DrawString( Canvas, S, TempRct, dt_CalcRect or dt_WordBreak or dt_ExpandTabs or dt_VCenter or dt_Center );

  case FAlignmentVertical of
    avTop:
      R.Bottom := R.Top + H;

    avCenter:
    begin
      R.Top := ( ( R.Bottom + R.Top ) - H ) div 2;
      R.Bottom := R.Top + H;
    end;

    avBottom:
      R.Top := R.Bottom - H - 1;
  end;
  TempRct := R;

  if UseRightToLeftAlignment then
    Flags := Flags or dt_RtlReading;

  if UsingSystemStyle then
  begin
    if Enabled then
    begin
      if FTextStyle in [ tsRecessed, tsRaised ] then
      begin
        if FTextStyle = tsRaised then
        begin
          ULColor := FTextHighlightColor;
          LRColor := FTextShadowColor;
        end
        else
        begin
          ULColor := FTextShadowColor;
          LRColor := FTextHighlightColor;
        end;

        if ( FTextStyle = tsRecessed ) or not FLightTextStyle then
        begin
          OffsetRect( TempRct, 1, 1 );
          Canvas.Font.Color := LRColor;
          DrawString( Canvas, S, TempRct, Flags );
        end;

        if ( FTextStyle = tsRaised ) or not FLightTextStyle then
        begin
          TempRct := R;
          OffsetRect( TempRct, -1, -1 );
          Canvas.Font.Color := ULColor;
          DrawString( Canvas, S, TempRct, Flags );
        end;
      end
      else if FTextStyle = tsShadow then
      begin
        if ( Flags and dt_Right ) = dt_Right then
          OffsetRect( TempRct, 0, FTextShadowDepth )
        else
          OffsetRect( TempRct, FTextShadowDepth, FTextShadowDepth );
        Canvas.Font.Color := FTextShadowColor;
        DrawString( Canvas, S, TempRct, Flags );
      end;

      Canvas.Font.Color := Self.Font.Color;
      if ( Flags and dt_Right ) = dt_Right then
        OffsetRect( R, -FTextShadowDepth, 0 );
    end
    else { if not Enabled }
    begin
      if ColorsTooClose( Color, clGrayText ) then
        Canvas.Font.Color := cl3DDkShadow
      else
        Canvas.Font.Color := clGrayText;
    end;
  end
  else // VCL Styles
  begin
    Canvas.Font.Color := GetStyleFontColor( Enabled );
    (*
    // Currently, StyleServices.DrawText does not utilize the font name, size,
    // or style defined for the elements in a style. If this changes, then
    // StyleServices.DrawText could be used to display the text appropriately.
    // However, an alternative would be to access the font settings from the
    // Style and still use DrawString below.
    //
    // StyleServices.DrawText( Canvas.Handle, StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal), S, R, Flags, 0 );
    *)
  end;
  DrawString( Canvas, S, R, Flags );

  Canvas.Brush.Style := bsSolid;
end; {= TRzCustomButton.Draw3DText =}


procedure TRzCustomButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if ( Button = mbLeft ) and Enabled then
  begin
    // Cannot call SetFocus method b/c if the control is active, it will not change the focus back to this control.
    // This can happen if a dialog is displayed as a result of the clicking the button and the button is disabled.
    Windows.SetFocus( Handle );

    if Focused then
    begin
      FShowDownVersion := True;
      UpdateDisplay;
      FDragging := True;
    end;
  end;
end;


procedure TRzCustomButton.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  NewState: Boolean;
begin
  inherited;

  if FDragging then
  begin
    NewState := ( X >= 0 ) and ( X < ClientWidth ) and ( Y >= 0 ) and ( Y <= ClientHeight );

    if NewState <> FShowDownVersion then
    begin
      FShowDownVersion := NewState;
      UpdateDisplay;
    end;
  end;
end;


procedure TRzCustomButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  Pt: TPoint;
begin
  inherited;

  if FDragging then
  begin
    FDragging := False;
    FShowDownVersion := False;
    Pt := Point( X, Y );
    if PtInRect( Rect( 0, 0, ClientWidth, ClientHeight ), Pt ) then
    begin
      ChangeState;
    end;
    UpdateDisplay;
  end;
end;


procedure TRzCustomButton.CMMouseEnter( var Msg: TMessage );
var
  R: TRect;
begin
  // At design-time in Delphi 7, the form designer now allows cm_MouseEnter
  // messages to get to the components. This was not true in previous versions.
  // Now, this wouldn't be such a bad thing, but the D7 Form Designer does NOT
  // send the cm_MouseLeave message when the mouse moves off of the component.
  if csDesigning in ComponentState then
    Exit;

  FMouseOverButton := True;

  inherited;

  if FHotTrack or UseThemes then
  begin
    R := GetHotTrackRect;
    InvalidateRect( Handle, @R, False );
  end;
end;


procedure TRzCustomButton.CMMouseLeave( var Msg: TMessage );
var
  R: TRect;
begin
  FMouseOverButton := False;

  inherited;

  if FHotTrack or UseThemes then
  begin
    R := GetHotTrackRect;
    InvalidateRect( Handle, @R, False );
  end;
end;


procedure TRzCustomButton.RemoveFocus( Removing: Boolean );
var
  Form: TCustomForm;
begin
  Form := GetParentForm( Self );
  if Form <> nil then
    Form.DefocusControl( Self, Removing );
end;


procedure TRzCustomButton.CMEnabledChanged( var Msg: TMessage );
begin
  if not Enabled and (Parent <> nil) then
    RemoveFocus( False );
  if HandleAllocated and not ( csDesigning in ComponentState ) then
    EnableWindow( Handle, Enabled );
  Invalidate;
end;


procedure TRzCustomButton.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  Repaint;
end;



{=======================}
{== TRzButton Methods ==}
{=======================}

constructor TRzButton.Create( AOwner: TComponent );
begin
  inherited;
  Height := 25;
  Width := 75;
  FAlignment := taCenter;

  FShowDownPattern := True;
  FShowFocusRect := True;
  FDropDownOnEnter := True;

  Color := clBtnFace;
  FFrameColor := cl3DDkShadow;
  {&RCI}
end;


procedure TRzButton.CreateWnd;
begin
  inherited;
  FActive := FDefault;
  {&RV}
end;


procedure TRzButton.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat property was renamed to HotTrack
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
end;


procedure TRzButton.ReadOldFrameFlatProp( Reader: TReader );
begin
  HotTrack := Reader.ReadBoolean;
end;


procedure TRzButton.ChangeState;
begin
  if FGroupIndex = 0 then
    UpdateDisplay
  else
    SetDown( not FDown );

  FClicking := True;
  try
    Click;
  finally
    FClicking := False;
  end;
end;


procedure TRzButton.UpdateDisplay;
begin
  Invalidate;
end;


function TRzButton.GetCaptionRect: TRect;
begin
  Result := ClientRect;
  InflateRect( Result, -5, -5 );

  if not HotTrack and not UseThemes then
  begin
    if FGroupIndex = 0 then
    begin
      if FShowDownVersion then
        OffsetRect( Result, 1, 1 );
    end
    else
    begin
      if ( FState in [ bsDown, bsExclusive ] ) or FShowDownVersion then
        OffsetRect( Result, 1, 1 );
    end;
  end;
end;


procedure TRzButton.DrawButtonContent;
var
  TempAlignment: TAlignment;
  Flags: DWord;
begin
  { Draw Caption }
  TempAlignment := Alignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment( TempAlignment );

  Flags := dt_WordBreak or dt_ExpandTabs or DrawTextAlignments[ TempAlignment ];

  Draw3DText( Canvas, GetCaptionRect, Flags );
end;


procedure TRzButton.CreateBrushPattern( PatternBmp: TBitmap );
var
  X: Integer;
  Y: Integer;
  C: TColor;
begin
  C := clSilver;
  if ColorToRGB( Color) = clSilver then
    C := clGray;

  PatternBmp.Width := 8;
  PatternBmp.Height := 8;
  with PatternBmp.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect( Rect( 0, 0, PatternBmp.Width, PatternBmp.Height ) );
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if ( Y mod 2 ) = ( X mod 2 ) then
          Pixels[ X, Y ] := C;
  end;
end;



procedure TRzButton.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
var
  CR: TRect;
begin
  if FTransparent then
  begin
//    PaintBackground( Msg.DC );
    // Transparent in TRzButton means that HotTrack is True.

    // If Transparent, then we only need to draw the parent image at the corners
    // of the button. The rest of the button is drawn in the Paint method.
    CR := ClientRect;
    if Enabled then
    begin
      InflateRect( CR, -1, -1 );
      ExcludeClipRect( Msg.DC, CR.Left, CR.Top, CR.Right, CR.Bottom );
    end
    else
    begin
      InflateRect( CR, -2, -2 );
      ExcludeClipRect( Msg.DC, CR.Left, CR.Top, CR.Right, CR.Bottom );
    end;
    InflateRect( CR, 1, -1 );
    ExcludeClipRect( Msg.DC, CR.Left, CR.Top, CR.Right, CR.Bottom );
    InflateRect( CR, -2, 2 );
    ExcludeClipRect( Msg.DC, CR.Left, CR.Top, CR.Right, CR.Bottom );

    DrawParentImage( Self, Msg.DC, True );
    SelectClipRgn( Msg.DC, 0 );


    // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from erasing background.
    // Set Msg.Result to 1 to indicate background is painted by the control.
    Msg.Result := 1;
  end
  else
    inherited;
end;


type
  TWinControlAccess = class( TWinControl )
  end;

procedure TRzButton.Paint;
var
  FocusRect, R: TRect;
  PatternBmp: TBitmap;
  C, HC, DarkColor, LightColor: TColor;
  ElementDetails: TThemedElementDetails;


  procedure DrawOuterRecessedBorder( Bounds: TRect; Enabled: Boolean );
  var
    C: TColor;
  begin
    if Enabled then
    begin
      if Parent <> nil then
        C := TWinControlAccess( Parent ).Color
      else
        C := clBtnFace;

      DarkColor := DarkerColor( C, 20 );
      LightColor := LighterColor( C, 20 );
    end
    else // Disabled
    begin
      if Parent <> nil then
        C := TWinControlAccess( Parent ).Color
      else
        C := clBtnFace;

      DarkColor := C;
      LightColor := C;
    end;

    Canvas.Pen.Color := DarkColor;
    // Left side
    Canvas.MoveTo( Bounds.Left, Bounds.Top + 2 );
    Canvas.LineTo( Bounds.Left, Bounds.Bottom - 2 );
    // Top side
    Canvas.MoveTo( Bounds.Left + 2, Bounds.Top );
    Canvas.LineTo( Bounds.Right - 2, Bounds.Top );

    Canvas.Pen.Color := LightColor;
    // Right side
    Canvas.MoveTo( Bounds.Right - 1, Bounds.Top + 2 );
    Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom - 2 );
    // Bottom side
    Canvas.MoveTo( Bounds.Left + 2, Bounds.Bottom - 1 );
    Canvas.LineTo( Bounds.Right - 2, Bounds.Bottom - 1 );

    Canvas.Pixels[ Bounds.Left + 1, Bounds.Top + 1 ] := DarkColor;
    Canvas.Pixels[ Bounds.Right - 2, Bounds.Top + 1 ] := DarkColor;
    Canvas.Pixels[ Bounds.Right - 2, Bounds.Bottom - 2 ] := LightColor;
    Canvas.Pixels[ Bounds.Left + 1, Bounds.Bottom - 2 ] := DarkColor;
  end;

  procedure DrawSolidBorder( Bounds: TRect; Enabled: Boolean );
  var
    BorderColor: TColor;
  begin
    if Enabled then
    begin
      BorderColor := FFrameColor;
    end
    else
    begin
      if Color = clBtnFace then
        BorderColor := LighterColor( clBtnShadow, 30 )
      else
        BorderColor := DarkerColor( Color, 80 );
    end;

    Canvas.Pen.Color := BorderColor;
    // Left side
    Canvas.MoveTo( Bounds.Left, Bounds.Top + 1 );
    Canvas.LineTo( Bounds.Left, Bounds.Bottom - 1 );
    // Top side
    Canvas.MoveTo( Bounds.Left + 1, Bounds.Top );
    Canvas.LineTo( Bounds.Right - 1, Bounds.Top );
    // Right side
    Canvas.MoveTo( Bounds.Right - 1, Bounds.Top + 1 );
    Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom - 1 );
    // Bottom side
    Canvas.MoveTo( Bounds.Left + 1, Bounds.Bottom - 1 );
    Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom - 1 );
  end;


  procedure DrawButtonFace( Bounds: TRect );
  begin
    Canvas.Pen.Color := DarkerColor( Color, 20 );
    Canvas.MoveTo( Bounds.Left, Bounds.Bottom - 2 );
    Canvas.LineTo( Bounds.Right, Bounds.Bottom - 2 );

    Canvas.Pen.Color := DarkerColor( Color, 30 );
    Canvas.MoveTo( Bounds.Left, Bounds.Bottom - 1 );
    Canvas.LineTo( Bounds.Right, Bounds.Bottom - 1 );

    Dec( Bounds.Bottom, 2 );
    if FullColorSupported then
    begin
      PaintGradient( Canvas, Bounds, gdHorizontalEnd, LighterColor( Color, 30 ),
                     DarkerColor( Color, 10 ) );
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect( Bounds );
    end;
  end;


begin {= TRzButton.Paint =}
  inherited;

  FocusRect := ClientRect;
  if FHotTrack or UseThemes then
    InflateRect( FocusRect, -3, -3 )
  else
    InflateRect( FocusRect, -4, -4 );

  if FGroupIndex <> 0 then
  begin
    if ( FState in [ bsDown, bsExclusive ] ) or FShowDownVersion then
      OffsetRect( FocusRect, 1, 1 );
  end;

  R := ClientRect;

  if UseThemes then
  begin
    if Enabled then
    begin
      if ( FState in [ bsDown, bsExclusive ] ) or FShowDownVersion then
        ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonPressed )
      else
      begin
        if FMouseOverButton then
          ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonHot )
        else if ( FGroupIndex = 0 ) and ( ( FActive or Focused ) and not FShowDownVersion ) then
          ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonDefaulted )
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonNormal )
      end;
    end
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonDisabled );

    ActiveStyleServices.DrawParentBackground( Handle, Canvas.Handle, @ElementDetails, True, @R );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end
  else if FHotTrack then
  begin
    if Enabled then
    begin
      DrawOuterRecessedBorder( R, Enabled );
      InflateRect( R, -1, -1 );
      DrawSolidBorder( R, Enabled );
      InflateRect( R, -1, -1 );

      // Draw Shading

      if ( FState in [ bsDown, bsExclusive ] ) or FShowDownVersion then
      begin
        // Show Down State
        Canvas.Brush.Color := DarkerColor( Color, 10 );
        Canvas.FillRect( R );
      end
      else // Up State
      begin
        if FMouseOverButton then
        begin
          // Show Hot State
          DrawButtonFace( R );

          if FHotTrackColorType = htctComplement then
          begin
            LightColor := ComplementaryColor( FHotTrackColor, 180 );
            DarkColor := DarkerColor( LightColor, 30 );
          end
          else
          begin
            DarkColor := FHotTrackColor;
            LightColor := BlendColors( clWhite, DarkColor, 190 );
          end;

          DrawHighlightBox( Canvas, R, gdHorizontalEnd, LightColor, DarkColor );
        end
        else if ( FGroupIndex = 0 ) and ( ( FActive or Focused ) and not FShowDownVersion ) then
        begin
          // Show Default/Focused State
          DrawButtonFace( R );

          C := LighterColor( Color, 30 );
          if ColorsTooClose( FHighlightColor, clBtnFace ) then
            HC := DarkerColor( FHighlightColor, 30 )
          else
            HC := FHighlightColor;
          DarkColor := BlendColors( HC, C, 160 );
          LightColor := BlendColors( HC, C, 60 );

          DrawHighlightBox( Canvas, R, gdHorizontalEnd, LightColor, DarkColor );
        end
        else
        begin
          // Show Normal State
          DrawButtonFace( R );
        end;
      end; { end Up State }
    end
    else // Disabled
    begin
      DrawOuterRecessedBorder( R, Enabled );
      InflateRect( R, -1, -1 );
      DrawSolidBorder( R, Enabled );
      InflateRect( R, -1, -1 );

      Canvas.Brush.Color := Color;
      Canvas.FillRect( R );
    end;
  end
  else // Traditional Button appearance
  begin
    if FGroupIndex = 0 then
    begin
      if ( ( FActive or Focused ) and not FShowDownVersion ) or FClicking then
        R := DrawSides( Canvas, R, clWindowFrame, clWindowFrame, sdAllSides );
    end;

    if Color = clBtnFace then
    begin
      if FGroupIndex = 0 then
      begin
        R := DrawButtonBorder( Canvas, R, FShowDownVersion );
      end
      else
      begin
        if ( FState in [ bsDown, bsExclusive ] ) or FShowDownVersion then
          R := DrawBorder( Canvas, R, fsLowered )
        else
          R := DrawBorder( Canvas, R, fsButtonUp );
      end;
    end
    else // Color <> clBtnFace
    begin
      if FGroupIndex = 0 then
      begin
        R := DrawColorButtonBorder( Canvas, R, Color, FShowDownVersion );
      end
      else
      begin
        if ( FState in [ bsDown, bsExclusive ] ) or FShowDownVersion then
          R := DrawColorBorder( Canvas, R, Color, fsLowered )
        else
          R := DrawColorButtonBorder( Canvas, R, Color, False );
      end;
    end;

    Canvas.Brush.Color := Color;
    Canvas.FillRect( R );
  end;


  if ( FGroupIndex <> 0 ) and ( FState = bsExclusive ) and FShowDownPattern then
  begin
    PatternBmp := TBitmap.Create;
    try
      CreateBrushPattern( PatternBmp );
      Canvas.Brush.Bitmap := PatternBmp;
      Canvas.FillRect( R );
    finally
      PatternBmp.Free;
    end;
  end;


  Canvas.Font := Self.Font;
  Canvas.Brush.Color := Color;

  DrawButtonContent;


  if ShowFocus and Focused and FShowFocusRect then
    DrawFocusBorder( Canvas, FocusRect );

end; {= TRzButton.Paint =}


procedure TRzButton.Click;
var
  Form: TCustomForm;
begin
  Form := GetParentForm( Self );
  if Form <> nil then
    Form.ModalResult := ModalResult;
  inherited;
end;


procedure TRzButton.KeyDown( var Key: Word; Shift: TShiftState );
begin
  if ( Key = vk_Space ) and not ( ssAlt in Shift ) then
  begin
    FKeyToggle := True;
    FShowDownVersion := True;
    UpdateDisplay;
  end;
  inherited;
end;


procedure TRzButton.KeyUp( var Key: Word; Shift: TShiftState );
begin
  if Key = vk_Space then
  begin
    FShowDownVersion := False;
    if FKeyToggle then
      ChangeState;
    UpdateDisplay;
  end;
  inherited;
end;


procedure TRzButton.SetAlignment( Value: TAlignment );
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TRzButton.SetDefault( Value: Boolean );
var
  Form: TCustomForm;
begin
  FDefault := Value;
  if HandleAllocated then
  begin
    Form := GetParentForm( Self );
    if Form <> nil then
      Form.Perform( cm_FocusChanged, 0, LParam( Form.ActiveControl ) );
  end;
end;


procedure TRzButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if ( FGroupIndex <> 0 ) and ( Parent <> nil ) then
  begin
    Msg.Msg := cm_RzButtonPressed;
    Msg.WParam := WParam( FGroupIndex );
    Msg.LParam := LParam( Self );
    Msg.Result := 0;
    Parent.Broadcast( Msg );
  end;
end;


procedure TRzButton.SetAllowAllUp( Value: Boolean );
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;


procedure TRzButton.SetDown( Value: Boolean );
begin
  if FGroupIndex = 0 then
    Value := False;

  if Value <> FDown then
  begin
    if FDown and ( not FAllowAllUp ) and ( FGroupIndex <> 0 ) then
      Exit;

    FDown := Value;
    if Value then
      FState := bsExclusive
    else
      FState := bsUp;

    Invalidate;

    if Value then
      UpdateExclusive;
  end;
end; {= TRzButton.SetDown =}


procedure TRzButton.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzButton.SetGroupIndex( Value: Integer );
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;


procedure TRzButton.SetHotTrack( Value: Boolean );
var
  R: TRect;
begin
  inherited;
  FTransparent := FHotTrack;
  if HandleAllocated then
  begin
    R := ClientRect;
    InvalidateRect( Handle, @R, True );
  end;
end;


procedure TRzButton.SetShowDownPattern( Value: Boolean );
begin
  if FShowDownPattern <> Value then
  begin
    FShowDownPattern := Value;
    Invalidate;
  end;
end;


procedure TRzButton.SetShowFocusRect( Value: Boolean );
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    Invalidate;
  end;
end;


procedure TRzButton.CMDialogKey( var Msg: TCMDialogKey );
begin
  if ( ( ( Msg.CharCode = vk_Return ) and FActive ) or
       ( ( Msg.CharCode = vk_Escape ) and FCancel ) ) and
     ( KeyDataToShiftState( Msg.KeyData ) = [] ) and CanFocus then
  begin
    if not FDropDownOnEnter then
      FKeyWasPressed := True;
    try
      Click;
    finally
      FKeyWasPressed := False;
    end;
    Msg.Result := 1;
  end
  else
    inherited;
end;


procedure TRzButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  if IsAccel( Msg.CharCode, Caption ) and CanFocus then
  begin
    Click;
    Msg.Result := 1;
  end
  else
    inherited;
end;


procedure TRzButton.CMFocusChanged( var Msg: TCMFocusChanged );
var
  MakeActive: Boolean;
begin
  if not ( csDestroying in ComponentState ) then
  begin
    with Msg do
    begin
      if ( Sender is TRzButton ) or ( Sender is TButton ) then
        MakeActive := Sender = Self
      else
        MakeActive := FDefault;
    end;

    if MakeActive <> FActive then
    begin
      FActive := MakeActive;
      Repaint;
    end;
    inherited;
  end;
end;


procedure TRzButton.CMRzButtonPressed( var Msg: TMessage );
var
  Sender: TRzButton;
begin
  if Msg.WParam = WParam( FGroupIndex ) then
  begin
    Sender := TRzButton( Msg.LParam );
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;

      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;


procedure TRzButton.WMKillFocus( var Msg: TMessage );
begin
  if not ( csDestroying in ComponentState ) then
  begin
    inherited;
    FKeyToggle := False;
    FShowDownVersion := False;
    UpdateDisplay;
  end;
end;


{=======================}
{== TRzBitBtn Methods ==}
{=======================}

constructor TRzBitBtn.Create( AOwner: TComponent );
begin
  inherited;

  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChangedHandler;
  FNumGlyphs := 1;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := 2;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;
  FImageIndex := -1;
  FDisabledIndex := -1;
  {&RCI}
end;


destructor TRzBitBtn.Destroy;
begin
  FGlyph.Free;
  FImagesChangeLink.Free;
  inherited;
end;


procedure TRzBitBtn.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FImages ) then
    SetImages( nil )  // Call access method so connections to link object can be cleared
end;


function TRzBitBtn.IsCustom: Boolean;
begin
  Result := Kind = bkCustom;
end;


function TRzBitBtn.IsCustomCaption: Boolean;
begin
  Result := AnsiCompareStr( Caption, BitBtnCaptions[ FKind ] ) <> 0;
end;


procedure TRzBitBtn.GlyphChangedHandler( Sender: TObject );
var
  N: Integer;
begin
  if ( FGlyph.Height <> 0 ) and ( FGlyph.Width mod FGlyph.Height = 0 ) then
  begin
    N := FGlyph.Width div FGlyph.Height;
    if ( N < 1 ) or ( N > 4 ) then
      N := 1;
    SetNumGlyphs( N );
  end;
  Invalidate;
end;


procedure TRzBitBtn.SetGlyph( Value: TBitmap );
begin
  {&RV}
  FGlyph.Assign( Value );
  FModifiedGlyph := True;
end;


function TRzBitBtn.GetKind: TBitBtnKind;
begin
  if FKind <> bkCustom then
  begin
    if ( ( FKind in [ bkOK, bkYes ] ) xor Default ) or
       ( ( FKind in [ bkCancel, bkNo ]) xor Cancel ) or
       ( ModalResult <> BitBtnModalResults[ FKind ] ) or
       FModifiedGlyph then
    begin
      FKind := bkCustom;
    end;
  end;
  Result := FKind;
end;


procedure TRzBitBtn.SetKind( Value: TBitBtnKind );
begin
  if Value <> FKind then
  begin
    if Value <> bkCustom then
    begin
      Default := Value in [ bkOK, bkYes ];
      Cancel := Value in [ bkCancel, bkNo ];

      if ( ( csLoading in ComponentState ) and ( Caption = '' ) ) or
         ( not ( csLoading in ComponentState ) ) then
      begin
        if BitBtnCaptions[ Value ] <> '' then
          Caption := BitBtnCaptions[ Value ];
      end;

      ModalResult := BitBtnModalResults[ Value ];
      FGlyph.Assign( GetBitBtnGlyph( Value ) );
      NumGlyphs := 2;
      FModifiedGlyph := False;
    end;
    FKind := Value;
    Invalidate;
  end;
end; {= TRzBitBtn.SetKind =}


procedure TRzBitBtn.SetLayout( Value: TButtonLayout );
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;


procedure TRzBitBtn.SetMargin( Value: Integer );
begin
  if ( Value <> FMargin ) and ( Value >= -1 ) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;


procedure TRzBitBtn.SetNumGlyphs( Value: TNumGlyphs );
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    Invalidate;
  end;
end;


procedure TRzBitBtn.SetSpacing( Value: Integer );
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;


procedure TRzBitBtn.ActionChange( Sender: TObject; CheckDefaults: Boolean );
begin
  inherited;

  if Sender is TCustomAction then
  begin
    if not CheckDefaults or ( Self.ImageIndex = -1 ) then
      Self.ImageIndex := TCustomAction( Sender ).ImageIndex;
  end;
end; {= TRzBitBtn.ActionChange =}


function TRzBitBtn.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;


function TRzBitBtn.GetImageSize: TPoint;
begin
  if ( FImages <> nil ) and ( FImageIndex <> -1 ) then
    Result := Point( FImages.Width, FImages.Height )
  else if FGlyph <> nil then
    Result := Point( FGlyph.Width div FNumGlyphs, FGlyph.Height )
  else
    Result := Point( 0, 0 );
end;


function TRzBitBtn.GetCaptionRect: TRect;
var
  GlyphSize: TPoint;
  CaptionHeight, Offset, TotalH: Integer;
  TempRct: TRect;
  TempLayout: TButtonLayout;
begin
  { Adjust the size of the Text Rectangle based on the size of the Glyph
    and the Layout, Margin, and Spacing properties. }

  Result := inherited GetCaptionRect;

  if ( ( FImages = nil ) or ( FImageIndex = -1 ) ) and FGlyph.Empty then
    Exit;

  GlyphSize := GetImageSize;

  if UseRightToLeftAlignment and ( FLayout = blGlyphLeft ) then
    TempLayout := blGlyphRight
  else
    TempLayout := FLayout;

  if FMargin >= 0 then
  begin
    case TempLayout of
      blGlyphLeft:
        Inc( Result.Left, FMargin + GlyphSize.X + FSpacing );

      blGlyphRight:
        Dec( Result.Right, FMargin + GlyphSize.X + FSpacing );

      blGlyphTop:
        Inc( Result.Top, FMargin + GlyphSize.Y + FSpacing );

      blGlyphBottom:
        Dec( Result.Bottom, FMargin + GlyphSize.Y + FSpacing );
    end;
  end
  else { Margin = -1    Therefore, center the glyph and caption}
  begin
    Canvas.Font := Self.Font;
    TempRct := Result;

    CaptionHeight := DrawString( Canvas, Caption, TempRct,
                                 dt_CalcRect or dt_WordBreak or dt_ExpandTabs or dt_VCenter or dt_Center );

    case TempLayout of
      blGlyphLeft:
        Inc( Result.Left, FMargin + GlyphSize.X + FSpacing );

      blGlyphRight:
        Dec( Result.Right, FMargin + GlyphSize.X + FSpacing );

      blGlyphTop:
      begin
        TotalH := CaptionHeight + GlyphSize.Y + FSpacing;
        Offset := ( Height - TotalH ) div 2;
        Inc( Result.Top, Offset + GlyphSize.Y );
        Result.Bottom := Result.Top + CaptionHeight;
      end;

      blGlyphBottom:
      begin
        TotalH := CaptionHeight + GlyphSize.Y + FSpacing;
        Offset := ( Height - TotalH ) div 2;
        Inc( Result.Top, Offset );
        Result.Bottom := Result.Top + CaptionHeight;
      end;
    end;
  end;
end; {= TRzBitBtn.GetCaptionRect =}


function TRzBitBtn.GetGlyphRect: TRect;
var
  GlyphSize: TPoint;
  CaptionHeight, Offset, TotalH: Integer;
  TempRct: TRect;
  TempLayout: TButtonLayout;
begin
  Result := inherited GetCaptionRect;
  if FShowDownVersion then
  begin
    if UseThemes then
      OffsetRect( Result, -1, 0 )
    else
      OffsetRect( Result, -1, -1 );
  end;

  GlyphSize := GetImageSize;

  if UseRightToLeftAlignment and ( FLayout = blGlyphLeft ) then
    TempLayout := blGlyphRight
  else
    TempLayout := FLayout;

  if FMargin >= 0 then
  begin
    case TempLayout of
      blGlyphLeft:
        Inc( Result.Left, FMargin );

      blGlyphRight:
        Result.Left := Result.Right - FMargin - GlyphSize.X;

      blGlyphTop:
        Inc( Result.Top, FMargin );

      blGlyphBottom:
        Result.Top := Result.Bottom - FMargin - GlyphSize.Y;
    end;
  end
  else  { Margin = -1 }
  begin
    Canvas.Font := Self.Font;
    TempRct := Result;

    CaptionHeight := DrawString( Canvas, Caption, TempRct,
                                 dt_CalcRect or dt_WordBreak or dt_ExpandTabs or dt_VCenter or dt_Center );

    case TempLayout of
      blGlyphRight:
        Result.Left := Result.Right - GlyphSize.X;

      blGlyphTop:
      begin
        TotalH := CaptionHeight + GlyphSize.Y + FSpacing;
        Offset := ( Height - TotalH ) div 2;
        Result.Top := Offset;
      end;

      blGlyphBottom:
      begin
        TotalH := CaptionHeight + GlyphSize.Y + FSpacing;
        Offset := ( Height - TotalH ) div 2;
        Inc( Result.Top, Offset + CaptionHeight );
      end;
    end;
  end;

  if TempLayout in [ blGlyphLeft, blGlyphRight ] then
    Result.Top := Result.Top + ( Result.Bottom - Result.Top - GlyphSize.Y + 1 ) div 2
  else
    Result.Left := Result.Left + ( Result.Right - Result.Left - GlyphSize.X + 1 ) div 2;

  Result.Right := Result.Left + GlyphSize.X;
  Result.Bottom := Result.Top + GlyphSize.Y;

  if FShowDownVersion then
  begin
    if UseThemes then
      OffsetRect( Result, 2, 0 )
    else
      OffsetRect( Result, 1, 1 );
  end;

end; {= TRzBitBtn.GetGlyphRect =}



procedure TRzBitBtn.ChangeScale( M, D: Integer );
begin
  inherited;
  FMargin := MulDiv( FMargin, M, D );
  FSpacing := MulDiv( FSpacing, M, D );
end;


procedure TRzBitBtn.DrawGlyph( R: TRect );
var
  DestRct, SrcRct: TRect;
  DestBmp: TBitmap;
  W, H: Integer;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  DestRct := Rect( 0, 0, W, H );

  if ( FNumGlyphs = 4 ) and ( FState = bsExclusive ) then
    SrcRct := Rect( 3 * W, 0, 4 * W, H )
  else if ( FNumGlyphs >= 3 ) and ( ( FState = bsDown ) or FShowDownVersion ) then
    SrcRct := Rect( 2 * W, 0, 3 * W, H )
  else if ( FNumGlyphs >= 2 ) and not Enabled then
    SrcRct := Rect( W, 0, W + W, H )
  else
    SrcRct := Rect( 0, 0, W, H );

  // The DestBmp holds the desired region of the FGlyph bitmap

  DestBmp := TBitmap.Create;
  try
    // Don't Forget to Set the Width and Height of Destination Bitmap
    DestBmp.Width := W;
    DestBmp.Height := H;
    DestBmp.Canvas.Brush.Color := Color;

    DestBmp.Canvas.CopyRect( DestRct, Canvas, R );
    DrawFullTransparentBitmap( DestBmp.Canvas, FGlyph, DestRct, SrcRct,
                               FGlyph.TransparentColor );
    Canvas.Draw( R.Left, R.Top, DestBmp );
  finally
    DestBmp.Free;
  end;
end; {= TRzBitBtn.DrawGlyph =}


procedure TRzBitBtn.DrawImage( R: TRect );
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
end; {= TRzBitBtn.DrawImage =}


procedure TRzBitBtn.DrawButtonContent;
var
  GlyphRect: TRect;
begin
  inherited;

  // Text display is handled by ancestor class.  Display glyph here.

  GlyphRect := GetGlyphRect;

  if ( Kind = bkCustom ) and ( FImages <> nil ) and ( FImageIndex <> -1 ) then
    DrawImage( GlyphRect )
  else if not FGlyph.Empty then
    DrawGlyph( GlyphRect );
end;


procedure TRzBitBtn.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;


procedure TRzBitBtn.SetDisabledIndex( Value: TImageIndex );
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    Invalidate;
  end;
end;


procedure TRzBitBtn.SetImages( Value: TCustomImageList );
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


procedure TRzBitBtn.ImagesChange( Sender: TObject );
begin
  if Sender = Images then
  begin
    Update;         // Call Update instead of Invalidate to prevent flicker
  end;
end;



{===========================}
{== TRzMenuButton Methods ==}
{===========================}

constructor TRzMenuButton.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
  ControlStyle := ControlStyle - [ csDoubleClicks ];
  FDropped := False;
  DragMode := dmManual;
  Width := 110;
  FShowArrow := True;
end;


procedure TRzMenuButton.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FDropDownMenu ) then
    FDropDownMenu := nil;
end;


procedure TRzMenuButton.DoDropDown;
var
  P: TPoint;
  MenuWidth, FarRightEdge: Integer;
  Monitor: TMonitor;
begin
  if not Assigned( FDropDownMenu ) then
    Exit;

  DropDown;                                        { Generate OnDropDown event }

  if UseRightToLeftAlignment then
    P.X := Width
  else
    P.X := 0;
  P.Y := Height;
  P := ClientToScreen( P );                    { Convert to screen coordinates }

  MenuWidth := GetMenuWidth( Self, FDropDownMenu );

  Monitor := GetMonitorContainingPoint( P );
  if Assigned( Monitor ) then
    FarRightEdge := GetMonitorWorkArea( Monitor ).Right
  else
    FarRightEdge := GetActiveWorkAreaWidth( Parent );
  if P.X + MenuWidth > FarRightEdge then
    Dec( P.X, MenuWidth - Width );

  FDropDownMenu.PopupComponent := Self;
  FDropDownMenu.Popup( P.X, P.Y );
end;


procedure TRzMenuButton.Click;
begin
  if FSkipNextClick then
    FSkipNextClick := False
  else if ( FDropDownMenu <> nil ) and not FKeyWasPressed then
    MouseDown( mbLeft, [ ssLeft ], 0, 0 )
  else
    inherited;
  {&RV}
end;


procedure TRzMenuButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  T: DWord;
begin
  FDropped := not FDropped;
  inherited;

  { Only go further if only left button is pressed }
  if ( FDropDownMenu = nil ) or ( Shift <> [ ssLeft ] ) then
    Exit;

  T := GetTickCount;
  if ( FDropTime > 0 ) and ( T < FDropTime + MinDelay ) then
  begin
    { If GetTickCount < FDropTime + MinDelay then this MouseDown method call
      is resulting from a "second" click on the button }
    if T < FDropTime + MinDelay then
      FSkipNextClick := True;
    FDropTime := 0;
    ReleaseCapture;
  end
  else
  begin
    DoDropDown;
    FSkipNextClick := True;
    FDropTime := GetTickCount;
  end;
  inherited MouseUp( Button, Shift, X, Y );                // Generate MouseUp event
end; {= TRzMenuButton.MouseDown =}


procedure TRzMenuButton.WMKeyDown( var Msg: TWMKeyDown );
begin
  FDropped := not FDropped;

  if Msg.CharCode = vk_F4 then
  begin
    DoDropDown;
    FSkipNextClick := True;
  end;
  inherited;
end;


procedure TRzMenuButton.DropDown;
begin
  if Assigned( FOnDropDown ) then
    FOnDropDown( FDropDownMenu );
end;


procedure TRzMenuButton.SetDropDownMenu( Value: TPopupMenu );
begin
  if FDropDownMenu <> Value then
  begin
    FDropDownMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzMenuButton.SetShowArrow( Value: Boolean );
begin
  if FShowArrow <> Value then
  begin
    FShowArrow := Value;
    Invalidate;
  end;
end;


function TRzMenuButton.GetCaptionRect: TRect;
begin
  Result := inherited GetCaptionRect;
  if FShowArrow then
  begin
    if UseRightToLeftAlignment then
      Inc( Result.Left, 14 )
    else
      Dec( Result.Right, 14 );
  end;
end;


function TRzMenuButton.GetGlyphRect: TRect;
begin
  Result := inherited GetGlyphRect;
  if FShowArrow then
  begin
    case FLayout of
      blGlyphRight:
        OffsetRect( Result, -14, 0 );

      blGlyphTop, blGlyphBottom:
        OffsetRect( Result, -7, 0 );
    end;
  end;
end; {= TRzMenuButton.GetGlyphRect =}


procedure TRzMenuButton.Paint;
var
  X, Y: Integer;
begin
  inherited;

  { Draw Arrow }
  if FShowArrow then
  begin
    if Enabled then
    begin
      Canvas.Brush.Color := clWindowText;
      Canvas.Pen.Color := clWindowText;
    end
    else
    begin
      Canvas.Brush.Color := clBtnShadow;
      Canvas.Pen.Color := clBtnShadow;
    end;

    if UseRightToLeftAlignment then
      X := 8
    else
      X := ClientRect.Right - 12;
    Y := ClientRect.Top + ( ClientRect.Bottom - ClientRect.Top ) div 2 + 2;
    if FShowDownVersion then
    begin
      Inc( X );
      Inc( Y );
    end;
    Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 3 ), Point( X + 3, Y - 3 ) ] );
  end;
end;



{========================================}
{== TRzToolbarButtonActionLink Methods ==}
{========================================}

function TRzToolbarButtonActionLink.IsCaptionLinked: Boolean;
begin
  if ( FClient is TRzToolbarButton ) and ( TRzToolbarButton( FClient ).IgnoreActionCaption ) then
    Result := False
  else
    Result := inherited IsCaptionLinked;
end;


{==============================}
{== TRzToolbarButton Methods ==}
{==============================}

constructor TRzToolbarButton.Create( AOwner: TComponent );
begin
  inherited;
  FIgnoreActionCaption := False;
  FMouseOverControl := False;

  FChangingGlyph := False;
  FUseHotGlyph := False;
  FHotGlyph := TBitmap.Create;
  FHotGlyph.OnChange := HotGlyphChangedHandler;
  FStdGlyph := TBitmap.Create;

  FShowCaption := True;

  {&RCI}
  Flat := True;
end;


destructor TRzToolbarButton.Destroy;
begin
  FHotGlyph.Free;
  FStdGlyph.Free;
  inherited;
end;


procedure TRzToolbarButton.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Save the FSaveCaption field to stream if ShowCaption is False
  Filer.DefineProperty( 'SaveCaption', ReadSaveCaption, WriteSaveCaption, not FShowCaption );
end;


procedure TRzToolbarButton.ReadSaveCaption( Reader: TReader );
begin
  FSaveCaption := Reader.ReadString;
end;


procedure TRzToolbarButton.WriteSaveCaption( Writer: TWriter );
begin
  Writer.WriteString( FSaveCaption );
end;


procedure TRzToolbarButton.ActionChange( Sender: TObject; CheckDefaults: Boolean );
var
  OldCaption: string;

  procedure CopyImage( ImageList: TCustomImageList; Index: Integer );
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;
      Canvas.FillRect( Rect( 0,0, Width, Height ) );
      ImageList.Draw( Canvas, 0, 0, Index );
    end;
  end;

begin
  OldCaption := Caption;
  inherited;

  { Redo inherited version to allow Glyph to change }
  if Sender is TCustomAction then
  begin
    with TCustomAction( Sender ) do
    begin
      { Copy image from action's imagelist }
      if {( Glyph.Empty ) and}
         ( ActionList <> nil ) and
         ( ActionList.Images <> nil ) and
         ( ImageIndex >= 0 ) and
         ( ImageIndex < ActionList.Images.Count ) then
      begin
        CopyImage( ActionList.Images, ImageIndex );
      end;
    end;
  end;


  if FIgnoreActionCaption then
    Caption := OldCaption;

  if Sender is TCustomAction then
  begin
    with TCustomAction( Sender ) do
    begin
      if not CheckDefaults or ( Self.Down = False ) then
        Self.Down := Checked;
    end;
  end;
end;


function TRzToolbarButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TRzToolbarButtonActionLink;
end;


procedure TRzToolbarButton.HotGlyphChangedHandler( Sender: TObject );
var
  N: Integer;
begin
  if ( FHotGlyph.Height <> 0 ) and ( FHotGlyph.Width mod FHotGlyph.Height = 0 ) then
  begin
    N := FHotGlyph.Width div FHotGlyph.Height;
    if N > 4 then
      N := 1;
    SetHotNumGlyphs( N );
  end;
  Invalidate;
end;


procedure TRzToolbarButton.SetHotGlyph( Value: TBitmap );
begin
  FHotGlyph.Assign( Value );
  FUseHotGlyph := FHotGlyph <> nil;
end;

procedure TRzToolbarButton.SetHotNumGlyphs( Value: TNumGlyphs );
begin
  if FHotNumGlyphs <> Value then
  begin
    FHotNumGlyphs := Value;
    Invalidate;
  end;
end;


function TRzToolbarButton.GetCaption: TCaption;
begin
  Result := inherited Caption;
end;

procedure TRzToolbarButton.SetCaption( const Value: TCaption );
begin
  if FShowCaption then
    inherited Caption := Value
  else
  begin
    FSaveCaption := GetCaption;
    inherited Caption := '';
  end;
end;


procedure TRzToolbarButton.SetShowCaption( Value: Boolean );
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    if FShowCaption then
      Caption := FSaveCaption
    else
      Caption := '';
    Invalidate;
  end;
end;


procedure TRzToolbarButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  with Msg do
  begin
    if IsAccel( CharCode, Caption ) and Enabled and Visible and
      ( Parent <> nil ) and Parent.Showing then
    begin
      if GroupIndex > 0 then
      begin
        Down := not Down;
        if Down then
          Repaint;
      end;
      Click;
      Result := 1;
    end
    else
      inherited;
  end;
end;


procedure TRzToolbarButton.MouseDown( Button: TMouseButton; Shift: TShiftState;
                                      X, Y: Integer );
begin
  inherited;

  // The inherited Click method causes a new cm_MouseEnter message to get sent.
  // We prevent the glyph from changing for this new message by setting FChangingGlyph to True.
  if FUseHotGlyph then
    FChangingGlyph := True;
end;


procedure TRzToolbarButton.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  // Setting FMouseOverControl must occur before inherited because Paint method
  // is called during the call to inherited.  If we don't set this flag now,
  // the Paint method will not draw the divider line.

  FMouseOverControl := True;

  {&RV}
  inherited;

  if FUseHotGlyph and Enabled then
  begin
    if not FChangingGlyph then
    begin
      FChangingGlyph := True;

      FStdGlyph.Assign( Glyph );
      FStdNumGlyphs := NumGlyphs;

      Glyph.Assign( FHotGlyph );
      NumGlyphs := FHotNumGlyphs;
    end
    else
      FChangingGlyph := False;
    Repaint;
  end;
end;


procedure TRzToolbarButton.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  FMouseOverControl := False;

  if FUseHotGlyph and Enabled then
  begin
    { The following test is needed because cm_MouseLeave is sent when
      Enabled property is changed }
    if FStdNumGlyphs <> 0 then
    begin
      Glyph.Assign( FStdGlyph );
      NumGlyphs := FStdNumGlyphs;
    end;

    FChangingGlyph := False;
    Repaint;
  end;
end;


{==================================}
{== TRzMenuToolbarButton Methods ==}
{==================================}

constructor TRzMenuToolbarButton.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csDoubleClicks ];
  FDropped := False;
  DragMode := dmManual;
  FMouseOverControl := False;
  FShowArrow := True;
  Width := 40;
  Margin := 2;
  {&RCI}
end;


procedure TRzMenuToolbarButton.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FDropDownMenu ) then
    FDropDownMenu := nil;
end;


procedure TRzMenuToolbarButton.DoDropDown;
var
  P: TPoint;
  MenuWidth, FarRightEdge: Integer;
  Monitor: TMonitor;
begin
  if not Assigned( FDropDownMenu ) then
    Exit;

  DropDown;                                        { Generate OnDropDown event }

  if UseRightToLeftAlignment then
    P.X := Width
  else
    P.X := 0;
  P.Y := Height;
  P := ClientToScreen( P );                    { Convert to screen coordinates }

  MenuWidth := GetMenuWidth( Self, FDropDownMenu );

  Monitor := GetMonitorContainingPoint( P );
  if Assigned( Monitor ) then
    FarRightEdge := GetMonitorWorkArea( Monitor ).Right
  else
    FarRightEdge := GetActiveWorkAreaWidth( Parent );
  if P.X + MenuWidth > FarRightEdge then
    Dec( P.X, MenuWidth - Width );

//  if UseRightToLeftAlignment then
//    FDropDownMenu.Alignment := paRight;
  FDropDownMenu.PopupComponent := Self;
  FDropDownMenu.Popup( P.X, P.Y );
  if ( GroupIndex > 0 ) and not Down then
    Click;
end;


procedure TRzMenuToolbarButton.Click;
begin
  if FSkipNextClick then
    FSkipNextClick := False
  else
    inherited;
  {&RV}
end;


procedure TRzMenuToolbarButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  T: DWord;
begin
  FDropped := not FDropped;
  inherited;

  { Only go further if only left button is pressed }
  if ( FDropDownMenu = nil ) or 
     {$IFDEF VCL140_OR_HIGHER}
     // Check for ssPen to handle doing dropdowns on Tablets
     ( ( Shift <> [ ssLeft ] ) and ( Shift <> [ ssLeft, ssPen ] ) and ( Shift <> [ ssLeft, ssTouch ] ) ) or
     {$ELSE}
     ( Shift <> [ ssLeft ] ) or
     {$ENDIF}
     ( FShowArrow and ( X < Width - 15 ) ) then
    Exit;

  T := GetTickCount;
  if ( FDropTime > 0 ) and ( T < FDropTime + MinDelay ) then
  begin
    { If GetTickCount < FDropTime + MinDelay then this MouseDown method call
      is resulting from a "second" click on the button }
    if T < FDropTime + MinDelay then
      FSkipNextClick := True;
    FDropTime := 0;
    ReleaseCapture;
  end
  else
  begin
    DoDropDown;
    FSkipNextClick := True;
    FDropTime := GetTickCount;
  end;
  inherited MouseUp( Button, Shift, X, Y );                // Generate MouseUp event
end; {= TRzMenuToolbarButton.MouseDown =}



procedure TRzMenuToolbarButton.DropDown;
begin
  if Assigned( FOnDropDown ) then
    FOnDropDown( FDropDownMenu );
end;


procedure TRzMenuToolbarButton.SetDropDownMenu( Value: TPopupMenu );
begin
  if FDropDownMenu <> Value then
  begin
    FDropDownMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzMenuToolbarButton.SetShowArrow( Value: Boolean );
begin
  if FShowArrow <> Value then
  begin
    FShowArrow := Value;
    Invalidate;
  end;
end;



procedure TRzMenuToolbarButton.Paint;
var
  X, Y: Integer;
begin
  inherited;

  { Draw Arrow }
  if FShowArrow then
  begin
    if ( FMouseOverControl and Enabled ) or not Flat then
    begin
      if UseRightToLeftAlignment then
        X := 15
      else
        X := Width - 15;
      if FState = bsDown then
        Inc( X );

      if FState <> bsDown then
      begin
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.MoveTo( X, 0 );
        Canvas.LineTo( X, Height );
      end;
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo( X - 1, 0 );
      Canvas.LineTo( X - 1, Height );
    end;

    if Enabled then
    begin
      Canvas.Brush.Color := clWindowText;
      Canvas.Pen.Color := clWindowText;
    end
    else
    begin
      Canvas.Brush.Color := clBtnShadow;
      Canvas.Pen.Color := clBtnShadow;
    end;

    if UseRightToLeftAlignment then
      X := 8
    else
      X := ClientRect.Right - 8;
    Y := ClientRect.Top + ( ClientRect.Bottom - ClientRect.Top ) div 2 + 2;
    if FState = bsDown then
    begin
      Inc( X );
      Inc( Y );
    end;
    Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 3 ), Point( X + 3, Y - 3 ) ] );
  end;
end;


{=====================================}
{== TRzToolButtonActionLink Methods ==}
{=====================================}

procedure TRzToolButtonActionLink.AssignClient( AClient: TObject );
begin
  inherited;
  FClient := AClient as TRzToolButton;
end;

function TRzToolButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and ( FClient.Down = ( Action as TCustomAction ).Checked );
end;

function TRzToolButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and ( FClient.ImageIndex = ( Action as TCustomAction ).ImageIndex );
end;

procedure TRzToolButtonActionLink.SetChecked( Value: Boolean );
begin
  if IsCheckedLinked then
    FClient.Down := Value;
end;

procedure TRzToolButtonActionLink.SetImageIndex( Value: Integer );
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;


{===========================}
{== TRzToolButton Methods ==}
{===========================}

constructor TRzToolButton.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := [ csCaptureMouse ];

  Width := 25;
  Height := 25;
  FUseToolbarButtonSize := True;

  FAlignment := taCenter;
  FLayout := blGlyphLeft;
  FUseToolbarButtonLayout := True;

  FShowCaption := False;
  FUseToolbarShowCaption := True;

  Color := clBtnFace;

  FVisualStyle := vsWinXP;
  FGradientColorStyle := gcsSystem;
  FUseToolbarVisualStyle := True;

  FSelectionColorStart := clBtnFace;
  FSelectionColorStop := clBtnShadow;
  FSelectionFrameColor := cl3DDkShadow;

  FTransparent := True;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;

  FDisabledImagesChangeLink := TChangeLink.Create;
  FDisabledImagesChangeLink.OnChange := DisabledImagesChange;

  FImageIndex := -1;
  FDownIndex := -1;
  FDisabledIndex := -1;
  FHotIndex := -1;

  FMouseOverButton := False;
  FInDoDropDown := False;
  FFlat := True;
  FSpacing := 4;

  DragMode := dmManual;
  FToolStyle := tsButton;
  FTreatAsNormal := True;
end;


destructor TRzToolButton.Destroy;
begin
  FImagesChangeLink.Free;
  FDisabledImagesChangeLink.Free;
  inherited;
end;


procedure TRzToolButton.Loaded;
begin
  inherited;
  PickupToolbarStyles;
end;


procedure TRzToolButton.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the GradientColorDefault and ThemeAware properties
  // were published in verison 3.x
  Filer.DefineProperty( 'GradientColorDefault',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );
  Filer.DefineProperty( 'ThemeAware',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );

  // Handle the fact that the GradientColorStart property was renamed
  // to SelectionColorStart
  Filer.DefineProperty( 'GradientColorStart', ReadOldGradientColorStartProp,
                        nil, False );

  // Handle the fact that the GradientColorStop property was renamed
  // to SelectionColorStop
  Filer.DefineProperty( 'GradientColorStop', ReadOldGradientColorStopProp,
                        nil, False );

  // Handle the fact that the FrameColor property was renamed
  // to SelectionFrameColor
  Filer.DefineProperty( 'FrameColor', ReadOldFrameColorProp, nil, False );

  // Handle the fact that the UseGradients property was replaced with the
  // VisualStyle property
  Filer.DefineProperty( 'UseGradients', ReadOldUseGradientsProp, nil, False );
end;


procedure TRzToolButton.ReadOldGradientColorStartProp( Reader: TReader );
begin
  if Reader.NextValue = vaIdent then
    SelectionColorStart := StringToColor( Reader.ReadIdent )
  else
    SelectionColorStart := Reader.ReadInteger;
end;

procedure TRzToolButton.ReadOldGradientColorStopProp( Reader: TReader );
begin
  if Reader.NextValue = vaIdent then
    SelectionColorStop := StringToColor( Reader.ReadIdent )
  else
    SelectionColorStop := Reader.ReadInteger;
end;

procedure TRzToolButton.ReadOldFrameColorProp( Reader: TReader );
begin
  if Reader.NextValue = vaIdent then
    SelectionFrameColor := StringToColor( Reader.ReadIdent )
  else
    SelectionFrameColor := Reader.ReadInteger;
end;


procedure TRzToolButton.ReadOldUseGradientsProp( Reader: TReader );
var
  UseGradients: Boolean;
begin
  UseGradients := Reader.ReadBoolean;
  if UseGradients then
    VisualStyle := vsGradient;
end;


procedure TRzToolButton.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FImages then
      SetImages( nil )  // Call access method so connections to link object can be cleared
    else if AComponent = FDisabledImages then
      SetDisabledImages( nil )
    else if AComponent = FDropDownMenu then
      FDropDownMenu := nil;
  end;
end;


procedure TRzToolButton.PickupToolbarStyles;
begin
  if Parent <> nil then
  begin
    Perform( cm_ToolbarShowCaptionChanged, 0, 0 );
    Perform( cm_ToolbarButtonLayoutChanged, 0, 0 );
    Perform( cm_ToolbarButtonSizeChanged, 0, 0 );
    Perform( cm_ToolbarVisualStyleChanged, 0, 0 );
  end;
end;


procedure TRzToolButton.SetParent( Value: TWinControl );
begin
  inherited;
  if not ( csLoading in ComponentState ) then
    PickupToolbarStyles;
end;


procedure TRzToolButton.DoDropDown;
var
  P: TPoint;
  MenuWidth, FarRightEdge, FarLeftEdge: Integer;
  Monitor: TMonitor;
begin
  if not Assigned( FDropDownMenu ) then
    Exit;

  FInDoDropDown := True;
  try
    DropDown;  // Generate OnDropDown event

    case FDropDownMenu.Alignment of
      paLeft:
      begin
        if UseRightToLeftAlignment then
          P.X := Width
        else
          P.X := 0;
      end;

      paRight:
      begin
        if UseRightToLeftAlignment then
          P.X := 0
        else
          P.X := Width;
      end;

      paCenter:
      begin
        P.X := Width div 2;
      end;
    end;
    P.Y := Height;
    P := ClientToScreen( P );

    MenuWidth := GetMenuWidth( Self, FDropDownMenu );

    Monitor := GetMonitorContainingPoint( P );

    case FDropDownMenu.Alignment of
      paLeft:
      begin
        if Assigned( Monitor ) then
          FarRightEdge := GetMonitorWorkArea( Monitor ).Right
        else
          FarRightEdge := GetActiveWorkAreaWidth( Parent );

        if P.X + MenuWidth > FarRightEdge then
          P.X := FarRightEdge - MenuWidth;
      end;

      paRight:
      begin
        if Assigned( Monitor ) then
          FarLeftEdge := GetMonitorWorkArea( Monitor ).Left
        else
          FarLeftEdge := 0;

        if P.X - MenuWidth < FarLeftEdge then
          P.X := FarLeftEdge + MenuWidth;
      end;

      paCenter:
      begin
      end;
    end;

    FDropDownMenu.PopupComponent := Self;
    FDropDownMenu.Popup( P.X, P.Y );
    if ( GroupIndex > 0 ) and not Down then
      Click;
  finally
    FInDoDropDown := False;
  end;
end;


procedure TRzToolButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  T: DWord;
begin
  inherited;

  case FToolStyle of
    tsButton:
    begin
      if ( Button = mbLeft ) and Enabled then
      begin
        if not FDown or ( FState = tbsExclusive ) then
        begin
          FState := tbsDown;
          Invalidate;
        end;
        FDragging := True;
      end;
    end;

    tsDropDown:
    begin
      { Only go further if only left button is pressed }
      if ( FDropDownMenu = nil ) or
         {$IFDEF VCL140_OR_HIGHER}
         // Check for ssPen to handle doing dropdowns on Tablets
         ( ( Shift <> [ ssLeft ] ) and ( Shift <> [ ssLeft, ssPen ] ) and ( Shift <> [ ssLeft, ssTouch ] ) ) or
         {$ELSE}
         ( Shift <> [ ssLeft ] ) or
         {$ENDIF}
         ( ( ( not UseRightToLeftAlignment and ( X < Width - ArrowRegionWidth ) ) or
             ( UseRightToLeftAlignment and ( X > ArrowRegionWidth ) ) ) and
           ( Assigned( OnClick ) or ( Action <> nil ) ) ) then
      begin
        { Treat as a normal button }
        if FTreatAsNormal then
        begin
          if ( Button = mbLeft ) and Enabled then
          begin
            if not FDown then
            begin
              FState := tbsDown;
              Invalidate;
            end;
            FDragging := True;
          end;
        end
        else
          FTreatAsNormal := True;

        Exit;
      end;

      // User clicked on the drop-down arrow.

      T := GetTickCount;
      if ( FDropTime > 0 ) and ( T < FDropTime + MinDelay ) then
      begin
        // If GetTickCount < FDropTime + MinDelay then this MouseDown method call
        // is resulting from a "second" click on the button
        if T < FDropTime + MinDelay then
        begin
          FTreatAsNormal := True;
        end;
        FDropTime := 0;
        ReleaseCapture;
      end
      else
      begin
        FState := tbsDropDown;
        Repaint;
        DoDropDown;
        FTreatAsNormal := False;
        FDropTime := GetTickCount;
      end;
      MouseUp( Button, Shift, X, Y );
    end;
  end;
end; {= TRzToolButton.MouseDown =}



procedure TRzToolButton.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  NewState: TRzToolButtonState;
begin
  inherited;

  if FDragging then
  begin
    if not FDown then
      NewState := tbsUp
    else
      NewState := tbsExclusive;

    if ( X >= 0 ) and ( X < ClientWidth ) and
       ( Y >= 0 ) and ( Y < ClientHeight ) then
    begin
      if FDown then
        NewState := tbsExclusive
      else
        NewState := tbsDown;
    end;

    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseOverButton then
    UpdateTracking;
end; {= TRzToolButton.MouseMove =}


function TRzToolButton.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzToolButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  DoClick, CurrentlyDown: Boolean;
begin
  inherited;
  if FDragging then
  begin
    FDragging := False;
    DoClick := ( X >= 0 ) and ( X < ClientWidth ) and
               ( Y >= 0 ) and ( Y < ClientHeight );

    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := tbsUp;
      FMouseOverButton := False;
      if DoClick and not ( FState in [ tbsExclusive, tbsDown ] ) then
        Repaint;
    end
    else
    begin
      if DoClick then
      begin
        CurrentlyDown := FDown;
        SetDown( not FDown );
        // If no change in down state, user clicked on an exclusive button already down. In this case, do not cause
        // OnClick event to fire as this will cause the associated action to fire.
        if CurrentlyDown = FDown then
          DoClick := False;
        if FDown then
          FState := tbsExclusive;
        if FDown then
          Repaint;
      end
      else
      begin
        if FDown then
          FState := tbsExclusive;
        Repaint;
      end;
    end;
    if DoClick then
      Click;
    UpdateTracking;
  end;

  if FToolStyle = tsDropDown then
  begin
    if not PtInRect( ClientRect, CursorPosition ) then
      FTreatAsNormal := True;
    FState := tbsUp;
    Repaint;
    UpdateTracking;
  end;
end; {= TRzToolButton.MouseUp =}


procedure TRzToolButton.Click;
begin
  if FToolStyle = tsDropDown then
  begin
    if FTreatAsNormal then
      inherited Click;
  end
  else
    inherited Click;
end;


procedure TRzToolButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  if IsAccel( Msg.CharCode, Caption ) and Enabled and Visible and ( Parent <> nil ) and Parent.Showing then
  begin
    if ( FToolStyle = tsButton ) or
       ( ( FToolStyle = tsDropDown ) and ( Assigned( OnClick ) or ( Action <> nil ) ) ) then
    begin
      Click;
    end
    else if FToolStyle = tsDropDown then
    begin
      DoDropDown;
    end;

    Msg.Result := 1;
  end
  else
    inherited;
end;


procedure TRzToolButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos( P );
    FMouseOverButton := not ( FindDragTarget( P, True ) = Self );
    if FMouseOverButton then
      Perform( cm_MouseLeave, 0, 0 )
    else
      Perform( cm_MouseEnter, 0, 0 );
  end;
end;


procedure TRzToolButton.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  FMouseOverButton := True;

  inherited;

  Refresh;
end;


procedure TRzToolButton.CMMouseLeave( var Msg: TMessage );
begin
  if not FInDoDropDown then
    FMouseOverButton := False;

  inherited;

  Refresh;
end;


procedure TRzToolButton.DrawBtnBorder( var R: TRect );
var
  ElementDetails: TThemedElementDetails;
  ThemeRect: TRect;
  FC, StartColor, StopColor: TColor;


  function DrawButtonFace: Boolean;
  begin
    Result := ( FState in [ tbsDown, tbsDropDown, tbsExclusive ] ) or
              ( FMouseOverButton and ( FState <> tbsDisabled ) ) or
              ( csDesigning in ComponentState );
  end;

begin
  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    ThemeRect := R;
    if FFlat then
    begin
      if ( FToolStyle = tsButton ) or ( ( FToolStyle = tsDropDown ) and not Assigned( OnClick ) and ( Action = nil ) ) then
      begin
        // Draw as normal tool button if tsButton or no OnClick handler
        if DrawButtonFace then
        begin
          if FState in [ tbsDown, tbsDropDown ] then
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonPressed )
          else if FState = tbsExclusive then
          begin
            if FMouseOverButton then
              ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonCheckedHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonChecked );
          end
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonHot );
        end
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonNormal );
      end
      else // FToolStyle = tsDropDown and there is an OnClick event handler
      begin
        if UseRightToLeftAlignment then
          ThemeRect.Left := ThemeRect.Left + ArrowRegionWidth
        else
          ThemeRect.Right := ThemeRect.Right - ArrowRegionWidth;

        if DrawButtonFace then
        begin
          if FState = tbsDown then
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonPressed )
          else if FState = tbsExclusive then
          begin
            if FMouseOverButton then
              ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonCheckedHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonChecked );
          end
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonHot );
        end
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonNormal );
      end;
    end
    else // not Flat
    begin
      if Enabled then
      begin
        if DrawButtonFace then
        begin
          if FState in [ tbsDown, tbsDropDown, tbsExclusive ] then
            ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonPressed )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonHot );
        end
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonNormal );
      end
      else
        ElementDetails := ActiveStyleServices.GetElementDetails( tbPushButtonDisabled );
    end;

    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, ThemeRect );

  end
  else // no XP Themes
  begin
    if DrawButtonFace or not FFlat then
    begin
      case FVisualStyle of
        vsClassic:
        begin
          if FState in [ tbsDown, tbsExclusive ] then
            R := DrawBorder( Canvas, R, fsLowered )
          else
            R := DrawBorder( Canvas, R, fsButtonUp );
        end;

        vsWinXP:
        begin
          if FState in [ tbsDown, tbsExclusive ] then
            R := DrawBorder( Canvas, R, fsStatus )
          else
            R := DrawBorder( Canvas, R, fsPopup );
        end;

        vsGradient:
        begin
          if FGradientColorStyle <> gcsCustom then
          begin
            GetGradientSelectionColors( FGradientColorStyle, FC, StartColor, StopColor );
          end
          else
          begin
            FC := FSelectionFrameColor;
            StartColor := FSelectionColorStart;
            StopColor := FSelectionColorStop;
          end;

          if csDesigning in ComponentState then
          begin
            InflateRect( R, -1, -1 );
            R := DrawBoxCorners( Canvas, R, FC, 4 );
          end
          else
            R := DrawBox( Canvas, R, FC );

          if FState in [ tbsDown, tbsDropDown ] then
            PaintGradient( Canvas, R, gdHorizontalEnd, StopColor, StartColor )
          else if FState = tbsExclusive then
          begin
            if FMouseOverButton then
            begin
              if FDown then
                PaintGradient( Canvas, R, gdHorizontalEnd, StopColor, StartColor )
              else
                PaintGradient( Canvas, R, gdHorizontalEnd, StartColor, StopColor )
            end
            else
              PaintGradient( Canvas, R, gdHorizontalEnd, DarkerColor( StopColor, 40 ), StartColor );
          end
          else
          begin
            if not ( csDesigning in ComponentState ) then
              PaintGradient( Canvas, R, gdHorizontalEnd, StartColor, StopColor );
          end;
        end;
      end; // case FVisualStyle
    end;
  end;
end; {= TRzToolButton.DrawBtnBorder =}


function TRzToolButton.ImageList: TCustomImageList;
begin
  if ( FImages = nil ) and ( Parent <> nil ) and ( Parent is TRzToolbar ) and ( TRzToolbar( Parent ).Images <> nil ) then
    Result := TRzToolbar( Parent ).Images
  else
    Result := FImages;
end;


function TRzToolButton.DisabledImageList: TCustomImageList;
begin
  if ( FDisabledImages = nil ) and ( Parent <> nil ) and ( Parent is TRzToolbar ) and ( TRzToolbar( Parent ).DisabledImages <> nil ) then
    Result := TRzToolbar( Parent ).DisabledImages
  else
    Result := FDisabledImages;
end;


function TRzToolButton.GetImageSize: TPoint;
begin
  if ( FImageIndex <> -1 ) and ( ImageList <> nil ) then
    Result := Point( ImageList.Width, ImageList.Height )
  else
    Result := Point( 0, 0 );
end;


procedure TRzToolButton.GetImageAndCaptionRects( var ImageRect, CaptionRect: TRect );
var
  ImageSize: TPoint;
  TextHeight, TextWidth, ImageLeft, ImageTop, TextLeft, TextTop: Integer;
  AlignmentOffset: Integer;
  BtnRect, TextRect: TRect;
begin
  BtnRect := ClientRect;

  if FToolStyle = tsDropDown then
  begin
    if UseRightToLeftAlignment then
      Inc( BtnRect.Left, ArrowRegionWidth )
    else
      Dec( BtnRect.Right, ArrowRegionWidth );
  end;

  InflateRect( BtnRect, -2, -2 );
  ImageSize := GetImageSize;

  Canvas.Font := Self.Font;

  AlignmentOffset := 0;

  if FShowCaption and ( Caption <> '' ) then
  begin
    TextRect := BtnRect;

    case FLayout of
      blGlyphLeft:
        Inc( TextRect.Left, ImageSize.X );

      blGlyphRight:
        Dec( TextRect.Right, ImageSize.X );

      blGlyphTop:
        Inc( TextRect.Top, ImageSize.Y );

      blGlyphBottom:
        Dec( TextRect.Bottom, ImageSize.Y );
    end;

    DrawString( Canvas, Caption, TextRect, dt_CalcRect or dt_WordBreak or dt_ExpandTabs or
                                           dt_VCenter or dt_Center );

    // TextRect now has the bound dimensions for the text
    TextWidth := TextRect.Right - TextRect.Left;
    TextHeight := TextRect.Bottom - TextRect.Top;


    // Determine position of Image and Text

    case FLayout of
      blGlyphLeft:
      begin
        ImageLeft := BtnRect.Left + ( ( BtnRect.Right - BtnRect.Left ) - ( ImageSize.X + FSpacing + TextWidth ) ) div 2;
        ImageTop := BtnRect.Top + ( ( BtnRect.Bottom - BtnRect.Top ) - ImageSize.Y ) div 2;

        TextLeft := ImageLeft + ImageSize.X + FSpacing;
        TextTop := BtnRect.Top + ( ( BtnRect.Bottom - BtnRect.Top ) - TextHeight ) div 2;

        if ImageSize.X <> 0 then
          AlignmentOffset := ImageLeft - FSpacing
        else
          AlignmentOffset := ImageLeft;
      end;

      blGlyphRight:
      begin
        TextLeft := BtnRect.Left + ( ( BtnRect.Right - BtnRect.Left ) - ( ImageSize.X + FSpacing + TextWidth ) ) div 2;
        TextTop := BtnRect.Top + ( ( BtnRect.Bottom - BtnRect.Top ) - TextHeight ) div 2;

        ImageLeft := TextLeft + TextWidth + FSpacing;
        ImageTop := BtnRect.Top + ( ( BtnRect.Bottom - BtnRect.Top ) - ImageSize.Y ) div 2;


        if ImageSize.X <> 0 then
          AlignmentOffset := TextLeft - FSpacing
        else
          AlignmentOffset := TextLeft;
      end;

      blGlyphTop:
      begin
        ImageLeft := BtnRect.Left +
                     ( ( BtnRect.Right - BtnRect.Left ) - ImageSize.X ) div 2;
        ImageTop := BtnRect.Top +
                    ( ( BtnRect.Bottom - BtnRect.Top ) -
                      ( ImageSize.Y + FSpacing + TextHeight ) ) div 2;

        TextLeft := BtnRect.Left +
                    ( ( BtnRect.Right - BtnRect.Left ) - TextWidth ) div 2;
        TextTop := ImageTop + ImageSize.Y + FSpacing;

        AlignmentOffset := Min( ImageLeft, TextLeft ) - FSpacing;
      end;

      blGlyphBottom:
      begin
        ImageLeft := BtnRect.Left +
                     ( ( BtnRect.Right - BtnRect.Left ) - ImageSize.X ) div 2;
        ImageTop := BtnRect.Bottom - ImageSize.Y -
                    ( ( BtnRect.Bottom - BtnRect.Top ) -
                      ( ImageSize.Y + FSpacing + TextHeight ) ) div 2;

        TextLeft := BtnRect.Left +
                    ( ( BtnRect.Right - BtnRect.Left ) - TextWidth ) div 2;
        TextTop := ImageTop - TextHeight - FSpacing;

        AlignmentOffset := Min( ImageLeft, TextLeft ) - FSpacing;
      end;
      
    else
      ImageLeft := BtnRect.Left;
      ImageTop := BtnRect.Top;
      TextLeft := BtnRect.Left;
      TextTop := BtnRect.Top;
    end;

    ImageRect := Rect( ImageLeft, ImageTop,
                       ImageLeft + ImageSize.X, ImageTop + ImageSize.Y );
    CaptionRect := Rect( TextLeft, TextTop,
                         TextLeft + TextWidth, TextTop + TextHeight );

  end
  else // No Caption Visible
  begin
    ImageRect := CenterRect( BtnRect, ImageSize.X, ImageSize.Y );
    CaptionRect := Rect( 0, 0, 0, 0 );
    AlignmentOffset := ImageRect.Left - FSpacing;
  end;

  // Offset Rectangles for alignment
  if FAlignment = taLeftJustify then
  begin
    OffsetRect( ImageRect, -AlignmentOffset, 0 );
    OffsetRect( CaptionRect, -AlignmentOffset, 0 );
  end
  else if FAlignment = taRightJustify then
  begin
    OffsetRect( ImageRect, AlignmentOffset, 0 );
    OffsetRect( CaptionRect, AlignmentOffset, 0 );
  end;


  if ( FState in [ tbsDown, tbsExclusive ] ) or
     ( ( FState in [ tbsDropDown ] ) and not Assigned( OnClick ) and
       ( Action = nil ) ) then
  begin
    if FVisualStyle <> vsGradient then
    begin
      if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
      begin
        OffsetRect( ImageRect, 1, 0 );
        OffsetRect( CaptionRect, 1, 0 );
      end
      else
      begin
        OffsetRect( ImageRect, 1, 1 );
        OffsetRect( CaptionRect, 1, 1 );
      end;
    end;
  end;
end; {= TRzToolButton.GetImageAndCaptionRects =}


procedure TRzToolButton.DrawImage( R: TRect );
var
  ImgIdx, L, T: Integer;

  function ButtonIsDown: Boolean;
  begin
    Result := ( FState in [ tbsDown, tbsExclusive ] ) or
              ( ( FState in [ tbsDropDown ] ) and not Assigned( OnClick ) and
                ( Action = nil ) );
  end;

begin
  if ( ImageList <> nil ) and ( FImageIndex <> -1 ) then
  begin
    L := R.Left;
    T := R.Top;

    if Enabled then
    begin
      if ( FDownIndex <> -1 ) and ButtonIsDown then
        ImgIdx := FDownIndex
      else if ( FHotIndex <> -1 ) and FMouseOverButton then
        ImgIdx := FHotIndex
      else
        ImgIdx := FImageIndex;

      ImageList.Draw( Canvas, L, T, ImgIdx );
    end
    else // Not Enabled
    begin
      if FDisabledIndex <> -1 then
        ImageList.Draw( Canvas, L, T, FDisabledIndex )
      else if DisabledImageList <> nil then
        DisabledImageList.Draw( Canvas, L, T, FImageIndex )
      else
        ImageList.Draw( Canvas, L, T, FImageIndex, False );
    end;
  end;
end; {= TRzToolButton.DrawImage =}



procedure TRzToolButton.DrawArrow;
var
  X, Y: Integer;
  R: TRect;
  ElementDetails: TThemedElementDetails;
  FC, C1, C2: TColor;

  procedure DrawTriangle;
  begin
    // Draw Actual Arrow
    
    if Enabled then
    begin
      if ( FVisualStyle = vsGradient ) and ( FGradientColorStyle = gcsCustom ) then
      begin
        Canvas.Brush.Color := Self.Font.Color;
        Canvas.Pen.Color := Self.Font.Color;
      end
      else
      begin
        Canvas.Brush.Color := clWindowText;
        Canvas.Pen.Color := clWindowText;
      end;
    end
    else
    begin
      Canvas.Brush.Color := clBtnShadow;
      Canvas.Pen.Color := clBtnShadow;
    end;

    if UseRightToLeftAlignment then
      X := 6
    else
      X := ClientRect.Right - 8;
    Y := ClientRect.Top + ( ClientRect.Bottom - ClientRect.Top ) div 2 + 2;
    if ( FVisualStyle <> vsGradient ) and ( FState in [ tbsDown, tbsDropDown ] ) then
    begin
      Inc( X );
      Inc( Y );
    end;
    Canvas.Polygon( [ Point( X, Y ), Point( X - 2, Y - 2 ), Point( X + 2, Y - 2 ) ] );
  end;

begin {= TRzToolButton.DrawArrow =}

  if UseRightToLeftAlignment then
    X := ArrowRegionWidth
  else
    X := Width - ArrowRegionWidth;

  if ( FVisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    if ( FToolStyle = tsButton ) or
       ( ( FToolStyle = tsDropDown ) and not Assigned( OnClick ) and
         ( Action = nil ) ) then
    begin
      DrawTriangle;
    end
    else
    begin
      R := ClientRect;
      if UseRightToLeftAlignment then
      begin
        if Assigned( OnClick ) or ( Action <> nil ) then
          R.Right := R.Left + ArrowRegionWidth;
      end
      else
      begin
        if Assigned( OnClick ) or ( Action <> nil ) then
          R.Left := R.Right - ArrowRegionWidth;
      end;

      if Enabled then
      begin
        if ( FState in [ tbsDown, tbsDropDown ] ) or
           ( FMouseOverButton and ( FState <> tbsDisabled ) ) or
           ( csDesigning in ComponentState ) then
        begin
          if FState in [ tbsDown, tbsDropDown ] then
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonDropDownPressed )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonDropDownHot );
        end
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonDropDownNormal );
      end
      else
        ElementDetails := ActiveStyleServices.GetElementDetails( ttbSplitButtonDropDownDisabled );
      ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
    end;
  end
  else // No Themes
  begin
    if FState <> tbsDropDown then
    begin
      if ( FFlat and FMouseOverButton and Enabled ) or
         ( not FFlat and Enabled ) then
      begin
        if ( FVisualStyle <> vsGradient ) and ( FState in [ tbsDown ] ) then
          Inc( X );

        if Assigned( OnClick ) or ( Action <> nil ) then
        begin
          // Only show divider line if there is an OnClick event handler
          if FVisualStyle = vsGradient then
          begin
            GetGradientSelectionColors( FGradientColorStyle, FC, C1, C2 );
            Canvas.Pen.Color := FC;
            Canvas.MoveTo( X - 1, 0 );
            Canvas.LineTo( X - 1, Height );
          end
          else
          begin
            if FState <> tbsDown then
            begin
              Canvas.Pen.Color := clBtnHighlight;
              Canvas.MoveTo( X, 0 );
              Canvas.LineTo( X, Height );
            end;
            Canvas.Pen.Color := clBtnShadow;
            Canvas.MoveTo( X - 1, 0 );
            Canvas.LineTo( X - 1, Height );
          end;
        end;
      end;
    end
    else { FState = tbsDropDown }
    begin
      R := ClientRect;

      if FVisualStyle = vsGradient then
      begin
        if FGradientColorStyle <> gcsCustom then
        begin
          GetGradientSelectionColors( FGradientColorStyle, FC, C1, C2 );
        end
        else
        begin
          FC := FSelectionFrameColor;
          C1:= FSelectionColorStart;
          C2 := FSelectionColorStop;
        end;

        if Assigned( OnClick ) or ( Action <> nil ) then
        begin
          if UseRightToLeftAlignment then
            R.Right := R.Left + ArrowRegionWidth + 1
          else
            R.Left := R.Right - ArrowRegionWidth - 1;
        end;
        R := DrawBox( Canvas, R, FC );
        PaintGradient( Canvas, R, gdHorizontalEnd, C2, C1 );
      end
      else
      begin
        if Assigned( OnClick ) or ( Action <> nil ) then
        begin
          if UseRightToLeftAlignment then
            R.Right := R.Left + ArrowRegionWidth
          else
            R.Left := R.Right - ArrowRegionWidth;
        end;
        DrawBorder( Canvas, R, fsStatus );
      end;
    end;

    DrawTriangle;
  end;

end; {= TRzToolButton.DrawArrow =}


procedure TRzToolButton.DrawCaption( R: TRect );
var
  Flags: Cardinal;
begin
  if FShowCaption then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font := Self.Font;
    if UsingSystemStyle then
    begin
      if ( FVisualStyle <> vsGradient ) and ( FState = tbsDown ) then
        Canvas.Font.Color := clHighlightText;
      if not Enabled then
        Canvas.Font.Color := clBtnShadow;
    end
    else // VCL Styles
    begin
      if Enabled then
        Canvas.Font.Color := ActiveStyleFontColor( sfToolItemTextNormal )
      else
        Canvas.Font.Color := ActiveStyleFontColor( sfToolItemTextDisabled );
    end;

    Flags := dt_WordBreak or dt_ExpandTabs or dt_VCenter or dt_Center;
    if UseRightToLeftAlignment then
      Flags := Flags or dt_RtlReading;

    DrawString( Canvas, Caption, R, Flags );

    Canvas.Brush.Style := bsSolid;
  end;
end; {= TRzToolButton.DrawCaption =}


procedure TRzToolButton.Paint;
var
  R, ImageRect, CaptionRect: TRect;
begin
  if not Enabled then
  begin
    FState := tbsDisabled;
    FDragging := False;
  end
  else if FState = tbsDisabled then
  begin
    if FDown and ( GroupIndex <> 0 ) then
      FState := tbsExclusive
    else
      FState := tbsUp;
  end;

  R := ClientRect;

  DrawBtnBorder( R );

  if not FTransparent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect( R );
  end;

  if FVisualStyle <> vsGradient then
  begin
    // Show dithered background if necessary
    if ( FVisualStyle = vsClassic ) or not ActiveStyleServicesEnabled then
    begin
      if ( FState = tbsExclusive ) and ( not FFlat or not FMouseOverButton ) then
      begin
        if FTransparent then
          Canvas.Brush.Bitmap := AllocPatternBitmap( clBtnFace, clBtnHighlight )
        else
          Canvas.Brush.Bitmap := AllocPatternBitmap( Color, LighterColor( Color, 20 ) );
        Canvas.FillRect( R );
      end;
    end;
  end;

  if FToolStyle = tsDropDown then
    DrawArrow;

  GetImageAndCaptionRects( ImageRect, CaptionRect );

  DrawImage( ImageRect );
  DrawCaption( CaptionRect );
end; {= TRzToolButton.Paint =}


procedure TRzToolButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if ( FGroupIndex <> 0 ) and ( Parent <> nil ) then
  begin
    Msg.Msg := cm_ButtonPressed;
    Msg.WParam := WParam( FGroupIndex );
    Msg.LParam := LParam( Self );
    Msg.Result := 0;
    Parent.Broadcast( Msg );
  end;
end;


procedure TRzToolButton.CMButtonPressed( var Msg: TMessage );
var
  Sender: TRzToolButton;
begin
  if Msg.WParam = WParam( FGroupIndex ) then
  begin
    Sender := TRzToolButton( Msg.LParam );
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := tbsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;


procedure TRzToolButton.SetAllowAllUp( Value: Boolean );
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;


procedure TRzToolButton.SetAlignment( Value: TAlignment );
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetGradientColorStyle( Value: TRzGradientColorStyle );
begin
  if FGradientColorStyle <> Value then
  begin
    FGradientColorStyle := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetSelectionColorStart( Value: TColor );
begin
  if FSelectionColorStart <> Value then
  begin
    FSelectionColorStart := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetSelectionColorStop( Value: TColor );
begin
  if FSelectionColorStop <> Value then
  begin
    FSelectionColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetSelectionFrameColor( Value: TColor );
begin
  if FSelectionFrameColor <> Value then
  begin
    FSelectionFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetDown( Value: Boolean );
begin
  if FGroupIndex = 0 then
    Value := False;

  if FDown <> Value then
  begin
    if FDown and ( not FAllowAllUp ) then
      Exit;

    FDown := Value;
    if Value then
    begin
      if FState = tbsUp then
        Invalidate;
      FState := tbsExclusive;
    end
    else
    begin
      FState := tbsUp;
      Repaint;
    end;
    if Value then
      UpdateExclusive;
  end;
end;


procedure TRzToolButton.SetFlat( Value: Boolean );
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Repaint;
  end;
end;


procedure TRzToolButton.SetGroupIndex( Value: Integer );
begin
  if FGroupIndex <> Value then
  begin
    if FToolStyle = tsDropDown then
      FGroupIndex := 0
    else
      FGroupIndex := Value;
    UpdateExclusive;
  end;
end;


procedure TRzToolButton.SetHotIndex( Value: TImageIndex );
begin
  if FHotIndex <> Value then
  begin
    FHotIndex := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetDownIndex( Value: TImageIndex );
begin
  if FDownIndex <> Value then
  begin
    FDownIndex := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetDisabledIndex( Value: TImageIndex );
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.DropDown;
begin
  if Assigned( FOnDropDown ) then
    FOnDropDown( FDropDownMenu );
end;


procedure TRzToolButton.SetDropDownMenu( Value: TPopupMenu );
begin
  if FDropDownMenu <> Value then
  begin
    FDropDownMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzToolButton.SetToolStyle( Value: TRzToolStyle );
var
  SaveFlag: Boolean;
begin
  if FToolStyle <> Value then
  begin
    FToolStyle := Value;
    FGroupIndex := 0;
    if not ( csLoading in ComponentState ) then
    begin
      SaveFlag := FUseToolbarButtonSize;
      if FToolStyle = tsButton then
        Width := Width - ArrowRegionWidth
      else
        Width := Width + ArrowRegionWidth;
      FUseToolbarButtonSize := SaveFlag;
    end;
    Invalidate;
  end;
end;


function TRzToolButton.IsSizeStored: Boolean;
begin
  if not FUseToolbarButtonSize then
    Result := True
  else if FUseToolbarButtonSize and ( Parent <> nil ) and ( Parent is TRzToolbar ) then
  begin
    // Store size if the button is not the same size as the button size defined by the toolbar.
    // This can happen when the text is too long to fit in the display.
    Result := ( Width <> TRzToolbar( Parent ).ButtonWidth ) or ( Height <> TRzToolbar( Parent ).ButtonHeight );
  end
  else
    Result := True;
end;


procedure TRzToolButton.SetTransparent( Value: Boolean );
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetUseToolbarButtonSize( Value: Boolean );
begin
  if FUseToolbarButtonSize <> Value then
  begin
    FUseToolbarButtonSize := Value;
    if ( Parent <> nil ) and not ( csReading in ComponentState ) then
      Perform( cm_ToolbarButtonSizeChanged, 0, 0 );
    Invalidate;
  end;
end;


function TRzToolButton.GetWidth: Integer;
begin
  Result := inherited Width;
end;


procedure TRzToolButton.SetWidth( Value: Integer );
begin
  if Width <> Value then
  begin
    inherited Width := Value;
    if not ( csLoading in ComponentState ) then
      FUseToolbarButtonSize := False;
    Invalidate;
  end;
end;


function TRzToolButton.GetHeight: Integer;
begin
  Result := inherited Height;
end;


procedure TRzToolButton.SetHeight( Value: Integer );
begin
  if Height <> Value then
  begin
    inherited Height := Value;
    if not ( csLoading in ComponentState ) then
      FUseToolbarButtonSize := False;
    Invalidate;
  end;
end;


function TRzToolButton.IsLayoutStored: Boolean;
begin
  Result := not FUseToolbarButtonLayout;
end;


procedure TRzToolButton.SetUseToolbarButtonLayout( Value: Boolean );
begin
  if FUseToolbarButtonLayout <> Value then
  begin
    FUseToolbarButtonLayout := Value;
    if ( Parent <> nil ) and not ( csReading in ComponentState ) then
      Perform( cm_ToolbarButtonLayoutChanged, 0, 0 );
    Invalidate;
  end;
end;



procedure TRzToolButton.SetLayout( Value: TButtonLayout );
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    if not ( csReading in ComponentState ) then
      FUseToolbarButtonLayout := False;
    Invalidate;
  end;
end;


procedure TRzToolButton.CMToolbarButtonLayoutChanged( var Msg: TMessage );
begin
  if FUseToolbarButtonLayout and ( Parent is TRzToolbar ) then
  begin
    SetLayout( TRzToolbar( Parent ).ButtonLayout );
    // Calling SetLayout sets FUseToolbarButtonLayout to False.
    // We need to reset it back to True.
    FUseToolbarButtonLayout := True;
  end;
end;


procedure TRzToolButton.CMToolbarButtonSizeChanged( var Msg: TMessage );
var
  W: Integer;
  R1, R2: TRect;
begin
  if FUseToolbarButtonSize and ( Parent is TRzToolbar ) then
  begin
    W := TRzToolbar( Parent ).ButtonWidth;
    if FShowCaption then
    begin
      Canvas.Font := Self.Font;
      if Layout in [ blGlyphTop, blGlyphBottom ] then
        W := Max( W, Canvas.TextWidth( Caption ) + 8 )
      else
      begin
        GetImageAndCaptionRects( R1, R2 );
        W := Max( W, Canvas.TextWidth( Caption ) + 12 + GetImageSize.X );
      end;
    end;
    if FToolStyle <> tsButton then
      W := W + ArrowRegionWidth;
    SetWidth( W );
    SetHeight( TRzToolbar( Parent ).ButtonHeight );
    // Calling SetWidth and SetHeight set FUseToolbarButtonSize to False.
    // We need to reset it back to True.
    FUseToolbarButtonSize := True;
  end;
end;


procedure TRzToolButton.CMToolbarShowCaptionChanged( var Msg: TMessage );
begin
  if FUseToolbarShowCaption and ( Parent is TRzToolbar ) then
  begin
    SetShowCaption( TRzToolbar( Parent ).ShowButtonCaptions );
    // Calling SetShowCaption sets FUseToolbarShowCaption to False.
    // We need to reset it back to True.
    FUseToolbarShowCaption := True;
  end;
end;


procedure TRzToolButton.CMToolbarVisualStyleChanged( var Msg: TMessage );
begin
  if FUseToolbarVisualStyle and ( Parent is TRzToolbar ) then
  begin
    SetVisualStyle( TRzToolbar( Parent ).VisualStyle );
    SetGradientColorStyle( TRzToolbar( Parent ).GradientColorStyle );
    // Calling SetVisualStyle and SetGradientColorStyle set
    // FUseToolbarVisualStyle to False. We need to reset it back to True.
    FUseToolbarVisualStyle := True;
  end;
end;


function TRzToolButton.IsShowCaptionStored: Boolean;
begin
  Result := not UseToolbarShowCaption;
end;


procedure TRzToolButton.SetUseToolbarShowCaption( Value: Boolean );
begin
  if FUseToolbarShowCaption <> Value then
  begin
    FUseToolbarShowCaption := Value;
    if ( Parent <> nil ) and not ( csReading in ComponentState ) then
      Perform( cm_ToolbarShowCaptionChanged, 0, 0 );
    Invalidate;
  end;
end;


procedure TRzToolButton.SetShowCaption( Value: Boolean );
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    FUseToolbarShowCaption := False;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetSpacing( Value: Integer );
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;


procedure TRzToolButton.SetImages( Value: TCustomImageList );
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


procedure TRzToolButton.ImagesChange( Sender: TObject );
begin
  if Sender = Images then
  begin
    CheckMinSize;
    Invalidate;
  end;
end;


procedure TRzToolButton.CheckMinSize;
begin
  // Ensures button area will display entire image
  if FImages.Width > Width then
    Width := FImages.Width;
  if FImages.Height > Height then
    Height := FImages.Height;
end;


procedure TRzToolButton.SetDisabledImages( Value: TCustomImageList );
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


procedure TRzToolButton.DisabledImagesChange( Sender: TObject );
begin
  if Sender = DisabledImages then
  begin
    Invalidate;
  end;
end;


function TRzToolButton.IsCheckedStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not TRzToolButtonActionLink( ActionLink ).IsCheckedLinked;
end;

function TRzToolButton.IsImageIndexStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not TRzToolButtonActionLink( ActionLink ).IsImageIndexLinked;
end;


procedure TRzToolButton.ActionChange( Sender: TObject; CheckDefaults: Boolean );
begin
  inherited;
  if Sender is TCustomAction then
    with TCustomAction (Sender ) do
    begin
      if not CheckDefaults or ( Self.Down = False ) then
        Self.Down := Checked;
      if not CheckDefaults or ( Self.ImageIndex = -1 ) then
        Self.ImageIndex := ImageIndex;
    end;
end;


function TRzToolButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TRzToolButtonActionLink;
end;


procedure TRzToolButton.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TCustomAction then
    with TCustomAction( Dest ) do
    begin
      Checked := Self.Down;
      ImageIndex := Self.ImageIndex;
    end;
end;


procedure TRzToolButton.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  PickupToolbarStyles;
  Invalidate;
end;


function TRzToolButton.IsVisualStyleStored: Boolean;
begin
  Result := not UseToolbarVisualStyle;
end;


procedure TRzToolButton.SetUseToolbarVisualStyle( Value: Boolean );
begin
  if FUseToolbarVisualStyle <> Value then
  begin
    FUseToolbarVisualStyle := Value;
    if ( Parent <> nil ) and not ( csReading in ComponentState ) then
      Perform( cm_ToolbarVisualStyleChanged, 0, 0 );
    Invalidate;
  end;
end;


procedure TRzToolButton.SetVisualStyle( Value: TRzVisualStyle );
begin
  if FVisualStyle <> Value then
  begin
    FVisualStyle := Value;
    FUseToolbarVisualStyle := False;
    Invalidate;
  end;
end;




{==============================}
{== TRzControlButton Methods ==}
{==============================}

constructor TRzControlButton.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := [ csCaptureMouse ];

  Width := 17;
  Height := 21;

  Color := clBtnFace;
  FMouseOverButton := False;
  FFlat := False;

  DragMode := dmManual;

  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChangedHandler;
  FNumGlyphs := 1;

  FRepeatClicks := False;
  FInitialDelay := 400;                                    // 400 milliseconds
  FDelay := 100;                                           // 100 milliseconds
  {&RCI}
  {&RV}
end;


destructor TRzControlButton.Destroy;
begin
  FGlyph.Free;
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited;
end;


procedure TRzControlButton.Click;
begin
  inherited;
end;


procedure TRzControlButton.GlyphChangedHandler( Sender: TObject );
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
end;


procedure TRzControlButton.SetGlyph( Value: TBitmap );
begin
  {&RV}
  FGlyph.Assign( Value );
end;


procedure TRzControlButton.SetNumGlyphs( Value: TNumGlyphs );
begin
  if FNumGlyphs <> Value then
  begin
    FNumGlyphs := Value;
    Invalidate;
  end;
end;


function TRzControlButton.GetImageSize: TPoint;
begin
  if FGlyph <> nil then
    Result := Point( FGlyph.Width div FNumGlyphs, FGlyph.Height )
  else
    Result := Point( 0, 0 );
end;


function TRzControlButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;


procedure TRzControlButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if ( Button = mbLeft ) and Enabled then
  begin
    if not FDown then
    begin
      FDown := True;
      Invalidate;
    end;
    FDragging := True;
  end;

  if FRepeatClicks then
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


function TRzControlButton.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzControlButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  DoClick: Boolean;
begin
  inherited;
  if FDragging then
  begin
    FDragging := False;
    DoClick := ( X >= 0 ) and ( X < ClientWidth ) and
               ( Y >= 0 ) and ( Y < ClientHeight );

    // Redraw face in-case mouse is captured
    FDown := False;
    FMouseOverButton := False;
    Repaint;
    if DoClick then
      Click;
    UpdateTracking;
  end;

  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled := False;
end;


procedure TRzControlButton.TimerExpired( Sender: TObject );
begin
  FRepeatTimer.Interval := FDelay;
  if FDown and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;


procedure TRzControlButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat or ActiveStyleServicesEnabled then
  begin
    if Enabled then
    begin
      GetCursorPos( P );
      FMouseOverButton := not ( FindDragTarget( P, True ) = Self );
      if FMouseOverButton then
        Perform( cm_MouseLeave, 0, 0 )
      else
        Perform( cm_MouseEnter, 0, 0 );
    end;
  end;
end;


procedure TRzControlButton.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  FMouseOverButton := True;

  inherited;

  if FFlat or ActiveStyleServicesEnabled then
    Refresh;
end;


procedure TRzControlButton.CMMouseLeave( var Msg: TMessage );
begin
  FMouseOverButton := False;

  inherited;

  if FFlat or ActiveStyleServicesEnabled then
    Refresh;
end;


procedure TRzControlButton.DrawBtnFace( var R: TRect );
var
  ElementDetails: TThemedElementDetails;
  ThemeRect, DestRect: TRect;
  MemImage: TBitmap;

  procedure DrawXPThemeButton( R: TRect; Style: TThemedScrollBar );
  begin
    ThemeRect := R;
    InflateRect( ThemeRect, 2, 2 );
    Inc( ThemeRect.Left );

    ElementDetails := ActiveStyleServices.GetElementDetails( Style );

    MemImage := TBitmap.Create;
    try
      MemImage.Width := ThemeRect.Right - ThemeRect.Left;
      MemImage.Height := ThemeRect.Bottom - ThemeRect.Top;

      ActiveStyleServices.DrawElement( MemImage.Canvas.Handle, ElementDetails, ThemeRect );

      DestRect := Rect( 2, 2, Width, Height );
      Canvas.CopyRect( DestRect, MemImage.Canvas, Rect( 0, 0, Width, Height ) );
      Canvas.Draw( 0, 0, MemImage );
    finally
      MemImage.Free;
    end;
  end;

  procedure DrawVistaThemeButton( R: TRect; Style: TThemedButton );
  begin
    ThemeRect := R;
    InflateRect( ThemeRect, 1, 1 );

    ElementDetails := ActiveStyleServices.GetElementDetails( Style );

    MemImage := TBitmap.Create;
    try
      MemImage.Width := ThemeRect.Right - ThemeRect.Left;
      MemImage.Height := ThemeRect.Bottom - ThemeRect.Top;

      MemImage.Canvas.Brush.Color := Color;
      MemImage.Canvas.FillRect( R );

      ActiveStyleServices.DrawElement( MemImage.Canvas.Handle, ElementDetails, ThemeRect );

      Canvas.Draw( 0, 0, MemImage );
    finally
      MemImage.Free;
    end;
  end;


  {$IFDEF VCL160_OR_HIGHER}
  procedure DrawVclStyledButton( R: TRect; Style: TThemedButton );
  begin
    ThemeRect := R;
    InflateRect( ThemeRect, 1, 1 );

    ElementDetails := StyleServices.GetElementDetails( Style );

    MemImage := TBitmap.Create;
    try
      MemImage.Width := ThemeRect.Right - ThemeRect.Left;
      MemImage.Height := ThemeRect.Bottom - ThemeRect.Top;

      MemImage.Canvas.Brush.Color := ActiveStyleColor( scEdit );
      MemImage.Canvas.FillRect( R );

      ActiveStyleServices.DrawElement( MemImage.Canvas.Handle, ElementDetails, ThemeRect );

      Canvas.Draw( 0, 0, MemImage );
    finally
      MemImage.Free;
    end;
  end;
  {$ENDIF}


begin {= TRzControlButton.DrawBtnFace =}
  if ActiveStyleServicesEnabled then
  begin
    if UsingSystemStyle then
    begin
      if FFlat then
      begin
        if FDown or ( FMouseOverButton and Enabled ) then
        begin
          if FDown then
          begin
            if RunningAtLeast( winVista ) then
              DrawVistaThemeButton( R, tbPushButtonPressed )
            else
              DrawXPThemeButton( R, tsThumbBtnHorzPressed );
          end
          else if FMouseOverButton and not ( csDesigning in ComponentState ) then
          begin
            if RunningAtLeast( winVista ) then
              DrawVistaThemeButton( R, tbPushButtonHot )
            else
              DrawXPThemeButton( R, tsThumbBtnHorzHot );
          end;
        end
        else
        begin
          Canvas.Brush.Color := Color;
          Canvas.FillRect( R );
        end;
      end
      else // not Flat
      begin
        if not Enabled then
        begin
          if RunningAtLeast( winVista ) then
            DrawVistaThemeButton( R, tbPushButtonDisabled )
          else
            DrawXPThemeButton( R, tsThumbBtnHorzDisabled )
        end
        else if FDown then
        begin
          if RunningAtLeast( winVista ) then
            DrawVistaThemeButton( R, tbPushButtonPressed )
          else
            DrawXPThemeButton( R, tsThumbBtnHorzPressed );
        end
        else if FMouseOverButton and not ( csDesigning in ComponentState ) then
        begin
          if RunningAtLeast( winVista ) then
            DrawVistaThemeButton( R, tbPushButtonHot )
          else
            DrawXPThemeButton( R, tsThumbBtnHorzHot );
        end
        else
        begin
          if RunningAtLeast( winVista ) then
            DrawVistaThemeButton( R, tbPushButtonNormal )
          else
            DrawXPThemeButton( R, tsThumbBtnHorzNormal );
        end;
      end;
    end
    else // VCL Styles
    begin
      {$IFDEF VCL160_OR_HIGHER}
      if FFlat then
      begin
        if FDown or ( FMouseOverButton and Enabled ) then
        begin
          if FDown then
            DrawVclStyledButton( R, tbPushButtonPressed )
          else if FMouseOverButton and not ( csDesigning in ComponentState ) then
            DrawVclStyledButton( R, tbPushButtonHot );
        end
        else
        begin
          Canvas.Brush.Color := ActiveStyleColor( scEdit );
          Canvas.FillRect( R );
        end;
      end
      else // not Flat
      begin
        if not Enabled then
          DrawVclStyledButton( R, tbPushButtonDisabled )
        else if FDown then
          DrawVclStyledButton( R, tbPushButtonPressed )
        else if FMouseOverButton and not ( csDesigning in ComponentState ) then
          DrawVclStyledButton( R, tbPushButtonHot )
        else
          DrawVclStyledButton( R, tbPushButtonNormal )
      end;
      {$ENDIF}
    end;

    InflateRect( R, -2, -2 );
  end
  else // No XP Themes
  begin
    if FFlat then
    begin
      if FDown or ( FMouseOverButton and Enabled ) or ( csDesigning in ComponentState ) then
      begin
        if FDown then
          R := DrawBorder( Canvas, R, fsStatus )
        else
          R := DrawBorder( Canvas, R, fsPopup );
      end;
      Canvas.Brush.Color := Color;
    end
    else
    begin
       if FDown then
        R := DrawBox( Canvas, R, clBtnShadow )
      else
        R := DrawBorder( Canvas, R, fsRaised );
      Canvas.Brush.Color := clBtnFace;
    end;

    Canvas.FillRect( R );
  end;
end; {= TRzControlButton.DrawBtnFace =}


procedure TRzControlButton.DrawSpinButton( var R: TRect );
type
  TRzSpinBtnState = ( sbsNormal, sbsDisabled, sbsHot, sbsPressed );
var
  ElementDetails: TThemedElementDetails;
  UIStyle: TRzUIStyle;

  procedure DrawThemeSpinButton( R: TRect; State: TRzSpinBtnState );
  begin
    case FStyle of
      cbsLeft:
      begin
        if UsingSystemStyle then
        begin
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsDownHorzNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsDownHorzDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsDownHorzHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsDownHorzPressed );
          end;
        end
        else // VCL Styles
        begin
          {$IFDEF VCL160_OR_HIGHER}
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnLeftNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnLeftDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnLeftHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnLeftPressed );
          end;
          {$ENDIF}
        end;
      end;

      cbsUp:
      begin
        if UsingSystemStyle then
        begin
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsUpNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsUpDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsUpHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsUpPressed );
          end;
        end
        else // VCL Styles
        begin
          {$IFDEF VCL160_OR_HIGHER}
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnUpNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnUpDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnUpHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnUpPressed );
          end;
          {$ENDIF}
        end;
      end;

      cbsRight:
      begin
        if UsingSystemStyle then
        begin
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsUpHorzNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsUpHorzDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsUpHorzHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsUpHorzPressed );
          end;
        end
        else // VCL Styles
        begin
          {$IFDEF VCL160_OR_HIGHER}
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnRightNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnRightDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnRightHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnRightPressed );
          end;
          {$ENDIF}
        end;
      end;

      cbsDown:
      begin
        if UsingSystemStyle then
        begin
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsDownNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsDownDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsDownHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsDownPressed );
          end;
        end
        else // VCL Styles
        begin
          {$IFDEF VCL160_OR_HIGHER}
          case State of
            sbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnDownNormal );
            sbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnDownDisabled );
            sbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnDownHot );
            sbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tsArrowBtnDownPressed );
          end;
          {$ENDIF}
        end;
      end;
    end;
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end;


  function StyleToDirection( S: TRzControlButtonStyle ): TDirection;
  begin
    Result := dirUp;
    case S of
      cbsLeft:  Result := dirLeft;
      cbsUp:    Result := dirUp;
      cbsRight: Result := dirRight;
      cbsDown:  Result := dirDown;
    end;
  end;

begin {= TRzControlButton.DrawSpinButton =}
  if ActiveStyleServicesEnabled then
  begin
    if FFlat then
    begin
      if FDown or ( FMouseOverButton and Enabled ) then
      begin
        if FDown then
          DrawThemeSpinButton( R, sbsPressed )
        else if FMouseOverButton and not ( csDesigning in ComponentState ) then
          DrawThemeSpinButton( R, sbsHot );
      end
      else
      begin
        if UsingSystemStyle then
          Canvas.Brush.Color := Color
        else
        begin
          Canvas.Brush.Color := ActiveStyleColor( scEdit );
        end;
        Canvas.FillRect( R );

        if UsingSystemStyle then
        begin
          if RunningAtLeast( winVista ) then
            UIStyle := uiWindowsVista
          else
            UIStyle := uiWindowsXP;
        end
        else // VCL Styles
        begin
          UIStyle := uiCustomVclStyle;
        end;
        DrawSpinArrow( Canvas, R, UIStyle, StyleToDirection( FStyle ), FDown, Enabled );
      end;
    end
    else // Not Flat
    begin
      if not Enabled then
        DrawThemeSpinButton( R, sbsDisabled )
      else if FDown then
        DrawThemeSpinButton( R, sbsPressed )
      else if FMouseOverButton and not ( csDesigning in ComponentState ) then
        DrawThemeSpinButton( R, sbsHot )
      else
        DrawThemeSpinButton( R, sbsNormal );

      if RunningAtLeast( winVista ) and ( R.Bottom - R.Top < 10 ) then
      begin
        Canvas.Pen.Color := GetXPThemeColor( xptcSpinButtonBorder );
        Canvas.MoveTo( R.Left + 1, R.Top );
        Canvas.LineTo( R.Right - 1, R.Top );
      end;
    end;

    InflateRect( R, -2, -2 );
  end
  else // No XP Themes
  begin
    if FFlat then
    begin
      if FDown or ( FMouseOverButton and Enabled ) or ( csDesigning in ComponentState ) then
      begin
        if FDown then
          R := DrawBorder( Canvas, R, fsStatus )
        else
          R := DrawBorder( Canvas, R, fsPopup );
      end;
      Canvas.Brush.Color := Color;
    end
    else
    begin
       if FDown then
        R := DrawBox( Canvas, R, clBtnShadow )
      else
        R := DrawBorder( Canvas, R, fsButtonUp );
      if ( Owner <> nil ) and ( Owner.Owner <> nil ) and ( Owner.Owner is TRzSpinEdit ) then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := Color;
    end;

    Canvas.FillRect( R );
    DrawSpinArrow( Canvas, R, uiWindows95, StyleToDirection( FStyle ), FDown, Enabled );
  end;
end; {= TRzControlButton.DrawSpinButton =}


procedure TRzControlButton.DrawDropDownButton( var R: TRect );
type
  TRzDropDownBtnState = ( ddbsNormal, ddbsDisabled, ddbsHot, ddbsPressed );
var
  ElementDetails: TThemedElementDetails;

  procedure DrawThemeDropDownButton( R: TRect; State: TRzDropDownBtnState );
  begin
    case State of
      ddbsNormal:   ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonNormal );
      ddbsDisabled: ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonDisabled );
      ddbsHot:      ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonHot );
      ddbsPressed:  ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonPressed );
    end;
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end;

begin {= TRzControlButton.DrawDropDownButton =}
  if ActiveStyleServicesEnabled then
  begin
    InflateRect( R, 1, 1 );
    if FFlat then
    begin
      if FDown or ( FMouseOverButton and Enabled ) or ( csDesigning in ComponentState ) then
      begin
        if FDown then
          DrawThemeDropDownButton( R, ddbsPressed )
        else
          DrawThemeDropDownButton( R, ddbsHot );
      end
      else
      begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect( R );

        DrawDropDownArrow( Canvas, R, uiWindowsXP, FDown, Enabled );
      end;
    end
    else // Not Flat
    begin
      if FDown or ( FMouseOverButton and Enabled ) or ( csDesigning in ComponentState ) then
      begin
        if FDown then
          DrawThemeDropDownButton( R, ddbsPressed )
        else
          DrawThemeDropDownButton( R, ddbsHot );
      end
      else if not Enabled then
        DrawThemeDropDownButton( R, ddbsDisabled )
      else
        DrawThemeDropDownButton( R, ddbsNormal );
    end;

    InflateRect( R, -2, -2 );
  end
  else // No XP Themes
  begin
    if FFlat then
    begin
      if FDown or ( FMouseOverButton and Enabled ) or ( csDesigning in ComponentState ) then
      begin
        if FDown then
          R := DrawBorder( Canvas, R, fsStatus )
        else
          R := DrawBorder( Canvas, R, fsPopup );
      end;
      Canvas.Brush.Color := Color;
    end
    else
    begin
       if FDown then
        R := DrawBox( Canvas, R, clBtnShadow )
      else
        R := DrawBorder( Canvas, R, fsButtonUp );
      Canvas.Brush.Color := clBtnFace;
    end;

    Canvas.FillRect( R );
    DrawDropDownArrow( Canvas, R, uiWindows95, FDown, Enabled );
  end;
end; {= TRzControlButton.DrawDropDownButton =}


procedure TRzControlButton.DrawGlyph( R: TRect );
var
  DestRct, SrcRct: TRect;
  DestBmp: TBitmap;
  W, H: Integer;
begin
  W := R.Right - R.Left;
  H := R.Bottom - R.Top;

  DestRct := Rect( 0, 0, W, H );

  if ( FNumGlyphs >= 3 ) and FDown then
    SrcRct := Rect( 2 * W, 0, 3 * W, H )
  else if ( FNumGlyphs >= 2 ) and not Enabled then
    SrcRct := Rect( W, 0, W + W, H )
  else
    SrcRct := Rect( 0, 0, W, H );

  // The DestBmp holds the desired region of the FGlyph bitmap

  DestBmp := TBitmap.Create;
  try
    DestBmp.Width := W;
    DestBmp.Height := H;
    DestBmp.Canvas.Brush.Color := Color;

    DestBmp.Canvas.CopyRect( DestRct, Canvas, R );
    DrawFullTransparentBitmap( DestBmp.Canvas, FGlyph, DestRct, SrcRct,
                               FGlyph.TransparentColor );
    Canvas.Draw( R.Left, R.Top, DestBmp );
  finally
    DestBmp.Free;
  end;
end; {= TRzControlButton.DrawGlyph =}


procedure TRzControlButton.Paint;
var
  R: TRect;
  GlyphSize: TPoint;
begin
  R := ClientRect;

  case FStyle of
    cbsNone:
    begin
      DrawBtnFace( R );

      if not FGlyph.Empty then
      begin
        InflateRect( R, -1, -1 );
        GlyphSize := GetImageSize;
        R := CenterRect( R, GlyphSize.X, GlyphSize.Y );
        OffsetRect( R, 1, 1 );
        if FDown then
        begin
          if ActiveStyleServicesEnabled then
            OffsetRect( R, 1, 0 )
          else
            OffsetRect( R, 1, 1 );
        end;

        DrawGlyph( R );
      end;
    end;

    cbsLeft, cbsUp, cbsRight, cbsDown:
    begin
      DrawSpinButton( R );
    end;

    cbsDropDown:
    begin
      DrawDropDownButton( R );
    end;
  end;
end; {= TRzControlButton.Paint =}


procedure TRzControlButton.SetStyle( Value: TRzControlButtonStyle );
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;


procedure TRzControlButton.SetFlat( Value: Boolean );
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    Repaint;
  end;
end;



{=======================================}
{== TRzShapeButton Support Procedures ==}
{=======================================}

type
  TRzPointArray = array[0..1] of Integer;


// Make a copy of a logical palette, returning the handle of the new palette.

function CopyPalette( SrcPalette: HPalette ): HPalette;
var
  Count: Cardinal;
  LogPal: PLogPalette;
begin
  Result := 0;
  Count := 0; // must init. because GetObject only passes back a 16-bit value

  // Is there a source palette? If not, then return zero.
  if SrcPalette = 0 then
    Exit;

  // Get the number of entries in the source palette.
  if GetObject( SrcPalette, SizeOf( Count ), @Count ) = 0 then
    raise Exception.Create( 'Invalid palette in CopyPalette' );

  if Count = 0 then
  begin
    // No entries is the equivalent of no palette.
    Result := 0;
    Exit;
  end;

  // TLogPalette already has room for one TPaletteEntry, so allocate
  // memory for an additional Count-1 entries.
  GetMem( LogPal, SizeOf( TLogPalette ) + ( Count-1 ) * SizeOf( TPaletteEntry ) );
  try
    // Get the palette entries from the source palette.
    if GetPaletteEntries( SrcPalette, 0, Count, LogPal^.palPalEntry ) <> Count then
      raise Exception.Create( 'Cannot get palette entries in CopyPalette' );
      
    LogPal^.palVersion := $300;
    LogPal^.palNumEntries := Count;
    // Create a new palette.
    Result := CreatePalette( LogPal^ );
    if Result = 0 then
      raise EOutOFResources.Create( 'Cannot create palette in CopyPalette' );
  finally
    FreeMem( LogPal, SizeOf( TLogPalette ) + ( Count-1 ) * SizeOf( TPaletteEntry ) );
  end;
end;


// Create a monochrome bitmap mask for use when overlaying images or
// when performing hit-testing.

function CreateMonoMask( ColorBmp: TBitmap; TransparentColor: TColor ): TBitmap;
var
  R: TRect;
  OldBkColor: TColorRef;
begin
  Result := TBitmap.Create;
  try
    Result.Monochrome := True;
    Result.Width := ColorBmp.Width;
    Result.Height := ColorBmp.Height;

    // Set background color for source bitmap -- this will be used
    // when copying to convert from a color bitmap to a mono bitmap

    OldBkColor := SetBkColor( ColorBmp.Canvas.Handle, TransparentColor );
    R := Rect( 0, 0, ColorBmp.Width, ColorBmp.Height );

    // Now copy to monochrome bitmap; all pixels in source bitmap that
    // were the transparent color will be white in the destination bitmap,
    // all other pixels will be black

    Result.Canvas.CopyMode := cmSrcCopy;
    Result.Canvas.CopyRect( R, ColorBmp.Canvas, R );
    SetBkColor( ColorBmp.Canvas.Handle, OldBkColor );
  except
    Result.Free;
    Raise;
  end;
end;


function CreateMonoOutlineMask( Source, NewSource: TBitmap; const OffsetPts: array of TRzPointArray;
                                TransparentColor: TColor ): TBitmap;
var
  I, W, H: Integer;
  R, NewR: TRect;
  SmallMask, BigMask, NewSourceMask: TBitmap;
begin
  Result := TBitmap.Create;
  try
    W := Source.Width;
    H := Source.Height;
    R := Rect( 0, 0, W, H );

    Result.Monochrome := True;
    Result.Width := W;
    Result.Height := H;

    SmallMask := CreateMonoMask( Source, TransparentColor );
    NewSourceMask := CreateMonoMask( NewSource, TransparentColor );
    BigMask := CreateMonoMask( NewSourceMask, TransparentColor );

    try
      BigMask.Canvas.CopyMode := cmSrcCopy;
      BigMask.Canvas.CopyRect( R, NewSourceMask.Canvas, R );

      for I := Low( OffsetPts ) to High( OffsetPts ) do
      begin
        if ( OffsetPts[I, 0] = 0 ) and ( OffsetPts[I, 1] = 0 ) then
          Break;
        NewR := R;
        OffsetRect( NewR, OffsetPts[I, 0], OffsetPts[I, 1] );
        BigMask.Canvas.CopyMode := cmSrcAnd; // DSa
        BigMask.Canvas.CopyRect( NewR, SmallMask.Canvas, R );
      end;
      BigMask.Canvas.CopyMode := cmSrcCopy;

      with Result do
      begin
        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect( R, NewSourceMask.Canvas, R );
        Canvas.CopyMode := $00DD0228; // SDno
        Canvas.CopyRect( R, BigMask.Canvas, R );
        Canvas.CopyMode := cmSrcCopy;
      end;

    finally
      SmallMask.Free;
      NewSourceMask.Free;
      BigMask.Free;
    end;

  except
    Result.Free;
    Raise;
  end;
end;


{============================}
{== TRzShapeButton Methods ==}
{============================}

constructor TRzShapeButton.Create( AOwner: TComponent );
begin
  inherited;

  SetBounds( 0, 0, 80, 80 );
  ControlStyle := [ csCaptureMouse ];
  FAutoSize := True;
  FBitmap := TBitmap.Create;
  FBitmap.OnChange := BitmapChanged;
  FBitmapUp := TBitmap.Create;
  FBitmapDown := TBitmap.Create;
  FHitTestMask := nil;
  ParentFont := True;
  FBevelWidth := 2;
  FBorderStyle := bsSingle;
  FState := bsUp;
  FPreciseClick := True;
  FPreciseShowHint := True;
  FBorderColor := cl3DDkShadow;
  FBevelHighlightColor := clBtnHighlight;
  FBevelShadowColor := clBtnShadow;
  FCaptionPosition := cpCentered;
end;


destructor TRzShapeButton.Destroy;
begin
  FBitmap.Free;
  FBitmapUp.Free;
  FBitmapDown.Free;
  FHitTestMask.Free;
  inherited;
end;


procedure TRzShapeButton.Paint;
var
  W, H: Integer;
  Composite, Mask, Overlay, CurrentBmp: TBitmap;
  R, NewR: TRect;
  BrushHandle: hBrush;
begin
  if csDesigning in ComponentState then
  begin
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle( 0, 0, Width, Height );
    end;
  end;

  if ( csDesigning in ComponentState ) or ( FState in [ bsDisabled, bsExclusive ] ) then
    FState := bsUp;

  if FState = bsUp then
    CurrentBmp := FBitmapUp
  else
    CurrentBmp := FBitmapDown;

  if not CurrentBmp.Empty then
  begin
    W := Width;
    H := Height;
    R := ClientRect;
    NewR := R;

    Composite := TBitmap.Create;
    Overlay := TBitmap.Create;

    // When not using a palette ( 4, 16 or 24 bit color ) CopyPalette returns
    // without doing anything, and thus doesn't impact performance on these systems.

    Composite.Palette := CopyPalette( FBitmap.Palette );
    Overlay.Palette := CopyPalette( FBitmap.Palette );
    InitPalette( Composite.Canvas.Handle );
    InitPalette( Overlay.Canvas.Handle );
    InitPalette( Canvas.Handle );

    try
      with Composite do
      begin
        Width := W;
        Height := H;
        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect( R, Self.Canvas, R ); // Start with existing background
      end;

      with Overlay do
      begin
        Width := W;
        Height := H;
        Canvas.CopyMode := cmSrcCopy;
        BrushHandle := CreateSolidBrush( FBitmap.TransparentColor );
        try
          FillRect( Canvas.Handle, R, BrushHandle );
        finally
          DeleteObject( BrushHandle );
        end;
        if FState = bsDown then
          OffsetRect( NewR, 1, 1 );
        Canvas.CopyRect( NewR, CurrentBmp.Canvas, R );
      end;

      Mask := CreateMonoMask( Overlay, FBitmap.TransparentColor );
      try
        // Combine the mask with the existing background; this will give
        // the background with black ( 'holes' ) where the overlay will
        // eventually be shown

        Composite.Canvas.CopyMode := cmSrcAnd; // DSa
        Composite.Canvas.CopyRect( R, Mask.Canvas, R );

        // Generate the overlay image by combining the mask and the
        // original image; this will give ( courtesy of the appropriate
        // ROP code ) the image on a black background

        Overlay.Canvas.CopyMode := $00220326; { DSna }
        Overlay.Canvas.CopyRect( R, Mask.Canvas, R );

        // Now put the overlay image onto the background; this will
        // fill in the black ( 'holes' ) with the overlay image, leaving
        // the rest of the background as is

        Composite.Canvas.CopyMode := cmSrcPaint; { DSo }
        Composite.Canvas.CopyRect( R, Overlay.Canvas, R );

        // Now copy the composite image back
        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect( R, Composite.Canvas, R );
      finally
        Mask.Free;
      end;

    finally
      Composite.Free;
      Overlay.Free;
    end;
  end;

  if Length( Caption ) > 0 then
  begin
    // Draw the button caption
    Canvas.Font := Self.Font;
    R := GetCaptionRect( Canvas, Caption );
    DrawButtonText( Canvas, Caption, R, FState );
  end;
end; {= TRzShapeButton.Paint =}


procedure TRzShapeButton.CMHitTest( var Msg: TCMHitTest );
begin
  inherited;
  if not FPreciseClick or PtInMask( Msg.XPos, Msg.YPos ) then
    Msg.Result := HTCLIENT
  else
    Msg.Result := HTNOWHERE;
end;


function TRzShapeButton.PtInMask( const X, Y: Integer ): Boolean;
begin
  Result := True;
  if FHitTestMask <> nil then
    Result := FHitTestMask.Canvas.Pixels[ X, Y ] = clBlack;
end;


procedure TRzShapeButton.MouseDown( Button: TMouseButton; Shift: TShiftState;
                                     X, Y: Integer );
var
  Clicked: Boolean;
begin
  inherited;

  if ( Button = mbLeft ) and Enabled then
  begin
    if FPreciseClick then
      Clicked := PtInMask( X, Y )
    else
      Clicked := True;

    if Clicked then
    begin
      FState := bsDown;
      Repaint;
    end;
    FDragging := True;
  end;
end;


procedure TRzShapeButton.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  NewState: TButtonState;
  InMask: Boolean;
begin
  inherited;
  InMask := PtInMask( X, Y );

  if FPreciseShowHint and not InMask then
  begin
    // The outcome of PreciseShowHint being True may not be quite
    // what the user/developer expects because the Application
    // may still display the hint for the parent ( if the parent
    // has ShowHint = True ).  Consider the situation where the
    // button has been placed over a TImage and the button, image and
    // form all have ShowHint = True.  In this case PreciseShowHint
    // will result in the hint for the form being shown ( because it is
    // the parent of the button ) rather than the hint for the image
    // when the cursor is not positioned inside the masked area.

    if not FPrevShowHintSaved then
    begin
      // Must save ParentShowHint before changing ShowHint
      FPrevParentShowHint := ParentShowHint;
      ParentShowHint := False;
      FPrevShowHint := ShowHint;
      ShowHint := False;
      FPrevShowHintSaved := True;
    end;
  end
  else if FPreciseClick and not InMask then
  begin
    if not FPrevCursorSaved then
    begin
      FPrevCursor := Cursor;
      Cursor := crDefault;
      FPrevCursorSaved := True;
    end;
  end
  else
  begin
    if FPrevShowHintSaved then
    begin
      // Must set ShowHint before changing ParentShowHint
      ShowHint := FPrevShowHint;
      ParentShowHint := FPrevParentShowHint;
      FPrevShowHintSaved := False;
    end;
    if FPrevCursorSaved then
    begin
      Cursor := FPrevCursor;
      FPrevCursorSaved := False;
    end;
  end;

  if FDragging then
  begin
    if FPreciseClick then
      if InMask then
        NewState := bsDown
      else
        NewState := bsUp
    else
      if ( X >= 0 ) and ( X < ClientWidth ) and ( Y >= 0 ) and ( Y <= ClientHeight ) then
        NewState := bsDown
      else
        NewState := bsUp;

    if NewState <> FState then
    begin
      FState := NewState;
      Repaint;
    end;
  end;
end; {= TRzShapeButton.MouseMove =}


procedure TRzShapeButton.MouseUp( Button: TMouseButton; Shift: TShiftState;
                                   X, Y: Integer );
var
  DoClick: Boolean;
begin
  inherited;

  if FDragging then
  begin
    FDragging := False;
    if FPreciseClick then
      DoClick := PtInMask( X, Y ) // Determine if mouse released while on masked area
    else
      DoClick := ( X >= 0 ) and ( X < ClientWidth ) and
                 ( Y >= 0 ) and ( Y <= ClientHeight );

    if FState = bsDown then
    begin
      FState := bsUp;
      Repaint;
    end;

    if DoClick then
      Click;
  end;
end;


procedure TRzShapeButton.Click;
begin
  inherited;
end;


function TRzShapeButton.GetPalette: HPALETTE;
begin
  Result := FBitmap.Palette;
end;


procedure TRzShapeButton.SetBitmap( Value: TBitmap );
begin
  FBitmap.Assign( Value );
end;


procedure TRzShapeButton.SetBitmapUp( Value: TBitmap );
begin
  FBitmapUp.Assign( Value );
end;


procedure TRzShapeButton.SetBitmapDown( Value: TBitmap );
begin
  FBitmapDown.Assign( Value );
end;


procedure TRzShapeButton.BitmapChanged( Sender: TObject );
var
  OldCursor: TCursor;
  W, H: Integer;
begin
  AdjustBounds;

  if not ( ( csReading in ComponentState ) or ( csLoading in ComponentState ) ) then
  begin
    if FBitmap.Empty then
    begin
      // Bitmap has been cleared, also clear up & down images
      SetBitmapUp( nil );
      SetBitmapDown( nil );
    end
    else
    begin
      W := FBitmap.Width;
      H := FBitmap.Height;
      OldCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        if ( FBitmapUp.Width <> W ) or ( FBitmapUp.Height <> H ) or
           ( FBitmapDown.Width <> W ) or ( FBitmapDown.Height <> H ) then
        begin
          FBitmapUp.Width := W;
          FBitmapUp.Height := H;
          FBitmapDown.Width := W;
          FBitmapDown.Height := H;
        end;
        Create3DBitmap( FBitmap, bsUp, FBitmapUp );
        Create3DBitmap( FBitmap, bsDown, FBitmapDown );

        FHitTestMask.Free;
        FHitTestMask := CreateMonoMask( FBitmapUp, FBitmap.TransparentColor );
      finally
        Screen.Cursor := OldCursor;
      end;
    end;
  end;
  Invalidate;
end; {= TRzShapeButton.BitmapChanged =}


procedure TRzShapeButton.CMDialogChar( var Msg: TCMDialogChar );
begin
  if IsAccel( Msg.CharCode, Caption ) and Enabled then
  begin
    Click;
    Msg.Result := 1;
  end
  else
    inherited;
end;


procedure TRzShapeButton.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzShapeButton.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzShapeButton.CMSysColorChange( var Msg: TMessage );
begin
  inherited;
  BitmapChanged( Self );
end;


function TRzShapeButton.BevelColor( const AState: TButtonState; const TopLeft: Boolean ): TColor;
begin
  if AState = bsUp then
  begin
    if TopLeft then
    begin
      if ( ColorToRGB( FBitmap.TransparentColor ) and $FFFFFF ) = ColorToRGB( FBevelHighlightColor ) then
        Result := DarkerColor( FBevelHighlightColor, 1 )
      else
        Result := FBevelHighlightColor;
    end
    else
      Result := FBevelShadowColor;
  end
  else
  begin
    if TopLeft then
      Result := FBevelShadowColor
    else
    begin
      if ColorToRGB( FBitmap.TransparentColor ) = ColorToRGB( FBevelHighlightColor ) then
        Result := DarkerColor( FBevelHighlightColor, 1 )
      else
        Result := FBevelHighlightColor;
    end;
  end;
end;


{===============================================================================
  Create3DBitmap:

  The source bitmap is converted to a 3D bitmap by adding successive
  borders ( outlines ) around each successive image using the appropriate
  color to get the 3D shading effect.  Masks are used to just add each
  successive outline without affecting the existing image.  The new outline
  for each layer is generated by offsetting the original image to
  successive different positions so as to enlarge its 'footprint'.
  Up to 3 layers of outlines are possible depending on BevelWidth ( 0..2 )
  and BorderStyle ( bsNone, bsSingle ).  Each bevel outline can consist
  of two parts each of which can be a different color ( the border is all
  one color ).  When the image is in the 'up' state the first
  part, consisting of the top-right corner to the bottom-right corner
  to the bottom-left corner, will ( by default ) be dark grey in color
  and the second part, consisting of the bottom-left corner to the
  top-left corner to the top-right corner will be ( by default ) be white.
  Each outline ( getting further away from the origin ) requires more
  points to define its path than the previous outline.  The reverse
  colors will apply when the button is in the 'down' state.
  The OutlineOffsetPts type is used to define the points for each
  successive outline.  The first subscript is the outline level ( 1..3 ),
  the next subscript is for the part, dark-grey or white ( 0..1 ),
  and the final subscript is for each of the points.  Unused points
  are specified as ( 0,0 ).  The correct sequence for processing the points
  is necessary to get the correct 3D shading effect; this is why the
  the points don't just start from the top-left corner but always start
  from the top-right and proceed clockwise from there.  The 3D image is
  also built up from the inside out so as to be able to extract each
  succesive outline so it can be combined with the original image.

  The points are derived from the following grids, where B = Black ( border )
  W = White and G = Dk Grey.  X is the origin of the original image.
  Each character represents one pixel.

  Up: BBBBBBB   Down: BBBBBBB
      BWWWWGB         BGGGGWB
      BWWWGGB         BGGGWWB
      BWWXGGB         BGGXWWB
      BWGGGGB         BGWWWWB
      BGGGGGB         BWWWWWB
      BBBBBBB         BBBBBBB

===============================================================================}

procedure TRzShapeButton.Create3DBitmap( Source: TBitmap; const AState: TButtonState;
                                          Target: TBitmap );
type
  OutlineOffsetPts = array[ 1..3, 0..1, 0..12 ] of TRzPointArray;
const
  OutlinePts: OutlineOffsetPts =
    (
      ( ( (  1, -1 ), (  1,  0 ), (  1,  1 ), (  0,  1 ), ( -1,  1 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ) ),
        ( ( -1,  0 ), ( -1, -1 ), (  0, -1 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ) ) ),

      ( ( (  2, -2 ), (  2, -1 ), (  2,  0 ), (  2,  1 ), (  2,  2 ), (  1,  2 ), (  0,  2 ), ( -1,  2 ), ( -2,  2 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ) ),
        ( ( -2,  1 ), ( -2,  0 ), ( -2, -1 ), ( -2, -2 ), ( -1, -2 ), (  0, -2 ), (  1, -2 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ), (  0,  0 ) ) ),

      ( ( (  3, -3 ), (  3, -2 ), (  3, -1 ), (  3,  0 ), (  3,  1 ), (  3,  2 ), (  3,  3 ), (  2,  3 ), (  1,  3 ), (  0,  3 ), ( -1,  3 ), ( -2,  3 ), ( -3,  3 ) ),
        ( ( -3,  2 ), ( -3,  1 ), ( -3,  0 ), ( -3, -1 ), ( -3, -2 ), ( -3, -3 ), ( -2, -3 ), ( -1, -3 ), (  0, -3 ), (  1, -3 ), (  2, -3 ), (  0,  0 ), (  0,  0 ) ) )
    );
var
  I, J, W, H, Outlines: Integer;
  R: TRect;
  OutlineMask, Overlay, NewSource: TBitmap;
  BrushHandle: hBrush;
  OldBrushHandle: hBrush;
begin
  if ( Source = nil ) or ( Target = nil ) then
    Exit;

  W := Source.Width;
  H := Source.Height;
  R := Rect( 0, 0, W, H );

  Overlay := TBitmap.Create;
  NewSource := TBitmap.Create;

  // The following lines may look strange -- they are just used to force the
  // bitmap and canvas handles for Source to be created before doing anything
  // with Source. ( The handles are only assigned back to themselves so it will
  // compile under Delphi 1 -- under Delphi 2 we could just reference each
  // Handle property without assigning to itself. )
  // I don't know why but if the handles aren't initialised like this for
  // 256 color systems then changing the bitmap at design-time won't always
  // 'take' and changing the bitmap at design or run-time causes memory losses
  // ( palettes not freed by Graphics unit ).

  Source.Handle := Source.Handle;
  Source.Canvas.Handle := Source.Canvas.Handle;

  Overlay.Palette := CopyPalette( Source.Palette );
  NewSource.Palette := CopyPalette( Source.Palette );
  Target.Palette := CopyPalette( Source.Palette );
  InitPalette( Overlay.Canvas.Handle );
  InitPalette( NewSource.Canvas.Handle );
  InitPalette( Target.Canvas.Handle );

  try
    NewSource.Width := W;
    NewSource.Height := H;

    // Copy source to target
    Target.Canvas.CopyMode := cmSrcCopy;
    Target.Canvas.CopyRect( R, Source.Canvas, R );

    Overlay.Width := W;
    Overlay.Height := H;

    Outlines := FBevelWidth;
    if FBorderStyle = bsSingle then
      Inc( Outlines );

    for I := 1 to Outlines do
    begin
      // Use the target bitmap as the basis for the new outline
      with NewSource.Canvas do
      begin
        CopyMode := cmSrcCopy;
        CopyRect( R, Target.Canvas, R );
      end;

      for J := 0 to 1 do
      begin
        if ( AState = bsDown ) and ( I = Outlines ) and ( J = 0 ) then
          Continue; // No shadow outline for final border is used

        // Use TransparentColor of FBitmap rather than that of
        // the 3D bitmap because the bevel/outline may have been drawn into
        // the pixel previously used to indicate the transparent color.

        OutlineMask := CreateMonoOutlineMask( Source, NewSource, OutlinePts[ I, J ],
                                              FBitmap.TransparentColor );
        try
          with Overlay.Canvas do
          begin
            // Create our own brush rather than using the canvas's -- not sure
            // if this is absolutely necessary but you never know when dealing
            // with palette colors!

            if ( I = Outlines ) and ( FBorderStyle = bsSingle ) then
              BrushHandle := CreateSolidBrush( ColorToRGB( FBorderColor ) )
            else
              BrushHandle := CreateSolidBrush( ColorToRGB( BevelColor( AState, ( J = 1 ) ) ) );
            OldBrushHandle := SelectObject( Handle, BrushHandle );
            try
              CopyMode := $0030032A; // PSna
              CopyRect( R, OutlineMask.Canvas, R );
            finally
              SelectObject( Handle, OldBrushHandle );
              DeleteObject( BrushHandle );
            end;
          end;

          with Target.Canvas do
          begin
            // Create black outline in target where colored outline is to go
            CopyMode := cmSrcAnd; // DSa
            CopyRect( R, OutlineMask.Canvas, R );
            // Copy colored outline into black outline area
            CopyMode := cmSrcPaint; // DSo
            CopyRect( R, Overlay.Canvas, R );
            CopyMode := cmSrcCopy;
          end;

        finally
          OutlineMask.Free;
        end;
      end;
    end;

  finally
    Overlay.Free;
    NewSource.Free;
  end;
end; {= TRzShapeButton.Create3DBitmap =}


procedure TRzShapeButton.SetBorderStyle( Value: TBorderStyle );
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    BitmapChanged( Self );
  end;
end;


procedure TRzShapeButton.SetBorderColor( Value: TColor );
begin
  if Value <> FBorderColor then
  begin
    FBorderColor := Value;
    BitmapChanged( Self );
  end;
end;


procedure TRzShapeButton.SetBevelWidth( Value: TRzBevelWidth );
begin
  if Value > 2 then
    Value := 2;
  if Value <> FBevelWidth then
  begin
    FBevelWidth := Value;
    BitmapChanged( Self );
  end;
end;


procedure TRzShapeButton.SetBevelHighlightColor( Value: TColor );
begin
  if Value <> FBevelHighlightColor then
  begin
    FBevelHighlightColor := Value;
    BitmapChanged( Self );
  end;
end;


procedure TRzShapeButton.SetBevelShadowColor( Value: TColor );
begin
  if Value <> FBevelShadowColor then
  begin
    FBevelShadowColor := Value;
    BitmapChanged( Self );
  end;
end;


procedure TRzShapeButton.SetCaptionPosition( Value: TRzCaptionPosition );
begin
  if Value <> FCaptionPosition then
  begin
    FCaptionPosition := Value;
    Invalidate;
  end;
end;


procedure TRzShapeButton.SetCaptionX( Value: Integer );
begin
  SetCaptionXY( Value, FCaptionY );
end;


procedure TRzShapeButton.SetCaptionY( Value: Integer );
begin
  SetCaptionXY( FCaptionX, Value );
end;


procedure TRzShapeButton.SetCaptionXY( const X, Y: Integer );
var
  Moved: Boolean;
begin
  Moved := False;
  if X <> FCaptionX then
  begin
    FCaptionX := X;
    Moved := True;
  end;
  if Y <> FCaptionY then
  begin
    FCaptionY := Y;
    Moved := True;
  end;
  if Moved then
  begin
    FCaptionPosition := cpXY;
    Invalidate;
  end;
end;


function TRzShapeButton.GetCaptionRect( Canvas: TCanvas; const Caption: string ): TRect;
begin
  if FCaptionPosition = cpCentered then
    Result := ClientRect
  else
  begin
    Result := Rect( 0, 0, ClientRect.Right - ClientRect.Left, 0 );
    DrawString( Canvas, Caption, Result, dt_CalcRect );
    OffsetRect( Result, FCaptionX, FCaptionY );
  end;
end;


procedure TRzShapeButton.DrawButtonText( Canvas: TCanvas; const Caption: string; TextBounds: TRect; State: TButtonState );
begin
  Canvas.Brush.Style := bsClear;
  if State = bsDown then
    OffsetRect( TextBounds, 1, 1 );

  DrawStringCentered( Canvas, Caption, TextBounds );
end;



procedure TRzShapeButton.Loaded;
var
  BigMask: TBitmap;
  R: TRect;
begin
  inherited;
  if ( FBitmap <> nil ) and ( FBitmap.Width > 0 ) and ( FBitmap.Height > 0 ) then
  begin
    // Combine the mask for the original image with one the mask of one
    // of the 'enlarged' images; this will remove any speckling inside
    // the image so that hit-testing will work correctly.
    // Use TransparentColor of FBitmap rather than that of
    // the 3D bitmap because the bevel/outline may have been drawn into
    // the pixel previously used to indicate the transparent color.

    FHitTestMask.Free;
    FHitTestMask := CreateMonoMask( FBitmap, FBitmap.TransparentColor );
    BigMask := CreateMonoMask( FBitmapUp, FBitmap.TransparentColor );
    try
      R := Rect( 0, 0, FBitmap.Width, FBitmap.Height );
      FHitTestMask.Canvas.CopyMode := cmSrcAnd;
      FHitTestMask.Canvas.CopyRect( R, BigMask.Canvas, R );
    finally
      BigMask.Free;
    end;
  end;
end;


// Fake BitmapUp and BitmapDown properties are defined so that
// the bitmaps for the button's up and down states are stored.

procedure TRzShapeButton.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineBinaryProperty( 'BitmapUp', ReadBitmapUpData, WriteBitmapUpData, not FBitmapUp.Empty );
  Filer.DefineBinaryProperty( 'BitmapDown', ReadBitmapDownData, WriteBitmapDownData, not FBitmapDown.Empty )
end;


procedure TRzShapeButton.ReadBitmapUpData( Stream: TStream );
begin
  FBitmapUp.LoadFromStream( Stream );
end;

procedure TRzShapeButton.WriteBitmapUpData( Stream: TStream );
begin
  FBitmapUp.SaveToStream( Stream );
end;

procedure TRzShapeButton.ReadBitmapDownData( Stream: TStream );
begin
  FBitmapDown.LoadFromStream( Stream );
end;

procedure TRzShapeButton.WriteBitmapDownData( Stream: TStream );
begin
  FBitmapDown.SaveToStream( Stream );
end;


procedure TRzShapeButton.AdjustBounds;
begin
  SetBounds( Left, Top, Width, Height );
end;


procedure TRzShapeButton.AdjustButtonSize( var W, H: Integer );
begin
  if not ( csReading in ComponentState ) and FAutoSize and not FBitmap.Empty then
  begin
    W := FBitmap.Width + 4;
    H := FBitmap.Height + 4;
  end;
end;


procedure TRzShapeButton.SetAutoSize( Value: Boolean );
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustBounds;
  end;
end;


procedure TRzShapeButton.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustButtonSize( W, H );
  inherited SetBounds( ALeft, ATop, W, H );
end;


procedure TRzShapeButton.Invalidate;
var
  R: TRect;
begin
  if ( Visible or ( csDesigning in ComponentState ) ) and ( Parent <> nil ) and Parent.HandleAllocated then
  begin
    R := BoundsRect;
    InvalidateRect( Parent.Handle, @R, True );
  end;
end;


// Select and realize the control's palette

procedure TRzShapeButton.InitPalette( DC: HDC );
var
  Palette: HPALETTE;
begin
  Palette := GetPalette;
  if Palette <> 0 then
  begin
    SelectPalette( DC, Palette, False );
    RealizePalette( DC );
  end;
end;





procedure FreeBitmaps; far;
var
  I: TBitBtnKind;
begin
  for I := Low( TBitBtnKind ) to High( TBitBtnKind ) do
    BitBtnGlyphs[ I ].Free;
end;

initialization
  FillChar( BitBtnGlyphs, SizeOf( BitBtnGlyphs ), 0 );
  {&RUI}

finalization
  FreeBitmaps;
end.







