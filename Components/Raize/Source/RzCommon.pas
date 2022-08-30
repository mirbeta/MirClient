{===============================================================================
  RzCommon Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDialogComponent
    Base component that implements extra features for dialog-based components.

  TRzFrameController
    Allows a single component to control the framing of many other components.

  TRzRegIniFile
    Nonvisual component that can read/write values to/from an Ini File or the
    Registry.

  TRzCustomColors
    Nonvisual component that manages a list of custom colors values for use in
    a Color Dialog.  Used by the TRzColorPicker and TRzColorEdit components.

  TRzMenuController
    Nonvisual component that alters the appears of TMenuItem controls used on
    the form.


  This unit also implements several general-purpose procedures and functions.


  Modification History
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Bitmap images associated directly with a menu item are now displayed when
      the TRzMenuController is used.
    * Added new UseMenuColorForMainMenu property. When set to True, the current
      form's main menu is drawn using the color specified in the MenuColor
      property.
    * Added new AutoUpdateIniFile property to TRzRegIniFile. When set to True,
      the default, the INI file is automatically updated when a setting is
      changed. When AutoUpdateIniFile is set to False, the INI file will get
      updated whenever the UpdateIniFile method is called or when the component
      is destroyed.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed issue in TRzMenuController where cascading menu arrow would overlap
      the menu item text if the menu used an image list larger than 16x16.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Added UsingSystemStyle function.
    * Added ActiveStyleSystemColor and GetStyleStyleColor functions.
    * Added new DrawFilledBox procedure.
    * Added Color parameter to DrawFocusBorder.
    * Made necessary modifications to TRzMenuController to support VCL Styles
      introduced in RAD Studio XE2. Standard menus and popup menus are not
      affected when applying VCL Styles. However, with the TRzMenuController,
      menu items are displayed using appropriate colors and shading from the
      selected VCL Style so that the menus match the rest of the interface.
    * Modified several drawing functions to support VCL Styles introduced in
      RAD Studio XE2.
  ------------------------------------------------------------------------------
  5.5.1  (31 Mar 2011)
    * Fixed issue in TRzMenuController where menu Short Cuts would overlap with
      the menu text in certain situations.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzMenuComponent now looks at the BiDiMode setting of the menu that
      it is modifying to determine if the menu needs to be displayed in right-
      to-left orientation. If so, the image bar of the custom menu appearance
      is drawn on the right edge of the menu rather than the left. Menu item
      captions, images, and short-cuts are also repositioned as appropriate.
    * Added MenuFont and UseCustomMenuFont properties to the TRzMenuController.
      When UseCustomMenuFont is set to True, the font attributes defined in
      the MenuFont property are used to display the non-top-level menu items
      in the menu. The height of top-level menu items do not change as a result
      of the font changing. Therefore, the system defined menu font is used
      for top level menus.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Added new FirstNonWhitespaceChar function.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Added XorColors function that combines a foreground color with a
      background color using XOR. The resulting color is identical to the colors
      that result when using cmSrcInvert values for TCanvas.CopyMode.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed problem where the TRzRegIniFile.ReadSectionValues method would not
      clear out the passed in list before populating the list with values from
      the section when the PathType property is set to ptRegistry.
    * TRzPropertyStore is now able to handle the "contents" of string list
      properties. For example, ListBox1.Items.
    * Fixed problem in GetSpecialFolderPath function where the ANSI string
      version SHGetSpecialFolderPathA API function was being used even under
      Delphi 2009 instead of SHGetSpecialFolderPathW. This resulted in problems
      in the TRzRegIniFile component with Special Folders.
    * Changed TRzRegIniFile component to use the TMemIniFile class instead of
      TIniFile. This was done because the TMemIniFile provides support for
      reading and writing an encoded text file.
    * As a result of the change above, a new FileEncoding property has been
      added to the TRzRegIniFile component. This property is only used in
      RAD Studio 2009 or higher, which provides Unicode support and file
      encodings. The default is feDefault. The other options are feUTF8 and
      feUnicode.
    * Fixed problem in the RunningAtLeast function so that it correctly returns
      True when running under Vista and testing for WinXP.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added the new TRzPropertyStore component, which is designed to work with a
      TRzRegIniFile component to persist property settings in your applications.
      To use the TRzPropertyStore, simply drop one on the form containing the
      components whose properties you wish to persist and then connect the
      TRzPropertyStore to a TRzRegIniFile component. Next, double-click the
      component (or edit the Properties property) to display the collection
      editor for the TRzPropertyStore.Properties collection. Use the collection
      editor to add a new property item (i.e. Component, Property pair). Use
      the drop down list to select the desired component on the form, and then
      use the drop down list to select one of the properties of the selected
      component. Keep adding items to collection to store more properites. To
      save the current values of the selected properties call the
      TRzPropertyStore.Save method. To restore property settings persisted
      previously, call the TRzPropertyStore.Load method. The TRzPropertyStore
      component will persist the following property types: Integers,
      Floating-Point Numbers, Booleans, Enumerations, Sets, Classes, and
      Collections.
    * Fixed issue with TRzRegIniFile component where an INI file with no name
      would get created if a SpecialFolder was specified and the Path property
      remained empty. The component now correctly creates an INI file using the
      name of the executable.
    * Removed static link to SHGetSpecialFolderPath API function as some users
      needed to run programs on Windows NT, which does not have this API
      function in Shell32.dll.
    * Renamed the RemovePrefixChars function to RemoveAccelerators to more
      accurately describe what the method does.
    * Added new DrawString function and DrawStringCentered procedure. These
      methods are wrappers around the highly used DrawText Windows API function
      and primarily eliminate the need to case the desired string to a PChar.
    * Added new SendTextMessage function. This is a wrapper around the
      SendMessage function and is designed specifically for situations where
      a string needs to be sent in a Window message. NOTE: RAD Studio 2009
      defines its own SendTextMessage, so in RS 2009 and later, the built-in
      SendTextMessage will be used instead.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Added new SpecialFolder property to TRzRegIniFile component. This property
      is used when PathType=ptIniFile in order to specify a special location for
      the ini file.  SpecialFolder can be set to one of the following values:

      sfNone:
        Default. Location determined by Path property (see below).

      sfUserAppDataRoaming:
        The user's Application Data directory that supports roaming is used.
        Under XP this is typically:
          C:\Documents and Settings\<User>\Application Data
        Under Vista this is typically:
          C:\Users\<User>\AppData\Roaming

      sfUserAppDataLocal:
        The user's Application Data directory for data that is local to the
        machine.
        Under XP this is typically:
          C:\Documents and Settings\<User>\Local Settings\Application Data
        Under Vista this is typically:
          C:\Users\<User>\AppData\Local

      sfUserDocuments:
        The user's Documents directory.
        Under XP this is typically:
          C:\Documents and Settings\<User>\My Documents
        Under Vista this is typically:
          C:\Users\<User>\Documents

      sfProgramData:
        The directory associated with storing data for all users of a system.
        Under XP this is typically:
          C:\Documents and Settings\All Users\Application Data
        Under Vista this is typically:
          C:\ProgramData

      The actual file that is used depends on the Path property. If Path is
      empty, the ini file has same name as Application.ExeName but with an
      '.ini' extension.  If Path is not empty, and specifies just a filename,
      then the specified filename is used and the file is located in the
      directory specified by SpecialFolder. If Path is not empty and specifies
      a full path and filename, then the specified full path and filename are
      used for the ini file.
    * Updated the DrawCloseX method to make the X symbol a little smaller. This
      method is used by the TRzPageControl and TRzTabControl to draw the tab-
      close button.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Fixed issue in TRzMenuController when reading UserPreferencesMask from
      Registry under Windows Vista.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added new RaizeComponents_Version constant that contains the complete
      version number of the current build of Raize Components.
    * Fixed problem where changes made to the ReadOnlyColorOnFocus property in
      TRzFrameController were not being transfered to connected controls.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * TRzMenuController now sizes the menu image bar according to the size of
      the images contained in the referenced image list.
    * Added InvalidateControls procedure, which is used by several container
      controls to force child controls that support transparency to invalidate
      themselves in order to pick up new styles of the container control.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Added ReadOnlyColorOnFocus property to TRzFrameController. This
      property determines if the ReadOnlyColor or FocusColor is used to color
      the control when the control has the focus and has its ReadOnly property
      set to True. By default, a focused control will use the FocusColor even
      if the control is read-only. Setting ReadOnlyColorOnFocus to True will
      cause a focused read-only control to be colored using ReadOnlyColor.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Fixed problem where accelerator characters were not displayed in
      TRzMenuController menus if the user turned off the "Hide underlined
      letters for keyboard navigation until I press the Alt key" option in the
      Display Settings.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added RemovePrefixChars function, which takes a passed string and then
      removes extra "&" characters which are typically used to define
      accelerator characters in a string.
    * Added ExpandEnvironmentVariables function, which takes a passed string and
      then replaces all envrionment variables with their equivalent strings.
    * Added IconBarColorStart, IconBarColorStop properties to TRzMenuController,
      which allows the user to change the vertical gradient bar in which menu
      item icons are displayed.
    * Added CurrentXPColorScheme global variable that is utilized by various
      controls and standard procedures. CurrentXPColorScheme is an enumeration
      that indicates the currently selected color scheme (blue, green, or
      silver). The supporting GetXPColorScheme procedure is also new in this
      release.
    * Removed GradientColorDefault property from TRzMenuController.
    * Added GradientColorStyle property to TRzMenuController, which can be set
      to gcsSystem (the default), gcsMSOffice, or gcsCustom. This property is
      used to determine the color values used when displaying menu items.
      Specifically, the color values used in displaying menu items has been
      redesigned in this version to provide a much better visual appearance
      as well as being compatible with Office applications if the user chooses.
    * The GradientColorStart, GradientColorStop, and FrameColor properties in
      TRzMenuController have been renamed to SelectionColorStart,
      SelectionColorStop, and SelectionFrameColor, respectively.  The component
      will automatically take care of switching to the new properties when
      loading a TRzMenuController that has the old names.
    * Added GetGradientSelectionColors, GetGradientPanelColors,
      GetGradientStatusBarColors global procedures, which are used by various
      components to determine appropriate colors for the specified gradient
      color style (gcsSystem or gcsMSOffice).
    * Added ReadOnlyColor to TRzFrameController. This property can be used to
      manage the new ReadOnlyColor property that has been added to the controls
      that support a ReadOnly property.
    * The TRzFrameController component supports the new
      FrameControllerNotifications property that has been added to all controls
      in Raize Components that support Custom Framing. Only properties that are
      included in the control's FrameControllerNotifications set will affect
      the control's appearance.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Added MenuColor and MenuFontColor properties to TRzMenuController.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Added ComplementaryColor function.
    * Added the new TRzMenuController component, that alters the appearance of
      TMainMenu and TPopupMenu menu items to reflect a more modern UI and match
      the display capabilities of the TRzToolButton component.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where FPath instance field of TRzRegIniFile was not updated
      when a default location (e.g. INI file or Reg Key) was generated by the
      component.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added RunningUnder function that can be used to easily determine which
      version of Windows is currently in use.
    * DrawSides function only draws sides with Color <> clNone.
    * Added DrawInnerOuterBorders function, which handles drawing inner and
      outer borders of all controls supporting inner and outer borders
      (e.g. TRzPanel, TRzBorder, TRzLabel, etc. ).
    * Added DrawGroupBarBackground procedure, which handles drawing the interior
      of a TRzGroupBar when GroupStyle is gbsCategoryView. This method is also
      used to display the new splitter bar of a TRzSplitter and a size bar of a
      TRzSizePanel when using the new ssGroupBar splitter style.
    * Fixed problem with TRzRegIniFile.ReadSectionValues where non-string values
      in a Registry section (i.e. key) would raise an exception.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Removed ThemeServices global variable. ThemeServices function now included
      in RzThemeSrv.pas.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem where TRzRegIniFile would raise an exception when trying to
      write to any Registry key not in the HKEY_CURRENT_USER branch.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added FullColorSupport function.
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
  * A new frame style has been added to Raize Components: fsFlatBold.

  << TRzFrameController >>
  * TRzFrameController can be linked to a TRzRegIniFile to save Custom Framing
    settings to the Registry or an INI file.
  * Added FocusColor and DisabledColor.
  * Added FramingPreference property.
  * Renamed FrameFlat property to FrameHotTrack.
  * Renamed FrameFocusStyle property to FrameHotStyle.
  * Removed FrameFlatStyle property.
  * The TRzFrameController can now be linked to a TRzRegIniFile component, which
    allows the custom framing settings to be stored persistently to the Registry
    or to an INI file.  The settings are saved using the Save method, and loaded
    using the Load method.
  * Added Assign method.

  << TRzRegIniFile >>
  * Initial release.

  << TRzCustomColors >>
  * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzCommon;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Types,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,                 
  {$ENDIF}
  Classes,
  Forms,
  Graphics,
  Controls,
  Menus,
  StdCtrls,
  ExtCtrls,
  SysUtils,
  ImgList,
  Themes,
  UxTheme,
  Registry,
  Contnrs,
  IniFiles;

const
  RaizeComponents_Version = '6.2.3';

  cm_GetBlinking                = cm_Base + 1000;
  cm_Blink                      = cm_Base + 1001;
  cm_ToolbarShowCaptionChanged  = cm_Base + 1002;
  cm_ToolbarButtonLayoutChanged = cm_Base + 1003;
  cm_ToolbarButtonSizeChanged   = cm_Base + 1004;
  cm_ToolbarVisualStyleChanged  = cm_Base + 1005;
  cm_HidePreviewPanel           = cm_Base + 1006;
  cm_GroupItemSelected          = cm_Base + 1007;


type
  TAlignmentVertical = ( avTop, avCenter, avBottom );

  TBlinkState = ( bsOn, bsOff );

  TTextStyle = ( tsNormal, tsRaised, tsRecessed, tsShadow );
  TOrientation = ( orHorizontal, orVertical );
  TBarStyle = ( bsTraditional, bsLED, bsGradient );
  TLineStyle = ( lsNone, lsFlat, lsGroove, lsBump );
  TFrameStyleEx = ( fsNone, fsFlat, fsGroove, fsBump, fsLowered, fsButtonDown, fsRaised, fsButtonUp, fsStatus, fsPopup, fsFlatBold, fsFlatRounded );
  TFrameStyle = fsNone..fsFlatBold;
  TFramingPreference = ( fpCustomFraming, fpXPThemes );

  TRzGroupBarGradientPath = ( gpTopToBottom, gpBottomToTop );

  TGradientDirection = ( gdDiagonalUp, gdDiagonalDown,
                         gdHorizontalEnd, gdHorizontalCenter, gdHorizontalBox,
                         gdVerticalEnd, gdVerticalCenter, gdVerticalBox,
                         gdSquareBox, gdBigSquareBox );

  TDirection = ( dirUp, dirDown, dirLeft, dirRight );
  TRzScrollStyle = ( scsNone, scsLeft, scsUp, scsRight, scsDown );

  TSide = ( sdLeft, sdTop, sdRight, sdBottom );
  TSides = set of TSide;

  TPositionChangingEvent = procedure( Sender: TObject; NewPos: Integer; var AllowChange: Boolean ) of object;

  TExpandOnType = ( etMouseButton2Click, etFocus, etNone );

  TPositiveByte = 1..255;
  TPositiveSmallint = 1..32767;
  TPositiveInteger = 1..MaxInt;
  TUnsignedSmallint = 0..32767;

  {$IFDEF VCL160_OR_HIGHER}
  TRzNativeInt = NativeInt;
  {$ELSE}
  TRzNativeInt = Integer;
  {$ENDIF}

  EInvalidFieldType = class( Exception );

  TRzTwoDigitYearConverter = ( ycStandard, ycPastNotFuture );
  TRzScrollType = ( stNone, stRightToLeft, stLeftToRight );
  TRzScrollDisplayEvent = procedure( Sender: TObject; CurrentStep, TotalSteps: Integer ) of object;

  TRzHotTrackColorType = ( htctComplement, htctActual );

  // Common Event Signatures
  TStateChangingEvent = procedure( Sender: TObject; Index: Integer; NewState: TCheckBoxState;
                                   var AllowChange: Boolean ) of object;

  TStateChangeEvent = procedure( Sender: TObject; Index: Integer; NewState: TCheckBoxState ) of object;

const
  sdAllSides = [ sdLeft, sdTop, sdRight, sdBottom ];
  fsDoubleBorders = [ fsGroove, fsBump, fsLowered, fsButtonDown, fsRaised, fsButtonUp, fsFlatBold, fsFlatRounded ];

const                                                           { Color Arrays }
  { Frame Style Color constant arrays }

  ULFrameColor: array[ TFrameStyle ] of TColor = ( clWindow,             // fsNone
                                                   cl3DDkShadow,         // fsFlat
                                                   clBtnShadow,          // fsGroove
                                                   clBtnHighlight,       // fsBump
                                                   clBtnShadow,          // fsLowered
                                                   clNone,               // fsButtonDown
                                                   cl3DDkShadow,         // fsRaised
                                                   clNone,               // fsButtonUp
                                                   clBtnShadow,          // fsStatus
                                                   clBtnHighlight,       // fsPopup
                                                   clBtnShadow );        // fsFlatBold

  LRFrameColor: array[ TFrameStyle ] of TColor = ( clWindow,             // fsNone
                                                   cl3DDkShadow,         // fsFlat
                                                   clBtnHighlight,       // fsGroove
                                                   clBtnShadow,          // fsBump
                                                   clBtnHighlight,       // fsLowered
                                                   clNone,               // fsButtonDown
                                                   clBtnFace,            // fsRaised
                                                   clNone,               // fsButtonUp
                                                   clBtnHighlight,       // fsStatus
                                                   clBtnShadow,          // fsPopup
                                                   clBtnShadow );        // fsFlatBold

  DrawTextAlignments: array[ TAlignment ] of Word = ( dt_Left,
                                                      dt_Right,
                                                      dt_Center );

  SetTextAlignments: array[ TAlignment ] of Word = ( ta_Left, ta_Right, ta_Center );

  DrawTextWordWrap: array[ Boolean ] of Word = ( 0, dt_WordBreak );


type
  TRzAboutInfo = ( aiRaizeComponents );

{===============================================================================
  TRzBlinkingControlsList Class

  Implementation specific class designed to wrap a timer component and
  maintain a list of Blinking controls.  When the internal timer fires,
  the cm_Blink message is sent to each control in the list.
===============================================================================}

type
  TRzBlinkingControlsList = class
  private
    FBlinkState: TBlinkState;
    FControls: TList;
    FTimer: TTimer;
    FIntervalOff: Word;
    FIntervalOn: Word;
  protected
    function GetCount: Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add( Control: TControl );
    procedure Remove( Control: TControl );
    procedure TimerFired( Sender: TObject );

    property Count: Integer
      read GetCount;

    property IntervalOff: Word
      read FIntervalOff
      write FIntervalOff;

    property IntervalOn: Word
      read FIntervalOn
      write FIntervalOn;
  end;

var
  BlinkingControls: TRzBlinkingControlsList = nil;

procedure FreeBlinkingControlsListIfEmpty;


{== Special Drawing Procedures ==}

function CenterRect( R: TRect; Width, Height: Integer ): TRect;

procedure ColorToHSL( C: TColor; var H, S, L: Byte );
function ColorHue( C: TColor ): Byte;
function ColorSaturation( C: TColor ): Byte;
function ColorLuminance( C: TColor ): Byte;
function HSLtoColor( H, S, L: Byte ): TColor;
function DarkerColor( C: TColor; Adjustment: Byte ): TColor;
function LighterColor( C: TColor; Adjustment: Byte ): TColor;
function AdjustColor( C: TColor; Adjustment: Integer ): TColor;
function BlendColors( ForeColor, BackColor: TColor; Alpha: Byte ): TColor;
function XorColors( ForeColor, BackColor: TColor ): TColor;
function ColorsTooClose( ForeColor, BackColor: TColor ): Boolean;
function ComplementaryColor( C: TColor ): TColor; overload;
function ComplementaryColor( C: TColor; Luminance: Byte ): TColor; overload;

type
  TRzGradientColorStyle = ( gcsSystem, gcsMSOffice, gcsCustom );
  TRzVisualStyle = ( vsClassic, vsWinXP, vsGradient );

const
  xpHotTrackColor        = $00149BF0;
  xpRadChkMarkColor      = $0021A121;
  xpRadChkFrameColor     = $0080511C;
  xpButtonFrameColor     = $00743C00;
  xpButtonFaceColor      = $00F0F4F4;
  xpEditFrameColor       = $00B99D7F;
  xpTabControlFrameColor = $009C9B91;
  xpTabControlColor      = $00F5F6F7;

  // Office Color Constants under Blue XP Color Scheme
  xpOfficeBlue_Selection_FrameColor: TColor                     = $00800000;
  xpOfficeBlue_Selection_ColorStart: TColor                     = $00C2EEFF;
  xpOfficeBlue_Selection_ColorStop: TColor                      = $00A0D9FF;
  xpOfficeBlue_Panel_ColorStart: TColor                         = $00FFEFE3;
  xpOfficeBlue_Panel_ColorStop: TColor                          = $00EABB99;
  xpOfficeBlue_Panel_FrameColor: TColor                         = $00BF7F57;
  xpOfficeBlue_GroupBar_ColorStart: TColor                      = $00E7A27B;
  xpOfficeBlue_GroupBar_ColorStop: TColor                       = $00D67563;

  xpOfficeBlue_CategoryGroup_CaptionBackColor                   = clWhite;
  xpOfficeBlue_CategoryGroup_CaptionBackColorStart              = clWhite;
  xpOfficeBlue_CategoryGroup_CaptionBackColorStop               = $00F7D3C6;
  xpOfficeBlue_CategoryGroup_CaptionFontColor                   = $00A53C00;
  xpOfficeBlue_CategoryGroup_CaptionFontHotColor                = $00FF8E42;
  xpOfficeBlue_CategoryGroup_CaptionButtonColor                 = $00FCFCFC;
  xpOfficeBlue_CategoryGroup_CaptionButtonBorderColor           = $00D9BAB3;
  xpOfficeBlue_CategoryGroup_CaptionDividerColor                = $00C56A31;
  xpOfficeBlue_CategoryGroup_GroupColor                         = $00F7DFD6;
  xpOfficeBlue_CategoryGroup_GroupBorderColor                   = clWhite;
  xpOfficeBlue_CategoryGroup_ItemFontColor                      = $00C65D21;
  xpOfficeBlue_CategoryGroup_ItemFontHotColor                   = $00FF8E42;

  xpOfficeBlue_CategoryGroupSpecial_CaptionBackColor            = $00C56A31;
  xpOfficeBlue_CategoryGroupSpecial_CaptionBackColorStart       = $00BE500F;
  xpOfficeBlue_CategoryGroupSpecial_CaptionBackColorStop        = $00CE5D29;
  xpOfficeBlue_CategoryGroupSpecial_CaptionFontColor            = clWhite;
  xpOfficeBlue_CategoryGroupSpecial_CaptionFontHotColor         = $00F7DFD6;
  xpOfficeBlue_CategoryGroupSpecial_CaptionButtonColor          = $00C35E2F;
  xpOfficeBlue_CategoryGroupSpecial_CaptionButtonBorderColor    = $00DE9A6A;
  xpOfficeBlue_CategoryGroupSpecial_CaptionDividerColor         = $00C56A31;
  xpOfficeBlue_CategoryGroupSpecial_GroupColor                  = $00FFF3EF;
  xpOfficeBlue_CategoryGroupSpecial_GroupBorderColor            = clWhite;


  // Office Color Constants under Green XP Color Scheme
  xpOfficeGreen_Selection_FrameColor: TColor                    = $00385D3F;
  xpOfficeGreen_Selection_ColorStart: TColor                    = $00C2EEFF;
  xpOfficeGreen_Selection_ColorStop: TColor                     = $00A0D9FF;
  xpOfficeGreen_Panel_ColorStart: TColor                        = $00EDFFFF;
  xpOfficeGreen_Panel_ColorStop: TColor                         = $0092C7B8;
  xpOfficeGreen_Panel_FrameColor: TColor                        = $0077A17F;
  xpOfficeGreen_GroupBar_ColorStart: TColor                     = $00ADD9CC;
  xpOfficeGreen_GroupBar_ColorStop: TColor                      = $0084BDA5;

  xpOfficeGreen_CategoryGroup_CaptionBackColor                  = $00B8E7E0;
  xpOfficeGreen_CategoryGroup_CaptionBackColorStart             = $00D5F3F1;
  xpOfficeGreen_CategoryGroup_CaptionBackColorStop              = $00B8E7E0;
  xpOfficeGreen_CategoryGroup_CaptionFontColor                  = $001C674B;
  xpOfficeGreen_CategoryGroup_CaptionFontHotColor               = $001D9272;
  xpOfficeGreen_CategoryGroup_CaptionButtonColor                = $00FCFCFC;
  xpOfficeGreen_CategoryGroup_CaptionButtonBorderColor          = $00BED5CA;
  xpOfficeGreen_CategoryGroup_CaptionDividerColor               = $0070A093;
  xpOfficeGreen_CategoryGroup_GroupColor                        = $00ECF6F6;
  xpOfficeGreen_CategoryGroup_GroupBorderColor                  = clWhite;
  xpOfficeGreen_CategoryGroup_ItemFontColor                     = $002D6656;
  xpOfficeGreen_CategoryGroup_ItemFontHotColor                  = $001D9272;

  xpOfficeGreen_CategoryGroupSpecial_CaptionBackColor           = $0070A093;
  xpOfficeGreen_CategoryGroupSpecial_CaptionBackColorStart      = $004E9682;
  xpOfficeGreen_CategoryGroupSpecial_CaptionBackColorStop       = $0067A896;
  xpOfficeGreen_CategoryGroupSpecial_CaptionFontColor           = clWhite;
  xpOfficeGreen_CategoryGroupSpecial_CaptionFontHotColor        = $00A9D6C8;
  xpOfficeGreen_CategoryGroupSpecial_CaptionButtonColor         = $004FA382;
  xpOfficeGreen_CategoryGroupSpecial_CaptionButtonBorderColor   = $0081B7A9;
  xpOfficeGreen_CategoryGroupSpecial_CaptionDividerColor        = $0070A093;
  xpOfficeGreen_CategoryGroupSpecial_GroupColor                 = $00ECF6F6;
  xpOfficeGreen_CategoryGroupSpecial_GroupBorderColor           = clWhite;


  // Office Color Constants under Silver XP Color Scheme
  xpOfficeSilver_Selection_FrameColor: TColor                   = $006F4B4B;
  xpOfficeSilver_Selection_ColorStart: TColor                   = $00C2EEFF;
  xpOfficeSilver_Selection_ColorStop: TColor                    = $00A0D9FF;
  xpOfficeSilver_Panel_ColorStart: TColor                       = $00FFF9F9;
  xpOfficeSilver_Panel_ColorStop: TColor                        = $00D0BCBD;
  xpOfficeSilver_Panel_FrameColor: TColor                       = $00B19F9F;
  xpOfficeSilver_GroupBar_ColorStart: TColor                    = $00D4C8C4;
  xpOfficeSilver_GroupBar_ColorStop: TColor                     = $00C8B3B1;

  xpOfficeSilver_CategoryGroup_CaptionBackColor                 = $00E0D7D6;
  xpOfficeSilver_CategoryGroup_CaptionBackColorStart            = $00FCFBFB;
  xpOfficeSilver_CategoryGroup_CaptionBackColorStop             = $00E0D7D6;
  xpOfficeSilver_CategoryGroup_CaptionFontColor                 = $003D3D3F;
  xpOfficeSilver_CategoryGroup_CaptionFontHotColor              = $007C7C7E;
  xpOfficeSilver_CategoryGroup_CaptionButtonColor               = $00FCFCFC;
  xpOfficeSilver_CategoryGroup_CaptionButtonBorderColor         = $00D1C5C3;
  xpOfficeSilver_CategoryGroup_CaptionDividerColor              = $00BFB4B2;
  xpOfficeSilver_CategoryGroup_GroupColor                       = $00F5F1F0;
  xpOfficeSilver_CategoryGroup_GroupBorderColor                 = clWhite;
  xpOfficeSilver_CategoryGroup_ItemFontColor                    = $003D3D3F;
  xpOfficeSilver_CategoryGroup_ItemFontHotColor                 = $007C7C7E;

  xpOfficeSilver_CategoryGroupSpecial_CaptionBackColor          = $00AB9594;
  xpOfficeSilver_CategoryGroupSpecial_CaptionBackColorStart     = $00A58D8C;
  xpOfficeSilver_CategoryGroupSpecial_CaptionBackColorStop      = $00C7B6B4;
  xpOfficeSilver_CategoryGroupSpecial_CaptionFontColor          = clWhite;
  xpOfficeSilver_CategoryGroupSpecial_CaptionFontHotColor       = $00E0D7D6;
  xpOfficeSilver_CategoryGroupSpecial_CaptionButtonColor        = $0096746E;
  xpOfficeSilver_CategoryGroupSpecial_CaptionButtonBorderColor  = $00DEC7C1;
  xpOfficeSilver_CategoryGroupSpecial_CaptionDividerColor       = $00BFB4B2;
  xpOfficeSilver_CategoryGroupSpecial_GroupColor                = $00F5F1F0;
  xpOfficeSilver_CategoryGroupSpecial_GroupBorderColor          = clWhite;

type
  TRzXPThemeColor = ( xptcEditBorder,
                      xptcNormalGroupFont,
                      xptcSpecialGroupFont,
                      xptcGroupBarFill,
                      xptcListGroupFill,
                      xptcListGroupFont,
                      xptcGroupBoxFont,
                      xptcSpinButtonBorder );

function GetXPThemeColor( Element: TRzXPThemeColor ): TColor;

type
  TRzXPColorScheme = ( xpcsBlue, xpcsGreen, xpcsSilver );

var
  CurrentXPColorScheme: TRzXPColorScheme;

function GetXPColorScheme: TRzXPColorScheme;

procedure GetGradientSelectionColors( ColorStyle: TRzGradientColorStyle;
                                      var FrameColor, StartColor, StopColor: TColor );

procedure GetGradientPanelColors( ColorStyle: TRzGradientColorStyle;
                                  var StartColor, StopColor: TColor );

function GetGradientPanelFrameColor( ColorStyle: TRzGradientColorStyle ): TColor;

procedure GetGradientStatusBarColors( ColorStyle: TRzGradientColorStyle;
                                      var StartColor, StopColor,
                                      DividerColor: TColor );

procedure GetGradientGroupBarColors( ColorStyle: TRzGradientColorStyle;
                                     var StartColor, StopColor: TColor );

procedure GetGradientCategoryGroupColors( ColorStyle: TRzGradientColorStyle;
                                          SpecialGroup: Boolean;
                                          var CaptionBackColor, CaptionBackColorStart,
                                          CaptionBackColorStop, CaptionFontColor,
                                          CaptionFontHotColor,
                                          CaptionButtonColor, CaptionButtonBorderColor,
                                          CaptionDividerColor, GroupColor,
                                          GroupBorderColor: TColor );

procedure GetGradientOutlookGroupColors( ColorStyle: TRzGradientColorStyle;
                                         var CaptionBackStartColor,
                                         CaptionBackStopColor, CaptionFontColor,
                                         CaptionFontHotColor,
                                         GroupStartColor, GroupStopColor: TColor );

procedure GetGradientGroupItemColors( ColorStyle: TRzGradientColorStyle;
                                      var ItemFontColor, ItemFontHotColor: TColor );

{== VCL Styles Support Functions ==}

{$IFNDEF VCL160_OR_HIGHER}
// TStyleColor and TStyleFont are declared for pre-XE2 versions to eliminate
// a lot of IFDEFs that would otherwise be needed in the code, and that in-turn
// would lead to lots of unnecessary compiler warnings.
type
  TStyleColor = (scBorder, scButtonDisabled, scButtonFocused, scButtonHot,
    scButtonNormal, scButtonPressed, scCategoryButtons, scCategoryButtonsGradientBase,
    scCategoryButtonsGradientEnd, scCategoryPanelGroup, scComboBox,
    scComboBoxDisabled, scEdit, scEditDisabled, scGrid, scGenericBackground,
    scGenericGradientBase, scGenericGradientEnd, scHintGradientBase,
    scHintGradientEnd, scListBox, scListBoxDisabled, scListView, scPanel, scPanelDisabled,
    scSplitter, scToolBarGradientBase, scToolBarGradientEnd, scTreeView, scWindow);

  TStyleFont = (
    sfButtonTextDisabled, sfButtonTextFocused, sfButtonTextHot, sfButtonTextNormal, sfButtonTextPressed,
    sfCaptionTextInactive, sfCaptionTextNormal,
    sfCategoryPanelGroupHeaderHot, sfCategoryPanelGroupHeaderNormal, sfCatgeoryButtonsCategoryNormal, sfCatgeoryButtonsCategorySelected,
    sfCatgeoryButtonsHot, sfCatgeoryButtonsNormal, sfCatgeoryButtonsSelected,
    sfCheckBoxTextDisabled, sfCheckBoxTextFocused, sfCheckBoxTextHot, sfCheckBoxTextNormal, sfCheckBoxTextPressed,
    sfComboBoxItemDisabled, sfComboBoxItemFocused, sfComboBoxItemHot, sfComboBoxItemNormal, sfComboBoxItemSelected,
    sfEditBoxTextDisabled, sfEditBoxTextFocused, sfEditBoxTextHot, sfEditBoxTextNormal, sfEditBoxTextSelected,
    sfGridItemFixedHot, sfGridItemFixedNormal, sfGridItemFixedPressed, sfGridItemNormal, sfGridItemSelected,
    sfGroupBoxTextDisabled, sfGroupBoxTextNormal,
    sfHeaderSectionTextDisabled, sfHeaderSectionTextHot, sfHeaderSectionTextNormal, sfHeaderSectionTextPressed,
    sfListItemTextDisabled, sfListItemTextFocused, sfListItemTextHot, sfListItemTextNormal, sfListItemTextSelected,
    sfMenuItemTextDisabled, sfMenuItemTextHot, sfMenuItemTextNormal, sfMenuItemTextSelected,
    sfPanelTextDisabled, sfPanelTextNormal,
    sfPopupMenuItemTextDisabled, sfPopupMenuItemTextHot, sfPopupMenuItemTextNormal, sfPopupMenuItemTextSelected,
    sfRadioButtonTextDisabled, sfRadioButtonTextFocused, sfRadioButtonTextHot, sfRadioButtonTextNormal, sfRadioButtonTextPressed,
    sfSmCaptionTextInactive, sfSmCaptionTextNormal,
    sfStatusPanelTextDisabled, sfStatusPanelTextNormal,
    sfTabTextActiveDisabled, sfTabTextActiveHot, sfTabTextActiveNormal, sfTabTextInactiveDisabled, sfTabTextInactiveHot, sfTabTextInactiveNormal,
    sfTextLabelDisabled, sfTextLabelFocused, sfTextLabelHot, sfTextLabelNormal,
    sfToolItemTextDisabled, sfToolItemTextHot, sfToolItemTextNormal, sfToolItemTextSelected,
    sfTreeItemTextDisabled, sfTreeItemTextFocused, sfTreeItemTextHot, sfTreeItemTextNormal, sfTreeItemTextSelected,
    sfWindowTextDisabled, sfWindowTextNormal
  );
{$ENDIF}


{$IFDEF VCL160_OR_HIGHER}
function ActiveStyleServices: TCustomStyleServices; inline;
{$ELSE}
function ActiveStyleServices: TThemeServices; inline;
{$ENDIF}
function ActiveStyleServicesEnabled: Boolean; inline;
function ActiveStyleServicesAvailable: Boolean; inline;

procedure ActiveStyleServicesDrawText( DC: HDC; Details: TThemedElementDetails; const S: string;
                                       R: TRect; Flags: Cardinal ); inline;


function UsingSystemStyle: Boolean;
function ActiveStyleSystemColor( C: TColor ): TColor;
function ActiveStyleColor( SC: TStyleColor ): TColor;
function ActiveStyleFontColor( SF: TStyleFont ): TColor;


var
  FullColorSupported: Boolean;

function IsFullColorSupported: Boolean;

procedure DrawDropShadow( Canvas: TCanvas; Bounds: TRect; Depth: Integer; ShadowColor: TColor = clBlack );

function DrawSides( Canvas: TCanvas; Bounds: TRect; ULColor, LRColor: TColor; Sides: TSides ): TRect;

function DrawBevel( Canvas: TCanvas; Bounds: TRect; ULColor, LRColor: TColor; Width: Integer; Sides: TSides ): TRect;

function DrawCtl3DBorder( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean ): TRect;

function DrawCtl3DBorderSides( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean; Sides: TSides ): TRect;

function DrawButtonBorder( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean ): TRect;

function DrawButtonBorderSides( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean; Sides: TSides ): TRect;

function DrawColorButtonBorder( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Lowered: Boolean ): TRect;

function DrawColorButtonBorderSides( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Lowered: Boolean; Sides: TSides ): TRect;

function DrawBorder( Canvas: TCanvas; Bounds: TRect; Style: TFrameStyle ): TRect;

function DrawBorderSides( Canvas: TCanvas; Bounds: TRect; Style: TFrameStyle; Sides: TSides ): TRect;

function DrawColorBorder( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Style: TFrameStyle ): TRect;

function DrawColorBorderSides( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Style: TFrameStyle; Sides: TSides ): TRect;

function DrawBox( Canvas: TCanvas; Bounds: TRect; LineColor: TColor ): TRect;

function DrawFilledBox( Canvas: TCanvas; Bounds: TRect; LineColor, FillColor: TColor ): TRect;

function DrawBoxCorners( Canvas: TCanvas; Bounds: TRect; LineColor: TColor;
                         CornerWidth: Integer ): TRect;

function DrawDashedBorder( Canvas: TCanvas; Bounds: TRect; DashColor: TColor ): TRect;

function DrawFocusBorder( Canvas: TCanvas; Bounds: TRect; Color: TColor = clNone ): TRect;

function DrawRoundedFlatBorder( Canvas: TCanvas; Bounds: TRect; Color: TColor; Sides: TSides ): TRect;

function DrawInnerOuterBorders( Canvas: TCanvas; Bounds: TRect;
                                BorderOuter, BorderInner: TFrameStyleEx;
                                BorderWidth: Integer; BorderSides: TSides; BevelWidth: Integer;
                                BorderColor, BorderHighlight, BorderShadow: TColor;
                                FlatColor: TColor; FlatColorAdjustment: Integer; Color, ParentColor: TColor;
                                Transparent: Boolean; SoftInnerFlatBorder: Boolean = False ): TRect;

procedure DrawGroupBarBackground( Canvas: TCanvas; Bounds: TRect;
                                  VisualStyle: TRzVisualStyle;
                                  ColorStyle: TRzGradientColorStyle;
                                  GradientPath: TRzGroupBarGradientPath;
                                  CustomStartColor, CustomStopColor: TColor );


procedure DrawLEDBar( Canvas: TCanvas; Bounds: TRect; Orientation: TOrientation; BarColor, BackColor: TColor;
                      NumSegments: Integer; Percent: Integer; ThemeAware, Transparent: Boolean );

procedure DrawPercentBar( Canvas: TCanvas; Bounds: TRect; Orientation: TOrientation;
                          BarColor, BackColor: TColor; Percent: Word;
                          ShowPercent, Transparent: Boolean;
                          ShowParts: Boolean = False;
                          PartsComplete: Longint = 0;
                          TotalParts: Longint = 0 );

procedure DrawGradientPercentBar( Canvas: TCanvas; Bounds: TRect; Orientation: TOrientation;
                                  BarColor, BarColorStop, BackColor, BackColorStop: TColor;
                                  gradientDirection: TGradientDirection;
                                  Percent: Word; ShowPercent, Transparent: Boolean;
                                  ShowParts: Boolean = False;
                                  PartsComplete: Longint = 0;
                                  TotalParts: Longint = 0 );


procedure DrawFrame( Canvas: TCanvas; Width, Height: Integer; FrameStyle: TFrameStyle; EraseColor, FrameColor: TColor;
                     FrameSides: TSides; Transparent: Boolean = False );

procedure InvalidateWindowFrame( Handle: HWnd; Bounds: TRect );

procedure InvalidateControls( Container: TWinControl );


type
  TRzUIStyle = ( uiWindows95, uiWindowsXP, uiWindowsVista, uiWindows10, uiCustomVclStyle, uiCustomColor );

procedure DrawDropDownArrow( Canvas: TCanvas; Bounds: TRect; UIStyle: TRzUIStyle; Down: Boolean;
                             Enabled: Boolean = True );

procedure DrawSpinArrow( Canvas: TCanvas; Bounds: TRect; Color, DisabledColor: TColor;
                         Direction: TDirection; Down: Boolean;
                         Enabled: Boolean = True ); overload;

procedure DrawSpinArrow( Canvas: TCanvas; Bounds: TRect; UIStyle: TRzUIStyle;
                         Direction: TDirection; Down: Boolean;
                         Enabled: Boolean = True ); overload;


procedure DrawCloseX( Canvas: TCanvas; Bounds: TRect; Color: TColor;
                      Down: Boolean; Enabled: Boolean = True ); overload;

procedure DrawCloseX( Canvas: TCanvas; Bounds: TRect; UIStyle: TRzUIStyle;
                      Down: Boolean; Enabled: Boolean = True ); overload;


procedure DrawHighlightBox( Canvas: TCanvas; Bounds: TRect;
                            GradientDirection: TGradientDirection;
                            StartColor, StopColor: TColor );


type
  TRzButtonDisplayState = ( bdsNormal, bdsDown, bdsHot, bdsDisabled );
  TRzButtonHotTrackStyle = ( htsInterior, htsFrame );


procedure DrawCheckBox( Canvas: TCanvas; Bounds: TRect; CheckState: TCheckBoxState;
                        DisplayState: TRzButtonDisplayState; Focused: Boolean;
                        HotTrackStyle: TRzButtonHotTrackStyle;
                        FrameColor, MarkColor, FillColor, FocusColor, DisabledColor,
                        HotTrackStartColor, HotTrackStopColor: TColor;
                        ReadOnly, ReadOnlyColorOnFocus: Boolean;
                        ReadOnlyColor: TColor );

procedure DrawRadioButton( Canvas: TCanvas; Bounds: TRect; Checked: Boolean;
                           DisplayState: TRzButtonDisplayState; Focused: Boolean;
                           HotTrackStyle: TRzButtonHotTrackStyle;
                           FrameColor, MarkColor, FillColor, FocusColor, DisabledColor,
                           HotTrackStartColor, HotTrackStopColor,
                           BackgroundColor: TColor;
                           Transparent: Boolean; TransparentColor: TColor;
                           ReadOnly, ReadOnlyColorOnFocus: Boolean;
                           ReadOnlyColor: TColor );


procedure AddImageToImageList( ImageList: TCustomImageList; Glyph: TBitmap; AddDisabled: Boolean;
                               var ImageIndex, DisabledIndex: Integer );


{ Function to get a valid new component name.  Used by Component Editors }

function GetNewComponentName( AOwner: TComponent; const BaseName: string; TryNoIndex: Boolean = True ): string;
function CreateValidIdent( const Ident, DefaultName: string ): string;

function IsTrueTypeFont( Font: TFont ): Boolean;
function RotateFont( Font: TFont; Angle: Integer ): HFont;
function GetMinFontHeight( Font: TFont ): Integer;
function GetAvgCharWidth( Font: TFont ): Integer;

{$IFNDEF UNICODE}
function IsLeadChar( C: Char ): Boolean;
function CharInSet( C: Char; const CharSet: TSysCharSet ): Boolean;
{$ENDIF}
function FirstNonWhitespaceChar( const S: string ): Char;
function LastChar( const S: string ): Char;
function CountChar( C: Char; const S: string ): Integer;
function CopyEx( const S: string; Start: Integer; C: Char; Count: Integer ): string;
function RemoveChar( var S: string; C: Char ): Boolean;
function RemoveAccelerators( const S: string ): string;
function ExpandEnvironmentVariables( const S: string ): string;

function Min( A, B: Integer ): Integer;
function Max( A, B: Integer ): Integer;

procedure Swap( var A, B: Integer ); overload;
procedure Swap( var A, B: Word ); overload;

procedure UpdateObjectInspector( AControl: TControl );


var
  IncrementalSearchResetDelay: Integer = 1500;


type
  TRzOldPropReader = class
    class procedure ReadOldBooleanProp( Reader: TReader );
    class procedure ReadOldEnumProp( Reader: TReader );
    class procedure ReadOldIdentProp( Reader: TReader );
    class procedure ReadOldIntegerProp( Reader: TReader );
    class procedure ReadOldSetProp( Reader: TReader );
    class procedure ReadOldStringProp( Reader: TReader );
    class procedure WriteOldProp( Writer: TWriter );
  end;


{== Base Dialog Component Helper Functions and Class ==}

function GetDesktopClientRect: TRect;
function GetActiveWorkArea( Window: TWinControl ): TRect;
function GetActiveWorkAreaWidth( Window: TWinControl ): Integer;
function GetActiveWorkAreaHeight( Window: TWinControl ): Integer;
function GetMonitorWorkArea( Monitor: TMonitor ): TRect;
function GetMonitorBoundsRect( Monitor: TMonitor ): TRect;
function GetMonitorContainingPoint( P: TPoint ): TMonitor;

procedure CenterToWindow( ChildWin, Window: TWinControl; var Left, Top: Integer );
procedure CenterToForm( ChildWin: TWinControl; AForm: TCustomForm; var Left, Top: Integer );
procedure CenterToMDIChild( ChildWin: TWinControl; AForm: TForm; var Left, Top: Integer );

type
  {==========================================}
  {== TRzDialogComponent Class Declaration ==}
  {==========================================}

  TRzDialogComponent = class( TComponent )
  private
    FBorderStyle: TFormBorderStyle;
    FCaption: string;
    FCaptionOk: string;
    FCaptionCancel: string;
    FCaptionHelp: string;
    FCenterToParent: Boolean;
    FFont: TFont;
    FFrameColor: TColor;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FHeight: Integer;
    FHelpContext: THelpContext;
    FOriginLeft: Integer;
    FOriginTop: Integer;
    FWidth: Integer;
    FPosition: TPosition;
  protected
    FAboutInfo: TRzAboutInfo;
    procedure CenterForm( Dlg: TForm );

    { Property Access Methods }
    procedure SetFont( Value: TFont ); virtual;
    procedure SetCenterToParent( Value: Boolean ); virtual;

    { Property Declarations }
    property BorderStyle: TFormBorderStyle
      read FBorderStyle
      write FBorderStyle
      default bsSizeable;

    property Caption: string
      read FCaption
      write FCaption;

    property CaptionOK: string
      read FCaptionOK
      write FCaptionOK;

    property CaptionCancel: string
      read FCaptionCancel
      write FCaptionCancel;

    property CaptionHelp: string
      read FCaptionHelp
      write FCaptionHelp;

    property Font: TFont
      read FFont
      write SetFont;

    property FrameColor: TColor
      read FFrameColor
      write FFrameColor
      default clBtnShadow;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write FFrameStyle
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write FFrameVisible
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write FFramingPreference
      default fpXPThemes;

    property Height: Integer
      read FHeight
      write FHeight
      default 300;

    property HelpContext: THelpContext
      read FHelpContext
      write FHelpContext
      default 0;

    property Width: Integer
      read FWidth
      write FWidth
      default 350;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property CenterToParent: Boolean
      read FCenterToParent
      write SetCenterToParent
      default False;

    property OriginLeft: Integer
      read FOriginLeft
      write FOriginLeft
      default 100;

    property Position: TPosition
      read FPosition
      write FPosition
      default poScreenCenter;

    property OriginTop: Integer
      read FOriginTop
      write FOriginTop
      default 100;
  end;


type
  TRzFrameControllerProperty =
    ( fcpColor, fcpFocusColor, fcpDisabledColor,
      fcpReadOnlyColor, fcpReadOnlyColorOnFocus,
      fcpParentColor, fcpFlatButtonColor, fcpFlatButtons, fcpFrameColor,
      fcpFrameHotColor, fcpFrameHotTrack, fcpFrameHotStyle, fcpFrameSides,
      fcpFrameStyle, fcpFrameVisible, fcpFramingPreference,
      fcpAll );

  TRzFrameControllerPropertyConnection = fcpColor..fcpFramingPreference;
  TRzFrameControllerNotifications = set of TRzFrameControllerPropertyConnection;

const
  fccAll = [ fcpColor, fcpFocusColor, fcpDisabledColor,
             fcpReadOnlyColor, fcpReadOnlyColorOnFocus,
             fcpParentColor, fcpFlatButtonColor, fcpFlatButtons, fcpFrameColor,
             fcpFrameHotColor, fcpFrameHotTrack, fcpFrameHotStyle, fcpFrameSides,
             fcpFrameStyle, fcpFrameVisible, fcpFramingPreference ];

  fcpColorBit                   = $00000001;
  fcpFocusColorBit              = $00000002;
  fcpDisabledColorBit           = $00000004;
  fcpReadOnlyColorBit           = $00000008;
  fcpReadOnlyColorOnFocusBit    = $00000010;
  fcpParentColorBit             = $00000020;
  fcpFlatButtonColorBit         = $00000040;
  fcpFlatButtonsBit             = $00000080;
  fcpFrameColorBit              = $00000100;
  fcpFrameHotColorBit           = $00000200;
  fcpFrameHotTrackBit           = $00000400;
  fcpFrameHotStyleBit           = $00000800;
  fcpFrameSidesBit              = $00001000;
  fcpFrameStyleBit              = $00002000;
  fcpFrameVisibleBit            = $00004000;
  fcpFramingPreferenceBit       = $00008000;


type
  TRzRegIniFile = class;

  {==========================================}
  {== TRzFrameController Class Declaration ==}
  {==========================================}

  IRzCustomFramingNotification = interface
    ['{64B1C9EA-C954-428A-95C4-4EA2EB0F1E16}']
    procedure CustomFramingChanged;
  end;


  TRzFrameController = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FColor: TColor;
    FParentColor: Boolean;
    FFrameList: TList;
    FFlatButtonColor: TColor;
    FFlatButtons: Boolean;
    FDisabledColor: TColor;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;
    FFocusColor: TColor;
    FFrameColor: TColor;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FUpdateCount: Integer;
    FRegIniFile: TRzRegIniFile;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    function StoreColor: Boolean;
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function GetNotifications( C: TComponent ): TRzFrameControllerNotifications;

    procedure UpdateControlFrame( C: TComponent;
                                  FrameProperty: TRzFrameControllerProperty ); virtual;
    procedure UpdateFrames( FrameProperty: TRzFrameControllerProperty ); virtual;

    { Property Access Methods }
    procedure SetColor( Value: TColor ); virtual;
    procedure SetParentColor( Value: Boolean ); virtual;
    procedure SetFlatButtonColor( Value: TColor ); virtual;
    procedure SetFlatButtons( Value: Boolean ); virtual;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetReadOnlyColor( Value: TColor ); virtual;
    procedure SetReadOnlyColorOnFocus( Value: Boolean ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;

    procedure SetRegIniFile( Value: TRzRegIniFile ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateControls;

    procedure AddControl( C: TComponent );
    procedure RemoveControl( C: TComponent );

    procedure Load( const Section: string );
    procedure Save( const Section: string );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Color: TColor
      read FColor
      write SetColor
      stored StoreColor
      default clWindow;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      default clBtnFace;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      default clInfoBk;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write SetReadOnlyColorOnFocus
      default False;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write SetFlatButtonColor
      default clBtnFace;

    property FlatButtons: Boolean
      read FFlatButtons
      write SetFlatButtons
      default True;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      default clBtnShadow;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      default fpXPThemes;

    property ParentColor: Boolean
      read FParentColor
      write SetParentColor
      default False;

    property RegIniFile: TRzRegIniFile
      read FRegIniFile
      write SetRegIniFile;
  end;


  {=====================================}
  {== TRzRegIniFile Class Declaration ==}
  {=====================================}

  ENoRegIniFile = class( Exception );

  TRzPathType = ( ptIniFile, ptRegistry );

  TRzRegKey = ( hkeyClassesRoot, hkeyCurrentUser, hkeyLocalMachine, hkeyUsers,
                hkeyPerformanceData, hkeyCurrentConfig, hkeyDynData );


  TRzRegAccessKey = ( keyQueryValue, keySetValue, keyCreateSubKey,
                      keyEnumerateSubKeys, keyNotify, keyCreateLink,
                      keyRead, keyWrite, keyExecute, keyAllAccess );
  TRzRegAccess = set of TRzRegAccessKey;

  TRzSpecialFolder = ( sfNone, sfUserAppDataRoaming, sfUserAppDataLocal,
                       sfUserDocuments, sfProgramData );

  TRzIniFileEncoding = ( feDefault, feUTF8, feUnicode );

  TRzRegIniFile = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FPath: string;
    FPathType: TRzPathType;
    FRegKey: TRzRegKey;
    FRegAccess: TRzRegAccess;
    FSpecialFolder: TRzSpecialFolder;
    FFileEncoding: TRzIniFileEncoding;

    FRefreshStorage: Boolean;
    FAutoUpdateIniFile: Boolean;

    FIni: TMemIniFile;
    FReg: TRegistryIniFile;
  protected
    procedure CheckAccess;

    procedure SetPath( const Value: string ); virtual;
    procedure SetPathType( Value: TRzPathType ); virtual;
    procedure SetRegKey( Value: TRzRegKey ); virtual;
    procedure SetRegAccess( Value: TRzRegAccess ); virtual;
    procedure SetSpecialFolder( Value: TRzSpecialFolder ); virtual;
    procedure SetFileEncoding( Value: TRzIniFileEncoding ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure UpdateIniFile;

    function SectionExists( const Section: string ): Boolean;
    function ValueExists( const Section, Name: string ): Boolean;

    function ReadBool( const Section, Name: string; Default: Boolean ): Boolean;
    procedure WriteBool( const Section, Name: string; Value: Boolean );

    function ReadDate( const Section, Name: string; Default: TDateTime ): TDateTime;
    procedure WriteDate( const Section, Name: string; Value: TDateTime );

    function ReadDateTime( const Section, Name: string; Default: TDateTime ): TDateTime;
    procedure WriteDateTime( const Section, Name: string; Value: TDateTime );

    function ReadFloat( const Section, Name: string; Default: Double ): Double;
    procedure WriteFloat( const Section, Name: string; Value: Double );

    function ReadInteger( const Section, Name: string; Default: Longint ): Longint;
    procedure WriteInteger( const Section, Name: string; Value: Longint );

    function ReadString( const Section, Name, Default: string ): string;
    procedure WriteString( const Section, Name, Value: string );

    function ReadTime( const Section, Name: string; Default: TDateTime ): TDateTime;
    procedure WriteTime( const Section, Name: string; Value: TDateTime );

    function ReadBinaryStream( const Section, Name: string; Value: TStream ): Integer;
    procedure WriteBinaryStream( const Section, Name: string; Value: TStream );

    procedure ReadSection( const Section: string; Strings: TStrings );
    procedure ReadSections( Strings: TStrings );
    procedure ReadSectionValues( const Section: string; Strings: TStrings );

    procedure EraseSection( const Section: string );
    procedure DeleteKey( const Section, Name: string );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AutoUpdateIniFile: Boolean
      read FAutoUpdateIniFile
      write FAutoUpdateIniFile
      default True;

    property FileEncoding: TRzIniFileEncoding
      read FFileEncoding
      write SetFileEncoding
      default feDefault;

    property Path: string
      read FPath
      write SetPath;

    property PathType: TRzPathType
      read FPathType
      write SetPathType
      default ptIniFile;

    property RegKey: TRzRegKey
      read FRegKey
      write SetRegKey
      default hkeyCurrentUser;

    property RegAccess: TRzRegAccess
      read FRegAccess
      write SetRegAccess
      default [ keyAllAccess ];

    property SpecialFolder: TRzSpecialFolder
      read FSpecialFolder
      write SetSpecialFolder
      default sfNone;
  end;


  {==============================}
  {== TRzPropertyStore Classes ==}
  {==============================}

  TRzPropertyStore = class;

  TRzPropertyItem = class( TCollectionItem )
  private
    FComponent: TComponent;
    FPropertyName: string;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create( Collection: TCollection ); override;

    procedure Assign( Source: TPersistent ); override;
  published
    property Component: TComponent
      read FComponent
      write FComponent;

    property PropertyName: string
      read FPropertyName
      write FPropertyName;
  end;


  TRzPropertyCollection = class( TCollection )
  private
    FStore: TRzPropertyStore;
    function GetItem( Index: Integer ): TRzPropertyItem;
    procedure SetItem( Index: Integer; Value: TRzPropertyItem );
  protected
    function GetOwner: TPersistent; override;
  public
    // Note: No override on constructor
    constructor Create( Store: TRzPropertyStore );

    function Add: TRzPropertyItem;
    function AddProperty( Component: TComponent;
                          const PropertyName: string ): TRzPropertyItem;

    // Array property provides access to collection items
    property Items[ Index: Integer ]: TRzPropertyItem
      read GetItem
      write SetItem; default;

    property Store: TRzPropertyStore
      read FStore;
  end;


  TRzPropertyStore = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FProperties: TRzPropertyCollection;
    FRegIniFile: TRzRegIniFile;
    FSection: string;

    procedure SaveProperties( const Section, ParentPropName: string; Obj: TObject );
    procedure SaveCollection( const Section, FullPropName: string; Collection: TCollection );
    procedure SaveStringList( const Section, FullPropName: string; StrList: TStrings );
    procedure LoadProperties( const Section, ParentPropName: string; Obj: TObject );
    procedure LoadCollection( const Section, FullPropName: string; Collection: TCollection );
    procedure LoadStringList( const Section, FullPropName: string; StrList: TStrings );
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function GetSection: string;

    // Property Access Methods
    procedure SetProperties( Value: TRzPropertyCollection ); virtual;
    procedure SetRegIniFile( Value: TRzRegIniFile ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Save; virtual;
    procedure Load; virtual;

    procedure AddProperty( Component: TComponent; const PropertyName: string );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Properties: TRzPropertyCollection
      read FProperties
      write SetProperties;

    property RegIniFile: TRzRegIniFile
      read FRegIniFile
      write SetRegIniFile;

    property Section: string
      read FSection
      write FSection;
  end;


  {=======================================}
  {== TRzCustomColors Class Declaration ==}
  {=======================================}

  TRzCustomColors = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FColors: TStrings;
    FRegIniFile: TRzRegIniFile;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure InitColors; virtual;
    function GetColorName( Index: Integer ): string;
    procedure FixupColors;

    // Property Access Methods
    procedure SetColors( Value: TStrings ); virtual;
    procedure SetRegIniFile( Value: TRzRegIniFile ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Load( const Section: string );
    procedure Save( const Section: string );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Colors: TStrings
      read FColors
      write SetColors;

    property RegIniFile: TRzRegIniFile
      read FRegIniFile
      write SetRegIniFile;
  end;


  {=========================================}
  {== TRzMenuController Class Declaration ==}
  {=========================================}

  TRzMenuController = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FMenuItemList: TList;

    FGradientColorStyle: TRzGradientColorStyle;
    // No need for VisualStyle property b/c TRzMenuController is designed
    // to only support vsGradient style. There is no XP specific style of menu.

    FSelectionColorStart: TColor;
    FSelectionColorStop: TColor;
    FSelectionFrameColor: TColor;

    FIconBarColorStart: TColor;
    FIconBarColorStop: TColor;

    FMenuColor: TColor;
    FMenuFontColor: TColor;
    FMenuFont: TFont;
    FUseCustomMenuFont: Boolean;
    FUseMenuColorForMainMenu: Boolean;
    FMainMenuBrushHandle: HBRUSH;

    function HideMenuPrefix: Boolean;

    procedure ReadOldGradientColorStartProp( Reader: TReader );
    procedure ReadOldGradientColorStopProp( Reader: TReader );
    procedure ReadOldFrameColorProp( Reader: TReader );

    procedure SetMenuFontColor( Value: TColor );
    procedure SetMenuFont( Value: TFont );
    procedure SetUseMenuColorForMainMenu( Value: Boolean );
    procedure UpdateMainMenuColor;

    { Internal Event Handlers }
    procedure AdvancedDrawItemHandler( Sender: TObject; ACanvas: TCanvas;
                                       ARect: TRect; State: TOwnerDrawState );
    procedure MeasureItemHandler( Sender: TObject; ACanvas: TCanvas;
                                  var Width, Height: Integer );
    procedure MenuFontChangeHandler( Sender: TObject );
  protected
    procedure DefineProperties( Filer: TFiler ); override;

    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure SetupMenus;
    procedure SetupMenuItems;
    procedure SetupMenuItem( Item: TMenuItem );

    procedure CleanupMenuItems;
    procedure CleanupMenuItem( Item: TMenuItem );

    function MenuIsRightToLeft( Item: TMenuItem ): Boolean;

    procedure MeasureMenuItem( Item: TMenuItem; Canvas: TCanvas;
                               var Width, Height: Integer );
    procedure PaintMenuItem( Item: TMenuItem; Canvas: TCanvas; Rect: TRect;
                             State: TOwnerDrawState );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property GradientColorStyle: TRzGradientColorStyle
      read FGradientColorStyle
      write FGradientColorStyle
      default gcsSystem;

    property SelectionColorStart: TColor
      read FSelectionColorStart
      write FSelectionColorStart
      default clBtnFace;

    property SelectionColorStop: TColor
      read FSelectionColorStop
      write FSelectionColorStop
      default clBtnShadow;

    property SelectionFrameColor: TColor
      read FSelectionFrameColor
      write FSelectionFrameColor
      default cl3DDkShadow;

    property IconBarColorStart: TColor
      read FIconBarColorStart
      write FIconBarColorStart
      default clWhite;

    property IconBarColorStop: TColor
      read FIconBarColorStop
      write FIconBarColorStop
      default clBtnFace;

    property MenuColor: TColor
      read FMenuColor
      write FMenuColor
      default clWindow;

    property MenuFontColor: TColor
      read FMenuFontColor
      write SetMenuFontColor
      default clWindowText;

    property MenuFont: TFont
      read FMenuFont
      write SetMenuFont
      stored FUseCustomMenuFont;

    property UseCustomMenuFont: Boolean
      read FUseCustomMenuFont
      write FUseCustomMenuFont
      default False;

    property UseMenuColorForMainMenu: Boolean
      read FUseMenuColorForMainMenu
      write SetUseMenuColorForMainMenu
      default False;

  end;


{== Windows API Support Functions ==}

type
  TRzWindowsVersion = ( win95, winNT, win98, winMe, win2000, winXP,
                        winServer2003, winVista, win7, win8, win10 );

function RunningUnder( ver: TRzWindowsVersion ): Boolean;
function RunningAtLeast( ver: TRzWindowsVersion ): Boolean;


function DrawString( Canvas: TCanvas; const S: string; var Bounds: TRect;
                     Flags: UINT ): Integer;
procedure DrawStringCentered( Canvas: TCanvas; const S: string; Bounds: TRect );


{$IFNDEF VCL150_OR_HIGHER}
var
  FormatSettings: TFormatSettings;
{$ENDIF}


resourcestring
  sRzNoRegIniFile = 'No TRzRegIniFile component specified';
  sRzCannotLoadCustomFraming = 'Cannot load Custom Framing settings--No TRzRegIniFile component specified';
  sRzCannotSaveCustomFraming = 'Cannot save Custom Framing settings--No TRzRegIniFile component specified';
  sRzCannotLoadCustomColors = 'Cannot load Custom Colors--No TRzRegIniFile component specified';
  sRzCannotSaveCustomColors = 'Cannot save Custom Colors--No TRzRegIniFile component specified';
  sRzCannotLoadProperties = 'Cannot load property settings--No TRzRegIniFile component specified';
  sRzCannotSaveProperties = 'Cannot save property settings--No TRzRegIniFile component specified';
  sRzCannotRestoreFormState = 'Cannot restore Form State--No TRzRegIniFile component specified';
  sRzCannotSaveFormState = 'Cannot save Form State--No TRzRegIniFile component specified';

{&RG}

implementation

uses
  MultiMon,
  Messages,
  Consts,
  Dialogs,
  TypInfo,
  ShlObj,
  {$IFDEF UNICODE}
  Character,
  {$ENDIF}
  RzGrafx;


procedure FreeBlinkingControlsListIfEmpty;
begin
  if BlinkingControls.Count = 0 then
  begin
    { If no more Blinking controls are left, destroy BlinkingControls }
    BlinkingControls.Free;
    BlinkingControls := nil;
  end;
end;


{=====================================}
{== TRzBlinkingControlsList Methods ==}
{=====================================}

constructor TRzBlinkingControlsList.Create;
begin
  inherited;

  FBlinkState := bsOff;
  FIntervalOff := 500;
  FIntervalOn := 500;

  FTimer := TTimer.Create( nil );
  FTimer.OnTimer := TimerFired;
  FTimer.Interval := FIntervalOff;
  FTimer.Enabled := True;

  FControls := TList.Create;
end;


destructor TRzBlinkingControlsList.Destroy;
begin
  FTimer.Free;
  FControls.Free;
  inherited;
end;


procedure TRzBlinkingControlsList.Add( Control: TControl );
begin
  FControls.Add( Control );
end;


procedure TRzBlinkingControlsList.Remove( Control: TControl );
begin
  with FControls do
    Delete( IndexOf( Control ) );
end;

procedure TRzBlinkingControlsList.TimerFired( Sender: TObject );
var
  I: Integer;
begin
  if FBlinkState = bsOn then
  begin
    FBlinkState := bsOff;
    FTimer.Interval := FIntervalOn;
  end
  else
  begin
    FBlinkState := bsOn;
    FTimer.Interval := FIntervalOff;
  end;

  for I := 0 to FControls.Count - 1 do
  begin
    if TControl( FControls.Items[ I ] ).Perform( cm_GetBlinking, 0, 0 ) = 1 then
    begin
      TControl( FControls.Items[ I ] ).Perform( cm_Blink, Integer( FBlinkState ), 0 );
    end;
  end;
end;

function TRzBlinkingControlsList.GetCount: Integer;
begin
  Result := FControls.Count;
end;


{=======================}
{== Drawing functions ==}
{=======================}

function CenterRect( R: TRect; Width, Height: Integer ): TRect;
begin
  Result := Bounds( ( R.Right + R.Left - Width ) div 2, ( R.Bottom + R.Top - Height ) div 2, Width, Height );
end;


procedure ColorToHSL( C: TColor; var H, S, L: Byte );
var
  Dif, CCmax, CCmin, RC, GC, BC, TempH, TempS, TempL: Double;
begin
  { Convert RGB color to Hue, Saturation and Luminance }

  { Convert Color to RGB color value. This is necessary if Color specifies
    a system color such as clHighlight }
  C := ColorToRGB( C );

  { Determine a percent (as a decimal) for each colorant }
  RC := GetRValue( C ) / 255;
  GC := GetGValue( C ) / 255;
  BC := GetBValue( C ) / 255;

  if RC > GC then
    CCmax := RC
  else
    CCmax := GC;
  if BC > CCmax then
    CCmax := BC;

  if RC < GC then
    CCmin := RC
  else
    CCmin := GC;

  if BC < CCmin then
    CCmin := BC;

  { Calculate Luminance }
  TempL := (CCmax + CCmin) / 2.0;

  if CCmax = CCmin then
  begin
    TempS := 0;
    TempH := 0;
  end
  else
  begin
    Dif := CCmax - CCmin;

    { Calculate Saturation }
    if TempL < 0.5 then
      TempS := Dif / (CCmax + CCmin)
    else
      TempS := Dif / ( 2.0 - CCmax - CCmin );

    { Calculate Hue }
    if RC = CCmax then
      TempH := (GC - BC) / Dif
    else if GC = CCmax then
      TempH := 2.0 + (BC - RC) / Dif
    else
      TempH := 4.0 + (RC - GC) / Dif;

    TempH := TempH / 6;
    if TempH < 0 then
      TempH := TempH + 1;
  end;

  H := Round( 240 * TempH );
  S := Round( 240 * TempS );
  L := Round( 240 * TempL );
end; {= ColorToHSL =}


function ColorHue( C: TColor ): Byte;
var
  S, L: Byte;
begin
  ColorToHSL( C, Result, S, L );
end;


function ColorSaturation( C: TColor ): Byte;
var
  H, L: Byte;
begin
  ColorToHSL( C, H, Result, L );
end;


function ColorLuminance( C: TColor ): Byte;
var
  H, S: Byte;
begin
  ColorToHSL( C, H, S, Result );
end;


function HSLtoColor( H, S, L: Byte ): TColor;
var
  HN, SN, LN, RD, GD, BD, V, M, SV, Fract, VSF, Mid1, Mid2: Double;
  R, G, B: Byte;
  Sextant: Integer;
begin
  { Hue, Saturation, and Luminance must be normalized to 0..1 }

  HN := H / 239;
  SN := S / 240;
  LN := L / 240;

  if LN < 0.5 then
    V := LN * ( 1.0 + SN )
  else
    V := LN + SN - LN * SN;
  if V <= 0 then
  begin
    RD := 0.0;
    GD := 0.0;
    BD := 0.0;
  end
  else
  begin
    M := LN + LN - V;
    SV := (V - M ) / V;
    HN := HN * 6.0;
    Sextant := Trunc( HN );
    Fract := HN - Sextant;
    VSF := V * SV * Fract;
    Mid1 := M + VSF;
    Mid2 := V - VSF;

    case Sextant of
      0:
      begin
        RD := V;
        GD := Mid1;
        BD := M;
      end;

      1:
      begin
        RD := Mid2;
        GD := V;
        BD := M;
      end;

      2:
      begin
        RD := M;
        GD := V;
        BD := Mid1;
      end;

      3:
      begin
        RD := M;
        GD := Mid2;
        BD := V;
      end;

      4:
      begin
        RD := Mid1;
        GD := M;
        BD := V;
      end;

      5:
      begin
        RD := V;
        GD := M;
        BD := Mid2;
      end;

      else
      begin
        RD := V;
        GD := Mid1;
        BD := M;
      end;
    end;
  end;

  if RD > 1.0 then
    RD := 1.0;
  if GD > 1.0 then
    GD := 1.0;
  if BD > 1.0 then
    BD := 1.0;
  R := Round( RD * 255 );
  G := Round( GD * 255 );
  B := Round( BD * 255 );
  Result := RGB( R, G, B );
end; {= HSLtoColor =}


function DarkerColor( C: TColor; Adjustment: Byte ): TColor;
var
  H, S, L: Byte;
begin
  ColorToHSL( C, H, S, L );
  Result := HSLtoColor( H, S, Max( L - Adjustment, 0 ) );
end;


function LighterColor( C: TColor; Adjustment: Byte ): TColor;
var
  H, S, L: Byte;
begin
  ColorToHSL( C, H, S, L );
  Result := HSLtoColor( H, S, Min( L + Adjustment, 255 ) );
end;


function AdjustColor( C: TColor; Adjustment: Integer ): TColor;
begin
  Result := C;
  if Adjustment < 0 then
    Result := DarkerColor( C, -Adjustment )
  else if Adjustment > 0 then
    Result := LighterColor( C, Adjustment );
end;


function BlendColors( ForeColor, BackColor: TColor; Alpha: Byte ): TColor;
var
  ForeRed, ForeGreen, ForeBlue: Byte;
  BackRed, BackGreen, BackBlue: Byte;
  BlendRed, BlendGreen, BlendBlue: Byte;
  AlphaValue: Single;
begin
  AlphaValue := Alpha / 255;

  ForeColor := ColorToRGB( ForeColor );
  ForeRed   := GetRValue( ForeColor );
  ForeGreen := GetGValue( ForeColor );
  ForeBlue  := GetBValue( ForeColor );

  BackColor := ColorToRGB( BackColor );
  BackRed   := GetRValue( BackColor );
  BackGreen := GetGValue( BackColor );
  BackBlue  := GetBValue( BackColor );

  BlendRed := Round( AlphaValue * ForeRed + ( 1 - AlphaValue ) * BackRed );
  BlendGreen := Round( AlphaValue * ForeGreen + ( 1 - AlphaValue ) * BackGreen );
  BlendBlue := Round( AlphaValue * ForeBlue + ( 1 - AlphaValue ) * BackBlue );

  Result := RGB( BlendRed, BlendGreen, BlendBlue );
end;


function XorColors( ForeColor, BackColor: TColor ): TColor;
var
  R, G, B: Byte;
  FC, BC: TColor;
begin
  FC := ColorToRGB( ForeColor );
  BC := ColorToRGB( BackColor );

  R := GetRValue( FC ) xor GetRValue( BC );
  G := GetGValue( FC ) xor GetGValue( BC );
  B := GetBValue( FC ) xor GetBValue( BC );

  Result := RGB( R, G, B );
end;


function ColorsTooClose( ForeColor, BackColor: TColor ): Boolean;
var
  ForeH, ForeS, ForeL: Byte;
  BackH, BackS, BackL: Byte;
begin
  ColorToHSL( ForeColor, ForeH, ForeS, ForeL );
  ColorToHSL( BackColor, BackH, BackS, BackL );

  Result := ( Abs( ForeH - BackH ) < 20 ) and
            ( Abs( ForeL - BackL ) < 60 );
end;


function ComplementaryColor( C: TColor ): TColor;
begin
  Result := ComplementaryColor( C, ColorLuminance( C ) );
end;


function ComplementaryColor( C: TColor; Luminance: Byte ): TColor;
var
  H: Byte;
begin
  H := ColorHue( C );
  if H >= 120 then
    H := H - 120
  else
    H := H + 120;
  Result := HSLToColor( H, 240, Luminance );
end;


function GetXPThemeColor( Element: TRzXPThemeColor ): TColor;
var
  C: DWord;
  ElementDetails: TThemedElementDetails;
begin
  if not ActiveStyleServicesEnabled then
  begin
    Result := clBtnFace;
    Exit;
  end;

  case Element of
    xptcEditBorder:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( teEditRoot );
      GetThemeColor( ActiveStyleServices.Theme[ teEdit ], ElementDetails.Part,
                     ElementDetails.State, TMT_BORDERCOLOR, C );
    end;

    xptcNormalGroupFont:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupBackground );
      GetThemeColor( ActiveStyleServices.Theme[ teExplorerBar ], ElementDetails.Part,
                     ElementDetails.State, TMT_TEXTCOLOR, C );
    end;

    xptcSpecialGroupFont:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupBackground );
      GetThemeColor( ActiveStyleServices.Theme[ teExplorerBar ], ElementDetails.Part,
                     ElementDetails.State, TMT_TEXTCOLOR, C );
    end;

    xptcGroupBarFill:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupBackground );
      GetThemeColor( ActiveStyleServices.Theme[ teExplorerBar ], ElementDetails.Part,
                     ElementDetails.State, TMT_FILLCOLOR, C );
    end;

    xptcListGroupFill:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tebExplorerBarRoot );
      GetThemeColor( ActiveStyleServices.Theme[ teExplorerBar ], ElementDetails.Part,
                     ElementDetails.State, TMT_GRADIENTCOLOR1, C );
    end;

    xptcListGroupFont:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupHead );
      GetThemeColor( ActiveStyleServices.Theme[ teExplorerBar ], ElementDetails.Part,
                     ElementDetails.State, TMT_FILLCOLOR, C );
    end;

    xptcGroupBoxFont:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tbGroupBoxNormal );
      GetThemeColor( ActiveStyleServices.Theme[ teButton ], ElementDetails.Part,
                     ElementDetails.State, TMT_TEXTCOLOR, C );
    end;

    xptcSpinButtonBorder:
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tsSpinRoot );
      GetThemeColor( ActiveStyleServices.Theme[ teSpin ], ElementDetails.Part,
                     ElementDetails.State, TMT_BORDERCOLOR, C );
    end;

    else
      C := clWhite;
  end; { case Element }

  Result := C;
end; {= GetXPThemeColor =}


function GetXPColorScheme: TRzXPColorScheme;
var
  R: TRegIniFile;
  ColorName: string;
begin
  Result := xpcsBlue;
  R := TRegIniFile.Create( '\Software\Microsoft\Windows\CurrentVersion' );
  try
    ColorName := UpperCase( R.ReadString( 'ThemeManager', 'ColorName', '' ) );
    if ColorName = 'HOMESTEAD' then
      Result := xpcsGreen
    else if ColorName = 'METALLIC' then
      Result := xpcsSilver;
  finally
    R.Free;
  end;
end;


procedure GetGradientSelectionColors( ColorStyle: TRzGradientColorStyle;
                                      var FrameColor, StartColor, StopColor: TColor );
var
  C: TColor;
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        FrameColor := xpOfficeBlue_Selection_FrameColor;
        StartColor := xpOfficeBlue_Selection_ColorStart;
        StopColor  := xpOfficeBlue_Selection_ColorStop;
      end;

      xpcsGreen:
      begin
        FrameColor := xpOfficeGreen_Selection_FrameColor;
        StartColor := xpOfficeGreen_Selection_ColorStart;
        StopColor  := xpOfficeGreen_Selection_ColorStop;
      end;

      xpcsSilver:
      begin
        FrameColor := xpOfficeSilver_Selection_FrameColor;
        StartColor := xpOfficeSilver_Selection_ColorStart;
        StopColor  := xpOfficeSilver_Selection_ColorStop;
      end;
    end;
  end
  else
  begin
    FrameColor := ActiveStyleSystemColor( clHighlight );
    C := BlendColors( ActiveStyleSystemColor( clHighlight ), ActiveStyleSystemColor( clWindow ), 77 );
    StartColor := LighterColor( C, 20 );
    StopColor := C;
  end;

  if not FullColorSupported then
    StartColor := StopColor;
end; {= GetGradientSelectionColors =}


procedure GetGradientPanelColors( ColorStyle: TRzGradientColorStyle;
                                  var StartColor, StopColor: TColor );
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        StartColor := xpOfficeBlue_Panel_ColorStart;
        StopColor  := xpOfficeBlue_Panel_ColorStop;
      end;

      xpcsGreen:
      begin
        StartColor := xpOfficeGreen_Panel_ColorStart;
        StopColor  := xpOfficeGreen_Panel_ColorStop;
      end;

      xpcsSilver:
      begin
        StartColor := xpOfficeSilver_Panel_ColorStart;
        StopColor  := xpOfficeSilver_Panel_ColorStop;
      end;
    end;
  end
  else
  begin
    StartColor := ActiveStyleSystemColor( clWindow );
    StopColor := DarkerColor( ActiveStyleSystemColor( clBtnFace ), 10 );
  end;

end; {= GetGradientPanelColors =}


function GetGradientPanelFrameColor( ColorStyle: TRzGradientColorStyle ): TColor;
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
        Result := xpOfficeBlue_Panel_FrameColor;

      xpcsGreen:
        Result := xpOfficeGreen_Panel_FrameColor;

      xpcsSilver:
        Result := xpOfficeSilver_Panel_FrameColor;

      else
        Result := clBtnShadow;
    end;
  end
  else
  begin
    Result := LighterColor( ActiveStyleSystemColor( clBtnShadow ), 30 );
  end;
end;


procedure GetGradientStatusBarColors( ColorStyle: TRzGradientColorStyle;
                                      var StartColor, StopColor,
                                      DividerColor: TColor );
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        StartColor := xpOfficeBlue_Panel_ColorStop;
        StopColor  := xpOfficeBlue_Panel_ColorStart;
      end;

      xpcsGreen:
      begin
        StartColor := xpOfficeGreen_Panel_ColorStop;
        StopColor  := xpOfficeGreen_Panel_ColorStart;
      end;

      xpcsSilver:
      begin
        StartColor := xpOfficeSilver_Panel_ColorStop;
        StopColor  := xpOfficeSilver_Panel_ColorStart;
      end;
    end;

    DividerColor := StartColor;
  end
  else
  begin
    StartColor := DarkerColor( ActiveStyleSystemColor( clBtnFace ), 20 );
    StopColor := ActiveStyleSystemColor( clWindow );
    DividerColor := DarkerColor( ActiveStyleSystemColor( clBtnFace ), 40 );
  end;

end; {= GetGradientStatusBarColors =}


procedure GetGradientGroupBarColors( ColorStyle: TRzGradientColorStyle;
                                     var StartColor, StopColor: TColor );
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        StartColor := xpOfficeBlue_GroupBar_ColorStart;
        StopColor  := xpOfficeBlue_GroupBar_ColorStop;
      end;

      xpcsGreen:
      begin
        StartColor := xpOfficeGreen_GroupBar_ColorStart;
        StopColor  := xpOfficeGreen_GroupBar_ColorStop;
      end;

      xpcsSilver:
      begin
        StartColor := xpOfficeSilver_GroupBar_ColorStart;
        StopColor  := xpOfficeSilver_GroupBar_ColorStop;
      end;
    end;
  end
  else
  begin
    StartColor := BlendColors( clBlack, ActiveStyleSystemColor( clBtnFace ), 30 );
    StopColor := BlendColors( clBlack, ActiveStyleSystemColor( clBtnFace ), 60 );
  end;

end; {= GetGradientGroupBarColors =}



procedure GetGradientCategoryGroupColors( ColorStyle: TRzGradientColorStyle;
                                          SpecialGroup: Boolean;
                                          var CaptionBackColor, CaptionBackColorStart,
                                          CaptionBackColorStop, CaptionFontColor,
                                          CaptionFontHotColor,
                                          CaptionButtonColor, CaptionButtonBorderColor,
                                          CaptionDividerColor, GroupColor,
                                          GroupBorderColor: TColor );
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        if not SpecialGroup then
        begin
          CaptionBackColor         := xpOfficeBlue_CategoryGroup_CaptionBackColor;
          CaptionBackColorStart    := xpOfficeBlue_CategoryGroup_CaptionBackColorStart;
          CaptionBackColorStop     := xpOfficeBlue_CategoryGroup_CaptionBackColorStop;
          CaptionFontColor         := xpOfficeBlue_CategoryGroup_CaptionFontColor;
          CaptionFontHotColor      := xpOfficeBlue_CategoryGroup_CaptionFontHotColor;
          CaptionButtonColor       := xpOfficeBlue_CategoryGroup_CaptionButtonColor;
          CaptionButtonBorderColor := xpOfficeBlue_CategoryGroup_CaptionButtonBorderColor;
          CaptionDividerColor      := xpOfficeBlue_CategoryGroup_CaptionDividerColor;
          GroupColor               := xpOfficeBlue_CategoryGroup_GroupColor;
          GroupBorderColor         := xpOfficeBlue_CategoryGroup_GroupBorderColor;
        end
        else
        begin
          CaptionBackColor         := xpOfficeBlue_CategoryGroupSpecial_CaptionBackColor;
          CaptionBackColorStart    := xpOfficeBlue_CategoryGroupSpecial_CaptionBackColorStart;
          CaptionBackColorStop     := xpOfficeBlue_CategoryGroupSpecial_CaptionBackColorStop;
          CaptionFontColor         := xpOfficeBlue_CategoryGroupSpecial_CaptionFontColor;
          CaptionFontHotColor      := xpOfficeBlue_CategoryGroupSpecial_CaptionFontHotColor;
          CaptionButtonColor       := xpOfficeBlue_CategoryGroupSpecial_CaptionButtonColor;
          CaptionButtonBorderColor := xpOfficeBlue_CategoryGroupSpecial_CaptionButtonBorderColor;
          CaptionDividerColor      := xpOfficeBlue_CategoryGroupSpecial_CaptionDividerColor;
          GroupColor               := xpOfficeBlue_CategoryGroupSpecial_GroupColor;
          GroupBorderColor         := xpOfficeBlue_CategoryGroupSpecial_GroupBorderColor;
        end;
      end;

      xpcsGreen:
      begin
        if not SpecialGroup then
        begin
          CaptionBackColor         := xpOfficeGreen_CategoryGroup_CaptionBackColor;
          CaptionBackColorStart    := xpOfficeGreen_CategoryGroup_CaptionBackColorStart;
          CaptionBackColorStop     := xpOfficeGreen_CategoryGroup_CaptionBackColorStop;
          CaptionFontColor         := xpOfficeGreen_CategoryGroup_CaptionFontColor;
          CaptionFontHotColor      := xpOfficeGreen_CategoryGroup_CaptionFontHotColor;
          CaptionButtonColor       := xpOfficeGreen_CategoryGroup_CaptionButtonColor;
          CaptionButtonBorderColor := xpOfficeGreen_CategoryGroup_CaptionButtonBorderColor;
          CaptionDividerColor      := xpOfficeGreen_CategoryGroup_CaptionDividerColor;
          GroupColor               := xpOfficeGreen_CategoryGroup_GroupColor;
          GroupBorderColor         := xpOfficeGreen_CategoryGroup_GroupBorderColor;
        end
        else
        begin
          CaptionBackColor         := xpOfficeGreen_CategoryGroupSpecial_CaptionBackColor;
          CaptionBackColorStart    := xpOfficeGreen_CategoryGroupSpecial_CaptionBackColorStart;
          CaptionBackColorStop     := xpOfficeGreen_CategoryGroupSpecial_CaptionBackColorStop;
          CaptionFontColor         := xpOfficeGreen_CategoryGroupSpecial_CaptionFontColor;
          CaptionFontHotColor      := xpOfficeGreen_CategoryGroupSpecial_CaptionFontHotColor;
          CaptionButtonColor       := xpOfficeGreen_CategoryGroupSpecial_CaptionButtonColor;
          CaptionButtonBorderColor := xpOfficeGreen_CategoryGroupSpecial_CaptionButtonBorderColor;
          CaptionDividerColor      := xpOfficeGreen_CategoryGroupSpecial_CaptionDividerColor;
          GroupColor               := xpOfficeGreen_CategoryGroupSpecial_GroupColor;
          GroupBorderColor         := xpOfficeGreen_CategoryGroupSpecial_GroupBorderColor;
        end;
      end;

      xpcsSilver:
      begin
        if not SpecialGroup then
        begin
          CaptionBackColor         := xpOfficeSilver_CategoryGroup_CaptionBackColor;
          CaptionBackColorStart    := xpOfficeSilver_CategoryGroup_CaptionBackColorStart;
          CaptionBackColorStop     := xpOfficeSilver_CategoryGroup_CaptionBackColorStop;
          CaptionFontColor         := xpOfficeSilver_CategoryGroup_CaptionFontColor;
          CaptionFontHotColor      := xpOfficeSilver_CategoryGroup_CaptionFontHotColor;
          CaptionButtonColor       := xpOfficeSilver_CategoryGroup_CaptionButtonColor;
          CaptionButtonBorderColor := xpOfficeSilver_CategoryGroup_CaptionButtonBorderColor;
          CaptionDividerColor      := xpOfficeSilver_CategoryGroup_CaptionDividerColor;
          GroupColor               := xpOfficeSilver_CategoryGroup_GroupColor;
          GroupBorderColor         := xpOfficeSilver_CategoryGroup_GroupBorderColor;
        end
        else
        begin
          CaptionBackColor         := xpOfficeSilver_CategoryGroupSpecial_CaptionBackColor;
          CaptionBackColorStart    := xpOfficeSilver_CategoryGroupSpecial_CaptionBackColorStart;
          CaptionBackColorStop     := xpOfficeSilver_CategoryGroupSpecial_CaptionBackColorStop;
          CaptionFontColor         := xpOfficeSilver_CategoryGroupSpecial_CaptionFontColor;
          CaptionFontHotColor      := xpOfficeSilver_CategoryGroupSpecial_CaptionFontHotColor;
          CaptionButtonColor       := xpOfficeSilver_CategoryGroupSpecial_CaptionButtonColor;
          CaptionButtonBorderColor := xpOfficeSilver_CategoryGroupSpecial_CaptionButtonBorderColor;
          CaptionDividerColor      := xpOfficeSilver_CategoryGroupSpecial_CaptionDividerColor;
          GroupColor               := xpOfficeSilver_CategoryGroupSpecial_GroupColor;
          GroupBorderColor         := xpOfficeSilver_CategoryGroupSpecial_GroupBorderColor;
        end;
      end;
    end; { case CurrentXPColorScheme }

  end
  else
  begin
    if not SpecialGroup then
    begin
      CaptionBackColor         := clBtnFace;
      CaptionBackColorStart    := clWindow;
      CaptionBackColorStop     := DarkerColor( clBtnFace, 10 );
      if CurrentXPColorScheme <> xpcsSilver then
      begin
        CaptionFontColor    := DarkerColor( clHighlight, 30 );
        CaptionFontHotColor := clHighlight;
        (*
        CaptionFontColor       := clHighlight;
        CaptionFontHotColor    := LighterColor( clHighlight, 30 );
        *)
      end
      else
      begin
        CaptionFontColor       := clBlack;
        CaptionFontHotColor    := $00555556;
      end;
      CaptionButtonColor       := clWindow;
      CaptionButtonBorderColor := DarkerColor( clBtnFace, 20 );
      CaptionDividerColor      := clHighlight;
      GroupColor               := clBtnFace;
      GroupBorderColor         := clWhite;
    end
    else // Special Group
    begin
      CaptionBackColor         := clHighlight;
      CaptionBackColorStart    := clHighlight;
      CaptionBackColorStop     := DarkerColor( clHighlight, 20 );
      CaptionFontColor         := clHighlightText;
      CaptionFontHotColor      := LighterColor( clHighlight, 30 );
      CaptionButtonColor       := clHighlight;
      CaptionButtonBorderColor := LighterColor( clHighlight, 30 );
      CaptionDividerColor      := clHighlight;
      GroupColor               := clBtnFace;
      GroupBorderColor         := clWhite;
    end;

  end;
end; {= GetGradientCategoryGroupColors =}


procedure GetGradientOutlookGroupColors( ColorStyle: TRzGradientColorStyle;
                                         var CaptionBackStartColor,
                                         CaptionBackStopColor, CaptionFontColor,
                                         CaptionFontHotColor,
                                         GroupStartColor, GroupStopColor: TColor );
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        CaptionFontColor    := xpOfficeBlue_CategoryGroup_CaptionFontColor;
        CaptionFontHotColor := xpOfficeBlue_CategoryGroup_CaptionFontHotColor;
      end;

      xpcsGreen:
      begin
        CaptionFontColor    := xpOfficeGreen_CategoryGroup_CaptionFontColor;
        CaptionFontHotColor := xpOfficeGreen_CategoryGroup_CaptionFontHotColor;
      end;

      xpcsSilver:
      begin
        CaptionFontColor    := xpOfficeSilver_CategoryGroup_CaptionFontColor;
        CaptionFontHotColor := xpOfficeSilver_CategoryGroup_CaptionFontHotColor;
      end;
    end; { case CurrentXPColorScheme }

  end
  else
  begin
    if CurrentXPColorScheme <> xpcsSilver then
    begin
      CaptionFontColor    := DarkerColor( clHighlight, 30 );
      CaptionFontHotColor := clHighlight;
    end
    else
    begin
      CaptionFontColor    := clBlack;
      CaptionFontHotColor := $00555556;
    end;
  end;

  // Get CaptionBackColorStart and CaptionBackColorStop from GetGradientPanelColors
  GetGradientPanelColors( ColorStyle, CaptionBackStartColor, CaptionBackStopColor );
  GroupStartColor := CaptionBackStartColor;
  GroupStopColor := CaptionBackStopColor;

end; {= GetGradientOutlookGroupColors =}



procedure GetGradientGroupItemColors( ColorStyle: TRzGradientColorStyle;
                                      var ItemFontColor, ItemFontHotColor: TColor );
begin
  if ( ColorStyle = gcsMSOffice ) and ActiveStyleServicesEnabled then
  begin
    // Determine the current XP color scheme and set colors appropriately

    case CurrentXPColorScheme of
      xpcsBlue:
      begin
        ItemFontColor     := xpOfficeBlue_CategoryGroup_ItemFontColor;
        ItemFontHotColor  := xpOfficeBlue_CategoryGroup_ItemFontHotColor;
      end;

      xpcsGreen:
      begin
        ItemFontColor     := xpOfficeGreen_CategoryGroup_ItemFontColor;
        ItemFontHotColor  := xpOfficeGreen_CategoryGroup_ItemFontHotColor;
      end;

      xpcsSilver:
      begin
        ItemFontColor     := xpOfficeSilver_CategoryGroup_ItemFontColor;
        ItemFontHotColor  := xpOfficeSilver_CategoryGroup_ItemFontHotColor;
      end;
    end; { case CurrentXPColorScheme }

  end
  else
  begin
    if CurrentXPColorScheme <> xpcsSilver then
    begin
      ItemFontColor := DarkerColor( clHighlight, 30 );
      ItemFontHotColor := clHighlight;
    end
    else
    begin
      ItemFontColor     := xpOfficeSilver_CategoryGroup_ItemFontColor;
      ItemFontHotColor  := xpOfficeSilver_CategoryGroup_ItemFontHotColor;
    end;
  end;
end; {= GetGradientGroupItemColors =}



{==================================}
{== VCL Styles Support Functions ==}
{==================================}


{$IFDEF VCL160_OR_HIGHER}

function ActiveStyleServices: TCustomStyleServices;
begin
  Result := StyleServices;
end;


function ActiveStyleServicesEnabled: Boolean;
begin
  Result := StyleServices.Enabled;
end;


function ActiveStyleServicesAvailable: Boolean;
begin
  Result := StyleServices.Available;
end;


procedure ActiveStyleServicesDrawText( DC: HDC; Details: TThemedElementDetails; const S: string;
                                       R: TRect; Flags: Cardinal );
begin
  StyleServices.DrawText( DC, Details, S, R, TTextFormatFlags( Flags ) );
end;

{$ELSE}

function ActiveStyleServices: TThemeServices;
begin
  Result := ThemeServices;
end;


function ActiveStyleServicesEnabled: Boolean;
begin
  Result := ThemeServices.ThemesEnabled;
end;


function ActiveStyleServicesAvailable: Boolean;
begin
  Result := ThemeServices.ThemesAvailable;
end;


procedure ActiveStyleServicesDrawText( DC: HDC; Details: TThemedElementDetails; const S: string;
                                       R: TRect; Flags: Cardinal );
begin
  ThemeServices.DrawText( DC, Details, S, R, Flags, 0 );
end;

{$ENDIF}


function UsingSystemStyle: Boolean;
begin
  {$IFDEF VCL160_OR_HIGHER}
  // StyleServices.IsSystemStyle will return False if a VCL Style is used
  Result := StyleServices.IsSystemStyle;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;


function ActiveStyleSystemColor( C: TColor ): TColor;
begin
  Result := C;
  if not UsingSystemStyle then
  begin
    // VCL Styles
    {$IFDEF VCL160_OR_HIGHER}
    Result := StyleServices.GetSystemColor( C );
    {$ENDIF}
  end;
end;


function ActiveStyleColor( SC: TStyleColor ): TColor;
begin
  Result := clNone;
  if not UsingSystemStyle then
  begin
    {$IFDEF VCL160_OR_HIGHER}
    Result := StyleServices.GetStyleColor( SC );
    {$ENDIF}
  end;
end;


function ActiveStyleFontColor( SF: TStyleFont ): TColor;
begin
  Result := clNone;
  if not UsingSystemStyle then
  begin
    {$IFDEF VCL160_OR_HIGHER}
    Result := StyleServices.GetStyleFontColor( SF );
    {$ENDIF}
  end;
end;


function IsFullColorSupported: Boolean;
var
  DC: HDC;
begin
  DC := GetDC( 0 );
  try
    Result := GetDeviceCaps( DC, NUMCOLORS ) = -1;
  finally
    ReleaseDC( 0, DC );
  end;
end;


procedure DrawDropShadow( Canvas: TCanvas; Bounds: TRect; Depth: Integer; ShadowColor: TColor = clBlack );
var
  A, D, I: Integer;

  procedure DrawShadow( Offset, Alpha: Integer );
  var
    X, Y: Integer;
  begin
    //                       4 ***
    //                           *
    //                           *
    //                         3 *
    //     *                     *
    //   1 *          2          *
    //     ***********************

    // Step 1
    X := Bounds.Left + 2*Depth - Offset;
    for Y := Bounds.Bottom - 1 to Bounds.Bottom - 1 + Offset - 1 do
      Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );
    Inc( X );
    Y := Bounds.Bottom - 1 + Offset - 1;
    Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );

    // Step 2
    Y := Bounds.Bottom - 1 + Offset;
    for X := Bounds.Left + 2*Depth - Offset + 1 to Bounds.Right + Offset - 2 do
      Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );
    Dec( Y );
    X := Bounds.Right + Offset - 2;
    Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );

    // Step 3
    Y := Bounds.Top + 2*Depth - Offset;
    for X := Bounds.Right - 1 to Bounds.Right - 1 + Offset - 1 do
      Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );
    Inc( Y );
    X := Bounds.Right - 1 + Offset - 1;
    Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );

    // Step 4
    X := Bounds.Right - 1 + Offset;
    for Y := Bounds.Top + 2 * Depth - Offset + 1 to Bounds.Bottom + Offset - 2 do
      Canvas.Pixels[ X, Y ] := BlendColors( ShadowColor, Canvas.Pixels[ X, Y ], Alpha );
  end;

begin
  if Depth <= 0 then
    Exit;
  D := 128 div Depth;
  A := 128;
  for I := 1 to Depth do
  begin
    DrawShadow( I, A );
    Dec( A, D );
  end;
end;


{======================}
{== Border Functions ==}
{======================}

function DrawSides( Canvas: TCanvas; Bounds: TRect; ULColor, LRColor: TColor; Sides: TSides ): TRect;
begin
  if ULColor <> clNone then
  begin
    Canvas.Pen.Color := ULColor;
    if sdLeft in Sides then
    begin
      Canvas.MoveTo( Bounds.Left, Bounds.Top );
      Canvas.LineTo( Bounds.Left, Bounds.Bottom );
    end;

    if sdTop in Sides then
    begin
      Canvas.MoveTo( Bounds.Left, Bounds.Top );
      Canvas.LineTo( Bounds.Right, Bounds.Top );
    end;
  end;

  if LRColor <> clNone then
  begin
    Canvas.Pen.Color := LRColor;
    if sdRight in Sides then
    begin
      Canvas.MoveTo( Bounds.Right - 1, Bounds.Top );
      Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom );
    end;

    if sdBottom in Sides then
    begin
      Canvas.MoveTo( Bounds.Left, Bounds.Bottom - 1 );
      Canvas.LineTo( Bounds.Right, Bounds.Bottom - 1 );
    end;
  end;

  if sdLeft in Sides then
    Inc( Bounds.Left );
  if sdTop in Sides then
    Inc( Bounds.Top );
  if sdRight in Sides then
    Dec( Bounds.Right );
  if sdBottom in Sides then
    Dec( Bounds.Bottom );

  Result := Bounds;
end; {= DrawSides =}



function DrawBevel( Canvas: TCanvas; Bounds: TRect; ULColor, LRColor: TColor; Width: Integer; Sides: TSides ): TRect;
var
  I: Integer;
begin
  Canvas.Pen.Width := 1;
  for I := 1 to Width do                         { Loop through width of bevel }
  begin
    Bounds := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides );
  end;
  Result := Bounds;
end;


{=======================================}
{== Generic DrawCtl3DBorder Procedure ==}
{=======================================}

function DrawCtl3DBorder( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean ): TRect;
begin
  Result := DrawCtl3DBorderSides( Canvas, Bounds, Lowered, sdAllSides );
end;


function DrawCtl3DBorderSides( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean; Sides: TSides ): TRect;
const
  Colors: array[ 1..4, Boolean ] of TColor = ( ( cl3DLight, clBtnShadow ),
                                                ( cl3DDkShadow, clBtnHighlight ),
                                                ( clBtnHighlight, cl3DDkShadow ),
                                                ( clBtnShadow, cl3DLight ) );
begin
  Bounds := DrawBevel( Canvas, Bounds,
                       ActiveStyleSystemColor( Colors[ 1, Lowered ] ),
                       ActiveStyleSystemColor( Colors[ 2, Lowered ] ), 1, Sides );

  Result := DrawBevel( Canvas, Bounds,
                       ActiveStyleSystemColor( Colors[ 3, Lowered ] ),
                       ActiveStyleSystemColor( Colors[ 4, Lowered ] ), 1, Sides );
end;


{========================================}
{== Generic DrawButtonBorder Procedure ==}
{========================================}

function DrawButtonBorder( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean ): TRect;
begin
  Result := DrawButtonBorderSides( Canvas, Bounds, Lowered, sdAllSides );
end;


function DrawButtonBorderSides( Canvas: TCanvas; Bounds: TRect; Lowered: Boolean; Sides: TSides ): TRect;
const
  Colors: array[ 1..4, Boolean ] of TColor = ( ( clBtnHighlight, clBtnText ),
                                                ( cl3DDkShadow, clBtnText ),
                                                ( cl3DLight, clBtnShadow ),
                                                ( clBtnShadow, clBtnShadow ) );
  StyleColors: array[ 1..4, Boolean ] of TColor = ( ( clBtnHighlight, cl3DDkShadow ),
                                                    ( cl3DDkShadow, cl3DDkShadow ),
                                                    ( cl3DLight, clBtnShadow ),
                                                    ( clBtnShadow, clBtnShadow ) );
begin
  if UsingSystemStyle then
  begin
    Bounds := DrawBevel( Canvas, Bounds, Colors[ 1, Lowered ], Colors[ 2, Lowered ], 1, Sides );
    Result := DrawBevel( Canvas, Bounds, Colors[ 3, Lowered ], Colors[ 4, Lowered ], 1, Sides );
  end
  else // VCL Styles
  begin
    Bounds := DrawBevel( Canvas, Bounds,
                         ActiveStyleSystemColor( StyleColors[ 1, Lowered ] ),
                         ActiveStyleSystemColor( StyleColors[ 2, Lowered ] ), 1, Sides );

    Result := DrawBevel( Canvas, Bounds,
                         ActiveStyleSystemColor( StyleColors[ 3, Lowered ] ),
                         ActiveStyleSystemColor( StyleColors[ 4, Lowered ] ), 1, Sides );
  end;
end;



function DrawColorButtonBorder( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Lowered: Boolean ): TRect;
begin
  Result := DrawColorButtonBorderSides( Canvas, Bounds, FaceColor, Lowered, sdAllSides );
end;


function DrawColorButtonBorderSides( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Lowered: Boolean; Sides: TSides ): TRect;
var
  ULColor, LRColor: TColor;
begin
  if Lowered then
  begin
    ULColor := DarkerColor( FaceColor, 100 );
    LRColor := ULColor;
  end
  else
  begin
    ULColor := LighterColor( FaceColor, 100 );
    LRColor := DarkerColor( FaceColor, 100 );
  end;

  Bounds := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides );

  if Lowered then
  begin
    ULColor := DarkerColor( FaceColor, 50 );
    LRColor := ULColor;
  end
  else
  begin
    ULColor := LighterColor( FaceColor, 40 );
    LRColor := DarkerColor( FaceColor, 50 );
  end;

  Result := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides );
end;



{==================================}
{== Generic DrawBorder Procedure ==}
{==================================}

function DrawBorder( Canvas: TCanvas; Bounds: TRect; Style: TFrameStyle ): TRect;
begin
  Result := DrawBorderSides( Canvas, Bounds, Style, sdAllSides );
end;


function DrawBorderSides( Canvas: TCanvas; Bounds: TRect; Style: TFrameStyle; Sides: TSides ): TRect;
var
  ULColor, LRColor: TColor;
  R: TRect;
begin
  ULColor := ActiveStyleSystemColor( ULFrameColor[ Style ] );
  LRColor := ActiveStyleSystemColor( LRFrameColor[ Style ] );

  { Draw the Frame }
  if Style <> fsNone then
  begin
    if Style in [ fsFlat, fsStatus, fsPopup ] then
      Bounds := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides )
    else if Style in [ fsFlatBold ] then
      Bounds := DrawBevel( Canvas, Bounds, ULColor, LRColor, 2, Sides )
    else if Style in [ fsLowered, fsRaised ] then
      Bounds := DrawCtl3DBorderSides( Canvas, Bounds, Style = fsLowered, Sides )
    else if Style in [ fsButtonDown, fsButtonUp ] then
      Bounds := DrawButtonBorderSides( Canvas, Bounds, Style = fsButtonDown, Sides )
    else
    begin
      { Style must be fsGroove or fsBump }
      R := Bounds;
      { Fill in the gaps created by offsetting the rectangle }
      { Upper Right Gap }
      if sdRight in Sides then
        Canvas.Pixels[ R.Right - 1, R.Top ] := LRColor;
      if ( sdTop in Sides ) and not ( sdRight in Sides ) then
        Canvas.Pixels[ R.Right - 1, R.Top ] := ULColor;

      { Lower Left Gap }
      if sdBottom in Sides then
        Canvas.Pixels[ R.Left, R.Bottom - 1 ] := LRColor;
      if ( sdLeft in Sides ) and not ( sdBottom in Sides ) then
        Canvas.Pixels[ R.Left, R.Bottom - 1 ] := ULColor;

      { Upper Left Gaps }
      if ( sdTop in Sides ) and not ( sdLeft in Sides ) then
        Canvas.Pixels[ R.Left, R.Top + 1 ] := LRColor;
      if not ( sdTop in Sides ) and ( sdLeft in Sides ) then
        Canvas.Pixels[ R.Left + 1, R.Top ] := LRColor;

      { Lower Right Gaps }
      if ( sdBottom in Sides ) and not ( sdRight in Sides ) then
        Canvas.Pixels[ R.Right - 1, R.Bottom - 2 ] := ULColor;
      if not ( sdBottom in Sides ) and ( sdRight in Sides ) then
        Canvas.Pixels[ R.Right - 2, R.Bottom - 1 ] := ULColor;

      Inc( R.Left );
      Inc( R.Top );
      DrawSides( Canvas, R, LRColor, LRColor, Sides );
      OffsetRect( R, -1, -1 );
      DrawSides( Canvas, R, ULColor, ULColor, Sides );
      if sdLeft in Sides then
        Inc( Bounds.Left, 2 );
      if sdTop in Sides then
        Inc( Bounds.Top, 2 );
      if sdRight in Sides then
        Dec( Bounds.Right, 2 );
      if sdBottom in Sides then
        Dec( Bounds.Bottom, 2 );
    end;
  end;
  Result := Bounds;
end; {= DrawBorderSides =}




function DrawColorBorder( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Style: TFrameStyle ): TRect;
begin
  Result := DrawColorBorderSides( Canvas, Bounds, FaceColor, Style, sdAllSides );
end;


function DrawColorBorderSides( Canvas: TCanvas; Bounds: TRect; FaceColor: TColor; Style: TFrameStyle; Sides: TSides ): TRect;
var
  ULColor, LRColor, C1, C2, C3, C4: TColor;
  R: TRect;
begin
  if Style <> fsNone then
  begin
    if Style in [ fsFlat, fsStatus, fsPopup ] then
    begin
      case Style of
        fsStatus:
        begin
          ULColor := DarkerColor( FaceColor, 50 );
          LRColor := LighterColor( FaceColor, 100 );
        end;

        fsPopup:
        begin
          ULColor := LighterColor( FaceColor, 100 );
          LRColor := DarkerColor( FaceColor, 50 );
        end;

        else { Style = fsFlat }
        begin
          ULColor := DarkerColor( FaceColor, 50 );
          LRColor := ULColor;
        end;
      end;
      Bounds := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides )
    end
    else if Style in [ fsFlatBold ] then
    begin
      ULColor := DarkerColor( FaceColor, 50 );
      LRColor := ULColor;
      Bounds := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides );
      Bounds := DrawSides( Canvas, Bounds, ULColor, LRColor, Sides );
    end
    else if Style in [ fsLowered, fsRaised ] then
    begin
      C1 := DarkerColor( FaceColor, 50 );             // Gray
      C2 := DarkerColor( FaceColor, 100 );            // Black
      C3 := LighterColor( FaceColor, 50 );            // Silver
      C4 := LighterColor( FaceColor, 100 );           // White
      if Style = fsLowered then
      begin
        Bounds := DrawSides( Canvas, Bounds, C1, C4, Sides );
        Bounds := DrawSides( Canvas, Bounds, C2, C3, Sides );
      end
      else
      begin
        Bounds := DrawSides( Canvas, Bounds, C3, C2, Sides );
        Bounds := DrawSides( Canvas, Bounds, C4, C1, Sides );
      end;
    end
    else if Style in [ fsButtonDown, fsButtonUp ] then
    begin
      Bounds := DrawColorButtonBorderSides( Canvas, Bounds, FaceColor, ( Style = fsButtonDown ), Sides );
    end
    else
    begin
      { Style must be fsGroove or fsBump }
      if Style = fsGroove then
      begin
        ULColor := DarkerColor( FaceColor, 50 );
        LRColor := LighterColor( FaceColor, 100 );
      end
      else
      begin
        ULColor := LighterColor( FaceColor, 100 );
        LRColor := DarkerColor( FaceColor, 50 );
      end;
      R := Bounds;
      { Fill in the gaps created by offsetting the rectangle }
      { Upper Right Gap }
      if sdRight in Sides then
        Canvas.Pixels[ R.Right - 1, R.Top ] := LRColor;
      if ( sdTop in Sides ) and not ( sdRight in Sides ) then
        Canvas.Pixels[ R.Right - 1, R.Top ] := ULColor;

      { Lower Left Gap }
      if sdBottom in Sides then
        Canvas.Pixels[ R.Left, R.Bottom - 1 ] := LRColor;
      if ( sdLeft in Sides ) and not ( sdBottom in Sides ) then
        Canvas.Pixels[ R.Left, R.Bottom - 1 ] := ULColor;

      { Upper Left Gaps }
      if ( sdTop in Sides ) and not ( sdLeft in Sides ) then
        Canvas.Pixels[ R.Left, R.Top + 1 ] := LRColor;
      if not ( sdTop in Sides ) and ( sdLeft in Sides ) then
        Canvas.Pixels[ R.Left + 1, R.Top ] := LRColor;

      { Lower Right Gaps }
      if ( sdBottom in Sides ) and not ( sdRight in Sides ) then
        Canvas.Pixels[ R.Right - 1, R.Bottom - 2 ] := ULColor;
      if not ( sdBottom in Sides ) and ( sdRight in Sides ) then
        Canvas.Pixels[ R.Right - 2, R.Bottom - 1 ] := ULColor;

      Inc( R.Left );
      Inc( R.Top );
      DrawSides( Canvas, R, LRColor, LRColor, Sides );
      OffsetRect( R, -1, -1 );
      DrawSides( Canvas, R, ULColor, ULColor, Sides );
      if sdLeft in Sides then
        Inc( Bounds.Left, 2 );
      if sdTop in Sides then
        Inc( Bounds.Top, 2 );
      if sdRight in Sides then
        Dec( Bounds.Right, 2 );
      if sdBottom in Sides then
        Dec( Bounds.Bottom, 2 );
    end;
  end;
  Result := Bounds;
end; {= DrawColorBorderSides =}


function DrawBox( Canvas: TCanvas; Bounds: TRect; LineColor: TColor ): TRect;
begin
  Canvas.Pen.Color := LineColor;

  // Left
  Canvas.MoveTo( Bounds.Left, Bounds.Top );
  Canvas.LineTo( Bounds.Left, Bounds.Bottom );
  // Top
  Canvas.MoveTo( Bounds.Left, Bounds.Top );
  Canvas.LineTo( Bounds.Right, Bounds.Top );
  // Right
  Canvas.MoveTo( Bounds.Right - 1, Bounds.Top );
  Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom );
  // Bottom
  Canvas.MoveTo( Bounds.Left, Bounds.Bottom - 1 );
  Canvas.LineTo( Bounds.Right, Bounds.Bottom - 1 );

  Result := Bounds;
  InflateRect( Result, -1, -1 );
end;


function DrawFilledBox( Canvas: TCanvas; Bounds: TRect; LineColor, FillColor: TColor ): TRect;
begin
  Result := Bounds;
  Canvas.Pen.Color := LineColor;
  Canvas.Brush.Color := FillColor;

  // Left
  Canvas.MoveTo( Bounds.Left, Bounds.Top );
  Canvas.LineTo( Bounds.Left, Bounds.Bottom );
  // Top
  Canvas.MoveTo( Bounds.Left, Bounds.Top );
  Canvas.LineTo( Bounds.Right, Bounds.Top );
  // Right
  Canvas.MoveTo( Bounds.Right - 1, Bounds.Top );
  Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom );
  // Bottom
  Canvas.MoveTo( Bounds.Left, Bounds.Bottom - 1 );
  Canvas.LineTo( Bounds.Right, Bounds.Bottom - 1 );

  InflateRect( Result, -1, -1 );
  Canvas.FillRect( Result );
end;


function DrawBoxCorners( Canvas: TCanvas; Bounds: TRect; LineColor: TColor;
                         CornerWidth: Integer ): TRect;
begin
  Canvas.Pen.Color := LineColor;
  Result := Bounds;

  // Decrease right and bottom b/c MoveTo/LineTo goes beyond edge of bounds
  Dec( Bounds.Right );
  Dec( Bounds.Bottom );

  // Upper Left
  Canvas.MoveTo( Bounds.Left, Bounds.Top + CornerWidth - 1 );
  Canvas.LineTo( Bounds.Left, Bounds.Top );
  Canvas.LineTo( Bounds.Left + CornerWidth, Bounds.Top );

  // Upper Right
  Canvas.MoveTo( Bounds.Right - CornerWidth + 1, Bounds.Top );
  Canvas.LineTo( Bounds.Right, Bounds.Top );
  Canvas.LineTo( Bounds.Right, Bounds.Top + CornerWidth );

  // Lower Right
  Canvas.MoveTo( Bounds.Right, Bounds.Bottom - CornerWidth + 1 );
  Canvas.LineTo( Bounds.Right, Bounds.Bottom );
  Canvas.LineTo( Bounds.Right - CornerWidth, Bounds.Bottom );

  // Lower Left
  Canvas.MoveTo( Bounds.Left + CornerWidth - 1, Bounds.Bottom );
  Canvas.LineTo( Bounds.Left, Bounds.Bottom );
  Canvas.LineTo( Bounds.Left, Bounds.Bottom - CornerWidth );

  InflateRect( Result, -1, -1 );
end; {= DrawBoxCorners =}


function DrawDashedBorder( Canvas: TCanvas; Bounds: TRect; DashColor: TColor ): TRect;
begin
  Canvas.Pen.Style := psDot;
  Canvas.Pen.Color := DashColor;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle( Bounds );

  Result := Bounds;
  InflateRect( Result, -1, -1 );
  Canvas.Brush.Style := bsSolid;
end;


function DrawFocusBorder( Canvas: TCanvas; Bounds: TRect; Color: TColor = clNone ): TRect;

  procedure DrawHorzLine( X1, X2, Y: Integer );
  var
    X: Integer;
    C: TColor;
  begin
    X := X1 + 1;
    Canvas.MoveTo( X, Y );
    if Color = clNone then
    begin
      while X < X2 do
      begin
        C := Canvas.Pixels[ X, Y ];
        Canvas.Pixels[ X, Y ] := C xor C;
        Inc( X, 2 );
      end;
    end
    else
    begin
      while X < X2 do
      begin
        Canvas.Pixels[ X, Y ] := Color;
        Inc( X, 2 );
      end;
    end;
  end;

  procedure DrawVertLine( X, Y1, Y2: Integer );
  var
    Y: Integer;
    C: TColor;
  begin
    Y := Y1 + 1;
    Canvas.MoveTo( X, Y );
    if Color = clNone then
    begin
      while Y < Y2 do
      begin
        C := Canvas.Pixels[ X, Y ];
        Canvas.Pixels[ X, Y ] := C xor C;
        Inc( Y, 2 );
      end;
    end
    else
    begin
      while Y < Y2 do
      begin
        Canvas.Pixels[ X, Y ] := Color;
        Inc( Y, 2 );
      end;
    end;
  end;

begin
  DrawVertLine( Bounds.Left, Bounds.Top, Bounds.Bottom );
  DrawVertLine( Bounds.Right - 1, Bounds.Top, Bounds.Bottom );
  DrawHorzLine( Bounds.Left, Bounds.Right, Bounds.Top );
  DrawHorzLine( Bounds.Left, Bounds.Right, Bounds.Bottom - 1 );

  Result := Bounds;
  InflateRect( Result, -1, -1 );
end; {= DrawFocusBorder =}


function DrawRoundedFlatBorder( Canvas: TCanvas; Bounds: TRect; Color: TColor; Sides: TSides ): TRect;
var
  X1, X2, Y1, Y2: Integer;
begin
  Canvas.Pen.Color := Color;

  if sdLeft in Sides then
  begin
    if sdTop in Sides then
      Y1 := 2
    else
      Y1 := 0;
    if sdBottom in Sides then
      Y2 := 2
    else
      Y2 := 0;
    Canvas.MoveTo( Bounds.Left, Bounds.Top + Y1 );
    Canvas.LineTo( Bounds.Left, Bounds.Bottom - Y2 );
  end;

  if sdTop in Sides then
  begin
    if sdLeft in Sides then
      X1 := 2
    else
      X1 := 0;
    if sdRight in Sides then
      X2 := 2
    else
      X2 := 0;
    Canvas.MoveTo( Bounds.Left + X1, Bounds.Top );
    Canvas.LineTo( Bounds.Right - X2, Bounds.Top );
  end;

  if sdRight in Sides then
  begin
    if sdTop in Sides then
      Y1 := 2
    else
      Y1 := 0;
    if sdBottom in Sides then
      Y2 := 2
    else
      Y2 := 0;
    Canvas.MoveTo( Bounds.Right - 1, Bounds.Top + Y1 );
    Canvas.LineTo( Bounds.Right - 1, Bounds.Bottom - Y2 );
  end;

  if sdBottom in Sides then
  begin
    if sdLeft in Sides then
      X1 := 2
    else
      X1 := 0;
    if sdRight in Sides then
      X2 := 2
    else
      X2 := 0;
    Canvas.MoveTo( Bounds.Left + X1, Bounds.Bottom - 1 );
    Canvas.LineTo( Bounds.Right - X2, Bounds.Bottom - 1 );
  end;

  if ( sdLeft in Sides ) and ( sdTop in Sides ) then
    Canvas.Pixels[ Bounds.Left + 1, Bounds.Top + 1 ] := Color;
  if ( sdTop in Sides ) and ( sdRight in Sides ) then
    Canvas.Pixels[ Bounds.Right - 2, Bounds.Top + 1 ] := Color;
  if ( sdRight in Sides ) and ( sdBottom in Sides ) then
    Canvas.Pixels[ Bounds.Right - 2, Bounds.Bottom - 2 ] := Color;
  if ( sdLeft in Sides ) and ( sdBottom in Sides ) then
    Canvas.Pixels[ Bounds.Left + 1, Bounds.Bottom - 2 ] := Color;


  if sdLeft in Sides then
    Inc( Bounds.Left, 2 );
  if sdTop in Sides then
    Inc( Bounds.Top, 2 );
  if sdRight in Sides then
    Dec( Bounds.Right, 2 );
  if sdBottom in Sides then
    Dec( Bounds.Bottom, 2 );

  Result := Bounds;
end; {= DrawRoundedFlatBorder =}


function DrawInnerOuterBorders( Canvas: TCanvas; Bounds: TRect;
                                BorderOuter, BorderInner: TFrameStyleEx;
                                BorderWidth: Integer; BorderSides: TSides; BevelWidth: Integer;
                                BorderColor, BorderHighlight, BorderShadow: TColor;
                                FlatColor: TColor; FlatColorAdjustment: Integer; Color, ParentColor: TColor;
                                Transparent: Boolean; SoftInnerFlatBorder: Boolean = False ): TRect;
var
  TempR: TRect;
  C: TColor;
begin
  Result := Bounds;

  { Outer Border }
  if BorderOuter in [ fsFlat, fsFlatBold, fsFlatRounded ] then
  begin
    C := AdjustColor( FlatColor, FlatColorAdjustment );
    if BorderOuter = fsFlat then
      Result := DrawBevel( Canvas, Result, C, C, 1, BorderSides )
    else if BorderOuter = fsFlatBold then
      Result := DrawBevel( Canvas, Result, C, C, 2, BorderSides )
    else
    begin
      if not Transparent then
      begin
        TempR := DrawBevel( Canvas, Result, ParentColor, ParentColor, 1, BorderSides );
        if ( BorderWidth > 0 ) or ( BorderInner <> fsNone ) then
          DrawBevel( Canvas, TempR, BorderColor, BorderColor, 1, BorderSides )
        else
          DrawBevel( Canvas, TempR, Color, Color, 1, BorderSides );
      end
      else // Transparent
      begin
        if ( BorderWidth > 0 ) or ( BorderInner <> fsNone ) then
        begin
          TempR := Result;
          InflateRect( TempR, -1, -1 );
          DrawBevel( Canvas, TempR, BorderColor, BorderColor, 1, BorderSides );
        end;
      end;
      Result := DrawRoundedFlatBorder( Canvas, Result, C, BorderSides );
    end;
  end
  else if BorderOuter = fsPopup then
    Result := DrawBevel( Canvas, Result, BorderHighlight, BorderShadow, BevelWidth, BorderSides )
  else if BorderOuter = fsStatus then
    Result := DrawBevel( Canvas, Result, BorderShadow, BorderHighlight, BevelWidth, BorderSides )
  else
    Result := DrawBorderSides( Canvas, Result, BorderOuter, BorderSides );

  { Space between borders }
  if BorderWidth > 0 then
    Result := DrawBevel( Canvas, Result, BorderColor, BorderColor, BorderWidth, BorderSides );

  { Inner Border }
  if BorderInner in [ fsFlat, fsFlatBold, fsFlatRounded ] then
  begin
    C := AdjustColor( FlatColor, FlatColorAdjustment );
    if BorderInner = fsFlat then
    begin
      if not SoftInnerFlatBorder then
        Result := DrawBevel( Canvas, Result, C, C, 1, BorderSides )
      else
      begin
        Canvas.Pen.Color := C;
        // Left side
        Canvas.MoveTo( Result.Left, Result.Top + 1 );
        Canvas.LineTo( Result.Left, Result.Bottom - 1 );
        // Top side
        Canvas.MoveTo( Result.Left + 1, Result.Top );
        Canvas.LineTo( Result.Right - 1, Result.Top );
        // Right side
        Canvas.MoveTo( Result.Right - 1, Result.Top + 1 );
        Canvas.LineTo( Result.Right - 1, Result.Bottom - 1 );
        // Bottom side
        Canvas.MoveTo( Result.Left + 1, Result.Bottom - 1 );
        Canvas.LineTo( Result.Right - 1, Result.Bottom - 1 );

        InflateRect( Result, -1, -1 );
      end;
    end
    else if BorderInner = fsFlatBold then
      Result := DrawBevel( Canvas, Result, C, C, 2, BorderSides )
    else
    begin
      if not Transparent then
      begin
        TempR := DrawBevel( Canvas, Result, BorderColor, BorderColor, 1, BorderSides );
        DrawBevel( Canvas, TempR, Color, Color, 1, BorderSides );
      end
      else // Transparent
        DrawBevel( Canvas, Result, BorderColor, BorderColor, 1, BorderSides );
      Result := DrawRoundedFlatBorder( Canvas, Result, C, BorderSides );
    end;
  end
  else if BorderInner = fsPopup then
    Result := DrawBevel( Canvas, Result, BorderHighlight, BorderShadow, BevelWidth, BorderSides )
  else if BorderInner = fsStatus then
    Result := DrawBevel( Canvas, Result, BorderShadow, BorderHighlight, BevelWidth, BorderSides )
  else
    Result := DrawBorderSides( Canvas, Result, BorderInner, BorderSides );

end; {= DrawInnerOuterBorders =}


procedure DrawGroupBarBackground( Canvas: TCanvas; Bounds: TRect;
                                  VisualStyle: TRzVisualStyle;
                                  ColorStyle: TRzGradientColorStyle;
                                  GradientPath: TRzGroupBarGradientPath;
                                  CustomStartColor, CustomStopColor: TColor );
var
  ElementDetails: TThemedElementDetails;
  StartColor, StopColor: TColor;
begin
  if UsingSystemStyle and ( VisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
  begin
    ElementDetails := ActiveStyleServices.GetElementDetails( tebExplorerBarRoot );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, Bounds );
  end
  else if ( VisualStyle <> vsClassic ) and FullColorSupported then
  begin
    if ColorStyle <> gcsCustom then
      GetGradientGroupBarColors( ColorStyle, StartColor, StopColor )
    else
    begin
      StartColor := CustomStartColor;
      StopColor := CustomStopColor;
    end;

    if GradientPath = gpTopToBottom then
      PaintGradient( Canvas, Bounds, gdHorizontalEnd, StartColor, StopColor )
    else
      PaintGradient( Canvas, Bounds, gdHorizontalend, StopColor, StartColor );
  end;

end; {= DrawGroupBarBackground =}


{==================================}
{== Generic DrawLEDBar Procedure ==}
{==================================}

procedure DrawLEDBar( Canvas: TCanvas; Bounds: TRect; Orientation: TOrientation;
                      BarColor, BackColor: TColor;
                      NumSegments: Integer; Percent: Integer;
                      ThemeAware, Transparent: Boolean );
var
  X, I, W, D, M, BoxWidth: Integer;
  BoxRct, ThemeRect, VertRect: TRect;
  Offset: Integer;
  SegmentsOn: Integer;
  ElementDetails: TThemedElementDetails;
  Bmp: TBitmap;
begin
  if ThemeAware and ActiveStyleServicesEnabled then
  begin
    if Orientation = orHorizontal then
      ElementDetails := ActiveStyleServices.GetElementDetails( tpChunk )
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( tpChunkVert );

    ThemeRect := Bounds;
    InflateRect( ThemeRect, -1, -1 );

    { Calculate the Size of the Left/Bottom portion of the Percentage Bar }

    if Orientation = orHorizontal then
    begin
      ThemeRect.Right := ThemeRect.Left + Round( ( Longint( ThemeRect.Right - ThemeRect.Left ) * Percent ) / 100 );
      ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, ThemeRect );
    end
    else
    begin
      ThemeRect.Top := ThemeRect.Bottom - Round( ( Longint( ThemeRect.Bottom - ThemeRect.Top ) * Percent ) / 100 );
      VertRect := ThemeRect;

      OffsetRect( ThemeRect, -ThemeRect.Left + Bounds.Left, -ThemeRect.Top + Bounds.Top  );
      if ThemeRect.Top = ThemeRect.Bottom then
        Inc( ThemeRect.Bottom );

      Bmp := TBitmap.Create;
      try
        Bmp.Width := ThemeRect.Right - ThemeRect.Left;
        Bmp.Height := ThemeRect.Bottom - ThemeRect.Top;

        ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, ElementDetails, ThemeRect );
        FlipBitmap( Bmp );
        Canvas.Draw( ThemeRect.Left, VertRect.Top, Bmp );
      finally
        Bmp.Free;
      end;
    end;
  end
  else // No Themes
  begin
    if Orientation = orHorizontal then
      W := Bounds.Right - Bounds.Left
    else
      W := Bounds.Bottom - Bounds.Top;

    BoxWidth := W div NumSegments;

    if ( W <= 80 ) and ( NumSegments > 10 ) then
    begin
      NumSegments := 10;
      BoxWidth := W div NumSegments;
    end;

    D := W - ( NumSegments * BoxWidth );
    if BoxWidth <> 0 then
      M := D div BoxWidth
    else
      M := 0;
    if M > 0 then
      Inc( NumSegments, M );

    Offset := ( W - ( NumSegments * BoxWidth ) ) div 2;

    if not Transparent then
    begin
      // Erase Sides of LED
      Canvas.Pen.Style := psClear;
      Canvas.Brush.Color := BackColor;
      if Orientation = orHorizontal then
      begin
        Canvas.Rectangle( Bounds.Left{ - 1}, Bounds.Top, Bounds.Left + Offset + 1, Bounds.Bottom + 1 );
        Canvas.Rectangle( Bounds.Right - Offset - 1, Bounds.Top, Bounds.Right + 1, Bounds.Bottom + 1 );
      end
      else
      begin
        Canvas.Rectangle( Bounds.Left, Bounds.Top, Bounds.Right + 1, Bounds.Top + Offset + 1 );
        Canvas.Rectangle( Bounds.Left, Bounds.Bottom - Offset - 1, Bounds.Right + 1, Bounds.Bottom + 1 );
      end;
    end;

    if Transparent then
    begin
      Canvas.Pen.Style := psClear;
    end
    else
    begin
      Canvas.Pen.Color := BackColor;
      Canvas.Pen.Style := psSolid;
    end;
    Canvas.Brush.Color := BarColor;

    SegmentsOn := Trunc( Percent * NumSegments / 100 );
    for I := 1 to SegmentsOn do
    begin
      if Orientation = orHorizontal then
      begin
        X := ( I - 1 ) * BoxWidth + Offset;
        BoxRct := Rect( Bounds.Left + X, Bounds.Top,
                        Bounds.Left + X + BoxWidth, Bounds.Bottom );
      end
      else
      begin
        X := ( NumSegments - I  ) * BoxWidth + Offset;
        BoxRct := Rect( Bounds.Left, Bounds.Top + X,
                        Bounds.Right, Bounds.Top + X + BoxWidth );
      end;

      if Transparent then
      begin
        Inc( BoxRct.Left );
        Inc( BoxRct.Top );
      end;

      Canvas.Rectangle( BoxRct );
    end;

    if not Transparent then
    begin
      Canvas.Brush.Color := BackColor;

      for I := SegmentsOn + 1 to NumSegments do
      begin
        if Orientation = orHorizontal then
        begin
          X := ( I - 1 ) * BoxWidth + Offset;
          BoxRct := Rect( Bounds.Left + X, Bounds.Top,
                          Bounds.Left + X + BoxWidth, Bounds.Bottom );
        end
        else
        begin
          X := ( NumSegments - I  ) * BoxWidth + Offset;
          BoxRct := Rect( Bounds.Left, Bounds.Top + X,
                          Bounds.Right, Bounds.Top + X + BoxWidth );
        end;

        Canvas.Rectangle( BoxRct );
      end;
    end;

    if Transparent then
      Canvas.Pen.Style := psSolid;
  end;
end; {= DrawLEDBar =}


{======================================}
{== Generic DrawPercentBar Procedure ==}
{======================================}

procedure DrawPercentBar( Canvas: TCanvas; Bounds: TRect; Orientation: TOrientation;
                          BarColor, BackColor: TColor; Percent: Word;
                          ShowPercent, Transparent: Boolean;
                          ShowParts: Boolean = False;
                          PartsComplete: Longint = 0;
                          TotalParts: Longint = 0 );
var
  PercentStr: string;
  PctRct: TRect;
  TopOffset: Integer;
begin
  if ShowPercent then
  begin
    if ShowParts then
      PercentStr := Format( '%u / %u', [ PartsComplete, TotalParts ] )
    else
      PercentStr := Format( '%u%%', [ Percent ] );
  end
  else
    PercentStr := '';

  Canvas.Font.Color := BackColor;
  Canvas.Brush.Color := BarColor;

  { Calculate the Size of the Left/Bottom portion of the Percentage Bar }

  if Percent >= 100 then
    PctRct := Rect( Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom )
  else if Orientation = orVertical then
  begin
    PctRct := Rect( Bounds.Left, Bounds.Bottom - Round( ( Longint( Bounds.Bottom - Bounds.Top ) * Percent ) / 100 ),
                    Bounds.Right, Bounds.Bottom );
  end
  else
  begin
    PctRct := Rect( Bounds.Left, Bounds.Top,
                    Bounds.Left + Round( ( Longint( Bounds.Right - Bounds.Left ) * Percent ) / 100 ), Bounds.Bottom );
  end;

  { Display the Left/Bottom portion of the Percentage Bar }

  SetTextAlign( Canvas.Handle, ta_Center or ta_Top );
  TopOffset := ( Bounds.Bottom - Bounds.Top - Canvas.TextHeight( 'X' ) ) div 2;
  Canvas.TextRect( PctRct,  Bounds.Right div 2, Bounds.Top + TopOffset, PercentStr );

  // Calculate the Size of the Right/Top portion of the Percentage Bar

  if Orientation = orVertical then
  begin
    PctRct.Bottom := PctRct.Top;
    PctRct.Top := Bounds.Top;
  end
  else
  begin
    PctRct.Left := PctRct.Right;
    PctRct.Right := Bounds.Right;
  end;

  // Display the Right/Top portion of the Percentage Bar

  Canvas.Font.Color := BarColor;
  if Transparent then
    Canvas.Brush.Style := bsClear
  else
    Canvas.Brush.Color := BackColor;
  Canvas.TextRect( PctRct,  Bounds.Right div 2, Bounds.Top + TopOffset, PercentStr );

  if Transparent then
    Canvas.Brush.Style := bsSolid;
end; {= DrawPercentBar =}


{==============================================}
{== Generic DrawGradientPercentBar Procedure ==}
{==============================================}

procedure DrawGradientPercentBar( Canvas: TCanvas; Bounds: TRect; Orientation: TOrientation;
                                  BarColor, BarColorStop, BackColor, BackColorStop: TColor;
                                  GradientDirection: TGradientDirection;
                                  Percent: Word; ShowPercent, Transparent: Boolean;
                                  ShowParts: Boolean = False;
                                  PartsComplete: Longint = 0;
                                  TotalParts: Longint = 0 );
var
  PercentStr: string;
  PctRct: TRect;
  TopOffset: Integer;
begin
  if ShowPercent then
  begin
    if ShowParts then
      PercentStr := Format( '%u / %u', [ PartsComplete, TotalParts ] )
    else
      PercentStr := Format( '%u%%', [ Percent ] );
  end
  else
    PercentStr := '';

  Canvas.Font.Color := BackColor;
  Canvas.Brush.Color := BarColor;

  { Calculate the Size of the Left/Bottom portion of the Percentage Bar }

  if Percent >= 100 then
    PctRct := bounds
  else if Orientation = orVertical then
  begin
    PctRct := Rect( Bounds.Left, Bounds.Bottom - Round( ( Longint( Bounds.Bottom - Bounds.Top ) * Percent ) / 100 ),
                    Bounds.Right, Bounds.Bottom );
  end
  else
  begin
    PctRct := Rect( Bounds.Left, Bounds.Top,
                    Bounds.Left + Round( ( Longint( Bounds.Right - Bounds.Left ) * Percent ) / 100 ), Bounds.Bottom );
  end;

  { Display the Left/Bottom portion of the Percentage Bar }

  SetTextAlign( Canvas.Handle, ta_Center or ta_Top );
  TopOffset := ( Bounds.Bottom - Bounds.Top - Canvas.TextHeight( 'X' ) ) div 2;

  PaintGradient( Canvas, PctRct, GradientDirection, BarColor, BarColorStop );
  if PercentStr <> '' then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect( PctRct, Bounds.Right div 2, Bounds.Top + TopOffset, PercentStr );
    Canvas.Brush.Style := bsSolid;
  end;

  { Calculate the Size of the Right/Top portion of the Percentage Bar }

  if Orientation = orVertical then
  begin
    PctRct.Bottom := PctRct.Top;
    PctRct.Top := Bounds.Top;
  end
  else
  begin
    PctRct.Left := PctRct.Right;
    PctRct.Right := Bounds.Right;
  end;

  // Display the Right/Top portion of the Percentage Bar

  Canvas.Font.Color := BarColor;
  if not Transparent then
  begin
    Canvas.Brush.Color := BackColor;
    PaintGradient( Canvas, PctRct, GradientDirection, BackColor, BackColorStop );
  end;

  if PercentStr <> '' then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.TextRect( PctRct,  Bounds.Right div 2, Bounds.Top + TopOffset, PercentStr );
    Canvas.Brush.Style := bsSolid;
  end;
end; {= DrawGradientPercentBar =}


procedure DrawFrame( Canvas: TCanvas; Width, Height: Integer; FrameStyle: TFrameStyle; EraseColor, FrameColor: TColor;
                     FrameSides: TSides; Transparent: Boolean = False );
var
  R: TRect;
begin
  R := Rect( 0, 0, Width, Height );
  if not Transparent then
    DrawBevel( Canvas, R, EraseColor, EraseColor, 2, sdAllSides );

  if FrameStyle = fsFlat then
    DrawSides( Canvas, R, FrameColor, FrameColor, FrameSides )
  else if FrameStyle = fsFlatBold then
    DrawBevel( Canvas, R, FrameColor, FrameColor, 2, FrameSides )
  else
  begin
    if EraseColor = clWindow then
      DrawBorderSides( Canvas, R, FrameStyle, FrameSides )
    else
      DrawColorBorderSides( Canvas, R, EraseColor, FrameStyle, FrameSides );
  end;
end;


procedure InvalidateWindowFrame( Handle: HWnd; Bounds: TRect );
var
  R, ClipRect: TRect;
  DC: HDC;
  V: Integer;
  DoRedraw: Boolean;
begin
  R := Bounds;
  DC := GetDC( Handle );
  try
    V := GetClipBox( DC, ClipRect );
    DoRedraw := ( V <> ERROR ) and ( V <> NULLREGION );
  finally
    ReleaseDC( Handle, DC );
  end;
  if DoRedraw then
    RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_Frame );
end;


procedure InvalidateControls( Container: TWinControl );
var
  I: Integer;
begin
  for I := 0 to Container.ControlCount - 1 do
  begin
    if IsPublishedProp( Container.Controls[ I ], 'Transparent' ) then
    begin
      if GetOrdProp( Container.Controls[ I ], 'Transparent' ) = 1 then
        Container.Controls[ I ].Invalidate;
    end;
  end;
end;


procedure DrawDropDownArrow( Canvas: TCanvas; Bounds: TRect; UIStyle: TRzUIStyle; Down: Boolean;
                             Enabled: Boolean = True );
var
  X, Y: Integer;
  C, OldBrushColor: TColor;
  ElementDetails: TThemedElementDetails;
  R: TRect;
  TempBmp: TBitmap;
begin
  if RunningAtLeast( win10 ) then
    UIStyle := uiWindows10
  else if RunningAtLeast( winVista ) then
    UIStyle := uiWindowsVista;

  OldBrushColor := Canvas.Brush.Color;
  if Enabled then
  begin
    // Get appropriate color of arrow
    case UIStyle of
      uiWindows95,
      uiWindowsVista,
      uiWindows10:
      begin
        if ColorsTooClose( Canvas.Brush.Color, clBlack ) then
          Canvas.Brush.Color := cl3DDkShadow
        else
          Canvas.Brush.Color := clBlack;
      end;

      uiWindowsXP:
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
      end;

      uiCustomVclStyle:
      begin
        {$IFDEF VCL160_OR_HIGHER}
        ElementDetails := StyleServices.GetElementDetails( tcDropDownButtonNormal );
        TempBmp := TBitmap.Create;
        try
          R := Rect( 0, 0, 16, 20 );
          TempBmp.Width := 17;
          TempBmp.Height := 21;
          StyleServices.DrawElement( TempBmp.Canvas.Handle, ElementDetails, R );
          C := TempBmp.Canvas.Pixels[ 8, 10 ];
        finally
          TempBmp.Free;
        end;
        Canvas.Brush.Color := C;

        {$ELSE}

        if ColorsTooClose( Canvas.Brush.Color, clBlack ) then
          Canvas.Brush.Color := cl3DDkShadow
        else
          Canvas.Brush.Color := clBlack;
        {$ENDIF}
      end;
    end;
  end
  else
    Canvas.Brush.Color := clBtnShadow;
  Canvas.Pen.Style := psClear;

  X := Bounds.Left + ( Bounds.Right - Bounds.Left ) div 2;
  Y := Bounds.Top + ( Bounds.Bottom - Bounds.Top ) div 2;

  if ( Bounds.Bottom - Bounds.Top ) mod 2 = 0 then
    Dec( Y );


  case UIStyle of
    uiWindows95:
    begin
      Inc( Y, 2 );
      Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 4 ), Point( X + 4, Y - 4 ) ] );
    end;

    uiWindowsVista:
    begin
      Inc( Y, 3 );
      Inc( X );
      Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 4 ), Point( X + 4, Y - 4 ) ] );
    end;

    uiWindowsXP:
    begin
      Inc( Y, 4 );
      Canvas.Polygon( [ Point( X, Y ), Point( X - 4, Y - 5 ),
                        Point( X - 3, Y - 7 ), Point( X, Y - 3 ),
                        Point( X + 3, Y - 7 ), Point( X + 5, Y - 5 ) ] );
    end;

    uiWindows10:
    begin
      Inc( Y, 4 );
      Canvas.Polygon( [ Point( X, Y - 1 ),
                        Point( X - 3, Y - 5 ),
                        Point( X - 3, Y - 7 ),
                        Point( X, Y - 3 ),
                        Point( X + 4, Y - 6 ),
                        Point( X + 5, Y - 6 ),
                        Point( X + 1, Y - 1 ) ] );


      (*
      Canvas.Polygon( [ Point( X, Y ), Point( X - 4, Y - 5 ),
                        Point( X - 3, Y - 7 ), Point( X, Y - 3 ),
                        Point( X + 3, Y - 7 ), Point( X + 5, Y - 5 ) ] );
                        *)
    end;

    uiCustomVclStyle:
    begin
      Inc( Y, 3 );
      Inc( X );
      Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 4 ), Point( X + 4, Y - 4 ) ] );
    end;
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := OldBrushColor;
end; {= DrawDropDownArrow =}



procedure DrawSpinArrow( Canvas: TCanvas; Bounds: TRect; Color, DisabledColor: TColor;
                         Direction: TDirection; Down: Boolean; Enabled: Boolean = True );
var
  X, Y: Integer;
  OldBrushColor: TColor;
begin
  OldBrushColor := Canvas.Brush.Color;

  if Enabled then
    Canvas.Brush.Color := Color
  else
    Canvas.Brush.Color := DisabledColor;

  Canvas.Pen.Style := psClear;

  X := Bounds.Left + ( Bounds.Right - Bounds.Left ) div 2;
  Y := Bounds.Top + ( Bounds.Bottom - Bounds.Top ) div 2;
  if ( Bounds.Bottom - Bounds.Top ) mod 2 = 0 then
    Dec( Y );

  case Direction of
    dirLeft, dirRight:
      Dec( X, 2 );
    dirUp, dirDown:
      Inc( Y, 1 );
  end;

  case Direction of
    dirLeft:
      Canvas.Polygon( [ Point( X, Y ), Point( X + 4, Y - 4 ), Point( X + 4, Y + 4 ) ] );
    dirUp:
      Canvas.Polygon( [ Point( X, Y - 5 ), Point( X + 4, Y ), Point( X - 4, Y ) ] );
    dirRight:
      Canvas.Polygon( [ Point( X + 4, Y ), Point( X, Y + 4 ), Point( X, Y - 4 ) ] );
    dirDown:
      Canvas.Polygon( [ Point( X, Y + 1 ), Point( X - 3, Y - 3 ), Point( X + 4, Y - 3 ) ] );
  end;


  (* uiCustomVclStyle values - smaller triangle
  case Direction of
    dirLeft, dirRight:
      Dec( X, 2 );
    dirUp, dirDown:
      Inc( Y, 1 );
  end;

  case Direction of
    dirLeft:
      Canvas.Polygon( [ Point( X + 1, Y ), Point( X + 4, Y - 3 ), Point( X + 4, Y + 3 ) ] );
    dirUp:
      Canvas.Polygon( [ Point( X, Y - 3 ), Point( X + 3, Y + 1 ), Point( X - 3, Y + 1 ) ] );
    dirRight:
      Canvas.Polygon( [ Point( X + 3, Y ), Point( X, Y + 3 ), Point( X, Y - 3 ) ] );
    dirDown:
      Canvas.Polygon( [ Point( X, Y ), Point( X - 2, Y - 3 ), Point( X + 3, Y - 3 ) ] );
  end;
  *)

  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := OldBrushColor;
end; {= DrawSpinArrow =}


procedure DrawSpinArrow( Canvas: TCanvas; Bounds: TRect; UIStyle: TRzUIStyle; Direction: TDirection; Down: Boolean;
                         Enabled: Boolean = True );
var
  X, Y: Integer;
  C, OldBrushColor: TColor;
  ElementDetails: TThemedElementDetails;
  R: TRect;
  TempBmp: TBitmap;
begin
  OldBrushColor := Canvas.Brush.Color;

  if Enabled then
  begin
    // Get appropriate color of arrow
    case UIStyle of
      uiWindows95:
      begin
        if ColorsTooClose( Canvas.Brush.Color, clBlack ) then
          Canvas.Brush.Color := cl3DDkShadow
        else
          Canvas.Brush.Color := clBlack;
      end;

      uiWindowsXP:
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
      end;

      uiWindowsVista:
      begin
        ElementDetails := ActiveStyleServices.GetElementDetails( tsDownNormal );
        TempBmp := TBitmap.Create;
        try
          R := Rect( 0, 0, 13, 10 );
          TempBmp.Width := 13;
          TempBmp.Height := 10;
          ActiveStyleServices.DrawElement( TempBmp.Canvas.Handle, ElementDetails, R );
          C := TempBmp.Canvas.Pixels[ 6, 4 ];
        finally
          TempBmp.Free;
        end;
        Canvas.Brush.Color := C;
      end;

      uiCustomVclStyle:
      begin
        Canvas.Brush.Color := ActiveStyleFontColor( sfButtonTextNormal );
      end;
    end;
  end
  else
  begin
    Canvas.Brush.Color := ActiveStyleSystemColor( clBtnShadow );
  end;

  Canvas.Pen.Style := psClear;

  X := Bounds.Left + ( Bounds.Right - Bounds.Left ) div 2;
  Y := Bounds.Top + ( Bounds.Bottom - Bounds.Top ) div 2;
  if ( Bounds.Bottom - Bounds.Top ) mod 2 = 0 then
    Dec( Y );

  case UIStyle of
    uiWindows95:
    begin
      case Direction of
        dirLeft, dirRight:
          Dec( X, 2 );
        dirUp, dirDown:
          Inc( Y, 1 );
      end;

      case Direction of
        dirLeft:
          Canvas.Polygon( [ Point( X, Y ), Point( X + 4, Y - 4 ), Point( X + 4, Y + 4 ) ] );
        dirUp:
          Canvas.Polygon( [ Point( X, Y - 5 ), Point( X + 4, Y ), Point( X - 4, Y ) ] );
        dirRight:
          Canvas.Polygon( [ Point( X + 4, Y ), Point( X, Y + 4 ), Point( X, Y - 4 ) ] );
        dirDown:
          Canvas.Polygon( [ Point( X, Y + 1 ), Point( X - 3, Y - 3 ), Point( X + 4, Y - 3 ) ] );
      end;
    end;

    uiWindowsXP:
    begin
      case Direction of
        dirLeft:   Dec( X, 1 );
        dirUp:     Inc( Y, 4 );
        dirRight:  Dec( X, 3 );
        dirDown:   Inc( Y, 2 );
      end;

      case Direction of
        dirLeft:
          Canvas.Polygon( [ Point( X, Y ), Point( X + 4, Y - 4 ), Point( X + 4, Y - 1 ), Point( X + 3, Y ),
                            Point( X + 4, Y + 1 ), Point( X + 4, Y + 4 ) ] );
        dirUp:
          Canvas.Polygon( [ Point( X, Y - 5 ), Point( X + 4, Y ), Point( X + 1, Y ), Point( X, Y - 2 ), Point( X - 1, Y ),
                            Point( X - 4, Y ) ] );
        dirRight:
          Canvas.Polygon( [ Point( X + 4, Y ), Point( X, Y + 4 ), Point( X, Y + 1 ), Point( X + 1, Y ), Point( X, Y - 1 ),
                            Point( X, Y - 4 ) ] );
        dirDown:
          Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 4 ), Point( X, Y - 4 ), Point( X, Y - 3 ),
                            Point( X + 1, Y - 4 ), Point( X + 4, Y - 4 ) ] );
      end;
    end;

    uiWindowsVista:
    begin
      case Direction of
        dirLeft, dirRight:
          Dec( X, 2 );
        dirUp, dirDown:
          Inc( Y, 1 );
      end;

      case Direction of
        dirLeft:
          Canvas.Polygon( [ Point( X + 1, Y ), Point( X + 4, Y - 3 ), Point( X + 4, Y + 3 ) ] );
        dirUp:
          Canvas.Polygon( [ Point( X, Y - 3 ), Point( X + 3, Y + 1 ), Point( X - 3, Y + 1 ) ] );
        dirRight:
          Canvas.Polygon( [ Point( X + 3, Y ), Point( X, Y + 3 ), Point( X, Y - 3 ) ] );
        dirDown:
          Canvas.Polygon( [ Point( X, Y ), Point( X - 2, Y - 3 ), Point( X + 3, Y - 3 ) ] );
      end;
    end;

    uiCustomVclStyle:
    begin
      case Direction of
        dirLeft, dirRight:
          Dec( X, 2 );
        dirUp, dirDown:
          Inc( Y, 1 );
      end;

      case Direction of
        dirLeft:
          Canvas.Polygon( [ Point( X + 1, Y ), Point( X + 4, Y - 3 ), Point( X + 4, Y + 3 ) ] );
        dirUp:
          Canvas.Polygon( [ Point( X, Y - 3 ), Point( X + 3, Y + 1 ), Point( X - 3, Y + 1 ) ] );
        dirRight:
          Canvas.Polygon( [ Point( X + 3, Y ), Point( X, Y + 3 ), Point( X, Y - 3 ) ] );
        dirDown:
          Canvas.Polygon( [ Point( X, Y ), Point( X - 2, Y - 3 ), Point( X + 3, Y - 3 ) ] );
      end;
    end;
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := OldBrushColor;
end; {= DrawSpinArrow =}




procedure DrawCloseX( Canvas: TCanvas; Bounds: TRect; Color: TColor;
                      Down: Boolean; Enabled: Boolean = True );
var
  X, Y: Integer;
  OldBrushColor: TColor;
begin
  OldBrushColor := Canvas.Brush.Color;

  Canvas.Brush.Color := Color;
  Canvas.Pen.Style := psClear;
  X := Bounds.Left + ( Bounds.Right - Bounds.Left ) div 2;
  Y := Bounds.Top + ( Bounds.Bottom - Bounds.Top ) div 2;
  if ( Bounds.Bottom - Bounds.Top ) mod 2 = 0 then
    Dec( Y );

  if ( ( Bounds.Bottom - Bounds.Top ) < 11 ) or ( ( Bounds.Right - Bounds.Left ) < 9 ) then
  begin
    // Draw Small X
    Canvas.Polygon( [ Point( X, Y - 1 ),
                      Point( X + 1, Y - 2 ),
                      Point( X + 3, Y - 2 ),
                      Point( X + 1, Y ),
                      Point( X + 3, Y + 3 ),
                      Point( X + 1, Y + 2 ),
                      Point( X, Y + 1 ),
                      Point( X - 3, Y + 3 ),
                      Point( X - 3, Y + 2 ),
                      Point( X - 1, Y ),
                      Point( X - 3, Y - 2 ),
                      Point( X - 1, Y - 2 ),
                      Point( X - 1, Y - 1 ) ] );
  end
  else
  begin
    // Draw Normal X
    Canvas.Polygon( [ Point( X, Y - 1 ),
                      Point( X + 2, Y - 3 ),
                      Point( X + 5, Y - 3 ),
                      Point( X + 2, Y ),
                      Point( X + 5, Y + 4 ),
                      Point( X + 2, Y + 3 ),
                      Point( X, Y + 1 ),
                      Point( X - 2, Y + 4 ),
                      Point( X - 4, Y + 3 ),
                      Point( X - 1, Y ),
                      Point( X - 4, Y - 3 ),
                      Point( X - 1, Y - 3 ) ] );
  end;

  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := OldBrushColor;
end; {= DrawCloseX =}


procedure DrawCloseX( Canvas: TCanvas; Bounds: TRect; UIStyle: TRzUIStyle;
                      Down: Boolean; Enabled: Boolean = True );
var
  XColor, C: TColor;
  ElementDetails: TThemedElementDetails;
  R: TRect;
  TempBmp: TBitmap;
begin
  XColor := clBlack;

  case UIStyle of
    uiWindowsXP:
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
      XColor := C;
    end;

    uiWindows95,
    uiWindowsVista:
    begin
      if Enabled then
        XColor := clBlack
      else
        XColor := clBtnShadow;
    end;

    uiCustomVclStyle:
    begin
      if Enabled then
        XColor := ActiveStyleFontColor( sfButtonTextNormal )
      else
        XColor := ActiveStyleFontColor( sfButtonTextDisabled );
    end;
  end;

  DrawCloseX( Canvas, Bounds, XColor, Down, Enabled );
end; {= DrawCloseX =}


procedure DrawHighlightBox( Canvas: TCanvas; Bounds: TRect;
                            GradientDirection: TGradientDirection;
                            StartColor, StopColor: TColor );
var
  ClipRect: TRect;
begin
  if not FullColorSupported then
    Exit;

  ClipRect := Bounds;
  InflateRect( ClipRect, -2, -2 );
  ExcludeClipRect( Canvas.Handle, ClipRect.Left, ClipRect.Top,
                   ClipRect.Right, ClipRect.Bottom );
  PaintGradient( Canvas, Bounds, GradientDirection, StartColor, StopColor );
  SelectClipRgn( Canvas.Handle, 0 );
end;


procedure DrawCheckBox( Canvas: TCanvas; Bounds: TRect; CheckState: TCheckBoxState;
                        DisplayState: TRzButtonDisplayState; Focused: Boolean;
                        HotTrackStyle: TRzButtonHotTrackStyle;
                        FrameColor, MarkColor, FillColor, FocusColor, DisabledColor,
                        HotTrackStartColor, HotTrackStopColor: TColor;
                        ReadOnly, ReadOnlyColorOnFocus: Boolean;
                        ReadOnlyColor: TColor );
var
  L, T: Integer;
begin
  if ( DisplayState = bdsHot ) and ( HotTrackStyle = htsFrame ) then
    Bounds := DrawBox( Canvas, Bounds, HotTrackStopColor )
  else
    Bounds := DrawBox( Canvas, Bounds, FrameColor );

  case DisplayState of
    bdsNormal, bdsHot:
    begin
      if Focused then
      begin
        if ReadOnly and ReadOnlyColorOnFocus then
          Canvas.Brush.Color := ReadOnlyColor
        else
          Canvas.Brush.Color := FocusColor;
      end
      else if ReadOnly then
        Canvas.Brush.Color := ReadOnlyColor
      else
        Canvas.Brush.Color := FillColor;
    end;

    bdsDown:
      Canvas.Brush.Color := DarkerColor( FocusColor, 20 );

    bdsDisabled:
    begin
      Canvas.Brush.Color := DisabledColor;
      MarkColor := clBtnShadow;
    end;
  end;
  Canvas.FillRect( Bounds );

  // Draw Check Box Mark
  case CheckState of
    cbUnchecked:
    begin
      // Empty.  Nothing to do.
    end;

    cbChecked:
    begin
      L := Bounds.Left - 1;
      T := Bounds.Top - 1;
      Canvas.Pixels[ L + 3, T + 5 ] := MarkColor;
      Canvas.Pixels[ L + 3, T + 6 ] := MarkColor;
      Canvas.Pixels[ L + 3, T + 7 ] := MarkColor;
      Canvas.Pixels[ L + 4, T + 6 ] := MarkColor;
      Canvas.Pixels[ L + 4, T + 7 ] := MarkColor;
      Canvas.Pixels[ L + 4, T + 8 ] := MarkColor;
      Canvas.Pixels[ L + 5, T + 7 ] := MarkColor;
      Canvas.Pixels[ L + 5, T + 8 ] := MarkColor;
      Canvas.Pixels[ L + 5, T + 9 ] := MarkColor;
      Canvas.Pixels[ L + 6, T + 6 ] := MarkColor;
      Canvas.Pixels[ L + 6, T + 7 ] := MarkColor;
      Canvas.Pixels[ L + 6, T + 8 ] := MarkColor;
      Canvas.Pixels[ L + 7, T + 5 ] := MarkColor;
      Canvas.Pixels[ L + 7, T + 6 ] := MarkColor;
      Canvas.Pixels[ L + 7, T + 7 ] := MarkColor;
      Canvas.Pixels[ L + 8, T + 4 ] := MarkColor;
      Canvas.Pixels[ L + 8, T + 5 ] := MarkColor;
      Canvas.Pixels[ L + 8, T + 6 ] := MarkColor;
      Canvas.Pixels[ L + 9, T + 3 ] := MarkColor;
      Canvas.Pixels[ L + 9, T + 4 ] := MarkColor;
      Canvas.Pixels[ L + 9, T + 5 ] := MarkColor;

    end;

    cbGrayed:
    begin
      InflateRect( Bounds, -2, -2 );
      Canvas.Brush.Color := MarkColor;
      Canvas.FillRect( Bounds );
      InflateRect( Bounds, 2, 2 );
    end;
  end;

  if DisplayState = bdsHot then
  begin
    case HotTrackStyle of
      htsInterior:
        DrawHighlightBox( Canvas, Bounds, gdDiagonalDown,
                          HotTrackStartColor, HotTrackStopColor );

      htsFrame:
        Bounds := DrawBox( Canvas, Bounds, HotTrackStopColor );
    end;
  end;

end; {= DrawCheckBox =}


procedure DrawRadioButton( Canvas: TCanvas; Bounds: TRect; Checked: Boolean;
                           DisplayState: TRzButtonDisplayState; Focused: Boolean;
                           HotTrackStyle: TRzButtonHotTrackStyle;
                           FrameColor, MarkColor, FillColor, FocusColor, DisabledColor,
                           HotTrackStartColor, HotTrackStopColor,
                           BackgroundColor: TColor;
                           Transparent: Boolean; TransparentColor: TColor;
                           ReadOnly, ReadOnlyColorOnFocus: Boolean;
                           ReadOnlyColor: TColor );
var
  B: TBitmap;
  R, ClipRect: TRect;
  BackColor, LighterFrameColor: TColor;
  DotColor1, DotColor2: TColor;
  FrameBlendColor1, FrameBlendColor2: TColor;
begin
  B := TBitmap.Create;
  try
    // Create temp bitmap that radio button will be drawn upon
    B.Width := Bounds.Right - Bounds.Left;
    B.Height := Bounds.Bottom - Bounds.Top;
    R := Rect( 0, 0, B.Width, B.Height );

    // Fill background with content color, or hot track selection

    case DisplayState of
      bdsNormal, bdsHot:
      begin
        if Focused then
        begin
          if ReadOnly and ReadOnlyColorOnFocus then
            B.Canvas.Brush.Color := ReadOnlyColor
          else
            B.Canvas.Brush.Color := FocusColor;
        end
        else if ReadOnly then
          B.Canvas.Brush.Color := ReadOnlyColor
        else
          B.Canvas.Brush.Color := FillColor;
      end;

      bdsDown:
        B.Canvas.Brush.Color := DarkerColor( FocusColor, 20 );

      bdsDisabled:
        B.Canvas.Brush.Color := DisabledColor;
    end;
    B.Canvas.FillRect( R );

    if ( DisplayState = bdsHot ) and ( HotTrackStyle = htsFrame ) then
      FrameColor := HotTrackStopColor;

    if FullColorSupported then
      LighterFrameColor := LighterColor( FrameColor, 10 )
    else
      LighterFrameColor := FrameColor;


    if ( DisplayState = bdsHot ) and FullColorSupported then
    begin
      if HotTrackStyle = htsInterior then
      begin
        ClipRect := R;
        InflateRect( ClipRect, -4, -4 );
        ExcludeClipRect( B.Canvas.Handle, ClipRect.Left, ClipRect.Top,
                         ClipRect.Right, ClipRect.Bottom );
        ExcludeClipRect( B.Canvas.Handle, 5, 3, 8, 10 );
        ExcludeClipRect( B.Canvas.Handle, 3, 5, 10, 8 );
        PaintGradient( B.Canvas, R, gdDiagonalDown,
                       HotTrackStartColor, HotTrackStopColor );
        SelectClipRgn( B.Canvas.Handle, 0 );
      end
      else // htsFrame
      begin

        B.Canvas.Pixels[  5,  1 ] := LighterFrameColor;
        B.Canvas.Pixels[  6,  1 ] := LighterFrameColor;
        B.Canvas.Pixels[  7,  1 ] := LighterFrameColor;
        B.Canvas.Pixels[  1,  5 ] := LighterFrameColor;
        B.Canvas.Pixels[ 11,  5 ] := LighterFrameColor;
        B.Canvas.Pixels[  1,  6 ] := LighterFrameColor;
        B.Canvas.Pixels[ 11,  6 ] := LighterFrameColor;
        B.Canvas.Pixels[  1,  7 ] := LighterFrameColor;
        B.Canvas.Pixels[ 11,  7 ] := LighterFrameColor;
        B.Canvas.Pixels[  5, 11 ] := LighterFrameColor;
        B.Canvas.Pixels[  6, 11 ] := LighterFrameColor;
        B.Canvas.Pixels[  7, 11 ] := LighterFrameColor;

        B.Canvas.Pixels[  4,  1 ] := FrameColor;
        B.Canvas.Pixels[  8,  1 ] := FrameColor;
        B.Canvas.Pixels[  1,  4 ] := FrameColor;
        B.Canvas.Pixels[ 11,  4 ] := FrameColor;
        B.Canvas.Pixels[  1,  8 ] := FrameColor;
        B.Canvas.Pixels[ 11,  8 ] := FrameColor;
        B.Canvas.Pixels[  4, 11 ] := FrameColor;
        B.Canvas.Pixels[  8, 11 ] := FrameColor;
      end;
    end;

    // Paint Background pixels (corners)
    if not Transparent then
      BackColor := BackgroundColor
    else
      BackColor := TransparentColor;
    B.Canvas.Pixels[  0,  0 ] := BackColor;
    B.Canvas.Pixels[  1,  0 ] := BackColor;
    B.Canvas.Pixels[  2,  0 ] := BackColor;
    B.Canvas.Pixels[ 10,  0 ] := BackColor;
    B.Canvas.Pixels[ 11,  0 ] := BackColor;
    B.Canvas.Pixels[ 12,  0 ] := BackColor;
    B.Canvas.Pixels[  0,  1 ] := BackColor;
    B.Canvas.Pixels[  1,  1 ] := BackColor;
    B.Canvas.Pixels[ 11,  1 ] := BackColor;
    B.Canvas.Pixels[ 12,  1 ] := BackColor;
    B.Canvas.Pixels[  0,  2 ] := BackColor;
    B.Canvas.Pixels[ 12,  2 ] := BackColor;
    B.Canvas.Pixels[  0, 10 ] := BackColor;
    B.Canvas.Pixels[ 12, 10 ] := BackColor;
    B.Canvas.Pixels[  0, 11 ] := BackColor;
    B.Canvas.Pixels[  1, 11 ] := BackColor;
    B.Canvas.Pixels[ 11, 11 ] := BackColor;
    B.Canvas.Pixels[ 12, 11 ] := BackColor;
    B.Canvas.Pixels[  0, 12 ] := BackColor;
    B.Canvas.Pixels[  1, 12 ] := BackColor;
    B.Canvas.Pixels[  2, 12 ] := BackColor;
    B.Canvas.Pixels[ 10, 12 ] := BackColor;
    B.Canvas.Pixels[ 11, 12 ] := BackColor;
    B.Canvas.Pixels[ 12, 12 ] := BackColor;

    // Paint Border Blending Pixels
    if Transparent then
    begin
      FrameBlendColor1 := TransparentColor;
      FrameBlendColor2 := TransparentColor;
    end
    else if FullColorSupported then
    begin
      FrameBlendColor1 := BlendColors( FrameColor, BackgroundColor, 128 );
      FrameBlendColor2 := BlendColors( FrameColor, BackgroundColor, 40 );
    end
    else
    begin
      FrameBlendColor1 := FrameColor;
      FrameBlendColor2 := BackgroundColor;
    end;
    B.Canvas.Pixels[  4,  0 ] := FrameBlendColor1;
    B.Canvas.Pixels[  8,  0 ] := FrameBlendColor1;
    B.Canvas.Pixels[  2,  1 ] := FrameBlendColor1;
    B.Canvas.Pixels[ 10,  1 ] := FrameBlendColor1;
    B.Canvas.Pixels[  1,  2 ] := FrameBlendColor1;
    B.Canvas.Pixels[ 11,  2 ] := FrameBlendColor1;
    B.Canvas.Pixels[  0,  4 ] := FrameBlendColor1;
    B.Canvas.Pixels[ 12,  4 ] := FrameBlendColor1;
    B.Canvas.Pixels[  0,  8 ] := FrameBlendColor1;
    B.Canvas.Pixels[ 12,  8 ] := FrameBlendColor1;
    B.Canvas.Pixels[  1, 10 ] := FrameBlendColor1;
    B.Canvas.Pixels[ 11, 10 ] := FrameBlendColor1;
    B.Canvas.Pixels[  2, 11 ] := FrameBlendColor1;
    B.Canvas.Pixels[ 10, 11 ] := FrameBlendColor1;
    B.Canvas.Pixels[  4, 12 ] := FrameBlendColor1;
    B.Canvas.Pixels[  8, 12 ] := FrameBlendColor1;

    B.Canvas.Pixels[  3,  0 ] := FrameBlendColor2;
    B.Canvas.Pixels[  9,  0 ] := FrameBlendColor2;
    B.Canvas.Pixels[  0,  3 ] := FrameBlendColor2;
    B.Canvas.Pixels[ 12,  3 ] := FrameBlendColor2;
    B.Canvas.Pixels[  0,  9 ] := FrameBlendColor2;
    B.Canvas.Pixels[ 12,  9 ] := FrameBlendColor2;
    B.Canvas.Pixels[  3, 12 ] := FrameBlendColor2;
    B.Canvas.Pixels[  9, 12 ] := FrameBlendColor2;


    // Paint Frame Border
    B.Canvas.Pixels[  6,  0 ] := FrameColor;
    B.Canvas.Pixels[  3,  1 ] := FrameColor;
    B.Canvas.Pixels[  9,  1 ] := FrameColor;
    B.Canvas.Pixels[  1,  3 ] := FrameColor;
    B.Canvas.Pixels[ 11,  3 ] := FrameColor;
    B.Canvas.Pixels[  0,  6 ] := FrameColor;
    B.Canvas.Pixels[ 12,  6 ] := FrameColor;
    B.Canvas.Pixels[  1,  9 ] := FrameColor;
    B.Canvas.Pixels[ 11,  9 ] := FrameColor;
    B.Canvas.Pixels[  3, 11 ] := FrameColor;
    B.Canvas.Pixels[  9, 11 ] := FrameColor;
    B.Canvas.Pixels[  6, 12 ] := FrameColor;

    B.Canvas.Pixels[  5,  0 ] := LighterFrameColor;
    B.Canvas.Pixels[  7,  0 ] := LighterFrameColor;
    B.Canvas.Pixels[  2,  2 ] := LighterFrameColor;
    B.Canvas.Pixels[ 10,  2 ] := LighterFrameColor;
    B.Canvas.Pixels[  0,  5 ] := LighterFrameColor;
    B.Canvas.Pixels[ 12,  5 ] := LighterFrameColor;
    B.Canvas.Pixels[  0,  7 ] := LighterFrameColor;
    B.Canvas.Pixels[ 12,  7 ] := LighterFrameColor;
    B.Canvas.Pixels[  2, 10 ] := LighterFrameColor;
    B.Canvas.Pixels[ 10, 10 ] := LighterFrameColor;
    B.Canvas.Pixels[  5, 12 ] := LighterFrameColor;
    B.Canvas.Pixels[  7, 12 ] := LighterFrameColor;

    if FullColorSupported then
    begin
      if ( DisplayState = bdsHot ) and ( HotTrackStyle = htsFrame ) then
      begin
        B.Canvas.Pixels[  3,  2 ] := BlendColors( FrameColor, B.Canvas.Pixels[  3,  2 ], 150 );
        B.Canvas.Pixels[  9,  2 ] := BlendColors( FrameColor, B.Canvas.Pixels[  9,  2 ], 150 );
        B.Canvas.Pixels[  2,  3 ] := BlendColors( FrameColor, B.Canvas.Pixels[  2,  3 ], 150 );
        B.Canvas.Pixels[ 10,  3 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 10,  3 ], 150 );
        B.Canvas.Pixels[  2,  9 ] := BlendColors( FrameColor, B.Canvas.Pixels[  2,  9 ], 150 );
        B.Canvas.Pixels[ 10,  9 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 10,  9 ], 150 );
        B.Canvas.Pixels[  3, 10 ] := BlendColors( FrameColor, B.Canvas.Pixels[  3, 10 ], 150 );
        B.Canvas.Pixels[  9, 10 ] := BlendColors( FrameColor, B.Canvas.Pixels[  9, 10 ], 150 );

        B.Canvas.Pixels[  4,  2 ] := BlendColors( FrameColor, B.Canvas.Pixels[  4,  2 ], 80 );
        B.Canvas.Pixels[  8,  2 ] := BlendColors( FrameColor, B.Canvas.Pixels[  8,  2 ], 80 );
        B.Canvas.Pixels[  2,  4 ] := BlendColors( FrameColor, B.Canvas.Pixels[  2,  4 ], 80 );
        B.Canvas.Pixels[ 10,  4 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 10,  4 ], 80 );
        B.Canvas.Pixels[  2,  8 ] := BlendColors( FrameColor, B.Canvas.Pixels[  2,  8 ], 80 );
        B.Canvas.Pixels[ 10,  8 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 10,  8 ], 80 );
        B.Canvas.Pixels[  4, 10 ] := BlendColors( FrameColor, B.Canvas.Pixels[  4, 10 ], 80 );
        B.Canvas.Pixels[  8, 10 ] := BlendColors( FrameColor, B.Canvas.Pixels[  8, 10 ], 80 );
      end
      else
      begin
        B.Canvas.Pixels[  4,  1 ] := BlendColors( FrameColor, B.Canvas.Pixels[  4,  1 ], 128 );
        B.Canvas.Pixels[  8,  1 ] := BlendColors( FrameColor, B.Canvas.Pixels[  8,  1 ], 128 );
        B.Canvas.Pixels[  1,  4 ] := BlendColors( FrameColor, B.Canvas.Pixels[  1,  4 ], 128 );
        B.Canvas.Pixels[ 11,  4 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 11,  4 ], 128 );
        B.Canvas.Pixels[  1,  8 ] := BlendColors( FrameColor, B.Canvas.Pixels[  1,  8 ], 128 );
        B.Canvas.Pixels[ 11,  8 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 11,  8 ], 128 );
        B.Canvas.Pixels[  4, 11 ] := BlendColors( FrameColor, B.Canvas.Pixels[  4, 11 ], 128 );
        B.Canvas.Pixels[  8, 11 ] := BlendColors( FrameColor, B.Canvas.Pixels[  8, 11 ], 128 );

        B.Canvas.Pixels[  5,  1 ] := BlendColors( FrameColor, B.Canvas.Pixels[  5,  1 ], 80 );
        B.Canvas.Pixels[  7,  1 ] := BlendColors( FrameColor, B.Canvas.Pixels[  7,  1 ], 80 );
        B.Canvas.Pixels[  3,  2 ] := BlendColors( FrameColor, B.Canvas.Pixels[  3,  2 ], 80 );
        B.Canvas.Pixels[  9,  2 ] := BlendColors( FrameColor, B.Canvas.Pixels[  9,  2 ], 80 );
        B.Canvas.Pixels[  2,  3 ] := BlendColors( FrameColor, B.Canvas.Pixels[  2,  3 ], 80 );
        B.Canvas.Pixels[ 10,  3 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 10,  3 ], 80 );
        B.Canvas.Pixels[  1,  5 ] := BlendColors( FrameColor, B.Canvas.Pixels[  1,  5 ], 80 );
        B.Canvas.Pixels[ 11,  5 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 11,  5 ], 80 );
        B.Canvas.Pixels[  1,  7 ] := BlendColors( FrameColor, B.Canvas.Pixels[  1,  7 ], 80 );
        B.Canvas.Pixels[ 11,  7 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 11,  7 ], 80 );
        B.Canvas.Pixels[  2,  9 ] := BlendColors( FrameColor, B.Canvas.Pixels[  2,  9 ], 80 );
        B.Canvas.Pixels[ 10,  9 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 10,  9 ], 80 );
        B.Canvas.Pixels[  3, 10 ] := BlendColors( FrameColor, B.Canvas.Pixels[  3, 10 ], 80 );
        B.Canvas.Pixels[  9, 10 ] := BlendColors( FrameColor, B.Canvas.Pixels[  9, 10 ], 80 );
        B.Canvas.Pixels[  5, 11 ] := BlendColors( FrameColor, B.Canvas.Pixels[  5, 11 ], 80 );
        B.Canvas.Pixels[  7, 11 ] := BlendColors( FrameColor, B.Canvas.Pixels[  7, 11 ], 80 );

        B.Canvas.Pixels[  6,  1 ] := BlendColors( FrameColor, B.Canvas.Pixels[  6,  1 ], 40 );
        B.Canvas.Pixels[  1,  6 ] := BlendColors( FrameColor, B.Canvas.Pixels[  1,  6 ], 40 );
        B.Canvas.Pixels[ 11,  6 ] := BlendColors( FrameColor, B.Canvas.Pixels[ 11,  6 ], 40 );
        B.Canvas.Pixels[  6, 11 ] := BlendColors( FrameColor, B.Canvas.Pixels[  6, 11 ], 40 );
      end;
    end
    else // No Full Color Support
    begin
      B.Canvas.Pixels[  4,  1 ] := FrameColor;
      B.Canvas.Pixels[  8,  1 ] := FrameColor;
      B.Canvas.Pixels[  1,  4 ] := FrameColor;
      B.Canvas.Pixels[ 11,  4 ] := FrameColor;
      B.Canvas.Pixels[  1,  8 ] := FrameColor;
      B.Canvas.Pixels[ 11,  8 ] := FrameColor;
      B.Canvas.Pixels[  4, 11 ] := FrameColor;
      B.Canvas.Pixels[  8, 11 ] := FrameColor;

      B.Canvas.Pixels[  5,  1 ] := clSilver;
      B.Canvas.Pixels[  7,  1 ] := clSilver;
      B.Canvas.Pixels[  3,  2 ] := clSilver;
      B.Canvas.Pixels[  9,  2 ] := clSilver;
      B.Canvas.Pixels[  2,  3 ] := clSilver;
      B.Canvas.Pixels[ 10,  3 ] := clSilver;
      B.Canvas.Pixels[  1,  5 ] := clSilver;
      B.Canvas.Pixels[ 11,  5 ] := clSilver;
      B.Canvas.Pixels[  1,  7 ] := clSilver;
      B.Canvas.Pixels[ 11,  7 ] := clSilver;
      B.Canvas.Pixels[  2,  9 ] := clSilver;
      B.Canvas.Pixels[ 10,  9 ] := clSilver;
      B.Canvas.Pixels[  3, 10 ] := clSilver;
      B.Canvas.Pixels[  9, 10 ] := clSilver;
      B.Canvas.Pixels[  5, 11 ] := clSilver;
      B.Canvas.Pixels[  7, 11 ] := clSilver;
    end;

    if Checked then
    begin
      if DisplayState = bdsDisabled then
        DotColor1 := clBtnShadow
      else
        DotColor1 := MarkColor;
      if FullColorSupported then
        DotColor2 := BlendColors( DotColor1, B.Canvas.Pixels[ 4, 4 ], 153 )
      else
        DotColor2 := DotColor1;

      B.Canvas.Brush.Color := DotColor1;
      B.Canvas.FillRect( Rect( 5, 5, 8, 8 ) );

      B.Canvas.Pixels[ 6, 4 ] := DotColor1;
      B.Canvas.Pixels[ 4, 6 ] := DotColor1;
      B.Canvas.Pixels[ 8, 6 ] := DotColor1;
      B.Canvas.Pixels[ 6, 8 ] := DotColor1;

      B.Canvas.Pixels[ 5, 4 ] := DotColor2;
      B.Canvas.Pixels[ 7, 4 ] := DotColor2;
      B.Canvas.Pixels[ 4, 5 ] := DotColor2;
      B.Canvas.Pixels[ 8, 5 ] := DotColor2;
      B.Canvas.Pixels[ 4, 7 ] := DotColor2;
      B.Canvas.Pixels[ 8, 7 ] := DotColor2;
      B.Canvas.Pixels[ 5, 8 ] := DotColor2;
      B.Canvas.Pixels[ 7, 8 ] := DotColor2;
    end;

    Canvas.Draw( Bounds.Left, Bounds.Top, B );
  finally
    B.Free;
  end;
end; {= DrawRadioButton =}


procedure AddImageToImageList( ImageList: TCustomImageList; Glyph: TBitmap; AddDisabled: Boolean;
                               var ImageIndex, DisabledIndex: Integer );
var
  B: TBitmap;
  R: TRect;
begin
  ImageIndex := -1;
  DisabledIndex := -1;

  if ( ImageList <> nil ) and ( Glyph <> nil ) then
  begin
    if Glyph.Width = Glyph.Height then
    begin
      // Easy case -- Only one glyph in bitmap (i.e. no disabled glyph)
      ImageList.AddMasked( Glyph, Glyph.Canvas.Pixels[ 0, 15 ] );
      ImageIndex := ImageList.Count - 1;
    end
    else
    begin
      // Assume Glyph has both a normal image and a disabled image
      if AddDisabled then
      begin
        ImageList.AddMasked( Glyph, Glyph.Canvas.Pixels[ 0, 15 ] );
        DisabledIndex := ImageList.Count - 1;
        ImageIndex := ImageList.Count - 2;
      end
      else
      begin
        // Extract out the normal image from Glyph
        B := TBitmap.Create;
        try
          B.Width := ImageList.Width;
          B.Height := ImageList.Height;
          R := Rect( 0, 0, B.Width, B.Height );
          B.Canvas.CopyRect( R, Glyph.Canvas, R );
          ImageList.AddMasked( B, B.Canvas.Pixels[ 0, 15 ] );
          ImageIndex := ImageList.Count - 1;
        finally
          B.Free;
        end;
      end;
    end;

  end;
end;


{===============================================================================
  GetNewComponentName

  Description
    This function scans generates a new component name based on the BaseName
    and an index. If the component name is already used, the index is
    incremented until a unique name is generated.
===============================================================================}

function GetNewComponentName( AOwner: TComponent; const BaseName: string; TryNoIndex: Boolean = True ): string;
var
  I: Integer;
begin
  Result := BaseName;
  if TryNoIndex then
  begin
    if AOwner.FindComponent( Result ) = nil then
      Exit;
  end;

  I := 0;
  repeat
    Inc( I );
    Result := BaseName + IntToStr( I );
  until AOwner.FindComponent( Result ) = nil;
end; {= GetNewComponentName =}


function CreateValidIdent( const Ident, DefaultName: string ): string;
{$IFNDEF UNICODE}
const
  Alpha = [ 'A'..'Z', 'a'..'z', '_' ];
  AlphaNumeric = Alpha + [ '0'..'9' ];
{$ELSE}
  function Alpha( C: Char ): Boolean; inline;
  begin
    {$IFDEF VCL180_OR_HIGHER}
    Result := C.IsLetter or ( C = '_' );
    {$ELSE}   
    Result := TCharacter.IsLetter( C ) or ( C = '_' );
    {$ENDIF}
  end;

  function AlphaNumeric( C: Char ): Boolean; inline;
  begin
    {$IFDEF VCL180_OR_HIGHER}
    Result := C.IsLetterOrDigit or ( C = '_' );
    {$ELSE}   
    Result := TCharacter.IsLetterOrDigit( C ) or ( C = '_' );
    {$ENDIF}
  end;
{$ENDIF}
var
  I: Integer;
begin
  Result := '';
  if IsValidIdent( Ident ) then
    Result := Ident
  else if Ident = '' then
    Result := DefaultName
  else
  begin
    {$IFNDEF UNICODE}
    if Ident[ 1 ] in Alpha then
      Result := Result + Ident[ 1 ];

    for I := 2 to Length( Ident ) do
    begin
      if Ident[ I ] in AlphaNumeric then
        Result := Result + Ident[ I ];
    end;
    {$ELSE}
    if Alpha( Ident[ 1 ] ) then
      Result := Result + Ident[ 1 ];

    for I := 2 to Length( Ident ) do
    begin
      if AlphaNumeric( Ident[ I ] ) then
        Result := Result + Ident[ I ];
    end;
    {$ENDIF}

    if Result = '' then
      Result := DefaultName;
  end;
end;


function RotateFont( Font: TFont; Angle: Integer ): HFont;
var
  LogFont: TLogFont;
begin
  FillChar( LogFont, SizeOf( LogFont ), #0 );
  with LogFont do
  begin
    lfHeight := Font.Height;
    lfWidth := 0;
    lfEscapement := Angle * 10;        { Escapement must be in 10th of degrees }
    lfOrientation := 0;
    if fsBold in Font.Style then
      lfWeight := fw_Bold
    else
      lfWeight := fw_Normal;
    lfItalic := Byte( fsItalic in Font.Style );
    lfUnderline := Byte( fsUnderline in Font.Style );
    lfStrikeOut := Byte( fsStrikeOut in Font.Style );
    lfCharSet := Font.Charset;
    lfOutPrecision := Out_Default_Precis;
    lfClipPrecision := Clip_Default_Precis;
    lfQuality := Default_Quality;
    case Font.Pitch of
      fpVariable:
        lfPitchAndFamily := Variable_Pitch;

      fpFixed:
        lfPitchAndFamily := Fixed_Pitch;

      else
        lfPitchAndFamily := Default_Pitch;
    end;
    StrPCopy( lfFaceName, Font.Name );
  end; { with }
  Result := CreateFontIndirect( LogFont );
end; {= RotateFont =}


function IsTrueTypeFont( Font: TFont ): Boolean;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC( 0 );
  try
    GetTextMetrics( DC, SysMetrics );
    SaveFont := SelectObject( DC, Font.Handle );
    GetTextMetrics( DC, Metrics );
    SelectObject( DC, SaveFont );
  finally
    ReleaseDC( 0, DC );
  end;

  Result := ( Metrics.tmPitchAndFamily and tmpf_TrueType ) = tmpf_TrueType;
end;


function GetMinFontHeight( Font: TFont ): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC( 0 );
  try
    GetTextMetrics( DC, SysMetrics );
    SaveFont := SelectObject( DC, Font.Handle );
    GetTextMetrics( DC, Metrics );
    SelectObject( DC, SaveFont );
  finally
    ReleaseDC( 0, DC );
  end;

  Result := Metrics.tmHeight + 2;
end;


function GetAvgCharWidth( Font: TFont ): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  S: TSize;
begin
  DC := GetDC( 0 );
  try
    SaveFont := SelectObject( DC, Font.Handle );

    GetTextMetrics( DC, Metrics );
    GetTextExtentPoint32( DC,'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz', 52, S );
    Result := Round( S.cx / 52 );

    SelectObject( DC, SaveFont );
  finally
    ReleaseDC( 0, DC );
  end;
end;



{$IFNDEF UNICODE}

function IsLeadChar( C: Char ): Boolean;
begin
  Result := C in LeadBytes;
end;


function CharInSet( C: Char; const CharSet: TSysCharSet ): Boolean;
begin
  Result := C in CharSet;
end;

{$ENDIF}


function FirstNonWhitespaceChar( const S: string ): Char;
var
  T: string;
begin
  Result := #0;
  if S <> '' then
  begin
    T := TrimLeft( S );
    if T <> '' then
      Result := T[ 1 ];
  end;
end;


function LastChar( const S: string ): Char;
begin
  Result := S[ Length( S ) ];
end;


function CountChar( C: Char; const S: string ): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length( S ) do
  begin
    if S[ I ] = C then
      Inc( Result );
  end;
end;


{===============================================================================
  function CopyEx

  This function is an enhanced version of the Copy function. Instead of
  specifying the number of characters to copy, the last character copied is
  determined by the Count instance of the C character in the string.

  For example,
    S := CopyEx( 'C:\Windows\System', 1, '\', 2 );

    S will be 'C:\Windows\'
===============================================================================}

function CopyEx( const S: string; Start: Integer; C: Char; Count: Integer ): string;
var
  I, J: Integer;
begin
  Result := S;
  J := 0;
  for I := Start to Length( S ) do
  begin
    if S[ I ] = C then
      Inc( J );

    if J = Count then
    begin
      Result := Copy( S, Start, I );
      Break;
    end;
  end;
end;


function RemoveChar( var S: string; C: Char ): Boolean;
var
  I: Integer;
begin
  I := Pos( C, S );
  Result := I > 0;
  if Result then
    Delete( S, I, 1 );
end;


function RemoveAccelerators( const S: string ): string;
var
  I, L: Integer;
begin
  Result := '';
  L := Length( S );
  for I := 1 to L - 1 do
  begin
    if S[ I ] <> '&' then
      Result := Result + S[ I ]
    else
    begin
      if S[ I + 1 ] = '&' then
        Result := Result + '&';
    end;
  end;
  if L > 0 then
    Result := Result + S[ L ];
end;


function ExpandEnvironmentVariables( const s: string ): string;
var
  lenNeeded, n: Integer;
  expanded: string;
begin
  SetLength( expanded, 1 );
  lenNeeded := ExpandEnvironmentStrings( PChar( s ), PChar( expanded ), 0 );

  SetLength( expanded, lenNeeded );
  n := ExpandEnvironmentStrings( PChar( s ), PChar( expanded ), lenNeeded );
  if n > 0 then
  begin
    SetLength( expanded, StrLen( PChar( expanded ) ) );
    Result := expanded;
  end
  else
    Result := s;
end;


function Min( A, B: Integer ): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;


function Max( A, B: Integer ): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;


procedure Swap( var A, B: Integer );
var
  T: Integer;
begin
  T := A;
  A := B;
  B := T;
end;

procedure Swap( var A, B: Word );
var
  T: Word;
begin
  T := A;
  A := B;
  B := T;
end;


procedure UpdateObjectInspector( AControl: TControl );
begin
  if ( csDesigning in AControl.ComponentState ) and
     not ( csLoading in AControl.ComponentState ) and
     not ( csReading in AControl.ComponentState ) and
     not ( csUpdating in AControl.ComponentState ) and
     not ( csFixups in AControl.ComponentState ) and
     not ( csWriting in AControl.ComponentState ) and
     not ( csDestroying in AControl.ComponentState ) and
     ( GetParentForm( AControl ) <> nil ) and
     ( GetParentForm( AControl ).Designer <> nil ) then
  begin
    GetParentForm( AControl ).Designer.Modified;
  end;
end;



{==============================}
{== TRzOldPropReader Methods ==}
{==============================}


class procedure TRzOldPropReader.ReadOldBooleanProp( Reader: TReader );
begin
  Reader.ReadBoolean;
end;


class procedure TRzOldPropReader.ReadOldEnumProp( Reader: TReader );
begin
  Reader.ReadIdent;
end;


class procedure TRzOldPropReader.ReadOldIdentProp( Reader: TReader );
begin
  Reader.ReadIdent;
end;


class procedure TRzOldPropReader.ReadOldIntegerProp( Reader: TReader );
begin
  Reader.ReadInteger;
end;


class procedure TRzOldPropReader.ReadOldSetProp( Reader: TReader );
begin
  Reader.ReadValue;
  while True do
  begin
    if Reader.ReadStr = '' then
      Break;
  end;
end;


class procedure TRzOldPropReader.ReadOldStringProp( Reader: TReader );
begin
  Reader.ReadString;
end;


class procedure TRzOldPropReader.WriteOldProp( Writer: TWriter );
begin
end;



function GetDesktopClientRect: TRect;
var
  TW, TH: Integer;
  TaskBarWnd: HWnd;
  TaskBarRect: TRect;
begin
  GetWindowRect( GetDesktopWindow, Result );

  TaskBarRect := Rect( 0, 0, 0, 0 );
  TaskBarWnd := FindWindow( 'Shell_TrayWnd', '' );
  if TaskBarWnd <> 0 then
  begin
    GetWindowRect( TaskBarWnd, TaskBarRect );
    IntersectRect( TaskBarRect, TaskBarRect, Result );

    TW := TaskBarRect.Right - TaskBarRect.Left;
    TH := TaskBarRect.Bottom - TaskBarRect.Top;
    if ( Result.Left = TaskBarRect.Left ) and ( Result.Top = TaskBarRect.Top ) then
    begin
      if Result.Right = TaskBarRect.Right then
        Inc( Result.Top, TH )
      else
        Inc( Result.Left, TW );
    end
    else if Result.Left = TaskBarRect.Left then
      Dec( Result.Bottom, TH )
    else
      Dec( Result.Right, TW );
  end;
end;


function GetActiveWorkArea( Window: TWinControl ): TRect;
var
  HM: HMonitor;
  MonInfo: TMonitorInfo;
begin
  HM := MonitorFromWindow( Window.Handle, MONITOR_DEFAULTTONEAREST );

  MonInfo.cbSize := SizeOf( MonInfo );
  GetMonitorInfo( HM, @MonInfo );
  Result := MonInfo.rcWork;
end;


function GetActiveWorkAreaWidth( Window: TWinControl ): Integer;
var
  R: TRect;
begin
  R := GetActiveWorkArea( Window );
  Result := R.Right - R.Left;
end;


function GetActiveWorkAreaHeight( Window: TWinControl ): Integer;
var
  R: TRect;
begin
  R := GetActiveWorkArea( Window );
  Result := R.Bottom - R.Top;
end;


function GetMonitorWorkArea( Monitor: TMonitor ): TRect;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf( MonInfo );
  GetMonitorInfo( Monitor.Handle, @MonInfo );
  Result := MonInfo.rcWork;
end;


function GetMonitorBoundsRect( Monitor: TMonitor ): TRect;
var
  MonInfo: TMonitorInfo;
begin
  MonInfo.cbSize := SizeOf( MonInfo );
  GetMonitorInfo( Monitor.Handle, @MonInfo );
  Result := MonInfo.rcMonitor;
end;


function GetMonitorContainingPoint( P: TPoint ): TMonitor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.MonitorCount - 1 do
  begin
    if PtInRect( GetMonitorBoundsRect( Screen.Monitors[ I ] ), P ) then
    begin
      Result := Screen.Monitors[ I ];
      Exit;
    end;
  end;
end;


procedure CenterToWindow( ChildWin, Window: TWinControl; var Left, Top: Integer );
var
  R: TRect;
  WorkArea: TRect;
begin
  R.TopLeft := Window.ClientToScreen( Window.BoundsRect.TopLeft );
  R.BottomRight := Window.ClientToScreen( Window.BoundsRect.BottomRight );

  WorkArea := GetActiveWorkArea( Window );

  Left := R.Left + ( R.Right - R.Left - ChildWin.Width ) div 2;
  if Left < 0 then
    Left := 0;
  if Left + ChildWin.Width > WorkArea.Right then
    Left := WorkArea.Right - ChildWin.Width;

  Top := R.Top + ( R.Bottom - R.Top - ChildWin.Height ) div 2;
  if Top < 0 then
    Top := 0;
  if Top + ChildWin.Height > WorkArea.Bottom then
    Top := WorkArea.Bottom - ChildWin.Height;
end;



procedure CenterToForm( ChildWin: TWinControl; AForm: TCustomForm; var Left, Top: Integer );
var
  WorkArea: TRect;
  R: TRect;
begin
  R := AForm.BoundsRect;

  WorkArea := GetActiveWorkArea( AForm );
  Left := R.Left + ( R.Right - R.Left - ChildWin.Width ) div 2;
  if Left < 0 then
    Left := 0;
  if Left + ChildWin.Width > WorkArea.Right then
    Left := WorkArea.Right - ChildWin.Width;

  Top := R.Top + ( R.Bottom - R.Top - ChildWin.Height ) div 2;
  if Top < 0 then
    Top := 0;
  if Top + ChildWin.Height > WorkArea.Bottom then
    Top := WorkArea.Bottom - ChildWin.Height;
end;


procedure CenterToMDIChild( ChildWin: TWinControl; AForm: TForm; var Left, Top: Integer );
var
  DeskRect: TRect;
  R: TRect;
  P: TPoint;
begin
  P := AForm.BoundsRect.TopLeft;
  ClientToScreen( Application.MainForm.ClientHandle, P );
  R.TopLeft := P;

  P := AForm.BoundsRect.BottomRight;
  ClientToScreen( Application.MainForm.ClientHandle, P );
  R.BottomRight := P;

  DeskRect := GetDesktopClientRect;
  Left := R.Left + ( R.Right - R.Left - ChildWin.Width ) div 2;
  if Left < 0 then
    Left := 0;
  if Left + ChildWin.Width > DeskRect.Right then
    Left := DeskRect.Right - ChildWin.Width;

  Top := R.Top + ( R.Bottom - R.Top - ChildWin.Height ) div 2;
  if Top < 0 then
    Top := 0;
  if Top + ChildWin.Height > DeskRect.Bottom then
    Top := DeskRect.Bottom - ChildWin.Height;
end;

{================================}
{== TRzDialogComponent Methods ==}
{================================}

constructor TRzDialogComponent.Create( AOwner: TComponent );
begin
  inherited;

  FFont := TFont.Create;
  if Owner is TForm then                               { If Owner is a form... }
    FFont.Assign( TForm( Owner ).Font )       { Use the form's font by default }
  else
  begin
    FFont := TFont.Create;
    FFont.Name := 'Tahoma';
    FFont.Size := 8;
    FFont.Style := [];
  end;
  FOriginLeft := 100;
  FOriginTop := 100;
  FHeight := 300;
  FWidth := 350;
  FBorderStyle := bsSizeable;
  FHelpContext := 0;

  FCenterToParent := False;
  FPosition := poScreenCenter;

  FFrameColor := clBtnShadow;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;

  FCaptionOK := SOKButton;
  FCaptionCancel := SCancelButton;
  FCaptionHelp := SHelpButton;
end;


destructor TRzDialogComponent.Destroy;
begin
  FFont.Free;
  inherited;
end;


procedure TRzDialogComponent.SetFont( Value: TFont );
begin
  FFont.Assign( Value );
end;


procedure TRzDialogComponent.SetCenterToParent( Value: Boolean );
begin
  if FCenterToParent <> Value then
  begin
    FCenterToParent := Value;
    if FCenterToParent then
      FPosition := poOwnerFormCenter
    else
      FPosition := poScreenCenter;
  end;
end;


procedure TRzDialogComponent.CenterForm( Dlg: TForm );
begin
  Dlg.Position := FPosition;
end;



{================================}
{== TRzFrameController Methods ==}
{================================}

constructor TRzFrameController.Create( AOwner: TComponent );
begin
  inherited;

  FUpdateCount := 0;

  FColor := clWindow;
  FParentColor := False;
  FFlatButtonColor := clBtnFace;
  FFlatButtons := True;
  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FReadOnlyColorOnFocus := False;
  FFocusColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
end;


procedure TRzFrameController.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzFrameController.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property
    // and the FrameStyle property. These may be overridden when the rest of the
    // stream is read in. However, we need to re-init them here because the
    // default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzFrameController.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzFrameController.Loaded;
begin
  inherited;
  UpdateControls;
end;


destructor TRzFrameController.Destroy;
begin
  if FFrameList <> nil then
  begin
    FFrameList.Free;
    FFrameList := nil;
  end;

  inherited;
end;


procedure TRzFrameController.Assign( Source: TPersistent );
begin
  if Source is TRzFrameController then
  begin
    BeginUpdate;
    try
      FColor := TRzFrameController( Source ).Color;
      FDisabledColor := TRzFrameController( Source ).DisabledColor;
      FReadOnlyColor := TRzFrameController( Source ).ReadOnlyColor;
      FReadOnlyColorOnFocus := TRzFrameController( Source ).ReadOnlyColorOnFocus;
      FFlatButtonColor := TRzFrameController( Source ).FlatButtonColor;
      FFlatButtons := TRzFrameController( Source ).FlatButtons;
      FFocusColor := TRzFrameController( Source ).FocusColor;
      FFrameColor := TRzFrameController( Source ).FrameColor;
      FFrameHotColor := TRzFrameController( Source ).FrameHotColor;
      FFrameHotStyle := TRzFrameController( Source ).FrameHotStyle;
      FFrameHotTrack := TRzFrameController( Source ).FrameHotTrack;
      FFrameSides := TRzFrameController( Source ).FrameSides;
      FFrameStyle := TRzFrameController( Source ).FrameStyle;
      FFrameVisible := TRzFrameController( Source ).FrameVisible;
      FFramingPreference := TRzFrameController( Source ).FramingPreference;
      FParentColor := TRzFrameController( Source ).ParentColor;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;


procedure TRzFrameController.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FRegIniFile ) then
    FRegIniFile := nil;
end;


procedure TRzFrameController.AddControl( C: TComponent );
begin
  if not Assigned( FFrameList ) then
    FFrameList := TList.Create;

  if FFrameList.IndexOf( C ) < 0 then
  begin
    FFrameList.Add( C );
    UpdateControlFrame( C, fcpAll );
  end;
end;


procedure TRzFrameController.RemoveControl( C: TComponent );
begin
  if FFrameList <> nil then
  begin
    FFrameList.Remove( C );
    if FFrameList.Count = 0 then
    begin
      FFrameList.Free;
      FFrameList := nil;
    end;
  end;
end;


procedure TRzFrameController.BeginUpdate;
begin
  Inc( FUpdateCount );
end;


procedure TRzFrameController.EndUpdate;
begin
  Dec( FUpdateCount );
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    UpdateFrames( fcpAll );
  end;
end;


procedure TRzFrameController.UpdateControls;
begin
  UpdateFrames( fcpAll );
end;


function TRzFrameController.GetNotifications( C: TComponent ): TRzFrameControllerNotifications;
var
  NotificationsSet: Cardinal;
begin
  if IsPublishedProp( C, 'FrameControllerNotifications' ) then
  begin
    NotificationsSet := GetOrdProp( C, 'FrameControllerNotifications' );

    Result := [];
    if ( NotificationsSet and fcpColorBit ) = fcpColorBit then
      Result := Result + [ fcpColor ];
    if ( NotificationsSet and fcpFocusColorBit ) = fcpFocusColorBit then
      Result := Result + [ fcpFocusColor ];
    if ( NotificationsSet and fcpDisabledColorBit ) = fcpDisabledColorBit then
      Result := Result + [ fcpDisabledColor ];
    if ( NotificationsSet and fcpReadOnlyColorBit ) = fcpReadOnlyColorBit then
      Result := Result + [ fcpReadOnlyColor ];
    if ( NotificationsSet and fcpReadOnlyColorOnFocusBit ) = fcpReadOnlyColorOnFocusBit then
      Result := Result + [ fcpReadOnlyColorOnFocus ];
    if ( NotificationsSet and fcpParentColorBit ) = fcpParentColorBit then
      Result := Result + [ fcpParentColor ];
    if ( NotificationsSet and fcpFlatButtonColorBit ) = fcpFlatButtonColorBit then
      Result := Result + [ fcpFlatButtonColor ];
    if ( NotificationsSet and fcpFlatButtonsBit ) = fcpFlatButtonsBit then
      Result := Result + [ fcpFlatButtons ];
    if ( NotificationsSet and fcpFrameColorBit ) = fcpFrameColorBit then
      Result := Result + [ fcpFrameColor ];
    if ( NotificationsSet and fcpFrameHotColorBit ) = fcpFrameHotColorBit then
      Result := Result + [ fcpFrameHotColor ];
    if ( NotificationsSet and fcpFrameHotTrackBit ) = fcpFrameHotTrackBit then
      Result := Result + [ fcpFrameHotTrack ];
    if ( NotificationsSet and fcpFrameHotStyleBit ) = fcpFrameHotStyleBit then
      Result := Result + [ fcpFrameHotStyle ];
    if ( NotificationsSet and fcpFrameSidesBit ) = fcpFrameSidesBit then
      Result := Result + [ fcpFrameSides ];
    if ( NotificationsSet and fcpFrameStyleBit ) = fcpFrameStyleBit then
      Result := Result + [ fcpFrameStyle ];
    if ( NotificationsSet and fcpFrameVisibleBit ) = fcpFrameVisibleBit then
      Result := Result + [ fcpFrameVisible ];
    if ( NotificationsSet and fcpFramingPreferenceBit ) = fcpFramingPreferenceBit then
      Result := Result + [ fcpFramingPreference ];
  end
  else
    Result := fccAll;
end;


procedure TRzFrameController.UpdateControlFrame( C: TComponent;
                                                 FrameProperty: TRzFrameControllerProperty );
var
  Ref: IRzCustomFramingNotification;
  Notifications: TRzFrameControllerNotifications;

  procedure SetBooleanProp( C: TComponent; const PropName: string; Value: Boolean );
  begin
    if C <> nil then
    begin
      if IsPublishedProp( C, PropName ) then
        SetOrdProp( C, PropName, Ord( Value ) );
    end;
  end;

  procedure SetStyleProp( C: TComponent; const PropName: string; Value: TFrameStyle );
  begin
    if C <> nil then
    begin
      if IsPublishedProp( C, PropName ) then
        SetOrdProp( C, PropName, Ord( Value ) );
    end;
  end;

  procedure SetPreferenceProp( C: TComponent; const PropName: string; Value: TFramingPreference );
  begin
    if C <> nil then
    begin
      if IsPublishedProp( C, PropName ) then
        SetOrdProp( C, PropName, Ord( Value ) );
    end;
  end;

  procedure SetColorProp( C: TComponent; const PropName: string; Value: TColor );
  begin
    if C <> nil then
    begin
      if IsPublishedProp( C, PropName ) then
        SetOrdProp( C, PropName, Value );
    end;
  end;

  procedure SetFrameSidesProp( C: TComponent );
  begin
    if C <> nil then
    begin
      if IsPublishedProp( C, 'FrameSides' ) then
        SetSetProp( C, 'FrameSides', GetSetProp( Self, 'FrameSides' ) );
    end;
  end;

begin {= UpdateControlFrame =}
  if not ( C is TControl ) then
    Exit;

  Notifications := GetNotifications( C );

  if Supports( C, IRzCustomFramingNotification ) then
  begin
    Ref := C as IRzCustomFramingNotification;
    Ref.CustomFramingChanged;
  end
  else // Use RTTI to update Custom Framing properties
  begin
    case FrameProperty of
      fcpAll:
      begin
        if fcpParentColor in Notifications then
          SetBooleanProp( C, 'ParentColor', FParentColor );

        if not FParentColor then
        begin
          if fcpColor in Notifications then
            SetColorProp( C, 'Color', FColor );
        end;

        if fcpDisabledColor in Notifications then
          SetColorProp( C, 'DisabledColor', FDisabledColor );
        if fcpReadOnlyColor in Notifications then
          SetColorProp( C, 'ReadOnlyColor', FReadOnlyColor );
        if fcpReadOnlyColorOnFocus in Notifications then
          SetBooleanProp( C, 'ReadOnlyColorOnFocus', FReadOnlyColorOnFocus );
        if fcpFocusColor in Notifications then
          SetColorProp( C, 'FocusColor', FFocusColor );
        if fcpFlatButtonColor in Notifications then
          SetColorProp( C, 'FlatButtonColor', FFlatButtonColor );
        if fcpFlatButtons in Notifications then
          SetBooleanProp( C, 'FlatButtons', FFlatButtons );
        if fcpFrameColor in Notifications then
          SetColorProp( C, 'FrameColor', FFrameColor );
        if fcpFrameHotColor in Notifications then
          SetColorProp( C, 'FrameHotColor', FFrameHotColor );
        if fcpFrameHotTrack in Notifications then
          SetBooleanProp( C, 'FrameHotTrack', FFrameHotTrack );
        if fcpFrameHotStyle in Notifications then
          SetStyleProp( C, 'FrameHotStyle', FFrameHotStyle );
        if fcpFrameSides in Notifications then
          SetFrameSidesProp( C );
        if fcpFrameStyle in Notifications then
          SetStyleProp( C, 'FrameStyle', FFrameStyle );
        if fcpFrameVisible in Notifications then
          SetBooleanProp( C, 'FrameVisible', FFrameVisible );
        if fcpFramingPreference in Notifications then
          SetPreferenceProp( C, 'FramingPreference', FFramingPreference );
      end;

      fcpColor:
      begin
        if fcpColor in Notifications then
          SetColorProp( C, 'Color', FColor );
      end;

      fcpFocusColor:
      begin
        if fcpFocusColor in Notifications then
          SetColorProp( C, 'FocusColor', FFocusColor );
      end;

      fcpDisabledColor:
      begin
        if fcpDisabledColor in Notifications then
          SetColorProp( C, 'DisabledColor', FDisabledColor );
      end;

      fcpReadOnlyColor:
      begin
        if fcpReadOnlyColor in Notifications then
          SetColorProp( C, 'ReadOnlyColor', FReadOnlyColor );
      end;

      fcpReadOnlyColorOnFocus:
      begin
        if fcpReadOnlyColoronFocus in Notifications then
          SetBooleanProp( C, 'ReadOnlyColorOnFocus', FReadOnlyColorOnFocus );
      end;

      fcpParentColor:
      begin
        if fcpParentColor in Notifications then
          SetBooleanProp( C, 'ParentColor', FParentColor );
      end;

      fcpFlatButtonColor:
      begin
        if fcpFlatButtonColor in Notifications then
          SetColorProp( C, 'FlatButtonColor', FFlatButtonColor );
      end;

      fcpFlatButtons:
      begin
        if fcpFlatButtons in Notifications then
          SetBooleanProp( C, 'FlatButtons', FFlatButtons );
      end;

      fcpFrameColor:
      begin
        if fcpFrameColor in Notifications then
          SetColorProp( C, 'FrameColor', FFrameColor );
      end;

      fcpFrameHotColor:
      begin
        if fcpFrameHotColor in Notifications then
          SetColorProp( C, 'FrameHotColor', FFrameHotColor );
      end;

      fcpFrameHotTrack:
      begin
        if fcpFrameHotTrack in Notifications then
          SetBooleanProp( C, 'FrameHotTrack', FFrameHotTrack );
      end;

      fcpFrameHotStyle:
      begin
        if fcpFrameHotStyle in Notifications then
          SetStyleProp( C, 'FrameHotStyle', FFrameHotStyle );
      end;

      fcpFrameSides:
      begin
        if fcpFrameSides in Notifications then
          SetFrameSidesProp( C );
      end;

      fcpFrameStyle:
      begin
        if fcpFrameStyle in Notifications then
          SetStyleProp( C, 'FrameStyle', FFrameStyle );
      end;

      fcpFrameVisible:
      begin
        if fcpFrameVisible in Notifications then
          SetBooleanProp( C, 'FrameVisible', FFrameVisible );
      end;

      fcpFramingPreference:
      begin
        if fcpFramingPreference in Notifications then
          SetPreferenceProp( C, 'FramingPreference', FFramingPreference );
      end;
    end;
  end;
end; {= TRzFrameController.UpdateControlFrame =}


procedure TRzFrameController.UpdateFrames( FrameProperty: TRzFrameControllerProperty );
var
  I: Integer;
begin
  if FUpdateCount > 0 then
    Exit;

  if FFrameList <> nil then
  begin
    { For each component on the FFrameList ... }
    for I := 0 to FFrameList.Count - 1 do
    begin
      UpdateControlFrame( FFrameList[ I ], FrameProperty );
    end;
  end;
end;


procedure TRzFrameController.SetColor( Value: TColor );
begin
  if FColor <> Value then
  begin
    if FFocusColor = FColor then
      FFocusColor := Value;
    FColor := Value;
    FParentColor := False;
    UpdateFrames( fcpColor );
  end;
end;


function TRzFrameController.StoreColor: Boolean;
begin
  Result := not ParentColor;
end;


procedure TRzFrameController.SetParentColor( Value: Boolean );
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    UpdateFrames( fcpParentColor );
    if not FParentColor then
      UpdateFrames( fcpColor );
  end;
end;


procedure TRzFrameController.SetFlatButtonColor( Value: TColor );
begin
  if FFlatButtonColor <> Value then
  begin
    FFlatButtonColor := Value;
    UpdateFrames( fcpFlatButtonColor );
  end;
end;


procedure TRzFrameController.SetFlatButtons( Value: Boolean );
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    UpdateFrames( fcpFlatButtons );
  end;
end;


procedure TRzFrameController.SetDisabledColor( Value: TColor );
begin
  if FDisabledColor <> Value then
  begin
    FDisabledColor := Value;
    UpdateFrames( fcpDisabledColor );
  end;
end;


procedure TRzFrameController.SetReadOnlyColor( Value: TColor );
begin
  if FReadOnlyColor <> Value then
  begin
    FReadOnlyColor := Value;
    UpdateFrames( fcpReadOnlyColor );
  end;
end;


procedure TRzFrameController.SetReadOnlyColorOnFocus( Value: Boolean );
begin
  if FReadOnlyColorOnFocus <> Value then
  begin
    FReadOnlyColorOnFocus := Value;
    UpdateFrames( fcpReadOnlyColorOnFocus );
  end;
end;


procedure TRzFrameController.SetFocusColor( Value: TColor );
begin
  if FFocusColor <> Value then
  begin
    FFocusColor := Value;
    UpdateFrames( fcpFocusColor );
  end;
end;


procedure TRzFrameController.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    UpdateFrames( fcpFrameColor );
  end;
end;


procedure TRzFrameController.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    UpdateFrames( fcpFrameHotColor );
  end;
end;


procedure TRzFrameController.SetFrameHotTrack( Value: Boolean );
begin
  if FFrameHotTrack <> Value then
  begin
    FFrameHotTrack := Value;
    if FFrameHotTrack then
    begin
      FrameVisible := True;
      if not ( csLoading in ComponentState ) then
        FFrameSides := sdAllSides;
    end;
    UpdateFrames( fcpFrameHotTrack );
  end;
end;


procedure TRzFrameController.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    UpdateFrames( fcpFrameHotStyle );
  end;
end;


procedure TRzFrameController.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    UpdateFrames( fcpFrameSides );
  end;
end;


procedure TRzFrameController.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    UpdateFrames( fcpFrameStyle );
  end;
end;


procedure TRzFrameController.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    UpdateFrames( fcpFrameVisible );
  end;
end;


procedure TRzFrameController.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    UpdateFrames( fcpFramingPreference );
  end;
end;


procedure TRzFrameController.Load( const Section: string );
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotLoadCustomFraming );


  BeginUpdate;
  try
    FColor := FRegIniFile.ReadInteger( Section, 'Color', clWindow );
    FFlatButtonColor := FRegIniFile.ReadInteger( Section, 'FlatButtonColor', clBtnFace );
    FFlatButtons := FRegIniFile.ReadBool( Section, 'FlatButtons', True );
    FDisabledColor := FRegIniFile.ReadInteger( Section, 'DisabledColor', clWindow );
    FReadOnlyColor := FRegIniFile.ReadInteger( Section, 'ReadOnlyColor', clInfoBk );
    FReadOnlyColorOnFocus := FRegIniFile.ReadBool( Section, 'ReadOnlyColorOnFocus', False );
    FFocusColor := FRegIniFile.ReadInteger( Section, 'FocusColor', clWindow );
    FFrameColor := FRegIniFile.ReadInteger( Section, 'FrameColor', clBtnShadow );
    FFrameHotColor := FRegIniFile.ReadInteger( Section, 'FrameHotColor', clBtnShadow );
    FFrameHotStyle := TFrameStyle( FRegIniFile.ReadInteger( Section, 'FrameHotStyle', Ord( fsFlatBold ) ) );
    FFrameHotTrack := FRegIniFile.ReadBool( Section, 'FrameHotTrack', False );

    FFrameSides := [];
    if FRegIniFile.ReadBool( Section, 'FrameSides_Left', True ) then
      FFrameSides := FFrameSides + [ sdLeft ];
    if FRegIniFile.ReadBool( Section, 'FrameSides_Top', True ) then
      FFrameSides := FFrameSides + [ sdTop ];
    if FRegIniFile.ReadBool( Section, 'FrameSides_Right', True ) then
      FFrameSides := FFrameSides + [ sdRight ];
    if FRegIniFile.ReadBool( Section, 'FrameSides_Bottom', True ) then
      FFrameSides := FFrameSides + [ sdBottom ];

    FFrameStyle := TFrameStyle( FRegIniFile.ReadInteger( Section, 'FrameStyle', Ord( fsFlat ) ) );
    FFrameVisible := FRegIniFile.ReadBool( Section, 'FrameVisible', False );
    FFramingPreference := TFramingPreference( FRegIniFile.ReadInteger( Section, 'FramingPreference', Ord( fpXPThemes ) ) );

  finally
    EndUpdate;
  end;
end;


procedure TRzFrameController.Save( const Section: string );
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotSaveCustomFraming );

  FRegIniFile.WriteInteger( Section, 'Color', FColor );
  FRegIniFile.WriteInteger( Section, 'FlatButtonColor', FFlatButtonColor );
  FRegIniFile.WriteBool( Section, 'FlatButtons', FFlatButtons );
  FRegIniFile.WriteInteger( Section, 'DisabledColor', FDisabledColor );
  FRegIniFile.WriteInteger( Section, 'ReadOnlyColor', FReadOnlyColor );
  FRegIniFile.WriteBool( Section, 'ReadOnlyColorOnFocus', FReadOnlyColorOnFocus );
  FRegIniFile.WriteInteger( Section, 'FocusColor', FFocusColor );
  FRegIniFile.WriteInteger( Section, 'FrameColor', FFrameColor );
  FRegIniFile.WriteInteger( Section, 'FrameHotColor', FFrameHotColor );
  FRegIniFile.WriteInteger( Section, 'FrameHotStyle', Ord( FFrameHotStyle ) );
  FRegIniFile.WriteBool( Section, 'FrameHotTrack', FFrameHotTrack );

  FRegIniFile.WriteBool( Section, 'FrameSides_Left', sdLeft in FFrameSides );
  FRegIniFile.WriteBool( Section, 'FrameSides_Top', sdTop in FFrameSides );
  FRegIniFile.WriteBool( Section, 'FrameSides_Right', sdRight in FFrameSides );
  FRegIniFile.WriteBool( Section, 'FrameSides_Bottom', sdBottom in FFrameSides );

  FRegIniFile.WriteInteger( Section, 'FrameStyle', Ord( FFrameStyle ) );
  FRegIniFile.WriteBool( Section, 'FrameVisible', FFrameVisible );
  FRegIniFile.WriteInteger( Section, 'FramingPreference', Ord( FFramingPreference ) );
end;


procedure TRzFrameController.SetRegIniFile( Value: TRzRegIniFile );
begin
  if FRegIniFile <> Value then
  begin
    FRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;



{===========================}
{== TRzRegIniFile Methods ==}
{===========================}

var
  ShellGetSpecialFolderPath: function( hwndOwner: HWND; lpszPath: PChar;
                                       nFolder: Integer; fCreate: BOOL): BOOL; stdcall;


function GetSpecialFolderPath( CSIDL: Integer ): string; overload;
var
  Path: array[ 0..MAX_PATH ] of Char;
  ShellModule: THandle;
begin
  Result := '';

  if ( Win32Platform = VER_PLATFORM_WIN32_NT ) and ( Win32MajorVersion >= 5 ) then
  begin
    // Only call SHGetSpecialFolderPath if running in Win2000 or higher

    ShellModule := LoadLibrary( 'Shell32' );
    try
      if ShellModule <> 0 then
      begin
        {$IFDEF UNICODE}
        @ShellGetSpecialFolderPath := GetProcAddress( ShellModule, 'SHGetSpecialFolderPathW' );
        {$ELSE}
        @ShellGetSpecialFolderPath := GetProcAddress( ShellModule, 'SHGetSpecialFolderPathA' );
        {$ENDIF}
        if Assigned( ShellGetSpecialFolderPath ) then
        begin
          if ShellGetSpecialFolderPath( 0, Path, CSIDL, False ) then
            Result := IncludeTrailingPathDelimiter( Path );
        end;
      end;
    finally
      if ShellModule <> 0 then
        FreeLibrary( ShellModule );
    end;
  end;
end;


function GetSpecialFolderPath( Folder: TRzSpecialFolder; SubDir: string ): string; overload;
begin
  case Folder of
    sfNone:
      Result := '';

    sfUserAppDataRoaming:
      Result := GetSpecialFolderPath( CSIDL_APPDATA );

    sfUserAppDataLocal:
      Result := GetSpecialFolderPath( CSIDL_LOCAL_APPDATA );

    sfUserDocuments:
      Result := GetSpecialFolderPath( CSIDL_PERSONAL );

    sfProgramData:
      Result := GetSpecialFolderPath( CSIDL_COMMON_APPDATA );
  end;

  if Result <> '' then
  begin
    Result := Result + SubDir;
    ForceDirectories( Result );
    Result := IncludeTrailingPathDelimiter( Result );
  end;
end;


const
  HKEYS: array[ TRzRegKey ] of HKEY = ( HKEY_CLASSES_ROOT,
                                        HKEY_CURRENT_USER,
                                        HKEY_LOCAL_MACHINE,
                                        HKEY_USERS,
                                        HKEY_PERFORMANCE_DATA,
                                        HKEY_CURRENT_CONFIG,
                                        HKEY_DYN_DATA );

  KeyAccess: array[ TRzRegAccessKey ] of LongWord = ( KEY_QUERY_VALUE,
                                                      KEY_SET_VALUE,
                                                      KEY_CREATE_SUB_KEY,
                                                      KEY_ENUMERATE_SUB_KEYS,
                                                      KEY_NOTIFY,
                                                      KEY_CREATE_LINK,
                                                      KEY_READ,
                                                      KEY_WRITE,
                                                      KEY_EXECUTE,
                                                      KEY_ALL_ACCESS );

constructor TRzRegIniFile.Create( AOwner: TComponent );
begin
  inherited;

  FPathType := ptIniFile;
  FSpecialFolder := sfNone;
  FFileEncoding := feDefault;

  FRegAccess := [ keyAllAccess ];
  FRegKey := hkeyCurrentUser;

  FRefreshStorage := True;
  FAutoUpdateIniFile := True;
end;


destructor TRzRegIniFile.Destroy;
begin
  if not FAutoUpdateIniFile and ( FIni <> nil ) then
    UpdateIniFile;

  if FIni <> nil then
    FreeAndNil( FIni );
  if FReg <> nil then
    FreeAndNil( FReg );
  inherited;
end;


procedure TRzRegIniFile.CheckAccess;
var
  S, P: string;
  Access: LongWord;
  K: TRzRegAccessKey;
begin
  if FRefreshStorage then
  begin
    if FIni <> nil then
      FreeAndNil( FIni );
    if FReg <> nil then
      FreeAndNil( FReg );

    S := FPath;
    case FPathType of
      ptIniFile:
      begin
        if FSpecialFolder <> sfNone then
        begin
          P := ExtractFilePath( S );
          P := GetSpecialFolderPath( FSpecialFolder, P );
          if P <> '' then
          begin
            if S = '' then
              S := P + ChangeFileExt( ExtractFileName( Application.ExeName ), '.ini' )
            else
              S := P + ChangeFileExt( ExtractFileName( S ), '.ini' );
          end;
        end;

        if S = '' then
          S := ChangeFileExt( Application.ExeName, '.ini' )
        else if ExtractFilePath( S ) = '' then
          S := ExtractFilePath( Application.Exename ) + ChangeFileExt( S, '.ini' );

        {$IFDEF UNICODE}
        case FFileEncoding of
          feDefault:
            FIni := TMemIniFile.Create( S );
          feUTF8:
            FIni := TMemIniFile.Create( S, TEncoding.UTF8 );
          feUnicode:
            FIni := TMemIniFile.Create( S, TEncoding.Unicode );
        end;
        {$ELSE}
        FIni := TMemIniFile.Create( S );
        {$ENDIF}
      end;

      ptRegistry:
      begin
        if S = '' then
        begin
          if Application.Title <> '' then
            S := '\Software\Temp\' + Application.Title
          else
            S := '\Software\Temp\' + ChangeFileExt( ExtractFileName( Application.ExeName ), '' );
        end;

        Access := 0;
        for K := keyQueryValue to keyAllAccess do
        begin
          if K in FRegAccess then
            Access := Access or KeyAccess[ K ];
        end;
        FReg := TRegistryIniFile.Create( '', Access );
        FReg.RegIniFile.RootKey := HKEYS[ FRegKey ];
        FReg.RegIniFile.OpenKey( S, True );
      end;
    end;

    FPath := S;
  end;
  FRefreshStorage := False;
end;


function TRzRegIniFile.SectionExists( const Section: string ): Boolean;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.SectionExists( Section )
  else
    Result := FReg.SectionExists( Section );
end;


function TRzRegIniFile.ValueExists( const Section, Name: string ): Boolean;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ValueExists( Section, Name )
  else
    Result := FReg.ValueExists( Section, Name );
end;


function TRzRegIniFile.ReadBool( const Section, Name: string; Default: Boolean ): Boolean;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadBool( Section, Name, Default )
  else
    Result := FReg.ReadBool( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteBool( const Section, Name: string; Value: Boolean );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteBool( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteBool( Section, Name, Value );
end;


function TRzRegIniFile.ReadDate( const Section, Name: string; Default: TDateTime ): TDateTime;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadDate( Section, Name, Default )
  else
    Result := FReg.ReadDate( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteDate( const Section, Name: string; Value: TDateTime );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteDate( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteDate( Section, Name, Value );
end;


function TRzRegIniFile.ReadDateTime( const Section, Name: string; Default: TDateTime ): TDateTime;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadDateTime( Section, Name, Default )
  else
    Result := FReg.ReadDateTime( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteDateTime( const Section, Name: string; Value: TDateTime );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteDateTime( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteDateTime( Section, Name, Value );
end;


function TRzRegIniFile.ReadFloat( const Section, Name: string; Default: Double ): Double;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadFloat( Section, Name, Default )
  else
    Result := FReg.ReadFloat( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteFloat( const Section, Name: string; Value: Double );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteFloat( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteFloat( Section, Name, Value );
end;


function TRzRegIniFile.ReadInteger( const Section, Name: string; Default: Longint ): Longint;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadInteger( Section, Name, Default )
  else
    Result := FReg.ReadInteger( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteInteger( const Section, Name: string; Value: Longint );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteInteger( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteInteger( Section, Name, Value );
end;


function TRzRegIniFile.ReadString( const Section, Name, Default: string ): string;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadString( Section, Name, Default )
  else
    Result := FReg.ReadString( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteString( const Section, Name, Value: string );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteString( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteString( Section, Name, Value );
end;


function TRzRegIniFile.ReadTime( const Section, Name: string; Default: TDateTime ): TDateTime;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadTime( Section, Name, Default )
  else
    Result := FReg.ReadTime( Section, Name, Default );
end;


procedure TRzRegIniFile.WriteTime( const Section, Name: string; Value: TDateTime );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteTime( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteTime( Section, Name, Value );
end;


function TRzRegIniFile.ReadBinaryStream( const Section, Name: string; Value: TStream ): Integer;
begin
  CheckAccess;
  if FPathType = ptIniFile then
    Result := FIni.ReadBinaryStream( Section, Name, Value )
  else
    Result := FReg.ReadBinaryStream( Section, Name, Value );
end;


procedure TRzRegIniFile.WriteBinaryStream( const Section, Name: string; Value: TStream );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.WriteBinaryStream( Section, Name, Value );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.WriteBinaryStream( Section, Name, Value );
end;


procedure TRzRegIniFile.ReadSection( const Section: string; Strings: TStrings );
begin
  CheckAccess;
  if FPathType = ptIniFile then
    FIni.ReadSection( Section, Strings )
  else
    FReg.ReadSection( Section, Strings );
end;


procedure TRzRegIniFile.ReadSections( Strings: TStrings );
begin
  CheckAccess;
  if FPathType = ptIniFile then
    FIni.ReadSections( Strings )
  else
    FReg.ReadSections( Strings );
end;


procedure TRzRegIniFile.ReadSectionValues( const Section: string; Strings: TStrings );
var
  Reg: TRegistry;
  FullKey: string;

  procedure ProcessValueName( const ValueName: string );
  var
    I, N, BinSize: Integer;
    S, HexStr, FmtStr: string;
    BinData, HexData: Pointer;
    P: PChar;
  begin
    FmtStr := '%s=%s';
    if ValueName = '' then
      S := Format( FmtStr, [ 'Default', Reg.ReadString( ValueName ) ] )
    else
    begin
      case Reg.GetDataType( ValueName ) of
        rdUnknown:
          S := Format( FmtStr, [ ValueName, 'Unknown Data Type' ] );

        rdString, rdExpandString:
        begin
          S := Format( FmtStr, [ ValueName, Reg.ReadString( ValueName ) ] );
        end;

        rdInteger:
        begin
          N := Reg.ReadInteger( ValueName );
          FmtStr := '%s=%d';
          S := Format( FmtStr, [ ValueName, N ] );
        end;

        rdBinary:
        begin
          BinSize := Reg.GetDataSize( ValueName );
          GetMem( BinData, BinSize );
          GetMem( HexData, BinSize * 2 );
          try
            Reg.ReadBinaryData( ValueName, BinData^, BinSize  );
            BinToHex( BinData, PChar( HexData ), BinSize );
            P := HexData;
            HexStr := '';
            I := 0;
            while I < BinSize do
            begin
              HexStr := HexStr + P[ I * 2 ] + P[ I * 2 + 1 ] + ' ';
              Inc( I );
            end;

            S := Format( FmtStr, [ ValueName, HexStr ] );
          finally
            FreeMem( BinData, BinSize );
            FreeMem( HexData, BinSize * 2 );
          end;
        end;
      end;
    end;
    Strings.Add( S );
  end; {= ProcessValueName =}


  procedure ProcessKey( const Key: string );
  var
    I: Integer;
    ValueNames: TStringList;
    RegKeyInfo: TRegKeyInfo;
  begin
    if Reg.OpenKey( Key, False ) then
    begin
      ValueNames := TStringList.Create;
      ValueNames.Sorted := True;
      try
        Reg.GetKeyInfo( RegKeyInfo );

        Reg.GetValueNames( ValueNames );
        for I := 0 to ValueNames.Count - 1 do
          ProcessValueName( ValueNames[ I ] );
      finally
        ValueNames.Free;
      end;
    end;
    Reg.CloseKey;
  end; {= ProcessKey =}


begin {= TRzRegIniFile.ReadSectionValues =}
  CheckAccess;
  if FPathType = ptIniFile then
    FIni.ReadSectionValues( Section, Strings )
  else
  begin
    // Cannot use FReg.ReadSectionValues( Section, Strings ) b/c TRegistryIniFile.ReadSectionValues simply uses
    // ReadString to read each value. This is fine for an INI file, but for the Registry, if a value is not a string
    // value (e.g. a DWORD), then an exception is raised.

    Reg := TRegistry.Create;
    Strings.BeginUpdate;
    Strings.Clear;
    try
      Reg.RootKey := HKEYS[ FRegKey ];

      if LastChar( FPath ) = '\' then
        FullKey := FPath + Section
      else
        FullKey := FPath + '\' + Section;
      if Reg.OpenKey( FullKey, False ) then
      begin
        Reg.CloseKey;
        ProcessKey( FullKey );
      end;
    finally
      Strings.EndUpdate;
      Reg.Free;
    end;
  end;
end; {= TRzRegIniFile.ReadSectionValues =}


procedure TRzRegIniFile.EraseSection( const Section: string );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.EraseSection( Section );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.EraseSection( Section );
end;


procedure TRzRegIniFile.DeleteKey( const Section, Name: string );
begin
  CheckAccess;
  if FPathType = ptIniFile then
  begin
    FIni.DeleteKey( Section, Name );
    if AutoUpdateIniFile then
      UpdateIniFile;
  end
  else
    FReg.DeleteKey( Section, Name );
end;


procedure TRzRegIniFile.SetPath( const Value: string );
begin
  if FPath <> Value then
  begin
    FPath := Value;
    FRefreshStorage := True;
  end;
end;


procedure TRzRegIniFile.SetPathType( Value: TRzPathType );
begin
  if FPathType <> Value then
  begin
    FPathType := Value;
    FRefreshStorage := True;
  end;
end;


procedure TRzRegIniFile.SetRegKey( Value: TRzRegKey );
begin
  if FRegKey <> Value then
  begin
    FRegKey := Value;
    FRefreshStorage := True;
  end;
end;


procedure TRzRegIniFile.SetRegAccess( Value: TRzRegAccess );
begin
  if FRegAccess <> Value then
  begin
    FRegAccess := Value;
    FRefreshStorage := True;
  end;
end;


procedure TRzRegIniFile.SetSpecialFolder( Value: TRzSpecialFolder );
begin
  if FSpecialFolder <> Value then
  begin
    FSpecialFolder := Value;
    FRefreshStorage := True;
  end;
end;


procedure TRzRegIniFile.SetFileEncoding( Value: TRzIniFileEncoding );
begin
  if FFileEncoding <> Value then
  begin
    FFileEncoding := Value;
    FRefreshStorage := True;
  end;
end;


procedure TRzRegIniFile.UpdateIniFile;
begin
  if FPathType = ptIniFile then
  begin
    {$IFDEF UNICODE}
    // Need to reset the FIni.Encoding property because during the load, the
    // TMemIniFile class may have reset it to a different format because it
    // misinterpreted the BOM.
    case FFileEncoding of
      feDefault:
        FIni.Encoding := TEncoding.Default;
      feUTF8:
        FIni.Encoding := TEncoding.UTF8;
      feUnicode:
        FIni.Encoding := TEncoding.Unicode;
    end;
    {$ENDIF}

    FIni.UpdateFile;
  end;
end;


{=============================}
{== TRzPropertyItem Methods ==}
{=============================}

constructor TRzPropertyItem.Create( Collection: TCollection );
begin
  inherited Create( Collection );
  FComponent := nil;;
  FPropertyName := '';
end;


procedure TRzPropertyItem.Assign( Source: TPersistent );
begin
  if Source is TRzPropertyItem then
  begin
    Component := TRzPropertyItem( Source ).Component;
    PropertyName := TRzPropertyItem( Source ).PropertyName;
  end
  else
    inherited Assign( Source );
end;


function TRzPropertyItem.GetDisplayName: string;
begin
  if ( FComponent <> nil ) and ( FComponent.Name <> '' ) then
    Result := FComponent.Name
  else
    Result := '<CompName>';

  if FPropertyName <> '' then
    Result := Result + '.' + FPropertyName
  else
    Result := Result + '.' + '<PropName>';
end;



{===================================}
{== TRzPropertyCollection Methods ==}
{===================================}

constructor TRzPropertyCollection.Create( Store: TRzPropertyStore );
begin
  // Inherited constructor is passed the "type" of the collection
  // item that the collection will manage.
  inherited Create( TRzPropertyItem );
  FStore := Store;
end;


function TRzPropertyCollection.Add: TRzPropertyItem;
begin
  Result := TRzPropertyItem( inherited Add );
end;


function TRzPropertyCollection.AddProperty( Component: TComponent;
                                            const PropertyName: string ): TRzPropertyItem;
begin
  Result := Add;
  Result.Component := Component;
  Result.PropertyName := PropertyName;
end;


function TRzPropertyCollection.GetItem( Index: Integer ): TRzPropertyItem;
begin
  Result := TRzPropertyItem( inherited GetItem( Index ) );
end;


procedure TRzPropertyCollection.SetItem( Index: Integer; Value: TRzPropertyItem );
begin
  inherited SetItem( Index, Value );
end;


function TRzPropertyCollection.GetOwner: TPersistent;
begin
  Result := FStore;
end;



{==============================}
{== TRzPropertyStore Methods ==}
{==============================}

constructor TRzPropertyStore.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  FProperties := TRzPropertyCollection.Create( Self );
end;


destructor TRzPropertyStore.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;


procedure TRzPropertyStore.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FRegIniFile ) then
    FRegIniFile := nil;
end;


procedure TRzPropertyStore.SaveProperties( const Section, ParentPropName: string;
                                           Obj: TObject );
var
  I: Integer;
  L: PPropList;
  D: PTypeData;
  PropInfo: PPropInfo;
  PropName, FullPropName: string;
begin
  D := GetTypeData( Obj.ClassInfo );

  GetMem( L, D^.PropCount * SizeOf( TPropInfo ) );
  try
    GetPropList( Obj.ClassInfo, tkAny, L );

    for I := 0 to D^.PropCount - 1 do
    begin
      PropName := string( L^[ I ]^.Name );
      PropInfo := GetPropInfo( Obj.ClassInfo, PropName );
      FullPropName := ParentPropName + '.' + PropName;

      // Check to make sure property is not write-only
      if PropInfo^.GetProc <> nil then
      begin
        if PropInfo^.PropType^.Kind in tkProperties then
        begin

          case PropInfo^.PropType^.Kind of
            tkInteger, tkChar, tkWChar:
              FRegIniFile.WriteInteger( Section, FullPropName, GetOrdProp( Obj, PropInfo ) );
            tkEnumeration:
              FRegIniFile.WriteString( Section, FullPropName, GetEnumProp( Obj, PropInfo ) );
            tkSet:
              FRegIniFile.WriteString( Section, FullPropName, GetSetProp( Obj, PropInfo ) );
            tkFloat:
              FRegIniFile.WriteFloat( Section, FullPropName, GetFloatProp( Obj, PropInfo ) );
            tkString, tkLString:
              FRegIniFile.WriteString( Section, FullPropName, GetStrProp( Obj, PropInfo ) );
            tkWString:
              FRegIniFile.WriteString( Section, FullPropName, GetWideStrProp( Obj, PropInfo ) );
            {$IFDEF UNICODE}
            tkUString:
              {$IFDEF RX102_OR_HIGHER}
              FRegIniFile.WriteString( Section, FullPropName, GetStrProp( Obj, PropInfo ) );
              {$ELSE}   
              FRegIniFile.WriteString( Section, FullPropName, GetUnicodeStrProp( Obj, PropInfo ) );
              {$ENDIF}
            {$ENDIF}
          end;
        end;
      end;
    end;

  finally
    FreeMem( L, D^.PropCount * SizeOf( TPropInfo ) );
  end;
end; {= TRzPropertyStore.SaveProperties =}


procedure TRzPropertyStore.SaveCollection( const Section, FullPropName: string;
                                           Collection: TCollection );
var
  I: Integer;
  S: string;
begin
  if Collection.Count > 0 then
  begin
    FRegIniFile.WriteInteger( Section, FullPropName + '_Count', Collection.Count );

    for I := 0 to Collection.Count - 1 do
    begin
      S := Format( '%s[%d]', [ FullPropName, I ] );
      SaveProperties( Section, S, Collection.Items[ I ] );
    end;
  end;
end;


procedure TRzPropertyStore.SaveStringList( const Section, FullPropName: string;
                                           StrList: TStrings );
var
  I: Integer;
  S: string;
begin
  if StrList.Count > 0 then
  begin
    FRegIniFile.WriteInteger( Section, FullPropName + '_Count', StrList.Count );

    for I := 0 to StrList.Count - 1 do
    begin
      S := Format( '%s[%d]', [ FullPropName, I ] );
      FRegIniFile.WriteString( Section, S, StrList[ I ] );
    end;
  end;
end;


procedure TRzPropertyStore.Save;
var
  I: Integer;
  Comp: TComponent;
  S, PropName, FullPropName: string;
  PropInfo: PPropInfo;
  Obj: TObject;
begin
  if FRegIniFile = nil then
  begin
    MessageDlg( sRzCannotSaveProperties, mtError, [ mbOK ], 0 );
    Exit;
  end;

  S := GetSection;

  for I := 0 to FProperties.Count - 1 do
  begin
    Comp := FProperties[ I ].Component;
    PropName := FProperties[ I ].PropertyName;
    if ( Comp <> nil ) and ( PropName <> '' ) and IsPublishedProp( Comp, PropName ) then
    begin
      FullPropName := Comp.Name + '.' + PropName;

      case PropType( Comp, PropName ) of
        tkInteger, tkChar, tkWChar:
          FRegIniFile.WriteInteger( S, FullPropName, GetOrdProp( Comp, PropName ) );

        tkClass:
        begin
          PropInfo := GetPropInfo( Comp.ClassInfo, PropName );
          Obj := TObject( GetOrdProp( Comp, PropInfo ) );
          if ( Obj <> nil ) and not ( Obj is TComponent ) then
          begin
            // Skip component reference properties
            if Obj is TCollection then
              SaveCollection( S, FullPropName, TCollection( Obj ) )
            else if Obj is TStrings then
              SaveStringList( S, FullPropName, TStrings( Obj ) )
            else
              SaveProperties( S, FullPropName, Obj );
          end;
        end;

        tkEnumeration:
          FRegIniFile.WriteString( S, FullPropName, GetEnumProp( Comp, PropName ) );

        tkSet:
          FRegIniFile.WriteString( S, FullPropName, GetSetProp( Comp, PropName ) );

        tkFloat:
          FRegIniFile.WriteFloat( S, FullPropName, GetFloatProp( Comp, PropName ) );

        {$IFDEF VCL170_OR_HIGHER}
        tkString, tkLString, tkWString, tkUString:
          FRegIniFile.WriteString( S, FullPropName, GetStrProp( Comp, PropName ) );

        {$ELSE}

        tkString, tkLString:
          FRegIniFile.WriteString( S, FullPropName, GetStrProp( Comp, PropName ) );

        tkWString:
          FRegIniFile.WriteString( S, FullPropName, GetWideStrProp( Comp, PropName ) );

        {$IFDEF UNICODE}
        tkUString:
          FRegIniFile.WriteString( S, FullPropName, GetUnicodeStrProp( Comp, PropName ) );
        {$ENDIF UNICODE}

        {$ENDIF}

      end;
    end;
  end;
end;



procedure TRzPropertyStore.LoadProperties( const Section, ParentPropName: string;
                                           Obj: TObject );
var
  I, IntValue: Integer;
  FloatValue: Extended;
  L: PPropList;
  D: PTypeData;
  PropInfo: PPropInfo;
  PropName, FullPropName, StrValue: string;
begin
  D := GetTypeData( Obj.ClassInfo );

  GetMem( L, D^.PropCount * SizeOf( TPropInfo ) );
  try
    GetPropList( Obj.ClassInfo, tkAny, L );

    for I := 0 to D^.PropCount - 1 do
    begin
      PropName := string( L^[ I ]^.Name );
      PropInfo := GetPropInfo( Obj.ClassInfo, PropName );
      FullPropName := ParentPropName + '.' + PropName;

      // Check to make sure property is not read-only
      if PropInfo^.SetProc <> nil then
      begin
        if PropInfo^.PropType^.Kind in tkProperties then
        begin
          if FRegIniFile.ValueExists( Section, FullPropName ) then
          begin
            case PropInfo^.PropType^.Kind of
              tkInteger, tkChar, tkWChar:
              begin
                IntValue := FRegIniFile.ReadInteger( Section, FullPropName, 0 );
                SetOrdProp( Obj, PropInfo, IntValue );
              end;

              tkEnumeration:
              begin
                StrValue := FRegIniFile.ReadString( Section, FullPropName, '' );
                SetEnumProp( Obj, PropInfo, StrValue );
              end;

              tkSet:
              begin
                StrValue := FRegIniFile.ReadString( Section, FullPropName, '' );
                SetSetProp( Obj, PropInfo, StrValue );
              end;

              tkFloat:
              begin
                FloatValue := FRegIniFile.ReadFloat( Section, FullPropName, 0.0 );
                SetFloatProp( Obj, PropInfo, FloatValue );
              end;

              tkString, tkLString:
              begin
                StrValue := FRegIniFile.ReadString( Section, FullPropName, '' );
                SetStrProp( Obj, PropInfo, StrValue );
              end;

              tkWString:
              begin
                StrValue := FRegIniFile.ReadString( Section, FullPropName, '' );
                SetWideStrProp( Obj, PropInfo, StrValue );
              end;

              {$IFDEF UNICODE}
              tkUString:
              begin
                StrValue := FRegIniFile.ReadString( Section, FullPropName, '' );
                {$IFDEF RX102_OR_HIGHER}
                SetStrProp( Obj, PropInfo, StrValue );
                {$ELSE}   
                SetUnicodeStrProp( Obj, PropInfo, StrValue );
                {$ENDIF}
              end;
              {$ENDIF}
            end;
          end;
        end;
      end;
    end;

  finally
    FreeMem( L, D^.PropCount * SizeOf( TPropInfo ) );
  end;
end; {= TRzPropertyStore.LoadProperties =}


procedure TRzPropertyStore.LoadCollection( const Section, FullPropName: string;
                                           Collection: TCollection );
var
  I, Count: Integer;
  S: string;
begin
  Count := FRegIniFile.ReadInteger( Section, FullPropName + '_Count', 0 );

  if Count > 0 then
  begin
    Collection.Clear; // Clear any existing items in the collection

    for I := 0 to Count - 1 do
    begin
      S := Format( '%s[%d]', [ FullPropName, I ] );
      LoadProperties( Section, S, Collection.Add );
    end;
  end;
end;


procedure TRzPropertyStore.LoadStringList( const Section, FullPropName: string;
                                           StrList: TStrings );
var
  I, Count: Integer;
  S: string;
begin
  Count := FRegIniFile.ReadInteger( Section, FullPropName + '_Count', 0 );

  if Count > 0 then
  begin
    StrList.Clear; // Clear any existing items in the collection

    for I := 0 to Count - 1 do
    begin
      S := Format( '%s[%d]', [ FullPropName, I ] );
      StrList.Add( FRegIniFile.ReadString( Section, S, '' ) );
    end;
  end;
end;


procedure TRzPropertyStore.Load;
var
  I, IntValue: Integer;
  FloatValue: Extended;
  Comp: TComponent;
  PropInfo: PPropInfo;
  Obj: TObject;
  S, PropName, FullPropName, StrValue: string;
begin
  if FRegIniFile = nil then
  begin
    MessageDlg( sRzCannotLoadProperties, mtError, [ mbOK ], 0 );
    Exit;
  end;

  S := GetSection;

  for I := 0 to FProperties.Count - 1 do
  begin
    Comp := FProperties[ I ].Component;
    PropName := FProperties[ I ].PropertyName;
    if ( Comp <> nil ) and ( PropName <> '' ) and IsPublishedProp( Comp, PropName ) then
    begin
      FullPropName := Comp.Name + '.' + PropName;

      if PropType( Comp, PropName ) = tkClass then
      begin
        PropInfo := GetPropInfo( Comp.ClassInfo, PropName );
        Obj := TObject( GetOrdProp( Comp, PropInfo ) );
        if ( Obj <> nil ) and not ( Obj is TComponent ) then
        begin
          // Skip component reference properties
          if Obj is TCollection then
            LoadCollection( S, FullPropName, TCollection( Obj ) )
          else if Obj is TStrings then
            LoadStringList( S, FullPropName, TStrings( Obj ) )
          else
            LoadProperties( S, FullPropName, Obj );
        end;
      end
      else if FRegIniFile.ValueExists( S, FullPropName ) then
      begin
        case PropType( Comp, PropName ) of
          tkInteger, tkChar, tkWChar:
          begin
            IntValue := FRegIniFile.ReadInteger( S, FullPropName, 0 );
            SetOrdProp( Comp, PropName, IntValue );
          end;

          tkEnumeration:
          begin
            StrValue := FRegIniFile.ReadString( S, FullPropName, '' );
            SetEnumProp( Comp, PropName, StrValue );
          end;

          tkSet:
          begin
            StrValue := FRegIniFile.ReadString( S, FullPropName, '' );
            SetSetProp( Comp, PropName, StrValue );
          end;

          tkFloat:
          begin
            FloatValue := FRegIniFile.ReadFloat( S, FullPropName, 0.0 );
            SetFloatProp( Comp, PropName, FloatValue );
          end;

          {$IFDEF VCL170_OR_HIGHER}

          tkString, tkLString, tkWString, tkUString:
          begin
            StrValue := FRegIniFile.ReadString( S, FullPropName, '' );
            SetStrProp( Comp, PropName, StrValue );
          end;

          {$ELSE}

          tkString, tkLString:
          begin
            StrValue := FRegIniFile.ReadString( S, FullPropName, '' );
            SetStrProp( Comp, PropName, StrValue );
          end;

          tkWString:
          begin
            StrValue := FRegIniFile.ReadString( S, FullPropName, '' );
            SetWideStrProp( Comp, PropName, StrValue );
          end;

          {$IFDEF UNICODE}
          tkUString:
          begin
            StrValue := FRegIniFile.ReadString( S, FullPropName, '' );
            SetUnicodeStrProp( Comp, PropName, StrValue );
          end;
          {$ENDIF UNICODE}

          {$ENDIF}
        end;
      end;
    end;
  end;
end;


function TRzPropertyStore.GetSection: string;
var
  F: TCustomForm;
begin
  if FSection <> '' then
    Result := FSection
  else
  begin
    if ( Owner <> nil ) and ( Owner is TCustomForm ) then
    begin
      F := TCustomForm( Owner );
      if F.Name <> '' then
        Result := F.Name + '_Properties'
      else
        Result := F.ClassName + '_Properties';
    end
    else
      Result := 'Properties';
  end;
end;


procedure TRzPropertyStore.AddProperty( Component: TComponent; const PropertyName: string );
begin
  FProperties.AddProperty( Component, PropertyName );
end;


procedure TRzPropertyStore.SetProperties( Value: TRzPropertyCollection );
begin
  FProperties.Assign( Value );
end;


procedure TRzPropertyStore.SetRegIniFile( Value: TRzRegIniFile );
begin
  if FRegIniFile <> Value then
  begin
    FRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


{=============================}
{== TRzCustomColors Methods ==}
{=============================}

constructor TRzCustomColors.Create( AOwner: TComponent );
begin
  inherited;
  FColors := TStringList.Create;
  InitColors;
end;


destructor TRzCustomColors.Destroy;
begin
  FColors.Free;
  inherited;
end;


procedure TRzCustomColors.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FRegIniFile ) then
    FRegIniFile := nil;
end;


procedure TRzCustomColors.InitColors;
begin
  FColors.Add( 'ColorA=FFFFFF' );
  FColors.Add( 'ColorB=FFFFFF' );
  FColors.Add( 'ColorC=FFFFFF' );
  FColors.Add( 'ColorD=FFFFFF' );
  FColors.Add( 'ColorE=FFFFFF' );
  FColors.Add( 'ColorF=FFFFFF' );
  FColors.Add( 'ColorG=FFFFFF' );
  FColors.Add( 'ColorH=FFFFFF' );
  FColors.Add( 'ColorI=FFFFFF' );
  FColors.Add( 'ColorJ=FFFFFF' );
  FColors.Add( 'ColorK=FFFFFF' );
  FColors.Add( 'ColorL=FFFFFF' );
  FColors.Add( 'ColorM=FFFFFF' );
  FColors.Add( 'ColorN=FFFFFF' );
  FColors.Add( 'ColorO=FFFFFF' );
  FColors.Add( 'ColorP=FFFFFF' );
end;


function TRzCustomColors.GetColorName( Index: Integer ): string;
begin
  Result := FColors.Names[ Index ];
end;


procedure TRzCustomColors.FixupColors;
var
  I: Integer;
  L: Longint;
  S, Ident: string;
begin
  for I := 0 to FColors.Count - 1 do
  begin
    Ident := GetColorName( I );

    L := StrToInt( '$' + FColors.Values[ Ident ] ) and $00FFFFFF;
    S := Format( '%.6x', [ L ] );
    FColors.Values[ Ident ] := S;
  end;
end;


procedure TRzCustomColors.Load( const Section: string );
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotLoadCustomColors );

  FRegIniFile.ReadSectionValues( Section, FColors );
  FixupColors;
end;


procedure TRzCustomColors.Save( const Section: string );
var
  I: Integer;
  Ident: string;
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotSaveCustomColors );

  for I := 0 to FColors.Count - 1 do
  begin
    Ident := GetColorName( I );
    FRegIniFile.WriteString( Section, Ident, FColors.Values[ Ident ] );
  end;
end;


procedure TRzCustomColors.SetColors( Value: TStrings );
begin
  FColors.Assign( Value );
  FixupColors;
end;


procedure TRzCustomColors.SetRegIniFile( Value: TRzRegIniFile );
begin
  if FRegIniFile <> Value then
  begin
    FRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


{===============================}
{== TRzMenuController Methods ==}
{===============================}

constructor TRzMenuController.Create( AOwner: TComponent );
begin
  inherited;
  FMenuItemList := TList.Create;

  FGradientColorStyle := gcsSystem;

  FSelectionColorStart := clBtnFace;
  FSelectionColorStop := clBtnShadow;
  FSelectionFrameColor := cl3DDkShadow;

  FIconBarColorStart := clWhite;
  FIconBarColorStop := clBtnFace;

  FMenuColor := clWindow;
  FMenuFontColor := clWindowText;

  FMenuFont := TFont.Create;
  if RunningAtLeast( winVista ) then
    FMenuFont.Name := 'Segoe UI'
  else
    FMenuFont.Name := 'Tahoma';
  FMenuFont.Style := [ ];
  FMenuFont.Size := 8;
  FMenuFont.Color := FMenuFontColor;
  FMenuFont.OnChange := MenuFontChangeHandler;
  FUseCustomMenuFont := False;
end;


destructor TRzMenuController.Destroy;
begin
  if not ( csDesigning in ComponentState ) then
    CleanupMenuItems;
  FMenuItemList.Free;
  FMenuFont.Free;
  inherited;
end;


procedure TRzMenuController.DefineProperties( Filer: TFiler );
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
end;


procedure TRzMenuController.ReadOldGradientColorStartProp( Reader: TReader );
begin
  if Reader.NextValue = vaIdent then
    SelectionColorStart := StringToColor( Reader.ReadIdent )
  else
    SelectionColorStart := Reader.ReadInteger;
end;

procedure TRzMenuController.ReadOldGradientColorStopProp( Reader: TReader );
begin
  if Reader.NextValue = vaIdent then
    SelectionColorStop := StringToColor( Reader.ReadIdent )
  else
    SelectionColorStop := Reader.ReadInteger;
end;

procedure TRzMenuController.ReadOldFrameColorProp( Reader: TReader );
begin
  if Reader.NextValue = vaIdent then
    SelectionFrameColor := StringToColor( Reader.ReadIdent )
  else
    SelectionFrameColor := Reader.ReadInteger;
end;


procedure TRzMenuController.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if csDesigning in ComponentState then
  begin
    if Operation = opInsert then
    begin
      if AComponent = Self then
      begin
        // When the controller is dropped onto a form, turn on OwnerDraw
        // property of any menus already on the form.
        SetupMenus;
      end
      else if AComponent is TMenu then
        TMenu( AComponent ).OwnerDraw := True;
    end;
  end
  else // at runtime
  begin
    if AComponent is TMenuItem then
    begin
      if Operation = opInsert then
        SetupMenuItem( TMenuItem( AComponent ) )
      else
        CleanupMenuItem( TMenuItem( AComponent ) );
    end;
  end;
end;


procedure TRzMenuController.Loaded;
begin
  inherited;

  if csDesigning in ComponentState then
    SetupMenus
  else
    SetupMenuItems;
end;


procedure TRzMenuController.SetupMenus;
var
  I: Integer;
begin
  for I := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[ I ] is TMenu then
      TMenu( Owner.Components[ I ] ).OwnerDraw := True;
  end;
end;


procedure TRzMenuController.SetupMenuItems;
var
  I: Integer;
begin
  for I := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[ I ] is TMenuItem then
      SetupMenuItem( TMenuItem( Owner.Components[ I ] ) );
  end;
end;


procedure TRzMenuController.SetupMenuItem( Item: TMenuItem );
begin
  if not Assigned( Item.OnAdvancedDrawItem ) then
  begin
    Item.OnAdvancedDrawItem := AdvancedDrawItemHandler;
    Item.OnMeasureItem := MeasureItemHandler;
    Item.FreeNotification( Self );
    FMenuItemList.Add( Item );
  end;
end;


procedure TRzMenuController.CleanupMenuItems;
var
  I: Integer;
begin
  for I := FMenuItemList.Count - 1 downto 0 do
    CleanupMenuItem( TMenuItem( FMenuItemList[ I ] ) );
end;


procedure TRzMenuController.CleanupMenuItem( Item: TMenuItem );
var
  Idx: Integer;
begin
  Idx := FMenuItemList.IndexOf( Item );
  if Idx <> -1 then
  begin
    Item.OnAdvancedDrawItem := nil;
    Item.OnMeasureItem := nil;
    FMenuItemList.Delete( Idx );
  end;
end;


function TRzMenuController.MenuIsRightToLeft( Item: TMenuItem ): Boolean;
var
  Menu: TMenu;
begin
  Result := False;
  Menu := Item.GetParentMenu;
  if ( Menu <> nil ) and ( Menu.IsRightToLeft ) then
    Result := True;
end;

procedure TRzMenuController.AdvancedDrawItemHandler( Sender: TObject; ACanvas: TCanvas;
                                                     ARect: TRect; State: TOwnerDrawState );
begin
  PaintMenuItem( Sender as TMenuItem, ACanvas, ARect, State );
end;


procedure TRzMenuController.MeasureItemHandler( Sender: TObject; ACanvas: TCanvas;
                                                var Width, Height: Integer );
begin
  MeasureMenuItem( Sender as TMenuItem, ACanvas, Width, Height );
end;


procedure TRzMenuController.PaintMenuItem( Item: TMenuItem; Canvas: TCanvas;
                                           Rect: TRect; State: TOwnerDrawState );
var
  R, GlyphRect, CaptionRect: TRect;
  TopLevel, RightToLeftMenu: Boolean;
  C, BackColor, FontColor, DisabledFontColor, LineColor: TColor;
  SelBarFrameColor, SelBarStartColor, SelBarStopColor, IcoStartColor, IcoStopColor: TColor;
  ImgList: TCustomImageList;
  Y: Integer;


  procedure PaintCaption( Canvas: TCanvas; CaptionRect: TRect; Caption: string;
                          TopLevel: Boolean );
  var
    Flags: Cardinal;
    S: string;
    R: TRect;
    OldStyle: TFontStyles;
  begin
    Flags := DT_VCENTER or DT_EXPANDTABS or DT_SINGLELINE;
    if TopLevel then
      Flags := Flags or DT_CENTER
    else
      if RightToLeftMenu then
        Flags := Flags or DT_RIGHT
      else
        Flags := Flags or DT_LEFT;

    if HideMenuPrefix and ( odNoAccel in State ) then
      Flags := Flags or DT_HIDEPREFIX;

    Canvas.Brush.Style := bsClear;

    OldStyle := Canvas.Font.Style;
    if Item.Default then
      Canvas.Font.Style := Canvas.Font.Style + [ fsBold ];

    if not Item.Enabled then
      Canvas.Font.Color := DisabledFontColor;

    if RightToLeftMenu then
      DrawString( Canvas, ' ' + Item.Caption, CaptionRect, Flags )
    else
      DrawString( Canvas, Item.Caption, CaptionRect, Flags );


    if ( Item.ShortCut <> 0) and not TopLevel then
    begin
      S := ShortCutToText( Item.ShortCut );
      if RightToLeftMenu then
      begin
        Flags := DT_VCENTER or DT_EXPANDTABS or DT_SINGLELINE;
        Flags := Flags or DT_LEFT;
        R := CaptionRect;
        Inc( R.Left, 5 );
        DrawString( Canvas, '  ' + S, R, Flags );
      end
     else
     begin
        Flags := Flags or DT_RIGHT;
        R := CaptionRect;
        Dec( R.Right, 10 );
        DrawString( Canvas, S, R, Flags );
      end;
    end;

    Canvas.Font.Style := OldStyle;
    Canvas.Brush.Style := bsSolid;
  end; {= PaintCaption =}


  procedure PaintCheckState( Item: TMenuItem; Canvas: TCanvas; GlyphRect: TRect;
                             Selected: Boolean );
  var
    R: TRect;
    Glyph, ColoredGlyph: TBitmap;
    OldFontColor: TColor;
  begin
    R := GlyphRect;
    InflateRect( R, -1, -1 );
    Dec( R.Right, 2 );
    R := DrawBox( Canvas, R, SelBarFrameColor );

    if Selected then
      Canvas.Brush.Color := SelBarFrameColor
    else
      Canvas.Brush.Color := SelBarStartColor;
    Canvas.FillRect( R );

    if ( Item.GetImageList = nil ) or ( Item.ImageIndex = -1 ) then
    begin
      Glyph := TBitmap.Create;
      ColoredGlyph := TBitmap.Create;
      try
        Glyph.Transparent := True;
        Glyph.Width := 16;
        Glyph.Height := 16;
        SetRect( R, 0, 0, 16, 16 );

        ColoredGlyph.Transparent := True;
        ColoredGlyph.Width := 16;
        ColoredGlyph.Height := 16;

        Glyph.Canvas.Font.Color := clYellow;

        if Item.RadioItem then
          DrawFrameControl( Glyph.Canvas.Handle, R, DFC_MENU, DFCS_MENUBULLET or DFCS_TRANSPARENT )
        else
          DrawFrameControl( Glyph.Canvas.Handle, R, DFC_MENU, DFCS_MENUCHECK or DFCS_TRANSPARENT );


        // Replace Black color of frame control symbol with FontColor
        ColoredGlyph.Canvas.Brush.Color := FontColor;
        ColoredGlyph.Canvas.BrushCopy( R, Glyph, R, clBlack );

        OldFontColor := Canvas.Font.Color;

        Canvas.Draw( GlyphRect.Left + ( GlyphRect.Right - GlyphRect.Left - Glyph.Width ) div 2,
                     GlyphRect.Top + ( GlyphRect.Bottom - GlyphRect.Top - Glyph.Height ) div 2, ColoredGlyph );

        Canvas.Font.Color := OldFontColor;
      finally
        Glyph.Free;
        ColoredGlyph.Free;
      end;
    end;
  end; {= PaintCheckState =}


begin {= PaintMenuItem =}
  TopLevel := Item.GetParentComponent is TMainMenu;
  ImgList := Item.GetImageList;
  RightToLeftMenu := MenuIsRightToLeft( Item );

  if UsingSystemStyle then
  begin
    if FGradientColorStyle <> gcsCustom then
    begin
      GetGradientSelectionColors( FGradientColorStyle, SelBarFrameColor, SelBarStartColor, SelBarStopColor );
      GetGradientPanelColors( FGradientColorStyle, IcoStartColor, IcoStopColor );
    end
    else
    begin
      SelBarFrameColor := FSelectionFrameColor;
      SelBarStartColor := FSelectionColorStart;
      SelBarStopColor := FSelectionColorStop;
      IcoStartColor := FIconBarColorStart;
      IcoStopColor := FIconBarColorStop;
    end;
    FontColor := FMenuFontColor;
    DisabledFontColor := clGrayText;
    BackColor := FMenuColor;
    LineColor := clBtnShadow;
  end
  else // VCL Styles
  begin
    SelBarFrameColor := ActiveStyleSystemColor( clHighlight );
    C := BlendColors( ActiveStyleSystemColor( clHighlight ), ActiveStyleSystemColor( clWindow ), 77 );
    SelBarStartColor := LighterColor( C, 20 );
    SelBarStopColor := C;
    IcoStopColor := LighterColor( ActiveStyleSystemColor( clBtnFace ), 30 );
    IcoStartColor := LighterColor( IcoStopColor, 30 );
    FontColor := ActiveStyleFontColor( sfMenuItemTextNormal );
    DisabledFontColor := ActiveStyleFontColor( sfMenuItemTextDisabled );
    BackColor := ActiveStyleSystemColor( clMenu );
    LineColor := DisabledFontColor;
  end;

  GlyphRect := Rect;
  if RightToLeftMenu then
  begin
    if ImgList <> nil then
      GlyphRect.Left := GlyphRect.Right - ImgList.Width - 8
    else
      GlyphRect.Left := GlyphRect.Right - 24;
    CaptionRect := Rect;
    if not TopLevel then
      CaptionRect.Right := CaptionRect.Right - ( Rect.Right - GlyphRect.Left );
  end
  else
  begin
    if ImgList <> nil then
      GlyphRect.Right := GlyphRect.Left + ImgList.Width + 8
    else
      GlyphRect.Right := GlyphRect.Left + 24;
    CaptionRect := Rect;
    if not TopLevel then
      CaptionRect.Left := GlyphRect.Right;
  end;

  if FUseCustomMenuFont and not TopLevel then
    Canvas.Font := FMenuFont
  else
    Canvas.Font := Screen.MenuFont;
  Canvas.Font.Color := FontColor;


  if TopLevel then
  begin
    // TopLevel block never gets called when VCL Styles are used
    if ( odSelected in State ) or ( odHotLight in State ) then
    begin
      R := Rect;
      InflateRect( R, -1, -1 );
      R := DrawBox( Canvas, R, SelBarFrameColor );
      if FullColorSupported then
      begin
        PaintGradient( Canvas, R, gdHorizontalEnd, SelBarStartColor, SelBarStopColor );
      end
      else
      begin
        Canvas.Brush.Color := SelBarStopColor;
        Canvas.FillRect( R );
      end;
    end
    else
    begin
      if UsingSystemStyle and FUseMenuColorForMainMenu then
        Canvas.Brush.Color := FMenuColor
      else if ActiveStyleServicesEnabled then
        Canvas.Brush.Color := clMenuBar
      else
        Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect( CaptionRect );
    end;

    if odInactive in State then
      Canvas.Font.Color := clGrayText;
  end
  else // not TopLevel
  begin
    if ( odSelected in State ) and ( Item.Enabled ) then
    begin
      R := DrawBox( Canvas, Rect, SelBarFrameColor );
      if FullColorSupported then
      begin
        PaintGradient( Canvas, R, gdHorizontalEnd, SelBarStartColor, SelBarStopColor );
      end
      else
      begin
        Canvas.Brush.Color := SelBarStopColor;
        Canvas.FillRect( R );
      end;

      if Item.Checked then
        PaintCheckState( Item, Canvas, GlyphRect, True );
    end
    else
    begin
      // Paint IconBar Background
      if FullColorSupported then
      begin
        if RightToLeftMenu then
          PaintGradient( Canvas, GlyphRect, gdVerticalEnd, IcoStopColor, IcoStartColor )
        else
          PaintGradient( Canvas, GlyphRect, gdVerticalEnd, IcoStartColor, IcoStopColor );
      end
      else
      begin
        Canvas.Brush.Color := IcoStopColor;
        Canvas.FillRect( GlyphRect );
      end;

      if Item.Checked then
        PaintCheckState( Item, Canvas, GlyphRect, False );

      Canvas.Brush.Color := BackColor;
      Canvas.FillRect( CaptionRect );
    end;

    // Adjust CaptionRect to Display Text
    Inc( CaptionRect.Left, 6 );

    if ( ImgList <> nil ) and ( Item.ImageIndex > -1 ) and ( Item.ImageIndex < ImgList.Count ) then
    begin
      Y := ( ( Rect.Bottom - Rect.Top ) - ( ImgList.Height ) ) div 2;
      ImgList.Draw( Canvas, GlyphRect.Left + 3, GlyphRect.Top + Y, Item.ImageIndex, Item.Enabled );
    end
    else if Item.Bitmap <> nil then
    begin
      Y := ( ( Rect.Bottom - Rect.Top ) - ( Item.Bitmap.Height ) ) div 2;
      Canvas.Draw( GlyphRect.Left + 3, GlyphRect.Top + Y, Item.Bitmap );
    end;
  end;

  if Item.Caption = cLineCaption then
  begin
    Canvas.Pen.Color := LineColor;
    Canvas.MoveTo( CaptionRect.Left, CaptionRect.Top + 1 );
    if RightToLeftMenu then
      Canvas.LineTo( CaptionRect.Right - Canvas.TextWidth( ' ' ), CaptionRect.Top + 1 )
    else
      Canvas.LineTo( CaptionRect.Right, CaptionRect.Top + 1 );
  end
  else
    PaintCaption( Canvas, CaptionRect, Item.Caption, TopLevel );
end; {= PaintMenuItem =}


procedure TRzMenuController.MeasureMenuItem( Item: TMenuItem; Canvas: TCanvas; var Width, Height: Integer );
var
  H, ImgWidth, ImgHeight: Integer;
  TopLevel: Boolean;
  ImgList: TCustomImageList;
begin
  ImgList := Item.GetImageList;
  TopLevel := Item.GetParentComponent is TMainMenu;

  if Item.Caption = cLineCaption then
    Height := 3
  else if not TopLevel then
  begin
    if FUseCustomMenuFont then
      Canvas.Font := FMenuFont
    else
      Canvas.Font := Screen.MenuFont;

    if ImgList <> nil then
    begin
      ImgWidth := ImgList.Width;
      ImgHeight := ImgList.Height;
    end
    else
    begin
      ImgWidth := 0;
      ImgHeight := 0;
    end;

    if Item.ShortCut <> 0 then
      Width := Canvas.TextWidth( Item.Caption + ShortCutToText( Item.ShortCut ) ) + ImgWidth + 20
    else
      Width := Canvas.TextWidth( Item.Caption ) + ImgWidth + 20;

    H := Canvas.TextHeight( 'Yy' ) + 6;
    if ImgList <> nil then
      Height := Max( H, ImgHeight + 6 )
    else
      Height := H;
  end;
end;


function TRzMenuController.HideMenuPrefix: Boolean;
var
  Reg: TRegistry;
  Count: Integer;
  MaskBuffer4: array[ 0..3 ] of Byte;
  MaskBuffer8: array[ 0..7 ] of Byte;
begin
  Result := False;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey( '\Control Panel\Desktop', False ) then
      begin
        if RunningAtLeast( winVista ) then
        begin
          Count := Reg.ReadBinaryData( 'UserPreferencesMask', MaskBuffer8, 8 );
          if Count = 8 then
            Result := ( MaskBuffer8[ 0 ] and $20 ) = $00;
        end
        else
        begin
          Count := Reg.ReadBinaryData( 'UserPreferencesMask', MaskBuffer4, 4 );
          if Count = 4 then
            Result := ( MaskBuffer4[ 0 ] and $20 ) = $00;
        end;
      end;
    finally
      Reg.Free;
    end;
  except
    Result := False;
  end;
end;


procedure TRzMenuController.SetMenuFontColor( Value: TColor );
begin
  if FMenuFontColor <> Value then
  begin
    FMenuFontColor := Value;
    FMenuFont.Color := Value;
  end;
end;


procedure TRzMenuController.SetMenuFont( Value: TFont );
begin
  FMenuFont.Assign( Value );
end;


procedure TRzMenuController.SetUseMenuColorForMainMenu( Value: Boolean );
begin
  if FUseMenuColorForMainMenu <> Value then
  begin
    FUseMenuColorForMainMenu := Value;
    UpdateMainMenuColor;
  end;
end;


procedure TRzMenuController.UpdateMainMenuColor;
var
  MenuInfo: TMenuInfo;
begin
  if ( Owner <> nil ) and ( Owner is TCustomForm ) then
  begin
    if TCustomForm( Owner ).Menu = nil then
      Exit;
  end;

  DeleteObject( FMainMenuBrushHandle );

  if FUseMenuColorForMainMenu then
    FMainMenuBrushHandle := CreateSolidBrush( ColorToRGB( FMenuColor ) )
  else
    FMainMenuBrushHandle := CreateSolidBrush( ColorToRGB( clBtnFace ) );


  FillChar( MenuInfo, SizeOf( MenuInfo ), 0 );
  MenuInfo.cbSize := SizeOf( MenuInfo );
  MenuInfo.hbrBack := FMainMenuBrushHandle;
  MenuInfo.fMask := MIM_BACKGROUND;

  SetMenuInfo( TCustomForm( Owner ).Menu.Handle, MenuInfo );
end;



procedure TRzMenuController.MenuFontChangeHandler( Sender: TObject );
begin
  FMenuFontColor := FMenuFont.Color;
end;


{== Windows API Support Functions ==}

function RunningUnder( Ver: TRzWindowsVersion ): Boolean;
begin
  case Ver of
    win95:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) and
                ( Win32MajorVersion <= 4 ) and ( Win32MinorVersion = 0 );

    winNT:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion <= 4 );

    win98:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) and
                ( Win32MajorVersion <= 4 ) and ( Win32MinorVersion = 10 );

    winMe:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) and
                ( Win32MajorVersion <= 4 ) and ( Win32MinorVersion = 90 );

    win2000:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 5 ) and ( Win32MinorVersion = 0 );

    winXP:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 5 ) and ( Win32MinorVersion = 1 );

    winServer2003:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 5 ) and ( Win32MinorVersion = 2 );

    winVista:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 6 ) and ( Win32MinorVersion = 0 );

    win7:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 6 ) and ( Win32MinorVersion = 1 );

    win8:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 6 ) and ( Win32MinorVersion >= 2 );

    win10:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion = 10 ) and ( Win32MinorVersion >= 0 );
  else
    Result := False;
  end;
end;


function RunningAtLeast( Ver: TRzWindowsVersion ): Boolean;
begin
  case Ver of
    win95:
      Result := ( ( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) or
                  ( Win32Platform = VER_PLATFORM_WIN32_NT ) )
                and
                ( Win32MajorVersion >= 4 ) and ( Win32MinorVersion >= 0 );

    winNT:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion >= 4 );

    win98:
      Result := ( ( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) or
                  ( Win32Platform = VER_PLATFORM_WIN32_NT ) )
                and
                ( Win32MajorVersion >= 4 ) and ( Win32MinorVersion >= 10 );

    winMe:
      Result := ( ( Win32Platform = VER_PLATFORM_WIN32_WINDOWS ) or
                  ( Win32Platform = VER_PLATFORM_WIN32_NT ) )
                and
                ( ( ( Win32MajorVersion >= 4 ) and ( Win32MinorVersion >= 90 ) ) or
                  ( Win32MajorVersion >= 5 ) );

    win2000:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion >= 5 );

    winXP:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( ( ( Win32MajorVersion = 5 ) and ( Win32MinorVersion >= 1 ) ) or
                  ( Win32MajorVersion >= 6 ) );

    winServer2003:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( ( ( Win32MajorVersion >= 5 ) and ( Win32MinorVersion >= 2 ) ) or
                  ( Win32MajorVersion >= 6 ) );

    winVista:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion >= 6 );

    win7:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion >= 6 ) and ( Win32MinorVersion >= 1 );

    win8:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion >= 6 ) and ( Win32MinorVersion >= 2 );

    win10:
      Result := ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
                ( Win32MajorVersion >= 10 ) and ( Win32MinorVersion >= 0 );
  
  else
    Result := False;
  end;
end;


function DrawString( Canvas: TCanvas; const S: string; var Bounds: TRect;
                     Flags: UINT ): Integer;
begin
  Result := DrawText( Canvas.Handle, PChar( S ), -1, Bounds, Flags );
end;


procedure DrawStringCentered( Canvas: TCanvas; const S: string; Bounds: TRect );
begin
  DrawText( Canvas.Handle, PChar( S ), -1, Bounds, dt_Center or dt_VCenter or dt_SingleLine );
end;


{&RCMS}
initialization
  FullColorSupported := IsFullColorSupported;
  CurrentXPColorScheme := GetXPColorScheme;
                                                  
  {$IFNDEF VCL150_OR_HIGHER}
  GetLocaleFormatSettings( LOCALE_SYSTEM_DEFAULT, FormatSettings );
  {$ENDIF}
  
  {&RCMI}

finalization
  {&RCMF}

end.

