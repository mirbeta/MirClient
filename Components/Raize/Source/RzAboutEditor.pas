{===============================================================================
  RzAboutEditor Unit

  Raize Components - Design Editor Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Design Editors
  ------------------------------------------------------------------------------
  TRzAboutBoxProperty
    About Box property editor


  Modification History
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Updated copyright information.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Updated copyright information.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Updated copyright information.
    * Added support for new TRzProgressDisplay component.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Updated copyright information.
    * Added support for new TRzCalculator component.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Modified About Box with new Product and Company Logos.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Added descriptions for TRzPathBar.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added descriptions for TRzStringGrid, TRzDBGrid, TRzGroupTemplate,
      TRzGroupController, and TRzFieldStatus.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added new components descriptions.
===============================================================================}

{$I RzComps.inc}

unit RzAboutEditor;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Classes,
  Graphics,
  Forms,
  Controls,
  DesignIntf,
  DesignEditors,
  StdCtrls,
  ExtCtrls,
  Buttons,
  RzCommon,
  RzLabel,
  RzPanel,
  RzBorder,
  RzStatus,
  RzBckgnd,
  RzBmpBtn,
  RzButton,
  jpeg, pngimage;

type
  TRzAboutBoxProperty = class( TPropertyEditor )
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;


  TRzAboutEditDlg = class( TForm )
    lblTrial1: TRzLabel;
    lblComponentName: TRzLabel;
    lblCopyright: TLabel;
    lblDescription: TRzLabel;
    urlRaizeWebsite: TRzURLLabel;
    lblVersion: TRzLabel;
    imgCompBmp: TImage;
    lblInfo: TRzLabel;
    urlEmail: TRzURLLabel;
    lblNewsgroups: TRzLabel;
    urlNewsgroup: TRzURLLabel;
    imgBackground: TImage;
    lblEmailSupport: TRzLabel;
    lblTechSupport: TRzLabel;
    imgClose: TImage;
    procedure FormCreate( Sender: TObject );
    procedure imgCloseClick( Sender: TObject );
    procedure imgBackgroundMouseDown( Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: Integer );
  private
    function FindDescription( CompName: string ): string;
  public
    procedure SetComponentName( Value: string );
  end;


resourcestring
  sCopyrightDate = '1995-2017';
  sCompanyName = 'Embarcadero Technologies, Inc.';
  sProductVersion  = 'Version ' + RaizeComponents_Version;
  {$IFDEF RAIZETRIAL}
  sProductName = 'Konopka Signature VCL Controls ' + RaizeComponents_Version + ' - Trial';
  {$ELSE}
  sProductName = 'Konopka Signature VCL Controls ' + RaizeComponents_Version;
  {$ENDIF}

implementation

{$R *.dfm}

uses
  SysUtils,
  ShellAPI;


resourcestring
  // Raize Panels
  sRzPanel              = 'This custom panel component features enhanced display capabilities including a custom Docking Manager.';
  sRzGroupBar           = 'This custom container displays groups of controls in a XP Category-style view or in an Outlook-style interface.';
  sRzGroupTemplate      = 'This nonvisual component is used to define a template of items that can be used to create actual instances of TRzGroup controls.';
  sRzPageControl        = 'This custom page control offers many more display options than the Common Control equivalent.';
  sRzTabControl         = 'This custom tab control offers many more display options than the Common Control equivalent.';
  sRzSplitter           = 'This custom container consists of two panes separated by a splitter bar.  The panes, and their contents are automatically resized when the splitter bar is moved.';
  sRzSizePanel          = 'This TRzCustomPanel descendant supports runtime resizing.';
  sRzToolbar            = 'This custom panel automatically positions controls, wraps controls to multiple lines, and provides an extensive component editor to add new controls.';
  sRzStatusBar          = 'This custom container is designed to hold status panes (or any other control) and even provides a SizeGrip for easy window resizing.';
  sRzPathBar            = 'This navigational control manages a set of path items that can be used to present a "you are here" path.';
  sRzGroupBox           = 'This TRzCustomPanel descendant provides more display options as well as automatic enabling/disabling of controls in the group.';
  sRzRadioGroup         = 'This TRzGroupBox descendant automatically creates and manages a set of radio buttons.';
  sRzCheckGroup         = 'This TRzGroupBox descendant automatically creates and manages a set of check boxes.';
  sRzDBRadioGroup       = 'This data-aware TRzGroupBox descendant automatically manages a set of radio buttons.';
  sRzDBCheckBoxGroup    = 'The Check Box state of this data-aware TRzGroupBox is controlled by the connected data field.';

  // Raize Edits
  sRzEdit               = 'This edit component supports Custom Framing options as well as right-alignment.';
  sRzMaskEdit           = 'This mask edit component supports the Custom Framing options and right-alignment.';
  sRzButtonEdit         = 'This TRzCustomEdit descendant supports two buttons embedded in the edit field.';
  sRzDateTimeEdit       = 'This TRzCustomEdit descendant supports entering dates and times. A TRzCalendar can be displayed for date selection.';
  sRzNumericEdit        = 'This TRzCustomEdit descendant supports entering numeric data values and custom display formatting.';
  sRzSpinEdit           = 'This TRzCustomEdit descendant supports entering numeric values using an embedded TRzSpinButtons component.';
  sRzColorEdit          = 'This TRzCustomEdit descendant supports selecting colors from a TRzColorPicker.';
  sRzExpandEdit         = 'This TRzCustomEdit descendant supports being automatically resized at runtime to allow more room to enter data.';
  sRzHotKeyEdit         = 'This THotKey descendant adds Custom Framing support.';
  sRzMemo               = 'This memo component supports mouse wheel scrolling, Custom Framing options, and cursor management properites.';
  sRzRichEdit           = 'This rich edit component supports mouse wheel scrolling, Custom Framing options, and cursor management properites.';
  sRzSpinner            = 'This custom component supports selecting an integer value using increment and decrement buttons that are located on each end of the control.';
  sRzTrackBar           = 'This highly flexible track bar component supports built-in thumb styles, owner-draw tick marks, and even custom thumbs.';
  sRzDateTimePicker     = 'This component has been deprecated--Use the TRzDateTimeEdit instead.';
  sRzDBEdit             = 'This data-aware edit component supports Custom Framing options as well as right-alignment.';
  sRzDBButtonEdit       = 'This data-aware TRzButtonEdit descendant supports two buttons embedded in the edit field.';
  sRzDBDateTimeEdit     = 'This data-aware TRzDBDateTimeEdit descendant supports entering dates and times. A TRzCalendar can be displayed for date selection.';
  sRzDBNumericEdit      = 'This data-aware TRzCustomEdit descendant supports entering numeric data values and custom display formatting.';
  sRzDBSpinEdit         = 'This data-aware TRzCustomEdit descendant supports entering numeric values using an embedded TRzSpinButtons component.';
  sRzDBExpandEdit       = 'This data-aware TRzExpandEdit descendant supports being automatically resized at runtime to allow more room to enter data.';
  sRzDBMemo             = 'This data-aware memo component supports mouse wheel scrolling, Custom Framing options, and cursor management properites.';
  sRzDBRichEdit         = 'This data-aware rich edit component supports mouse wheel scrolling, Custom Framing options, and cursor management properites.';
  sRzDBSpinner          = 'This data-aware TRzSpinner descendant supports selecting an integer value using increment and decrement buttons that are located on each end of the control.';
  sRzDBTrackBar         = 'This highly flexible data-aware track bar component supports built-in thumb styles, owner-draw tick marks, and even custom thumbs.';
  sRzDBDateTimePicker   = 'This component has been deprecated--Use the TRzDBDateTimeEdit instead.';

  // Raize Lists
  sRzListBox            = 'This list box component supports incremental keyboard searching, mouse wheel scrolling, and Custom Framing options.';
  sRzRankListBox        = 'This TRzListBox descendant supports re-ordering the items in the list at runtime using the mouse.';
  sRzTabbedListBox      = 'This TRzCustomListBox descendant handles embedded tab characters to display multiple columns.';
  sRzCheckList          = 'This TRzTabbedListBox descendant associates a check box with each item.';
  sRzEditListBox        = 'This TRzCustomListBox descendant supports runtime editing of items using a popup edit window.';
  sRzFontListBox        = 'This TRzCustomListBox descendant supports selecting font names, and includes properties for custom filtering.';
  sRzComboBox           = 'This combo box component supports incremental keyboard searching, auto-complete, and Custom Framing options.';
  sRzImageComboBox      = 'This TRzCustomComboBox descendant supports displaying an image next to each item in the combo box as well as indenting individual items in the list.';
  sRzColorComboBox      = 'This TRzCustomComboBox descendant supports selecting color values, including the standard colors, the Windows system colors, and even custom colors.';
  sRzFontComboBox       = 'This TRzCustomComboBox descendant supports selecting font names, and includes properties for custom filtering.';
  sRzMRUComboBox        = 'This TRzComboBox descendant uses the Windows Registry (32-bit) or an Ini file (16-bit) to manage a most-recently-used list.';
  sRzTreeView           = 'This tree view component supports mouse wheel scrolling and Custom Framing options.';
  sRzCheckTree          = 'This TRzCustomTreeView descendant associates a check box with each node in the tree.';
  sRzListView           = 'This list view component supports mouse wheel scrolling, Custom Framing options, and additional display properties.';
  sRzStringGrid         = 'This enhanced string grid supports Custom Framing as well as additional display options for grid cells.';
  sRzDBListBox          = 'This data-aware list box component supports incremental keyboard searching, mouse wheel scrolling, and Custom Framing options.';
  sRzDBComboBox         = 'This combo box component supports incremental keyboard searching, auto-complete, and Custom Framing options.';
  sRzDBLookupComboBox   = 'This TDBLookupComboBox descendant provides Custom Framing options and the ability to enter a NULL value.';
  sRzDBGrid             = 'This enhanced data-aware grid supports Custom Framing as well as additional display options for grid cells.';

  // Raize Buttons
  sRzButton             = 'This custom button component supports multi-line captions, 3D text styles, and custom button colors.';
  sRzBitBtn             = 'This custom TRzButton descendant supports displaying a glyph as well as multi-line captions, 3D text styles, and custom button colors.';
  sRzMenuButton         = 'This TRzButton descendant supports displaying a popup menu when clicked.';
  sRzDialogButtons      = 'This custom composite component is specifically designed for quickly adding OK, Cancel, and Help buttons to a form.';
  sRzSpinButtons        = 'This custom component manages two buttons which can be oriented either horizontally or vertically and when clicked generated custom events.';
  sRzRapidFireButton    = 'This speed button repeatedly fires its click event as the button is held down.';
  sRzCheckBox           = 'This custom check box supports multi-line captions, customizable glyphs, and three dimensional text styles.';
  sRzRadioButton        = 'This custom radio button supports multi-line captions, customizable glyphs, and three dimensional text styles.';
  sRzToolButton         = 'This custom button control is designed for use on toolbars. It can display images from an image list and can be used to invoke a dropdown menu.';
  sRzSpacer             = 'This custom graphic control is used to separate controls on a TRzToolbar.';
  sRzShapeButton        = 'This custom button component draws a beveled border around the specified bitmap image to create non-rectangular buttons.';
  sRzBmpButton          = 'This custom button component uses bitmaps to depict each state of the button. As a result, the entire button (including the borders) can be customized.';
  sRzToolbarButton      = 'This component has been deprecated--Use the TRzToolButton instead.';
  sRzMenuToolbarButton  = 'This component has been deprecated--Use the TRzToolButton instead.';
  sRzDBNavigator        = 'This enhanced DB Navigator provides updated button images, and provides ImageList support to customize the images.';
  sRzDBCheckBox         = 'This data-aware TRzCheckBox descendant supports multi-line captions, customizable glyphs, and three dimensional text styles.';

  // Raize Display
  sRzFrameController    = 'This nonvisual component provides a single location where all controls that support the Custom Framing properties on a form can be changed.';
  sRzGroupController    = 'This nonvisual component provides a single location where the appearance of all TRzGroup controls can be changed.';
  sRzMenuController     = 'This nonvisual component alters the appearance of TMainMenu, TPopupMenu, and TMenuItem components with a more modern appearance.';
  sRzLabel              = 'This custom label component supports 3D text styles and can be rotated along any angle.';
  sRzURLLabel           = 'This TRzLabel descendant supports creating a hyperlink to a web page, an email address, or a file.';
  sRzBorder             = 'This custom graphic component supports many different bordering display capabilities. For containers, see the TRzPanel component.';
  sRzLine               = 'This custom graphic component displays a line from one corner of the bounding rectangle to the other. Line can be customized in many ways: Arrows, Style, Thickness.';
  sRzSeparator          = 'This custom grahpic component is designed to be used as a separator between UI elements. The client area can be filled with a gradient style.';
  sRzStatusPane         = 'This custom status pane displays non-editable text in a frame.';
  sRzFieldStatus        = 'This TRzStatusPane descendant allows a field label to be displayed next to the status caption.';
  sRzGlyphStatus        = 'This TRzStatusPane descendant allows a bitmap to be displayed within the status area.';
  sRzProgressStatus     = 'This TRzStatusPane descendant displays a progress bar within the status area.';
  sRzMarqueeStatus      = 'This TRzStatusPane descendant scrolls its caption across the status area.';
  sRzClockStatus        = 'This TRzStatusPane descendant displays the current time and/or date in a customizable format.';
  sRzKeyStatus          = 'This TRzStatusPane descendant displays the current state of the Caps Lock, Scroll Lock, or Num Lock key.';
  sRzVersionInfoStatus  = 'This TRzFieldStatus descendant displays the specified field of the connected TRzVersionInfo component.';
  sRzResourceStatus     = 'This TRzCustomStatusPane descendant displays current levels of system resources (i.e. Memory under Windows NT, 2000, or XP -- System Resources under Windows 95, 98, Me).';
  sRzProgressBar        = 'This custom progress bar component supports many display capabilities including custom borders and LED-style bars.';
  sRzProgressDisplay    = 'This custom graphic component displays progress steps (i.e. text messages) in a scrolling window display.';
  sRzMeter              = 'This custom graphic component displays an integer value using multi-colored LED-bars.';
  sRzLEDDisplay         = 'This custom graphic component displays its caption in an LED-style format.';
  sRzBackground         = 'This custom graphic component supports displaying gradients or textures behind an optional image.';
  sRzAnimator           = 'This custom component supports animating through the images of an image list.';
  sRzDBLabel            = 'This data-aware label component supports 3D text styles and can be rotated along any angle.';
  sRzDBStatusPane       = 'This data-aware TRzStatusPane descendant displays non-editable text in a frame.';
  sRzDBStateStatus      = 'This data-aware TRzStatusPane descendant displays the current state of the selected dataset.';
  sRzDBProgressBar      = 'This data-aware TRzProgressBar descendant supports calculating its percentage from a single column, a single column against a base value, or between two columns.';

  // Raize Shell
  sRzShellTree          = 'This custom tree view displays a Windows Explorer-like view of the Shell namespace.';
  sRzShellList          = 'This custom list view displays a Windows Explorer-like view of a Shell Folder''s contents.';
  sRzShellCombo         = 'This custom combo box displays a Windows Explorer-like combo box of the Shell namespace.';
  sRzOpenDialog         = 'This custom dialog component mimics the common Open dialog, but with more functionality and more display options.';
  sRzSaveDialog         = 'This custom dialog component mimics the common Save dialog, but with more functionality and more display options.';
  sRzSelectFolderDialog = 'This custom dialog component supports selecting and creating new folders in the Shell namespace.';
  sRzDirectoryTree      = 'This component has been deprecated--Use the TRzShellTree instead.';
  sRzDirectoryListBox   = 'This component has been deprecated--Use the TRzShellTree instead.';
  sRzFileListBox        = 'This component has been deprecated--Use the TRzShellList instead.';
  sRzDriveComboBox      = 'This component has been deprecated--Use the TRzShellCombo instead.';
  sRzSelDirDialog       = 'This component has been deprecated--Use the TRzSelectFolderDialog instead.';

  // Raize Widgets
  sRzCalendar           = 'This custom control allows a user to select a date from a month-view calendar (Office-style).';
  sRzTimePicker         = 'This custom control allows a user to specify a time by moving the hands of a clock.';
  sRzCalculator         = 'This custom control allows a user to perform simply math calculations.';
  sRzColorPicker        = 'This custom control allows a user to select a colors from a grid of colors (Office-style).';
  sRzCustomColors       = 'This nonvisual component maintains a list of custom color values that can be used by TRzColorPicker and TRzColorEdit components.';
  sRzRegIniFile         = 'This nonvisual component allows a user to use a single interface to read/write values to/from an Ini File or the Registry.';
  sRzFormState          = 'This nonvisual component automatically stores the position, size, and maximized state of a form.';
  sRzFormShape          = 'This custom TImage component is used to create non-rectangular forms.  Transparent areas of the image define the portion of the form to be removed.';
  sRzLauncher           = 'This nonvisual component can be used to start applications or processes from within your application.';
  sRzTrayIcon           = 'This nonvisual component automatically handles placing your application in the System Tray.';
  sRzVersionInfo        = 'This nonvisual component provides access to the Version Info block of an application.';
  sRzBalloonHints       = 'This custom nonvisual component provides support for multi-line hints as well as balloon-style hint windows.';
  sRzLookupDialog       = 'This dialog component provides an alternative to using a combo box for simple lookups.';
  sRzSendMessage        = 'This nonvisual component supports send a MAPI email message to any number of recipients with any number of file attachments.';
  sRzDBLookupDialog     = 'This dialog component supports searching through records in a dataset.';



{=================================}
{== TRzAboutBoxProperty Methods ==}
{=================================}

function TRzAboutBoxProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [ paDialog, paReadOnly ];
end;


function TRzAboutBoxProperty.GetValue: string;
begin
  Result := SProductVersion;
end;


procedure TRzAboutBoxProperty.Edit;
var
  Dialog: TRzAboutEditDlg;
begin
  Dialog := TRzAboutEditDlg.Create( Application );
  try
    Dialog.SetComponentName( GetComponent( 0 ).ClassName );
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;


type
  TRzCompMap = record
    Name: string;
    Description: string;
  end;

const
  NumComponents = 128;
  StringTable: array[ 1..NumComponents ] of TRzCompMap =
  (
    // Raize Panels
    ( Name: 'TRzPanel';               Description: sRzPanel ),
    ( Name: 'TRzGroupBar';            Description: sRzGroupBar ),
    ( Name: 'TRzGroupTemplate';       Description: sRzGroupTemplate ),
    ( Name: 'TRzPageControl';         Description: sRzPageControl ),
    ( Name: 'TRzTabControl';          Description: sRzTabControl ),
    ( Name: 'TRzSplitter';            Description: sRzSplitter ),
    ( Name: 'TRzSizePanel';           Description: sRzSizePanel ),
    ( Name: 'TRzToolbar';             Description: sRzToolbar ),
    ( Name: 'TRzStatusBar';           Description: sRzStatusBar ),
    ( Name: 'TRzPathBar';             Description: sRzPathBar ),
    ( Name: 'TRzGroupBox';            Description: sRzGroupBox ),
    ( Name: 'TRzRadioGroup';          Description: sRzRadioGroup ),
    ( Name: 'TRzCheckGroup';          Description: sRzCheckGroup ),
    ( Name: 'TRzDBRadioGroup';        Description: sRzDBRadioGroup ),
    ( Name: 'TRzDBCheckBoxGroup';     Description: sRzDBCheckBoxGroup ),

    // Raize Edits
    ( Name: 'TRzEdit';                Description: sRzEdit ),
    ( Name: 'TRzMaskEdit';            Description: sRzMaskEdit ),
    ( Name: 'TRzButtonEdit';          Description: sRzButtonEdit ),
    ( Name: 'TRzDateTimeEdit';        Description: sRzDateTimeEdit ),
    ( Name: 'TRzNumericEdit';         Description: sRzNumericEdit ),
    ( Name: 'TRzSpinEdit';            Description: sRzSpinEdit ),
    ( Name: 'TRzColorEdit';           Description: sRzColorEdit ),
    ( Name: 'TRzExpandEdit';          Description: sRzExpandEdit ),
    ( Name: 'TRzHotKeyEdit';          Description: sRzHotKeyEdit ),
    ( Name: 'TRzMemo';                Description: sRzMemo ),
    ( Name: 'TRzRichEdit';            Description: sRzRichEdit ),
    ( Name: 'TRzSpinner';             Description: sRzSpinner ),
    ( Name: 'TRzTrackBar';            Description: sRzTrackBar ),
    ( Name: 'TRzDateTimePicker';      Description: sRzDateTimePicker ),
    ( Name: 'TRzDBEdit';              Description: sRzDBEdit ),
    ( Name: 'TRzDBButtonEdit';        Description: sRzDBButtonEdit ),
    ( Name: 'TRzDBDateTimeEdit';      Description: sRzDBDateTimeEdit ),
    ( Name: 'TRzDBNumericEdit';       Description: sRzDBNumericEdit ),
    ( Name: 'TRzDBSpinEdit';          Description: sRzDBSpinEdit ),
    ( Name: 'TRzDBExpandEdit';        Description: sRzDBExpandEdit ),
    ( Name: 'TRzDBMemo';              Description: sRzDBMemo ),
    ( Name: 'TRzDBRichEdit';          Description: sRzDBRichEdit ),
    ( Name: 'TRzDBSpinner';           Description: sRzDBSpinner ),
    ( Name: 'TRzDBTrackBar';          Description: sRzDBTrackBar ),
    ( Name: 'TRzDBDateTimePicker';    Description: sRzDBDateTimePicker ),

    // Raize Lists
    ( Name: 'TRzListBox';             Description: sRzListBox ),
    ( Name: 'TRzRankListBox';         Description: sRzRankListBox ),
    ( Name: 'TRzTabbedListBox';       Description: sRzTabbedListBox ),
    ( Name: 'TRzCheckList';           Description: sRzCheckList ),
    ( Name: 'TRzEditListBox';         Description: sRzEditListBox ),
    ( Name: 'TRzFontListBox';         Description: sRzFontListBox ),
    ( Name: 'TRzComboBox';            Description: sRzComboBox ),
    ( Name: 'TRzImageComboBox';       Description: sRzImageComboBox ),
    ( Name: 'TRzColorComboBox';       Description: sRzColorComboBox ),
    ( Name: 'TRzFontComboBox';        Description: sRzFontComboBox ),
    ( Name: 'TRzMRUComboBox';         Description: sRzMRUComboBox ),
    ( Name: 'TRzTreeView';            Description: sRzTreeView ),
    ( Name: 'TRzCheckTree';           Description: sRzCheckTree ),
    ( Name: 'TRzListView';            Description: sRzListView ),
    ( Name: 'TRzStringGrid';          Description: sRzStringGrid ),
    ( Name: 'TRzDBListBox';           Description: sRzDBListBox ),
    ( Name: 'TRzDBComboBox';          Description: sRzDBComboBox ),
    ( Name: 'TRzDBLookupComboBox';    Description: sRzDBLookupComboBox ),
    ( Name: 'TRzDBGrid';              Description: sRzDBGrid ),

    // Raize Buttons
    ( Name: 'TRzButton';              Description: sRzButton ),
    ( Name: 'TRzBitBtn';              Description: sRzBitBtn ),
    ( Name: 'TRzMenuButton';          Description: sRzMenuButton ),
    ( Name: 'TRzDialogButtons';       Description: sRzDialogButtons ),
    ( Name: 'TRzSpinButtons';         Description: sRzSpinButtons ),
    ( Name: 'TRzRapidFireButton';     Description: sRzRapidFireButton ),
    ( Name: 'TRzCheckBox';            Description: sRzCheckBox ),
    ( Name: 'TRzRadioButton';         Description: sRzRadioButton ),
    ( Name: 'TRzToolButton';          Description: sRzToolButton ),
    ( Name: 'TRzSpacer';              Description: sRzSpacer ),
    ( Name: 'TRzShapeButton';         Description: sRzShapeButton ),
    ( Name: 'TRzBmpButton';           Description: sRzBmpButton ),
    ( Name: 'TRzToolbarButton';       Description: sRzToolbarButton ),
    ( Name: 'TRzMenuToolbarButton';   Description: sRzMenuToolbarButton ),
    ( Name: 'TRzDBNavigator';         Description: sRzDBNavigator ),
    ( Name: 'TRzDBCheckBox';          Description: sRzDBCheckBox ),

    // Raize Display
    ( Name: 'TRzFrameController';     Description: sRzFrameController ),
    ( Name: 'TRzGroupController';     Description: sRzGroupController ),
    ( Name: 'TRzMenuController';      Description: sRzMenuController ),
    ( Name: 'TRzLabel';               Description: sRzLabel ),
    ( Name: 'TRzURLLabel';            Description: sRzURLLabel ),
    ( Name: 'TRzBorder';              Description: sRzBorder ),
    ( Name: 'TRzLine';                Description: sRzLine ),
    ( Name: 'TRzSeparator';           Description: sRzSeparator ),
    ( Name: 'TRzStatusPane';          Description: sRzStatusPane ),
    ( Name: 'TRzFieldStatus';         Description: sRzFieldStatus ),
    ( Name: 'TRzGlyphStatus';         Description: sRzGlyphStatus ),
    ( Name: 'TRzProgressStatus';      Description: sRzProgressStatus ),
    ( Name: 'TRzMarqueeStatus';       Description: sRzMarqueeStatus ),
    ( Name: 'TRzClockStatus';         Description: sRzClockStatus ),
    ( Name: 'TRzKeyStatus';           Description: sRzKeyStatus ),
    ( Name: 'TRzVersionInfoStatus';   Description: sRzVersionInfoStatus ),
    ( Name: 'TRzResourceStatus';      Description: sRzResourceStatus ),
    ( Name: 'TRzProgressBar';         Description: sRzProgressBar ),
    ( Name: 'TRzProgressDisplay';     Description: sRzProgressDisplay ),
    ( Name: 'TRzMeter';               Description: sRzMeter ),
    ( Name: 'TRzLEDDisplay';          Description: sRzLEDDisplay ),
    ( Name: 'TRzBackground';          Description: sRzBackground ),
    ( Name: 'TRzAnimator';            Description: sRzAnimator ),
    ( Name: 'TRzDBLabel';             Description: sRzDBLabel ),
    ( Name: 'TRzDBStatusPane';        Description: sRzDBStatusPane ),
    ( Name: 'TRzDBStateStatus';       Description: sRzDBStateStatus ),
    ( Name: 'TRzDBProgressBar';       Description: sRzDBProgressBar ),

    // Raize Shell
    ( Name: 'TRzShellTree';           Description: sRzShellTree ),
    ( Name: 'TRzShellList';           Description: sRzShellList ),
    ( Name: 'TRzShellCombo';          Description: sRzShellCombo ),
    ( Name: 'TRzOpenDialog';          Description: sRzOpenDialog ),
    ( Name: 'TRzSaveDialog';          Description: sRzSaveDialog ),
    ( Name: 'TRzSelectFolderDialog';  Description: sRzSelectFolderDialog ),
    ( Name: 'TRzDirectoryTree';       Description: sRzDirectoryTree ),
    ( Name: 'TRzDirectoryListBox';    Description: sRzDirectoryListBox ),
    ( Name: 'TRzFileListBox';         Description: sRzFileListBox ),
    ( Name: 'TRzDriveComboBox';       Description: sRzDriveComboBox ),
    ( Name: 'TRzSelDirDialog';        Description: sRzSelDirDialog ),

    // Raize Widgets
    ( Name: 'TRzCalendar';            Description: sRzCalendar ),
    ( Name: 'TRzTimePicker';          Description: sRzTimePicker ),
    ( Name: 'TRzCalculator';          Description: sRzCalculator ),
    ( Name: 'TRzColorPicker';         Description: sRzColorPicker ),
    ( Name: 'TRzCustomColors';        Description: sRzCustomColors ),
    ( Name: 'TRzRegIniFile';          Description: sRzRegIniFile ),
    ( Name: 'TRzFormState';           Description: sRzFormState ),
    ( Name: 'TRzFormShape';           Description: sRzFormShape ),
    ( Name: 'TRzLauncher';            Description: sRzLauncher ),
    ( Name: 'TRzTrayIcon';            Description: sRzTrayIcon ),
    ( Name: 'TRzVersionInfo';         Description: sRzVersionInfo ),
    ( Name: 'TRzBalloonHints';        Description: sRzBalloonHints ),
    ( Name: 'TRzLookupDialog';        Description: sRzLookupDialog ),
    ( Name: 'TRzSendMessage';         Description: sRzSendMessage ),
    ( Name: 'TRzDBLookupDialog';      Description: sRzDBLookupDialog )
  );

{============================}
{== TRzAboutEditDlg Methods =}
{============================}


procedure TRzAboutEditDlg.FormCreate(Sender: TObject);
begin
  PopupMode := pmAuto;

  lblCopyright.Caption := 'Copyright © '+ sCopyrightDate + ' ' + sCompanyName;
  lblVersion.Caption := sProductVersion;

  {$IFDEF RAIZETRIAL}
  lblTrial1.Visible := True;
  lblTrial1.Blinking := True;
  {$ENDIF}
end;


function TRzAboutEditDlg.FindDescription( CompName: string ): string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while ( I <= NumComponents ) and ( CompareText( CompName, StringTable[ I ].Name ) <> 0 ) do
  begin
    Inc( I );
  end;

  if I <= NumComponents then
    Result := StringTable[ I ].Description
end;


procedure TRzAboutEditDlg.SetComponentName( Value: string );
begin
  if Value[ 1 ] = 'T' then
    lblComponentName.Caption := Copy( Value, 2, 255 )
  else
    lblComponentName.Caption := Value;
  if lblComponentName.Width > 200 then
    lblComponentName.Font.Size := 14;
  lblDescription.Caption := FindDescription( Value );

  // Load Component Bitmap from Delphi's Component Library
  imgCompBmp.Picture.Bitmap.Handle := LoadBitmap( HInstance, PChar( UpperCase( Value ) ) );
  imgCompBmp.Visible := True;
  imgCompBmp.Transparent := True;
end;


procedure TRzAboutEditDlg.imgCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TRzAboutEditDlg.imgBackgroundMouseDown( Sender: TObject; Button: TMouseButton;
                                                  Shift: TShiftState; X, Y: Integer );
const
  sc_DragMove = $F012;
begin
  ReleaseCapture;
  Perform( wm_SysCommand, sc_DragMove, 0 );
end;

end.




