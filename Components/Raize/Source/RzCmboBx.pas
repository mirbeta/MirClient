{===============================================================================
  RzCmboBx Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzComboBox
    Raize Combo Box.  Provides incremental keyboard searching, AutoComplete, and
    Custom Framing.

  TRzColorComboBox
    Drop-down list populated with color values (incl. System Colors)

  TRzFontComboBox
    Select fonts from drop-down list.

  TRzMRUComboBox
    Component automatically manages Most Recently Used items.

  TRzImageComboBox
    Images can be displayed next to each item.  Items can also be indented.

  NOTE:
  wm_NCPaint processing used in other controls to draw the frame does not work
  on combo boxes. The actual border is drawn in the default wm_Paint handler.


  Modification History
  ------------------------------------------------------------------------------
  6.2.1  (01 Sep 2015)
    * Fixed issue in TRzComboBox where mouse wheel configured to scroll by
      screen would not scroll through the combo box items correctly.
  ------------------------------------------------------------------------------
  6.1.7  (07 Mar 2014)
    * Fixed potential problem populating TRzFontComboBox in 64-bit Windows.
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Fixed issue in TRzColorComboBox where clearing the color list and then
      trying to access one of the ColorNames elements would raise and exception.
  ------------------------------------------------------------------------------
  6.1.3  (01 May 2013)
    * Fixed issue in TRzComboBox where entering alternate characters using
      Alt+NumPad combinations would get erased when typing additional
      characters.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Fixed issue in TRzImageComboBox where selected items would always appear
      in the default font color rather than the highlight font color.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed issue in TRzComboBox where an item's value could get out of sync
      when calling AddItemValue or InsertItemValue on a Sorted combo box.
    * Fixed issue in TRzComboBox and descendants where the Delete key could be
      used to modify text portion even if ReadOnly was set to True.
    * Fixed issue in TRzColorComboBox that could potentially cause an invalid
      pointer exception.
    * Fixed issue in the TRzComboBox where the selected ItemIndex and internal
      FValue value could get out of sync.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Fixed display issue in TRzComboBox and descendants where the under Vista
      and Windows 7, the background behind the dropdown arrow would not get
      changed if the Color property was changed and FlatButtons was True.
    * Fixed display issue under Vista and Windows 7 in TRzComboBox where the
      standard themed control border would still be visible when using Custom
      Framing and removing one or more sides.
    * Fixed issue in TRzComboBox where Values entries needed to be unique in
      order to properly locate an item. Now, multiple items in the list may
      be set to the same "value". Selecting an item with a duplicate value -- no
      longer -- results in the first item with that value from being
      automatically selected.
    * Fixed issue where the dropdown list would not be size appropriately if
      the combo box was set to use a larger font size.
    * Fixed display issue with the Preview Panel in the TRzFontComboBox when it
      is initially displayed.
    * Made necessary modifications to TRzComboBox, TRzColorComboBox,
      TRzFontComboBox, TRzMRUComboBox, and TRzImageComboBox to fully support VCL
      Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzComboBox and descendants to support
      64-bit development.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * Fixed issue in TRzComboBox and descendants with ReadOnly set to True and
      Style set to csDropDownList where the drop down list could still be
      displayed by using Alt+DownArrow.
    * Fixed display issue in ReadOnly TRzComboBox and descendants under Windows
      Vista and Windows 7 when the control was set to csDropDownList style.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed performance issue with TRzComboBox controls and descendants that set
      the Style property to csOwnerDrawFixed.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzComboBox, TRzColorComboBox, TRzFontComboBox, TRzZMRUComboBox, and
      TRzImageComboBox controls.
    * Fixed custom framing display issue with the TRzComboBox (and descendants)
      when running under Widows Vista, but Themes are NOT used.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Fixed issue with the display of TRzComboBox controls under Windows Vista,
      when FlatButtons was set to True.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Added DeleteItemValue method to the TRzComboBox class. This method goes
      along with the AddItemValue and InsertItemValue methods in keeping the
      set of Values associated with the Items in sync.
    * Fixed display issue with TRzComboBox buttons under Windows Vista.
    * Eliminated the white bar that would appear on a TRzComboBox with its Style
      set to csDropDownList when running under Windows Vista.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * The DropDownWidth property of TRzComboBox (and descendants) has been
      enhanced such that when the value of the property is 0, the width of the
      drop down list is automatically adjusted so that all items are completely
      in view. In previous versions the width of the drop down list would always
      match the width of the combo box unless a non-zero value was specified
      for DropDownWidth.
    * Added new TextHint property to TRzComboBox. The TextHint property allows a
      developer to specify a prompt that appears inside the edit area of the
      control, when the control is empty. The text prompt appears grayed.
      NOTES:
        - TextHint is only applicable under WinXP or Vista, and only when
          XP/Vista themes are in use.
        - TextHint is also only applicable when the Style of the combo box is
          csDropDown (i.e. the user can type into the edit area).
        - Unlike the Windows Edit Control, the Combo Box does not support the
          ability to keep the prompt visible while the control has the focus.
          Therefore, there is no TextHintVisibleOnFocus property like there is
          in TRzEdit.
        - RAD Studio 2009 supports TextHint for TComboBox. TRzComboBox utilizes
          the inherited TextHint property where possible.
    * Added ClearItems method to TRzImageComboBox that handles freeing the
      associated objects that define the image index and indent level when
      deleting each item.
    * Modified the header file (RzCmboBx.hpp) that gets generated by the Delphi
      compiler when compiling components for use in C++Builder. The new
      modifications allow C++Builder developers to create custom controls that
      descend from the TRzCustomComboBox class (or from other classes that
      descend from this class) without resulting in linker errors because of
      differences in how the HWnd type is defined in Delphi and C++.
      NOTE:
        - When using C++Builder 2009 or later, the above modifications are not
          necessary because of changes made to the Delphi compiler. However, the
          changes are dependent on the STRICT conditional symbol being defined.
          However, C++Builder projects define the NO_STRICT symbol by default.
          Therefore, in order to compile and link descendant controls in
          C++Builder 2009 or later, the NO_STRICT symbol must be replaced with
          the STRICT symbol.
    * Fixed issue in TRzFontComboBox with ShowStyle set to ssFontPreview that
      resulted in the Preview panel being displayed on a different monitor when
      the combo box was positioned close to the right edge of a monitor.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Reverted drawing code for TRzComboBox and descendants for drawing the drop
      down buttons when Vista/XP themes are in use.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Fixed issue where the dropdown button in TRzComboBox and descendants would
      get drawn themed even if the FramingPreference was set to fpCustomFraming.
  ------------------------------------------------------------------------------
  4.2.1  (07 Jun 2007)
    * Changed the way the TRzComboBox handles the Escape key. In previous
      versions, when the combo box was in csDropDownList mode and the drop down
      list was not displayed, pressing Escape would cause the ItemIndex property
      to be changed to the first item in the list. When the combo box was in
      csDropDown or csSimple mode, pressing Escape set ItemIndex to -1, thus
      clearing the display. Starting with this version, pressing the Escape
      key in the TRzComboBox will close the drop down list if it is visible.
      In addition, pressing Escape will clear the current search string entered
      by the user for AutoComplete. However, pressing Escape no longer modifies
      the ItemIndex property.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Fixed issue where setting TabOnEnter to True and pressing Enter would not
      change the focus to the next control if the TRzComboBox was ReadOnly.
    * Modified the code that creates the Font Preview Panel in TRzFontComboBox
      to only occur at runtime. This was necessary because the Delphi 2007 IDE
      was making the hidden panel visible at design-time.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Fixed issue where using custom framing to remove the border of a
      TRzComboBox (or descendant) while also using XP themes would not work
      correctly if the control's Color property was clWhite.
    * Surfaced KeepSearchCase in TRzMRUComboBox.
    * Further adjusted TRzComboBox coding to handle repeated mouse enter/leave
      events--in particular, invoking coding when style is csSimple to prevent
      flickering in the displayed list when using XP themes.
    * Fixed painting issue of TRzComboBox in the csSimple style when used under
      XP Themes.
    * Fixed memory leak in TRzColorComboBox that would occur if you modified
      the ColorNames property.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where focusing an item in a TRzImageComboBox would cause all
      other images in the associated ImageList to be displayed in their selected
      state.
    * Added new IndexOfItem and IndexOfData methods to the TRzImageComboBox.
    * Adjusted the custom coding introduced in 4.0.2 to handle the repeated
      OnMouseEnter/OnMouseLeave events from being generated in the TRzComboBox
      to only take effect when the Style property is csDropDown. This change
      indirectly fixes the problem of hints not getting displayed when the
      Style is set to csDropDownList.
    * Fixed problem where a TRzComboBox using csOwnerDrawFixed style could still
      be modified even though the ReadOnly property was set to True.
    * Added ClearItemsValues and InsertItemValue methods to TRzComboBox.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * The font preview window in TRzFontComboBox no longer gets displayed off-
      screen if the control is positioned to close to the edge of the screen.
    * Fixed issue where dynamically reparenting a TRzImageComboBox would cause
      the control to lose its contents.
    * Surfaced OnMouseWheel event in TRzComboBox and descendants.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * In VCL 10 (i.e. BDS 2006), changes were made to the VCL base classes that
      cause the generation of OnMouseEnter/OnMouseLeave event sets every time
      the mouse is moved over the TComboBox. This change in behavior affects
      the custom framing capabilities of the TRzComboBox especially when
      FrameHotTrack is set to True. The effect is excessive flicker. In this
      build, the base VCL behavior has been redesigned to take into account that
      the Windows combo box is made up of more than one window handle.  The
      end result is that the OnMouseEnter and OnMouseLeave events are generated
      in a more reasonable manner, thus eliminating the flicker.
    * Fixed "Unable to insert line" error that would occur in TRzMRUComboBox
      when trying to load MRU items that contained just white space.
    * Added ReadOnlyColorOnFocus property to TRzComboBox and descendants. This
      property determines if the ReadOnlyColor or FocusColor is used to color
      the control when the control has the focus and has its ReadOnly property
      set to True. By default, a focused control will use the FocusColor even
      if the control is read-only. Setting ReadOnlyColorOnFocus to True will
      cause a focused read-only control to be colored using ReadOnlyColor.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Changed the preference order between FocusColor and ReadOnlyColor
      properties in TRzComboBox. In the previous ordering, a ReadOnly control
      would be displayed in ReadOnlyColor even when the control had the focus.
      Now, a ReadOnly control that has the input focus is displayed in
      FocusColor.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where TRzComboBox would not recognize certain characters
      when the user entered them using the U.S. International keyboard
      functionality in Windows.  The international keyboard allows a user to
      entered composite characters (e.g. ë, é, etc.) by entering two keystrokes
      instead of typing Alt+NNNN on the keypad.  For instance, typing "'"
      followed by "e" results in "é" being entered.  The combo box would
      correctly recognize these characters, but would not recognize non-
      composite patterns such as "'" followed by "t".  This problem has been
      fixed in this build.
    * Fixed problem where the TRzComboBox.OnMatch event would occur as the user
      typed more of the item's text even though the ItemIndex did not change.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzComboBox to
      account for changes introduced in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzComboBox and
      descendants when FrameVisible was set to True and changes were made to
      control's appearance within calls to LockWindowUpdate.
    * Added ReadOnlyColor property to TRzComboBox and descendant classes. This
      color property is used to change the color of the control when the
      ReadOnly property is set to True.
    * Added new FrameControllerNotifications property to TRzComboBox and all
      descendant combo boxes (e.g. TRzFontComboBox).
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
    * The TRzComboBox and descendants now utilize the new global variable
      IncrementalSearchResetDelay which is defined in the RzCommon unit. This
      variable defines how much inactivity during incremental keyboard searching
      will cause the search string to reset itself. The default is 1.5 seconds.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added the new Values property to TRzComboBox, in which each string
      in the Values list is related to the string in the Items list at the same
      index.  And when the user selects a particular items from the dropdown
      list (which displays the Items), the corresponding item from the
      Values list is stored in the new Value property.  If the Values list is
      not used, the Value property matches the selected Items string.
    * Added TRzComboBox.Value property, which provides easy access to the
      selected "value" from the Values list that corresponds to the ItemIndex.
      The Value property can be assigned a "value" from the Values list and
      the corresponding ItemIndex is used.
    * Added the TRzComboBox.AddItemValue method, which makes it easy to
      add an item and corresponding value to the Items and Values lists,
      respectively.
    * Further refined processing of custom color values in TRzColorComboBox when
      the user selects a an empty color cell from the Color Dialog box.
    * Fixed problem where placing a TRzFontComboBox onto a TRzSizePanel which
      was aligned alRight would prevent the size panel from resizing.
    * Fixed problems where the TRzMRUComboBox control would not always record
      the item entered by the user. In previous versions, the updating of the
      MRU list took place when the user would leave the field by changing focus.
      In addition to this, now the control records the selected item and updates
      the MRU list when the user makes a selection from the list using the mouse
      or the keyboard.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed issue where selecting invoking the Custom Color dialog box from a
      TRzColorComboBox and then selecting an empty custom color box caused a
      exception when the dialog was closed.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where combo box could still be modified when ReadOnly set to
      True and the Style was set to csOwnerDrawFixed.
    * Fixed problem where auto-complete functionality would not update text
      correctly in cases where the data-aware version would reset its data.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where inserting MBCS characters in the middle of the text in
      the combo box would result in incorrect characters being displayed.
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
    * The TRzImageComboBox now allows keyboard access under all versions of
      Windows except Windows NT 4.0.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem where OnSelEndOK was not fired when user selects an item
      from a TRzComboBox by pressing Enter.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where AutoDropDown functionality was not working.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem where Alt+F4 would not exit app if focus was in a
      TRzComboBox with its ReadOnly property set to True.
    * Fixed problem where it was possible to type in characters into a ReadOnly
      combo box.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Made IndexOf virtual. Overrode IndexOf in TRzCustomImageComboBox to
      correctly search for matching caption.
    * Fixed problem where items in the list could not be moved or exchanged
      without raising an exception.
    * Added Delete method to TRzCustomImageComboBox, which should be used to
      manually delete an item from the list.  This will cause the associated
      object to be freed.
    * Added MruRegIniFile property to TRzMRUComboBox. This property should be
      used instead of MRUPath.  MRUPath is still provided for
      backward-compatibility.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomComboBox and TRzComboBox >>
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
    * Add FocusColor and DisabledColor properties.
    * AutoComplete now works correctly under Multi-Byte Character Systems.
    * Fixed problem where an item that was the same as another item except in a
      different case could not be selected.
    * Added AutoComplete property.  This will allow a user to disable the
      auto-complete feature of the combo box.  Also added the ForceText method,
      which turns off AutoComplete, sets the text and then turns on
      AutoComplete.
    * Fixed problem where mouse wheel may not always scroll up to the top of the
      list.
    * When the combo box is csDropDownList and the user presses the Escape key
      while the List is down, the list only closes and does not put the first
      item in the list into the text area.
    * ComboBox button style has been updated.
    * Added the KeepSearchCase property.  This property controls whether or not
      the case of the search string the user types in is maintained as matches
      are found.
    * Fixed problem where moving the mouse over the edit portion of the combo
      box caused the selected item in the list to be displayed in the edit
      portion.  This problem occurred when the FrameFlat (now called
      FrameHotTrack) property was set to True.
    * Added XP visual style support in drawing of buttons during Custom Framing.

    << TRzFontCombobox >>
    * Added a ShowSymbolFonts property.
    * Added a new ShowStyle called ssFontPreview. When this style is selected,
      when the user drops the list of fonts down, a preview window is displayed
      that shows a sample string formatted in the selected font.  This effect is
      similar to the one used in CorelDRAW when selecting fonts. The size of the
      preview window can be changed with the PreviewWidth and PreviewHeight
      properties. The sample text displayed can be changed using the PreviewText
      property, or by specifying the PreviewEdit property. When the PreviewEdit
      property is set to a valid TCustomEdit and there the user has selected
      some text in the edit control, the selected text is used as the preview
      text.
    * The TRzFontComboBox now maintains the most recently used fonts selected by
      the user.  As the user selects a font, the font is added to the top of the
      list.  If the font is already in the MRU section, then it is moved to the
      top of the list when selected.

    << TRzColorComboBox >>
    * The LoadCustomColors and SaveCustomColors methods have been changed.
      Instead of passing the path to the registry key of where to store the
      custom colors, the TRzColorComboBox needs to be linked to a TRzRegIniFile
      component to handle saving the information off to either an INI file
      or the Registry.

    * TRzImageComboBox component added.
===============================================================================}

{$I RzComps.inc}

unit RzCmboBx;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Classes,
  Forms,
  Graphics,
  Controls,
  StdCtrls,
  Menus,
  ExtCtrls,
  RzCommon,
  Dialogs,
  ImgList;

const
  MaxStdColors = 16;
  MaxSysColors = 25;

type
  {=========================================}
  {== TRzCustomComboBox Class Declaration ==}
  {=========================================}

  TRzDeleteComboItemEvent = procedure( Sender: TObject; Item: Pointer ) of object;

  TRzCustomComboBox = class( TCustomComboBox )
  private
    FAutoComplete: Boolean;
    FAllowEdit: Boolean;
    FBeepOnInvalidKey: Boolean;
    FFlatButtons: Boolean;
    FFlatButtonColor: TColor;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FReadOnlyColor: TColor;
    FReadOnlyColorOnFocus: Boolean;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FKeepSearchCase: Boolean;
    FSearchString: string;
    FDropDownWidth: Integer;
    FKeyCount: Integer;
    FTimer: TTimer;
    FTabOnEnter: Boolean;
    FTyping: Boolean;
    FEnterPressed: Boolean;
    FReadOnly: Boolean;
    FSysKeyDown: Boolean;

    FOnDeleteItem: TRzDeleteComboItemEvent;
    FOnMatch: TNotifyEvent;
    FOnNotInList: TNotifyEvent;
    FOnSelEndCancel: TNotifyEvent;
    FOnSelEndOk: TNotifyEvent;

    FMouseControl: TControl;
    FMouseInClient: Boolean;
    FOverEditArea: Boolean;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    procedure SetItemHeight2( Value: Integer );

    // Message Handling Methods
    procedure WMKeyDown( var Msg: TWMKeyDown ); message wm_KeyDown;
    procedure WMCut( var Msg: TMessage ); message wm_Cut;
    procedure WMPaste( var Msg: TMessage ); message wm_Paste;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
    procedure CNCommand( var Msg: TWMCommand ); message cn_Command;
    procedure CNDrawItem( var Msg: TWMDrawItem ); message cn_DrawItem;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure WMDeleteItem( var Msg: TWMDeleteItem ); message wm_DeleteItem;
    procedure WMLButtonDown( var Msg: TWMLButtonDown ); message wm_LButtonDown;
    procedure WMLButtonDblClick( var Msg: TWMLButtonDblClk ); message wm_LButtonDblClk;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
  protected
    FAboutInfo: TRzAboutInfo;
    FCanvas: TCanvas;
    FInControl: Boolean;
    FOverControl: Boolean;
    FShowFocus: Boolean;
    FIsFocused: Boolean;
    FActualDropDownWidth: Integer;

    procedure CreateWnd; override;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;

    procedure InvalidKeyPressed; virtual;
    procedure SearchTimerExpired( Sender: TObject );

    procedure UpdateIndex( const FindStr: string; Msg: TWMChar ); virtual;
    function FindListItem( const FindStr: string; Msg: TMessage ): Boolean; virtual;
    function FindClosest( const S: string ): Integer; virtual;

    // BCB Header Translation Required for HWnd
    procedure ComboWndProc( var Msg: TMessage; ComboWnd: HWnd; ComboProc: Pointer ); override;

    procedure EditWndProc( var Msg: TMessage ); override;
    procedure WndProc( var Msg: TMessage ); override;
    procedure UpdateSearchStr;

    // Event Dispatch Methods
    procedure DropDown; override;
    procedure CloseUp; override;
    procedure KeyPress( var Key: Char ); override;
    procedure Match; dynamic;
    procedure NotInList; dynamic;

    procedure DeleteItem( Item: Pointer ); virtual;
    procedure SelEndCancel; dynamic;
    procedure SelEndOk; dynamic;

    // Property Access Methods
    procedure SetFlatButtons( Value: Boolean ); virtual;
    procedure SetFlatButtonColor( Value: TColor ); virtual;
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function StoreDisabledColor: Boolean;
    function StoreReadOnlyColor: Boolean;
    function StoreReadOnlyColorOnFocus: Boolean;
    function StoreParentColor: Boolean;
    function StoreFlatButtonColor: Boolean;
    function StoreFlatButtons: Boolean;
    function StoreFrameColor: Boolean;
    function StoreFrameHotColor: Boolean;
    function StoreFrameHotTrack: Boolean;
    function StoreFrameHotStyle: Boolean;
    function StoreFrameSides: Boolean;
    function StoreFrameStyle: Boolean;
    function StoreFrameVisible: Boolean;
    function StoreFramingPreference: Boolean;
    procedure SetDisabledColor( Value: TColor ); virtual;
    procedure SetFocusColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetFrameHotColor( Value: TColor ); virtual;
    procedure SetFrameHotTrack( Value: Boolean ); virtual;
    procedure SetFrameHotStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameSides( Value: TSides ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetFrameVisible( Value: Boolean ); virtual;
    procedure SetFramingPreference( Value: TFramingPreference ); virtual;

    procedure SetReadOnly( Value: Boolean ); virtual;
    procedure SetReadOnlyColor( Value: TColor ); virtual;

    { Property Declarations }
    property AllowEdit: Boolean
      read FAllowEdit
      write FAllowEdit
      default True;

    property AutoComplete: Boolean
      read FAutoComplete
      write FAutoComplete
      default True;

    property Color
      stored StoreColor
      default clWindow;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write SetFlatButtonColor
      stored StoreFlatButtonColor
      default clBtnFace;

    property FlatButtons: Boolean
      read FFlatButtons
      write SetFlatButtons
      stored StoreFlatButtons
      default False;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored StoreDisabledColor
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored StoreFrameColor
      default clBtnShadow;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored StoreFrameHotColor
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored StoreFrameHotStyle
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored StoreFrameHotTrack
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored StoreFrameSides
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored StoreFrameStyle
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored StoreFrameVisible
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      stored StoreFramingPreference
      default fpXPThemes;

    property KeepSearchCase: Boolean
      read FKeepSearchCase
      write FKeepSearchCase
      default False;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    (*
    // Do not surface control canvas property b/c interfers with w/ existing
    // canvas, which is used for owner-draw drawing in descendant components

    property Canvas: TCanvas
      read FCanvas;
    *)

    property DropDownWidth: Integer
      read FDropDownWidth
      write FDropDownWidth
      default 0;

    property ReadOnly: Boolean
      read FReadOnly
      write SetReadOnly
      default False;

    property ReadOnlyColor: TColor
      read FReadOnlyColor
      write SetReadOnlyColor
      stored StoreReadOnlyColor
      default clInfoBk;

    property ReadOnlyColorOnFocus: Boolean
      read FReadOnlyColorOnFocus
      write FReadOnlyColorOnFocus
      stored StoreReadOnlyColorOnFocus
      default False;

    property OnDeleteItem: TRzDeleteComboItemEvent
      read FOnDeleteItem
      write FOnDeleteItem;

    property OnMatch: TNotifyEvent
      read FOnMatch
      write FOnMatch;

    property OnNotInList: TNotifyEvent
      read FOnNotInList
      write FOnNotInList;

    property OnSelEndCancel: TNotifyEvent
      read FOnSelEndCancel
      write FOnSelEndCancel;

    property OnSelEndOk: TNotifyEvent
      read FOnSelEndOk
      write FOnSelEndOk;

    property ItemHeight
      write SetItemHeight2;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    function Focused: Boolean; override;

    procedure ForceText( const Value: string ); virtual;

    { Wrapper methods arounds Items object }
    function Add( const S: string ): Integer;
    function AddObject( const S: string; AObject: TObject ): Integer;
    procedure ClearSearchString;
    procedure Delete( Index: Integer );
    procedure ClearItems;
    function IndexOf( const S: string ): Integer; virtual;
    procedure Insert( Index: Integer; const S: string );
    procedure InsertObject( Index: Integer; const S: string; AObject: TObject );
    function Count: Integer;
    function FindItem( const S: string ): Boolean;
    function FindClosestItem( const S: string ): Boolean;

    property BeepOnInvalidKey: Boolean
      read FBeepOnInvalidKey
      write FBeepOnInvalidKey
      default True;

    property SearchString: string
      read FSearchString;
  end;


  {===================================}
  {== TRzComboBox Class Declaration ==}
  {===================================}

  TRzComboBox = class( TRzCustomComboBox )
  private
    FValue: string;
    FValues: TStrings;
    FInSetValue: Boolean;
    FForceSetValue: Boolean;

    // Internal Event Handlers
    procedure ValuesChangedHandler( Sender: TObject );
  protected
    // Event Dispatch Methods
    procedure Change; override;
    procedure Click; override;

    // Property Access Methods
    function GetItemValue( Index: Integer ): string; virtual;
    function GetValue: string; virtual;
    procedure SetValue( const Value: string ); virtual;
    procedure SetValues( Value: TStrings ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ClearItemsValues;
    procedure AddItemValue( const Item, Value: string );
    procedure InsertItemValue( Index: Integer; const Item, Value: string );
    procedure DeleteItemValue( Index: Integer );

    property Value: string
      read GetValue
      write SetValue;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Align;
    property AllowEdit;
    property Anchors;
    property AutoComplete;
    property AutoCloseUp;
    property AutoDropDown;
    property BeepOnInvalidKey;
    property BiDiMode;
    property Style;                            // Must be published before Items
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property DropDownWidth;
    property Enabled;
    property Font;
    property FlatButtonColor;
    property FlatButtons;
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
    property KeepSearchCase;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property Sorted;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDeleteItem;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
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
    property OnMatch;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnNotInList;
    property OnSelect;
    property OnSelEndCancel;
    property OnSelEndOk;
    property OnStartDock;
    property OnStartDrag;

    property Items;                     // Must be published after OnMeasureItem
    property ItemIndex default -1;

    property Values: TStrings
      read FValues
      write SetValues;
  end;


  TRzColorComboBox = class;

  {=====================================}
  {== TRzColorNames Class Declaration ==}
  {=====================================}

  TRzColorNames = class( TPersistent )
  private
    FComboBox: TRzColorComboBox;
    FDefaultColor: string;
    FCustomColor: string;
    FStdColors: array[ 0..MaxStdColors - 1 ] of string;
    FSysColors: array[ 0..MaxSysColors - 1 ] of string;
  protected
    procedure SetDefaultColor( const Value: string );
    function GetStdColor( Index: Integer ): string;
    procedure SetStdColor( Index: Integer; const Value: string );
    function GetSysColor( Index: Integer ): string;
    procedure SetSysColor( Index: Integer; const Value: string );
    procedure SetCustomColor( const Value: string );
  public
    ShowSysColors: Boolean;
    ShowDefaultColor: Boolean;
    ShowCustomColor: Boolean;
    constructor Create;
    procedure Assign( Source: TPersistent ); override;

    property StdColors[ Index: Integer ]: string
      read GetStdColor
      write SetStdColor;

    property SysColors[ Index: Integer ]: string
      read GetSysColor
      write SetSysColor;
  published
    property Default: string
      read FDefaultColor
      write SetDefaultColor;

    property Black: string index 0 read GetStdColor write SetStdColor;
    property Maroon: string index 1 read GetStdColor write SetStdColor;
    property Green: string index 2 read GetStdColor write SetStdColor;
    property Olive: string index 3 read GetStdColor write SetStdColor;
    property Navy: string index 4 read GetStdColor write SetStdColor;
    property Purple: string index 5 read GetStdColor write SetStdColor;
    property Teal: string index 6 read GetStdColor write SetStdColor;
    property Gray: string index 7 read GetStdColor write SetStdColor;
    property Silver: string index 8 read GetStdColor write SetStdColor;
    property Red: string index 9 read GetStdColor write SetStdColor;
    property Lime: string index 10 read GetStdColor write SetStdColor;
    property Yellow: string index 11 read GetStdColor write SetStdColor;
    property Blue: string index 12 read GetStdColor write SetStdColor;
    property Fuchsia: string index 13 read GetStdColor write SetStdColor;
    property Aqua: string index 14 read GetStdColor write SetStdColor;
    property White: string index 15 read GetStdColor write SetStdColor;

    property ScrollBar: string index 0 read GetSysColor write SetSysColor;
    property Background: string index 1 read GetSysColor write SetSysColor;
    property ActiveCaption: string index 2 read GetSysColor write SetSysColor;
    property InactiveCaption: string index 3 read GetSysColor write SetSysColor;
    property Menu: string index 4 read GetSysColor write SetSysColor;
    property Window: string index 5 read GetSysColor write SetSysColor;
    property WindowFrame: string index 6 read GetSysColor write SetSysColor;
    property MenuText: string index 7 read GetSysColor write SetSysColor;
    property WindowText: string index 8 read GetSysColor write SetSysColor;
    property CaptionText: string index 9 read GetSysColor write SetSysColor;
    property ActiveBorder: string index 10 read GetSysColor write SetSysColor;
    property InactiveBorder: string index 11 read GetSysColor write SetSysColor;
    property AppWorkSpace: string index 12 read GetSysColor write SetSysColor;
    property Highlight: string index 13 read GetSysColor write SetSysColor;
    property HighlightText: string index 14 read GetSysColor write SetSysColor;
    property BtnFace: string index 15 read GetSysColor write SetSysColor;
    property BtnShadow: string index 16 read GetSysColor write SetSysColor;
    property GrayText: string index 17 read GetSysColor write SetSysColor;
    property BtnText: string index 18 read GetSysColor write SetSysColor;
    property InactiveCaptionText: string index 19 read GetSysColor write SetSysColor;
    property BtnHighlight: string index 20 read GetSysColor write SetSysColor;
    property DkShadow3D: string index 21 read GetSysColor write SetSysColor;
    property Light3D: string index 22 read GetSysColor write SetSysColor;
    property InfoText: string index 23 read GetSysColor write SetSysColor;
    property InfoBk: string index 24 read GetSysColor write SetSysColor;

    property Custom: string
      read FCustomColor
      write SetCustomColor;
  end;


  {========================================}
  {== TRzColorComboBox Class Declaration ==}
  {========================================}

  TRzColorComboBox = class( TRzCustomComboBox )
  private
    FCancelPick: Boolean;
    FDefaultColor: TColor;
    FCustomColor: TColor;
    FColorNames: TRzColorNames;
    FSaveColorNames: TRzColorNames;
    FShowSysColors: Boolean;
    FShowColorNames: Boolean;
    FShowDefaultColor: Boolean;
    FShowCustomColor: Boolean;
    FColorDlgOptions: TColorDialogOptions;
    FCustomColors: TStrings;
    FStoreColorNames: Boolean;
    FSaveItemIndex: Integer;
    FRegIniFile: TRzRegIniFile;


    { Message Handling Methods }
    procedure CNDrawItem( var Msg: TWMDrawItem ); message cn_DrawItem;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CNCommand( var Msg: TWMCommand ); message cn_Command;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function GetCustomColorName( Index: Integer ): string;
    procedure FixupCustomColors; virtual;
    procedure InitColorNames; virtual;
    function GetColorFromItem( Index: Integer ): TColor; virtual;

    { Event Dispatch Methods }
    procedure DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;

    procedure CloseUp; override;

    { Property Access Methods }
    procedure SetDefaultColor( Value: TColor ); virtual;
    procedure SetColorNames( Value: TRzColorNames ); virtual;
    procedure SetCustomColor( Value: TColor ); virtual;
    procedure SetCustomColors( Value: TStrings ); virtual;
    procedure SetShowCustomColor( Value: Boolean ); virtual;
    procedure SetShowDefaultColor( Value: Boolean ); virtual;
    procedure SetShowSysColors( Value: Boolean ); virtual;
    procedure SetShowColorNames( Value: Boolean ); virtual;
    function GetSelectedColor: TColor; virtual;
    procedure SetSelectedColor( Value: TColor ); virtual;

    procedure SetFrameVisible( Value: Boolean ); override;
    procedure SetRegIniFile( Value: TRzRegIniFile ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure LoadCustomColors( const Section: string );
    procedure SaveCustomColors( const Section: string );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ColorNames: TRzColorNames
      read FColorNames
      write SetColorNames
      stored FStoreColorNames;

    property CustomColor: TColor
      read FCustomColor
      write SetCustomColor
      default clBlack;

    property CustomColors: TStrings
      read FCustomColors
      write SetCustomColors;

    property ColorDlgOptions: TColorDialogOptions
      read FColorDlgOptions
      write FColorDlgOptions
      default [ cdFullOpen ];

    property DefaultColor: TColor
      read FDefaultColor
      write SetDefaultColor
      default clBlack;

    property RegIniFile: TRzRegIniFile
      read FRegIniFile
      write SetRegIniFile;

    property ShowColorNames: Boolean
      read FShowColorNames
      write SetShowColorNames
      default True;

    property ShowCustomColor: Boolean
      read FShowCustomColor
      write SetShowCustomColor
      default True;

    property ShowDefaultColor: Boolean
      read FShowDefaultColor
      write SetShowDefaultColor
      default True;

    property ShowSysColors: Boolean
      read FShowSysColors
      write SetShowSysColors
      default True;

    { Must occur after ShowCustomColor, ShowDefaultColor, and ShowSysColors }
    property SelectedColor: TColor
      read GetSelectedColor
      write SetSelectedColor
      default clBlack;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoCloseUp;
    property AutoDropDown;
    property BeepOnInvalidKey;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property DropDownWidth;
    property Enabled;
    property FlatButtonColor;
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
    property ItemHeight;
    {property Items;    User does not have access to the Items list }
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    {property Sorted;   Color list should not be sorted }
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

  {===========================================}
  {== TRzPreviewFontPanel Class Declaration ==}
  {===========================================}

  TRzFontComboBox = class;

  TRzPreviewFontPanel = class( TCustomPanel )
  private
    FControl: TWinControl;

    { Message Handling Methods }
    procedure CMCancelMode( var Msg: TCMCancelMode ); message cm_CancelMode;
    procedure CMShowingChanged( var Msg: TMessage ); message cm_ShowingChanged;
    procedure WMKillFocus( var Msg: TMessage ); message wm_KillFocus;
  protected
    procedure CreateParams( var Params: TCreateParams ); override;
    procedure Paint; override;
  public
    constructor Create( AOwner: TComponent ); override;

    property Control: TWinControl
      write FControl;

    property Alignment;
    property Canvas;
    property Caption;
    property Font;
  end;

  {=======================================}
  {== TRzFontComboBox Class Declaration ==}
  {=======================================}

  TRzFontDevice = ( fdScreen, fdPrinter );
  TRzFontType = ( ftAll, ftTrueType, ftFixedPitch, ftPrinter );
  TRzShowStyle = ( ssFontName, ssFontSample, ssFontNameAndSample, ssFontPreview );

  TRzFontComboBox = class( TRzCustomComboBox )
  private
    FSaveFontName: string;
    FFont: TFont;

    FFontDevice: TRzFontDevice;
    FFontType: TRzFontType;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    FShowSymbolFonts: Boolean;

    FShowStyle: TRzShowStyle;

    FTrueTypeBmp: TBitmap;
    FFixedPitchBmp: TBitmap;
    FTrueTypeFixedBmp: TBitmap;
    FPrinterBmp: TBitmap;
    FDeviceBmp: TBitmap;

    FPreviewVisible: Boolean;
    FPreviewPanel: TRzPreviewFontPanel;
    FPreviewFontSize: Integer;
    FPreviewWidth: Integer;
    FPreviewHeight: Integer;
    FPreviewEdit: TCustomEdit;
    FPreviewText: string;

    FMRUCount: Integer;
    FMaintainMRUFonts: Boolean;

    { Message Handling Methods }
    procedure CNDrawItem( var Msg: TWMDrawItem ); message cn_DrawItem;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMCancelMode( var Msg: TCMCancelMode ); message cm_CancelMode;
    procedure CMHidePreviewPanel( var Msg: TMessage ); message cm_HidePreviewPanel;
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure LoadFonts; virtual;
    procedure LoadBitmaps; virtual;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdatePreviewText;

    procedure HidePreviewPanel; virtual;
    procedure ShowPreviewPanel; virtual;

    { Event Dispatch Methods }
    procedure DropDown; override;
    procedure CloseUp; override;
    procedure DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;

    { Property Access Methods }
    procedure SetFontDevice( Value: TRzFontDevice ); virtual;
    procedure SetFontType( Value: TRzFontType ); virtual;
    function GetSelectedFont: TFont; virtual;
    procedure SetSelectedFont( Value: TFont ); virtual;
    function GetFontName: string; virtual;
    procedure SetFontName( const Value: string ); virtual;
    procedure SetPreviewEdit( Value: TCustomEdit ); virtual;
    procedure SetShowSymbolFonts( Value: Boolean ); virtual;
    procedure SetShowStyle( Value: TRzShowStyle ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property SelectedFont: TFont
      read GetSelectedFont
      write SetSelectedFont;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property FontDevice: TRzFontDevice
      read FFontDevice
      write SetFontDevice
      default fdScreen;

    property FontName: string
      read GetFontName
      write SetFontName;

    property FontSize: Integer
      read FFontSize
      write FFontSize
      default 8;

    property FontStyle: TFontStyles
      read FFontStyle
      write FFontStyle
      default [];

    property FontType: TRzFontType
      read FFontType
      write SetFontType
      default ftAll;

    property MaintainMRUFonts: Boolean
      read FMaintainMRUFonts
      write FMaintainMRUFonts
      default True;

    property PreviewEdit: TCustomEdit
      read FPreviewEdit
      write FPreviewEdit;

    property PreviewFontSize: Integer
      read FPreviewFontSize
      write FPreviewFontSize
      default 36;

    property PreviewHeight: Integer
      read FPreviewHeight
      write FPreviewHeight
      default 65;

    property PreviewText: string
      read FPreviewText
      write FPreviewText;

    property PreviewWidth: Integer
      read FPreviewWidth
      write FPreviewWidth
      default 260;

    property ShowSymbolFonts: Boolean
      read FShowSymbolFonts
      write SetShowSymbolFonts
      default True;

    property ShowStyle: TRzShowStyle
      read FShowStyle
      write SetShowStyle
      default ssFontName;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoCloseUp;
    property AutoDropDown;
    property BeepOnInvalidKey;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount default 14;
    property DropDownWidth;
    property Enabled;
    property FlatButtonColor;
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
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property Sorted default True;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDropDown;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;


  {======================================}
  {== TRzMRUComboBox Class Declaration ==}
  {======================================}

  TRzMRUComboBox = class( TRzCustomComboBox )
  private
    FRemoveItemCaption: string;
    FEmbeddedMenu: TPopupMenu;
    FSelectFirstItemOnLoad: Boolean;
    FMnuUndo: TMenuItem;
    FMnuSeparator1: TMenuItem;
    FMnuCut: TMenuItem;
    FMnuCopy: TMenuItem;
    FMnuPaste: TMenuItem;
    FMnuDelete: TMenuItem;
    FMnuSeparator2: TMenuItem;
    FMnuSelectAll: TMenuItem;
    FMnuSeparator3: TMenuItem;
    FMnuRemove: TMenuItem;

    FMruRegIniFile: TRzRegIniFile;
    FMruPath: string;
    FMruSection: string;
    FMruID: string;
    FMaxHistory: Integer;

    FOnEscapeKeyPressed: TNotifyEvent;
    FOnEnterKeyPressed: TNotifyEvent;

    { Internal Event Handlers }
    procedure EmbeddedMenuPopupHandler( Sender: TObject );
    procedure MnuUndoClickHandler( Sender: TObject );
    procedure MnuCutClickHandler( Sender: TObject );
    procedure MnuCopyClickHandler( Sender: TObject );
    procedure MnuPasteClickHandler( Sender: TObject );
    procedure MnuDeleteClickHandler( Sender: TObject );
    procedure MnuSelectAllClickHandler( Sender: TObject );
    procedure MnuRemoveItemClickHandler( Sender: TObject );
  protected
    FPopupMenuTag: Integer;
    FDataIsLoaded: Boolean;

    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure Loaded; override;
    procedure CreateWnd; override;

    procedure SetupMenuItem( AMenuItem: TMenuItem; ACaption: string;
                             AChecked, ARadioItem: Boolean;
                             AGroupIndex, AShortCut: Integer;
                             AHandler: TNotifyEvent ); dynamic;

    procedure CreatePopupMenuItems; virtual;
    procedure InitializePopupMenuItems; virtual;
    procedure AddMenuItemsToPopupMenu; virtual;

    { Event Dispatch Methods }
    procedure EnterKeyPressed; dynamic;
    procedure EscapeKeyPressed; dynamic;
    procedure KeyPress( var Key: Char ); override;
    procedure CloseUp; override;

    procedure DoExit; override;

    { Property Access Methods }
    procedure SetMruRegIniFile( Value: TRzRegIniFile ); virtual;
    procedure SetRemoveItemCaption( const Value: string );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure LoadMRUData( FromStream: Boolean ); dynamic;
    procedure SaveMRUData; dynamic;
    procedure UpdateMRUList; dynamic;
    procedure UpdateMRUListFromCloseUp;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property MaxHistory: Integer
      read FMaxHistory
      write FMaxHistory
      default 25;

    property MruPath: string
      read FMruPath
      write FMruPath;

    property MruRegIniFile: TRzRegIniFile
      read FMruRegIniFile
      write SetMruRegIniFile;

    property MruSection: string
      read FMruSection
      write FMruSection;

    property MruID: string
      read FMruID
      write FMruID;

    property RemoveItemCaption: string
      read FRemoveItemCaption
      write SetRemoveItemCaption;

    property SelectFirstItemOnLoad: Boolean
      read FSelectFirstItemOnLoad
      write FSelectFirstItemOnLoad
      default False;

    property OnEnterKeyPressed: TNotifyEvent
      read FOnEnterKeyPressed
      write FOnEnterKeyPressed;

    property OnEscapeKeyPressed: TNotifyEvent
      read FOnEscapeKeyPressed
      write FOnEscapeKeyPressed;

    { Inherited Properties & Events }
    property Style;                           { Must be published before Items }
    property Align;
    property AllowEdit;
    property Anchors;
    property AutoComplete;
    property AutoCloseUp;
    property AutoDropDown;
    property BeepOnInvalidKey;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property DropDownWidth;
    property Enabled;
    property FlatButtonColor;
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
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Items;
    property KeepSearchCase;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    {property PopupMenu;}              { Prevent user from modifying PopupMenu }
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    {property Sorted;}                      { An MRU list should not be sorted }
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
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
    property OnMatch;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnNotInList;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

  {=========================================}
  {== TRzImageComboBox Class Declarations ==}
  {=========================================}

  TRzCustomImageComboBox = class;

  TRzImageComboBoxItem = class
  protected
    FOwner: TRzCustomImageComboBox;
    FItemIndex: Integer;

    FIndex: Integer;
    FIndentLevel: Integer;
    FImageIndex: Integer;
    FOverlayIndex: Integer;
    FCaption: string;
    FTag: Integer;
    FData: Pointer;

    procedure SetIndentLevel( Value: Integer );
    procedure SetImageIndex( Value: Integer );
    procedure SetCaption( const Value: string );
    procedure SetOverlayIndex( Value: Integer );
  public
    constructor Create( AOwner: TRzCustomImageComboBox );
    destructor Destroy; override;

    property Index: Integer
      read FIndex;    // Index in the list of the owning combobox

    property IndentLevel: Integer
      read FIndentLevel
      write SetIndentLevel;

    property ImageIndex: Integer
      read FImageIndex
      write SetImageIndex;

    property OverlayIndex: Integer
      read FOverlayIndex
      write SetOverlayIndex;

    property Caption: string
      read FCaption
      write SetCaption;

    property Data: Pointer
      read FData
      write FData;

    property Tag: Integer
      read FTag
      write FTag;
  end;


  TRzDeleteImageComboBoxItemEvent = procedure( Sender: TObject; Item: TRzImageComboBoxItem ) of object;

  TRzImageComboBoxGetItemDataEvent = procedure( Sender: TObject; Item: TRzImageComboBoxItem ) of object;

  TRzCustomImageComboBox = class( TRzCustomComboBox )
  private
    FAutoSizeHeight: Boolean;
    FImages: TCustomImageList;
    FItemIndent: Integer;
    FOnDeleteImageComboBoxItem: TRzDeleteImageComboBoxItemEvent;
    FOnGetItemData: TRzImageComboBoxGetItemDataEvent;
    FInWMSetFont: Boolean;
    FFreeObjOnDelete: Boolean;

    function GetImageComboBoxItem( index: Integer ): TRzImageComboBoxItem;

    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message WM_ERASEBKGND;
    procedure WMSetFont( var Msg: TWMSetFont ); message WM_SETFONT;
  protected
    procedure DoAutoSize( hf: HFont );
    procedure AutoSize( hf: HFont ); dynamic;

    procedure SetItemIndent( Value: Integer );
    procedure SetImages( const Value: TCustomImageList );

    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;

    procedure DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure DeleteItem( Item: Pointer ); override;

    procedure GetItemData( Item: TRzImageComboBoxItem ); virtual;

    property Text
      stored False;

    property AutoSizeHeight: Boolean
      read FAutoSizeHeight
      write FAutoSizeHeight
      default True;

    property ItemIndent: Integer
      read FItemIndent
      write SetItemIndent
      default 12;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property OnDeleteItem: TRzDeleteImageComboBoxItemEvent
      read FOnDeleteImageComboBoxItem
      write FOnDeleteImageComboBoxItem;

    property OnGetItemData: TRzImageComboBoxGetItemDataEvent
      read FOnGetItemData
      write FOnGetItemData;

  public
    constructor Create( AOwner: TComponent ); override;

    function AddItem( Caption: string; ImageIndex: Integer;
                      IndentLevel: Integer ): TRzImageComboBoxItem; reintroduce; virtual;

    procedure ItemsBeginUpdate;
    procedure ItemsEndUpdate;

    procedure Delete( Index: Integer );
    procedure ClearItems;
    function IndexOf( const S: string ): Integer; override;
    function IndexOfItem( Item: TRzImageComboBoxItem ): Integer;
    function IndexOfData( Data: Pointer ): Integer;

    property ImageComboItem[ Index: Integer ]: TRzImageComboBoxItem
      read GetImageComboBoxItem;
  end;


  TRzImageComboBox = class( TRzCustomImageComboBox )
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property AutoCloseUp;
    property AutoDropDown;
    property AutoSizeHeight;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property DropDownWidth;
    property Enabled;
    property Font;
    property FlatButtonColor;
    property FlatButtons;
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
    property Images;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndent;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property Sorted;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDeleteItem;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetItemData;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnSelect;
    property OnSelEndCancel;
    property OnSelEndOk;
    property OnStartDock;
    property OnStartDrag;
  end;


{=========================}
{== ColorItems constant ==}
{=========================}

type
  TRzColorRec = record
    Name: string;
    Color: TColor;
  end;

const
  DefaultColorItem: TRzColorRec =
    ( Name: 'Default';             Color: clBlack );

  CustomColorItem: TRzColorRec =
    ( Name: 'Custom';              Color: clBlack );

  StdColorItems: array[ 0..15 ] of TRzColorRec =
  ( ( Name: 'Black';               Color: clBlack ),
    ( Name: 'Maroon';              Color: clMaroon ),
    ( Name: 'Green';               Color: clGreen ),
    ( Name: 'Olive';               Color: clOlive ),
    ( Name: 'Navy';                Color: clNavy ),
    ( Name: 'Purple';              Color: clPurple ),
    ( Name: 'Teal';                Color: clTeal ),
    ( Name: 'Gray';                Color: clGray ),
    ( Name: 'Silver';              Color: clSilver ),
    ( Name: 'Red';                 Color: clRed ),
    ( Name: 'Lime';                Color: clLime ),
    ( Name: 'Yellow';              Color: clYellow ),
    ( Name: 'Blue';                Color: clBlue ),
    ( Name: 'Fuchsia';             Color: clFuchsia ),
    ( Name: 'Aqua';                Color: clAqua ),
    ( Name: 'White';               Color: clWhite )
  );

  SysColorItems: array[ 0..MaxSysColors - 1 ] of TRzColorRec =
  ( ( Name: 'ScrollBar';           Color: clScrollBar ),
    ( Name: 'Background';          Color: clBackground ),
    ( Name: 'ActiveCaption';       Color: clActiveCaption ),
    ( Name: 'InactiveCaption';     Color: clInactiveCaption ),
    ( Name: 'Menu';                Color: clMenu ),
    ( Name: 'Window';              Color: clWindow ),
    ( Name: 'WindowFrame';         Color: clWindowFrame ),
    ( Name: 'MenuText';            Color: clMenuText ),
    ( Name: 'WindowText';          Color: clWindowText ),
    ( Name: 'CaptionText';         Color: clCaptionText ),
    ( Name: 'ActiveBorder';        Color: clActiveBorder ),
    ( Name: 'InactiveBorder';      Color: clInactiveBorder ),
    ( Name: 'AppWorkSpace';        Color: clAppWorkSpace ),
    ( Name: 'Highlight';           Color: clHighlight ),
    ( Name: 'HighlightText';       Color: clHighlightText ),
    ( Name: 'BtnFace';             Color: clBtnFace ),
    ( Name: 'BtnShadow';           Color: clBtnShadow ),
    ( Name: 'GrayText';            Color: clGrayText ),
    ( Name: 'BtnText';             Color: clBtnText ),
    ( Name: 'InactiveCaptionText'; Color: clInactiveCaptionText ),
    ( Name: 'BtnHighlight';        Color: clBtnHighlight ),

    ( Name: '3DDkShadow';          Color: cl3DDkShadow ),
    ( Name: '3DLight';             Color: cl3DLight ),
    ( Name: 'InfoText';            Color: clInfoText ),
    ( Name: 'InfoBk';              Color: clInfoBk )
  );


const
  ptDefault = 'AaBbYyZz';
  ptDefault1 = 'AaBbYyZ';
  ptDefault2 = 'AaBbYy';
  
implementation

// Link in glyphs for color and font combo boxes
{$R RzCmboBx.res}

uses
  {&RAS}
  Types,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Themes,
  ClipBrd,
  TypInfo,
  Registry,
  IniFiles,
  SysUtils,
  Printers,
  CommCtrl;

{&RT}
{===============================}
{== TRzCustomComboBox Methods ==}
{===============================}

constructor TRzCustomComboBox.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle - [ csSetCaption ];

  inherited AutoComplete := False;
  FAutoComplete := True;
  FTyping := False;
  FShowFocus := True;

  FAllowEdit := True;
  FBeepOnInvalidKey := True;
  FKeepSearchCase := False;
  FSearchString := '';
  FDropDownWidth := 0;

  FTimer := TTimer.Create( nil );
  FTimer.Enabled := False;
  FTimer.OnTimer := SearchTimerExpired;
  FTimer.Interval := IncrementalSearchResetDelay;  // Default 1.5 second delay

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FFlatButtons := False;
  FFlatButtonColor := clBtnFace;
  FDisabledColor := clBtnFace;
  FReadOnlyColor := clInfoBk;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FrameControllerNotifications := fccAll;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
  {&RCI}
end;


procedure TRzCustomComboBox.CreateWnd;
begin
  inherited;
  {&RV}
end;


destructor TRzCustomComboBox.Destroy;
begin
  FTimer.Free;
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


function TRzCustomComboBox.Focused: Boolean;
begin
  // The inherited Focused method does not accurately determine if the control has the focus.
  // Therefore, we update the FIsFocused field in the CMEnter and CMExit methods.
  Result := FIsFocused;
end;


procedure TRzCustomComboBox.ForceText( const Value: string );
begin
  if Text <> Value then
  begin
    FAutoComplete := False;
    try
      Text := Value;
    finally
      FAutoComplete := True;
    end;
  end;
end;


procedure TRzCustomComboBox.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzCustomComboBox.ReadOldFrameFlatProp( Reader: TReader );
begin
  FFrameHotTrack := Reader.ReadBoolean;
  if FFrameHotTrack then
  begin
    // If the FrameFlat property is stored, then init the FrameHotStyle property and the FrameStyle property.
    // These may be overridden when the rest of the stream is read in. However, we need to re-init them here
    // because the default values of fsStatus and fsLowered have changed in RC3.
    FFrameStyle := fsStatus;
    FFrameHotStyle := fsLowered;
  end;
end;


procedure TRzCustomComboBox.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzCustomComboBox.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzCustomComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzCustomComboBox.InvalidKeyPressed;
begin
  if FBeepOnInvalidKey then
    Beep;
end;

{= wm_KeyDown is generated only when ComboBox has csDropDownList style =}

procedure TRzCustomComboBox.WMKeyDown( var Msg: TWMKeyDown );
begin
  if Msg.CharCode in [ vk_Escape, vk_Prior..vk_Down ] then
    FSearchString := '';
  inherited;
end;


function TRzCustomComboBox.FindClosest( const S: string ): Integer;
begin
  Result := SendTextMessage( Handle, cb_FindString, WParam( -1 ), S );
end;


procedure TRzCustomComboBox.UpdateIndex( const FindStr: string; Msg: TWMChar );
var
  Index: Integer;
begin
  Index := FindClosest( FindStr );
  if Index <> -1 then
  begin
    ItemIndex := Index;
    FSearchString := FindStr;
    Click;
    Change;
    DoKeyPress( Msg );
  end
  else
    InvalidKeyPressed;
end;



procedure TRzCustomComboBox.SearchTimerExpired( Sender: TObject );
begin
  if FKeyCount = 0 then
  begin
    FTimer.Enabled := False;
    FSearchString := '';
  end;
end;


procedure TRzCustomComboBox.EditWndProc( var Msg: TMessage );
var
  P: TPoint;
  LMouseEvent: TTrackMouseEvent;
begin
  if Msg.Msg = wm_MouseMove then
  begin
    if not FOverEditArea then
    begin
      // We are not over the edit area of the combo. Register a tracking
      // event so that we get notified when the mouse leaves the edit area.
      FOverEditArea := True;
      LMouseEvent.cbSize := SizeOf(LMouseEvent);
      LMouseEvent.dwFlags := TME_LEAVE;
      LMouseEvent.hwndTrack := EditHandle;
      LMouseEvent.dwHoverTime := HOVER_DEFAULT;
      _TrackMouseEvent(@LMouseEvent);
    end;
  end;

  if Msg.Msg = wm_MouseLeave then
  begin
    FOverEditArea := False;
    // Technically we are not over the edit window anymore, but we may be
    // still inside the combo box, just over the button.
    GetCursorPos( P );
    P := ScreenToClient( P );
    // Only generate the OnMouseLeave if outside the control
    if not PtInRect( ClientRect, P ) then
      Perform( CM_MOUSELEAVE, 0, 0 );
  end;

  inherited;
end;



procedure TRzCustomComboBox.WndProc( var Msg: TMessage );
var
  TempStr: string;
  P: TPoint;
  Target, CaptureControl: TControl;
  LMouseEvent: TTrackMouseEvent;
begin
  if Style in [ csDropDown, csSimple ] then
  begin
    if Msg.Msg = wm_MouseLeave then
    begin
      FMouseInClient := False;
      if not FOverEditArea then
      begin
        Perform( CM_MOUSELEAVE, 0, 0 );
        FMouseControl := nil;
      end;

      inherited; // Added in 5.1 to allow Vista to animate dropdown button
      Exit;
    end;

    if Msg.Msg = wm_MouseMove then
    begin
      P := ClientToScreen( Point( TWMMouse( Msg ).XPos, TWMMouse( Msg ).YPos ) );
      Target := FindDragTarget( P, True );
      CaptureControl := GetCaptureControl;
      if ( FMouseControl <> Target ) then
      begin
        if ( ( FMouseControl <> nil ) and ( CaptureControl = nil ) ) or
           ( ( CaptureControl <> nil ) and ( FMouseControl = CaptureControl ) ) then
        begin
          FMouseControl.Perform( CM_MOUSELEAVE, 0, 0 );
        end;

        if FMouseControl <> nil then
          FMouseControl.RemoveFreeNotification( Self );

        FMouseControl := Target;

        if FMouseControl <> nil then
          FMouseControl.FreeNotification( Self );

        if ( ( FMouseControl <> nil ) and ( CaptureControl = nil ) ) or
           ( ( CaptureControl <> nil ) and ( FMouseControl = CaptureControl ) ) then
        begin
          FMouseControl.Perform(CM_MOUSEENTER, 0, 0);
        end;
      end;

      if not FMouseInClient then
      begin
        FMouseInClient := True;
        // Register for a WM_MOUSELEAVE message which ensures CM_MOUSELEAVE
        // is called when the mouse leaves the TWinControl
        LMouseEvent.cbSize := SizeOf(LMouseEvent);
        LMouseEvent.dwFlags := TME_LEAVE;
        LMouseEvent.hwndTrack := Handle;
        LMouseEvent.dwHoverTime := HOVER_DEFAULT;
        _TrackMouseEvent(@LMouseEvent);
      end;

      inherited; // Added in 5.1 to allow Vista to animate dropdown button
      Exit;
    end;
  end;

  if ( Msg.Msg = wm_KeyDown ) or ( Msg.Msg = wm_SysKeyDown ) then
  begin
    if FReadOnly then
      Exit;
  end;

  if Msg.Msg = wm_Char then
  begin
    if FReadOnly then
      Exit;
    TempStr := FSearchString;

    case TWMKey( Msg ).CharCode of
      vk_Back:
      begin
        if Length( TempStr ) > 0 then
        begin
          while ByteType( TempStr, Length( TempStr ) ) = mbTrailByte do
            System.Delete( TempStr, Length( TempStr ), 1 );
          System.Delete( TempStr, Length( TempStr ), 1 );

          if Length( TempStr ) = 0 then
          begin
            ItemIndex := 0;
            Click;
            Change;
            FSearchString := '';
            DoKeyPress( TWMKey( Msg ) );
            Exit;
          end;
        end
        else
          InvalidKeyPressed;

        UpdateIndex( TempStr, TWMKey( Msg ) );
      end;

      32..255:
      begin
        FKeyCount := 1;
        TempStr := TempStr + Char( TWMKey( Msg ).CharCode );
        UpdateIndex( TempStr, TWMKey( Msg ) );

        FTimer.Enabled := False;
        FTimer.Interval := IncrementalSearchResetDelay;
        FTimer.Enabled := True;
        FKeyCount := 0;
      end;

      else
        inherited;
    end;
  end
  else
    inherited;
end; {= TRzCustomComboBox.WndProc =}



procedure TRzCustomComboBox.Match;
begin
  if Assigned( FOnMatch ) then
    FOnMatch( Self );
end;


function TRzCustomComboBox.FindListItem( const FindStr: string; Msg: TMessage ): Boolean;
var
  OldIndex, Index: Integer;
begin
  OldIndex := ItemIndex;
  Index := FindClosest( FindStr );
  if Index <> -1 then
  begin
    if FKeepSearchCase then
    begin
      FTyping := True;
      try
        Text := FindStr + Copy( Items[ Index ], Length( FindStr ) + 1, MaxInt );
      finally
        FTyping := False;
      end;
    end
    else
    begin
      // Set ItemIndex to -1 in case ItemIndex is already equal to Index
      ItemIndex := -1;
      ItemIndex := Index;
    end;

    Click;
    Change;
    DoKeyPress( TWMKey( Msg ) );

    SelStart := Length( FindStr );
    SelLength := Length( Items[ Index ] ) - SelStart;

    if ItemIndex <> OldIndex then
      Match;
    Result := True;
  end
  else
    Result := False;
end;


{= ComboWndProc has keyboard actions when Style is csSimple or csDropDown =}

procedure TRzCustomComboBox.ComboWndProc( var Msg: TMessage; ComboWnd: HWnd; ComboProc: Pointer );
var
  TempStr, OldSearchString: string;
  Mask: LongWord;
  PasteViaShiftInsert: Boolean;
  CW, OldSelStart: Integer;
  PeekMsg: TMsg;

  procedure DeleteSelectedText( var S: string );
  var
    StartPos, EndPos: DWord;
    OldText: string;
  begin
    OldText := S;
    SendMessage( Handle, CB_GETEDITSEL, WParam( @StartPos ), LParam( @EndPos ) );
    System.Delete( OldText, StartPos + 1, EndPos - StartPos );
    SendMessage( Handle, CB_SETCURSEL, WParam( -1 ), 0 );
    S := OldText;
    SendMessage( Handle, CB_SETEDITSEL, 0, MakeLParam( StartPos, StartPos ) );
  end;

begin
  PasteViaShiftInsert := False;
  if FAutoComplete then
  begin
    case Msg.Msg of
      wm_LButtonDown:
      begin
        if Style = csDropDown then
          FSearchString := Text;
      end;

      wm_Char:
      begin
        if not FReadOnly then
        begin
          TempStr := FSearchString;

          case Msg.WParam of
            vk_Return:
            begin
              if DroppedDown then
                SelEndOk;
              FEnterPressed := True;
            end;

            vk_Back:
            begin
              if Length( TempStr ) > 0 then
              begin
                if SelStart >= Length( FSearchString ) then
                begin
                  while ByteType( TempStr, Length( TempStr ) ) = mbTrailByte do
                    System.Delete( TempStr, Length( TempStr ), 1 );
                  System.Delete( TempStr, Length( TempStr ), 1 );
                end
                else if ( SelLength > 0 ) and FAllowEdit then
                begin
                  DeleteSelectedText( TempStr );
                end
                else if FAllowEdit then
                begin
                  CW := 1;
                  if ByteType( TempStr, SelStart ) = mbTrailByte then
                    Inc( CW );
                  System.Delete( TempStr, SelStart, CW );
                end;

                Change;
                if Length( TempStr ) = 0 then
                begin
                  ItemIndex := -1;
                  if FKeepSearchCase then
                    Text := '';
                  FSearchString := '';
                  Click;
                  Change;
                  DoKeyPress( TWMKey( Msg ) );
                end;
              end
              else
                InvalidKeyPressed;

              FSearchString := TempStr;
              if FindListItem( FSearchString, Msg ) then
                Exit;

            end; { vk_Back }

            vk_Escape:
            begin
              FSearchString := '';
              if not DroppedDown then
                Exit;
            end;

            22, 24: { Ctrl+V, Ctrl+X }
            begin
              if not FAllowEdit then
              begin
                Msg.WParam := 0;
              end;
            end;

            else
            begin
              if Msg.WParam >= 32 then
              begin
                // Do not process key if SysKey (i.e. Alt) was pressed and the WParam is Up or Down arrow.
                // This means that the combo box was opened/closed with Alt+DownArrow or Alt+UpArrow

                if not FSysKeyDown or ( FSysKeyDown and ( Msg.WParam <> 9787 ) and ( Msg.WParam <> 9688 ) ) then
                begin
                  // Invoke any user defined OnKeyPress handlers
                  DoKeyPress( TWMKey( Msg ) );
                  // Then use NEW character in case user changed it
                  if Msg.WParam >= 32 then
                  begin
                    // If text is selected, it will be erased when new char is inserted.
                    // Therefore, delete the selected text from the search string

                    if SelLength > 0 then
                      System.Delete( FSearchString, SelStart + 1, SelLength );


                    OldSearchString := FSearchString;
                    System.Insert( Char( Msg.WParam ), FSearchString, SelStart + 1 );
                    if IsLeadChar( Char( Msg.WParam ) ) then
                    begin
                      if PeekMessage( PeekMsg, Handle, 0, 0, pm_NoRemove ) and ( PeekMsg.Message = wm_Char ) then
                        System.Insert( Char( PeekMsg.WParam ), FSearchString, SelStart + 2 );
                    end;

                    if FindListItem( FSearchString, Msg ) then
                    begin
                      if IsLeadChar( Char( Msg.WParam ) ) then
                      begin
                        // Only need to remove next message if current char
                        // (Msg.WParam) is in LeadBytes
                        PeekMessage( PeekMsg, Handle, 0, 0, pm_Remove );
                      end;
                      Exit;
                    end
                    else
                    begin
                      if FAllowEdit then
                      begin
                        OldSelStart := SelStart;
                        FTyping := True;
                        try
                          Text := OldSearchString;
                        finally
                          FTyping := False;
                        end;
                        SelStart := OldSelStart;
                        SelLength := 0;
                      end
                      else
                      begin
                        InvalidKeyPressed;
                        FSearchString := OldSearchString;
                        Msg.WParam := 0;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end
        else // FReadOnly
        begin
          if FTabOnEnter and ( Msg.WParam = vk_Return ) and not DroppedDown then
            PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );

          if Msg.WParam <> 3 then  // <> Ctrl+C
            Exit;
        end;
      end; { wm_Char }

      wm_KeyDown:
      begin
        FSysKeyDown := False;
        case Msg.WParam of
          vk_Insert:
          begin
            Mask := $80000000;
            if ( GetKeyState( vk_Shift ) and Mask ) = Mask then
              PasteViaShiftInsert := True;
            if not FAllowEdit then
            begin
              Msg.WParam := 0;
            end;
          end;

          vk_Delete:
          begin
            if FReadOnly then
              Exit;

            FSearchString := Text;

            // Check current character to see if it is a lead byte
            CW := 1;
            if ByteType( FSearchString, SelStart + 1 ) = mbLeadByte then
              Inc( CW );
            System.Delete( FSearchString, SelStart + 1, Max( SelLength, CW ) );

            if not FAllowEdit and ( SelLength < Length( Text ) ) then
              Msg.WParam := 0;
          end;

          vk_End, vk_Home, vk_Left, vk_Right:
          begin
            FSearchString := Text;
          end;

          vk_Prior, vk_Next, vk_Up, vk_Down:
          begin
            FTyping := True;
            FSearchString := '';
            if FReadOnly then
              Exit;
          end;

          vk_F4:
          begin
            if FReadOnly then
              Exit;
          end;
        end; { case }
      end; { wm_KeyDown }

      wm_SysKeyDown:
      begin
        FSysKeyDown := True;
        if FReadOnly and ( Msg.WParam <> vk_F4 ) then
          Exit;
      end;
    end;
  end; { if FAutoComplete }

  inherited;

  if FAutoComplete then
  begin
    { Handle Ctrl+V and Ctrl+X and Shift+Insert combinations }
    if PasteViaShiftInsert or
       ( ( Msg.Msg = wm_Char ) and
         ( ( Msg.WParam = 22 ) or ( Msg.WParam = 24 ) ) ) then
    begin
      if FAllowEdit then
      begin
        FSearchString := Text;
        FindListItem( FSearchString, Msg );
      end;
    end;
  end;
end; {= TRzCustomComboBox.ComboWndProc =}


procedure TRzCustomComboBox.UpdateSearchStr;
var
  Index: Integer;
begin
  if Style = csDropDown then
  begin
    if not FEnterPressed then
      FSearchString := Text
    else
      FEnterPressed := False;

    Index := FindClosest( FSearchString );

    if Index <> -1 then
    begin
      ItemIndex := Index;
      SelStart := Length( FSearchString );
      SelLength := Length( Items[ ItemIndex ] ) - SelStart;
    end;
  end;
end;


procedure TRzCustomComboBox.ClearSearchString;
begin
  FSearchString := '';
end;


procedure TRzCustomComboBox.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) and not DroppedDown then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
  begin
    if ( Style = csDropDown ) and
       ( Key >= #32 ) and
       ( MaxLength > 0 ) and
       ( Length( Text ) >= MaxLength ) and
       ( SelLength = 0 ) then
    begin
      InvalidKeyPressed;
      Key := #0;
    end;


    inherited;

    // Setting AutoDropDown does not cause the list to drop down when the user starts typing.  This is because the
    // inherited KeyPress event dispatch method, which implements the AutoDropDown functionality immediately exits if
    // AutoComplete is turned off.  The problem is that the RC combo boxes turn off the inherited AutoComplete
    // functionality so that it doesn't interfere with our own. The following case statement mimics the AutoDropDown
    // functionality.

    case Ord( Key ) of
      vk_Escape, vk_Back:
        ;
      vk_Tab:
        if AutoDropDown and DroppedDown then
          DroppedDown := False;
      else
        if AutoDropDown and not DroppedDown then
          DroppedDown := True;
    end;
  end;
end;


procedure TRzCustomComboBox.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  if FAutoComplete then
  begin
    if not ( csDesigning in ComponentState ) and not FTyping and not DroppedDown then
      UpdateSearchStr;
  end;
  FTyping := False;
end;


procedure TRzCustomComboBox.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzCustomComboBox.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
end;


procedure TRzCustomComboBox.WMCut( var Msg: TMessage );
begin
  if FAllowEdit then
  begin
    FSearchString := Text;
    FindListItem( FSearchString, Msg );
    inherited;
  end;
end;


procedure TRzCustomComboBox.WMPaste( var Msg: TMessage );
begin
  if FAllowEdit then
  begin
    inherited;
    FSearchString := Text;
    FindListItem( FSearchString, Msg );
  end;
end;


procedure TRzCustomComboBox.WMKillFocus( var Msg: TWMKillFocus );
begin
  inherited;
  FSearchString := '';
end;


procedure TRzCustomComboBox.CNCommand( var Msg: TWMCommand );
begin
  inherited;
  case Msg.NotifyCode of
    cbn_SelEndOk:
    begin
      // Setting FTyping to True here is necessary to allow a user to select
      // an item from the list that happens to be a substring of another item
      // in the list. If FTyping is not set to True, then the other item will
      // be placed in the edit area if it occurs earlier in the list.
      FTyping := True;
      SelEndOk;
    end;

    cbn_SelEndCancel:
    begin
      SelEndCancel;
    end;
  end;
end;


procedure TRzCustomComboBox.CNDrawItem( var Msg: TWMDrawItem );
var
  State: TOwnerDrawState;
  ItemColor, ItemFontColor: TColor;
begin
  State := TOwnerDrawState( LongRec( Msg.DrawItemStruct^.itemState ).Lo );

  if Msg.DrawItemStruct^.itemState and ODS_COMBOBOXEDIT <> 0 then
    Include( State, odComboBoxEdit );

  if Msg.DrawItemStruct^.itemState and ODS_DEFAULT <> 0 then
    Include( State, odDefault );

  Canvas.Handle := Msg.DrawItemStruct^.hDC;
  Canvas.Font := Font;
  Canvas.Brush := Brush;

  if UsingSystemStyle then
  begin
    ItemColor := Brush.Color;
    ItemFontColor := Font.Color;
  end
  else
  begin
    ItemColor := ActiveStyleColor( scListBox );
    ItemFontColor := ActiveStyleFontColor( sfListItemTextNormal );
  end;
  Canvas.Font.Color := ItemFontColor;
  Canvas.Brush.Color := ItemColor;


  if ( Integer( Msg.DrawItemStruct^.itemID ) >= 0 ) and ( odSelected in State ) then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Font.Color := clHighlightText
  end;

  if Integer( Msg.DrawItemStruct^.itemID ) >= 0 then
  begin
    DrawItem( Msg.DrawItemStruct.itemID, Msg.DrawItemStruct^.rcItem, State );
    if ( odSelected in State ) and FShowFocus then
      DrawFocusRect( Canvas.Handle, Msg.DrawItemStruct^.rcItem );
  end
  else
  begin
    if odSelected in State then
      DrawFocusRect( Canvas.Handle, Msg.DrawItemStruct^.rcItem )
    else
      Canvas.FillRect( Msg.DrawItemStruct^.rcItem );
  end;

  Canvas.Handle := 0;
end;


procedure TRzCustomCombobox.WMDeleteItem( var Msg: TWMDeleteItem );
begin
  if Msg.deleteItemStruct.itemData <> 0 then
  begin  // Windows NT4 can send some strange WM_DELETEITEM messages. We filter them here.
    DeleteItem( Pointer( Msg.deleteItemStruct.itemData ) );
    Pointer( Msg.deleteItemStruct.itemData ) := nil;
  end;

  inherited;
end;


procedure TRzCustomComboBox.WMLButtonDown( var Msg: TWMLButtonDown );
begin
  if FReadOnly then
    Exit;
  inherited;
end;


procedure TRzCustomComboBox.WMLButtonDblClick( var Msg: TWMLButtonDblClk );
begin
  if FReadOnly then
    Exit;
  inherited;
end;


procedure TRzCustomComboBox.DropDown;
var
  I, W: Integer;
begin
  inherited;

  if FDropDownWidth > 0 then
  begin
    FActualDropDownWidth := FDropDownWidth;
  end
  else // Automatically adjust width of drop down list to show all items
  begin
    FActualDropDownWidth := 0;
    Canvas.Font := Self.Font;
    for I := 0 to Items.Count - 1 do
    begin
      W := Canvas.TextWidth( Items[ I ] );
      if W > FActualDropDownWidth then
        FActualDropDownWidth := W;
    end;
    // Determine if vertical scroll bar is visible
    if Items.Count > DropDownCount then
      FActualDropDownWidth := FActualDropDownWidth + GetSystemMetrics( sm_CxVScroll );
    Inc( FActualDropDownWidth, 8 );
    if FActualDropDownWidth < Width then
      FActualDropDownWidth := Width;
  end;
  SendMessage( Handle, cb_SetDroppedWidth, FActualDropDownWidth, 0 );
end;



procedure TRzCustomComboBox.CloseUp;
begin
  inherited;
  Invalidate;
end;


procedure TRzCustomCombobox.DeleteItem( Item: Pointer );
begin
  if Assigned( FOnDeleteItem ) then
    FOnDeleteItem( Self, Item );
end;


procedure TRzCustomCombobox.SelEndCancel;
begin
  if Assigned( FOnSelEndCancel ) then
    FOnSelEndCancel( Self );
end;


procedure TRzCustomCombobox.SelEndOk;
begin
  if Assigned( OnSelEndOk ) then
    OnSelEndOk( Self );
end;



function TRzCustomComboBox.Add( const S: string ): Integer;
begin
  Result := Items.Add( S );
end;

function TRzCustomComboBox.AddObject( const S: string; AObject: TObject ): Integer;
begin
  Result := Items.AddObject( S, AObject );
end;

procedure TRzCustomComboBox.Delete( Index: Integer );
begin
  Items.Delete( Index );
end;


procedure TRzCustomComboBox.ClearItems;
begin
  Items.Clear;
end;


function TRzCustomComboBox.IndexOf( const S: string ): Integer;
begin
  Result := Items.IndexOf( S );
end;

procedure TRzCustomComboBox.Insert( Index: Integer; const S: string );
begin
  Items.Insert( Index, S );
end;

procedure TRzCustomComboBox.InsertObject( Index: Integer; const S: string; AObject: TObject );
begin
  Items.InsertObject( Index, S, AObject );
end;

function TRzCustomComboBox.Count: Integer;
begin
  Result := Items.Count;
end;


function TRzCustomComboBox.FindItem( const S: string ): Boolean;
var
  Idx: Integer;
begin
  Idx := Items.IndexOf( S );
  if Idx <> -1 then
    ItemIndex := Idx;
  Result := Idx <> -1;
end;


function TRzCustomComboBox.FindClosestItem( const S: string ): Boolean;
var
  Idx: Integer;
begin
  Idx := FindClosest( S );
  if Idx <> -1 then
    ItemIndex := Idx;
  Result := Idx <> -1;
end;


procedure TRzCustomComboBox.SetReadOnly( Value: Boolean );
var
  H: HWnd;
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;

    H := EditHandle;
    if HandleAllocated then
      SendMessage( H, em_SetReadOnly, Ord( FReadOnly ), 0 );

    UpdateColors;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetReadOnlyColor( Value: TColor );
begin
  FReadOnlyColor := Value;
  if ReadOnly then
    UpdateColors;
end;


procedure TRzCustomComboBox.SetFlatButtonColor( Value: TColor );
begin
  if FFlatButtonColor <> Value then
  begin
    FFlatButtonColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFlatButtons( Value: Boolean );
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    Invalidate;
  end;
end;


function TRzCustomComboBox.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzCustomComboBox.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzCustomComboBox.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreReadOnlyColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColor in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreReadOnlyColorOnFocus: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpReadOnlyColorOnFocus in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzCustomComboBox.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzCustomComboBox.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzCustomComboBox.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzCustomComboBox.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomComboBox.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFrameHotTrack( Value: Boolean );
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
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    ParentCtl3D := not FFrameVisible;
    Ctl3D := not FFrameVisible;
    Invalidate;
  end;
end;


procedure TRzCustomComboBox.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      Invalidate;
  end;
end;


function TRzCustomComboBox.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzCustomComboBox.WMPaint( var Msg: TWMPaint );
var
  BtnRect, TempRect, R: TRect;
  Offset: Integer;
  ElementDetails: TThemedElementDetails;
begin
  inherited;

  if ActiveStyleServicesEnabled then
    Offset := 1
  else
    Offset := 2;
  if not UseRightToLeftAlignment then
    BtnRect := Rect( Width - GetSystemMetrics( sm_CxVScroll ) - Offset, Offset, Width - Offset, Height - Offset )
  else
    BtnRect := Rect( Offset, Offset, GetSystemMetrics( sm_CxVScroll ) + Offset, Height - Offset );

  if FFrameVisible and not UseThemes and
     ( not ( RunningAtLeast( WinVista ) and ActiveStyleServicesEnabled ) or ( Style <> csDropDownList ) ) then
  begin
    // Erase Ctl3D Border
    if ActiveStyleServicesEnabled then
    begin
      DrawBox( FCanvas, ClientRect, Color );

      R := ClientRect;
      if not UseRightToLeftAlignment then
        R.Right := BtnRect.Left
      else
        R.Left := BtnRect.Right;

      InflateRect( R, -1, -1 );
      DrawBevel( FCanvas, R, Color, Color, 2, sdAllSides );
    end
    else
      DrawBevel( FCanvas, ClientRect, Color, Color, 2, sdAllSides );
  end
  else if ActiveStyleServicesEnabled then
  begin
    // Remove white border inside blue flat border when XP Themes in use
    R := ClientRect;
    if not UseRightToLeftAlignment then
      R.Right := BtnRect.Left
    else
      R.Left := BtnRect.Right;

    InflateRect( R, -1, -1 );

    if ( Style <> csDropDownList ) or not RunningAtLeast( winVista ) or not FReadOnly then
    begin
      // This block gets skipped if Style = csDropDownList and running Vista or higher, and ReadOnly
      // which is necessary because of the way Vista/Win7 draw drop down list combo boxes.
      if ColorToRGB( Color ) <> clWhite then
        DrawBevel( FCanvas, R, Color, Color, 2, sdAllSides );
    end;
  end;

  if ActiveStyleServicesEnabled and ( Style <> csSimple ) and
     ( RunningUnder( winXP ) or
       ( RunningAtLeast( winVista ) and ( Style <> csDropDownList ) ) ) then
  begin
    // Fill in the gap between the edit area and the dropdown button in themes
    R := BtnRect;
    if not UseRightToLeftAlignment then
    begin
      R.Right := R.Left;
      Dec( R.Left, 2 );
    end
    else
    begin
      R.Left := R.Right;
      Inc( R.Right, 2 );
    end;

    FCanvas.Brush.Color := Color;
    FCanvas.FillRect( R );
  end;

  // The Visual Style of combo boxes under Vista eliminates the need for FlatButtons.
  // Therefore, we ignore the property value if we are running under Vista
  // and Themes are being used.

  if FFlatButtons and not FReadOnly and
     ( not ( RunningAtLeast( WinVista ) and ActiveStyleServicesEnabled ) or ( Style <> csDropDownList ) ) then
  begin
    if not ( FInControl or FOverControl ) then
    begin
      // Erase Button Border
      if UsingSystemStyle then
        FCanvas.Brush.Color := Color
      else
        FCanvas.Brush.Color := ActiveStyleColor( scEdit );
      FCanvas.FillRect( BtnRect );

      if ActiveStyleServicesEnabled then
        DrawDropDownArrow( FCanvas, BtnRect, uiWindowsXP, False, Enabled )
      else
        DrawDropDownArrow( FCanvas, BtnRect, uiWindows95, False, Enabled );
    end
    else
    begin
      // Erase Button Border
      if ActiveStyleServicesEnabled then
      begin
        if DroppedDown then
          ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonPressed )
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( tcDropDownButtonHot );

        ActiveStyleServices.DrawElement( FCanvas.Handle, ElementDetails, BtnRect );
      end
      else // No Themes
      begin
        FCanvas.Brush.Color := FFlatButtonColor;

        if FFlatButtonColor = clBtnFace then
        begin
          if DroppedDown then
            TempRect := DrawBevel( FCanvas, BtnRect, clBtnShadow, clBtnHighlight, 1, sdAllSides )
          else
            TempRect := DrawBevel( FCanvas, BtnRect, clBtnHighlight, clBtnShadow, 1, sdAllSides );
        end
        else
        begin
          if DroppedDown then
            TempRect := DrawColorBorder( FCanvas, BtnRect, FFlatButtonColor, fsStatus )
          else
            TempRect := DrawColorBorder( FCanvas, BtnRect, FFlatButtonColor, fsPopup );
        end;

        FCanvas.FillRect( TempRect );
        DrawDropDownArrow( FCanvas, TempRect, uiWindows95, DroppedDown, Enabled );
      end;
    end;
  end
  else if FReadOnly and ( Style <> csDropDownList ) then
  begin
    // Erase drop down button
    FCanvas.Brush.Color := Color;
    FCanvas.FillRect( BtnRect );
  end;

  if FFrameVisible and not UseThemes then
  begin
    if FFrameHotTrack and ( FInControl or FOverControl ) then
    begin
      if FFrameHotStyle = fsFlat then
        DrawSides( FCanvas, ClientRect, FFrameHotColor, FFrameHotColor, FFrameSides )
      else if FFrameHotStyle = fsFlatBold then
        DrawBevel( FCanvas, ClientRect, FFrameHotColor, FFrameHotColor, 2, FFrameSides )
      else if Color = clWindow then
        DrawBorderSides( FCanvas, ClientRect, FFrameHotStyle, FFrameSides )
      else
        DrawColorBorderSides( FCanvas, ClientRect, Color, FFrameHotStyle, FFrameSides );
    end
    else
    begin
      if FFrameStyle = fsFlat then
        DrawSides( FCanvas, ClientRect, FFrameColor, FFrameColor, FFrameSides )
      else if FFrameStyle = fsFlatBold then
        DrawBevel( FCanvas, ClientRect, FFrameColor, FFrameColor, 2, FFrameSides )
      else if Color = clWindow then
        DrawBorderSides( FCanvas, ClientRect, FFrameStyle, FFrameSides )
      else
        DrawColorBorderSides( FCanvas, ClientRect, Color, FFrameStyle, FFrameSides );
    end;
  end;
end; {= TRzCustomComboBox.WMPaint =}


procedure TRzCustomComboBox.UpdateColors;
begin
  if csLoading in ComponentState then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
    begin
      if ReadOnly and FReadOnlyColorOnFocus then
        Color := FReadOnlyColor
      else
        Color := FFocusColor;
    end
    else if ReadOnly then
      Color := FReadOnlyColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;



procedure TRzCustomComboBox.UpdateFrame( ViaMouse, InFocus: Boolean );
var
  PaintIt: Boolean;
  R: TRect;
begin
  if ViaMouse then
    FOverControl := InFocus
  else
    FInControl := InFocus;

  PaintIt := ( FFlatButtons and not ( RunningAtLeast( WinVista ) and ActiveStyleServicesEnabled ) ) or FFrameHotTrack;

  if PaintIt and not DroppedDown then
  begin
    R := ClientRect;
    if not FFrameHotTrack then
      R.Left := R.Right - GetSystemMetrics( sm_CxVScroll ) - 2;
    RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_NoErase );
  end;

  UpdateColors;
end;


procedure TRzCustomComboBox.CMEnter( var Msg: TCMEnter );
begin
  inherited;
  FIsFocused := True;
  UpdateFrame( False, True );
end;


procedure TRzCustomComboBox.NotInList;
begin
  if Assigned( FOnNotInList ) then
    FOnNotInList( Self );
end;

procedure TRzCustomComboBox.CMExit( var Msg: TCMExit );
begin
  inherited;
  FIsFocused := False;
  UpdateFrame( False, False );

  if ( Style = csDropDown ) and ( Text <> '' ) and not ( Items.IndexOf( Text ) <> -1 ) then
    NotInList;
end;


procedure TRzCustomComboBox.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzCustomComboBox.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzCustomComboBox.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    Invalidate;
end;


procedure TRzCustomComboBox.SetItemHeight2( Value: Integer );
begin
  if ( ItemHeight <> Value ) and ( Style in [ csOwnerDrawFixed, csOwnerDrawVariable ] ) then
  begin
    inherited ItemHeight := Value;
    RecreateWnd;
  end;
end;


procedure TRzCustomComboBox.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
end;


{=========================}
{== TRzComboBox Methods ==}
{=========================}

constructor TRzComboBox.Create( AOwner: TComponent );
begin
  inherited;
  FValues := TStringList.Create;
  TStringList( FValues ).OnChange := ValuesChangedHandler;
end;


destructor TRzComboBox.Destroy;
begin
  FValues.Free;
  inherited;
end;


procedure TRzComboBox.ClearItemsValues;
begin
  Items.Clear;
  Values.Clear;
end;


procedure TRzComboBox.AddItemValue( const Item, Value: string );
var
  Idx: Integer;
begin
  Idx := Items.Add( Item );
  Values.Insert( Idx, Value );
end;


procedure TRzComboBox.InsertItemValue( Index: Integer; const Item, Value: string );
begin
  Items.Insert( Index, Item );
  Values.Insert( Index, Value );
end;


procedure TRzComboBox.DeleteItemValue( Index: Integer );
begin
  Items.Delete( Index );
  Values.Delete( Index );
end;


procedure TRzComboBox.ValuesChangedHandler( Sender: TObject );
begin
  FForceSetValue := True;
  try
    Value := GetValue;
  finally
    FForceSetValue := False;
  end;
end;


function TRzComboBox.GetItemValue( Index: Integer ): string;
begin
  if ( Index < FValues.Count ) and ( FValues[ Index ] <> '' ) then
    Result := FValues[ Index ]
  else if Index < Items.Count then
    Result := Items[ Index ]
  else
    Result := '';
end;


function TRzComboBox.GetValue: string;
var
  I: Integer;
  S: string;
begin
  if Style in [ csDropDown, csSimple ] then
  begin
    S := Text;
    if S = '' then
      I := -1
    else
      I := Items.IndexOf( S );
  end
  else
    I := ItemIndex;

  if ( I < 0 ) or ( I > ( Values.Count - 1 ) ) or ( Values[ I ] = '' ) then
    Result := Text
  else
  begin
    FValue := Values[ I ];
    Result := Values[ I ];
  end;
end;


procedure TRzComboBox.SetValue( const Value: string );
var
  I, Index: Integer;
begin
  if ( FValue <> Value ) or FForceSetValue then
  begin
    FInSetValue := True;
    try
      Index := -1;
      for I := 0 to Items.Count - 1 do
      begin
        if Value = GetItemValue( I ) then
        begin
          Index := I;
          Break;
        end;
      end;
      ItemIndex := Index;
    finally
      FInSetValue := False;
    end;
    FValue := Value;
  end;
end;


procedure TRzComboBox.Change;
begin
  if not FInSetValue and not FForceSetValue then
  begin
    inherited;
  end;
end;


procedure TRzComboBox.Click;
begin
  if not FInSetValue then
  begin
    inherited;
  end;
end;


procedure TRzComboBox.SetValues( Value: TStrings );
begin
  FValues.Assign( Value );
end;



{===========================}
{== TRzColorNames Methods ==}
{===========================}

constructor TRzColorNames.Create;
begin
  inherited;
  ShowDefaultColor := True;
  ShowCustomColor := True;
  ShowSysColors := True;
end;

procedure TRzColorNames.Assign( Source: TPersistent );
var
  I: Integer;
begin
  if Source is TRzColorNames then
  begin
    if ShowDefaultColor then
      FDefaultColor := TRzColorNames( Source ).Default;

    for I := 0 to MaxStdColors - 1 do
      SetStdColor( I, TRzColorNames( Source ).GetStdColor( I ) );

    if ShowSysColors then
    begin
      for I := 0 to MaxSysColors - 1 do
        SetSysColor( I, TRzColorNames( Source ).GetSysColor( I ) );
    end;

    if ShowCustomColor then
      FCustomColor := TRzColorNames( Source ).Custom;

    Exit;
  end;
  inherited;
end;


procedure TRzColorNames.SetDefaultColor( const Value: string );
var
  Idx: Integer;
begin
  FDefaultColor := Value;
  if ( FComboBox <> nil ) and ShowDefaultColor then
  begin
    Idx := FComboBox.ItemIndex;
    FComboBox.Items[ 0 ] := Value;
    FComboBox.ItemIndex := Idx;
    FComboBox.FStoreColorNames := True;
  end;
end;


function TRzColorNames.GetStdColor( Index: Integer ): string;
begin
  Result := FStdColors[ Index ];
end;


procedure TRzColorNames.SetStdColor( Index: Integer; const Value: string );
var
  Idx, ColorIdx: Integer;
begin
  FStdColors[ Index ] := Value;
  if FComboBox <> nil then
  begin
    Idx := FComboBox.ItemIndex;
    ColorIdx := Index;
    if ShowDefaultColor then
      Inc( ColorIdx );
    if ( ColorIdx > 0 ) and ( ColorIdx < FComboBox.Items.Count ) then
    begin
      FComboBox.Items[ ColorIdx ] := Value;
      FComboBox.ItemIndex := Idx;
      FComboBox.FStoreColorNames := True;
    end;
  end;
end;


function TRzColorNames.GetSysColor( Index: Integer ): string;
begin
  Result := FSysColors[ Index ];
end;

procedure TRzColorNames.SetSysColor( Index: Integer; const Value: string );
var
  Idx, ColorIdx: Integer;
begin
  FSysColors[ Index ] := Value;
  if ( FComboBox <> nil ) and ShowSysColors then
  begin
    Idx := FComboBox.ItemIndex;
    ColorIdx := MaxStdColors + Index;
    if ShowDefaultColor then
      Inc( ColorIdx );
    if ( ColorIdx > 0 ) and ( ColorIdx < FComboBox.Items.Count ) then
    begin
      FComboBox.Items[ ColorIdx ] := Value;
      FComboBox.ItemIndex := Idx;
      FComboBox.FStoreColorNames := True;
    end;
  end;
end;


procedure TRzColorNames.SetCustomColor( const Value: string );
var
  Idx: Integer;
begin
  FCustomColor := Value;
  if ( FComboBox <> nil ) and ShowCustomColor then
  begin
    Idx := FComboBox.ItemIndex;
    FComboBox.Items[ FComboBox.Items.Count - 1 ] := Value;
    FComboBox.ItemIndex := Idx;
    FComboBox.FStoreColorNames := True;
  end;
end;


{==============================}
{== TRzColorComboBox Methods ==}
{==============================}

constructor TRzColorComboBox.Create( AOwner: TComponent );
begin
  inherited;

  FSaveItemIndex := 0;
  Style := csOwnerDrawFixed;                // Style is not published

  FColorNames := TRzColorNames.Create;
  InitColorNames;
  FColorNames.FComboBox := Self;
  FStoreColorNames := False;

  FShowColorNames := True;
  FShowSysColors := True;
  {&RCI}
  FShowDefaultColor := True;
  FDefaultColor := clBlack;
  FShowCustomColor := True;
  FCustomColor := clBlack;

  FColorDlgOptions := [ cdFullOpen ];
  FCustomColors := TStringList.Create;
  FCancelPick := False;
end;


procedure TRzColorComboBox.CreateWnd;
var
  I: Integer;
begin
  inherited;

  Clear;

  { Add Default Color Item }
  if FShowDefaultColor then
    Items.AddObject( DefaultColorItem.Name, TObject( DefaultColorItem.Color ) );
  SetDefaultColor( FDefaultColor );

  { Add Standard Colors Always }
  for I := 0 to MaxStdColors - 1 do
    Items.AddObject( StdColorItems[ I ].Name, TObject( StdColorItems[ I ].Color ) );

  { Add System Colors }
  if FShowSysColors then
  begin
    for I := 0 to MaxSysColors - 1 do
      Items.AddObject( SysColorItems[ I ].Name, TObject( SysColorItems[ I ].Color ) );
  end;

  { Add Custom Color Item }
  if FShowCustomColor then
    Items.AddObject( CustomColorItem.Name, TObject( CustomColorItem.Color ) );
  SetCustomColor( FCustomColor );

  if FSaveColorNames <> nil then
  begin
    FColorNames.Assign( FSaveColorNames );
    FSaveColorNames.Free;
    FSaveColorNames := nil;
  end;

  { Select Default color entry or clBlack -- needed for when control dynamically created }
  ItemIndex := FSaveItemIndex;
  {&RV}
end;


procedure TRzColorComboBox.Loaded;
begin
  inherited;

  if ItemIndex = -1 then
    ItemIndex := 0;               { Select Default color entry by default }
end;


destructor TRzColorComboBox.Destroy;
begin
  FCustomColors.Free;
  FColorNames.Free;
  if FSaveColorNames <> nil then
    FSaveColorNames.Free;
  inherited;
end;


procedure TRzColorComboBox.DestroyWnd;
begin
  FSaveItemIndex := ItemIndex;
  if ( Items.Count > 0 ) and FStoreColorNames then
  begin
    FSaveColorNames := TRzColorNames.Create;
    FSaveColorNames.ShowDefaultColor := FShowDefaultColor;
    FSaveColorNames.ShowCustomColor := FShowCustomColor;
    FSaveColorNames.ShowSysColors := FShowSysColors;
    FSaveColorNames.Assign( FColorNames );
  end;
  inherited;
end;


procedure TRzColorComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FRegIniFile ) then
    FRegIniFile := nil;
end;


function TRzColorComboBox.GetCustomColorName( Index: Integer ): string;
begin
  Result := FCustomColors.Names[ Index ];
end; 


procedure TRzColorComboBox.FixupCustomColors;
var
  I: Integer;
  L: Longint;
  S, Ident: string;
begin
  for I := 0 to FCustomColors.Count - 1 do
  begin
    Ident := GetCustomColorName( I );

    { This code removes the high bit of the color value--
      only the lower 3 bytes are needed }
      
    L := StrToInt( '$' + FCustomColors.Values[ Ident ] ) and $00FFFFFF;
    S := Format( '%.6x', [ L ] );
    FCustomColors.Values[ Ident ] := S;
  end;
end;


procedure TRzColorComboBox.LoadCustomColors( const Section: string );
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotLoadCustomColors );

  FRegIniFile.ReadSectionValues( Section, FCustomColors );
  FixupCustomColors;
end;


procedure TRzColorComboBox.SaveCustomColors( const Section: string );
var
  I: Integer;
  Ident: string;
begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotSaveCustomColors );

  for I := 0 to FCustomColors.Count - 1 do
  begin
    Ident := GetCustomColorName( I );
    FRegIniFile.WriteString( Section, Ident, FCustomColors.Values[ Ident ] );
  end;
end;


procedure TRzColorComboBox.InitColorNames;
var
  I: Integer;
begin
  FColorNames.Default := DefaultColorItem.Name;
  for I := 0 to MaxStdColors - 1 do
    FColorNames.SetStdColor( I, StdColorItems[ I ].Name );
  for I := 0 to MaxSysColors - 1 do
    FColorNames.SetSysColor( I, SysColorItems[ I ].Name );
  FColorNames.Custom := CustomColorItem.Name;
end;


procedure TRzColorComboBox.SetCustomColors( Value: TStrings );
begin
  FCustomColors.Assign( Value );
  FixupCustomColors;
end;


procedure TRzColorComboBox.SetColorNames( Value: TRzColorNames );
begin
  FColorNames.Assign( Value );
end;


procedure TRzColorComboBox.SetShowColorNames( Value: Boolean );
begin
  if FShowColorNames <> Value then
  begin
    FShowColorNames := Value;
    Invalidate;
  end;
end;


procedure TRzColorComboBox.SetShowCustomColor( Value: Boolean );
begin
  if FShowCustomColor <> Value then
  begin
    FShowCustomColor := Value;
    FColorNames.ShowCustomColor := FShowCustomColor;
    RecreateWnd;
  end;
end;


procedure TRzColorComboBox.SetShowDefaultColor( Value: Boolean );
begin
  if FShowDefaultColor <> Value then
  begin
    FShowDefaultColor := Value;
    FColorNames.ShowDefaultColor := FShowDefaultColor;
    RecreateWnd;
  end;
end;


procedure TRzColorComboBox.SetShowSysColors( Value: Boolean );
begin
  if FShowSysColors <> Value then
  begin
    FShowSysColors := Value;
    FColorNames.ShowSysColors := FShowSysColors;
    RecreateWnd;
  end;
end;


function TRzColorComboBox.GetColorFromItem( Index: Integer ): TColor;
begin
  Result := TColor( Items.Objects[ Index ] );
end;

procedure TRzColorComboBox.SetDefaultColor( Value: TColor );
begin
  FDefaultColor := Value;
  if FShowDefaultColor then
  begin
    Items.Objects[ 0 ] := TObject( Value );
    Invalidate;
  end;
end;

procedure TRzColorComboBox.SetCustomColor( Value: TColor );
begin
  if Value <> -1 then
    FCustomColor := Value
  else
    FCustomColor := 0;

  if FShowCustomColor then
  begin
    Items.Objects[ Items.Count - 1 ] := TObject( FCustomColor );
    Invalidate;
  end;
end;


function TRzColorComboBox.GetSelectedColor: TColor;
begin
  if ItemIndex = -1 then
    Result := FDefaultColor
  else
    Result := GetColorFromItem( ItemIndex );
end;


procedure TRzColorComboBox.SetSelectedColor( Value: TColor );
var
  I: Integer;
  Found: Boolean;
begin
  Found := False;
  I := 0;

  while ( I < Items.Count ) and not Found do
  begin
    if TColor( Items.Objects[ I ] ) = Value then
      Found := True
    else
      Inc( I );
  end;

  if Found then
    ItemIndex := I
  else
  begin
    SetCustomColor( Value );
    ItemIndex := Items.Count - 1;
  end;

  Change;                                   // Generate the OnChange event
end; {= TRzColorComboBox.SetSelectedColor =}


procedure TRzColorComboBox.SetFrameVisible( Value: Boolean );
var
  C: TColor;
begin
  C := SelectedColor;
  inherited;
  SelectedColor := C;
end;


procedure TRzColorComboBox.SetRegIniFile( Value: TRzRegIniFile );
begin
  if FRegIniFile <> Value then
  begin
    FRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;



procedure TRzColorComboBox.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  RecreateWnd;
end;


procedure TRzColorComboBox.CNDrawItem( var Msg: TWMDrawItem );
var
  R: TRect;
begin
  { Indent owner-draw rectangle so focus rect doesn't cover color sample }
  if FShowColorNames then
  begin
    Canvas.Handle := Msg.DrawItemStruct^.hDC;
    R := Msg.DrawItemStruct^.rcItem;
    R.Right := R.Left + 24;
    if UsingSystemStyle then
      Canvas.Brush.Color := Color
    else
      Canvas.Brush.Color := ActiveStyleColor( scComboBox );
    Canvas.FillRect( R );
    Canvas.Handle := 0;

    Msg.DrawItemStruct^.rcItem.Left := Msg.DrawItemStruct^.rcItem.Left + 24;
  end;
  inherited;
end;


procedure TRzColorComboBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  R: TRect;
  InEditField: Boolean;
  RGBColor: Longint;
  YOffset: Integer;
begin
  InEditField := odComboBoxEdit in State;

  Canvas.FillRect( Rect );

  R := Rect;                         { R represents size of color block }
  InflateRect( R, -2, -2 );
  if not InEditField then
    OffsetRect( R, 2, 0 );

  if FShowColorNames then
  begin
    Dec( R.Left, 24 );
    R.Right := R.Left + 16;
  end
  else if FShowCustomColor and ( Index = Items.Count - 1 ) then
    Dec( R.Right, 20 );

  Canvas.Brush.Color := GetColorFromItem( Index );
  Canvas.Pen.Color := ActiveStyleSystemColor( clBtnShadow );
  Canvas.Rectangle( R.Left, R.Top, R.Right, R.Bottom );

  RGBColor := ColorToRGB( Brush.Color );
  if ( ( RGBColor and $00FF0000 ) <= $00800000 ) and
     ( ( RGBColor and $0000FF00 ) <= $00008000 ) and
     ( ( RGBColor and $000000FF ) <= $00000080 ) then
    DrawSides( Canvas, R, clBlack, clBlack, sdAllSides )
  else
    DrawSides( Canvas, R, clGray, clGray, sdAllSides );

  if odSelected in State then
  begin
    Canvas.Brush.Color := clHighlight;
    Canvas.Pen.Color := clHighlightText;
  end
  else
  begin
    if UsingSystemStyle then
    begin
      Canvas.Brush.Color := Color;
      if Enabled then
        Canvas.Pen.Color := clWindowText
      else
        Canvas.Pen.Color := clBtnShadow;
    end
    else // VCL Styles
    begin
      Canvas.Brush.Color := ActiveStyleColor( scComboBox );
      if Enabled then
        Canvas.Pen.Color := ActiveStyleFontColor( sfComboBoxItemNormal )
      else
        Canvas.Pen.Color := ActiveStyleFontColor( sfComboBoxItemDisabled );
    end;
  end;

  if FShowCustomColor and ( Index = Items.Count - 1 ) then
  begin
    { Custom Color Entry -- draw an ellipsis }
    Canvas.Rectangle( Rect.Right - 16, Rect.Bottom - 7, Rect.Right - 14, Rect.Bottom - 4 );
    Canvas.Rectangle( Rect.Right - 12, Rect.Bottom - 7, Rect.Right - 10, Rect.Bottom - 4 );
    Canvas.Rectangle( Rect.Right - 8, Rect.Bottom - 7, Rect.Right - 6, Rect.Bottom - 4 );
  end;

  if FShowColorNames then
  begin
    if not Enabled then
      Canvas.Font.Color := clBtnShadow;
    YOffset := ( ( Rect.Bottom - Rect.Top ) - Canvas.TextHeight( 'Yy' ) ) div 2;
    Canvas.TextOut( Rect.Left + 2, Rect.Top + YOffset, Items[ Index ] );
  end;
end;


procedure TRzColorComboBox.CNCommand( var Msg: TWMCommand );
begin
  inherited;
  if Msg.NotifyCode = cbn_SelEndCancel then
    FCancelPick := True
  else
    FCancelPick := False;
end;


procedure TRzColorComboBox.CloseUp;
var
  FColorDlg: TColorDialog;
begin
  inherited;

  if not FCancelPick and FShowCustomColor and ( ItemIndex = Items.Count - 1 ) then
  begin

    { Display color dialog box }
    FColorDlg := TColorDialog.Create( Self );
    try
      with FColorDlg do
      begin
        Color := SelectedColor;
        CustomColors := FCustomColors;
        Options := FColorDlgOptions;
        if Execute then
        begin
          SetCustomColors( CustomColors );
          SetCustomColor( Color );
        end;
      end;
    finally
      FColorDlg.Free;
    end;
  end;
end;




{=================================}
{== TRzPreviewFontPanel Methods ==}
{=================================}

constructor TRzPreviewFontPanel.Create( AOwner: TComponent );
begin
  inherited;

  // Delphi 7 sets the csParentBackground style and removes the csOpaque style when Themes are available, which causes
  // all kinds of other problems, so we restore these.
  ControlStyle := ControlStyle - [ csParentBackground ] + [ csOpaque ];

  Color := clWindow;
  Width := 260;
  Height := 65;
  Visible := False;
  Caption := ptDefault;
  BevelOuter := bvNone;
  Font.Size := 36;
end;


procedure TRzPreviewFontPanel.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or WS_POPUP;
  Params.WindowClass.Style := CS_SAVEBITS;
end;


procedure TRzPreviewFontPanel.Paint;
begin
  inherited;
  Canvas.Rectangle( 0, 0, Width, Height );
end;


procedure TRzPreviewFontPanel.CMCancelMode( var Msg: TCMCancelMode );
begin
  // cm_CancelMode is sent when user clicks somewhere in same application
  if Msg.Sender <> Self then
    SendMessage( FControl.Handle, cm_HidePreviewPanel, 0, 0 );
end;


procedure TRzPreviewFontPanel.WMKillFocus( var Msg: TMessage );
begin
  // wm_KillFocus is sent went user switches to another application or window
  inherited;
  SendMessage( FControl.Handle, cm_HidePreviewPanel, 0, 0 );
end;


procedure TRzPreviewFontPanel.CMShowingChanged( var Msg: TMessage );
begin
  // Ignore showing using the Visible property
end;


{=============================}
{== TRzFontComboBox Methods ==}
{=============================}

constructor TRzFontComboBox.Create( AOwner: TComponent );
begin
  inherited;
  Style := csOwnerDrawFixed;                  // Style is not published

  FSaveFontName := '';

  Sorted := True;
  FShowStyle := ssFontName;
  FShowSymbolFonts := True;

  FFont := TFont.Create;
  FFontSize := 8;
  FFont.Size := FFontSize;
  FFontStyle := [];
  FFontType := ftAll;

  FTrueTypeBmp := TBitmap.Create;
  FFixedPitchBmp := TBitmap.Create;
  FTrueTypeFixedBmp := TBitmap.Create;
  FPrinterBmp := TBitmap.Create;
  FDeviceBmp := TBitmap.Create;
  LoadBitmaps;

  DropDownCount := 14;
  FMaintainMRUFonts := True;
  FMRUCount := -1;
  FPreviewVisible := False;

  FPreviewFontSize := 36;
  FPreviewHeight := 65;
  FPreviewWidth := 260;
  
  if not ( csDesigning in ComponentState ) then
  begin
    FPreviewPanel := TRzPreviewFontPanel.Create( Self );
    FPreviewPanel.Parent := TWinControl( Self );
    FPreviewPanel.Control := Self;
  end;

  {&RCI}
end;


procedure TRzFontComboBox.CreateWnd;
begin
  {&RV}
  inherited;
  Clear;
  LoadFonts;
  if FSaveFontName <> '' then
    SetFontName( FSaveFontName );
end;


destructor TRzFontComboBox.Destroy;
begin
  FFont.Free;
  FTrueTypeBmp.Free;
  FFixedPitchBmp.Free;
  FTrueTypeFixedBmp.Free;
  FPrinterBmp.Free;
  FDeviceBmp.Free;
  inherited;
end;


procedure TRzFontComboBox.DestroyWnd;
begin
  FSaveFontName := GetFontName;
  inherited;
end;


function EnumFontsProc( var LogFont: TLogFont; var TextMetric: TTextMetric; FontType: Integer;
                        Data: Pointer ): Integer; stdcall;
begin
  with TRzFontComboBox( Data ), TextMetric do
  begin
    case FontType of
      ftAll:
      begin
        if ShowSymbolFonts or ( LogFont.lfCharSet <> SYMBOL_CHARSET ) then
          Items.AddObject( LogFont.lfFaceName, TObject( tmPitchAndFamily ) );
      end;

      ftTrueType:
      begin
        if ( tmPitchAndFamily and tmpf_TrueType) = tmpf_TrueType then
          if ShowSymbolFonts or ( LogFont.lfCharSet <> SYMBOL_CHARSET ) then
            Items.AddObject( LogFont.lfFaceName, TObject( tmPitchAndFamily ) );
      end;

      ftFixedPitch:
      begin
        if ( tmPitchAndFamily and tmpf_Fixed_Pitch ) = 0 then
          if ShowSymbolFonts or ( LogFont.lfCharSet <> SYMBOL_CHARSET ) then
            Items.AddObject( LogFont.lfFaceName, TObject( tmPitchAndFamily ) );
      end;
    end; { case }
    Result := 1;
  end;
end;


procedure TRzFontComboBox.LoadFonts;
var
  DC: HDC;
begin
  if FFontDevice = fdScreen then
  begin
    DC := GetDC( 0 );
    EnumFontFamilies( DC, nil, @EnumFontsProc, LPARAM( Self ) );
    ReleaseDC( 0, DC );
  end
  else
  begin
    EnumFontFamilies( Printer.Handle, nil, @EnumFontsProc, LPARAM( Self ) );
  end;
end;


procedure TRzFontComboBox.LoadBitmaps;
begin
  FTrueTypeBmp.Handle := LoadBitmap( HInstance, 'RZCMBOBX_TRUETYPE' );
  FFixedPitchBmp.Handle := LoadBitmap( HInstance, 'RZCMBOBX_FIXEDPITCH' );
  FTrueTypeFixedBmp.Handle := LoadBitmap( HInstance, 'RZCMBOBX_TRUETYPEFIXED' );
  FPrinterBmp.Handle := LoadBitmap( HInstance, 'RZCMBOBX_PRINTER' );
  FDeviceBmp.Handle := LoadBitmap( HInstance, 'RZCMBOBX_DEVICE' );
end;


procedure TRzFontComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( AComponent = FPreviewEdit ) and ( Operation = opRemove ) then
    FPreviewEdit := nil;
end;


procedure TRzFontComboBox.HidePreviewPanel;
begin
  if FPreviewVisible then
  begin
    FPreviewVisible := False;
    SetWindowPos( FPreviewPanel.Handle, 0, 0, 0, 0, 0,
                  swp_NoActivate or swp_NoZOrder or swp_NoMove or swp_NoSize or
                  swp_HideWindow );
  end;
end;


procedure TRzFontComboBox.ShowPreviewPanel;
var
  P: TPoint;
  Monitor, MonitorOrigin: TMonitor;
  X, FarRightEdge: Integer;
begin
  // Make sure there are items in the list
  if Items.Count = 0 then
    Exit;

  P := ClientToScreen( Point( 0, 0 ) );
  MonitorOrigin := GetMonitorContainingPoint( P );

  X := P.X;
  P.X := P.X + FActualDropDownWidth;

  Monitor := GetMonitorContainingPoint( P );
  if Assigned( Monitor ) then
    FarRightEdge := GetMonitorWorkArea( Monitor ).Right
  else
    FarRightEdge := GetActiveWorkAreaWidth( Parent );

  if ( P.X + FPreviewWidth > FarRightEdge ) or ( Monitor <> MonitorOrigin ) then
    P.X := X - FPreviewWidth;

  FPreviewPanel.Font.Size := FPreviewFontSize;

  // Because FPreviewPanel has style WS_POPUP, Left and Top values to SetWindowPos are screen coordinates

  SetWindowPos( FPreviewPanel.Handle, 0, P.X - 1, P.Y,
                FPreviewWidth, FPreviewHeight,
                swp_NoActivate or swp_ShowWindow );
  FPreviewVisible := True;
  FPreviewPanel.Update;
end;


procedure TRzFontComboBox.CMCancelMode( var Msg: TCMCancelMode );
begin
  // cm_CancelMode is sent when user clicks somewhere in same application
  if ( FShowStyle = ssFontPreview ) and ( Msg.Sender <> Self ) then
    HidePreviewPanel;
end;


procedure TRzFontComboBox.CMHidePreviewPanel( var Msg: TMessage );
begin
  inherited;
  HidePreviewPanel;
end;


procedure TRzFontComboBox.UpdatePreviewText;
var
  Preview: string;
begin
  if FPreviewText = '' then
    Preview := ptDefault
  else
    Preview := FPreviewText;

  FPreviewPanel.Alignment := taCenter;

  if Assigned( FPreviewEdit ) then
  begin
    FPreviewPanel.Alignment := taLeftJustify;
    if FPreviewEdit.SelLength > 0 then
      Preview := FPreviewEdit.SelText
    else
      Preview := Copy( FPreviewEdit.Text, 1, 10 );
  end
  else
  begin
    if FPreviewPanel.Canvas.TextWidth( FPreviewText ) >= PreviewWidth then
      Preview := ptDefault1;
    if FPreviewPanel.Canvas.TextWidth( FPreviewText ) >= PreviewWidth then
      Preview := ptDefault2;
  end;
  FPreviewPanel.Caption := Preview;
end;


procedure TRzFontComboBox.DropDown;
begin
  UpdatePreviewText;
  inherited;
  if FShowStyle = ssFontPreview then
    ShowPreviewPanel;
end;


procedure TRzFontComboBox.CloseUp;
var
  Idx, I: Integer;
  FoundMRUFont: Boolean;
begin
  inherited;
  if FShowStyle = ssFontPreview then
    HidePreviewPanel;

  if FMaintainMRUFonts and ( ItemIndex <> 0 ) then
  begin
    Idx := ItemIndex;
    if Idx = -1 then
      Exit;
    // Add selected item to Top of list if not already at the Top
    FoundMRUFont := False;
    I := 0;
    while ( I <= FMRUCount ) and not FoundMRUFont do
    begin
      if Items[ I ] = Items[ Idx ] then
        FoundMRUFont := True
      else
        Inc( I );
    end;
    if FoundMRUFont then
    begin
      Items.Move( I, 0 );                   // Move MRU font to Top of list
    end
    else
    begin
      // Make a copy of the selected font to appear in MRU portion at Top of list
      Items.InsertObject( 0, Items[ Idx ], Items.Objects[ Idx ] );
      if Idx > FMRUCount then
        Inc( FMRUCount );
    end;
    ItemIndex := 0;
  end;
end; {= TRzFontComboBox.CloseUp =}



procedure TRzFontComboBox.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  RecreateWnd;
end;


procedure TRzFontComboBox.CNDrawItem( var Msg: TWMDrawItem );
var
  R: TRect;
begin
  // Indent owner-draw rectangle so focus rect doesn't cover glyph

  Canvas.Handle := Msg.DrawItemStruct^.hDC;
  R := Msg.DrawItemStruct^.rcItem;
  R.Right := R.Left + 24;
  if UsingSystemStyle then
    Canvas.Brush.Color := Color
  else
    Canvas.Brush.Color := ActiveStyleColor( scComboBox );
  Canvas.FillRect( R );
  Canvas.Handle := 0;

  Msg.DrawItemStruct^.rcItem.Left := Msg.DrawItemStruct^.rcItem.Left + 24;
  Msg.DrawItemStruct^.rcItem.Right := Msg.DrawItemStruct^.rcItem.Right - 1;

  inherited;
end;


procedure TRzFontComboBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  Bmp: TBitmap;
  DestRct, SrcRct, R: TRect;
  BmpOffset, TextOffset: Integer;
  FT: Byte;
  TransparentColor: TColor;
  InEditField: Boolean;
  TempStyle: TRzShowStyle;
begin
  InEditField := odComboBoxEdit in State;

  Bmp := TBitmap.Create;
  try
    Canvas.FillRect( Rect );   // Clear area for icon and text

    DestRct := Classes.Rect( 0, 0, 12, 12 );
    SrcRct := DestRct;
    BmpOffset := ( ( Rect.Bottom - Rect.Top ) - 12 ) div 2;

    // Don't Forget to Set the Width and Height of Destination Bitmap
    Bmp.Width := 12;
    Bmp.Height := 12;

    if UsingSystemStyle then
      Bmp.Canvas.Brush.Color := Color
    else
      Bmp.Canvas.Brush.Color := ActiveStyleColor( scListBox );

    TransparentColor := clOlive;

    FT := Longint( Items.Objects[ Index ] ) and $0000000F;
    if ( ( FT and tmpf_TrueType ) = tmpf_TrueType ) and
       ( ( FT and tmpf_Fixed_Pitch ) <> tmpf_Fixed_Pitch ) then
    begin
      Bmp.Canvas.BrushCopy( DestRct, FTrueTypeFixedBmp, SrcRct, TransparentColor );
      Canvas.Draw( Rect.Left - 20, Rect.Top + BmpOffset, Bmp );
    end
    else if ( FT and tmpf_TrueType ) = tmpf_TrueType then
    begin
      Bmp.Canvas.BrushCopy( DestRct, FTrueTypeBmp, SrcRct, TransparentColor );
      Canvas.Draw( Rect.Left - 20, Rect.Top + BmpOffset, Bmp );
    end
    else if ( FT and tmpf_Fixed_Pitch ) <> tmpf_Fixed_Pitch then
    begin
      Bmp.Canvas.BrushCopy( DestRct, FFixedPitchBmp, SrcRct, TransparentColor );
      Canvas.Draw( Rect.Left - 20, Rect.Top + BmpOffset, Bmp );
    end
    else if FFontDevice = fdPrinter then
    begin
      Bmp.Canvas.BrushCopy( DestRct, FPrinterBmp, SrcRct, TransparentColor );
      Canvas.Draw( Rect.Left - 20, Rect.Top + BmpOffset, Bmp );
    end
    else
    begin
      Bmp.Canvas.BrushCopy( DestRct, FDeviceBmp, SrcRct, TransparentColor );
      Canvas.Draw( Rect.Left - 20, Rect.Top + BmpOffset, Bmp );
    end;

    if not Enabled then
    begin
      if UsingSystemStyle then
        Canvas.Font.Color := clBtnShadow
      else
        Canvas.Font.Color := ActiveStyleFontColor( sfListItemTextDisabled );
    end;

    TempStyle := FShowStyle;
    if InEditField and ( TempStyle = ssFontNameAndSample ) then
      TempStyle := ssFontName;

    TextOffset := ( ( Rect.Bottom - Rect.Top ) - Canvas.TextHeight( 'Yy' ) ) div 2;
    case TempStyle of
      ssFontName, ssFontPreview:
      begin
        Canvas.TextOut( Rect.Left + 2, Rect.Top + TextOffset, Items[ Index ] );
      end;

      ssFontSample:
      begin
        Canvas.Font.Name := Items[ Index ];
        Canvas.TextOut( Rect.Left + 2, Rect.Top + TextOffset, Items[ Index ] );
      end;

      ssFontNameAndSample:
      begin
        R := Rect;
        R.Right := R.Left + ( R.Right - R.Left ) div 2 - 4;
        Canvas.Font.Name := Self.Font.Name;
        Canvas.TextRect( R, R.Left + 2, R.Top + TextOffset, Items[ Index ] );

        if UsingSystemStyle then
        begin
          if Enabled then
            Canvas.Pen.Color := clWindowText
          else
            Canvas.Pen.Color := clBtnShadow;
        end
        else // VCL Styles
        begin
          if Enabled then
            Canvas.Pen.Color := ActiveStyleFontColor( sfListItemTextNormal )
          else
            Canvas.Pen.Color := ActiveStyleFontColor( sfListItemTextDisabled );
        end;

        Canvas.MoveTo( R.Right + 2, R.Top );
        Canvas.LineTo( R.Right + 2, R.Bottom );

        Canvas.Font.Name := Items[ Index ];
        R.Left := R.Right + 4;
        R.Right := Rect.Right;
        Canvas.TextRect( R, R.Left + 2, R.Top + TextOffset, Items[ Index ] );
      end;
    end;
  finally
    Bmp.Free;
  end;

  if ( FShowStyle = ssFontPreview ) and ( odFocused in State ) then
  begin
    FPreviewPanel.Font.Name := Items[ Index ];
    FPreviewPanel.Canvas.Font := FPreviewPanel.Font;
    UpdatePreviewText;
  end;

  if FMaintainMRUFonts and not InEditField and ( Index = FMRUCount ) then
  begin
    if UsingSystemStyle then
      Canvas.Pen.Color := clWindowText
    else
      Canvas.Pen.Color := ActiveStyleFontColor( sfListItemTextNormal );
    Canvas.MoveTo( 0, Rect.Bottom - 1 );
    Canvas.LineTo( Rect.Right, Rect.Bottom - 1 );
  end;


end; {= TRzFontComboBox.DrawItem =}


procedure TRzFontComboBox.SetFontDevice( Value: TRzFontDevice );
begin
  if FFontDevice <> Value then
  begin
    FFontDevice := Value;
    RecreateWnd;
  end;
end;


procedure TRzFontComboBox.SetFontType( Value: TRzFontType );
begin
  if FFontType <> Value then
  begin
    FFontType := Value;
    RecreateWnd;
  end;
end;


function TRzFontComboBox.GetSelectedFont: TFont;
begin
  if ItemIndex = -1 then
    Result := nil
  else
  begin
    FFont.Name := Items[ ItemIndex ];
    FFont.Size := FFontSize;
    FFont.Style := FFontStyle;
    Result := FFont;
  end;
end;


procedure TRzFontComboBox.SetSelectedFont( Value: TFont );
begin
  ItemIndex := Items.IndexOf( Value.Name );
end;


function TRzFontComboBox.GetFontName: string;
begin
  if ItemIndex >= 0 then
    Result := Items[ ItemIndex ]
  else
    Result := '';
end;


procedure TRzFontComboBox.SetFontName( const Value: string );
begin
  ItemIndex := Items.IndexOf( Value );
end;


procedure TRzFontComboBox.SetShowSymbolFonts( Value: Boolean );
begin
  if FShowSymbolFonts <> Value then
  begin
    FShowSymbolFonts := Value;
    RecreateWnd;
  end;
end;


procedure TRzFontComboBox.SetShowStyle( Value: TRzShowStyle );
begin
  if FShowStyle <> Value then
  begin
    FShowStyle := Value;
    Invalidate;
  end;
end;


procedure TRzFontComboBox.SetPreviewEdit( Value: TCustomEdit );
begin
  FPreviewEdit := Value;
  if FPreviewEdit <> nil then
    FPreviewEdit.FreeNotification( Self );
end;



{============================}
{== TRzMRUComboBox Methods ==}
{============================}

constructor TRzMRUComboBox.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
  FSelectFirstItemOnLoad := False;
  FRemoveItemCaption := '&Remove item from history list';

  FMruPath := '';

  FMruSection := '';
  FMruID := '';

  FDataIsLoaded := False;
  FMaxHistory := 25;
  inherited Sorted := False;

  { Build custom popup menu }
  CreatePopupMenuItems;
  InitializePopupMenuItems;
  AddMenuItemsToPopupMenu;
end;


procedure TRzMRUComboBox.CreateWnd;
begin
  inherited;

  if not ( csLoading in ComponentState ) and not FDataIsLoaded then
  begin
    if ( ( FMruPath <> '' ) or ( FMruRegIniFile <> nil ) ) and
       ( FMruSection <> '' ) and ( FMruID <> '' ) then
    begin
      LoadMRUData( False );
    end;
  end;
end;


destructor TRzMRUComboBox.Destroy;
begin
  inherited;
end;


procedure TRzMRUComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FMruRegIniFile ) then
    FMruRegIniFile := nil;
end;


procedure TRzMRUComboBox.Loaded;
begin
  inherited;
  LoadMRUData( True );
  {&RV}
end;


procedure TRzMRUComboBox.LoadMRUData( FromStream: Boolean );
var
  I, Idx, AddIdx, NumMRUItems: Integer;
  ItemStr: string;
  R: TRzRegIniFile;
begin
  { Make sure we have the necessary data to read the MRU values }
  if ( csDesigning in ComponentState ) or
     ( ( FMruPath = '' ) and ( FMruRegIniFile = nil ) ) or
     ( FMruSection = '' ) or
     ( FMruID = '' ) then
  begin
    Exit;
  end;

  if FMruRegIniFile <> nil then
    R := FMruRegIniFile
  else
  begin
    R := TRzRegIniFile.Create( Self );
    R.PathType := ptRegistry;
    R.Path := FMruPath;
  end;

  try
    NumMRUItems := R.ReadInteger( FMruSection, FMruID + '_Count', 0 );
    for I := 0 to NumMRUItems - 1 do
    begin
      ItemStr := R.ReadString( FMruSection, FMruID + '_Item' + IntToStr( I ), '' );

      if ItemStr <> '' then
      begin
        if FromStream then
        begin
          Idx := Items.IndexOf( ItemStr );
          if Idx = -1 then
          begin
            AddIdx := Items.Add( ItemStr );
            if I >= Items.Count then
              Items.Move( AddIdx, Items.Count - 1 )
            else
              Items.Move( AddIdx, I );
          end
          else
          begin
            if I >= Items.Count then
              Items.Move( Idx, Items.Count - 1 )
            else
              Items.Move( Idx, I );
          end;
        end
        else
        begin
          Idx := Items.IndexOf( ItemStr );
          if Idx = -1 then
            Items.Add( ItemStr );
        end;
      end;
    end;

    if FSelectFirstItemOnLoad and ( Items.Count > 0 ) then
      ItemIndex := 0;                      { Select the first item in the list }

    FDataIsLoaded := True;
  finally
    if FMruRegIniFile = nil then
      R.Free;
  end;
end; {= TRzMRUComboBox.LoadMRUData =}


procedure TRzMRUComboBox.SaveMRUData;
var
  NumItemsToSave, I: Integer;
  R: TRzRegIniFile;
begin
  { Make sure we have the necessary data to save the MRU values }
  if ( ( FMruPath = '' ) and ( FMruRegIniFile = nil ) ) or ( FMruSection = '' ) or ( FMruID = '' ) then
    Exit;

  if FMruRegIniFile <> nil then
    R := FMruRegIniFile
  else
  begin
    R := TRzRegIniFile.Create( Self );
    R.PathType := ptRegistry;
    R.Path := FMruPath;
  end;

  try
    NumItemsToSave := Items.Count;
    if NumItemsToSave > FMaxHistory then
      NumItemsToSave := FMaxHistory;

    R.WriteInteger( FMruSection, FMruID + '_Count', NumItemsToSave );

    for I := 0 to NumItemsToSave - 1 do
      R.WriteString( FMruSection, FMruID + '_Item' + IntToStr( I ), Items[ I ] );

    { Clean up entries no longer being used }
    for I := NumItemsToSave to FMaxHistory - 1 do
      R.DeleteKey( FMruSection, FMruID + '_Item' + IntToStr( I ) );
  finally
    if FMruRegIniFile = nil then
      R.Free;
  end;
end; {= TRzMRUComboBox.SaveMRUData =}



procedure TRzMRUComboBox.UpdateMRUList;
var
  S: string;
  Idx: Integer;
begin
  if Text = '' then
    Exit;

  Idx := Items.IndexOf( Text );
  if Idx = -1 then
    Items.Insert( 0, Text )	                     { Insert entry at Top of list }
  else
  begin
    { Entry is already in list. Let's move it to the Top }
    { Must save and restore since call to Move clears text }
    S := Text;
    Items.Move( Idx, 0 );
    if Style = csDropDownList then
      ItemIndex := 0
    else
      Text := S;

  end;
  SaveMRUData;	                       { Save data since we just made a change }
end;


procedure TRzMRUComboBox.UpdateMRUListFromCloseUp;
begin
  if ItemIndex = -1 then
    Exit;

  { Entry is already in list. Let's move it to the Top }
  Items.Move( ItemIndex, 0 );
  ItemIndex := 0;

  SaveMRUData;	                       { Save data since we just made a change }
end;


procedure TRzMRUComboBox.DoExit;
begin
  inherited;
  UpdateMRUList;
end;


procedure TRzMRUComboBox.CreatePopupMenuItems;
begin
  FEmbeddedMenu := TPopupMenu.Create( Self );
  inherited PopupMenu := FEmbeddedMenu;

  FMnuUndo := TMenuItem.Create( FEmbeddedMenu );
  FMnuSeparator1 := TMenuItem.Create( FEmbeddedMenu );
  FMnuCut := TMenuItem.Create( FEmbeddedMenu );
  FMnuCopy := TMenuItem.Create( FEmbeddedMenu );
  FMnuPaste := TMenuItem.Create( FEmbeddedMenu );
  FMnuDelete := TMenuItem.Create( FEmbeddedMenu );
  FMnuSeparator2 := TMenuItem.Create( FEmbeddedMenu );
  FMnuSelectAll := TMenuItem.Create( FEmbeddedMenu );
  FMnuSeparator3 := TMenuItem.Create( FEmbeddedMenu );
  FMnuRemove := TMenuItem.Create( FEmbeddedMenu );
end;


procedure TRzMRUComboBox.SetupMenuItem( AMenuItem: TMenuItem; ACaption: string; AChecked, ARadioItem: Boolean;
                                        AGroupIndex, AShortCut: Integer; AHandler: TNotifyEvent );
begin
  with AMenuItem do
  begin
    Caption := ACaption;
    Checked := AChecked;
    RadioItem := ARadioItem;
    GroupIndex := AGroupIndex;
    ShortCut := AShortCut;
    OnClick := AHandler;
    Tag := FPopupMenuTag;
    Inc( FPopupMenuTag );
  end;
end;



procedure TRzMRUComboBox.InitializePopupMenuItems;
var
  ShellLib: THandle;

  { Get popup menu captions from Shell32.dll }
  function GetMenuText( ID: DWORD ): string;
  var
    Stz: array[ 0..255 ] of Char;
  begin
    if LoadString( ShellLib, ID, Stz, 255 ) <> 0 then
      Result := Stz
    else
      Result := '';
  end;

begin
  ShellLib := LoadLibrary( 'Shell32' );
  try
    FPopupMenuTag := 0;
    SetupMenuItem( FMnuUndo, GetMenuText( 33563 ), False, False, 0, 0, MnuUndoClickHandler );
    SetupMenuItem( FMnuSeparator1, '-', False, False, 0, 0, nil );
    SetupMenuItem( FMnuCut, GetMenuText( 33560 ), False, False, 0, 0, MnuCutClickHandler );
    SetupMenuItem( FMnuCopy, GetMenuText( 33561 ), False, False, 0, 0, MnuCopyClickHandler );
    SetupMenuItem( FMnuPaste, GetMenuText( 33562 ), False, False, 0, 0, MnuPasteClickHandler );
    SetupMenuItem( FMnuDelete, GetMenuText( 33553 ), False, False, 0, 0, MnuDeleteClickHandler );
    SetupMenuItem( FMnuSeparator2, '-', False, False, 0, 0, nil );
    SetupMenuItem( FMnuSelectAll, GetMenuText( 4171 ), False, False, 0, 0, MnuSelectAllClickHandler );
    SetupMenuItem( FMnuSeparator3, '-', False, False, 0, 0, nil );
    SetupMenuItem( FMnuRemove, FRemoveItemCaption, False, False, 0, 0, MnuRemoveItemClickHandler );
  finally
    FreeLibrary( ShellLib );
  end;
end;


procedure TRzMRUComboBox.AddMenuItemsToPopupMenu;
begin
  with FEmbeddedMenu do
  begin
    OnPopup := EmbeddedMenuPopupHandler;

    { Add menu items in the order they should appear in the popup menu }
    Items.Add( FMnuUndo );
    Items.Add( FMnuSeparator1 );
    Items.Add( FMnuCut );
    Items.Add( FMnuCopy );
    Items.Add( FMnuPaste );
    Items.Add( FMnuDelete );
    Items.Add( FMnuSeparator2 );
    Items.Add( FMnuSelectAll );
    Items.Add( FMnuSeparator3 );
    Items.Add( FMnuRemove );
  end;
end;


procedure TRzMRUComboBox.EmbeddedMenuPopupHandler( Sender: TObject );
var
  CanUndo, TextIsOnClipboard, HasSelection: Boolean;
begin
  Windows.SetFocus( Handle );
  if Focused then
  begin
    HasSelection := ( SelLength <> 0 );
    CanUndo := SendMessage( EditHandle, em_CanUndo, 0, 0 ) <> 0;

    TextIsOnClipboard := Clipboard.AsText <> '';

    FMnuCut.Enabled := HasSelection;
    FMnuCopy.Enabled := HasSelection;
    FMnuDelete.Enabled := HasSelection;
    FMnuUndo.Enabled := CanUndo;
    FMnuPaste.Enabled := TextIsOnClipboard;
    FMnuSelectAll.Enabled := Text <> '';
    FMnuRemove.Enabled := Text <> '';
  end;
end;


procedure TRzMRUComboBox.MnuUndoClickHandler( Sender: TObject );
begin
  SendMessage( EditHandle, em_Undo, 0, 0 );
end;

procedure TRzMRUComboBox.MnuCutClickHandler( Sender: TObject );
begin
  Perform( wm_Cut, 0, 0 );
end;

procedure TRzMRUComboBox.MnuCopyClickHandler( Sender: TObject );
begin
  Perform( wm_Copy, 0, 0 );
end;

procedure TRzMRUComboBox.MnuPasteClickHandler( Sender: TObject );
begin
  Perform( wm_Paste, 0, 0 );
end;

procedure TRzMRUComboBox.MnuDeleteClickHandler( Sender: TObject );
begin
  Perform( wm_Clear, 0, 0 );
end;

procedure TRzMRUComboBox.MnuSelectAllClickHandler( Sender: TObject );
begin
  Perform( cb_SetEditSel, 0, MakeLong( 0, Word( -1 ) ) );
end;


procedure TRzMRUComboBox.MnuRemoveItemClickHandler( Sender: TObject );
var
  I: Integer;
begin
  I := Items.IndexOf( Text );
  if I >= 0 then
  begin
    Items.Delete( I );
    SaveMRUData;
  end;
  Text := '';
  Change;
end;


procedure TRzMRUComboBox.SetRemoveItemCaption( const Value: string );
begin
  if FRemoveItemCaption <> Value then
  begin
    FRemoveItemCaption := Value;
    if Assigned( FMnuRemove ) then
      FMnuRemove.Caption := FRemoveItemCaption;
  end;
end;


procedure TRzMRUComboBox.SetMruRegIniFile( Value: TRzRegIniFile );
begin
  if FMruRegIniFile <> Value then
  begin
    FMruRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzMRUComboBox.EscapeKeyPressed;
begin
  if Assigned( FOnEscapeKeyPressed ) then
    FOnEscapeKeyPressed( Self );
end;

procedure TRzMRUComboBox.EnterKeyPressed;
begin
  if Assigned( FOnEnterKeyPressed ) then
    FOnEnterKeyPressed( Self );
  {&RV}
end;


procedure TRzMRUComboBox.KeyPress( var Key: Char );
begin
  if ( Ord( Key ) = vk_Return ) and not DroppedDown then    // Enter key pressed
  begin
    UpdateMRUList;
    EnterKeyPressed;
    if FTabOnEnter then
    begin
      Key := #0;
      PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
    end;
  end
  else if Ord( Key ) = vk_Escape then                      // Escape key pressed
  begin
    EscapeKeyPressed;
  end
  else
    inherited;
end;


procedure TRzMRUComboBox.CloseUp;
begin
  inherited;
  UpdateMRUListFromCloseUp;
end;


{==================================}
{== TRzImageComboBoxItem Methods ==}
{==================================}

constructor TRzImageComboBoxItem.Create( AOwner: TRzCustomImageComboBox );
begin
  inherited Create;
  FOwner := aOwner;
  FOverlayIndex := -1;
  {$IFDEF PTDEBUG}
  Inc( FOwner.mdbgItems );
  Inc( g_dbgItems );
  {$ENDIF}
end;

destructor TRzImageComboBoxItem.Destroy;
begin
  {$IFDEF PTDEBUG}
  Dec( FOwner.mdbgItems );
  Dec( g_dbgItems );
  {$ENDIF}
  inherited;
end;

procedure TRzImageComboBoxItem.SetIndentLevel( Value: Integer );
begin
  FIndentLevel := Value;
  FOwner.Invalidate;
end;

procedure TRzImageComboBoxItem.SetImageIndex( Value: Integer );
begin
  FImageIndex := Value;
  FOwner.Invalidate;
end;

procedure TRzImageComboBoxItem.SetCaption( const Value: string );
begin
  FCaption := Value;
  FOwner.Invalidate;
end;

procedure TRzImageComboBoxItem.SetOverlayIndex( Value: Integer );
begin
  FOverlayIndex := Value;
  FOwner.Invalidate;
end;


{============================}
{== TRzCustomImageComboBox ==}
{============================}

constructor TRzCustomImageComboBox.Create( AOwner: TComponent );
begin
  inherited;

  FShowFocus := False;
  Style := csOwnerDrawFixed;
  FItemIndent := 12;
  FAutoSizeHeight := True;
end;


procedure TRzCustomImageComboBox.CreateParams( var Params: TCreateParams );
begin
  inherited;

  if RunningUnder( WinNT ) then
  begin
    // PT Comments -
    // Under Windows NT (4.0) if the combo has the CBS_HASSTRINGS style set,
    // then when the WM_DELETEITEM message is sent, the itemData member of the
    // DELETEITEMSTRUCT record is 0 (so we GP fault and items leak).
    // The unfortunate side effect of this fix is that keyboard access doesn't
    // work anymore.

    Params.Style := Params.Style and not CBS_HASSTRINGS;
  end;
end;


procedure TRzCustomImageComboBox.CreateWnd;
begin
  inherited;
  SendMessage( Handle, CB_SETEXTENDEDUI, 1, 0 );
end;


procedure TRzCustomImageComboBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FImages ) then
    FImages := nil;
end;


function TRzCustomImageComboBox.GetImageComboBoxItem( Index: Integer ): TRzImageComboBoxItem;
begin
  Result := inherited Items.Objects[ Index ] as TRzImageComboBoxItem;
end;


procedure TRzCustomImageComboBox.DrawItem( Index: Integer; Rect: TRect; State: TOwnerDrawState );
var
  Item: TRzImageComboBoxItem;
  X, Top, Indent: Integer;
  R: TRect;
  BrushColor: TColor;
  OldBkColor: TColorRef;
begin
  if Assigned( OnDrawItem ) then
    OnDrawItem( Self, Index, Rect, State )
  else
  begin
    Item := inherited Items.Objects[ Index ] as TRzImageComboBoxItem;
    if not Assigned( Item ) then
      Exit;

    GetItemData( Item );

    if odComboBoxEdit in State then
      Indent := 0
    else
      Indent := Item.IndentLevel;

    if UsingSystemStyle then
      BrushColor := Color
    else
      BrushColor := ActiveStyleColor( scComboBox );
    Canvas.Brush.Color := BrushColor;
    Canvas.FillRect( Rect );

    if Assigned( FImages ) then
    begin
      // Use the API to prevent a Change event occuring for the imagelist component
      OldBkColor := ImageList_GetBkColor( FImages.Handle );
      try
        ImageList_SetBkColor( FImages.Handle, ColorToRGB( BrushColor ) );
        Top := ( Rect.Top + Rect.Bottom - FImages.Height ) div 2;
        if ( Item.OverlayIndex < 0 ) then
          FImages.Draw( Canvas, Rect.Left + Indent * FItemIndent + 2, Top, Item.ImageIndex )
        else
          FImages.DrawOverlay( Canvas, Rect.Left + Indent * FItemIndent + 2, Top,
                               Item.ImageIndex, Item.OverlayIndex );
      finally
        ImageList_SetBkColor( FImages.Handle, OldBkColor );
      end;
    end;

    if odSelected in State then
      Canvas.Brush.Color := clHighlight;

    if Item.Caption <> '' then
    begin
      if Assigned( FImages ) then
        X := FImages.Width + 4
      else
        X := 4;
      R.Left := Rect.Left + Indent * FItemIndent + 2 + X - 1;
      R.Top := Rect.Top;
      R.Right := R.Left + Canvas.TextWidth( Item.Caption ) + 1 + 2;
      R.Bottom := Rect.Bottom - 1;

      if UsingSystemStyle then
      begin
        if Enabled then
        begin
          if odSelected in State then
            Canvas.Font.Color := clHighlightText
          else
            Canvas.Font.Color := Font.Color
        end
        else
          Canvas.Font.Color := clBtnShadow;
      end
      else // VCL Styles
      begin
        if Enabled then
        begin
          if odSelected in State then
            Canvas.Font.Color := ActiveStyleFontColor( sfComboBoxItemSelected )
          else
            Canvas.Font.Color := ActiveStyleFontColor( sfComboBoxItemNormal );
        end
        else
          Canvas.Font.Color := ActiveStyleFontColor( sfComboBoxItemDisabled );
      end;

      Canvas.TextRect( R, R.Left + 1, R.Top + 1, Item.Caption );
    end;
  end;
end; {= TRzCustomImageComboBox.DrawItem =}


procedure TRzCustomImageComboBox.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
var
  Brush: TBrush;
begin
  Brush := TBrush.Create;
  if Owner is TForm then
    Brush.Color := TForm( Owner ).Color
  else
    Brush.Color := clWindow;
  Windows.FillRect( Msg.DC, ClientRect, Brush.Handle );
  Brush.Free;
  Msg.Result := 1;
end;


procedure TRzCustomImageComboBox.WMSetFont( var Msg: TWMSetFont );
begin
  if not FInWMSetFont then
  begin
    try
      FInWMSetFont := True;
      AutoSize( Msg.Font );
      inherited;
    finally
      FInWMSetFont := False;
    end;
  end
  else
    inherited;
end;


function TRzCustomImageComboBox.AddItem( Caption: string; ImageIndex: Integer; IndentLevel: Integer ): TRzImageComboBoxItem;
const
  NOSTRING: string = '';
begin
  Result := TRzImageComboBoxItem.Create( Self );
  Result.FCaption := Caption;
  Result.FImageIndex := ImageIndex;
  Result.FIndentLevel := IndentLevel;
  if RunningUnder( WinNT ) then
    Result.FIndex := inherited Items.AddObject( NOSTRING, Result )
  else
    Result.FIndex := inherited Items.AddObject( Caption, Result );
end;


procedure TRzCustomImageComboBox.ItemsBeginUpdate;
begin
  inherited Items.BeginUpdate;
end;


procedure TRzCustomImageComboBox.ItemsEndUpdate;
begin
  inherited Items.EndUpdate;
end;


procedure TRzCustomImageComboBox.DoAutoSize( HF: HFONT );
var
  H: Integer;
  OldF: HFONT;
  DC: HDC;
  TM: TTextMetric;
begin
  DC := GetDC( 0 );
  OldF := 0;
  try
    Oldf := SelectObject( DC, HF );
    GetTextMetrics( DC, TM );
    H := Abs( TM.tmHeight ) + 4;
  finally
    if ( Oldf <> 0 ) then
      SelectObject( DC, OldF );
    ReleaseDC( 0, dc );
  end;
  if Assigned( FImages ) and ( FImages.Height > H ) then
    H := FImages.Height;
  ItemHeight := H;
end;


procedure TRzCustomImageComboBox.AutoSize( HF: HFONT );
begin
  if AutoSizeHeight then
    DoAutoSize( HF );
end;


procedure TRzCustomImageComboBox.SetItemIndent( Value: Integer );
begin
  if FItemIndent <> Value then
  begin
    FItemIndent := Value;
    Invalidate;
  end;
end;


procedure TRzCustomImageComboBox.SetImages( const Value: TCustomImageList );
begin
  FImages := Value;
  Invalidate;
  if Assigned( FImages ) then
    FImages.FreeNotification( Self );
  if not ( csLoading in ComponentState ) then
    AutoSize( Font.Handle );
end;


procedure TRzCustomImageComboBox.DeleteItem( Item: Pointer );
begin
  if Assigned( OnDeleteItem ) then
    OnDeleteItem( Self, TRzImageComboBoxItem( Item ) );

  // This method gets called as a result of calling Items.Move and Items.Exchange.
  // As such, we do not want to free the associated object in these cases.
  if ( FFreeObjOnDelete or ( csDestroying in ComponentState ) ) and Assigned( Item ) then
    TObject( Item ).Free;
end;


procedure TRzCustomImageComboBox.GetItemData( Item: TRzImageComboBoxItem );
begin
  if Assigned( FOnGetItemData ) then
    FOnGetItemData( Self, Item );
end;


procedure TRzCustomImageComboBox.Delete( Index: Integer );
begin
  FFreeObjOnDelete := True;
  try
    Items.Delete( Index );
  finally
    FFreeObjOnDelete := False;
  end;
end;


procedure TRzCustomImageComboBox.ClearItems;
var
  I: Integer;
begin
  ItemsBeginUpdate;
  try
    for I := Items.Count - 1 downto 0 do
      Delete( I );
  finally
    ItemsEndUpdate;
  end;
end;


function TRzCustomImageComboBox.IndexOf( const S: string ): Integer;

  function GetCaption( Idx: Integer ): string;
  var
    Item: TRzImageComboBoxItem;
  begin
    Item := inherited Items.Objects[ Idx ] as TRzImageComboBoxItem;
    if Item <> nil then
      Result := Item.Caption
    else
      Result := '';
  end;

begin
  for Result := 0 to Items.Count - 1 do
  begin
    if AnsiCompareText( GetCaption( Result ), S ) = 0 then
      Exit;
  end;
  Result := -1;
end;


function TRzCustomImageComboBox.IndexOfItem( Item: TRzImageComboBoxItem ): Integer;
var
  ComboItem: TRzImageComboBoxItem;
begin
  for Result := 0 to Items.Count - 1 do
  begin
    ComboItem := inherited Items.Objects[ Result ] as TRzImageComboBoxItem;
    if ComboItem = Item then
      Exit;
  end;
  Result := -1;
end;


function TRzCustomImageComboBox.IndexOfData( Data: Pointer ): Integer;
var
  ComboItem: TRzImageComboBoxItem;
begin
  for Result := 0 to Items.Count - 1 do
  begin
    ComboItem := inherited Items.Objects[ Result ] as TRzImageComboBoxItem;
    if ComboItem.Data = Data then
      Exit;
  end;
  Result := -1;
end;



{&RUIF}
end.

