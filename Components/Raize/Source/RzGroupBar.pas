{===============================================================================
  RzGroupBar Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzGroupBar
    Container control that manages one or more TRzGroup instance.  Groups are
    displayed in an XP category view style, or in an Outlook navigator bar style.

  TRzGroup
    Container control used within a TRzGroupBar to hold other controls or
    TRzGroupItems collection.

  TRzGroupItems
    Collection class to hold TRzGroupItem instances.

  TRzGroupItem
    Represents an entry on a TRzGroup--entry can be clicked to invoke OnClick
    event or an Action.

  TRzGroupTemplate
    Nonvisual component used to define a template that can be used to create
    actual instances of TRzGroup controls.

  TRzGroupTemplateItems
    Collection class to hold TRzGroupTemplateItem instances.

  TRzGroupTemplateItem
    Represents an entry in a TRzGroupTemplate.

  TRzGroupController
    Nonvisual component used to control the appearance of TRzGroup controls.


  Modification History
  ------------------------------------------------------------------------------
  6.2    (16 Jul 2015)
    * Fixed issues in TRzGroupBar where items and group captions would not be
      displayed in the correct color under certain VCL Styles.
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * Fixed issue in TRzGroupBar where registered style hook was not correctly
      unregistered when the hook was no longer needed.
  ------------------------------------------------------------------------------
  6.1.7  (07 Mar 2014)
    * Fixed 'group has no parent window' issue in TRzGroupBar.
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * When the TRzGroupBar is reparented, the scroll bar position is reset so
      that the scroll bar position is at the top of the control.
    * Fixed issue with WPARAM and LPARAM values in TRzGroupItem methods.
  ------------------------------------------------------------------------------
  6.1.5  (02 Oct 2013)
    * Added ItemHotZone property to TRzGroup and TRzGroupController. This
      property controls the hot zone for items in the group. The default is
      ihzImageCaption, which means the hot zone is when the mouse is positioned
      over the image or caption. The other option is ihzItemWidth, which means
      the hot zone will be the entire width of the item extending beyond the
      caption.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzGroupBar to fully support VCL Styles
      introduced in RAD Studio XE2. When VCL Styles are used, the TRzGroupBar
      utilizes style appropriate colors to colorize the groups depending on the
      GroupBar style (i.e. CategoryView, TaskList, Outlook).
    * Made necessary modifications to TRzGroupBar to support 64-bit development.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue where changes to the GroupBorderSize property in TRzGroupBar
      at design-time were not being honored at runtime of the Style of the
      TRzGroupBar was something other than gbsCategoryView.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzGroupBar and TRzGroup controls.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added HideAccelerators property to TRzGroup and TRzGroupController.
      Normally, the captions of group items are stripped of their Ampersand (&)
      characters before display to handle situations where actions are assigned
      to group items, and the actions' captions have accelerators in them.
      Set this property to False to display the & characters in the group item
      captions.
    * Added new SelectionFontColor property to TRzGroup and TRzGroupController.
      This property is used to specify the text color of a selected item in a
      group. Like this SelectionColor and SelectionFrameColor properties, the
      SelctionFontColor property only takes affect when the
      TRzGroupBar.GradientColorStyle is set to gcsCustom.
    * Moved the TRzGroupBar.IsScrollBarVisible method to the public section.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Added the public property TopItem to the TRzGroup class.  This property
      is used when the group's Style property is set to gbsOutlook, and
      specifically controls the item that is displayed at the top of the group.
      The scroll buttons that appear in a gbsOutlook style group internally
      change the top item index value.  The new TopItem property provides
      programmatic access to this same value.
    * If an item is partially visible in a gbsOutlook style group in a
      TRzGroupBar and the item is clicked, after the click event is processed,
      the TopItem property is adjusted so that the clicked item is completely
      in view.
    * Fixed problem where the initial position of a scroll bar in a TRzGroupBar
      would be positioned at the bottom of the view on startup if there were
      more groups than could fit in the current view.
  ------------------------------------------------------------------------------
  4.3.1  (21 Sep 2007)
    * Added a new IgnoreSelectedItemClick property to the TRzGroup. By default
      this property is True and is tied to the functionality added in version
      4.3, where a selected item would not generate another OnClick event if the
      item was clicked again. Setting the IgnoreSelectedItemClick property to
      False, causes the TRzGroup to generate OnClick events each time the item
      is clicked.
    * Fixed problem where clicking an item in a TRzGroup would not invoke the
      OnClick event if the group was not positioned at the top of the
      TRzGroupBar.
    * As a result of the new scrolling design introduced in 4.3 it was possible
      to move and reposition groups such that the groups were no longer
      arranged appropriately. This has been fixed.
    * The group bar not rearranging groups when a group was hidden was also
      related to the above issue. This too has been fixed.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Redesigned the way scrolling is performed in the TRzGroupBar. The result
      is that scrolling a TRzGroupBar with several groups no longer results in
      excessive flicker and delayed painting.
    * As a result of the above change, the TRzGroupBar.PositionGroups method
      was moved to the protected section. If you were calling this method, you
      need to replace the call with the new TRzGroupBar.UpdateLayout method.
    * Added new methods BeginUpdateLayout and EndUpdateLayout to TRzGroupBar.
      These methods are useful when you need to make a lot of changes to a
      TRzGroupBar that result in changes to group layout. For example, adding
      a lot of new groups to the TRzGroupBar. Wrap the changes in calls to
      BeginUpdateLayout and EndUpdateLayout to boost performance.
    * The TRzGroupBar.ShowEntireGroup method was made public.
    * Fixed problem where removing the first group in a gbsOutlook style
      TRzGroupBar would raise an "Index out of Range" exception if the group bar
      contained just two groups.
    * Added a new default value for the TRzGroup.ItemSelectionStyle property:
      issImageAndCaption. With this new style, when the ShowItemSelection
      property is set to True, the selection bar is drawn around both the
      caption and image of the selected item.
    * When the ShowItemSelection property is set to True--indicating a mutually
      exclusive mode, if the currently selected item is clicked again, the
      associated OnClick event or action is NOT fired a second time. This
      behavior makes it more compatible with other mutually exclusive lists of
      items such as radio buttons.
    * Fixed problem where you could not turn off TRzGroupBar.UniqueItemSelection
      once you set it to True.
    * Fixed problem where you could not set the TRzGroup.ItemIndex property to
      0, which represents the first item in the group.
    * Updated TRzGroup.ItemAtPos method to check the Visible property of an item
      before checking mouse position.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Added CloseAllGroups and OpenAllGroups to TRzGroupBar. This method is only
      effective when the group bar Style is set to gbsCategoryView.
    * Fixed problem where TRzGroup.DividerVisible property would not be honored
      when changed at design-time and the group style was set to gbsTaskList.
    * Fixed problem where TRzGroupBar would not auto size correctly when aligned
      to the top or bottom of a form.
    * TRzGroupBar now correctly adjusts its size when groups are opened and
      closed (in gbsCategoryView) and AutoSize is set to True.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Fixed problem where non-visible category style groups with CaptionStyle
      set to csLarge, would still get the associated image painted in the group
      bar.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Fixed problem where reading a TRzGroupBar previously saved using an
      earlier version of Raize Components would result in a stream error. The
      problem would occur when the user modified the TRzGroupBar.ColorAdjustment
      property in the old version and then tried to load the form file
      containing the group bar under 4.0.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Several changes were made to TRzGroupBar and supporting controls regarding
      the user interface. Specifically, the ThemeAware, UseGradients, and
      ColorAdjustment properties have been removed. They have been replaced
      with the new VisualStyle property, which can be set to vsClassic, vsWinXP,
      or vsGradient.  The default value for TRzGroupBar is vsGradient, which
      means that gradient fills will be used to paint the group bar and the
      groups it contains to provide a sophisticated look. The colors of the
      various elements of the group bar are managed by the new
      GradientColorStyle property, which can be set to gcsSystem (the default),
      gcsMSOffice, or gcsCustom. When the gcsMSOffice value is specified and XP
      Themes are present, the control colors match those used by Microsoft
      Office products. When XP Themes are not present, the selection defaults to
      gcsSystem. The end result is that when choosing gcsSystem or gcsMSOffice,
      you will get an appropriate appearance that blends in naturally with the
      current user's color scheme. If you would prefer to have the control drawn
      using the XP theme elements, then simply set the VisualStyle property to
      vsWinXP. If XP themes are not present, then the control defaults to
      vsGradient.
    * Fixed problem where a group that had its Visible property set to False at
      design-time would not appear when Visible was set to True at runtime. This
      problem only occurred for groups that were not empty and did not contain
      any group items.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzGroup to
      account for changes introduced in Borland Developer Studio 2006.
    * Added OnEnter and OnExit events to TRzGroup.
    * Setting the Caption of a group item to '-' results in a separator line
      being drawn in the group for TRzGroup.ItemStyle values of isLarge and
      isSmall.
    * The ItemIndex property of the TRzGroup has been changed from a read-only
      property to a read/write property. This allows the developer greater
      flexibility when programmatically setting item selection.  Of course,
      using the Selected property of a TRzGroupItem still functions as before.
    * Added the UniqueItemSelection property to TRzGroupBar, which when set to
      True results in only one group item to be selected at any one time across
      all groups in the group bar.
    * Added new AddToGroup method to the TRzGroup class.  This method is
      designed to be used with TRzGroupTemplate instances. In particular, the
      AddToGroup method can be used to take the items from a template and add
      them to an existing group instead of only allowing the template to be
      used in creating a new group.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Refined the frequency TRzGroup.OnCanOpen and TRzGroup.OnCanClose events
      are fired.  Now, these events are only fired when the group's Open/Close
      state is about to change.
    * In the gbsOutlook style, the selected Group's OnOpen event is now called
      after all of the other groups have been closed.
    * Added new CaptionStyle property to TRzGroup, which controls which style
      of icon is displayed in the group's caption. When set to csSmall, the
      CaptionImageIndex image from the SmallImages image list is displayed.
      When CaptionStyle is set to csLarge, the CaptionImageIndex image from the
      LargeImages image list is displayed. When a large image (32x32 pixels) is
      displayed, the top of the image extends beyond the top of the group's
      caption area.
    * The CaptionStyle property was also added to TRzGroupTemplate.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem where group caption would not get repainted when
      CaptionColorDefault property was changed.
  ------------------------------------------------------------------------------
  3.0.12 (15 Dec 2004)
    * Further refined solution to problem of hidden groups not appearing when
      made visible.  This in turn fixed the problem introduced in 3.0.11 of not
      being able to resize the height of groups.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where a List Index out of Bounds exception would get raised
      if a TRzGroupBar did not have any groups in it and was set to gbsOutlook.
    * Fixed problem where Caption of TRzGroup in gbsTaskList style would get
      clipped if the group was too thin.
    * Fixed problem where setting a TRzGroup's Visible property to False at
      design-time did not hide the group at runtime.
    * Fixed problem where a group that was hidden does not appear when made
      visible. The problem was that the open height of the group was set to 0.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Added ItemSelectionStyle to TRzGroup and TRzGroupController. This property
      determines whether the image associated with an item is highlighted when
      selected, or the item's Caption.
    * Fixed problem where clicking on the first visible item in a group
      (in gbsOutlook style) would select the first item in the group instead of
      the first visible item.
    * Fixed problem where Font.Style settings were ignored by group items when
      assigned to an action or have an OnClick event handler.
    * Fixed problem where hints would not show up correctly when the mouse was
      positioned over the image associated with a group item.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Added FontColor and FontStyle properties to TRzGroupTemplateItem.
    * When creating a TRzGroup from a TRzGroupTemplate, the Tag property of the
      template is now transferred to the group.
    * Surfaced the PopupMenu property for TRzGroup.
    * Added ItemPopupMenu property to TRzGoup. This menu is displayed when the
      user right-clicks an item in the group.
    * Added an OnItemPopupMenu event to TRzGroup. This event is raised just
      before the ItemPopupMenu is displayed for an item. Use this event to make
      state changes or even assign differt menus to ItemPopupMenu.
    * Added PopupItem read-only public property to TRzGroup. This property
      references the TRzGroupItem for which the popup menu was displayed. This
      property is usefully within TMenuItem.OnClick or TAction.OnExecute event
      handlers to identify the group item.
    * Removed the OnPopupClick event of TRzGroupItem. With the above changes,
      this event is no longer necessary.
    * Added ItemPopupMenu and PopupMenu properties to TRzGroupTemplate. Also
      added the OnItemPopupMenu event.
    * Fixed problem where separator items in a TRzGroup would get hidden
      inappropriately.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where programmatically setting an item's Selected property
      to True did not cause the corresponding group's ItemIndex property to be
      updated appropriately.
    * System cursor (IDC_HAND) is now used instead of the custom hand cursor
      when running under Windows 98 or higher.
    * Added new GradientPath property to TRzGroupBar. This property controls the
      way the gradient background of the group bar is drawn.
    * Added ItemStaticFont property to TRzGroup. Use this property to specify a
      different font for items in a group that do not have an OnClick event
      handler nor an assigned Action.
    * Added FlatColorAdjustment property.
    * Fixed problem where setting BorderInner or BorderOuter to fsFlatBold would
      not display border.
    * Added fsFlatRounded to inner and outer border styles.
    * Refactored inner and outer border painting to common DrawInnerOuterBorders
      function.
    * Fixed problem in WMNCCalcSize where border styles of fsFlatBold and the
      new fsFlatRounded were not property considered in the size calculation.
    * Added OnMouseOverItem event to the TRzGroup component. This event is
      triggered as the mouse is moved over a linked item (i.e. OnClick or Action
      is assigned) in a TRzGroup.
    * Fixed problem where dynamically created group needed to have parent set
      before adding to a TRzGroupBar.
    * Fixed problem where removing a group from a TRzGroupBar by calling
      RemoveGroup would leave remants of the group on the group bar's display.
    * TRzGroup will now automatically hide separators that are adjacent to other
      separators or located at the top or bottom of the group.
    * Added the TRzGroupTemplate component, and the supporting
      TRzGroupTemplateItems and TRzGroupTemplateItem classes.  The
      TRzGroupTemplate component is used to define a template (of items) that
      can be used to dynamically create actual instances of TRzGroup controls.
    * The TRzGroupBar.AddGroup and TRzGroupBar.InsertGroup methods have been
      overloaded with versions that accept a TRzGroupTemplate. These methods
      create a new instance of a TRzGroup and populate the group with items
      matching those defined in the template.
    * Added the TRzGroupController component. This nonvisual component can be
      used to manipulate the appearance of multiple TRzGroup controls via the
      properties of the TRzGroupController component.
    * Added FontColor and FontStyle properties to TRzGroupItem.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where a group with Style=gbsOutlook and more icons than can
      be displayed fully would display the down scrolling button in the wrong
      position.
    * Fixed problem where a PopupMenu assigned to the TRzGroupBar would appear
      when the user right- clicked on a group item. In order to display a popup
      menu on a group item, the PopupMenu must be invoked from within the
      OnPopupClick event of the item.
    * Added ItemHotColor property.
    * Added dt_RtlReading flag to DrawText function calls when
      UseRightToLeftAlignment = True.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Added separation between items when TRzGroup.ItemStyle = isLarge to more
      clearly associate the a caption to its icon.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * The TRzGroupBar now displays groups and items correctly (i.e. right
      aligned) when running under Right-To-Left locales.
    * Added public Data property to TRzGroup.
    * Added mouse wheel support.
    * When a group is opened and its opened height is greater than the height of
      the group bar, the group is positioned so that the top of the group is in
      view.
    * Fixed problem where scrollbar thumb would in some cases get positioned
      incorrectly when a group was opened.
    * Fixed problem where gradient background of group bar would get corrupted.
    * Added ItemDestroyed notification method to TRzGroup in order to update
      ItemIndex property when items are removed the group.
    * Fixed problem where group items did not invoke standard action OnExecute
      events.
    * In gbsCategoryView, groups now align child controls so that they do not
      cover the border of the group.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Test FullColorSupport before all calls to PaintGradient.
    * Added code to remove & chars that cause underscores to appear in item
      captions.
    * Fixed problem where border around large icons did not appear when color of
      group was clBtnFace.
    * Added Selected property to TRzGroupItem.
    * Added ShowItemSelection to TRzGroup.
    * Items displayed as links only when an OnClick event is assigned.
    * Fixed problem where groups would not appear on Load when TRzGroupBar
      parented by panel.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzGroupBar;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Windows,
  Messages,
  SysUtils,
  Graphics,
  Forms,
  Menus,
  Dialogs,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  ExtCtrls,
  StdCtrls,
  Controls,
  Buttons,
  ActnList,
  Classes,
  RzCommon,
  RzSpnEdt;

const
  um_ResetScrollPosition = wm_User + $2024;

type
  ERzGroupBarError = class( Exception );

  TRzCaptionState = ( csNormal, csHot, csDown );
  TRzCanOpenEvent = procedure( Sender: TObject; var CanOpen: Boolean ) of object;
  TRzCanCloseEvent = procedure( Sender: TObject; var CanClose: Boolean ) of object;

  TRzGroupBarStyle = ( gbsCategoryView, gbsTaskList, gbsOutlook );
  TRzItemStyle = ( isSmall, isLarge );
  TRzCaptionStyle = ( csSmall, csLarge );
  TRzItemSelectionStyle = ( issImage, issCaption, issImageAndCaption );

  TRzGroupBar = class;
  TRzGroup = class;
  TRzGroupClass = class of TRzGroup;
  TRzGroupItem = class;

  TRzGroupTemplate = class;
  TRzGroupTemplateItem = class;

  TRzGroupController = class;


  {==============================================}
  {== TRzGroupItemActionLink Class Declaration ==}
  {==============================================}

  TRzGroupItemActionLink = class( TActionLink )
  protected
    FClient: TRzGroupItem;
    procedure AssignClient( AClient: TObject ); override;
    function DoShowHint( var HintStr: string ): Boolean; virtual;

    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;

    procedure SetCaption( const Value: string ); override;
    procedure SetChecked( Value: Boolean ); override;
    procedure SetEnabled( Value: Boolean ); override;
    procedure SetHint( const Value: string ); override;
    procedure SetImageIndex( Value: Integer ); override;
    procedure SetVisible( Value: Boolean ); override;
    procedure SetOnExecute( Value: TNotifyEvent ); override;
  end;

  TRzGroupItemActionLinkClass = class of TRzGroupItemActionLink;

  TCMGroupItemMsg = packed record
    Msg: Cardinal;
    Unused: Integer;
    {$IFDEF CPUX64}
    Reserved1: TDWordFiller;
    {$ENDIF}
    Sender: TRzGroupItem;
    {$IFDEF CPUX64}
    Reserved2: TDWordFiller;
    {$ENDIF}
    Result: LRESULT;
  end;

  {====================================}
  {== TRzGroupItem Class Declaration ==}
  {====================================}

  TRzGroupItem = class( TCollectionItem )
  private
    FCaption: TCaption;
    FCaptionChanged: Boolean;
    FDisabledIndex: TImageIndex;
    FEnabled: Boolean;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FHint: string;
    FImageIndex: TImageIndex;
    FIndentLevel: Byte;
    FVisible: Boolean;
    FSelected: Boolean;
    FTag: Longint;
    FData: Pointer;
    FOnClick: TNotifyEvent;

    {$IFDEF USE_GROUP_DESIGNER}
    FSkipDesignerSelection: Boolean;
    {$ENDIF}

    FActionLink: TRzGroupItemActionLink;
    FClickingCaption: Boolean;
    FCaptionState: TRzCaptionState;
    FHotCaptionRect: TRect;
    FHotImageRect: TRect;

    { Internal Event Handlers }
    procedure ActionChangeHandler( Sender: TObject );

    { Message Handling Methods }
    procedure CMGroupItemSelected( var Msg: TCMGroupItemMsg ); message cm_GroupItemSelected;
  protected
    function GetDisplayName: string; override;

    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); dynamic;

    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsVisibleStored: Boolean;
    function IsOnClickStored: Boolean;

    function GetAction: TBasicAction; virtual;
    procedure SetAction( Value: TBasicAction ); virtual;
    function GetActionLinkClass: TRzGroupItemActionLinkClass; dynamic;
    procedure SetCaption( const Value: TCaption ); virtual;
    procedure SetDisabledIndex( Value: TImageIndex ); virtual;
    procedure SetEnabled( Value: Boolean ); virtual;
    procedure SetFontColor( Value: TColor ); virtual;
    procedure SetFontStyle( Value: TFontStyles ); virtual;
    function GetGroup: TRzGroup; virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;
    procedure SetIndentLevel( Value: Byte ); virtual;
    procedure SetVisible( Value: Boolean ); virtual;

    {$IFDEF USE_GROUP_DESIGNER}
    function GetSelected: Boolean; virtual;
    {$ENDIF}
    procedure SetSelected( Value: Boolean ); virtual;
    procedure UpdateSelection;

    property ActionLink: TRzGroupItemActionLink
      read FActionLink
      write FActionLink;
  public
    constructor Create( Collection: TCollection ); override;
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;
    procedure AssignTo( Dest: TPersistent ); override;

    procedure InitiateAction; virtual;
    procedure Click; dynamic;

    property HotCaptionRect: TRect
      read FHotCaptionRect
      write FHotCaptionRect;

    property HotImageRect: TRect
      read FHotImageRect
      write FHotImageRect;

    property CaptionState: TRzCaptionState
      read FCaptionState
      write FCaptionState;

    property ClickingCaption: Boolean
      read FClickingCaption
      write FClickingCaption;

    property Data: Pointer
      read FData
      write FData;

    property Group: TRzGroup
      read GetGroup;
  published
    property Action: TBasicAction
      read GetAction
      write SetAction;

    property Caption: TCaption
      read FCaption
      write SetCaption
      stored IsCaptionStored;

    property DisabledIndex: TImageIndex
      read FDisabledIndex
      write SetDisabledIndex
      default -1;
      
    property Enabled: Boolean
      read FEnabled
      write SetEnabled
      stored IsEnabledStored
      default True;

    property FontColor: TColor
      read FFontColor
      write SetFontColor
      default clNone;

    property FontStyle: TFontStyles
      read FFontStyle
      write SetFontStyle
      default [];

    property Hint: string
      read FHint
      write FHint
      stored IsHintStored;

    property ImageIndex: TImageIndex
      read FImageIndex
      write SetImageIndex
      stored IsImageIndexStored
      default -1;

    property IndentLevel: Byte
      read FIndentLevel
      write SetIndentLevel
      default 0;

    property Selected: Boolean
      read FSelected
      write SetSelected
      default False;

    {$IFDEF USE_GROUP_DESIGNER}
    property SelectedInDesigner: Boolean
      read GetSelected
      write SetSelected
      stored False;
    {$ENDIF}

    property Tag: Longint
      read FTag
      write FTag
      default 0;

    property Visible: Boolean
      read FVisible
      write SetVisible
      default True;

    property OnClick: TNotifyEvent
      read FOnClick
      write FOnClick
      stored IsOnClickStored;
  end;


  {=====================================}
  {== TRzGroupItems Class Declaration ==}
  {=====================================}

  TRzGroupItems = class( TCollection )
  private
    FGroup: TRzGroup;
    function GetItem( Index: Integer ): TRzGroupItem;
    procedure SetItem( Index: Integer; Value: TRzGroupItem );
  protected
    function GetOwner: TPersistent; override;
    procedure Update( Item: TCollectionItem ); override;
  public
    // Note: No override on constructor
    constructor Create( Group: TRzGroup );

    function Add: TRzGroupItem;

    // Array property provides access to collection items
    property Items[ Index: Integer ]: TRzGroupItem
      read GetItem
      write SetItem; default;

    property Group: TRzGroup
      read FGroup;
  end;


  {================================}
  {== TRzGroup Class Declaration ==}
  {================================}

  TRzGroupItemHotZone = ( ihzImageCaption, ihzItemWidth );

  TRzMouseOverItemEvent = procedure( Sender: TObject; Item: TRzGroupItem ) of object;

  TRzGroupItemPopupMenuEvent = procedure( Sender: TObject; Item: TRzGroupItem ) of object;

  TRzGroup = class( TCustomControl )
  private
    FGroupBar: TRzGroupBar;
    FGroupController: TRzGroupController;

    FOpenedHeight: Integer;
    FOpened: Boolean;
    FCanClose: Boolean;

    FStyle: TRzGroupBarStyle;
    FColorDefault: Boolean;
    FCaptionColor: TColor;
    FCaptionColorStart: TColor;
    FCaptionColorStop: TColor;
    FCaptionColorDefault: Boolean;
    FCaptionHotColor: TColor;
    FCaptionImageIndex: TImageIndex;
    FCaptionHeight: Integer;
    FCaptionFont: TFont;
    FCaptionFontChanged: Boolean;
    FItemHotColor: TColor;
    FItemHotZone: TRzGroupItemHotZone;
    FFontChanged: Boolean;
    FItemStaticFont: TFont;
    FItemStaticFontChanged: Boolean;
    FDividerVisible: Boolean;
    FDividerColor: TColor;
    FBorderColor: TColor;
    FSpecial: Boolean;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FSelectionShadowColor: TColor;
    FSelectionFrameColor: TColor;
    FHideAccelerators: Boolean;
    FShowItemSelection: Boolean;
    FItemSelectionStyle: TRzItemSelectionStyle;
    FIgnoreSelectedItemClick: Boolean;

    FCaptionState: TRzCaptionState;
    FClickingCaption: Boolean;
    FCaptionCursor: HCursor;

    FItems: TRzGroupItems;
    FItemStyle: TRzItemStyle;
    FItemHeight: Integer;
    FItemIndent: Byte;
    FTopItem: Integer;
    FActionClientCount: Integer;
    FCaptionStyle: TRzCaptionStyle;
    FItemIndex: Integer;
    FOverItem: TRzGroupItem;

    FData: Pointer;

    FSmallImages: TCustomImageList;
    FSmallImagesChangeLink: TChangeLink;
    FLargeImages: TCustomImageList;
    FLargeImagesChangeLink: TChangeLink;

    FScrollUpBtn: TRzRapidFireButton;
    FScrollDownBtn: TRzRapidFireButton;

    FOnCanClose: TRzCanCloseEvent;
    FOnCanOpen: TRzCanOpenEvent;
    FOnClose: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FOnMouseOverItem: TRzMouseOverItemEvent;

    FItemPopupMenu: TPopupMenu;
    FPopupItem: TRzGroupItem;
    FOnItemPopupMenu: TRzGroupItemPopupMenuEvent;

    function DrawCaptionImage: Integer;
    function CaptionImageHeight: Integer;
    function CaptionYOffset: Integer;
    function HeaderHeight: Integer;

    { Internal Event Handlers }
    procedure ImagesChange( Sender: TObject );
    procedure CaptionFontChangeHandler( Sender: TObject );
    procedure ItemStaticFontChangeHandler( Sender: TObject );
    procedure ScrollUpHandler( Sender: TObject );
    procedure ScrollDownHandler( Sender: TObject );

    { Message Handling Methods }
    procedure CMGroupItemSelected( var Msg: TCMGroupItemMsg ); message cm_GroupItemSelected;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message cm_DesignHitTest;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMHintShow( var Msg: TMessage ); message cm_HintShow;
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure WMWindowPosChanged( var Msg: TWMWindowPosChanged ); message wm_WindowPosChanged;
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    procedure CreateScrollButtons;
    procedure UpdateScrollBtnPositions;
    procedure UpdateScrollBtnVisibility;
    procedure Resize; override;
    procedure UpdateGroupBarLayout;

    {$IFDEF USE_GROUP_DESIGNER}
    procedure DrawDesignFocus( var Bounds: TRect ); virtual;
    {$ENDIF}
    function CalcBaseXOffset: Integer; virtual;
    function SkipSeparator( Index: Integer ): Boolean;
    procedure DrawItems; virtual;
    procedure DrawOpenCloseButton( Opened: Boolean; Left, Top: Integer;
                                   FillColor, LineColor, ChevronColor: TColor ); virtual;

    procedure DrawCategoryGroup; virtual;
    procedure DrawThemedCategoryGroup; virtual;
    procedure DrawOutlookGroup; virtual;
    procedure DrawTaskListGroup; virtual;
    procedure Paint; override;

    procedure Loaded; override;
    procedure ReadState( Reader: TReader ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure SetParent( AParent: TWinControl ); override;

    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;
    function ClosedHeight: Integer;
    procedure ChangeScale( M, D: Integer ); override;
    function CaptionRect: TRect;
    function CursorPosition: TPoint;

    procedure ToggleState; virtual;

    procedure WndProc( var Msg: TMessage ); override;
    procedure DragOver( Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean ); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;

    procedure ItemDestroyed( Index: Integer ); virtual;

    procedure DisplayItemPopupMenu( Index, X, Y: Integer );

    procedure DrawOutOfBounds;
    procedure InvalidateGroupBar;

    { Event Dispatch Methods }
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure DoContextPopup( MousePos: TPoint; var Handled: Boolean ); override;
    procedure MouseOverItem( Item: TRzGroupItem ); dynamic;

    { Property Access Methods }
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetCanClose( Value: Boolean ); virtual;
    function DefaultCaptionColor( Style: TRzGroupBarStyle ): TColor;
    procedure SetCaptionColor( Value: TColor ); virtual;
    procedure SetCaptionColorStart( Value: TColor ); virtual;
    procedure SetCaptionColorStop( Value: TColor ); virtual;
    function IsCaptionColorStored: Boolean;
    procedure SetCaptionColorDefault( Value: Boolean ); virtual;
    function IsCaptionFontStored: Boolean;
    procedure SetCaptionFont( Value: TFont ); virtual;
    procedure SetCaptionHeight( Value: Integer ); virtual;
    procedure SetCaptionHotColor( Value: TColor ); virtual;
    procedure SetCaptionImageIndex( Value: TImageIndex ); virtual;
    procedure SetCaptionStyle( const Value: TRzCaptionStyle ); virtual;
    procedure SetItemHotColor( Value: TColor ); virtual;
    procedure SetItemHotZone( Value: TRzGroupItemHotZone ); virtual;
    function IsItemStaticFontStored: Boolean;
    procedure SetItemStaticFont( Value: TFont ); virtual;

    function StoreColor: Boolean;
    function GetColor: TColor; virtual;
    procedure SetColor( Value: TColor ); virtual;
    procedure SetColorDefault( Value: Boolean ); virtual;
    function DefaultColor( Style: TRzGroupBarStyle ): TColor;
    procedure SetSelectionColor( Value: TColor ); virtual;
    procedure SetSelectionFontColor( Value: TColor ); virtual;
    procedure SetSelectionShadowColor( Value: TColor ); virtual;
    procedure SetSelectionFrameColor( Value: TColor ); virtual;
    procedure SetHideAccelerators( Value: Boolean ); virtual;
    procedure SetShowItemSelection( Value: Boolean ); virtual;
    procedure SetItemSelectionStyle( Value: TRzItemSelectionStyle ); virtual;

    function GetSelectedItem: TRzGroupItem; virtual;
    procedure SetItems( Value: TRzGroupItems ); virtual;
    procedure SetItemStyle( Value: TRzItemStyle ); virtual;
    procedure SetItemHeight( Value: Integer ); virtual;
    procedure SetItemIndent( Value: Byte ); virtual;
    procedure SetItemIndex( Value: Integer ); virtual;
    procedure SetOpened( Value: Boolean ); virtual;
    procedure SetOpenedHeight( Value: Integer ); virtual;
    procedure SetLargeImages( Value: TCustomImageList ); virtual;
    procedure SetGroupBar( Value: TRzGroupBar ); virtual;
    function GetGroupIndex: Integer; virtual;
    procedure SetGroupIndex( Value: Integer ); virtual;
    procedure SetSmallImages( Value: TCustomImageList ); virtual;
    procedure SetStyle( Value: TRzGroupBarStyle ); virtual;
    procedure SetDividerVisible( Value: Boolean ); virtual;
    procedure SetDividerColor( Value: TColor ); virtual;

    procedure SetItemPopupMenu( Value: TPopupMenu ); virtual;
    procedure SetSpecial( Value: Boolean ); virtual;
    procedure SetTopItem( Value: Integer ); virtual;

    function NotUsingGroupController: Boolean;
    procedure SetGroupController( Value: TRzGroupController ); virtual;

    property Style: TRzGroupBarStyle
      read FStyle
      write SetStyle
      default gbsCategoryView;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure AddToGroup( Template: TRzGroupTemplate );

    function SmallImageList: TCustomImageList;
    function LargeImageList: TCustomImageList;
    function CaptionImageList: TCustomImageList;

    procedure InitiateAction; override;
    procedure ActionClientConnect;
    procedure ActionClientDisconnect;
    procedure AssignActionList( ActionList: TCustomActionList; const Category: string = '' );

    procedure Reposition;
    function CalculateHeight( W: Integer ): Integer;

    function ItemAtPos( P: TPoint ): Integer;
    function ItemsVisible: Integer;

    property Data: Pointer
      read FData
      write FData;

    property GroupBar: TRzGroupBar
      read FGroupBar
      write SetGroupBar;

    property ItemIndex: Integer
      read FItemIndex
      write SetItemIndex;

    property SelectedItem: TRzGroupItem
      read GetSelectedItem;

    property PopupItem: TRzGroupItem
      read FPopupItem;

    property TopItem: Integer
      read FTopItem
      write SetTopItem;
  published
    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      stored NotUsingGroupController
      default clBtnHighlight;

    property CanClose: Boolean
      read FCanClose
      write SetCanClose
      default True;

    property CaptionColor: TColor
      read FCaptionColor
      write SetCaptionColor
      stored IsCaptionColorStored;

    property CaptionColorStart: TColor
      read FCaptionColorStart
      write SetCaptionColorStart
      default clWindow;

    property CaptionColorStop: TColor
      read FCaptionColorStop
      write SetCaptionColorStop
      default clBtnFace;

    property CaptionColorDefault: Boolean
      read FCaptionColorDefault
      write SetCaptionColorDefault
      stored NotUsingGroupController
      default True;

    property CaptionFont: TFont
      read FCaptionFont
      write SetCaptionFont
      stored IsCaptionFontStored;

    property CaptionHeight: Integer
      read FCaptionHeight
      write SetCaptionHeight
      stored NotUsingGroupController
      default 20;

    property CaptionHotColor: TColor
      read FCaptionHotColor
      write SetCaptionHotColor
      stored NotUsingGroupController
      default clHotLight;

    property CaptionImageIndex: TImageIndex
      read FCaptionImageIndex
      write SetCaptionImageIndex
      default -1;

    property CaptionStyle: TRzCaptionStyle
      read FCaptionStyle
      write SetCaptionStyle
      default csSmall;

    property Color: TColor
      read GetColor
      write SetColor
      stored StoreColor;

    property ColorDefault: Boolean
      read FColorDefault
      write SetColorDefault
      stored NotUsingGroupController
      default True;

    property GroupController: TRzGroupController
      read FGroupController
      write SetGroupController;

    property GroupIndex: Integer
      read GetGroupIndex
      write SetGroupIndex
      stored False;

    property Items: TRzGroupItems
      read FItems
      write SetItems;

    property ItemHeight: Integer
      read FItemHeight
      write SetItemHeight
      stored NotUsingGroupController
      default 20;

    property ItemHotColor: TColor
      read FItemHotColor
      write SetItemHotColor
      stored NotUsingGroupController
      default clHotLight;

    property ItemHotZone: TRzGroupItemHotZone
      read FItemHotZone
      write SetItemHotZone
      stored NotUsingGroupController
      default ihzImageCaption;

    property ItemStaticFont: TFont
      read FItemStaticFont
      write SetItemStaticFont
      stored IsItemStaticFontStored;

    property ItemIndent: Byte
      read FItemIndent
      write SetItemIndent
      stored NotUsingGroupController
      default 20;

    property ItemStyle: TRzItemStyle
      read FItemStyle
      write SetItemStyle
      stored NotUsingGroupController
      default isSmall;

    property ItemPopupMenu: TPopupMenu
      read FItemPopupMenu
      write SetItemPopupMenu;

    property LargeImages: TCustomImageList
      read FLargeImages
      write SetLargeImages;

    property Opened: Boolean
      read FOpened
      write SetOpened;

    property OpenedHeight: Integer
      read FOpenedHeight
      write SetOpenedHeight
      default 50;

    property DividerColor: TColor
      read FDividerColor
      write SetDividerColor
      stored NotUsingGroupController
      default clHighlight;

    property DividerVisible: Boolean
      read FDividerVisible
      write SetDividerVisible
      stored NotUsingGroupController;

    property SmallImages: TCustomImageList
      read FSmallImages
      write SetSmallImages;

    property SelectionColor: TColor
      read FSelectionColor
      write SetSelectionColor
      stored NotUsingGroupController
      default clBtnFace;

    property SelectionFontColor: TColor
      read FSelectionFontColor
      write SetSelectionFontColor
      default clWindowText;

    property SelectionShadowColor: TColor
      read FSelectionShadowColor
      write SetSelectionShadowColor
      stored NotUsingGroupController
      default clBtnShadow;

    property SelectionFrameColor: TColor
      read FSelectionFrameColor
      write SetSelectionFrameColor
      stored NotUsingGroupController
      default cl3DDkShadow;

    property HideAccelerators: Boolean
      read FHideAccelerators
      write SetHideAccelerators
      stored NotUsingGroupController
      default True;

    property ShowItemSelection: Boolean
      read FShowItemSelection
      write SetShowItemSelection
      stored NotUsingGroupController
      default False;

    property ItemSelectionStyle: TRzItemSelectionStyle
      read FItemSelectionStyle
      write SetItemSelectionStyle
      stored NotUsingGroupController
      default issImageAndCaption;

    property IgnoreSelectedItemClick: Boolean
      read FIgnoreSelectedItemClick
      write FIgnoreSelectedItemClick
      default True;

    property Special: Boolean
      read FSpecial
      write SetSpecial
      stored NotUsingGroupController
      default False;

    property OnCanClose: TRzCanCloseEvent
      read FOnCanClose
      write FOnCanClose;

    property OnCanOpen: TRzCanOpenEvent
      read FOnCanOpen
      write FOnCanOpen;

    property OnClose: TNotifyEvent
      read FOnClose
      write FOnClose;

    property OnOpen: TNotifyEvent
      read FOnOpen
      write FOnOpen;

    property OnMouseOverItem: TRzMouseOverItemEvent
      read FOnMouseOverItem
      write FOnMouseOverItem;

    property OnItemPopupMenu: TRzGroupItemPopupMenuEvent
      read FOnItemPopupMenu
      write FOnItemPopupMenu;

    { Inherited Properties & Events }
    property BiDiMode;
    property Caption;
    property DragCursor;
    property DragMode;
    property Font;
    property Height stored False;
    property Left stored False;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Top stored False;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;
    property Width stored False;

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


  {======================================================}
  {== TRzGroupTemplateItemActionLink Class Declaration ==}
  {======================================================}

  TRzGroupTemplateItemActionLink = class( TActionLink )
  protected
    FClient: TRzGroupTemplateItem;
    procedure AssignClient( AClient: TObject ); override;

    function IsCaptionLinked: Boolean; override;
    function IsEnabledLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsVisibleLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;

    procedure SetCaption( const Value: string ); override;
    procedure SetEnabled( Value: Boolean ); override;
    procedure SetHint( const Value: string ); override;
    procedure SetImageIndex( Value: Integer ); override;
    procedure SetVisible( Value: Boolean ); override;
    procedure SetOnExecute( Value: TNotifyEvent ); override;
  end;

  TRzGroupTemplateItemActionLinkClass = class of TRzGroupTemplateItemActionLink;


  {============================================}
  {== TRzGroupTemplateItem Class Declaration ==}
  {============================================}

  TRzGroupTemplateItem = class( TCollectionItem )
  private
    FCaption: TCaption;
    FCaptionChanged: Boolean;
    FDisabledIndex: TImageIndex;
    FEnabled: Boolean;
    FFontColor: TColor;
    FFontStyle: TFontStyles;
    FHint: string;
    FImageIndex: TImageIndex;
    FIndentLevel: Byte;
    FVisible: Boolean;
    FTag: Longint;
    FData: Pointer;
    FOnClick: TNotifyEvent;

    FActionLink: TRzGroupTemplateItemActionLink;

    { Internal Event Handlers }
    procedure ActionChangeHandler( Sender: TObject );
  protected
    function GetDisplayName: string; override;

    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); dynamic;

    function IsCaptionStored: Boolean;
    function IsEnabledStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsVisibleStored: Boolean;
    function IsOnClickStored: Boolean;

    function GetAction: TBasicAction; virtual;
    procedure SetAction( Value: TBasicAction ); virtual;
    function GetActionLinkClass: TRzGroupTemplateItemActionLinkClass; dynamic;
    procedure SetCaption( const Value: TCaption ); virtual;
    procedure SetDisabledIndex( Value: TImageIndex ); virtual;
    procedure SetEnabled( Value: Boolean ); virtual;
    procedure SetFontColor( Value: TColor ); virtual;
    procedure SetFontStyle( Value: TFontStyles ); virtual;
    function GetTemplate: TRzGroupTemplate; virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;
    procedure SetIndentLevel( Value: Byte ); virtual;
    procedure SetVisible( Value: Boolean ); virtual;

    property ActionLink: TRzGroupTemplateItemActionLink
      read FActionLink
      write FActionLink;
  public
    constructor Create( Collection: TCollection ); override;
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;
    procedure AssignTo( Dest: TPersistent ); override;

    procedure InitiateAction; virtual;

    property Data: Pointer
      read FData
      write FData;

    property Template: TRzGroupTemplate
      read GetTemplate;
  published
    property Action: TBasicAction
      read GetAction
      write SetAction;

    property Caption: TCaption
      read FCaption
      write SetCaption
      stored IsCaptionStored;

    property DisabledIndex: TImageIndex
      read FDisabledIndex
      write SetDisabledIndex
      default -1;

    property Enabled: Boolean
      read FEnabled
      write SetEnabled
      stored IsEnabledStored
      default True;

    property FontColor: TColor
      read FFontColor
      write SetFontColor
      default clNone;

    property FontStyle: TFontStyles
      read FFontStyle
      write SetFontStyle
      default [];

    property Hint: string
      read FHint
      write FHint
      stored IsHintStored;

    property ImageIndex: TImageIndex
      read FImageIndex
      write SetImageIndex
      stored IsImageIndexStored
      default -1;

    property IndentLevel: Byte
      read FIndentLevel
      write SetIndentLevel
      default 0;

    property Tag: Longint
      read FTag
      write FTag
      default 0;

    property Visible: Boolean
      read FVisible
      write SetVisible
      default True;

    property OnClick: TNotifyEvent
      read FOnClick
      write FOnClick
      stored IsOnClickStored;
  end;


  {=============================================}
  {== TRzGroupTemplateItems Class Declaration ==}
  {=============================================}

  TRzGroupTemplateItems = class( TCollection )
  private
    FTemplate: TRzGroupTemplate;
    function GetItem( Index: Integer ): TRzGroupTemplateItem;
    procedure SetItem( Index: Integer; Value: TRzGroupTemplateItem );
  protected
    function GetOwner: TPersistent; override;
  public
    // Note: No override on constructor
    constructor Create( Template: TRzGroupTemplate );

    function Add: TRzGroupTemplateItem;

    // Array property provides access to collection items
    property Items[ Index: Integer ]: TRzGroupTemplateItem
      read GetItem
      write SetItem; default;

    property Template: TRzGroupTemplate
      read FTemplate;
  end;


  {======================================================}
  {== TRzGroupTemplatePreviewOptions Class Declaration ==}
  {======================================================}

  TRzGroupTemplatePreviewOptions = class( TPersistent )
  private
    FTemplate: TRzGroupTemplate;

    FItemStyle: TRzItemStyle;
    FSmallImages: TCustomImageList;
    FSmallImagesChangeLink: TChangeLink;
    FLargeImages: TCustomImageList;
    FLargeImagesChangeLink: TChangeLink;
  protected
    { Property Access Methods }
    procedure SetLargeImages( Value: TCustomImageList );
    procedure SetSmallImages( Value: TCustomImageList );
  public
    constructor Create( ATemplate: TRzGroupTemplate );
    destructor Destroy; override;
  published
    { Property Declarations }
    property ItemStyle: TRzItemStyle
      read FItemStyle
      write FItemStyle
      default isSmall;

    property LargeImages: TCustomImageList
      read FLargeImages
      write SetLargeImages;

    property SmallImages: TCustomImageList
      read FSmallImages
      write SetSmallImages;
  end;

  {========================================}
  {== TRzGroupTemplate Class Declaration ==}
  {========================================}

  TRzGroupTemplate = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FCanClose: Boolean;
    FCaption: string;
    FCaptionImageIndex: TImageIndex;
    FCaptionStyle: TRzCaptionStyle;
    FOpened: Boolean;
    FSpecial: Boolean;

    FItems: TRzGroupTemplateItems;
    FItemPopupMenu: TPopupMenu;
    FPopupMenu: TPopupMenu;
    FOnItemPopupMenu: TRzGroupItemPopupMenuEvent;

    FPreviewOptions: TRzGroupTemplatePreviewOptions;
  protected
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Property Access Methods }
    procedure SetItems( Value: TRzGroupTemplateItems ); virtual;
    procedure SetItemPopupMenu( Value: TPopupMenu ); virtual;
    procedure SetPopupMenu( Value: TPopupMenu ); virtual;
    procedure SetPreviewOptions( Value: TRzGroupTemplatePreviewOptions );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure AssignActionList( ActionList: TCustomActionList; const Category: string = '' );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property CanClose: Boolean
      read FCanClose
      write FCanClose
      default True;

    property Caption: string
      read FCaption
      write FCaption;

    property CaptionImageIndex: TImageIndex
      read FCaptionImageIndex
      write FCaptionImageIndex
      default -1;

    property CaptionStyle: TRzCaptionStyle
      read FCaptionStyle
      write FCaptionStyle
      default csSmall;

    property Items: TRzGroupTemplateItems
      read FItems
      write SetItems;

    property ItemPopupMenu: TPopupMenu
      read FItemPopupMenu
      write SetItemPopupMenu;

    property PopupMenu: TPopupMenu
      read FPopupMenu
      write SetPopupMenu;

    property Opened: Boolean
      read FOpened
      write FOpened
      default True;

    property PreviewOptions: TRzGroupTemplatePreviewOptions
      read FPreviewOptions
      write SetPreviewOptions;

    property Special: Boolean
      read FSpecial
      write FSpecial
      default False;

    property OnItemPopupMenu: TRzGroupItemPopupMenuEvent
      read FOnItemPopupMenu
      write FOnItemPopupMenu;
  end;



  {===================================}
  {== TRzGroupBar Class Declaration ==}
  {===================================}

  TRzGroupBar = class( TCustomControl )
  {$IFDEF VCL160_OR_HIGHER}
  strict private
    class constructor Create;
    class destructor Destroy;
  {$ENDIF}
  private
    FAboutInfo: TRzAboutInfo;
    FCanvas: TCanvas;
    FGroups: TList;
    FUpdatingLayout: Boolean;
    FUpdateLayoutCount: Integer;

    FStyle: TRzGroupBarStyle;

    FBevelWidth: TBevelWidth;
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
    FGradientPath: TRzGroupBarGradientPath;

    FGroupBorderSize: Integer;

    FScrolling: Boolean;
    FScrollPosition: Integer;

    FExclusiveMode: Boolean;
    FUniqueItemSelection: Boolean;

    FSmallImages: TCustomImageList;
    FSmallImagesChangeLink: TChangeLink;
    FLargeImages: TCustomImageList;
    FLargeImagesChangeLink: TChangeLink;

    { Internal Event Handlers }
    procedure ImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure CMGroupItemSelected( var Msg: TCMGroupItemMsg ); message cm_GroupItemSelected;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMFocusChanged( var Msg: TCMFocusChanged ); message cm_FocusChanged;
    procedure WMVScroll( var Msg: TWMVScroll ); message wm_VScroll;
    procedure WMNCHitTest( var Msg: TWMNCHitTest ); message wm_NCHitTest;
    procedure WMNCCalcSize( var Msg: TWMNCCalcSize ); message wm_NCCalcSize;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure WMThemeChanged( var Msg: TMessage ); message wm_ThemeChanged;
    procedure UMResetScrollPosition( var Msg: TMessage ); message um_ResetScrollPosition;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
  protected
    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure Loaded; override;
    procedure SetChildOrder( Child: TComponent; Order: Integer ); override;
    procedure SetParent( AParent: TWinControl ); override;

    procedure RepaintBorder;
    procedure RepaintGroups;
    procedure Paint; override;

    procedure Resize; override;
    function GetControlExtents: TRect; override;
    procedure PositionGroups;

//    procedure AlignControls( AControl: TControl; var Rect: TRect ); override;

    function GetTotalHeight: Integer;
    procedure ScrollControls( Delta: Integer );
    procedure UpdateScrollBar; virtual;

    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    procedure SetBevelWidth( Value: TBevelWidth ); virtual;
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderInner( Value: TFrameStyleEx ); virtual;
    procedure SetBorderOuter( Value: TFrameStyleEx ); virtual;
    procedure SetBorderSides( Value: TSides ); virtual;
    procedure SetBorderHighlight( Value: TColor ); virtual;
    procedure SetBorderShadow( Value: TColor ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetGradientColorStyle( Value: TRzGradientColorStyle ); virtual;
    procedure SetGradientColorStart( Value: TColor ); virtual;
    procedure SetGradientColorStop( Value: TColor ); virtual;
    procedure SetGradientPath( Value: TRzGroupBarGradientPath ); virtual;
    procedure SetGroupBorderSize( Value: Integer ); virtual;
    function GetGroup( Index: Integer ): TRzGroup; virtual;
    function GetGroupCount: Integer; virtual;
    procedure SetExclusiveMode( Value: Boolean ); virtual;
    procedure SetLargeImages( Value: TCustomImageList ); virtual;
    procedure SetSmallImages( Value: TCustomImageList ); virtual;
    procedure SetScrollPosition( Value: Integer ); virtual;
    procedure SetStyle( Value: TRzGroupBarStyle ); virtual;
    procedure SetUniqueItemSelection( Value: Boolean ); virtual;
    procedure SetVisualStyle( Value: TRzVisualStyle ); virtual;

    property TotalHeight: Integer
      read GetTotalHeight;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;

    procedure ShowEntireGroup( Group: TRzGroup );
    procedure ScrollInView( AControl: TControl );
    function IsScrollBarVisible: Boolean;

    procedure UpdateLayout( Group: TRzGroup = nil );
    procedure BeginUpdateLayout;
    procedure EndUpdateLayout;

    function GroupClass: TRzGroupClass; virtual;

    function CreateGroupFromTemplate( Template: TRzGroupTemplate ): TRzGroup;

    procedure AddGroup( Group: TRzGroup ); overload;
    function AddGroup( Template: TRzGroupTemplate ): TRzGroup; overload;
    procedure InsertGroup( Index: Integer; Group: TRzGroup ); overload;
    function InsertGroup( Index: Integer; Template: TRzGroupTemplate ): TRzGroup; overload;
    procedure RemoveGroup( Group: TRzGroup );

    procedure CloseAllGroups;
    procedure OpenAllGroups;

    property GroupCount: Integer
      read GetGroupCount;

    property Groups[ Index: Integer ]: TRzGroup
      read GetGroup;

    property ScrollPosition: Integer
      read FScrollPosition
      write SetScrollPosition;

    property DockManager;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ExclusiveMode: Boolean
      read FExclusiveMode
      write SetExclusiveMode
      default False;

    property BevelWidth: TBevelWidth
      read FBevelWidth
      write SetBevelWidth
      default 1;

    property BorderInner: TFrameStyleEx
      read FBorderInner
      write SetBorderInner
      default fsNone;

    property BorderOuter: TFrameStyleEx
      read FBorderOuter
      write SetBorderOuter
      default fsNone;

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

    property GradientPath: TRzGroupBarGradientPath
      read FGradientPath
      write SetGradientPath
      default gpTopToBottom;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property LargeImages: TCustomImageList
      read FLargeImages
      write SetLargeImages;

    property SmallImages: TCustomImageList
      read FSmallImages
      write SetSmallImages;

    property Style: TRzGroupBarStyle
      read FStyle
      write SetStyle
      default gbsCategoryView;

    property GroupBorderSize: Integer
      read FGroupBorderSize
      write SetGroupBorderSize
      nodefault;

    property UniqueItemSelection: Boolean
      read FUniqueItemSelection
      write SetUniqueItemSelection
      default False;

    property VisualStyle: TRzVisualStyle
      read FVisualStyle
      write SetVisualStyle
      default vsGradient;


    { Inherited Properties & Events }
    property Align default alLeft;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderWidth;
    property Caption;
    property Color nodefault;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
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
    property Width default 160;

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
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;


  {==========================================}
  {== TRzGroupController Class Declaration ==}
  {==========================================}

  TRzGroupProperty = ( gpAll, gpBorderColor, gpCaptionColor, gpCaptionColorStart,
                       gpCaptionColorStop, gpCaptionFont, gpCaptionHeight,
                       gpCaptionHotColor, gpColor, gpParentColor, gpDividerColor,
                       gpDividerVisible, gpFont, gpParentFont, gpItemHeight,
                       gpItemHotColor, gpItemHotZone, gpItemIndent, gpItemStaticFont,
                       gpSelectionColor, gpSelectionFontColor, gpSelectionShadowColor,
                       gpSelectionFrameColor, gpShowItemSelection,
                       gpItemSelectionStyle, gpIgnoreSelectedItemClick,
                       gpHideAccelerators );


  TRzGroupController = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FGroupList: TList;
    FUpdateCount: Integer;
    FRegIniFile: TRzRegIniFile;

    FBorderColor: TColor;
    FCaptionColor: TColor;
    FCaptionColorStart: TColor;
    FCaptionColorStop: TColor;
    FCaptionFont: TFont;
    FCaptionFontChanged: Boolean;
    FCaptionHeight: Integer;
    FCaptionHotColor: TColor;
    FColor: TColor;
    FDividerColor: TColor;
    FDividerVisible: Boolean;
    FDefaultFontName: string;
    FDefaultFontSize: Integer;
    FFont: TFont;
    FFontChanged: Boolean;
    FItemHeight: Integer;
    FItemHotColor: TColor;
    FItemHotZone: TRzGroupItemHotZone;
    FItemIndent: Byte;
    FItemStaticFont: TFont;
    FItemStaticFontChanged: Boolean;
    FParentColor: Boolean;
    FParentFont: Boolean;
    FSelectionColor: TColor;
    FSelectionFontColor: TColor;
    FSelectionShadowColor: TColor;
    FSelectionFrameColor: TColor;
    FShowItemSelection: Boolean;
    FItemSelectionStyle: TRzItemSelectionStyle;
    FIgnoreSelectedItemClick: Boolean;
    FHideAccelerators: Boolean;

    { Internal Event Handlers }
    procedure FontChangeHandler( Sender: TObject );
    procedure CaptionFontChangeHandler( Sender: TObject );
    procedure ItemStaticFontChangeHandler( Sender: TObject );

    { Message Handling Methods }
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateGroupProperty( G: TRzGroup; GroupProperty: TRzGroupProperty ); virtual;
    procedure UpdateAllGroups( GroupProperty: TRzGroupProperty ); virtual;
    procedure UpdateFonts;

    { Property Access Methods }
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetCaptionColor( Value: TColor ); virtual;
    procedure SetCaptionColorStart( Value: TColor ); virtual;
    procedure SetCaptionColorStop( Value: TColor ); virtual;
    procedure SetCaptionFont( Value: TFont ); virtual;
    procedure SetCaptionHeight( Value: Integer ); virtual;
    procedure SetCaptionHotColor( Value: TColor ); virtual;

    function IsFontStored: Boolean;
    procedure SetFont( Value: TFont ); virtual;
    procedure SetItemHotColor( Value: TColor ); virtual;
    procedure SetItemHotZone( Value: TRzGroupItemHotZone ); virtual;
    procedure SetItemStaticFont( Value: TFont ); virtual;

    function StoreColor: Boolean;
    procedure SetColor( Value: TColor ); virtual;
    procedure SetSelectionColor( Value: TColor ); virtual;
    procedure SetSelectionFontColor( Value: TColor ); virtual;
    procedure SetSelectionShadowColor( Value: TColor ); virtual;
    procedure SetSelectionFrameColor( Value: TColor ); virtual;
    procedure SetHideAccelerators( Value: Boolean ); virtual;
    procedure SetShowItemSelection( Value: Boolean ); virtual;
    procedure SetItemSelectionStyle( Value: TRzItemSelectionStyle ); virtual;
    procedure SetIgnoreSelectedItemClick( Value: Boolean ); virtual;

    procedure SetItemHeight( Value: Integer ); virtual;
    procedure SetItemIndent( Value: Byte ); virtual;
    procedure SetDividerVisible( Value: Boolean ); virtual;
    procedure SetDividerColor( Value: TColor ); virtual;

    procedure SetParentColor( Value: Boolean ); virtual;
    procedure SetParentFont( Value: Boolean ); virtual;

    procedure SetRegIniFile( Value: TRzRegIniFile ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateGroups;

    procedure SetDefaults( Style: TRzGroupBarStyle );

    procedure AddGroup( G: TRzGroup );
    procedure RemoveGroup( G: TRzGroup );

    procedure Load( const Section: string );
    procedure Save( const Section: string );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      default clBtnHighlight;

    property CaptionColor: TColor
      read FCaptionColor
      write SetCaptionColor
      default clHighlightText;

    property CaptionColorStart: TColor
      read FCaptionColorStart
      write SetCaptionColorStart
      default clWindow;

    property CaptionColorStop: TColor
      read FCaptionColorStop
      write SetCaptionColorStop
      default clBtnFace;

    property CaptionFont: TFont
      read FCaptionFont
      write SetCaptionFont
      stored FCaptionFontChanged;

    property CaptionHeight: Integer
      read FCaptionHeight
      write SetCaptionHeight
      default 20;

    property CaptionHotColor: TColor
      read FCaptionHotColor
      write SetCaptionHotColor
      default clHotLight;

    property Color: TColor
      read FColor
      write SetColor
      stored StoreColor
      default clBtnFace;

    property DividerColor: TColor
      read FDividerColor
      write SetDividerColor
      default clHighlight;

    property DividerVisible: Boolean
      read FDividerVisible
      write SetDividerVisible
      default False;

    property Font: TFont
      read FFont
      write SetFont
      stored IsFontStored;

    property ItemHeight: Integer
      read FItemHeight
      write SetItemHeight
      default 20;

    property ItemHotColor: TColor
      read FItemHotColor
      write SetItemHotColor
      default clHotLight;

    property ItemHotZone: TRzGroupItemHotZone
      read FItemHotZone
      write SetItemHotZone
      default ihzImageCaption;

    property ItemIndent: Byte
      read FItemIndent
      write SetItemIndent
      default 20;

    property ItemStaticFont: TFont
      read FItemStaticFont
      write SetItemStaticFont
      stored FItemStaticFontChanged;

    property ParentColor: Boolean
      read FParentColor
      write SetParentColor
      default False;

    property ParentFont: Boolean
      read FParentFont
      write SetParentFont
      default True;

    property SelectionColor: TColor
      read FSelectionColor
      write SetSelectionColor
      default clBtnFace;

    property SelectionFontColor: TColor
      read FSelectionFontColor
      write SetSelectionFontColor
      default clWindowText;
      
    property SelectionShadowColor: TColor
      read FSelectionShadowColor
      write SetSelectionShadowColor
      default clBtnShadow;
      
    property SelectionFrameColor: TColor
      read FSelectionFrameColor
      write SetSelectionFrameColor
      default cl3DDkShadow;

    property HideAccelerators: Boolean
      read FHideAccelerators
      write SetHideAccelerators
      default True;
      
    property ShowItemSelection: Boolean
      read FShowItemSelection
      write SetShowItemSelection
      default False;

    property ItemSelectionStyle: TRzItemSelectionStyle
      read FItemSelectionStyle
      write SetItemSelectionStyle
      default issImageAndCaption;

    property IgnoreSelectedItemClick: Boolean
      read FIgnoreSelectedItemClick
      write SetIgnoreSelectedItemClick
      default True;
      
    property RegIniFile: TRzRegIniFile
      read FRegIniFile
      write SetRegIniFile;
  end;




{$IFDEF USE_GROUP_DESIGNER}
  IRzGroupDesigner = interface
  ['{01BF8F45-157B-4EF5-B7F3-84BAEB8CC87E}']
    procedure SetSelection( APersistent: TPersistent );
  end;

var
  GroupDesigner: IRzGroupDesigner = nil;
{$ENDIF}

resourcestring
  sRzCannotLoadGroupController = 'Cannot load Group Controller settings--No TRzRegIniFile component specified';
  sRzCannotSaveGroupController = 'Cannot save Group Controller settings--No TRzRegIniFile component specified';

implementation

uses
  {&RAS}
  Themes,
  UxTheme,
  RzGrafx,
  RzCommonCursors;

const
  fsBoldMask      = 8;                { Constants Used to Determine Font Style }
  fsItalicMask    = 4;
  fsUnderlineMask = 2;
  fsStrikeOutMask = 1;
  fsNormal        = 0;

  isLargeSeparatorHeight = 4;

var
  Registered: Boolean = False;


resourcestring
  sRzGroupIndexError = '%d is an invalid GroupIndex value. GroupIndex must be between 0 and %d';
  sRzGroupParentError = 'TRzGroup components can only be placed in a TRzGroupBar component';


{&RT}
{====================================}
{== TRzGroupItemActionLink Methods ==}
{====================================}

procedure TRzGroupItemActionLink.AssignClient( AClient: TObject );
begin
  FClient := AClient as TRzGroupItem;
end;


function TRzGroupItemActionLink.DoShowHint( var HintStr: string ): Boolean;
begin
  Result := True;
  if Action is TCustomAction then
  begin
    if TCustomAction( Action ).DoHint( HintStr ) and
       Application.HintShortCuts and
       (TCustomAction( Action ).ShortCut <> scNone) then
    begin
      if HintStr <> '' then
        HintStr := Format( '%s (%s)', [ HintStr, ShortCutToText( TCustomAction( Action ).ShortCut ) ] );
    end;
  end;
end;


function TRzGroupItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and ( FClient.Caption = ( Action as TCustomAction ).Caption );
end;


function TRzGroupItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and FClient.Group.ShowItemSelection and
            ( FClient.Selected = ( Action as TCustomAction ).Checked );
end;


function TRzGroupItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and ( FClient.Enabled = ( Action as TCustomAction ).Enabled );
end;


function TRzGroupItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and ( FClient.Hint = ( Action as TCustomAction ).Hint );
end;


function TRzGroupItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and ( FClient.ImageIndex = ( Action as TCustomAction ).ImageIndex );
end;


function TRzGroupItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and ( FClient.Visible = ( Action as TCustomAction ).Visible );
end;


function TRzGroupItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and ( @FClient.OnClick = @Action.OnExecute );
end;


procedure TRzGroupItemActionLink.SetCaption( const Value: string );
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;


procedure TRzGroupItemActionLink.SetChecked( Value: Boolean );
begin
  if IsCheckedLinked then
    FClient.Selected := Value;
end;


procedure TRzGroupItemActionLink.SetEnabled( Value: Boolean );
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;


procedure TRzGroupItemActionLink.SetHint( const Value: string );
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;


procedure TRzGroupItemActionLink.SetImageIndex( Value: Integer );
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;


procedure TRzGroupItemActionLink.SetVisible( Value: Boolean );
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;


procedure TRzGroupItemActionLink.SetOnExecute( Value: TNotifyEvent );
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;



{==========================}
{== TRzGroupItem Methods ==}
{==========================}

constructor TRzGroupItem.Create( Collection: TCollection );
begin
  inherited;

  if ( csDesigning in Group.ComponentState ) and not ( csLoading in Group.ComponentState ) then
    FCaption := 'Item' + IntToStr( Index + 1 );

  FEnabled := True;
  FFontColor := clNone;
  FFontStyle := [];
  FImageIndex := -1;
  FDisabledIndex := -1;
  // Call access method and not set FVisible b/c we want the Update method to be
  // called on the Collection after adding a new item.  Otherwise, the group
  // does not show the new item correctly.
  SetVisible( True );
end;


destructor TRzGroupItem.Destroy;
begin
  if Group <> nil then
    Group.ItemDestroyed( Index );
  FActionLink.Free;
  FActionLink := nil;
  inherited;
end;


procedure TRzGroupItem.Assign( Source: TPersistent );
begin
  if Source is TRzGroupItem then
  begin
    Action := TRzGroupItem( Source ).Action;
    Caption := TRzGroupItem( Source ).Caption;
    DisabledIndex := TRzGroupItem( Source ).DisabledIndex;
    Enabled := TRzGroupItem( Source ).Enabled;
    FontColor := TRzGroupItem( Source ).FontColor;
    FontStyle := TRzGroupItem( Source ).FontStyle;
    Hint := TRzGroupItem( Source ).Hint;
    ImageIndex := TRzGroupItem( Source ).ImageIndex;
    Visible := TRzGroupItem( Source ).Visible;
    OnClick := TRzGroupItem( Source ).OnClick;
  end
  else
    inherited;
end;


procedure TRzGroupItem.AssignTo( Dest: TPersistent );
begin
  if Dest is TCustomAction then
  begin
    TCustomAction( Dest ).Caption := Self.Caption;
    TCustomAction( Dest ).Enabled := Self.Enabled;
    TCustomAction( Dest ).Hint := Self.Hint;
    TCustomAction( Dest ).ImageIndex := Self.ImageIndex;
    TCustomAction( Dest ).Visible := Self.Visible;
    TCustomAction( Dest ).OnExecute := Self.OnClick;
  end
  else
    inherited;
end;



function TRzGroupItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;


procedure TRzGroupItem.Click;
begin
  // Call OnClick if assigned and not equal to associated action's OnExecute.
  // If associated action's OnExecute assigned then call it, otherwise, call OnClick.
  if Assigned( FOnClick ) and ( Action <> nil ) and ( @FOnClick <> @Action.OnExecute ) then
    FOnClick( Self )
  else if not ( csDesigning in Group.ComponentState ) and ( ActionLink <> nil ) then
  begin
    ActionLink.Execute( Group );
  end
  else if Assigned( FOnClick ) then
    FOnClick( Self );
end;


function TRzGroupItem.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action
  else
    Result := nil;
end;


procedure TRzGroupItem.SetAction( Value: TBasicAction );
begin
  if Value = nil then
  begin
    ActionLink.Free;
    ActionLink := nil;
    Group.ActionClientDisconnect;
  end
  else
  begin
    Group.ActionClientConnect;
    if ActionLink = nil then
      ActionLink := GetActionLinkClass.Create( Self );
    ActionLink.Action := Value;
    ActionLink.OnChange := ActionChangeHandler;
    ActionChange( Value, csLoading in Value.ComponentState );

    Value.FreeNotification( Group );
  end;
end;


procedure TRzGroupItem.ActionChange( Sender: TObject; CheckDefaults: Boolean );
var
  NewAction: TCustomAction;
begin
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction( Sender );

    if not CheckDefaults or ( Self.Caption = '' ) or not FCaptionChanged then
      Self.Caption := NewAction.Caption;

    if not CheckDefaults or ( Self.Enabled = True ) then
      Self.Enabled := NewAction.Enabled;

    if not CheckDefaults or ( Self.Hint = '' ) then
      Self.Hint := NewAction.Hint;

    if not CheckDefaults or ( Self.ImageIndex = -1 ) then
      Self.ImageIndex := NewAction.ImageIndex;

    if not CheckDefaults or ( Self.Visible = True ) then
      Self.Visible := NewAction.Visible;

    if not CheckDefaults or not Assigned( Self.OnClick ) then
      Self.OnClick := NewAction.OnExecute;
  end;
end;


procedure TRzGroupItem.ActionChangeHandler( Sender: TObject );
begin
  if Sender = Action then
    ActionChange( Sender, False );
end;


function TRzGroupItem.GetActionLinkClass: TRzGroupItemActionLinkClass;
begin
  Result := TRzGroupItemActionLink;
end;


procedure TRzGroupItem.InitiateAction;
begin
  if ActionLink <> nil then
    ActionLink.Update;
end;


function TRzGroupItem.IsCaptionStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsCaptionLinked;
end;


function TRzGroupItem.IsEnabledStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsEnabledLinked;
end;


function TRzGroupItem.IsHintStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsHintLinked;
end;


function TRzGroupItem.IsHelpContextStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsHelpContextLinked;
end;


function TRzGroupItem.IsImageIndexStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsImageIndexLinked;
end;


function TRzGroupItem.IsVisibleStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsVisibleLinked;
end;


function TRzGroupItem.IsOnClickStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsOnExecuteLinked;
end;


procedure TRzGroupItem.SetCaption( const Value: TCaption );
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FCaptionChanged := True;
    Changed( True ); // Reposition and Repaint in TRzGroupItems.Update
  end;
end;


procedure TRzGroupItem.SetEnabled( Value: Boolean );
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed( False ); // Repaint Only
  end;
end;


procedure TRzGroupItem.SetFontColor( Value: TColor );
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed( False ); // Repaint Only
  end;
end;


procedure TRzGroupItem.SetFontStyle( Value: TFontStyles );
begin
  if FFontStyle <> Value then
  begin
    FFontStyle := Value;
    Changed( True );  // Reposition and Repaint
  end;
end;


function TRzGroupItem.GetGroup: TRzGroup;
begin
  Result := TRzGroupItems( Collection ).Group;
end;


procedure TRzGroupItem.SetDisabledIndex( Value: TImageIndex );
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    Changed( True );  // Reposition and Repaint
  end;
end;


procedure TRzGroupItem.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed( True );  // Reposition and Repaint
  end;
end;


procedure TRzGroupItem.SetIndentLevel( Value: Byte );
begin
  if FIndentLevel <> Value then
  begin
    FIndentLevel := Value;
    Changed( True );  // Reposition and Repaint
  end;
end;


procedure TRzGroupItem.SetVisible( Value: Boolean );
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed( True );  // Reposition and Repaint
  end;
end;


{$IFDEF USE_GROUP_DESIGNER}

function TRzGroupItem.GetSelected: Boolean;
begin
  Result := FSelected;

  if ( Group <> nil ) and ( csDesigning in Group.ComponentState ) and not FSelected and not FSkipDesignerSelection then
  begin
    // Since no other code uses the SelectedInDesigner property, if this property access method is being invoked, the
    // Object Inspector is querying the property at Design-Time, so set the item as Selected.
    // Set SkipDesignerSelection to True to avoid stack-overflow
    FSkipDesignerSelection := True;
    try
      SetSelected( True );
    finally
      FSkipDesignerSelection := False;
    end;
  end;
end;


procedure TRzGroupItem.SetSelected( Value: Boolean );
begin
  FSelected := Value;
  if Value then
    UpdateSelection;
  Changed( False );
end;


procedure TRzGroupItem.UpdateSelection;
var
  Msg: TMessage;
begin
  // Items can be selected but they don't have to be enabled
  if ( Group <> nil ) and ( Group.GroupBar <> nil ) and ( Group.GroupBar.Parent <> nil ) then
  begin
    Msg.Msg := cm_GroupItemSelected;
    Msg.WParam := 0;
    Msg.LParam := LParam( Self );
    Msg.Result := 0;

    Group.GroupBar.Parent.Broadcast( Msg );
  end;
end;


procedure TRzGroupItem.CMGroupItemSelected( var Msg: TCMGroupItemMsg );
begin
  if ( Msg.Sender <> Self ) and Msg.Sender.FSelected and Self.FSelected then
    SetSelected( False );

  if ( Group <> nil ) and ( csDesigning in Group.ComponentState ) and FSelected and Assigned( GroupDesigner ) and
     not FSkipDesignerSelection then
  begin
    GroupDesigner.SetSelection( Self );
  end;
end;

{$ENDIF}


procedure TRzGroupItem.SetSelected( Value: Boolean );
begin
  FSelected := Value;
  if Value then
    UpdateSelection
  else
    Group.FItemIndex := -1;  // If removing selection, then update group's ItemIndex
  Changed( False ); // Repaint Only
end;


procedure TRzGroupItem.UpdateSelection;
var
  Msg: TCMGroupItemMsg;
begin
  // Items can be selected but they don't have to be enabled
  if ( Group <> nil ) and ( Group.GroupBar <> nil ) and ( Group.GroupBar.Parent <> nil ) then
  begin
    Msg.Msg := cm_GroupItemSelected;
    Msg.Unused := 0;
    Msg.Sender := Self;
    Msg.Result := 0;

    Group.CMGroupItemSelected( TCMGroupItemMsg( Msg ) );

    Group.FItemIndex := Index;

    if Group.GroupBar.UniqueItemSelection then
      Group.GroupBar.Parent.Broadcast( Msg );
  end;
end;


procedure TRzGroupItem.CMGroupItemSelected( var Msg: TCMGroupItemMsg );
begin
  if ( Msg.Sender <> Self ) and Msg.Sender.FSelected and Self.FSelected then
    SetSelected( False );
end;



{===========================}
{== TRzGroupItems Methods ==}
{===========================}

constructor TRzGroupItems.Create( Group: TRzGroup );
begin
  // Inherited constructor is passed the "type" of the collection
  // item that the collection will manage.
  inherited Create( TRzGroupItem );
  FGroup := Group;
end;


function TRzGroupItems.Add: TRzGroupItem;
begin
  Result := TRzGroupItem( inherited Add );
end;


function TRzGroupItems.GetItem( Index: Integer ): TRzGroupItem;
begin
  Result := TRzGroupItem( inherited GetItem( Index ) );
end;


procedure TRzGroupItems.SetItem( Index: Integer; Value: TRzGroupItem );
begin
  // Must specify SetItem b/c SetItem is not virtual
  inherited SetItem( Index, Value );
end;


function TRzGroupItems.GetOwner: TPersistent;
begin
  Result := FGroup;
end;


procedure TRzGroupItems.Update( Item: TCollectionItem );
begin
  // If Item is nil, assume all items have changed
  // Otherwise, Item represents the item that has changed

  // Caption and other properties may have changed -- request that Group reposition the items
  if Item = nil then
    FGroup.Reposition;
  FGroup.Repaint;
end;



{======================}
{== TRzGroup Methods ==}
{======================}

constructor TRzGroup.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle + [ csSetCaption, csAcceptsControls ];

  DoubleBuffered := True;

  FGroupController := nil;

  FStyle := gbsCategoryView;
  FItemStyle := isSmall;
  FCaptionStyle := csSmall;
  FItemIndent := 20;
  FItemHeight := 20;
  FCanClose := True;
  FSpecial := False;

  FBorderColor := clBtnHighlight;
  FCaptionColor := DefaultCaptionColor( FStyle );
  FCaptionColorDefault := True;
  FCaptionColorStart := clWindow;
  FCaptionColorStop := clBtnFace;
  FCaptionHotColor := clHotLight;
  FItemHotColor := clHotLight;
  FItemHotZone := ihzImageCaption;

  FSelectionColor := clBtnFace;
  FSelectionFontColor := clWindowText;
  FSelectionShadowColor := clBtnShadow;
  FSelectionFrameColor := cl3DDkShadow;
  FShowItemSelection := False;
  FItemSelectionStyle := issImageAndCaption;
  FIgnoreSelectedItemClick := True;
  FHideAccelerators := True;

  Font.Color := clWindowText;  // Used for Items
  FFontChanged := False;
  {&RCI}

  FCaptionFont := TFont.Create;
  FCaptionFont.Assign( Self.Font );
  FCaptionFont.Style := [ fsBold ];
  FCaptionFont.Color := clHighlight;
  FCaptionFontChanged := False;
  FCaptionFont.OnChange := CaptionFontChangeHandler;

  FItemStaticFont := TFont.Create;
  FItemStaticFont.Assign( Self.Font );
  FItemStaticFontChanged := False;
  FItemStaticFont.OnChange := ItemStaticFontChangeHandler;

  FCaptionHeight := 20;
  FCaptionImageIndex := -1;
  FCaptionState := csNormal;
  FClickingCaption := False;

  if RunningUnder( WinNT ) or RunningUnder( Win95 ) then
    FCaptionCursor := LoadCursor( HInstance, 'RZCOMMON_HANDCURSOR' )
  else
    FCaptionCursor := LoadCursor( 0, IDC_HAND );

  ParentColor := False;
  FColorDefault := True;

  FDividerVisible := False;
  FDividerColor := clHighlight;

  Height := 50;
  FOpenedHeight := 50;
  SetOpened( True );

  FSmallImagesChangeLink := TChangeLink.Create;
  FSmallImagesChangeLink.OnChange := ImagesChange;
  FLargeImagesChangeLink := TChangeLink.Create;
  FLargeImagesChangeLink.OnChange := ImagesChange;

  FActionClientCount := 0;

  CreateScrollButtons;

  FItems := TRzGroupItems.Create( Self );
  FItemIndex := -1;
end; {= TRzGroup.Create =}


procedure TRzGroup.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Handle the fact that the UseGradients property was published in verison 3.x
  Filer.DefineProperty( 'UseGradients',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzGroup.CreateScrollButtons;
begin
  FScrollUpBtn := TRzRapidFireButton.Create( Self );
  FScrollUpBtn.Parent := Self;
  FScrollUpBtn.ControlStyle := FScrollUpBtn.ControlStyle + [ csNoDesignVisible ];
  FScrollUpBtn.ScrollStyle := scsUp;
  FScrollUpBtn.Visible := False;
  FScrollUpBtn.OnClick := ScrollUpHandler;

  FScrollDownBtn := TRzRapidFireButton.Create( Self );
  FScrollDownBtn.Parent := Self;
  FScrollDownBtn.ControlStyle := FScrollDownBtn.ControlStyle + [ csNoDesignVisible ];
  FScrollDownBtn.ScrollStyle := scsDown;
  FScrollDownBtn.Visible := False;
  FScrollDownBtn.OnClick := ScrollDownHandler;
end;


destructor TRzGroup.Destroy;
begin
  if FGroupController <> nil then
    FGroupController.RemoveGroup( Self );

  if FGroupBar <> nil then
    FGroupBar.RemoveGroup( Self );

  FCaptionFont.Free;
  FItemStaticFont.Free;
  FItems.Free;
  FSmallImagesChangeLink.Free;
  FLargeImagesChangeLink.Free;

  if RunningUnder( WinNT ) or RunningUnder( Win95 ) then
    DestroyCursor( FCaptionCursor );

  inherited;
end;


procedure TRzGroup.Loaded;
var
  I: Integer;
begin
  inherited;

  for I := 0  to FItems.Count - 1 do
  begin
    if FItems[ I ].Action <> nil then
      FItems[ I ].ActionChange( FItems[ I ].Action, True );
  end;

  UpdateScrollBtnPositions;
end;


procedure TRzGroup.ReadState( Reader: TReader );
begin
  inherited;
  if Reader.Parent is TRzGroupBar then
    GroupBar := TRzGroupBar( Reader.Parent );
end;


procedure TRzGroup.Notification( AComponent: TComponent; Operation: TOperation );
var
  I: Integer;
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FSmallImages then
      SetSmallImages( nil )
    else if AComponent = FLargeImages then
      SetLargeImages( nil )
    else if AComponent = FGroupController then
      FGroupController := nil
    else if AComponent = FItemPopupMenu then
      FItemPopupMenu := nil
    else if AComponent is TBasicAction then
    begin
      for I := 0 to FItems.Count - 1 do
      begin
        if AComponent = FItems[ I ].Action then
          FItems[ I ].Action := nil;
      end;
    end;
  end;
end;


procedure TRzGroup.InitiateAction;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FItems.Count - 1 do
    FItems[ I ].InitiateAction;
end;


procedure TRzGroup.ActionClientConnect;
begin
  Inc( FActionClientCount );
  ControlStyle := ControlStyle + [ csActionClient ];
end;


procedure TRzGroup.ActionClientDisconnect;
begin
  Dec( FActionClientCount );
  if FActionClientCount = 0 then
    ControlStyle := ControlStyle - [ csActionClient ];
end;


procedure TRzGroup.AssignActionList( ActionList: TCustomActionList; const Category: string = '' );
var
  I: Integer;
  Item: TRzGroupItem;
begin
  if ActionList <> nil then
  begin
    FItems.Clear;

    for I := 0 to ActionList.ActionCount - 1 do
    begin
      if ( Category = '' ) or ( UpperCase( ActionList.Actions[ I ].Category ) = UpperCase( Category ) ) then
      begin
        Item := FItems.Add;
        Item.Action := ActionList.Actions[ I ];
      end;
    end;
  end;
end;


procedure TRzGroup.SetParent( AParent: TWinControl );
begin
  if ( AParent <> nil ) and not ( AParent is TRzGroupBar ) then
    raise ERzGroupBarError.Create( sRzGroupParentError );

  inherited;
end;


function TRzGroup.SmallImageList: TCustomImageList;
begin
  if ( FSmallImages = nil ) and ( GroupBar <> nil ) and ( GroupBar.SmallImages <> nil ) then
    Result := GroupBar.SmallImages
  else
    Result := FSmallImages;
end;


function TRzGroup.LargeImageList: TCustomImageList;
begin
  if ( FLargeImages = nil ) and ( GroupBar <> nil ) and ( GroupBar.LargeImages <> nil ) then
    Result := GroupBar.LargeImages
  else
    Result := FLargeImages;
end;


function TRzGroup.CalculateHeight( W: Integer ): Integer;
var
  I, H, BaseXOffset, TextOffset, XOffset, YTop, YBottom, RowHeight: Integer;
  R: TRect;
begin
  Result := FCaptionHeight;

  YTop := FCaptionHeight + 4;
  YBottom := YTop;
  Canvas.Brush.Color := Self.Color;
  Canvas.Font := Self.Font;
  TextOffset := ( FItemHeight - Canvas.TextHeight( 'Pp' ) ) div 2;

  if FItems.Count > 0 then
  begin
    if FItemStyle = isSmall then
    begin

      // Determine if all items need to be indented by small image size
      BaseXOffset := CalcBaseXOffset;

      for I := FTopItem to FItems.Count - 1 do
      begin
        if FItems[ I ].Visible then
        begin
          Canvas.Font := Self.Font;

          if ( FItems[ I ].CaptionState = csNormal ) and
             not ( Assigned( FItems[ I ].OnClick ) or ( FItems[ I ].Action <> nil ) ) then
          begin
            // Inactive Item
            Canvas.Font := FItemStaticFont;
          end;

          Canvas.Font.Style := Canvas.Font.Style + FItems[ I ].FontStyle;

          XOffset := BaseXOffset;
          if FItems[ I ].IndentLevel > 0 then
            XOffset := BaseXOffset + FItems[ I ].IndentLevel * FItemIndent;

          R := Rect( XOffset, YTop, W - 8, YTop + FItemHeight );
          if R.Right - R.Left <= 0 then
            R.Right := R.Left + 1;
          H := DrawString( Canvas, FItems[ I ].Caption, R, dt_CalcRect or dt_WordBreak or dt_ExpandTabs );

          if ( FItems[ I ].Caption = '-' ) and SkipSeparator( I ) then
            Continue;

          YBottom := YTop + 2 * TextOffset + H + 4;
          Inc( YTop, Max( H + 2 * TextOffset, FItemHeight ) );
        end;
      end; { for I }
      Result := YBottom;
    end
    else // if FItemStyle = isLarge
    begin
      if LargeImageList <> nil then
        RowHeight := LargeImageList.Height + 2
      else
        RowHeight := 34;
      for I := FTopItem to FItems.Count - 1 do
      begin
        if FItems[ I ].Visible then
        begin
          if FItems[ I ].Caption <> '-' then
          begin
            Canvas.Font := Self.Font;
            Canvas.Font.Style := Canvas.Font.Style + FItems[ I ].FontStyle;
            XOffset := 8;

            R := Rect( XOffset, YTop + RowHeight, W - 8, YTop + RowHeight + FItemHeight );
            if R.Right - R.Left <= 0 then
              R.Right := R.Left + 1;
            H := DrawString( Canvas, FItems[ I ].Caption, R,
                             dt_CalcRect or dt_Center or dt_WordBreak or dt_ExpandTabs );

            R := Rect( XOffset, YTop + RowHeight + TextOffset, W - 8, YTop + RowHeight + TextOffset + H );
            if R.Right - R.Left <= 0 then
              R.Right := R.Left + 1;
            YBottom := YTop + 2 * TextOffset + H + RowHeight + 4;
            Inc( YTop, RowHeight + Max( H + 2 * TextOffset, FItemHeight ) + 8 );
          end
          else // FItems[ I ].Caption = '-' ... Adjust height for a separator
          begin
            if not SkipSeparator( I ) then
            begin
              YBottom := YTop + isLargeSeparatorHeight;
              Inc( YTop, isLargeSeparatorHeight + 8 );
            end;
          end;
        end;
      end; { for I }
      Result := YBottom;
    end;

  end;

end; {= TRzGroup.CalculateHeight =}


procedure TRzGroup.Reposition;
begin
  UpdateGroupBarLayout;
  if FStyle = gbsOutlook then
    UpdateScrollBtnVisibility;
end;


{$IFDEF USE_GROUP_DESIGNER}

procedure TRzGroup.DrawDesignFocus( var Bounds: TRect );
var
  OldColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldPenWidth: Integer;
begin
  OldBrushStyle := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  OldPenWidth := Canvas.Pen.Width;
  OldColor := Canvas.Pen.Color;
  Canvas.Pen.Color := BlendColors( clHighlight, Color, 128 );
  Canvas.Pen.Width := 2;
  Canvas.Rectangle( Bounds );
  Canvas.Brush.Style := OldBrushStyle;
  Canvas.Pen.Width := OldPenWidth;
  Canvas.Pen.Color := OldColor;
end;

{$ENDIF}


function TRzGroup.CalcBaseXOffset: Integer;
var
  SomeItemHasImage: Boolean;
  I: Integer;
begin
  Result := 8;
  if SmallImageList <> nil then
  begin
    SomeItemHasImage := False;
    for I := 0 to FItems.Count - 1 do
    begin
      if FItems[ I ].ImageIndex <> -1 then
      begin
        SomeItemHasImage := True;
        Break;
      end;
    end;

    if SomeItemHasImage then
      Result := Result + SmallImageList.Width + 4;
  end;
end;


function TRzGroup.SkipSeparator( Index: Integer ): Boolean;
var
  K: Integer;
  Skip: Boolean;
begin
  Skip := False;

  // If separator is top item or last item, then skip...
  if ( Index = FTopItem ) or ( Index = FItems.Count - 1 ) then
  begin
    Result := True;
    Exit;
  end;

  if Index > FTopItem then
  begin
    if FItems[ Index - 1 ].Visible and ( FItems[ Index - 1 ].Caption = '-' ) then
      Skip := True
    else
    begin
      for K := Index - 1 downto FTopItem do
      begin
        if FItems[ K ].Visible and ( FItems[ K ].Caption <> '-' ) then
          Break  // out of K loop
        else if FItems[ K ].Visible and ( FItems[ K ].Caption = '-' ) then
        begin
          Skip := True;
          Break;
        end
        else if not FItems[ K ].Visible and ( K = FTopItem ) then
        begin
          Skip := True;
          Break;
        end;
      end;
    end;

    if not Skip and ( Index < FItems.Count - 1 ) then
    begin
      for K := Index + 1 to FItems.Count - 1 do
      begin
        if FItems[ K ].Visible and ( FItems[ K ].Caption <> '-' ) then
          Break
        else if ( K = FItems.Count - 1 ) and ( not FItems[ K ].Visible or ( FItems[ K ].Caption = '-' ) ) then
        begin
          Skip := True;
          Break;
        end;
      end;
    end;
  end;
  Result := Skip;
end;


procedure TRzGroup.DrawItems;
var
  R: TRect;
  I, H, BaseXOffset, XOffset, YOffset, YTop, TextOffset, Temp, RowHeight: Integer;
  ImgXOffset, SelHeight: Integer;
  C, DivColor, ItemFontColor, ItemFontHotColor, ItemFontDownColor: TColor;
  SelectionBorderColor, SelectionStartColor, SelectionStopColor, SelectionFontColor: TColor;
  S: string;

  procedure UpdateFontColorAndStyle( Index: Integer );
  begin
    if FItems[ Index ].Enabled then
    begin
      case FItems[ Index ].CaptionState of
        csNormal:
        begin
          if Assigned( FItems[ Index ].OnClick ) or ( FItems[ Index ].Action <> nil ) then
          begin
            // Linked Item
            Canvas.Font.Color := ItemFontColor;
          end
          else
          begin
            // Static Item
            Canvas.Font := FItemStaticFont;
            if not UsingSystemStyle then
              Canvas.Font.Color := ActiveStyleFontColor( sfCatgeoryButtonsNormal );
          end;

          if FItems[ Index ].FontColor <> clNone then
            Canvas.Font.Color := FItems[ Index ].FontColor;
        end;

        csHot:
        begin
          Canvas.Font.Color := ItemFontHotColor;
          if FItemStyle = isSmall then
            Canvas.Font.Style := Canvas.Font.Style + [ fsUnderline ];
        end;

        csDown:
        begin
          Canvas.Font.Color := ItemFontDownColor;
          if FItemStyle = isSmall then
            Canvas.Font.Style := Canvas.Font.Style + [ fsUnderline ];
        end;
      end;

      Canvas.Font.Style := Canvas.Font.Style + FItems[ Index ].FontStyle;
    end
    else // item is disabled
    begin
      if UsingSystemStyle then
      begin
        if ColorToRGB( Color ) = ColorToRGB( clBtnShadow ) then
          Canvas.Font.Color := clBtnFace
        else
          Canvas.Font.Color := clBtnShadow;
      end
      else // VCL Styles
      begin
        Canvas.Font.Color := ActiveStyleFontColor( sfCaptionTextInactive );
      end;
      Canvas.Font.Style := [];
    end;
  end; {= UpdateFontColorAndStyle =}


begin {= TRzGroup.DrawItems =}

  Canvas.Brush.Style := bsClear;
  Canvas.Font := Self.Font;

  // Determine appropriate Font colors

  if FStyle = gbsCategoryView then
  begin
    if UsingSystemStyle then
    begin
      if ( GroupBar.VisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
      begin
        if FSpecial then
          ItemFontColor := GetXPThemeColor( xptcSpecialGroupFont )
        else
          ItemFontColor := GetXPThemeColor( xptcNormalGroupFont );
        ItemFontHotColor := BlendColors( clWhite, ItemFontColor, 50 );
      end
      else if GroupBar.GradientColorStyle <> gcsCustom then
      begin
        GetGradientGroupItemColors( GroupBar.GradientColorStyle, ItemFontColor,
                                    ItemFontHotColor );
      end
      else // Custom
      begin
        ItemFontColor := Self.Font.Color;
        ItemFontHotColor := FItemHotColor;
      end;
    end
    else // VCL Styles
    begin
      ItemFontColor := ActiveStyleFontColor( sfCatgeoryButtonsNormal );
      ItemFontHotColor := ActiveStyleSystemColor( clHighlight );
    end;
  end
  else // Outlook or TaskList
  begin
    if UsingSystemStyle then
    begin
      ItemFontColor := Self.Font.Color;
      ItemFontHotColor := FItemHotColor;
    end
    else
    begin
      ItemFontColor := ActiveStyleFontColor( sfCatgeoryButtonsNormal );
      ItemFontHotColor := ActiveStyleSystemColor( clHighlight );
    end;
  end;
  ItemFontDownColor := BlendColors( clBlack, ItemFontColor, 50 );


  if GroupBar.GradientColorStyle <> gcsCustom then
  begin
    GetGradientSelectionColors( GroupBar.GradientColorStyle, SelectionBorderColor,
                                SelectionStartColor, SelectionStopColor );
    SelectionFontColor := ItemFontColor;
  end
  else
  begin
    SelectionBorderColor := FSelectionFrameColor;
    SelectionStartColor := FSelectionColor;
    SelectionStopColor := FSelectionShadowColor;
    SelectionFontColor := FSelectionFontColor;
  end;


  if FItemStyle = isSmall then
  begin
    YTop := FCaptionHeight + 4;
    TextOffset := ( FItemHeight - Canvas.TextHeight( 'Pp' ) ) div 2;

    // Determine if all items need to be indented by small image size
    BaseXOffset := CalcBaseXOffset;

    // Draw each item in view

    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Visible then
      begin
        Canvas.Font := Self.Font;
        UpdateFontColorAndStyle( I );
        Canvas.Brush.Style := bsClear;

        XOffset := BaseXOffset;
        if FItems[ I ].IndentLevel > 0 then
          XOffset := BaseXOffset + FItems[ I ].IndentLevel * FItemIndent;

        R := Rect( XOffset, YTop, Width - 8, YTop + FItemHeight );
        H := DrawString( Canvas, FItems[ I ].Caption, R,
                       dt_CalcRect or dt_WordBreak or dt_ExpandTabs );

        if FItems[ I ].Caption <> '' then
        begin
          if FItems[ I ].Caption <> '-' then
          begin
            if FHideAccelerators then
              S := RemoveAccelerators( FItems[ I ].Caption )
            else
              S := FItems[ I ].Caption;

            // Draw Image-and-Caption Selection if necessary

            if ( FItemSelectionStyle = issImageAndCaption ) and FShowItemSelection and
               FItems[ I ].FSelected then
            begin
              if ( SmallImageList <> nil ) and ( FItems[ I ].ImageIndex <> -1 ) then
              begin
                ImgXOffset := SmallImageList.Width + 4;
                SelHeight := Max( H, SmallImageList.Height );
              end
              else
              begin
                ImgXOffset := 0;
                SelHeight := H;
              end;


              if ( ( Canvas.TextWidth( FItems[ I ].Caption ) < ( R.Right - R.Left ) ) or
                   ( H < FItemHeight ) ) and
                 ( SmallImageList <> nil ) then
              begin
                YOffset := YTop + ( FItemHeight - SmallImageList.Height ) div 2;
              end
              else
                YOffset := YTop + TextOffset;


              if not UseRightToLeftAlignment then
                R := Rect( XOffset - ImgXOffset, YOffset, Width - 8, YOffset + SelHeight )
              else
                R := Rect( 8, YOffset, Width - XOffset + ImgXOffset, YOffset + SelHeight );

              InflateRect( R, 2, 2 );
              R := DrawBox( Canvas, R, SelectionBorderColor );

              if ( FItems[ I ].CaptionState = csHot ) or ( FItems[ I ].FSelected and
                 ( FItems[ I ].CaptionState <> csDown ) ) then
                PaintGradient( Canvas, R, gdHorizontalEnd, SelectionStartColor, SelectionStopColor )
              else
                PaintGradient( Canvas, R, gdHorizontalEnd, SelectionStopColor, SelectionStartColor );

              InflateRect( R, -1, -1 );  // Revert back to original size  (DrawBox shrinks by 1)

              Canvas.Font.Color := SelectionFontColor;
            end;



            if not UseRightToLeftAlignment then
              R := Rect( XOffset, YTop + TextOffset, Width - 8, YTop + TextOffset + H )
            else
              R := Rect( 8, YTop + TextOffset, Width - XOffset, YTop + TextOffset + H );

            // Show Selected State if no image associated with item
            if ( FItemSelectionStyle <> issImageAndCaption ) and
               ( ( FItemSelectionStyle = issCaption ) or
                 ( ( SmallImageList = nil ) or ( FItems[ I ].ImageIndex = -1 ) ) ) and
               FShowItemSelection and FItems[ I ].FSelected then
            begin
              InflateRect( R, 2, 2 );
              R := DrawBox( Canvas, R, SelectionBorderColor );

              if ( FItems[ I ].CaptionState = csHot ) or ( FItems[ I ].FSelected and
                 ( FItems[ I ].CaptionState <> csDown ) ) then
                PaintGradient( Canvas, R, gdHorizontalEnd, SelectionStartColor, SelectionStopColor )
              else
                PaintGradient( Canvas, R, gdHorizontalEnd, SelectionStopColor, SelectionStartColor );

              InflateRect( R, -1, -1 );  // Revert back to original size  (DrawBox shrinks by 1)
            end;

            if not UseRightToLeftAlignment then
              DrawString( Canvas, S, R, dt_WordBreak or dt_ExpandTabs or dt_NoPrefix )
            else
              DrawString( Canvas, S, R, dt_WordBreak or dt_ExpandTabs or dt_NoPrefix or dt_Right or dt_RtlReading );

            Canvas.Brush.Style := bsClear;   // Restore brush to clear in case just drew selected item.
          end
          else // FItems[ I ].Caption = '-'... Draw a separator
          begin
            if SkipSeparator( I ) then
              Continue;

            R.Left := 8;
            R.Top := R.Top + FItemHeight div 2;
            R.Right := Width - 8;
            R.Bottom := R.Top + 1;

            if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
            begin
              C := ActiveStyleSystemColor( Color );
              DivColor := ActiveStyleSystemColor( FDividerColor );

              PaintGradient( Canvas, R, gdVerticalCenter, C, DivColor );
            end
            else
            begin
              Canvas.Pen.Color := FDividerColor;
              Canvas.MoveTo( R.Left, R.Top );
              Canvas.LineTo( R.Right, R.Top );
            end;
          end;
        end;


        // Set item's HotCaptionRect and HotImageRect for use in hit testing later
        if not UseRightToLeftAlignment then
        begin
          if ( FItemHotZone = ihzImageCaption ) and ( Canvas.TextWidth( FItems[ I ].Caption ) < ( R.Right - R.Left ) ) then
            Temp := R.Left + Canvas.TextWidth( FItems[ I ].Caption )
          else
            Temp := Width - 8;
          FItems[ I ].HotCaptionRect := Rect( R.Left, YTop + TextOffset, Temp, YTop + TextOffset + H );
        end
        else
        begin
          if ( FItemHotZone = ihzImageCaption ) and ( Canvas.TextWidth( FItems[ I ].Caption ) < ( R.Right - R.Left ) ) then
            Temp := R.Right - Canvas.TextWidth( FItems[ I ].Caption )
          else
            Temp := 8;
          FItems[ I ].HotCaptionRect := Rect( Temp, YTop + TextOffset, R.Right, YTop + TextOffset + H );
        end;
        FItems[ I ].HotImageRect := Rect( -1, -1, -1, -1 );

        if ( SmallImageList <> nil ) and ( FItems[ I ].ImageIndex <> -1 ) then
        begin
          XOffset := 8;
          if FItems[ I ].IndentLevel > 0 then
            XOffset := XOffset + FItems[ I ].IndentLevel * FItemIndent;

          if UseRightToLeftAlignment then
            XOffset := Width - XOffset - SmallImageList.Width;

          if ( Canvas.TextWidth( FItems[ I ].Caption ) < ( R.Right - R.Left ) ) or ( H < FItemHeight ) then
            YOffset := YTop + ( FItemHeight - SmallImageList.Height ) div 2
          else
            YOffset := FItems[ I ].HotCaptionRect.Top + TextOffset;


          // Show Hot, Down, or Selected State
          if FItemSelectionStyle = issImage then
          begin
            if ( ( FItems[ I ].CaptionState in [ csHot, csDown ] ) and FShowItemSelection ) or
               ( FShowItemSelection and FItems[ I ].FSelected ) then
            begin
              R := Rect( XOffset - 2, YOffset - 2, XOffset + SmallImageList.Width + 2, YOffset + SmallImageList.Height + 2 );
              R := DrawBox( Canvas, R, SelectionBorderColor );

              if ( FItems[ I ].CaptionState = csHot ) or
                 ( FItems[ I ].FSelected and ( FItems[ I ].CaptionState <> csDown ) ) then
                PaintGradient( Canvas, R, gdHorizontalEnd,
                               SelectionStartColor, SelectionStopColor )
              else
                PaintGradient( Canvas, R, gdHorizontalEnd,
                               SelectionStopColor, SelectionStartColor );
            end;
          end;

          if FItems[ I ].DisabledIndex <> -1 then
          begin
            if FItems[ I ].Enabled then
            begin
              if FItems[ I ].ImageIndex <> -1 then
                SmallImageList.Draw( Canvas, XOffset, YOffset, FItems[ I ].ImageIndex );
            end
            else
              SmallImageList.Draw( Canvas, XOffset, YOffset, FItems[ I ].DisabledIndex );
          end
          else
            SmallImageList.Draw( Canvas, XOffset, YOffset, FItems[ I ].ImageIndex, FItems[ I ].Enabled );

          if not UseRightToLeftAlignment then
            FItems[ I ].HotImageRect := Rect( XOffset, YOffset, XOffset + 20, YOffset + 16 )
          else
            FItems[ I ].HotImageRect := Rect( XOffset - 4, YOffset, XOffset + 16, YOffset + 16 );
        end;

        {$IFDEF USE_GROUP_DESIGNER}
        if ( csDesigning in ComponentState ) and FItems[ I ].FSelected then
        begin
          R := FItems[ I ].HotCaptionRect;
          InflateRect( R, 2, 2 );
          DrawDesignFocus( R );
        end;
        {$ENDIF}

        Inc( YTop, Max( H + 2 * TextOffset, FItemHeight ) );
      end;
    end; { for I }
  end
  else // if FItemStyle = isLarge
  begin
    YTop := FCaptionHeight + 4;
    TextOffset := ( FItemHeight - Canvas.TextHeight( 'Pp' ) ) div 2;
    if LargeImageList <> nil then
      RowHeight := LargeImageList.Height + 2
    else
      RowHeight := 34;

    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Visible then
      begin
        Canvas.Font := Self.Font;
        UpdateFontColorAndStyle( I );
        Canvas.Brush.Style := bsClear;

        XOffset := 8;

        if FHideAccelerators then
          S := RemoveAccelerators( FItems[ I ].Caption )
        else
          S := FItems[ I ].Caption;

        R := Rect( XOffset, YTop + RowHeight, Width - 8, YTop + RowHeight + FItemHeight );
        H := DrawString( Canvas, S, R, dt_CalcRect or dt_Center or dt_WordBreak or dt_ExpandTabs or dt_NoPrefix );

        if FItems[ I ].Caption <> '' then
        begin
          if FItems[ I ].Caption <> '-' then
          begin
            R := Rect( XOffset, YTop + RowHeight + TextOffset, Width - 8, YTop + RowHeight + TextOffset + H );

            DrawString( Canvas, S, R, dt_Center or dt_WordBreak or dt_ExpandTabs or dt_NoPrefix );

            // Restore brush to clear in case just drew selected item
            Canvas.Brush.Style := bsClear;
          end
          else // FItems[ I ].Caption = '-' ... Draw a separator
          begin
            if SkipSeparator( I ) then
              Continue;

            R.Left := 8;
            R.Top := YTop + isLargeSeparatorHeight div 2;
            R.Right := Width - 8;
            R.Bottom := R.Top + 1;

            if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
              PaintGradient( Canvas, R, gdVerticalCenter, Color, FDividerColor )
            else
            begin
              Canvas.Pen.Color := FDividerColor;
              Canvas.MoveTo( R.Left, R.Top );
              Canvas.LineTo( R.Right, R.Top );
            end;
          end;
        end;


        // Set item's HotCaptionRect and HotImageRect for use in hit testing later
        FItems[ I ].HotCaptionRect := R;
        FItems[ I ].HotImageRect := Rect( -1, -1, -1, -1 );

        if ( LargeImageList <> nil ) and ( FItems[ I ].ImageIndex <> -1 ) then
        begin
          XOffset := ( ClientWidth - LargeImageList.Width ) div 2;

          // Show Hot, Down, or Selected State
          if ( FItems[ I ].CaptionState in [ csHot, csDown ] ) or ( FShowItemSelection and FItems[ I ].FSelected ) then
          begin
            R := Rect( XOffset - 2, YTop, XOffset + LargeImageList.Width + 2, YTop + LargeImageList.Height + 4 );
            R := DrawBox( Canvas, R, SelectionBorderColor );

            if ( FItems[ I ].CaptionState = csHot ) or
               ( FItems[ I ].FSelected and ( FItems[ I ].CaptionState <> csDown ) ) then
              PaintGradient( Canvas, R, gdHorizontalEnd,
                             SelectionStartColor, SelectionStopColor )
            else
              PaintGradient( Canvas, R, gdHorizontalEnd,
                             SelectionStopColor, SelectionStartColor );
          end;

          if FItems[ I ].DisabledIndex <> -1 then
          begin
            if FItems[ I ].Enabled then
            begin
              if FItems[ I ].ImageIndex <> -1 then
                LargeImageList.Draw( Canvas, XOffset, YTop + 2, FItems[ I ].ImageIndex );
            end
            else
              LargeImageList.Draw( Canvas, XOffset, YTop + 2, FItems[ I ].DisabledIndex );
          end
          else
            LargeImageList.Draw( Canvas, XOffset, YTop + 2, FItems[ I ].ImageIndex, FItems[ I ].Enabled );

          FItems[ I ].HotImageRect := Rect( XOffset, YTop + 2, XOffset + LargeImageList.Width, YTop + 2 + LargeImageList.Height + 4 );
        end;

        {$IFDEF USE_GROUP_DESIGNER}
        if ( csDesigning in ComponentState ) and FItems[ I ].FSelected then
        begin
          R := FItems[ I ].HotCaptionRect;
          InflateRect( R, 2, 2 );
          DrawDesignFocus( R );
        end;
        {$ENDIF}

        if FItems[ I ].Caption <> '-' then
          Inc( YTop, RowHeight + Max( H + 2 * TextOffset, FItemHeight ) + 8 )
        else
          Inc( YTop, isLargeSeparatorHeight + 8 );
      end;
    end; { for I }

  end;
  Canvas.Brush.Style := bsSolid;
end; {= TRzGroup.DrawItems =}


procedure TRzGroup.DrawOpenCloseButton( Opened: Boolean; Left, Top: Integer;
                                        FillColor, LineColor, ChevronColor: TColor );
var
  OldPenColor: TColor;
begin
  // Draw the outer circle
  Canvas.Pixels[ Left +  4, Top +  0 ] := BlendColors( LineColor, Canvas.Pixels[ Left +  4, Top +  0 ], 128 );
  Canvas.Pixels[ Left +  5, Top +  0 ] := LineColor;
  Canvas.Pixels[ Left +  6, Top +  0 ] := LineColor;
  Canvas.Pixels[ Left +  7, Top +  0 ] := LineColor;
  Canvas.Pixels[ Left +  8, Top +  0 ] := LineColor;
  Canvas.Pixels[ Left +  9, Top +  0 ] := LineColor;
  Canvas.Pixels[ Left + 10, Top +  0 ] := BlendColors( LineColor, Canvas.Pixels[ Left + 10, Top +  0 ], 128 );

  Canvas.Pixels[ Left +  3, Top +  1 ] := LineColor;
  Canvas.Pixels[ Left +  4, Top +  1 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 10, Top +  1 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 11, Top +  1 ] := LineColor;

  Canvas.Pixels[ Left +  2, Top +  2 ] := LineColor;
  Canvas.Pixels[ Left + 12, Top +  2 ] := LineColor;

  Canvas.Pixels[ Left +  1, Top +  3 ] := LineColor;
  Canvas.Pixels[ Left + 13, Top +  3 ] := LineColor;
  Canvas.Pixels[ Left +  1, Top +  4 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 13, Top +  4 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 14, Top +  4 ] := BlendColors( LineColor, Canvas.Pixels[ Left + 14, Top +  4 ], 128 );

  Canvas.Pixels[ Left +  0, Top +  4 ] := BlendColors( LineColor, Canvas.Pixels[ Left +  0, Top +  4 ], 128 );
  Canvas.Pixels[ Left +  0, Top +  5 ] := LineColor;
  Canvas.Pixels[ Left + 14, Top +  5 ] := LineColor;
  Canvas.Pixels[ Left +  0, Top +  6 ] := LineColor;
  Canvas.Pixels[ Left + 14, Top +  6 ] := LineColor;
  Canvas.Pixels[ Left +  0, Top +  7 ] := LineColor;
  Canvas.Pixels[ Left + 14, Top +  7 ] := LineColor;
  Canvas.Pixels[ Left +  0, Top +  8 ] := LineColor;
  Canvas.Pixels[ Left + 14, Top +  8 ] := LineColor;
  Canvas.Pixels[ Left +  0, Top +  9 ] := LineColor;
  Canvas.Pixels[ Left + 14, Top +  9 ] := LineColor;

  Canvas.Pixels[ Left +  0, Top + 10 ] := BlendColors( LineColor, Canvas.Pixels[ Left +  0, Top + 10 ], 128 );
  Canvas.Pixels[ Left +  1, Top + 10 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 13, Top + 10 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left +  1, Top + 11 ] := LineColor;
  Canvas.Pixels[ Left + 13, Top + 11 ] := LineColor;
  Canvas.Pixels[ Left + 14, Top + 10 ] := BlendColors( LineColor, Canvas.Pixels[ Left + 14, Top + 10 ], 128 );

  Canvas.Pixels[ Left +  2, Top + 12 ] := LineColor;
  Canvas.Pixels[ Left + 12, Top + 12 ] := LineColor;

  Canvas.Pixels[ Left +  3, Top + 13 ] := LineColor;
  Canvas.Pixels[ Left +  4, Top + 13 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 10, Top + 13 ] := BlendColors( LineColor, FillColor, 128 );
  Canvas.Pixels[ Left + 11, Top + 13 ] := LineColor;

  Canvas.Pixels[ Left +  4, Top + 14 ] := BlendColors( LineColor, Canvas.Pixels[ Left +  4, Top + 14 ], 128 );
  Canvas.Pixels[ Left +  5, Top + 14 ] := LineColor;
  Canvas.Pixels[ Left +  6, Top + 14 ] := LineColor;
  Canvas.Pixels[ Left +  7, Top + 14 ] := LineColor;
  Canvas.Pixels[ Left +  8, Top + 14 ] := LineColor;
  Canvas.Pixels[ Left +  9, Top + 14 ] := LineColor;
  Canvas.Pixels[ Left + 10, Top + 14 ] := BlendColors( LineColor, Canvas.Pixels[ Left + 10, Top + 14 ], 128 );

  // Fill Circle
  OldPenColor := Canvas.Pen.Color;
  Canvas.Pen.Color := FillColor;
  Canvas.MoveTo( Left +  1, Top +  5 );
  Canvas.LineTo( Left +  1, Top + 10 );
  Canvas.MoveTo( Left +  2, Top +  3 );
  Canvas.LineTo( Left +  2, Top + 12 );
  Canvas.MoveTo( Left +  3, Top +  2 );
  Canvas.LineTo( Left +  3, Top + 13 );
  Canvas.MoveTo( Left +  4, Top +  2 );
  Canvas.LineTo( Left +  4, Top + 13 );
  Canvas.MoveTo( Left +  5, Top +  1 );
  Canvas.LineTo( Left +  5, Top + 14 );
  Canvas.MoveTo( Left +  6, Top +  1 );
  Canvas.LineTo( Left +  6, Top + 14 );
  Canvas.MoveTo( Left +  7, Top +  1 );
  Canvas.LineTo( Left +  7, Top + 14 );
  Canvas.MoveTo( Left +  8, Top +  1 );
  Canvas.LineTo( Left +  8, Top + 14 );
  Canvas.MoveTo( Left +  9, Top +  1 );
  Canvas.LineTo( Left +  9, Top + 14 );
  Canvas.MoveTo( Left + 10, Top +  2 );
  Canvas.LineTo( Left + 10, Top + 13 );
  Canvas.MoveTo( Left + 11, Top +  2 );
  Canvas.LineTo( Left + 11, Top + 13 );
  Canvas.MoveTo( Left + 12, Top +  3 );
  Canvas.LineTo( Left + 12, Top + 12 );
  Canvas.MoveTo( Left + 13, Top +  5 );
  Canvas.LineTo( Left + 13, Top +  10 );
  Canvas.Pen.Color := OldPenColor;

  // Draw the Chevron
  if Opened then
  begin
    Canvas.Pixels[ Left +  7, Top +  3 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left +  7, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  4, Top +  6 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  6 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  6 ] := ChevronColor;
    Canvas.Pixels[ Left + 10, Top +  6 ] := ChevronColor;

    Canvas.Pixels[ Left +  7, Top +  7 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left +  7, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  4, Top +  10 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  10 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  10 ] := ChevronColor;
    Canvas.Pixels[ Left + 10, Top +  10 ] := ChevronColor;
  end
  else
  begin
    Canvas.Pixels[ Left +  4, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left + 10, Top +  4 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  5 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  6 ] := ChevronColor;
    Canvas.Pixels[ Left +  7, Top +  6 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  6 ] := ChevronColor;
    Canvas.Pixels[ Left +  7, Top +  7 ] := ChevronColor;

    Canvas.Pixels[ Left +  4, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left + 10, Top +  8 ] := ChevronColor;
    Canvas.Pixels[ Left +  5, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  9, Top +  9 ] := ChevronColor;
    Canvas.Pixels[ Left +  6, Top +  10 ] := ChevronColor;
    Canvas.Pixels[ Left +  7, Top +  10 ] := ChevronColor;
    Canvas.Pixels[ Left +  8, Top +  10 ] := ChevronColor;
    Canvas.Pixels[ Left +  7, Top +  11 ] := ChevronColor;
  end;
end;


procedure TRzGroup.DrawCategoryGroup;
var
  R: TRect;
  ForeColor, CornerColor: TColor;
  CaptionBackColor, CaptionBackColorStart, CaptionBackColorStop: TColor;
  CaptionFontColor, CaptionFontHotColor, CaptionFontDownColor: TColor;
  CaptionButtonColor, CaptionButtonBorderColor: TColor;
  CaptionDividerColor, GroupColor, GroupBorderColor: TColor;
  XOffset, TextOffset: Integer;
  OldTextAlign: Longint;
begin
  CornerColor := GroupBar.Canvas.Pixels[ Left - 1, Top ];

  if UsingSystemStyle then
  begin
    if GroupBar.GradientColorStyle <> gcsCustom then
    begin
      GetGradientCategoryGroupColors( GroupBar.GradientColorStyle, FSpecial,
                                      CaptionBackColor, CaptionBackColorStart,
                                      CaptionBackColorStop, CaptionFontColor,
                                      CaptionFontHotColor,
                                      CaptionButtonColor, CaptionButtonBorderColor,
                                      CaptionDividerColor, GroupColor,
                                      GroupBorderColor );
    end
    else // GroupBar.GradientColorStyle = gcsCustom
    begin
      if FSpecial then
      begin
        CaptionBackColor := FCaptionFont.Color;
        CaptionFontColor := FCaptionColor;
        CaptionBackColorStart := FCaptionFont.Color;
        CaptionBackColorStop := LighterColor( CaptionBackColorStart, 30 );
      end
      else
      begin
        CaptionBackColor := FCaptionColor;
        CaptionFontColor := FCaptionFont.Color;
        CaptionBackColorStart := FCaptionColorStart;
        CaptionBackColorStop := FCaptionColorStop;
      end;
      CaptionFontHotColor := FCaptionHotColor;
      CaptionDividerColor := FDividerColor;
      CaptionButtonColor := CaptionBackColor;
      CaptionButtonBorderColor := DarkerColor( CaptionButtonColor, 30 );
      GroupColor := Color;
      GroupBorderColor := FBorderColor;
    end;
  end
  else // VCL Styles
  begin
    if FSpecial then
    begin
      CaptionBackColor := ActiveStyleSystemColor( clHighlight );
      CaptionFontColor := ActiveStyleSystemColor( clHighlightText );
      CaptionFontHotColor := ActiveStyleSystemColor( clHighlightText );
      CaptionBackColorStart := ActiveStyleSystemColor( clHighlight );
      CaptionBackColorStop := DarkerColor( CaptionBackColorStart, 30 );
    end
    else
    begin
      CaptionBackColor := ActiveStyleColor( scToolBarGradientBase );
      CaptionFontColor := ActiveStyleFontColor( sfCategoryPanelGroupHeaderNormal );
      CaptionFontHotColor := ActiveStyleFontColor( sfCategoryPanelGroupHeaderHot );
      CaptionBackColorStart := LighterColor( ActiveStyleSystemColor( clBtnFace ), 30 );
      CaptionBackColorStop := LighterColor( CaptionBackColorStart, 30 );
    end;
    CaptionDividerColor := ActiveStyleSystemColor( clHighlight );
    CaptionButtonColor := CaptionBackColor;
    CaptionButtonBorderColor := DarkerColor( CaptionButtonColor, 30 );
    GroupColor := ActiveStyleSystemColor( clBtnFace );
    GroupBorderColor := ActiveStyleSystemColor( clBtnHighlight );
  end;

  CaptionFontDownColor := BlendColors( clBlack, CaptionFontColor, 50 );


  // Determine Font Color

  Canvas.Font := FCaptionFont;
  ForeColor := CaptionFontColor;
  if not ( csDesigning in ComponentState ) then
  begin
    // Only change colors of caption at runtime because the group does not
    // receive OnMouseLeave events at design-time and so the caption bar does
    // not get redrawn when the mouse leaves the group.
    case FCaptionState of
      csHot:  ForeColor := CaptionFontHotColor;
      csDown: ForeColor := CaptionFontDownColor;
    end;
  end;
  Canvas.Font.Color := ForeColor;

  // Draw Caption Background

  if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
  begin
    if not UseRightToLeftAlignment then
    begin
      R := Rect( 0, 0, Width div 2, FCaptionHeight );
      Canvas.Brush.Color := CaptionBackColorStart;
      Canvas.FillRect( R );
      R := Rect( Width div 2, 0, Width, FCaptionHeight );
      PaintGradient( Canvas, R, gdVerticalEnd, CaptionBackColorStart,
                     CaptionBackColorStop );
    end
    else
    begin
      R := Rect( Width div 2, 0, Width, FCaptionHeight );
      Canvas.Brush.Color := CaptionBackColorStart;
      Canvas.FillRect( R );
      R := Rect( 0, 0, Width div 2, FCaptionHeight );
      PaintGradient( Canvas, R, gdVerticalEnd, CaptionBackColorStop,
                     CaptionBackColorStart );
    end;
  end
  else
  begin
    Canvas.Brush.Color := CaptionBackColorStart;
    R := Rect( 0, 0, Width, FCaptionHeight );
    Canvas.FillRect( R );
  end;
  {&RV}

  // Draw Caption Image
  XOffset := DrawCaptionImage;

  // Draw Caption Text

  TextOffset := ( FCaptionHeight - Canvas.TextHeight( 'Pp' ) ) div 2;
  Canvas.Brush.Style := bsClear;
  if not UseRightToLeftAlignment then
  begin
    R := Rect( XOffset, 0, Width - 24, FCaptionHeight );
    Canvas.TextRect( R, R.Left, R.Top + TextOffset, Caption );
  end
  else
  begin
    R := Rect( 24, 0, Width - XOffset, FCaptionHeight );
    OldTextAlign := SetTextAlign( Canvas.Handle, ta_Right );
    Canvas.TextRect( R, R.Right, R.Top + TextOffset, Caption );
    SetTextAlign( Canvas.Handle, OldTextAlign );
  end;
  Canvas.Brush.Style := bsSolid;

  // Draw Open/Close Circle
  if FCanClose then
  begin
    if not UseRightToLeftAlignment then
    begin
      DrawOpenCloseButton( FOpened, Width - 20, ( FCaptionHeight - 15 ) div 2,
                           CaptionButtonColor, CaptionButtonBorderColor, ForeColor );
    end
    else
    begin
      DrawOpenCloseButton( FOpened, 4, ( FCaptionHeight - 15 ) div 2,
                           CaptionButtonColor, CaptionButtonBorderColor, ForeColor );
    end;

    // Draw (i.e. Erase) top corners
    Canvas.Pixels[ 0, 0 ] := CornerColor;
    Canvas.Pixels[ 1, 0 ] := CornerColor;
    Canvas.Pixels[ 0, 1 ] := CornerColor;

    Canvas.Pixels[ Width - 1, 0 ] := CornerColor;
    Canvas.Pixels[ Width - 2, 0 ] := CornerColor;
    Canvas.Pixels[ Width - 1, 1 ] := CornerColor;
  end;

  // Draw dividing line
  if FDividerVisible and FOpened then
  begin
    if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
    begin
      R := Rect( 0, FCaptionHeight - 1, Width, FCaptionHeight );
      if not UseRightToLeftAlignment then
        PaintGradient( Canvas, R, gdVerticalEnd, CaptionDividerColor, GroupColor )
      else
        PaintGradient( Canvas, R, gdVerticalEnd, GroupColor, CaptionDividerColor );
    end
    else
    begin
      Canvas.Pen.Color := CaptionDividerColor;
      Canvas.MoveTo( 0, FCaptionHeight - 1 );
      Canvas.LineTo( Width, FCaptionHeight - 1 );
    end;
  end;

  // Draw Left, Bottom, Right Edge and fill interior

  if FOpened then
  begin
    Canvas.Pen.Color := GroupBorderColor;
    Canvas.MoveTo( 0, FCaptionHeight );
    Canvas.LineTo( 0, Height - 1 );
    Canvas.LineTo( Width - 1, Height - 1 );
    Canvas.LineTo( Width - 1, FCaptionHeight - 1 );

    R := Rect( 1, FCaptionHeight, Width - 1, Height - 1 );
    Canvas.Brush.Color := GroupColor;
    Canvas.FillRect( R );
  end;

  DrawItems;
end; {= TRzGroup.DrawCategoryGroup =}


procedure TRzGroup.DrawThemedCategoryGroup;
var
  R, SrcRect, ThemeRect: TRect;
  Bmp: TBitmap;
  TS: TSize;
  XOffset: Integer;
  ElementDetails: TThemedElementDetails;
begin
  // Draw Caption
  Bmp := TBitmap.Create;
  try
    R := Rect( 0, 0, Width, FCaptionHeight );

    ThemeRect := GroupBar.ClientRect;
    Bmp.Width := ThemeRect.Right - ThemeRect.Left;
    Bmp.Height := ThemeRect.Bottom - ThemeRect.Top;

    ElementDetails := ActiveStyleServices.GetElementDetails( tebExplorerBarRoot );
    ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, ElementDetails, ThemeRect );

    SrcRect := Rect( BoundsRect.Left + R.Left, BoundsRect.Top + R.Top,
                     BoundsRect.Left + R.Right, BoundsRect.Top + R.Bottom );
    Canvas.CopyRect( R, Bmp.Canvas, SrcRect );

    if FSpecial then
      ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupHead )
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupHead );
    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );

    if not FCanClose then
    begin
      // Fill in top corners
      Canvas.Pixels[ 0, 0 ] := Canvas.Pixels[ 0, 2 ];
      Canvas.Pixels[ 1, 0 ] := Canvas.Pixels[ 1, 2 ];
      Canvas.Pixels[ 0, 1 ] := Canvas.Pixels[ 0, 2 ];

      Canvas.Pixels[ Width - 1, 0 ] := Canvas.Pixels[ Width - 1, 2 ];
      Canvas.Pixels[ Width - 2, 0 ] := Canvas.Pixels[ Width - 2, 2 ];
      Canvas.Pixels[ Width - 1, 1 ] := Canvas.Pixels[ Width - 1, 2 ];
    end;
  finally
    Bmp.Free;
  end;

  // Draw Caption Image
  XOffset := DrawCaptionImage;

  // Draw Caption Text
  if not UseRightToLeftAlignment then
    R := Rect( XOffset, 0, Width - 24, FCaptionHeight )
  else
    R := Rect( 0, 0, Width - XOffset - 20, FCaptionHeight );
  if FSpecial then
    ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupHead )
  else
    ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupHead );
  if not UseRightToLeftAlignment then
    ActiveStyleServicesDrawText( Canvas.Handle, ElementDetails, Caption, R, dt_Left or dt_VCenter or dt_SingleLine )
  else
    ActiveStyleServicesDrawText( Canvas.Handle, ElementDetails, Caption, R,
                                 dt_Right or dt_RtlReading or dt_VCenter or dt_SingleLine );

  // Draw Open/Close Circle
  if FCanClose then
  begin
    if FOpened then
    begin
      if FSpecial then
        ElementDetails := ActiveStyleServices.GetElementDetails( {$IFDEF VCL160_OR_HIGHER}
                                                                 tebSpecialGroupCollapseNormal
                                                                 {$ELSE}
                                                                 tebSpecialGroupCollapseSpecial
                                                                 {$ENDIF} )
      else
        ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupCollapseNormal );
      if not ( csDesigning in ComponentState ) then
      begin
        case FCaptionState of
          csHot:
          begin
            if FSpecial then
              ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupCollapseHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupCollapseHot );
          end;

          csDown:
          begin
            if FSpecial then
              ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupCollapsePressed )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupCollapsePressed );
          end;
        end;
      end;
    end
    else
    begin
      if FSpecial then
        ElementDetails := ActiveStyleServices.GetElementDetails( {$IFDEF VCL160_OR_HIGHER}
                                                                 tebSpecialGroupExpandNormal
                                                                 {$ELSE}
                                                                 tebSpecialGroupExpandSpecial
                                                                 {$ENDIF} )
      else
        ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupExpandNormal );
      if not ( csDesigning in ComponentState ) then
      begin
        case FCaptionState of
          csHot:
          begin
            if FSpecial then
              ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupExpandHot )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupExpandHot );
          end;

          csDown:
          begin
            if FSpecial then
              ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupExpandPressed )
            else
              ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupExpandPressed );
          end;
        end;
      end;
    end;

    GetThemePartSize( ActiveStyleServices.Theme[ teExplorerBar ], Canvas.Handle, ElementDetails.Part, ElementDetails.State,
                      nil, TS_TRUE, TS );

    // Theme does not support right-to-left orientation (i.e. gradient), so Open/Close circle must appear on right
    R := Bounds( Width - TS.cx - 2, ( FCaptionHeight - TS.cy ) div 2, TS.cx, TS.cy );

    (*
    // The following would be used in place of the above if XP Themes supported
    // positioning the open/close button on the left side of the gradient
    if not UseRightToLeftAlignment then
      R := Bounds( Width - TS.cx - 2, ( FCaptionHeight - TS.cy ) div 2, TS.cx, TS.cy )
    else
      R := Bounds( 4, ( FCaptionHeight - TS.cy ) div 2, TS.cx, TS.cy );
    *)

    ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );
  end;

  // Draw Client Area
  R := Rect( 0, FCaptionHeight, Width, Height );
  if FSpecial then
    ElementDetails := ActiveStyleServices.GetElementDetails( tebSpecialGroupBackground )
  else
    ElementDetails := ActiveStyleServices.GetElementDetails( tebNormalGroupBackground );
  ActiveStyleServices.DrawElement( Canvas.Handle, ElementDetails, R );

  DrawItems;
end; {= TRzGroup.DrawThemedCategoryGroup =}


procedure TRzGroup.DrawOutlookGroup;
var
  R: TRect;
  ForeColor, CaptionBackStartColor, CaptionBackStopColor: TColor;
  CaptionFontColor, CaptionFontHotColor, CaptionFontDownColor: TColor;
  GroupStartColor, GroupStopColor: TColor;
  XOffset, YOffset, TextOffset: Integer;
begin

  if UsingSystemStyle then
  begin
    if GroupBar.GradientColorStyle <> gcsCustom then
    begin
      GetGradientOutlookGroupColors( GroupBar.GradientColorStyle,
                                     CaptionBackStartColor,
                                     CaptionBackStopColor, CaptionFontColor,
                                     CaptionFontHotColor,
                                     GroupStartColor, GroupStopColor );
    end
    else // GroupBar.GradientColorStyle = gcsCustom
    begin
      CaptionBackStartColor := FCaptionColorStart;
      CaptionBackStopColor := FCaptionColorStop;

      CaptionFontColor := FCaptionFont.Color;
      CaptionFontHotColor := FCaptionHotColor;
      GroupStartColor := LighterColor( Color, 20 );
      GroupStopColor:= Color;
    end;
  end
  else // VCL Styles
  begin
    CaptionBackStopColor := ActiveStyleColor( scButtonNormal );
    CaptionBackStartColor := LighterColor( CaptionBackStopColor, 30 );

    CaptionFontColor := ActiveStyleFontColor( sfButtonTextNormal );
    CaptionFontHotColor := ActiveStyleFontColor( sfButtonTextHot );
    GroupStartColor := LighterColor( ActiveStyleSystemColor( clBtnFace ), 20 );
    GroupStopColor:= ActiveStyleSystemColor( clBtnFace );
  end;

  CaptionFontDownColor := BlendColors( clBlack, CaptionFontColor, 50 );

  // Determine Font Color

  Canvas.Font := FCaptionFont;
  ForeColor := CaptionFontColor;
  if not ( csDesigning in ComponentState ) then
  begin
    // Only change colors of caption at runtime because the group does not
    // receive OnMouseLeave events at design-time and so the caption bar does
    // not get redrawn when the mouse leaves the group.
    case FCaptionState of
      csHot:  ForeColor := CaptionFontHotColor;
      csDown: ForeColor := CaptionFontDownColor;
    end;
  end;
  Canvas.Font.Color := ForeColor;


  // Draw Caption Bar border

  R := Rect( 0, 0, Width, FCaptionHeight );

  if not ( csDesigning in ComponentState ) then
  begin
    case FCaptionState of
      csHot:
        R := DrawBorder( Canvas, R, fsButtonUp );

      csDown:
        R := DrawBorder( Canvas, R, fsStatus );

      else
        R := DrawBorder( Canvas, R, fsPopup );
    end;
  end
  else
  begin
    R := DrawBorder( Canvas, R, fsPopup );
  end;

  // Draw Caption Bar interior

  if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
  begin
    PaintGradient( Canvas, R, gdHorizontalEnd,
                   CaptionBackStartColor, CaptionBackStopColor );
  end
  else
  begin
    Canvas.Brush.Color := FCaptionColor;
    Canvas.FillRect( R );
  end;

  XOffset := ( Width - Canvas.TextWidth( Caption ) ) div 2;
  if XOffset < 0 then
    XOffset := 0;
  if ( CaptionImageList <> nil ) and ( FCaptionImageIndex <> -1 ) then
  begin
    YOffset := ( FCaptionHeight - CaptionImageList.Height ) div 2;
    XOffset := XOffset - ( CaptionImageList.Width + 4 ) div 2;
    if XOffset < 0 then
      XOffset := 0;
    CaptionImageList.Draw( Canvas, XOffset, YOffset, FCaptionImageIndex, Enabled );
    XOffset := XOffset + CaptionImageList.Width + 4;
  end;

  Canvas.Brush.Style := bsClear;
  R := Rect( XOffset, 2, Width - 2, FCaptionHeight - 2 );
  TextOffset := ( FCaptionHeight - 4 - Canvas.TextHeight( 'Pp' ) ) div 2;
  Canvas.TextRect( R, R.Left, R.Top + TextOffset, Caption );
  Canvas.Brush.Style := bsSolid;

  // Fill Interior if necessary
  if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
  begin
    R := ClientRect;
    Inc( R.Top, FCaptionHeight );

    PaintGradient( Canvas, R, gdHorizontalEnd, GroupStartColor, GroupStopColor );
  end;

  DrawItems;
end; {= TRzGroup.DrawOutlookGroup =}


procedure TRzGroup.DrawTaskListGroup;
var
  R, TempRect: TRect;
  ViewColor, CaptionFontColor, CaptionDividerColor: TColor;
  XOffset, TextOffset: Integer;
  OldTextAlign: Longint;
begin
  if UsingSystemStyle then
  begin
    ViewColor := Color;
    CaptionFontColor := FCaptionFont.Color;
  end
  else
  begin
    ViewColor := DarkerColor( ActiveStyleSystemColor( clBtnFace ), 20 );
    CaptionFontColor := ActiveStyleFontColor( sfButtonTextNormal );
  end;
  CaptionDividerColor := ActiveStyleSystemColor( clHighlight );

  // Draw Caption Bar

  Canvas.Brush.Color := ViewColor;
  Canvas.Font := FCaptionFont;
  Canvas.Font.Color := CaptionFontColor;

  Canvas.FillRect( ClientRect );

  if csDesigning in ComponentState then
  begin
    TempRect := ClientRect;
    Inc( TempRect.Top, FCaptionHeight - 1 );
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Color := CaptionDividerColor;
    Canvas.Rectangle( TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom );
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.style := psSolid;
  end;

  // Draw Caption Image
  XOffset := DrawCaptionImage;

  // Draw Caption Text

  TextOffset := ( FCaptionHeight - Canvas.TextHeight( 'Pp' ) ) div 2;
  Canvas.Brush.Style := bsClear;
  if not UseRightToLeftAlignment then
  begin
    R := Rect( XOffset, 0, Width, FCaptionHeight );
    Canvas.TextRect( R, R.Left, R.Top + TextOffset, Caption );
  end
  else
  begin
    R := Rect( 0, 0, Width - XOffset, FCaptionHeight );
    OldTextAlign := SetTextAlign( Canvas.Handle, ta_Right );
    Canvas.TextRect( R, R.Right, R.Top + TextOffset, Caption );
    SetTextAlign( Canvas.Handle, OldTextAlign );
  end;
  Canvas.Brush.Style := bsSolid;


  // Draw dividing line
  if FDividerVisible then
  begin
    if ( GroupBar.VisualStyle <> vsClassic ) and FullColorSupported then
    begin
      R := Rect( 0, FCaptionHeight - 1, Width, FCaptionHeight );
      PaintGradient( Canvas, R, gdVerticalEnd, CaptionDividerColor, ViewColor );
    end
    else
    begin
      Canvas.Pen.Color := CaptionDividerColor;
      Canvas.MoveTo( 0, FCaptionHeight - 1 );
      Canvas.LineTo( Width, FCaptionHeight - 1 );
    end;
  end;

  DrawItems;
end; {= TRzGroup.DrawTaskListGroup =}


procedure TRzGroup.Paint;
begin
  inherited;

  if GroupBar <> nil then
  begin
    case GroupBar.Style of
      gbsCategoryView:
      begin
        if UsingSystemStyle and ( GroupBar.VisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
          DrawThemedCategoryGroup
        else
          DrawCategoryGroup;
      end;

      gbsTaskList:
        DrawTaskListGroup;

      gbsOutlook:
        DrawOutlookGroup;
    end;
  end;
end; {= TRzGroup.Paint =}


procedure TRzGroup.AlignControls( AControl: TControl; var Rect: TRect );
begin
  Inc( Rect.Top, FCaptionHeight );
  if FStyle = gbsCategoryView then
  begin
    Dec( Rect.Right );
    Dec( Rect.Bottom );
    Inc( Rect.Left );
  end;
  inherited;
end;


procedure TRzGroup.ChangeScale( M, D: Integer );
begin
  inherited;
  OpenedHeight := FOpenedHeight * M div D;
end;


procedure TRzGroup.UpdateScrollBtnPositions;
var
  Offset: Integer;
begin
  if FStyle = gbsOutlook then
  begin
    // Reposition the Scroll Buttons
    FScrollUpBtn.SetBounds( ClientWidth - 21, FCaptionHeight + 4, 17, 17 );
    if Opened then
      Offset := ClientHeight - 21
    else
      Offset := FCaptionHeight + 4;
    FScrollDownBtn.SetBounds( ClientWidth - 21, Offset, 17, 17 );
  end;
end;


procedure TRzGroup.UpdateScrollBtnVisibility;
begin
  if ( FStyle <> gbsOutlook ) or ( ( FStyle = gbsOutlook ) and not FOpened ) then
  begin
    FScrollUpBtn.Visible := False;
    FScrollDownBtn.Visible := False;
  end
  else // if FStyle = gbsOutlook
  begin
    FScrollUpBtn.Visible := FTopItem > 0;
    if FItems.Count > 0 then
    begin
      FScrollDownBtn.Visible := CalculateHeight( ClientWidth ) > ClientHeight;
    end
    else
      FScrollDownBtn.Visible := False;
  end;
end;


procedure TRzGroup.Resize;
begin
  inherited;
  if FStyle = gbsOutlook then
  begin
    UpdateScrollBtnPositions;

    // Do the Scroll Buttons need to be Visible?
    UpdateScrollBtnVisibility;
  end;
end;


procedure TRzGroup.UpdateGroupBarLayout;
begin
  if FGroupBar <> nil then
    FGroupBar.UpdateLayout( Self );
end;


procedure TRzGroup.ScrollUpHandler( Sender: TObject );
begin
  Dec( FTopItem );

  // The following test is needed because the TRzRapidFireButton may fire an additional click event before the button
  // can be hidden by the UpdateScrollBtnVisibility method.
  if FTopItem < 0 then
    FTopItem := 0;

  UpdateScrollBtnVisibility;
  Invalidate;
end;


procedure TRzGroup.ScrollDownHandler( Sender: TObject );
begin
  Inc( FTopItem );

  // The following test is needed because the TRzRapidFireButton may fire an additional click event before the button
  // can be hidden by the UpdateScrollBtnVisibility method.
  if FTopItem > Items.Count - 1 then
    FTopItem := Items.Count - 1;

  UpdateScrollBtnVisibility;
  Invalidate;
end;


procedure TRzGroup.SetTopItem( Value: Integer );
begin
  if ( FStyle = gbsOutlook) and ( FTopItem <> Value ) and
     ( Value >= 0 ) and ( Value < Items.Count ) then
  begin
    FTopItem := Value;
    UpdateScrollBtnVisibility;
    Invalidate;
  end;
end;


function TRzGroup.NotUsingGroupController: Boolean;
begin
  Result := FGroupController = nil;
end;


procedure TRzGroup.SetGroupController( Value: TRzGroupController );
begin
  if FGroupController <> nil then
    FGroupController.RemoveGroup( Self );
  FGroupController := Value;
  if Value <> nil then
  begin
    Value.AddGroup( Self );
    Value.FreeNotification( Self );
  end;
end;


procedure TRzGroup.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetCanClose( Value: Boolean );
begin
  if FStyle <> gbsCategoryView then
    Exit;                            // Changing CanClose by user only makes sense for gbsCategoryView

  if FCanClose <> Value then
  begin
    if not Value then
      Open;
    FCanClose := Value;
    Invalidate;
  end;
end;


function TRzGroup.DefaultCaptionColor( Style: TRzGroupBarStyle ): TColor;
begin
  case Style of
    gbsCategoryView:
      Result := clHighlightText;

    gbsTaskList:
      Result := clWindow;

    gbsOutlook:
      Result := clBtnFace;
  else
    Result := clHighlightText;
  end;
end;


procedure TRzGroup.SetCaptionColor( Value: TColor );
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    FCaptionColorDefault := False;
    Invalidate;
  end;
end;


procedure TRzGroup.SetCaptionColorStart( Value: TColor );
begin
  if FCaptionColorStart <> Value then
  begin
    FCaptionColorStart := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetCaptionColorStop( Value: TColor );
begin
  if FCaptionColorStop <> Value then
  begin
    FCaptionColorStop := Value;
    Invalidate;
  end;
end;


function TRzGroup.IsCaptionColorStored: Boolean;
begin
  Result := NotUsingGroupController and not FCaptionColorDefault;
end;


procedure TRzGroup.SetCaptionColorDefault( Value: Boolean );
begin
  FCaptionColorDefault := Value;
  if FCaptionColorDefault then
    CaptionColor := DefaultCaptionColor( FStyle );
end;


function TRzGroup.IsCaptionFontStored: Boolean;
begin
  Result := NotUsingGroupController and FCaptionFontChanged;
end;


procedure TRzGroup.SetCaptionFont( Value: TFont );
begin
  FCaptionFont.Assign( Value );
end;


procedure TRzGroup.SetCaptionHeight( Value: Integer );
begin
  if FCaptionHeight <> Value then
  begin
    FCaptionHeight := Value;
    if FCaptionHeight < 16 then
      FCaptionHeight := 16;
    Invalidate;
    UpdateGroupBarLayout;
  end;
end;


procedure TRzGroup.SetCaptionHotColor( Value: TColor );
begin
  if FCaptionHotColor <> Value then
  begin
    FCaptionHotColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItemHotColor( Value: TColor );
begin
  if FItemHotColor <> Value then
  begin
    FItemHotColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItemHotZone( Value: TRzGroupItemHotZone );
begin
  if FItemHotZone <> Value then
  begin
    FItemHotZone := Value;
    Invalidate;
  end;
end;


function TRzGroup.IsItemStaticFontStored: Boolean;
begin
  Result := NotUsingGroupController and FItemStaticFontChanged;
end;


procedure TRzGroup.SetItemStaticFont( Value: TFont );
begin
  FItemStaticFont.Assign( Value );
end;


procedure TRzGroup.SetCaptionImageIndex( Value: TImageIndex );
begin
  if FCaptionImageIndex <> Value then
  begin
    FCaptionImageIndex := Value;
    Reposition;
    InvalidateGroupBar;
    Invalidate;
  end;
end;


function TRzGroup.StoreColor: Boolean;
begin
  Result := NotUsingGroupController and not FColorDefault;
end;


function TRzGroup.GetColor: TColor;
begin
  Result := inherited Color;
end;


procedure TRzGroup.SetColor( Value: TColor );
begin
  if Color <> Value then
  begin
    inherited Color := Value;
    FColorDefault := False;
    Invalidate;
  end;
end;


procedure TRzGroup.SetColorDefault( Value: Boolean );
begin
  FColorDefault := Value;
  if FColorDefault then
    inherited Color := DefaultColor( FStyle );
end;


function TRzGroup.DefaultColor( Style: TRzGroupBarStyle ): TColor;
begin
  case Style of
    gbsCategoryView:
    begin
      if ( GroupBar.VisualStyle = vsWinXP ) and ActiveStyleServicesEnabled then
      begin
        Result := GetXPThemeColor( xptcGroupBarFill );
      end
      else
        Result := clBtnFace;
    end;

    gbsTaskList:
      Result := GroupBar.Color;

    gbsOutlook:
      Result := clBtnShadow;
      
  else
    Result := clBtnFace;
  end;
end;


procedure TRzGroup.SetSelectionColor( Value: TColor );
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetSelectionFontColor( Value: TColor );
begin
  if FSelectionFontColor <> Value then
  begin
    FSelectionFontColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetSelectionShadowColor( Value: TColor );
begin
  if FSelectionShadowColor <> Value then
  begin
    FSelectionShadowColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetSelectionFrameColor( Value: TColor );
begin
  if FSelectionFrameColor <> Value then
  begin
    FSelectionFrameColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetHideAccelerators( Value: Boolean );
begin
  if FHideAccelerators <> Value then
  begin
    FHideAccelerators := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetShowItemSelection( Value: Boolean );
begin
  if FShowItemSelection <> Value then
  begin
    FShowItemSelection := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItemSelectionStyle( Value: TRzItemSelectionStyle );
begin
  if FItemSelectionStyle <> Value then
  begin
    FItemSelectionStyle := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItems( Value: TRzGroupItems );
begin
  FItems.Assign( Value );
end;


procedure TRzGroup.SetItemHeight( Value: Integer );
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    Reposition;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItemIndent( Value: Byte );
begin
  if FItemIndent <> Value then
  begin
    FItemIndent := Value;
    Reposition;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItemStyle( Value: TRzItemStyle );
begin
  if FItemStyle <> Value then
  begin
    FItemStyle := Value;
    Reposition;
    Invalidate;
  end;
end;


procedure TRzGroup.SetItemPopupMenu( Value: TPopupMenu );
begin
  if FItemPopupMenu <> Value then
  begin
    FItemPopupMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;



procedure TRzGroup.SetSmallImages( Value: TCustomImageList );
begin
  if FSmallImages <> nil then
    FSmallImages.UnRegisterChanges( FSmallImagesChangeLink );

  FSmallImages := Value;

  if FSmallImages <> nil then
  begin
    FSmallImages.RegisterChanges( FSmallImagesChangeLink );
    FSmallImages.FreeNotification( Self );
  end;
  Reposition;
  Invalidate;
end;


procedure TRzGroup.SetLargeImages( Value: TCustomImageList );
begin
  if FLargeImages <> nil then
    FLargeImages.UnRegisterChanges( FLargeImagesChangeLink );

  FLargeImages := Value;

  if FLargeImages <> nil then
  begin
    FLargeImages.RegisterChanges( FLargeImagesChangeLink );
    FLargeImages.FreeNotification( Self );
  end;
  Reposition;
  Invalidate;
end;


procedure TRzGroup.ImagesChange( Sender: TObject );
begin
  if Sender = SmallImages then
    Update
  else if Sender = LargeImages then
    Update;
end;


function TRzGroup.ClosedHeight: Integer;
begin
  Result := FCaptionHeight
end;


procedure TRzGroup.ToggleState;
begin
  Opened := not FOpened;
  UpdateObjectInspector( Self );
end;


procedure TRzGroup.Open;
begin
  SetOpened( True );
end;

procedure TRzGroup.Close;
begin
  SetOpened( False );
end;


procedure TRzGroup.SetOpened( Value: Boolean );
var
  I: Integer;
  AllowChange: Boolean;
begin
  if GroupBar = nil then
  begin
    FOpened := Value;
    Exit;
  end;

  AllowChange := True;

  AllowChange := True;
  if ( FOpened and Value ) or ( not FOpened and not Value ) then
  begin
    // No change in Open/Close state
    Exit;
  end
  else
  begin
    if FOpened then
    begin
      // Must be trying to close
      if Assigned( FOnCanClose ) then
        FOnCanClose( Self, AllowChange );
    end
    else
    begin
      // Must be trying to open
      if Assigned( FOnCanOpen ) then
        FOnCanOpen( Self, AllowChange );
    end;
  end;

  if not AllowChange then
    Exit;

  case FStyle of
    gbsCategoryView:
    begin
      if FCanClose and ( FOpened <> Value ) then
      begin
        FOpened := Value;

        if FOpened and Assigned( FOnOpen ) then
          FOnOpen( Self );

        if not FOpened and Assigned( FOnClose ) then
          FOnClose( Self );

        UpdateGroupBarLayout;

        if FGroupBar <> nil then
          FGroupBar.AdjustSize;
        if FOpened then
        begin
          for I := 0 to ControlCount - 1 do
            Controls[ I ].Invalidate;
        end;
        UpdateObjectInspector( Self );
      end;
    end;

    gbsTaskList:
      FOpened := True;

    gbsOutlook:
    begin
      if not FOpened and Value then
      begin
        FOpened := True;

        // Close all other groups
        if FGroupBar <> nil then
        begin
          for I := 0 to FGroupBar.GroupCount - 1 do
          begin
            if TRzGroup( FGroupBar.FGroups[ I ] ) <> Self then
              TRzGroup( FGroupBar.FGroups[ I ] ).FOpened := False;

            if Assigned( TRzGroup( FGroupBar.FGroups[ I ] ).FOnClose ) then
              TRzGroup( FGroupBar.FGroups[ I ] ).FOnClose( FGroupBar.FGroups[ I ] );
          end;
        end;

        if Assigned( FOnOpen ) then
          FOnOpen( Self );

        UpdateGroupBarLayout;
        UpdateObjectInspector( Self );
      end;
    end;

  end;
end; {= TRzGroup.SetOpened =}


procedure TRzGroup.SetOpenedHeight( Value: Integer );
begin
  if FOpenedHeight <> Value then
  begin
    FOpenedHeight := Value;
    if not ( csDesigning in ComponentState ) and
       not ( csLoading in ComponentState ) then
    begin
      // Each group will always have at least 2 controls (the scroll buttons)
      if ( Items.Count = 0 ) and ( ControlCount = 2 ) then
        FOpenedHeight := 50;
    end;
    if FOpened then
      Height := FOpenedHeight;
     // RequestAlign - called from SetHeight
  end;
end;


procedure TRzGroup.SetGroupBar( Value: TRzGroupBar );
begin
  if FGroupBar <> Value then
  begin
    if FGroupBar <> nil then
      FGroupBar.RemoveGroup( Self );
    Parent := Value;
    if Value <> nil then
      Value.AddGroup( Self );
  end;
end;


function TRzGroup.GetGroupIndex: Integer;
begin
  if FGroupBar <> nil then
    Result := FGroupBar.FGroups.IndexOf( Self )
  else
    Result := -1;
end;


procedure TRzGroup.SetGroupIndex( Value: Integer );
var
  MaxGroupIndex: Integer;
begin
  if FGroupBar <> nil then
  begin
    MaxGroupIndex := FGroupBar.FGroups.Count - 1;

    if Value > MaxGroupIndex then
      raise EListError.Create( Format( sRzGroupIndexError, [ Value, MaxGroupIndex ] ) );

    FGroupBar.FGroups.Move( GroupIndex, Value );
    FGroupBar.UpdateLayout( Self );
  end;
end;


procedure TRzGroup.SetStyle( Value: TRzGroupBarStyle );
var
  OldStyle: TRzGroupBarStyle;
begin
  OldStyle := FStyle;
  FStyle := Value;

  case FStyle of
    gbsCategoryView:
    begin
      if FColorDefault then
      begin
        inherited Color := DefaultColor( FStyle );
        ParentColor := False;
        FColorDefault := True;
      end;
      if FCaptionColorDefault then
        FCaptionColor := DefaultCaptionColor( gbsCategoryView );
      if not FFontChanged then
      begin
        Font.Color := clWindowText;
        FFontChanged := False;
      end;
      if not FCaptionFontChanged then
      begin
        FCaptionFont.OnChange := nil;
        FCaptionFont.Style := [ fsBold ];
        FCaptionFont.Color := clHighlight;
        FCaptionFontChanged := False;
        FCaptionFont.OnChange := CaptionFontChangeHandler;
      end;
      FDividerColor := clHighlight;
      if OldStyle <> gbsCategoryView then
        FDividerVisible := False;

      if OldStyle = gbsOutlook then
      begin
        FOpened := True;
        if ( FItems.Count = 0 ) and ( ControlCount = 2 ) then
        begin
          // Reset the OpenedHeight if there are no items in the group and there are no extra controls in the group
          // other than the scrolling buttons (for gbsOutlook style).
          OpenedHeight := 50;
        end;
      end
      else if OldStyle = gbsTaskList then
      begin
        FCanClose := True;
      end;
    end;

    gbsTaskList:
    begin
      if FColorDefault then
      begin
        inherited Color := DefaultColor( FStyle );
        ParentColor := True;
        FColorDefault := True;
      end;
      if FCaptionColorDefault then
        FCaptionColor := DefaultCaptionColor( gbsTaskList );
      if not FFontChanged then
      begin
        Font.Color := clWindowText;
        FFontChanged := False;
      end;
      if not FCaptionFontChanged then
      begin
        FCaptionFont.OnChange := nil;
        FCaptionFont.Style := [ fsBold ];
        FCaptionFont.Color := clWindowText;
        FCaptionFontChanged := False;
        FCaptionFont.OnChange := CaptionFontChangeHandler;
      end;

      if not ( csLoading in ComponentState ) then
      begin
        FDividerVisible := True;
        FDividerColor := clHighlight;
      end;

      FCanClose := False;
      if OldStyle = gbsOutlook then
      begin
        FOpened := True;
        if ( FItems.Count = 0 ) and ( ControlCount = 2 ) then
        begin
          // Reset the OpenedHeight if there are no items in the group and there are no extra controls in the group
          // other than the scrolling buttons (for gbsOutlook style).
          OpenedHeight := 50;
        end;
      end;
    end;

    gbsOutlook:
    begin
      if FColorDefault then
      begin
        inherited Color := DefaultColor( FStyle );
        ParentColor := False;
        FColorDefault := True;
      end;
      if FCaptionColorDefault then
        FCaptionColor := DefaultCaptionColor( gbsOutlook );
      if not FFontChanged then
      begin
        Font.Color := clWindowText;
        FFontChanged := False;
      end;
      if not FCaptionFontChanged then
      begin
        FCaptionFont.OnChange := nil;
        FCaptionFont.Style := [ ];
        FCaptionFont.Color := clWindowText;
        FCaptionFontChanged := False;
        FCaptionFont.OnChange := CaptionFontChangeHandler;
      end;

      FCanClose := True;
      if not ( csLoading in ComponentState ) then
      begin
        if GroupIndex > 0 then
          FOpened := False
        else if GroupIndex = 0 then
          FOpened := True;
      end;
    end;

  end; { case FStyle }
  UpdateScrollBtnVisibility;
end; {= TRzGroup.SetStyle =}


procedure TRzGroup.SetSpecial( Value: Boolean );
begin
  if FSpecial <> Value then
  begin
    FSpecial := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetDividerColor( Value: TColor );
begin
  if FDividerColor <> Value then
  begin
    FDividerColor := Value;
    Invalidate;
  end;
end;


procedure TRzGroup.SetDividerVisible( Value: Boolean );
begin
  if FDividerVisible <> Value then
  begin
    FDividerVisible := Value;
    Invalidate;
  end;
end;


function TRzGroup.CaptionRect: TRect;
begin
  Result := Rect( 0, 0, ClientWidth, FCaptionHeight );
end;


function TRzGroup.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzGroup.ItemDestroyed( Index: Integer );
begin
  if FItemIndex = Index then
    FItemIndex := -1
  else if FItemIndex > Index then
    Dec( FItemIndex );
end;


function TRzGroup.GetSelectedItem: TRzGroupItem;
begin
  if ( FItemIndex >= 0 ) and ( FItemIndex < FItems.Count ) then
    Result := FItems[ FItemIndex ]
  else
    Result := nil;
end;


procedure TRzGroup.SetItemIndex( Value: Integer );
var
  I: Integer;
begin
  if FItemIndex <> Value then
  begin
    if ( Value >= 0 ) and ( Value < FItems.Count ) then
    begin
      FItems[ Value ].Selected := True;
    end
    else if Value = -1 then
    begin
      for I := 0 to FItems.Count - 1 do
        FItems[ I ].Selected := False;
      FItemIndex := -1;
    end;
  end;
end;


procedure TRzGroup.WndProc( var Msg: TMessage );
var
  I, X, Y: Integer;
begin
  inherited;

  // This method is needed to set the FItemIndex value

  if ( Msg.Msg = wm_LButtonDown ) or ( Msg.Msg = wm_LButtonDblClk ) then
  begin
    if Enabled and ( FItems.Count > 0 ) and
       not ( csDesigning in ComponentState ) then
    begin
      // Check to see if user is clicking LEFT button on an enabled item at runtime

      X := TWMMouse( Msg ).XPos;
      Y := TWMMouse( Msg ).YPos;
      for I := FTopItem to FItems.Count - 1 do
      begin
        if FItems[ I ].Enabled and FItems[ I ].Visible and
           ( PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
             PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) ) ) then
        begin
          FItemIndex := I;
          Break;
        end;
      end;
    end;
  end
end;


procedure TRzGroup.DragOver( Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean );
var
  I, Idx: Integer;
begin
  inherited;
  if Accept then
  begin
    for I := 0 to Items.Count - 1 do
      Items[ I ].CaptionState := csNormal;
    Idx := ItemAtPos( Point( X, Y ) );
    if Idx <> -1 then
      Items[ Idx ].CaptionState := csHot;
    Invalidate;
//    Accept := Idx <> -1;
  end;
end;


procedure TRzGroup.DoEndDrag(Target: TObject; X, Y: Integer);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[ I ].CaptionState := csNormal;
    FItems[ I ].ClickingCaption := False;
  end;
  inherited;
end;


function TRzGroup.ItemAtPos( P: TPoint ): Integer;
var
  I: Integer;
begin
  Result := -1;

  if not PtInRect( ClientRect, P ) then
    Exit;

  for I := FTopItem to FItems.Count - 1 do
  begin
    if FItems[ I ].Visible and
       ( PtInRect( FItems[ I ].HotCaptionRect, P ) or
         PtInRect( FItems[ I ].HotImageRect, P ) ) then
    begin
      Result := I;
      Break;
    end;
  end;
end;


function TRzGroup.ItemsVisible: Integer;
var
  ItemH: Integer;
begin
  ItemH := CalculateHeight( ClientWidth ) div Items.Count;
  Result := ClientHeight div ItemH;
end;


procedure TRzGroup.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  I: Integer;
  IgnoreClick: Boolean;
begin
  inherited;

  if ( Button = mbLeft ) and Enabled and FCanClose and PtInRect( CaptionRect, Point( X, Y ) ) then
  begin
    // User pressed the left mouse button while over the caption bar
    if not ( csDesigning in ComponentState ) and ( GroupBar <> nil ) then
      GroupBar.SetFocus; // This is necessary to have mouse wheel work

    SetCapture( Handle );
    FCaptionState := csDown;
    Invalidate;
    FClickingCaption := True;
  end
  else if ( Button = mbLeft ) and Enabled and ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    // Check to see if user is clicking LEFT button on an enabled item at runtime

    for I := FTopItem to FItems.Count - 1 do
    begin
      if FIgnoreSelectedItemClick then
        IgnoreClick := FShowItemSelection and ( I = FItemIndex )
      else
        IgnoreClick := False;

      if FItems[ I ].Enabled and FItems[ I ].Visible and not IgnoreClick and
         ( Assigned( FItems[ I ].OnClick ) or ( FItems[ I ].Action <> nil ) ) and
         ( PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
           PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) ) ) then
      begin
        FItems[ I ].CaptionState := csDown;
        FItems[ I ].SetSelected( True );
        Invalidate;
        FItems[ I ].ClickingCaption := True;
        Break;
      end;
    end;
  end
  else if ( Button = mbRight ) and Enabled and ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    // Check to see if user is clicking RIGHT button on an enabled item at runtime

    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Enabled and FItems[ I ].Visible and
         ( PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
           PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) ) ) then
      begin
        FItems[ I ].ClickingCaption := True;
        Break;
      end;
    end;
  end
  else if ( Button = mbLeft ) and ( FItems.Count > 0 ) and ( csDesigning in ComponentState ) then
  begin
    // Check to see if user is clicking LEFT button on an item at design-time
    {$IFDEF USE_GROUP_DESIGNER}
    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Visible and
         ( PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
           PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) ) ) then
      begin
        // Setting the item to be selected causes it to be selected in the Object Inspector
        FItems[ I ].SetSelected( True );
        Break;
      end;
    end;
    {$ENDIF}
  end;
end;


procedure TRzGroup.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  CapState: TRzCaptionState;
  ItemStates: array of TRzCaptionState;
  ItemStatesChanged: Boolean;
  I, OverItem: Integer;
begin
  inherited;

  CapState := csNormal;
  ItemStatesChanged := False;

  if FCanClose and PtInRect( CaptionRect, Point( X, Y ) ) then
  begin
    if FClickingCaption then
      CapState := csDown
    else
      CapState := csHot;
  end
  else if ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    OverItem := -1;

    // Check to see if mouse is over one of the items
    SetLength( ItemStates, FItems.Count );
    for I := FTopItem to FItems.Count - 1 do
      ItemStates[ I ] := csNormal;

    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Visible and
         ( Assigned( FItems[ I ].OnClick ) or ( FItems[ I ].Action <> nil ) ) and
         ( PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
           PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) ) ) then
      begin
        if FItems[ I ].ClickingCaption then
          ItemStates[ I ] := csDown
        else
          ItemStates[ I ] := csHot;
        OverItem := I;
      end;
    end;

    // Did any of the ItemStates change?

    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Visible and ( ItemStates[ I ] <> FItems[ I ].CaptionState ) then
      begin
        ItemStatesChanged := True;
        FItems[ I ].CaptionState := ItemStates[ I ];
        Break;
      end;
    end;

    if OverItem <> -1 then
      MouseOverItem( FItems[ OverItem ] )
    else
      MouseOverItem( nil );
  end;

  if ( CapState <> FCaptionState ) or ItemStatesChanged then
  begin
    FCaptionState := CapState;
    Invalidate;
  end;
end;


procedure TRzGroup.DisplayItemPopupMenu( Index, X, Y: Integer );
begin
  FPopupItem := nil;

  if Assigned( FOnItemPopupMenu ) then
    FOnItemPopupMenu( Self, FItems[ Index ] );

  if FItemPopupMenu <> nil then
  begin
    FPopupItem := FItems[ Index ];
    FItemPopupMenu.PopupComponent := Self;
    FItemPopupMenu.Popup( X, Y );
  end;
end;


procedure TRzGroup.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  DoClick: Boolean;
  I: Integer;
  P: TPoint;
begin
  inherited;

  if FClickingCaption then
  begin
    FClickingCaption := False;
    DoClick := PtInRect( CaptionRect, Point( X, Y ) );

    FCaptionState := csHot;

    if DoClick and not ( FCaptionState = csDown ) then
      Repaint;

    if DoClick then
      ToggleState;
  end
  else if FItems.Count > 0 then
  begin
    if Button = mbLeft then
    begin
      // Check to see if user clicked one of the items
      for I := FTopItem to FItems.Count - 1 do
      begin
        if FItems[ I ].ClickingCaption then
        begin
          FItems[ I ].ClickingCaption := False;
          DoClick := PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
                     PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) );

          FItems[ I ].CaptionState := csHot;

          if DoClick and not ( FItems[ I ].CaptionState = csDown ) then
            Repaint;

          if DoClick then
          begin
            FItems[ I ].Click;

            if Style = gbsOutlook then
            begin
              if TopItem > FItems[ I ].Index then
                TopItem := FItems[ I ].Index
              else if FItems[ I ].Index + 1 > TopItem + ItemsVisible then
                TopItem := FItems[ I ].Index - ItemsVisible + 1;
            end;
          end;

          Break;
        end
      end;
    end
    else if Button = mbRight then
    begin
      // Check to see if user clicked one of the items
      for I := FTopItem to FItems.Count - 1 do
      begin
        if FItems[ I ].ClickingCaption then
        begin
          FItems[ I ].ClickingCaption := False;
          DoClick := PtInRect( FItems[ I ].HotCaptionRect, Point( X, Y ) ) or
                     PtInRect( FItems[ I ].HotImageRect, Point( X, Y ) );

          FItems[ I ].CaptionState := csNormal;

          if DoClick and not ( FItems[ I ].CaptionState = csDown ) then
            Repaint;

          if DoClick then
          begin
            P := ClientToScreen( Point( X, Y ) );
            DisplayItemPopupMenu( I, P.X, P.Y );
          end;
          Break;
        end
      end;
    end;
  end;
end; {= TRzGroup.MouseUp =}


procedure TRzGroup.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  Refresh;
end;


procedure TRzGroup.CMMouseLeave( var Msg: TMessage );
var
  I: Integer;
begin
  FCaptionState := csNormal;
  for I := 0 to FItems.Count - 1 do
    FItems[ I ].CaptionState := csNormal;
  MouseOverItem( nil );

  inherited;

  Refresh;
end;


procedure TRzGroup.MouseOverItem( Item: TRzGroupItem );
begin
  FOverItem := Item;
  if Assigned( FOnMouseOverItem ) then
    FOnMouseOverItem( Self, Item );
end;


procedure TRzGroup.DoContextPopup( MousePos: TPoint; var Handled: Boolean );
var
  I: Integer;
  ClickedOnItem: Boolean;
begin
  ClickedOnItem := False;
  for I := FTopItem to FItems.Count - 1 do
  begin
    if FItems[ I ].Visible and
       ( PtInRect( FItems[ I ].HotCaptionRect, Point( MousePos.X, MousePos.Y ) ) or
         PtInRect( FItems[ I ].HotImageRect, Point( MousePos.X, MousePos.Y ) ) ) then
    begin
      ClickedOnItem := True;
      Break;
    end;
  end;

  if ClickedOnItem then
    Handled := True  // This prevents a PopupMenu assigned to the GroupBar from being displayed.
  else
    inherited;
end;


procedure TRzGroup.CMGroupItemSelected( var Msg: TCMGroupItemMsg );
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[ I ].CMGroupItemSelected( Msg );
end;


procedure TRzGroup.CMDesignHitTest( var Msg: TCMDesignHitTest );
{$IFDEF USE_GROUP_DESIGNER}
var
  I: Integer;
{$ENDIF}
begin
  if ( FStyle <> gbsTaskList ) and PtInRect( CaptionRect, Point( Msg.XPos, Msg.YPos ) ) then
  begin
    if FStyle = gbsOutlook then
      Msg.Result := 1
    else if Msg.XPos >= Width - 20 then
      Msg.Result := 1;           // Only allow mouse event to go through if over the Open/Close circle
  end
  else
  begin
    {$IFDEF USE_GROUP_DESIGNER}
    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Visible and
         ( PtInRect( FItems[ I ].HotCaptionRect, Point( Msg.XPos, Msg.YPos ) ) or
           PtInRect( FItems[ I ].HotImageRect, Point( Msg.XPos, Msg.YPos ) ) ) then
      begin
        Msg.Result := 1;
        Break;
      end;
    end;
    {$ENDIF}
  end;
end;


procedure TRzGroup.CaptionFontChangeHandler( Sender: TObject );
begin
  FCaptionFontChanged := True;
  Invalidate;
end;


function TRzGroup.CaptionYOffset: Integer;
begin
  if CaptionImageHeight > CaptionHeight then
    Result := ( CaptionImageHeight - CaptionHeight ) + 4
  else
    Result := 0;
end;


function TRzGroup.HeaderHeight: Integer;
begin
  Result := FCaptionHeight + CaptionYOffset;
end;


procedure TRzGroup.ItemStaticFontChangeHandler( Sender: TObject );
begin
  FItemStaticFontChanged := True;
  Invalidate;
end;


procedure TRzGroup.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  FFontChanged := True;
  if not FCaptionFontChanged and ( FCaptionFont <> nil ) then
  begin
    FCaptionFont.Name := Self.Font.Name;
    FCaptionFont.Size := Self.Font.Size;
    FCaptionFont.Style := [ fsBold ];
    // Reset FCaptionFontChanged b/c internal handler has gotten called from above statements
    FCaptionFontChanged := False;
  end;
  if not FItemStaticFontChanged and ( FItemStaticFont <> nil ) then
  begin
    FItemStaticFont.Assign( Self.Font );
    // Reset FItemStaticFontChanged b/c internal handler has gotten called from above statements
    FItemStaticFontChanged := False;
  end;

  UpdateGroupBarLayout;
end;


procedure TRzGroup.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzGroup.CMHintShow( var Msg: TMessage );
var
  I: Integer;
begin
  inherited;

  if FCaptionState = csHot then
  begin
    // If CaptionState = csHot, then mouse is over caption. Change CursorRect
    // of HintInfo so that item hints can be displayed.
    TCMHintShow( Msg ).HintInfo^.CursorRect := CaptionRect;
    Msg.Result := 0;
  end
  else
  begin

    // Not over the group caption.  Scan through all items and see if any
    // of them are csHot (i.e. the mouse is over them). If so, then set
    // HintInfo^.HintStr and CursorRect
    Msg.Result := 1;

    for I := 0 to FItems.Count - 1 do
    begin
      if FItems[ I ].CaptionState = csHot then
      begin
        TCMHintShow( Msg ).HintInfo^.HintStr := GetShortHint( FItems[ I ].Hint );

        if not IsRectEmpty( FItems[ I ].FHotImageRect ) then
        begin
          // Set CursorRect to union of HotCaptionRect and HotImageRect to that
          // hints will be displayed when mouse is over image as well as caption.
          UnionRect( TCMHintShow( Msg ).HintInfo^.CursorRect,
                     FItems[ I ].FHotCaptionRect, FItems[ I ].FHotImageRect );
        end
        else
          TCMHintShow( Msg ).HintInfo^.CursorRect := FItems[ I ].FHotCaptionRect;

        Msg.Result := 0;
        Break;
      end;
    end;
  end;
end; {= TRzGroup.CMHintShow =}


function TRzGroup.CaptionImageHeight: Integer;
begin
  if ( CaptionImageList <> nil ) and ( CaptionImageIndex > -1 ) then
    Result := CaptionImageList.Height
  else
    Result := 0;
end;


function TRzGroup.CaptionImageList: TCustomImageList;
begin
  if ( FCaptionStyle = csSmall ) then
    Result := SmallImageList
  else  //isLarge
    Result := LargeImageList;
end;


function TRzGroup.DrawCaptionImage: Integer;
var
  XOffset: Integer;
  YOffset: Integer;
begin
  // Draw Caption Image
  XOffset := 8;
  if ( CaptionImageList <> nil ) and ( FCaptionImageIndex <> -1 ) then
  begin
    YOffset := ( ( HeaderHeight - CaptionImageHeight ) div 2 ) - CaptionYOffset;
    if not UseRightToLeftAlignment then
      CaptionImageList.Draw( Canvas, XOffset, YOffset, FCaptionImageIndex, Enabled )
    else
      CaptionImageList.Draw( Canvas, Width - XOffset - CaptionImageList.Width, YOffset, FCaptionImageIndex, Enabled );
    XOffset := 8 + CaptionImageList.Width + 4;
  end;
  Result := XOffset;
end;


procedure TRzGroup.DrawOutOfBounds;
var
  XOffset: Integer;
  YOffset: Integer;
begin
  // Do not draw caption if not in view
  if Top + CaptionHeight < 0 then
    Exit
  else if ( Top - CaptionYOffset ) > GroupBar.Height then
    Exit;

  // Draw Caption Image
  XOffset := Left + 8;
  if ( CaptionImageList <> nil ) and ( FCaptionImageIndex <> -1 ) then
  begin
    YOffset := Top - CaptionYOffset + ( ( HeaderHeight - CaptionImageHeight ) div 2 );
    if not UseRightToLeftAlignment then
      CaptionImageList.Draw( GroupBar.Canvas, XOffset, YOffset, FCaptionImageIndex, Enabled )
    else
      CaptionImageList.Draw( GroupBar.Canvas, Width - XOffset - CaptionImageList.Width, YOffset, FCaptionImageIndex, Enabled );
  end;
end;


procedure TRzGroup.InvalidateGroupBar;
begin
  if GroupBar <> nil then
    GroupBar.Invalidate;
end;


procedure TRzGroup.SetCaptionStyle( const Value: TRzCaptionStyle );
begin
  if FCaptionStyle <> Value then
  begin
    FCaptionStyle := Value;
    Reposition;
    InvalidateGroupBar;
    Invalidate;
  end;
end;


procedure TRzGroup.WMSetCursor( var Msg: TWMSetCursor );
var
  I: Integer;
  OverItem: Boolean;
begin
  // If mouse is over caption, then use custom cursor
  if PtInRect( CaptionRect, CursorPosition ) and ( FStyle <> gbsTaskList ) then
  begin
    if FStyle = gbsOutlook then
      SetCursor( FCaptionCursor )
    else if ( FStyle = gbsCategoryView ) and FCanClose then
    begin
      if csDesigning in ComponentState then
      begin
        if CursorPosition.X >= Width - 20 then
          SetCursor( FCaptionCursor )
        else
          inherited;
      end
      else
        SetCursor( FCaptionCursor );
    end
    else
      inherited;
  end
  else if ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    // Test if mouse is over one of the items
    OverItem := False;
    for I := FTopItem to FItems.Count - 1 do
    begin
      if FItems[ I ].Enabled and FItems[ I ].Visible and Assigned( FItems[ I ].OnClick ) and
         ( PtInRect( FItems[ I ].HotCaptionRect, CursorPosition ) or
           PtInRect( FItems[ I ].HotImageRect, CursorPosition ) ) then
      begin
        OverItem := True;
        Break;
      end;
    end;
    if OverItem then
      SetCursor( FCaptionCursor )
    else
      inherited;
  end
  else
    inherited;
end; {= TRzGroup.WMSetCursor =}


procedure TRzGroup.WMSize( var Msg: TWMSize );
begin
  if csDesigning in ComponentState then
  begin
    // If user resizes the group at design-time, then automatically adjust
    // the OpenHeight property
    if FOpened and Visible then
      OpenedHeight := Height;
  end;
  inherited;
end;


procedure TRzGroup.WMWindowPosChanged( var Msg: TWMWindowPosChanged );
begin
  inherited;
  if ( FGroupBar <> nil ) and not FGroupBar.FUpdatingLayout then
    UpdateGroupBarLayout;
end;


procedure TRzGroup.AddToGroup( Template: TRzGroupTemplate );
var
  I: Integer;
  NewItem: TRzGroupItem;
begin
  for I := 0 to Template.Items.Count - 1 do
  begin
    NewItem := Items.Add;
    NewItem.Assign( Template.Items[ I ] );
  end;
end;


{============================================}
{== TRzGroupTemplateItemActionLink Methods ==}
{============================================}

procedure TRzGroupTemplateItemActionLink.AssignClient( AClient: TObject );
begin
  FClient := AClient as TRzGroupTemplateItem;
end;


function TRzGroupTemplateItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and ( FClient.Caption = ( Action as TCustomAction ).Caption );
end;


(*
function TRzGroupTemplateItemActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and FClient.Template.ShowItemSelection and
            ( FClient.Selected = ( Action as TCustomAction ).Checked );
end;
*)

function TRzGroupTemplateItemActionLink.IsEnabledLinked: Boolean;
begin
  Result := inherited IsEnabledLinked and ( FClient.Enabled = ( Action as TCustomAction ).Enabled );
end;


function TRzGroupTemplateItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and ( FClient.Hint = ( Action as TCustomAction ).Hint );
end;


function TRzGroupTemplateItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and ( FClient.ImageIndex = ( Action as TCustomAction ).ImageIndex );
end;


function TRzGroupTemplateItemActionLink.IsVisibleLinked: Boolean;
begin
  Result := inherited IsVisibleLinked and ( FClient.Visible = ( Action as TCustomAction ).Visible );
end;


function TRzGroupTemplateItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and ( @FClient.OnClick = @Action.OnExecute );
end;


procedure TRzGroupTemplateItemActionLink.SetCaption( const Value: string );
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;


(*
procedure TRzGroupTemplateItemActionLink.SetChecked( Value: Boolean );
begin
  if IsCheckedLinked then
    FClient.Selected := Value;
end;
*)

procedure TRzGroupTemplateItemActionLink.SetEnabled( Value: Boolean );
begin
  if IsEnabledLinked then
    FClient.Enabled := Value;
end;


procedure TRzGroupTemplateItemActionLink.SetHint( const Value: string );
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;


procedure TRzGroupTemplateItemActionLink.SetImageIndex( Value: Integer );
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;


procedure TRzGroupTemplateItemActionLink.SetVisible( Value: Boolean );
begin
  if IsVisibleLinked then
    FClient.Visible := Value;
end;


procedure TRzGroupTemplateItemActionLink.SetOnExecute( Value: TNotifyEvent );
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;



{==========================}
{== TRzGroupItem Methods ==}
{==========================}

constructor TRzGroupTemplateItem.Create( Collection: TCollection );
begin
  inherited;

  if ( csDesigning in Template.ComponentState ) and not ( csLoading in Template.ComponentState ) then
    FCaption := 'Item' + IntToStr( Index + 1 );

  FEnabled := True;
  FFontColor := clNone;
  FFontStyle := [];
  FImageIndex := -1;
  FDisabledIndex := -1;
  FVisible := True;
end;


destructor TRzGroupTemplateItem.Destroy;
begin
  FActionLink.Free;
  FActionLink := nil;
  inherited;
end;


procedure TRzGroupTemplateItem.Assign( Source: TPersistent );
begin
  if Source is TRzGroupTemplateItem then
  begin
    Action := TRzGroupTemplateItem( Source ).Action;
    Caption := TRzGroupTemplateItem( Source ).Caption;
    DisabledIndex := TRzGroupTemplateItem( Source ).DisabledIndex;
    Enabled := TRzGroupTemplateItem( Source ).Enabled;
    FontColor := TRzGroupTemplateItem( Source ).FontColor;
    FontStyle := TRzGroupTemplateItem( Source ).FontStyle;
    Hint := TRzGroupTemplateItem( Source ).Hint;
    ImageIndex := TRzGroupTemplateItem( Source ).ImageIndex;
    Visible := TRzGroupTemplateItem( Source ).Visible;
    OnClick := TRzGroupTemplateItem( Source ).OnClick;
  end
  else
    inherited;
end;


procedure TRzGroupTemplateItem.AssignTo( Dest: TPersistent );
begin
  if Dest is TCustomAction then
  begin
    TCustomAction( Dest ).Caption := Self.Caption;
    TCustomAction( Dest ).Enabled := Self.Enabled;
    TCustomAction( Dest ).Hint := Self.Hint;
    TCustomAction( Dest ).ImageIndex := Self.ImageIndex;
    TCustomAction( Dest ).Visible := Self.Visible;
    TCustomAction( Dest ).OnExecute := Self.OnClick;
  end
  else if Dest is TRzGroupItem then
  begin
    TRzGroupItem( Dest ).Action := Self.Action;
    TRzGroupItem( Dest ).Caption := Self.Caption;
    TRzGroupItem( Dest ).DisabledIndex := Self.DisabledIndex;
    TRzGroupItem( Dest ).Enabled := Self.Enabled;
    TRzGroupItem( Dest ).FontColor := Self.FontColor;
    TRzGroupItem( Dest ).FontStyle := Self.FontStyle;
    TRzGroupItem( Dest ).Hint := Self.Hint;
    TRzGroupItem( Dest ).ImageIndex := Self.ImageIndex;
    TRzGroupItem( Dest ).IndentLevel := Self.IndentLevel;
    TRzGroupItem( Dest ).Tag := Self.Tag;
    TRzGroupItem( Dest ).Visible := Self.Visible;
    TRzGroupItem( Dest ).OnClick := Self.OnClick;
  end
  else
    inherited;
end;



function TRzGroupTemplateItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;


function TRzGroupTemplateItem.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action
  else
    Result := nil;
end;


procedure TRzGroupTemplateItem.SetAction( Value: TBasicAction );
begin
  if Value = nil then
  begin
    ActionLink.Free;
    ActionLink := nil;
  end
  else
  begin
    if ActionLink = nil then
      ActionLink := GetActionLinkClass.Create( Self );
    ActionLink.Action := Value;
    ActionLink.OnChange := ActionChangeHandler;
    ActionChange( Value, csLoading in Value.ComponentState );

    Value.FreeNotification( Template );
  end;
end;


procedure TRzGroupTemplateItem.ActionChange( Sender: TObject; CheckDefaults: Boolean );
var
  NewAction: TCustomAction;
begin
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction( Sender );

    if not CheckDefaults or ( Self.Caption = '' ) or not FCaptionChanged then
      Self.Caption := NewAction.Caption;

    if not CheckDefaults or ( Self.Enabled = True ) then
      Self.Enabled := NewAction.Enabled;

    if not CheckDefaults or ( Self.Hint = '' ) then
      Self.Hint := NewAction.Hint;

    if not CheckDefaults or ( Self.ImageIndex = -1 ) then
      Self.ImageIndex := NewAction.ImageIndex;

    if not CheckDefaults or ( Self.Visible = True ) then
      Self.Visible := NewAction.Visible;

    if not CheckDefaults or not Assigned( Self.OnClick ) then
      Self.OnClick := NewAction.OnExecute;
  end;
end;


procedure TRzGroupTemplateItem.ActionChangeHandler( Sender: TObject );
begin
  if Sender = Action then
    ActionChange( Sender, False );
end;


function TRzGroupTemplateItem.GetActionLinkClass: TRzGroupTemplateItemActionLinkClass;
begin
  Result := TRzGroupTemplateItemActionLink;
end;


procedure TRzGroupTemplateItem.InitiateAction;
begin
  if ActionLink <> nil then
    ActionLink.Update;
end;


function TRzGroupTemplateItem.IsCaptionStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsCaptionLinked;
end;


function TRzGroupTemplateItem.IsEnabledStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsEnabledLinked;
end;


function TRzGroupTemplateItem.IsHintStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsHintLinked;
end;


function TRzGroupTemplateItem.IsHelpContextStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsHelpContextLinked;
end;


function TRzGroupTemplateItem.IsImageIndexStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsImageIndexLinked;
end;


function TRzGroupTemplateItem.IsVisibleStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsVisibleLinked;
end;


function TRzGroupTemplateItem.IsOnClickStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsOnExecuteLinked;
end;


procedure TRzGroupTemplateItem.SetCaption( const Value: TCaption );
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FCaptionChanged := True;
    // Changed causes TRzGroupItems.Update to be called.  Passing True is interpreted that ALL items have been changed.
    // For this component, this is sufficient.
    Changed( True );
  end;
end;


procedure TRzGroupTemplateItem.SetEnabled( Value: Boolean );
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;


procedure TRzGroupTemplateItem.SetFontColor( Value: TColor );
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;


procedure TRzGroupTemplateItem.SetFontStyle( Value: TFontStyles );
begin
  if FFontStyle <> Value then
  begin
    FFontStyle := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;


function TRzGroupTemplateItem.GetTemplate: TRzGroupTemplate;
begin
  Result := TRzGroupTemplateItems( Collection ).Template;
end;


procedure TRzGroupTemplateItem.SetDisabledIndex( Value: TImageIndex );
begin
  if FDisabledIndex <> Value then
  begin
    FDisabledIndex := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;


procedure TRzGroupTemplateItem.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;


procedure TRzGroupTemplateItem.SetIndentLevel( Value: Byte );
begin
  if FIndentLevel <> Value then
  begin
    FIndentLevel := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;


procedure TRzGroupTemplateItem.SetVisible( Value: Boolean );
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed( True );                                       // Causes TRzGroupTemplateItems.Update to be called
  end;
end;



{===================================}
{== TRzGroupTemplateItems Methods ==}
{===================================}

constructor TRzGroupTemplateItems.Create( Template: TRzGroupTemplate );
begin
  // Inherited constructor is passed the "type" of the collection
  // item that the collection will manage.
  inherited Create( TRzGroupTemplateItem );
  FTemplate := Template;
end;


function TRzGroupTemplateItems.Add: TRzGroupTemplateItem;
begin
  Result := TRzGroupTemplateItem( inherited Add );
end;


function TRzGroupTemplateItems.GetItem( Index: Integer ): TRzGroupTemplateItem;
begin
  Result := TRzGroupTemplateItem( inherited GetItem( Index ) );
end;


procedure TRzGroupTemplateItems.SetItem( Index: Integer; Value: TRzGroupTemplateItem );
begin
  // Must specify SetItem b/c SetItem is not virtual
  inherited SetItem( Index, Value );
end;


function TRzGroupTemplateItems.GetOwner: TPersistent;
begin
  Result := FTemplate;
end;



{============================================}
{== TRzGroupTemplatePreviewOptions Methods ==}
{============================================}

constructor TRzGroupTemplatePreviewOptions.Create( ATemplate: TRzGroupTemplate );
begin
  inherited Create;

  FTemplate := ATemplate;
  FItemStyle := isSmall;
  FSmallImagesChangeLink := TChangeLink.Create;
  FLargeImagesChangeLink := TChangeLink.Create;
end;


destructor TRzGroupTemplatePreviewOptions.Destroy;
begin
  FSmallImagesChangeLink.Free;
  FLargeImagesChangeLink.Free;
  inherited;
end;


procedure TRzGroupTemplatePreviewOptions.SetSmallImages( Value: TCustomImageList );
begin
  if FSmallImages <> nil then
    FSmallImages.UnRegisterChanges( FSmallImagesChangeLink );

  FSmallImages := Value;

  if FSmallImages <> nil then
  begin
    FSmallImages.RegisterChanges( FSmallImagesChangeLink );
    FSmallImages.FreeNotification( FTemplate );
  end;
end;


procedure TRzGroupTemplatePreviewOptions.SetLargeImages( Value: TCustomImageList );
begin
  if FLargeImages <> nil then
    FLargeImages.UnRegisterChanges( FLargeImagesChangeLink );

  FLargeImages := Value;

  if FLargeImages <> nil then
  begin
    FLargeImages.RegisterChanges( FLargeImagesChangeLink );
    FLargeImages.FreeNotification( FTemplate );
  end;
end;



{==============================}
{== TRzGroupTemplate Methods ==}
{==============================}

constructor TRzGroupTemplate.Create( AOwner: TComponent );
begin
  inherited;
  FCanClose := True;
  FSpecial := False;
  FOpened := True;
  FCaptionImageIndex := -1;

  FPreviewOptions := TRzGroupTemplatePreviewOptions.Create( Self );
  {&RCI}
  FItems := TRzGroupTemplateItems.Create( Self );
end; {= TRzGroupTemplate.Create =}


destructor TRzGroupTemplate.Destroy;
begin
  FItems.Free;
  FPreviewOptions.Free;
  inherited;
end;


procedure TRzGroupTemplate.Loaded;
var
  I: Integer;
begin
  inherited;

  for I := 0  to FItems.Count - 1 do
  begin
    if FItems[ I ].Action <> nil then
      FItems[ I ].ActionChange( FItems[ I ].Action, True );
  end;
end;



procedure TRzGroupTemplate.Notification( AComponent: TComponent; Operation: TOperation );
var
  I: Integer;
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FPreviewOptions.SmallImages then
      FPreviewOptions.SmallImages := nil
    else if AComponent = FPreviewOptions.LargeImages then
      FPreviewOptions.LargeImages := nil
    else if AComponent = FPopupMenu then
      FPopupMenu := nil
    else if AComponent = FItemPopupMenu then
      FItemPopupMenu := nil
    else if AComponent is TBasicAction then
    begin
      for I := 0 to FItems.Count - 1 do
      begin
        if AComponent = FItems[ I ].Action then
          FItems[ I ].Action := nil;
      end;
    end;
  end;
end;


procedure TRzGroupTemplate.AssignActionList( ActionList: TCustomActionList; const Category: string = '' );
var
  I: Integer;
  Item: TRzGroupTemplateItem;
begin
  if ActionList <> nil then
  begin
    FItems.Clear;

    for I := 0 to ActionList.ActionCount - 1 do
    begin
      if ( Category = '' ) or ( UpperCase( ActionList.Actions[ I ].Category ) = UpperCase( Category ) ) then
      begin
        Item := FItems.Add;
        Item.Action := ActionList.Actions[ I ];
      end;
    end;
  end;
end;


procedure TRzGroupTemplate.SetItems( Value: TRzGroupTemplateItems );
begin
  FItems.Assign( Value );
end;


procedure TRzGroupTemplate.SetItemPopupMenu( Value: TPopupMenu );
begin
  if FItemPopupMenu <> Value then
  begin
    FItemPopupMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzGroupTemplate.SetPopupMenu( Value: TPopupMenu );
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzGroupTemplate.SetPreviewOptions( Value: TRzGroupTemplatePreviewOptions );
begin
  FPreviewOptions.Assign( Value );
end;



{=========================}
{== TRzGroupBar Methods ==}
{=========================}

{$IFDEF VCL160_OR_HIGHER}
class constructor TRzGroupBar.Create;
begin
  TCustomStyleEngine.RegisterStyleHook( TRzGroupBar, TScrollingStyleHook );
end;


class destructor TRzGroupBar.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook( TRzGroupBar, TScrollingStyleHook );
end;
{$ENDIF}


constructor TRzGroupBar.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csSetCaption, csAcceptsControls ];

  FUpdateLayoutCount := 0;
  FGroups := TList.Create;

  Width := 160;
  Align := alLeft;

  FBevelWidth := 1;
  FBorderSides := [ sdLeft, sdTop, sdRight, sdBottom ];
  FBorderColor := clBtnFace;
  FBorderHighlight := clBtnHighlight;
  FBorderShadow := clBtnShadow;
  FBorderInner := fsNone;
  FBorderOuter := fsNone;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;

  FGroupBorderSize := 8;
  FStyle := gbsCategoryView;
  FExclusiveMode := False;

  ParentColor := False;
  Color := clBtnShadow;
  FVisualStyle := vsGradient;
  FGradientColorStyle := gcsSystem;
  FGradientColorStart := clBtnFace;
  FGradientColorStop := clBtnShadow;

  {&RCI}

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FSmallImagesChangeLink := TChangeLink.Create;
  FSmallImagesChangeLink.OnChange := ImagesChange;
  FLargeImagesChangeLink := TChangeLink.Create;
  FLargeImagesChangeLink.OnChange := ImagesChange;

  DoubleBuffered := True;

  if not Registered then
  begin
    Classes.RegisterClasses( [ TRzGroup ] );
    Registered := True;
  end;
end; {= TRzGroupBar.Create =}


function TRzGroupBar.GroupClass: TRzGroupClass;
begin
  Result := TRzGroup;
end;


procedure TRzGroupBar.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or ws_ClipChildren or ws_VScroll;
  Params.ExStyle := Params.ExStyle or ws_ex_ControlParent;

end;


procedure TRzGroupBar.CreateWnd;
begin
  inherited;
  UpdateScrollBar;
end;


destructor TRzGroupBar.Destroy;
var
  I: Integer;
begin
  FCanvas.Free;

  for I := 0 to FGroups.Count - 1 do
    TRzGroup( FGroups[ I ] ).FGroupBar := nil;
  FGroups.Free;

  FSmallImagesChangeLink.Free;
  FLargeImagesChangeLink.Free;

  inherited;
end;


procedure TRzGroupBar.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Handle the fact that the ColorAdjustment, ThemeAware, and UseGradients
  // properties were published in verison 3.x
  Filer.DefineProperty( 'ColorAdjustment',
                        TRzOldPropReader.ReadOldIntegerProp, nil, False );

  Filer.DefineProperty( 'ThemeAware',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );

  Filer.DefineProperty( 'UseGradients',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzGroupBar.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FSmallImages then
      SetSmallImages( nil )
    else if AComponent = FLargeImages then
      SetLargeImages( nil );
  end;
end;


procedure TRzGroupBar.Loaded;
begin
  inherited;

  Resize;
  UpdateLayout;

  PostMessage( Handle, um_ResetScrollPosition, 0, 0 );
end;


procedure TRzGroupBar.GetChildren( Proc: TGetChildProc; Root: TComponent );
var
  I: Integer;
begin
  for I := 0 to FGroups.Count - 1 do
    Proc( TComponent( FGroups[ I ] ) );
end;


procedure TRzGroupBar.SetChildOrder( Child: TComponent; Order: Integer );
begin
  TRzGroup( Child ).GroupIndex := Order;
end;


procedure TRzGroupBar.SetParent( AParent: TWinControl );
begin
  inherited SetParent( AParent );
  // After reparenting the control, reset the ScrollPosition because
  // if there are lots of groups, the scroll bar will be positioned
  // at the end.
  if AParent <> nil then
    ScrollPosition := 0;
end;


function TRzGroupBar.CreateGroupFromTemplate( Template: TRzGroupTemplate ): TRzGroup;
begin
  Result := TRzGroup.Create( Self );

  Result.Caption := Template.Caption;
  Result.CaptionImageIndex := Template.CaptionImageIndex;
  Result.CaptionStyle := Template.CaptionStyle;
  Result.Opened := Template.Opened;
  Result.CanClose := Template.CanClose;
  Result.Special := Template.Special;
  Result.Tag := Template.Tag;
  Result.PopupMenu := Template.PopupMenu;
  Result.ItemPopupMenu := Template.ItemPopupMenu;
  Result.OnItemPopupMenu := Template.OnItemPopupMenu;

  Result.Items.Assign( Template.Items );
end;


procedure TRzGroupBar.InsertGroup( Index: Integer; Group: TRzGroup );
begin
  FGroups.Insert( Index, Group );
  Group.Parent := Self;
  Group.FGroupBar := Self;
  Group.Style := Style;
  UpdateLayout( Group );
  if csDesigning in ComponentState then
    Invalidate;
end;


function TRzGroupBar.InsertGroup( Index: Integer; Template: TRzGroupTemplate ): TRzGroup;
begin
  Result := CreateGroupFromTemplate( Template );
  InsertGroup( Index, Result );
end;


procedure TRzGroupBar.AddGroup( Group: TRzGroup );
begin
  FGroups.Add( Group );
  Group.Parent := Self;
  Group.FGroupBar := Self;
  Group.Style := Style;

  UpdateLayout( Group );

  if csDesigning in ComponentState then
    Invalidate;
end;


function TRzGroupBar.AddGroup( Template: TRzGroupTemplate ): TRzGroup;
begin
  Result := CreateGroupFromTemplate( Template );
  AddGroup( Result );
end;


procedure TRzGroupBar.RemoveGroup( Group: TRzGroup );
var
  I, GroupIdx, ActiveIdx: Integer;
begin
  // Need to ensure that one group remains open in gbsOutlook style or in
  // gbsCategoryView if ExclusiveMode = True
  GroupIdx := -1;
  ActiveIdx := -1;
  if ( FStyle = gbsOutlook ) or ( ( FStyle = gbsCategoryView ) and FExclusiveMode ) then
  begin
    GroupIdx := Group.GroupIndex;
    for I := 0 to GroupCount - 1 do
    begin
      if Groups[ I ].Opened then
      begin
        ActiveIdx := I;
        Break;
      end;
    end;
  end;

  Group.FGroupBar := nil;
  if not ( csDesigning in ComponentState ) then
  begin
    Group.Visible := False;
    Group.Parent := nil;
  end;

  FGroups.Remove( Group );

  if ( FStyle = gbsOutlook ) or ( ( FStyle = gbsCategoryView ) and FExclusiveMode ) then
  begin
    if GroupIdx = ActiveIdx then
    begin
      if GroupIdx > 0 then
        Groups[ GroupIdx - 1 ].Open
      else if FGroups.Count > 0 then
        Groups[ 0 ].Open;
    end;
  end;

  UpdateLayout;
  if csDesigning in ComponentState then
    Invalidate;
end; {= TRzGroupBar.RemoveGroup =}


procedure TRzGroupBar.CloseAllGroups;
var
  I: Integer;
begin
  if FStyle <> gbsCategoryView then
    Exit;

  for I := 0 to GroupCount - 1 do
    Groups[ I ].Close;
end;


procedure TRzGroupBar.OpenAllGroups;
var
  I: Integer;
begin
  if FStyle <> gbsCategoryView then
    Exit;

  for I := 0 to GroupCount - 1 do
    Groups[ I ].Open;
end;


function TRzGroupBar.GetGroup( Index: Integer ): TRzGroup;
begin
  Result := FGroups[ Index ];
end;


function TRzGroupBar.GetGroupCount: Integer;
begin
  Result := FGroups.Count;
end;


procedure TRzGroupBar.PositionGroups;
var
  I, T, H: Integer;
  Group: TRzGroup;
  OpenedGroup, TotalClosedHeight: Integer;
begin
  case FStyle of

    gbsCategoryView, gbsTaskList:
    begin
      for I := 0 to FGroups.Count - 1 do
      begin
        Group := TRzGroup( FGroups[ I ] );
        if I > 0 then
          T := TRzGroup( FGroups[ I - 1 ] ).BoundsRect.Bottom
        else
          T := -ScrollPosition;

        if Group.Visible or ( csDesigning in ComponentState ) then
        begin
          if Group.Opened or ( FStyle = gbsTaskList ) then
          begin
            if Group.Items.Count > 0 then
              H := Group.CalculateHeight( ClientWidth - 2 * GroupBorderSize )
            else
              H := Group.OpenedHeight;
          end
          else
            H := Group.ClosedHeight;


          Group.SetBounds( GroupBorderSize, T + GroupBorderSize + Group.CaptionYOffset,
                           ClientWidth - 2 * GroupBorderSize, H );
        end
        else
        begin
          // Group not visible
          Group.SetBounds( GroupBorderSize, T, ClientWidth - 2 * GroupBorderSize, 0 );
        end;
      end;

      UpdateScrollBar;
    end;

    gbsOutlook:
    begin
      if GroupCount > 0 then
      begin
        OpenedGroup := 0;
        TotalClosedHeight := 0;
        for I := 0 to GroupCount - 1 do
        begin
          if TRzGroup( FGroups[ I ] ).Opened then
            OpenedGroup := I
          else if TRzGroup( FGroups[ I ] ).Visible then
            Inc( TotalClosedHeight, TRzGroup( FGroups[ I ] ).ClosedHeight + GroupBorderSize );
        end;
        Inc( TotalClosedHeight, 2 * GroupBorderSize );

        // Handle Closed Groups above the Opened Group
        for I := 0 to OpenedGroup - 1 do
        begin
          Group := TRzGroup( FGroups[ I ] );
          if I > 0 then
            T := TRzGroup( FGroups[ I - 1 ] ).BoundsRect.Bottom
          else
            T := 0;
          if Group.Visible or ( csDesigning in ComponentState ) then
            Group.SetBounds( GroupBorderSize, T + GroupBorderSize, ClientWidth - 2 * GroupBorderSize, Group.ClosedHeight )
          else
            Group.SetBounds( GroupBorderSize, T, ClientWidth - 2 * GroupBorderSize, 0 );
        end;

        // Handle Open Group
        if OpenedGroup > 0 then
          T := TRzGroup( FGroups[ OpenedGroup - 1 ] ).BoundsRect.Bottom
        else
          T := 0;
        Group := TRzGroup( FGroups[ OpenedGroup ] );
        Group.SetBounds( GroupBorderSize, T + GroupBorderSize, ClientWidth - 2 * GroupBorderSize, ClientHeight - TotalClosedHeight );

        // Handle Closed Groups below the Opened Group

        for I := OpenedGroup + 1 to GroupCount - 1 do
        begin
          T := TRzGroup( FGroups[ I - 1 ] ).BoundsRect.Bottom;
          Group := TRzGroup( FGroups[ I ] );
          if Group.Visible then
            Group.SetBounds( GroupBorderSize, T + GroupBorderSize, ClientWidth - 2 * GroupBorderSize, Group.ClosedHeight )
          else
            Group.SetBounds( GroupBorderSize, T, ClientWidth - 2 * GroupBorderSize, 0 );
        end;
      end;
    end;

  end; { case FStyle }
end; {= TRzGroupBar.PositionGroups =}


procedure TRzGroupBar.UpdateLayout( Group: TRzGroup = nil );
var
  I: Integer;
  V: Boolean;

  procedure RemoveGapAfterLastGroup;
  begin
    if ( TRzGroup( FGroups[ FGroups.Count - 1 ] ).BoundsRect.Bottom + GroupBorderSize < ClientHeight ) and
       ( TRzGroup( FGroups[ 0 ] ).Top < 0 ) then
    begin
      //   +---------+    <-- Top of first Group is < 0
      // ==|    0    |==  <-- Top of View
      //   +---------+
      //       :
      //   +---------+
      //   |    N    |
      //   +---------+    <-- Bottom of last Group is < Height of View
      //
      // ===============  <-- Height of View

      if ClientHeight - TotalHeight > 0 then
      begin
        // View can accommodate all groups--scroll to top Group
        ScrollControls( -TRzGroup( FGroups[ 0 ] ).Top + GroupBorderSize );
      end
      else
      begin
        // Remove gap below last group
        ScrollControls( ClientHeight - ( TotalHeight - FScrollPosition ) );
      end;
    end;
  end; {= RemoveGapAfterLastGroup =}

begin {= TRzGroupBar.UpdateLayout =}
  if ( FUpdateLayoutCount > 0 ) or ( csLoading in ComponentState ) then
    Exit;

  FUpdatingLayout := True;
  try
    case FStyle of

      gbsCategoryView, gbsTaskList:
      begin

        if ( FStyle = gbsCategoryView ) and ( Group <> nil ) and
           Group.FOpened and FExclusiveMode then
        begin
          // Make sure only current Group is opened
          for I := 0 to FGroups.Count - 1 do
          begin
            if TRzGroup( FGroups[ I ] ) <> Group then
              TRzGroup( FGroups[ I ] ).Close;
          end;
        end;

        repeat
          V := IsScrollBarVisible;
          if FGroups.Count > 0 then
          begin
            PositionGroups;
            RemoveGapAfterLastGroup;
            ShowEntireGroup( Group );
          end;
          UpdateScrollBar;
        until V = IsScrollBarVisible;
      end;

      gbsOutlook:
      begin
        UpdateScrollBar;
        if FGroups.Count > 0 then
          PositionGroups;
      end;

    end; { case FStyle }
  finally
    FUpdatingLayout := False;
  end;
end; {= TRzGroupBar.UpdateLayout =}


procedure TRzGroupBar.BeginUpdateLayout;
begin
  Inc( FUpdateLayoutCount );
end;


procedure TRzGroupBar.EndUpdateLayout;
begin
  Dec( FUpdateLayoutCount );
  if FUpdateLayoutCount <= 0 then
  begin
    FUpdateLayoutCount := 0;
    UpdateLayout;
  end;
end;


procedure TRzGroupBar.Resize;
begin
  inherited;
  UpdateLayout;
end;


function TRzGroupBar.GetControlExtents: TRect;
begin
  Result := Rect( 0, 0, Width, TotalHeight + 1 );
end;


function TRzGroupBar.GetTotalHeight: Integer;
begin
  case FStyle of
    gbsCategoryView, gbsTaskList:
    begin
      Result := 2 * GroupBorderSize;
      if FGroups.Count > 0 then
      begin
        Result := Result
                  + TRzGroup( FGroups[ FGroups.Count - 1 ] ).BoundsRect.Bottom
                  - TRzGroup( FGroups[ 0 ] ).Top
                  + TRzGroup( FGroups[ 0 ] ).CaptionYOffset;
      end;
    end;

    gbsOutlook:
      Result := ClientHeight - 1;
      
  else
    Result := 2 * GroupBorderSize;
  end;
end;


procedure TRzGroupBar.ShowEntireGroup( Group: TRzGroup );
begin
  if ( Group <> nil ) and Group.FOpened and not ( csLoading in Group.ComponentState ) then
  begin
    UpdateScrollBar;  // This ensures that the new scroll bar range is established correctly

    if Group.Height > ClientHeight then
    begin
      if Group.Top < 0 then
      begin
        ScrollControls( -Group.Top + GroupBorderSize );
      end
      else
      begin
        ScrollControls( ClientHeight - Group.BoundsRect.Bottom - GroupBorderSize );
        ScrollControls( -Group.Top + GroupBorderSize );
      end;
    end
    else if ( Group.BoundsRect.Bottom + GroupBorderSize > ClientHeight ) and ( Group.Top > 0 ) then
      ScrollControls( ClientHeight - Group.BoundsRect.Bottom - GroupBorderSize )
    else if Group.Top < 0 then
    begin
      if Group.CaptionStyle = csSmall then
        ScrollControls( -Group.Top + GroupBorderSize )
      else
        ScrollControls( -Group.Top + Group.CaptionYOffset + GroupBorderSize );
    end;
  end;
end; {= TRzGroupBar.ShowEntireGroup =}



procedure TRzGroupBar.ScrollControls( Delta: Integer );
begin
  FScrolling := True;
  try
    ScrollPosition := ScrollPosition - Delta;
  finally
    FScrolling := False;
  end;
end;


function TRzGroupBar.IsScrollBarVisible: Boolean;
var
  SI: TScrollInfo;
begin
  SI.cbSize := SizeOf( SI );
  SI.fMask := SIF_ALL;
  GetScrollInfo( Self.Handle, SB_VERT, SI );
  Result :=  Integer( SI.nPage ) < ( SI.nMax - SI.nMin );
end;


procedure TRzGroupBar.UpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
begin
  if HandleAllocated then
  begin
    SIOld.cbSize := SizeOf( SIOld );
    SIOld.fMask := SIF_ALL;
    GetScrollInfo( Self.Handle, SB_VERT, SIOld );
    SINew := SIOld;

    SINew.nMin := 0;
    SINew.nPage := Self.ClientHeight;
    SINew.nMax := TotalHeight;
    SINew.nPos := FScrollPosition;

    if ( SINew.nMin <> SIOld.nMin ) or
       ( SINew.nMax <> SIOld.nMax ) or
       ( SINew.nPage <> SIOld.nPage ) or
       ( SINew.nPos <> SIOld.nPos ) then
    begin
      SetScrollInfo( Self.Handle, SB_VERT, SINew, True );

      if Integer( SINew.nPage ) > SINew.nMax then
        ScrollPosition := SINew.nMin;
      Invalidate;
    end;
  end;
end;


function TRzGroupBar.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  ScrollPosition := ScrollPosition + 10;
  Result := True;
end;


function TRzGroupBar.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  ScrollPosition := ScrollPosition - 10;
  Result := True;
end;


procedure TRzGroupBar.WMVScroll( var Msg: TWMVScroll );
var
  SI: TScrollInfo;
begin
  SI.cbSize := SizeOf( SI );
  SI.fMask := SIF_ALL;
  GetScrollInfo( Self.Handle, SB_VERT, SI );

  case Msg.ScrollCode of
    sb_LineUp:
      ScrollPosition := ScrollPosition - 10;

    sb_LineDown:
      ScrollPosition := ScrollPosition + 10;

    sb_PageUp:
      ScrollPosition := ScrollPosition - Integer( SI.nPage );

    sb_PageDown:
      ScrollPosition := ScrollPosition + Integer( SI.nPage );

    sb_ThumbPosition, sb_ThumbTrack:
    begin
      if SI.nTrackPos <= SI.nMin then
        ScrollPosition := SI.nMin
      else if SI.nTrackPos >= SI.nMax then
        ScrollPosition := SI.nMax
      else
        ScrollPosition := SI.nTrackPos;
    end;

    sb_Top :
      ScrollPosition := SI.nMin;

    sb_Bottom:
      ScrollPosition := SI.nMax;
  end;
end; {= TRzGroupBar.WMVScroll =}


procedure TRzGroupBar.SetScrollPosition( Value: Integer );
var
  SI: TScrollInfo;
  NewPos: Integer;
begin
  if FScrollPosition <> Value then
  begin
    NewPos := Value;
    if not ( csLoading in ComponentState ) then
    begin
      SI.cbSize := SizeOf( SI );
      SI.fMask := SIF_ALL;
      GetScrollInfo( Self.Handle, SB_VERT, SI );

      if NewPos > SI.nMax - Integer( SI.nPage ) + SI.nMin then
        NewPos := SI.nMax - Integer( SI.nPage ) + SI.nMin;
      if NewPos < SI.nMin then
        NewPos := SI.nMin;

      UpdateObjectInspector( Self );

      ScrollBy( 0, FScrollPosition - NewPos );
      FScrollPosition := NewPos;
      UpdateScrollBar;
    end;
  end;
end;



procedure TRzGroupBar.ScrollInView( AControl: TControl );
var
  I: Integer;
  Group: TRzGroup;
begin
  Group := nil;
  for I := 0 to FGroups.Count - 1 do
  begin
    if TRzGroup( FGroups[ I ] ).ContainsControl( AControl ) then
    begin
      Group := TRzGroup( FGroups[ I ] );
      Break;
    end;
  end;

  if Group <> nil then
  begin
    Group.Opened := True;
    // Commenting out the next line fixes the shifting group problem on focus change
    //ShowEntireGroup( Group );
  end;
end;


procedure TRzGroupBar.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  if ( Button = mbLeft ) and Enabled then
  begin
    if not ( csDesigning in ComponentState ) then
      SetFocus; // This is necessary to have mouse wheel work
  end;
end;


procedure TRzGroupBar.Paint;
var
  I: Integer;
const
  HelpStr: string = 'Add groups by selecting "Add Group" from the context menu.';
var
  X, Y: Integer;
  R: TRect;
begin
  inherited;


  if ( FVisualStyle <> vsClassic ) and ( FStyle = gbsCategoryView ) then
  begin
    DrawGroupBarBackground( Canvas, ClientRect, FVisualStyle,
                            FGradientColorStyle, FGradientPath,
                            FGradientColorStart, FGradientColorStop );
  end
  else if not UsingSystemStyle then
  begin
    Canvas.Brush.Color := DarkerColor( ActiveStyleSystemColor( clBtnFace ), 20 );
    Canvas.FillRect( ClientRect );
  end;

  // Draw Designer Message
  if ( csDesigning in ComponentState ) and ( FGroups.Count = 0 ) and ( Canvas.Handle <> 0 ) then
  begin
    Canvas.Brush.Style := bsClear;

    R := ClientRect;
    InflateRect( R, -FGroupBorderSize, -FGroupBorderSize );

    DrawString( Canvas, HelpStr, R, dt_CalcRect or dt_WordBreak or dt_Center );

    X := ( ClientWidth - ( R.Right - R.Left ) ) div 2;
    Y := ( ClientHeight - ( R.Bottom - R.Top ) ) div 2;
    OffsetRect( R, X - R.Left, Y - R.Top );

    DrawString( Canvas, HelpStr, R, dt_WordBreak or dt_Center );
    Canvas.Brush.Style := bsSolid;
  end;

  for I := 0 to GroupCount - 1 do
  begin
    // Draw group's out of bound areas
    if Groups[ I ].Visible and ( Groups[ I ].CaptionStyle = csLarge ) then
      Groups[ I ].DrawOutOfBounds;
  end;
end; {= TRzGroupBar.Paint =}


procedure TRzGroupBar.SetExclusiveMode( Value: Boolean );
var
  I: Integer;
begin
  if FStyle <> gbsCategoryView then
    Exit;                            // ExclusiveMode only makes sense for gbsCategoryView

  if FExclusiveMode <> Value then
  begin
    FExclusiveMode := Value;
    if FExclusiveMode then
    begin
      // Open First Group
      if GroupCount > 0 then
      begin
        Groups[ 0 ].Open;
        for I := 1 to GroupCount - 1 do
          Groups[ I ].Close;
      end;
    end;
  end;
end;


procedure TRzGroupBar.RepaintBorder;
begin
  if HasParent then
  begin
    SetWindowPos( Handle, 0, 0, 0, 0, 0, swp_FrameChanged or swp_NoMove or swp_NoSize or swp_NoZOrder );
    InvalidateWindowFrame( Handle, ClientRect );
  end;
end;


procedure TRzGroupBar.SetBevelWidth( Value: TBevelWidth );
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    UpdateLayout;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetBorderSides( Value: TSides );
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    UpdateLayout;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetBorderHighlight( Value: TColor );
begin
  if FBorderHighlight <> Value then
  begin
    FBorderHighlight := Value;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetBorderShadow( Value: TColor );
begin
  if FBorderShadow <> Value then
  begin
    FBorderShadow := Value;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetBorderInner( Value: TFrameStyleEx );
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    UpdateLayout;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetBorderOuter( Value: TFrameStyleEx );
begin
  if FBorderOuter <> Value then
  begin
    FBorderOuter := Value;
    UpdateLayout;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetGradientColorStyle( Value: TRzGradientColorStyle );
begin
  if FGradientColorStyle <> Value then
  begin
    FGradientColorStyle := Value;
    Invalidate;
    RepaintGroups;
  end;
end;


procedure TRzGroupBar.SetGradientColorStart( Value: TColor );
begin
  if FGradientColorStart <> Value then
  begin
    FGradientColorStart := Value;
    Invalidate;
  end;
end;


procedure TRzGroupBar.SetGradientColorStop( Value: TColor );
begin
  if FGradientColorStop <> Value then
  begin
    FGradientColorStop := Value;
    Invalidate;
  end;
end;


procedure TRzGroupBar.SetGradientPath( Value: TRzGroupBarGradientPath );
begin
  if FGradientPath <> Value then
  begin
    FGradientPath := Value;
    Invalidate;
  end;
end;


procedure TRzGroupBar.SetGroupBorderSize( Value: Integer );
begin
  if FGroupBorderSize <> Value then
  begin
    FGroupBorderSize := Value;
    UpdateLayout;
    RepaintBorder;
  end;
end;


procedure TRzGroupBar.SetSmallImages( Value: TCustomImageList );
var
  I: Integer;
begin
  if FSmallImages <> nil then
    FSmallImages.UnRegisterChanges( FSmallImagesChangeLink );

  FSmallImages := Value;

  if FSmallImages <> nil then
  begin
    FSmallImages.RegisterChanges( FSmallImagesChangeLink );
    FSmallImages.FreeNotification( Self );
  end;

  for I := 0 to GroupCount - 1 do
    Groups[ I ].Invalidate;

  UpdateLayout;
  Invalidate;
end;


procedure TRzGroupBar.SetLargeImages( Value: TCustomImageList );
begin
  if FLargeImages <> nil then
    FLargeImages.UnRegisterChanges( FLargeImagesChangeLink );

  FLargeImages := Value;

  if FLargeImages <> nil then
  begin
    FLargeImages.RegisterChanges( FLargeImagesChangeLink );
    FLargeImages.FreeNotification( Self );
  end;

  UpdateLayout;
  Invalidate;
end;


procedure TRzGroupBar.ImagesChange( Sender: TObject );
begin
  if Sender = SmallImages then
    Update
  else if Sender = LargeImages then
    Update;
end;


procedure TRzGroupBar.SetStyle( Value: TRzGroupBarStyle );
var
  I: Integer;
begin
  if FStyle <> Value then
  begin
    FStyle := Value;

    // Change GroupBar settings affected by Style
    case FStyle of
      gbsCategoryView:
      begin
        Color := clBtnShadow;
        FGroupBorderSize := 8;
      end;

      gbsTaskList:
      begin
        Color := clWindow;
        FGroupBorderSize := 4;
      end;

      gbsOutlook:
      begin
        Color := clBtnShadow;
        FGroupBorderSize := 0;
      end;
    end;

    // Change Group settings affected by Style
    for I := 0 to GroupCount - 1 do
      Groups[ I ].Style := FStyle;

    UpdateLayout;
    Invalidate;
  end;
end; {= TRzGroupBar.SetStyle =}


procedure TRzGroupBar.SetUniqueItemSelection( Value: Boolean );
var
  I: Integer;
begin
  if FUniqueItemSelection <> Value then
  begin
    FUniqueItemSelection := Value;
    if FUniqueItemSelection then
    begin
      // If changing to True, then we need to remove all selections that may
      // already be set in the groups.
      for I := 0 to GroupCount - 1 do
        Groups[ I ].ItemIndex := -1;
    end;
  end;
end;


procedure TRzGroupBar.SetVisualStyle( Value: TRzVisualStyle );
begin
  if FVisualStyle <> Value then
  begin
    FVisualStyle := Value;
    Invalidate;
    RepaintGroups;
  end;
end;


procedure TRzGroupBar.RepaintGroups;
var
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
    Groups[ I ].Invalidate;
  {&RV}
end;


procedure TRzGroupBar.CMGroupItemSelected( var Msg: TCMGroupItemMsg );
var
  I: Integer;
begin
  for I := 0 to GroupCount - 1 do
  begin
    if Groups[ I ].ShowItemSelection then
      Groups[ I ].CMGroupItemSelected( Msg );
  end;
end;


procedure TRzGroupBar.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  RepaintGroups;
end;


procedure TRzGroupBar.CMFocusChanged( var Msg: TCMFocusChanged );
begin
  inherited;
  if ( Msg.Sender <> nil ) and ContainsControl( Msg.Sender ) and ( Parent <> nil ) then
    ScrollInView( Msg.Sender );
end;


procedure TRzGroupBar.WMNCHitTest( var Msg: TWMNCHitTest );
begin
  DefaultHandler( Msg );                    // Bypass design time hooks to allow design time interactive scrollbars
end;


procedure TRzGroupBar.WMNCCalcSize( var Msg: TWMNCCalcSize );
var
  R: TRect;

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

begin {= TRzGroupBar.WMNCCalcSize =}
  if Msg.CalcValidRects then
  begin
    R := Msg.CalcSize_Params^.rgrc[ 0 ];

    if FBorderOuter = fsFlat then
      AdjustRect( R, FBorderSides, 1 )
    else if FBorderOuter in [ fsStatus, fsPopup ] then
      AdjustRect( R, FBorderSides, BevelWidth )
    else if FBorderOuter in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
      AdjustRect( R, FBorderSides, 2 );

    if FBorderInner = fsFlat then
      AdjustRect( R, FBorderSides, 1 )
    else if FBorderInner in [ fsStatus, fsPopup ] then
      AdjustRect( R, FBorderSides, BevelWidth )
    else if FBorderInner in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
      AdjustRect( R, FBorderSides, 2 );

    Msg.CalcSize_Params^.rgrc[ 0 ] := R;
  end;

  inherited;
end;


type
  TWinControlAccess = class( TWinControl );


procedure TRzGroupBar.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
  R: TRect;
begin
  inherited;                                // Must call inherited so that scroll bar shows up correctly

  DC := GetWindowDC( Handle );
  FCanvas.Handle := DC;
  try
    GetWindowRect( Handle, R );
    OffsetRect( R, -R.Left, -R.Top );

    R := DrawInnerOuterBorders( FCanvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, BevelWidth,
                                FBorderColor, FBorderHighlight, FBorderShadow,
                                FlatColor, FlatColorAdjustment, Color, TWinControlAccess( Parent ).Color, False );
  finally
    FCanvas.Handle := 0;
    ReleaseDC( Handle, DC );
  end;

  Msg.Result := 0;
end;


procedure TRzGroupBar.WMThemeChanged( var Msg: TMessage );
begin
  inherited;
  // Update CurrentXPColorScheme global variable
  CurrentXPColorScheme := GetXPColorScheme;
end;


procedure TRzGroupBar.UMResetScrollPosition( var Msg: TMessage );
{$IFDEF VCL160_OR_HIGHER}
var
  R: TRect;
{$ENDIF}
begin
  inherited;
  ScrollPosition := 0;

  {$IFDEF VCL160_OR_HIGHER}
  if not UsingSystemStyle then
  begin
    R := ClientRect;
    if not UseRightToLeftAlignment then
      R.Left := R.Right - GetSystemMetrics( sm_CxVScroll ) - 2
    else
      R.Right := R.Left + GetSystemMetrics( sm_CxVScroll ) + 2;
    RedrawWindow( Handle, @R, 0, rdw_Invalidate or rdw_Frame or rdw_NoErase );
  end;
  {$ENDIF}
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzGroupBar.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  ScrollPosition := 0;
end;

{$ENDIF}


{================================}
{== TRzGroupController Methods ==}
{================================}

constructor TRzGroupController.Create( AOwner: TComponent );
begin
  inherited;

  FUpdateCount := 0;

  FItemIndent := 20;
  FItemHeight := 20;

  FColor := clBtnFace;
  FBorderColor := clBtnHighlight;
  FCaptionColor := clHighlightText;
  FCaptionColorStart := clWindow;
  FCaptionColorStop := clBtnFace;
  FCaptionHotColor := clHotLight;
  FItemHotColor := clHotLight;
  FItemHotZone := ihzImageCaption;
  FSelectionColor := clBtnFace;
  FSelectionFontColor := clWindowText;
  FSelectionShadowColor := clBtnShadow;
  FSelectionFrameColor := cl3DDkShadow;
  FShowItemSelection := False;
  FItemSelectionStyle := issImageAndCaption;
  FIgnoreSelectedItemClick := True;
  FHideAccelerators := True;

  FFont := TFont.Create;
  FFont.Color := clWindowText;  // Used for Items
  FFontChanged := False;
  FFont.OnChange := FontChangeHandler;
  FDefaultFontName := FFont.Name;
  FDefaultFontSize := FFont.Size;
  {&RCI}

  FCaptionFont := TFont.Create;
  FCaptionFont.Assign( FFont );
  FCaptionFont.Style := [ fsBold ];
  FCaptionFont.Color := clHighlight;
  FCaptionFontChanged := False;
  FCaptionFont.OnChange := CaptionFontChangeHandler;

  FItemStaticFont := TFont.Create;
  FItemStaticFont.Assign( FFont );
  FItemStaticFontChanged := False;
  FItemStaticFont.OnChange := ItemStaticFontChangeHandler;

  FCaptionHeight := 20;

  FDividerVisible := False;
  FDividerColor := clHighlight;
end; {= TRzGroupController.Create =}


procedure TRzGroupController.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Handle the fact that the UseGradients property was published in verison 3.x
  Filer.DefineProperty( 'UseGradients',
                        TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzGroupController.Loaded;
begin
  inherited;
  UpdateGroups;
end;


destructor TRzGroupController.Destroy;
begin
  FFont.Free;
  FCaptionFont.Free;
  FItemStaticFont.Free;

  if FGroupList <> nil then
  begin
    FGroupList.Free;
    FGroupList := nil;
  end;

  inherited;
end;


procedure TRzGroupController.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( AComponent = FRegIniFile ) then
    FRegIniFile := nil;
end;



procedure TRzGroupController.Assign( Source: TPersistent );
begin
  if Source is TRzGroupController then
  begin
    BeginUpdate;
    try
      FBorderColor := TRzGroupController( Source ).BorderColor;

      FCaptionColor := TRzGroupController( Source ).CaptionColor;
      FCaptionColorStart := TRzGroupController( Source ).CaptionColorStart;
      FCaptionColorStop := TRzGroupController( Source ).CaptionColorStop;
      FCaptionHotColor := TRzGroupController( Source ).CaptionHotColor;
      FCaptionHeight := TRzGroupController( Source ).CaptionHeight;
      FCaptionFont := TRzGroupController( Source ).CaptionFont;
      FColor := TRzGroupController( Source ).Color;

      FDividerVisible := TRzGroupController( Source ).DividerVisible;
      FDividerColor := TRzGroupController( Source ).DividerColor;

      FItemHotColor := TRzGroupController( Source ).ItemHotColor;
      FItemHotZone := TRzGroupController( Source ).ItemHotZone;
      FItemHeight := TRzGroupController( Source ).ItemHeight;
      FItemIndent := TRzGroupController( Source ).ItemIndent;
      FItemStaticFont := TRzGroupController( Source ).ItemStaticFont;

      FSelectionColor := TRzGroupController( Source ).SelectionColor;
      FSelectionFontColor := TRzGroupController( Source ).SelectionFontColor;
      FSelectionShadowColor := TRzGroupController( Source ).SelectionShadowColor;
      FSelectionFrameColor := TRzGroupController( Source ).SelectionFrameColor;
      FShowItemSelection := TRzGroupController( Source ).ShowItemSelection;
      FItemSelectionStyle := TRzGroupController( Source ).ItemSelectionStyle;
      FIgnoreSelectedItemClick := TRzGroupController( Source ).IgnoreSelectedItemClick;
      FHideAccelerators := TRzGroupController( Source ). HideAccelerators;

      FParentColor := TRzGroupController( Source ).ParentColor;
      FParentFont := TRzGroupController( Source ).ParentFont;
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;


procedure TRzGroupController.AddGroup( G: TRzGroup );
begin
  if not Assigned( FGroupList ) then
    FGroupList := TList.Create;

  if FGroupList.IndexOf( G ) < 0 then
  begin
    FGroupList.Add( G );
    UpdateGroupProperty( G, gpAll );
  end;
end;


procedure TRzGroupController.RemoveGroup( G: TRzGroup );
begin
  if FGroupList <> nil then
  begin
    FGroupList.Remove( G );
    if FGroupList.Count = 0 then
    begin
      FGroupList.Free;
      FGroupList := nil;
    end;
  end;
end;


procedure TRzGroupController.BeginUpdate;
begin
  Inc( FUpdateCount );
end;


procedure TRzGroupController.EndUpdate;
begin
  Dec( FUpdateCount );
  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    UpdateAllGroups( gpAll );
  end;
end;


procedure TRzGroupController.UpdateGroups;
begin
  UpdateAllGroups( gpAll );
end;


procedure TRzGroupController.UpdateGroupProperty( G: TRzGroup; GroupProperty: TRzGroupProperty );
begin
  case GroupProperty of
    gpAll:
    begin
      G.ParentColor := FParentColor;
      if not FParentColor then
        G.Color := FColor;

      G.ParentFont := FParentFont;
      if not FParentFont then
        G.Font := FFont;

      G.BorderColor := FBorderColor;
      G.CaptionColor := FCaptionColor;
      G.CaptionColorStart := FCaptionColorStart;
      G.CaptionColorStop := FCaptionColorStop;
      G.CaptionFont := FCaptionFont;
      G.CaptionHeight := FCaptionHeight;
      G.CaptionHotColor := FCaptionHotColor;
      G.DividerColor := FDividerColor;
      G.DividerVisible := FDividerVisible;
      G.ItemHeight := FItemHeight;
      G.ItemHotColor := FItemHotColor;
      G.ItemHotZone := FItemHotZone;
      G.ItemIndent := FItemIndent;
      G.ItemStaticFont := FItemStaticFont;
      G.SelectionColor := FSelectionColor;
      G.SelectionFontColor := FSelectionFontColor;
      G.SelectionShadowColor := FSelectionShadowColor;
      G.SelectionFrameColor := FSelectionFrameColor;
      G.ShowItemSelection := FShowItemSelection;
      G.ItemSelectionStyle := FItemSelectionStyle;
      G.IgnoreSelectedItemClick := FIgnoreSelectedItemClick;
      G.HideAccelerators := FHideAccelerators;
    end;

    gpBorderColor:
      G.BorderColor := FBorderColor;

    gpCaptionColor:
      G.CaptionColor := FCaptionColor;

    gpCaptionColorStart:
      G.CaptionColorStart := FCaptionColorStart;

    gpCaptionColorStop:
      G.CaptionColorStop := FCaptionColorStop;

    gpCaptionFont:
      G.CaptionFont := FCaptionFont;

    gpCaptionHeight:
      G.CaptionHeight := FCaptionHeight;

    gpCaptionHotColor:
      G.CaptionHotColor := FCaptionHotColor;

    gpColor:
      G.Color := FColor;

    gpParentColor:
      G.ParentColor := FParentColor;

    gpDividerColor:
      G.DividerColor := FDividerColor;

    gpDividerVisible:
      G.DividerVisible := FDividerVisible;

    gpFont:
      G.Font := FFont;

    gpParentFont:
      G.ParentFont := FParentFont;

    gpItemHeight:
      G.ItemHeight := FItemHeight;

    gpItemHotColor:
      G.ItemHotColor := FItemHotColor;

    gpItemHotZone:
      G.ItemHotZone := FItemHotZone;

    gpItemIndent:
      G.ItemIndent := FItemIndent;

    gpItemStaticFont:
      G.ItemStaticFont := FItemStaticFont;

    gpSelectionColor:
      G.SelectionColor := FSelectionColor;

    gpSelectionFontColor:
      G.SelectionFontColor := FSelectionFontColor;
      
    gpSelectionShadowColor:
      G.SelectionShadowColor := FSelectionShadowColor;

    gpSelectionFrameColor:
      G.SelectionFrameColor := FSelectionFrameColor;

    gpShowItemSelection:
      G.ShowItemSelection := FShowItemSelection;

    gpItemSelectionStyle:
      G.ItemSelectionStyle := FItemSelectionStyle;

    gpIgnoreSelectedItemClick:
      G.IgnoreSelectedItemClick := FIgnoreSelectedItemClick;

    gpHideAccelerators:
      G.HideAccelerators := FHideAccelerators;
  end;
end; {= TRzGroupController.UpdateGroupProperty =}


procedure TRzGroupController.UpdateAllGroups( GroupProperty: TRzGroupProperty );
var
  I: Integer;
begin
  if FUpdateCount > 0 then
    Exit;

  if FGroupList <> nil then
  begin
    { For each component on the FGroupList ... }
    for I := 0 to FGroupList.Count - 1 do
    begin
      UpdateGroupProperty( FGroupList[ I ], GroupProperty );
    end;
  end;
end;


procedure TRzGroupController.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    UpdateAllGroups( gpBorderColor );
  end;
end;


procedure TRzGroupController.SetCaptionColor( Value: TColor );
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    UpdateAllGroups( gpCaptionColor );
  end;
end;


procedure TRzGroupController.SetCaptionColorStart( Value: TColor );
begin
  if FCaptionColorStart <> Value then
  begin
    FCaptionColorStart := Value;
    UpdateAllGroups( gpCaptionColorStart );
  end;
end;


procedure TRzGroupController.SetCaptionColorStop( Value: TColor );
begin
  if FCaptionColorStop <> Value then
  begin
    FCaptionColorStop := Value;
    UpdateAllGroups( gpCaptionColorStop );
  end;
end;


procedure TRzGroupController.SetCaptionFont( Value: TFont );
begin
  FCaptionFont.Assign( Value );
  UpdateAllGroups( gpCaptionFont );
end;


procedure TRzGroupController.SetCaptionHeight( Value: Integer );
begin
  if FCaptionHeight <> Value then
  begin
    FCaptionHeight := Value;
    if FCaptionHeight < 16 then
      FCaptionHeight := 16;
    UpdateAllGroups( gpCaptionHeight );
  end;
end;


procedure TRzGroupController.SetCaptionHotColor( Value: TColor );
begin
  if FCaptionHotColor <> Value then
  begin
    FCaptionHotColor := Value;
    UpdateAllGroups( gpCaptionHotColor );
  end;
end;


procedure TRzGroupController.SetItemHotColor( Value: TColor );
begin
  if FItemHotColor <> Value then
  begin
    FItemHotColor := Value;
    UpdateAllGroups( gpItemHotColor );
  end;
end;


procedure TRzGroupController.SetItemHotZone( Value: TRzGroupItemHotZone );
begin
  if FItemHotZone <> Value then
  begin
    FItemHotZone := Value;
    UpdateAllGroups( gpItemHotZone );
  end;
end;


procedure TRzGroupController.SetItemStaticFont( Value: TFont );
begin
  FItemStaticFont.Assign( Value );
  UpdateAllGroups( gpItemStaticFont );
end;


function TRzGroupController.IsFontStored: Boolean;
begin
  Result := not FParentFont;
end;


procedure TRzGroupController.SetFont( Value: TFont );
begin
  FFont.Assign( Value );
  UpdateAllGroups( gpFont );
end;


function TRzGroupController.StoreColor: Boolean;
begin
  Result := not FParentColor;
end;


procedure TRzGroupController.SetColor( Value: TColor );
begin
  if FColor <> Value then
  begin
    FColor := Value;
    UpdateAllGroups( gpColor );
  end;
end;


procedure TRzGroupController.SetSelectionColor( Value: TColor );
begin
  if FSelectionColor <> Value then
  begin
    FSelectionColor := Value;
    UpdateAllGroups( gpSelectionColor );
  end;
end;


procedure TRzGroupController.SetSelectionFontColor( Value: TColor );
begin
  if FSelectionFontColor <> Value then
  begin
    FSelectionFontColor := Value;
    UpdateAllGroups( gpSelectionFontColor );
  end;
end;


procedure TRzGroupController.SetSelectionShadowColor( Value: TColor );
begin
  if FSelectionShadowColor <> Value then
  begin
    FSelectionShadowColor := Value;
    UpdateAllGroups( gpSelectionShadowColor );
  end;
end;


procedure TRzGroupController.SetSelectionFrameColor( Value: TColor );
begin
  if FSelectionFrameColor <> Value then
  begin
    FSelectionFrameColor := Value;
    UpdateAllGroups( gpSelectionFrameColor );
  end;
end;


procedure TRzGroupController.SetHideAccelerators( Value: Boolean );
begin
  if FHideAccelerators <> Value then
  begin
    FHideAccelerators := Value;
    UpdateAllGroups( gpHideAccelerators );
  end;
end;

procedure TRzGroupController.SetShowItemSelection( Value: Boolean );
begin
  if FShowItemSelection <> Value then
  begin
    FShowItemSelection := Value;
    UpdateAllGroups( gpShowItemSelection );
  end;
end;


procedure TRzGroupController.SetItemSelectionStyle( Value: TRzItemSelectionStyle );
begin
  if FItemSelectionStyle <> Value then
  begin
    FItemSelectionStyle := Value;
    UpdateAllGroups( gpItemSelectionStyle );
  end;
end;


procedure TRzGroupController.SetIgnoreSelectedItemClick( Value: Boolean );
begin
  if FIgnoreSelectedItemClick <> Value then
  begin
    FIgnoreSelectedItemClick := Value;
    UpdateAllGroups( gpIgnoreSelectedItemClick );
  end;
end;


procedure TRzGroupController.SetItemHeight( Value: Integer );
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    UpdateAllGroups( gpItemHeight );
  end;
end;


procedure TRzGroupController.SetItemIndent( Value: Byte );
begin
  if FItemIndent <> Value then
  begin
    FItemIndent := Value;
    UpdateAllGroups( gpItemIndent );
  end;
end;


procedure TRzGroupController.SetParentColor( Value: Boolean );
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    UpdateAllGroups( gpParentColor );
    if not FParentColor then
      UpdateAllGroups( gpColor );
  end;
end;


procedure TRzGroupController.SetParentFont( Value: Boolean );
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    UpdateAllGroups( gpParentFont );
    if not FParentFont then
      UpdateAllGroups( gpFont );
  end;
end;


procedure TRzGroupController.SetDividerColor( Value: TColor );
begin
  if FDividerColor <> Value then
  begin
    FDividerColor := Value;
    UpdateAllGroups( gpDividerColor );
  end;
end;


procedure TRzGroupController.SetDividerVisible( Value: Boolean );
begin
  if FDividerVisible <> Value then
  begin
    FDividerVisible := Value;
    UpdateAllGroups( gpDividerVisible );
  end;
end;


procedure TRzGroupController.FontChangeHandler( Sender: TObject );
begin
  FFontChanged := True;
  UpdateFonts;
  UpdateAllGroups( gpFont );
end;


procedure TRzGroupController.CaptionFontChangeHandler( Sender: TObject );
begin
  FCaptionFontChanged := True;
  UpdateAllGroups( gpCaptionFont );
end;


procedure TRzGroupController.ItemStaticFontChangeHandler( Sender: TObject );
begin
  FItemStaticFontChanged := True;
  UpdateAllGroups( gpItemStaticFont );
end;


procedure TRzGroupController.UpdateFonts;
begin
  FFontChanged := True;
  if not FCaptionFontChanged and ( FCaptionFont <> nil ) then
  begin
    FCaptionFont.Name := Self.Font.Name;
    FCaptionFont.Size := Self.Font.Size;
    FCaptionFont.Style := [ fsBold ];
    // Reset FCaptionFontChanged b/c internal handler has gotten called from above statements
    FCaptionFontChanged := False;
  end;
  if not FItemStaticFontChanged and ( FItemStaticFont <> nil ) then
  begin
    FItemStaticFont.Assign( Self.Font );
    // Reset FItemStaticFontChanged b/c internal handler has gotten called from above statements
    FItemStaticFontChanged := False;
  end;
end;


procedure TRzGroupController.SetDefaults( Style: TRzGroupBarStyle );
begin
  FParentFont := True;
  FFont.Name := FDefaultFontName;
  FFont.Size := FDefaultFontSize;
  FFont.Style := [];
  FFont.Color := clWindowText;
  FFontChanged := False;
  FItemStaticFont.Assign( FFont );
  FItemStaticFontChanged := False;

  case Style of
    gbsCategoryView:
    begin
      FColor := clBtnFace;
      FParentColor := False;
      FCaptionColor := clHighlightText;
      FCaptionColorStart := clWindow;
      FCaptionColorStop := clBtnFace;
      FCaptionFont.Assign( FFont );
      FCaptionFont.Style := [ fsBold ];
      FCaptionFont.Color := clHighlight;
      FCaptionFontChanged := False;
      FDividerVisible := False;
    end;

    gbsTaskList:
    begin
      FColor := clWindow;
      FParentColor := True;
      FCaptionColor := clWindow;
      FCaptionColorStart := clWindow;
      FCaptionColorStop := clWindow;
      FCaptionFont.Assign( FFont );
      FCaptionFont.Style := [ fsBold ];
      FCaptionFont.Color := clWindowText;
      FCaptionFontChanged := False;
      FDividerVisible := True;
    end;

    gbsOutlook:
    begin
      FColor := clBtnShadow;
      FParentColor := False;
      FCaptionColor := clBtnFace;
      FCaptionColorStart := clBtnFace;
      FCaptionColorStop := clBtnFace;
      FCaptionFont.Assign( FFont );
      FCaptionFont.Style := [ ];
      FCaptionFont.Color := clWindowText;
      FCaptionFontChanged := False;
      FDividerVisible := False;
    end;
  end;

  FDividerColor := clHighlight;
  FItemIndent := 20;
  FItemHeight := 20;
  FBorderColor := clBtnHighlight;
  FCaptionHotColor := clHotLight;
  FItemHotColor := clHotLight;
  FItemHotZone := ihzImageCaption;
  FSelectionColor := clBtnFace;
  FSelectionFontColor := clWindowText;
  FSelectionShadowColor := clBtnShadow;
  FSelectionFrameColor := cl3DDkShadow;
  FShowItemSelection := False;
  FItemSelectionStyle := issImage;
  FIgnoreSelectedItemClick := True;
  FCaptionHeight := 20;
  FHideAccelerators := True;

  UpdateAllGroups( gpAll );
end;


procedure TRzGroupController.Load( const Section: string );

  procedure ReadFontData( const Section, Prefix: string; BoldStyle: Boolean; Font: TFont );
  var
    StyleBits: Byte;
  begin
    with Font do
    begin
      Name := FRegIniFile.ReadString( Section, Prefix + 'Name', FDefaultFontName );
      Size := FRegIniFile.ReadInteger( Section, Prefix + 'Size', FDefaultFontSize );
      Color := FRegIniFile.ReadInteger( Section, Prefix + 'Color', clWindowText );
      if BoldStyle then
        StyleBits := FRegIniFile.ReadInteger( Section, Prefix + 'Style', Ord( fsBold ) )
      else
        StyleBits := FRegIniFile.ReadInteger( Section, Prefix + 'Style', Ord( fsNormal ) );
      Style := [];
      if StyleBits and fsBoldMask = fsBoldMask then
        Style := Style + [ fsBold ];
      if StyleBits and fsItalicMask = fsItalicMask then
        Style := Style + [ fsItalic ];
      if StyleBits and fsUnderlineMask = fsUnderlineMask then
        Style := Style + [ fsUnderline ];
      if StyleBits and fsStrikeOutMask = fsStrikeOutMask then
        Style := Style + [ fsStrikeOut ];
    end; { with Font }
  end; {= ReadFontData =}

begin
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotLoadGroupController );

  BeginUpdate;
  try
    FParentColor := FRegIniFile.ReadBool( Section, 'ParentColor', FParentColor );
    FColor := FRegIniFile.ReadInteger( Section, 'Color', clBtnFace );
    FParentFont := FRegIniFile.ReadBool( Section, 'ParentFont', FParentFont );
    ReadFontData( Section, 'Font', False, FFont );
    FBorderColor := FRegIniFile.ReadInteger( Section, 'BorderColor', clBtnHighlight );
    FCaptionColor := FRegIniFile.ReadInteger( Section, 'CaptionColor', clHighlightText );
    FCaptionColorStart := FRegIniFile.ReadInteger( Section, 'CaptionColorStart', clWindow );
    FCaptionColorStop := FRegIniFile.ReadInteger( Section, 'CaptionColorStop', clBtnFace );
    ReadFontData( Section, 'CaptionFont', True, FCaptionFont );
    FCaptionHeight := FRegIniFile.ReadInteger( Section, 'CaptionHeight', 20 );
    FCaptionHotColor := FRegIniFile.ReadInteger( Section, 'CaptionHotColor', clHotLight );
    FDividerColor := FRegIniFile.ReadInteger( Section, 'DividerColor', clHighlight );
    FDividerVisible := FRegIniFile.ReadBool( Section, 'DividerVisible', False );
    FItemHeight := FRegIniFile.ReadInteger( Section, 'ItemHeight', 20 );
    FItemHotColor := FRegIniFile.ReadInteger( Section, 'ItemHotColor', clHotLight );
    FItemHotZone := TRzGroupItemHotZone( FRegIniFile.ReadInteger( Section, 'ItemHotZone', 0 ) );
    FItemIndent := FRegIniFile.ReadInteger( Section, 'ItemIndent', 20 );
    ReadFontData( Section, 'ItemStaticFont', False, FItemStaticFont );
    FSelectionColor := FRegIniFile.ReadInteger( Section, 'SelectionColor', clBtnFace );
    FSelectionFontColor := FRegInifile.ReadInteger( Section, 'SelectionFontColor', clWindowText );
    FSelectionShadowColor := FRegIniFile.ReadInteger( Section, 'SelectionShadowColor', clBtnShadow );
    FSelectionFrameColor := FRegIniFile.ReadInteger( Section, 'SelectionFrameColor', cl3DDkShadow );
    FShowItemSelection := FRegIniFile.ReadBool( Section, 'ShowItemSelection', False );
    FItemSelectionStyle := TRzItemSelectionStyle( FRegIniFile.ReadInteger( Section, 'ItemSelectionStyle', 0 ) );
    FIgnoreSelectedItemClick := FRegIniFile.ReadBool( Section, 'IgnoreSelectedItemClick', True );
    FHideAccelerators := FRegIniFile.ReadBool( Section, 'HideAccelerators', True );

  finally
    EndUpdate;
  end;
end;


procedure TRzGroupController.Save( const Section: string );

  procedure WriteFontData( const Section, Prefix: string; Font: TFont );
  var
    StyleBits: Byte;
  begin
    with Font do
    begin
      FRegIniFile.WriteString( Section, Prefix + 'Name', Name );
      FRegIniFile.WriteInteger( Section, Prefix + 'Size', Size );
      FRegIniFile.WriteInteger( Section, Prefix + 'Color', Color );

      StyleBits := 0;
      if fsBold in Style then
        StyleBits := fsBoldMask;
      if fsItalic in Style then
        StyleBits := StyleBits + fsItalicMask;
      if fsUnderline in Style then
        StyleBits := StyleBits + fsUnderlineMask;
      if fsStrikeOut in Style then
        StyleBits := StyleBits + fsStrikeOutMask;
      FRegIniFile.WriteInteger( Section, 'Style', StyleBits );
    end; { with Font }
  end; {= WriteFontData =}

begin {= TRzGroupController.Save =}
  if FRegIniFile = nil then
    raise ENoRegIniFile.Create( sRzCannotSaveGroupController );

  FRegIniFile.WriteBool( Section, 'ParentColor', FParentColor );
  FRegIniFile.WriteInteger( Section, 'Color', FColor );
  FRegIniFile.WriteBool( Section, 'ParentFont', FParentFont );
  WriteFontData( Section, 'Font', FFont );
  FRegIniFile.WriteInteger( Section, 'BorderColor', FBorderColor );
  FRegIniFile.WriteInteger( Section, 'CaptionColor', FCaptionColor );
  FRegIniFile.WriteInteger( Section, 'CaptionColorStart', FCaptionColorStart );
  FRegIniFile.WriteInteger( Section, 'CaptionColorStop', FCaptionColorStop );
  WriteFontData( Section, 'CaptionFont', FCaptionFont );
  FRegIniFile.WriteInteger( Section, 'CaptionHeight', FCaptionHeight );
  FRegIniFile.WriteInteger( Section, 'CaptionHotColor', FCaptionHotColor );
  FRegIniFile.WriteInteger( Section, 'DividerColor', FDividerColor );
  FRegIniFile.WriteBool( Section, 'DividerVisible', FDividerVisible );
  FRegIniFile.WriteInteger( Section, 'ItemHeight', FItemHeight );
  FRegIniFile.WriteInteger( Section, 'ItemHotColor', FItemHotColor );
  FRegIniFile.WriteInteger( Section, 'ItemHotZone', Ord( FItemHotZone ) );
  FRegIniFile.WriteInteger( Section, 'ItemIndent', FItemIndent );
  WriteFontData( Section, 'ItemStaticFont', FItemStaticFont );
  FRegIniFile.WriteInteger( Section, 'SelectionColor', FSelectionColor );
  FRegIniFile.WriteInteger( Section, 'SelectionFontColor', FSelectionFontColor );
  FRegIniFile.WriteInteger( Section, 'SelectionShadowColor', FSelectionShadowColor );
  FRegIniFile.WriteInteger( Section, 'SelectionFrameColor', FSelectionFrameColor );
  FRegIniFile.WriteBool( Section, 'ShowItemSelection', FShowItemSelection );
  FRegIniFile.WriteInteger( Section, 'ItemSelectionStyle', Ord( FItemSelectionStyle ) );
  FRegIniFile.WriteBool( Section, 'IgnoreSelectedItemClick', FIgnoreSelectedItemClick );
  FRegIniFile.WriteBool( Section, 'HideAccelerators', FHideAccelerators );
end; {= TRzGroupController.Save =}


procedure TRzGroupController.SetRegIniFile( Value: TRzRegIniFile );
begin
  if FRegIniFile <> Value then
  begin
    FRegIniFile := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


{&RUIF}
end.



