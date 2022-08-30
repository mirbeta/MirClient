{===============================================================================
  RzTreeVw Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzTreeView
    Enhanced tree view control.

  TRzCheckTree
    Each node in the tree is associated with a check box.


  Modification History
  ------------------------------------------------------------------------------
  6.2    (16 Jul 2015)
    * Fixed issue where custom selection highlight would cover the built-in
      selection highlight from Windows. Custom selection display is still used
      when a Custom VCL Style is active.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed display issue in TRzTreeView and TRzCheckTree when FocusColor is
      set to a non-default color and VCL Styles are used.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzTreeView and TRzCheckTree to fully
      support VCL Styles introduced in RAD Studio XE2.
    * Fixed issue where changing the background color of a TRzCheckTree at
      runtime would not change the background color of the check state images.
    * Added new ItemHeightMargin property that can be used to add some extra
      spacing between items in a TRzTreeView (or descendant).
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue where the check boxes in a TRzCheckTree would have a white
      area around the check box when the Color of the TRzCheckTree was changed
      to a non-white color.
    * Fixed issue where saving the contents of a TRzCheckTree to a stream and
      then loading that stream into another TRzCheckTree would lose the current
      node states of the original tree.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzTreeView and TRzCheckTree controls.
    * For RAD Studio 2010, surfaced OnHint event in the TRzTreeView and
      TRzCheckTree controls.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Fixed issue where calling SetAllNodes on an empty TRzCheckTree would
      result in an exception.
    * Fixed scrolling issue in TRzTreeView when Mouse Wheel settings were set
      to scroll one screen at a time.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Updated the TRzCheckTree component such that when a parent node is checked
      the OnStateChanging and OnStateChange events are only generated for child
      nodes that actually change state.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Reimplemented SaveToFile, SaveToStream, LoadFromFile, and LoadFromStream
      methods for TRzTreeView and TRzCheckTree. The change was necessary in
      order to support using text encodings, which is not possible using the
      inherited methods from TCustomTreeView. Without TEncoding support, the
      files are streams saved with the base methods do not specify any BOM. The
      problem with this is that the tree view is unable to correctly load the
      same file/steam (without the BOM).
    * Fixed problem where editing a node with a long caption would leave
      remnants of the selection rectangle (HideSelection = False) if the caption
      was made shorter.
    * Added the ItemHeight property to TRzTreeView and TRzCheckTree, which can
      be used to specify the number of pixels to be used to display each node in
      the tree. The default value is 0, which instructs the tree view to use the
      default height as determined by the selected font.
    * Modified the display of check boxes in TRzCheckTree. Specifically, a few
      more pixels of space have been added between the check box and the node's
      caption or image, whichever is adjacent to the check box.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Added new PathDelimiter property to TRzTreeView and descendants.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surface OnMouseWheel event in TRzTreeView and descendants.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem where check boxes would not appear in TRzCheckTree.
    * The color of the check box frames in TRzCheckTree are now correctly
      updated when the FrameColor property is changed.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed issue where states of nodes would get out of sync when a node was
      deleted from the tree.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzCustomTreeView to
      account for changes introduced in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzTreeView and
      descendants when FrameVisible was set to True and changes were made to
      control's appearance within calls to LockWindowUpdate.
    * Added new FrameControllerNotifications property to TRzTreeView and
      TRzCheckTree.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where scrolling the mouse wheel would not always scroll
      the tree view so that the top node became visible.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Added OnCascadeComplete event to TRzCheckTree. This event fires when a 
      node's check state changes and all parent nodes and child nodes have been 
      updated accordingly.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where OnNodeContextMenu would not get generated in the
      TRzCheckTree component.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where NodeFromPath method would not return correct node when
      non-English codepages were being used.
    * Fixed problem where pressing space key from within an Empty TRzCheckTree
      would cause an AV.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Added HighlightColor to TRzCheckTree to allow user to change check mark
      color.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem where FullExpand and FullCollapse did not work correctly
      when AutoSelect=True.
    * Fixed problem where correct node was not selected when RightClickSelect
      was set to True and user right-clicked a different node from the currently
      selected node.
    * Fixed problem where PopupMenu would not be displayed unless user right-
      clicked on a node.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
    * Enhanced TRzCheckTreeStrings.SaveTreeToStream and
      TRzCheckTreeStrings.LoadTreeFromStream methods so that the ImageIndex and
      SelectedIndex in addition to the StateIndex for each node is also saved.
      The index values can also be more than one digit long.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    << TRzCustomTreeView and TRzTreeView >>
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
    * Add FocusColor and DisabledColor properties.
    * Added the following new method:
      procedure UpdateStateIndexDisplay( Node: TTreeNode );

      This method is used after moving a node with the MoveTo method. The
      standard TTreeView component does not correctly update the display of a
      node that specifies a StateIndex.  More precisely, the state image
      associated with the index is displayed next to the node after it has been
      moved.  Call this method to ensure that the state image of the node (and
      all of its children) are displayed.

    << TRzCheckTree >>
    * Added the following new methods:
      procedure UpdateCascadingStates( Node: TTreeNode );
      procedure UpdateChildrenCascadingStates( ParentNode: TTreeNode );

      These methods are used after moving a node in a TRzCheckTree with the
      MoveTo method. These two methods need to be called after the move to
      ensure that the check states of the other nodes in the tree remain in
      sync.  Please note that these methods are only needed when CascadeChecks
      is True.

      Before moving a node in a TRzCheckTree, save the ParentNode of the node
      to be moved. After performing the move (and calling
      UpdateStateIndexDisplay--see above), then make the following calls:

      var
        Node, ParentNode: TTreeNode;
      begin
        Node := RzCheckTree1.Selected;
        ParentNode := Node.Parent;

        Node.MoveTo( RzCheckTree1.Items[ 0 ], naAdd );
        Node.Expand( False );

        RzCheckTree1.UpdateStateIndexDisplay( Node );
        RzCheckTree1.UpdateChildrenCascadingStates( ParentNode );
        RzCheckTree1.UpdateCascadingStates( Node );
      end;

    * Surfaced StateImages property.  This allows a user to provide custom
      images to be used for the check states.
    * Added the following methods: LoadFromFile, LoadFromStream, SaveToFile,
      SaveToStream.  These methods allow you to persist not only the items and
      their hierarchical relations, but also the states of each node in the
      tree.
===============================================================================}

{$I RzComps.inc}

unit RzTreeVw;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  CommCtrl,
  Menus,
  ImgList,
  RzCommon;

const
  RESOURCE_CHECKS   = 'RZTREEVW_CHECKS';
  STATE_UNCHECKED   = 1;
  STATE_CHECKED     = 2;
  STATE_PARTCHECKED = 3;


type
  TRzTvOnNodeContextMenuEvent = procedure( aSender: TObject; aNode: TTreeNode; var aPos: TPoint;
                                           var aMenu: TPopupMenu ) of object;

  TRzCustomTreeView = class( TCustomTreeView )
  private
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
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

    FAutoSelect: Boolean;
    FSelectionPen: TPen;
    FItemHeight: Integer;
    FItemHeightMargin: Integer;

    FRClickNode: TTreeNode;
    FOnNodeContextMenu: TRzTvOnNodeContextMenuEvent;
    FMenuAlreadyHandled: Boolean;
    FPathDelimiter: Char;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Internal Event Handlers }
    procedure PenChanged( Sender: TObject );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CMSysColorChange( var Msg: TMessage ); message cm_SysColorChange;
    procedure CNNotify( var Msg: TWMNotify ); message cn_Notify;
    procedure WMRButtonUp( var Msg: TWMRButtonUp ); message wm_RButtonUp;
    procedure WMContextMenu( var Msg: TMessage ); message wm_ContextMenu;
  protected
    FAboutInfo: TRzAboutInfo;
    FCanvas: TControlCanvas;
    FOverControl: Boolean;
    FRecreating: Boolean;

    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateItemHeight; virtual;
    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    procedure Collapse( Node: TTreeNode ); override;
    procedure Expand( Node: TTreeNode ); override;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    procedure DoPreNodeContextMenu; dynamic;
    procedure DoNodeContextMenu( Node: TTreeNode; P: TPoint ); dynamic;
    procedure KeyDown( var Key: Word;  ShiftState: TShiftState ); override;
    procedure NodeContextMenu( Node: TTreeNode; var Pos: TPoint; var Menu: TPopupMenu ); dynamic;

    function GetSelected: TTreeNode;
    procedure SetSelected( Value: TTreeNode );

    procedure LoadTreeFromList( List: TStrings ); virtual;
    procedure SaveTreeToList( List: TStrings ); virtual;

    { Property Access Methods }
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function StoreDisabledColor: Boolean;
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
    function GetAutoExpand: Boolean; virtual;
    procedure SetAutoExpand( Value: Boolean ); virtual;
    procedure SetAutoSelect( Value: Boolean ); virtual;
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
    procedure SetItemHeight( Value: Integer ); virtual;
    procedure SetItemHeightMargin( Value: Integer ); virtual;
    procedure SetSelectionPen( Value: TPen ); virtual;

    { Property Declarations }

    property AutoExpand: Boolean
      read GetAutoExpand
      write SetAutoExpand
      default False;

    property AutoSelect: Boolean
      read FAutoSelect
      write SetAutoSelect
      default False;

    property Color
      stored StoreColor
      default clWindow;

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

    property ItemHeight: Integer
      read FItemHeight
      write SetItemHeight
      default 0;

    property ItemHeightMargin: Integer
      read FItemHeightMargin
      write SetItemHeightMargin
      default 0;

    property PathDelimiter: Char
      read FPathDelimiter
      write FPathDelimiter
      default PathDelim;

    property SelectionPen: TPen
      read FSelectionPen
      write SetSelectionPen;

    { Inherited Properties & Events }
    property ParentColor default False;
    property TabStop default True;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

    property OnNodeContextMenu: TRzTvOnNodeContextMenuEvent
      read FOnNodeContextMenu
      write FOnNodeContextMenu;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;

    function NodeFromPath( Path: string ): TTreeNode;
    function PathFromNode( Node: TTreeNode ): string;
    procedure SelectByPath( const Path: string );

    procedure UpdateStateIndexDisplay( Node: TTreeNode );

    procedure FullCollapse;
    procedure FullExpand;

    {$IFDEF UNICODE}
    procedure LoadFromFile( const FileName: string ); overload;
    procedure LoadFromFile( const FileName: string; Encoding: TEncoding ); overload;
    procedure LoadFromStream( Stream: TStream ); overload;
    procedure LoadFromStream( Stream: TStream; Encoding: TEncoding ); overload;
    procedure SaveToFile( const FileName: string ); overload;
    procedure SaveToFile( const FileName: string; Encoding: TEncoding ); overload;
    procedure SaveToStream( Stream: TStream ); overload;
    procedure SaveToStream( Stream: TStream; Encoding: TEncoding ); overload;
    {$ELSE}
    procedure LoadFromFile( const FileName: string );
    procedure LoadFromStream( Stream: TStream );
    procedure SaveToFile( const FileName: string );
    procedure SaveToStream( Stream: TStream );
    {$ENDIF}

    procedure InvalidateNode( Node: TTreeNode; TextOnly: Boolean; EraseBkgnd: Boolean );

    property Selected: TTreeNode
      read GetSelected
      write SetSelected;
  end;


  TRzTreeView = class( TRzCustomTreeView )
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property SelectionPen;         { Surface SelectionPen declared in ancestor }

    { Inherited Properties and Events }
    property Align;
    property Anchors;
    property AutoExpand;
    property AutoSelect;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
    property HotTrack;
    property Images;
    property Indent;
    property ItemHeight;
    property ItemHeightMargin;
    property MultiSelect;
    property MultiSelectStyle;
    property PathDelimiter;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop;
    property ToolTips;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    {$IFDEF VCL140_OR_HIGHER}
    property OnHint;
    {$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnNodeContextMenu;
    property OnStartDock;
    property OnStartDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

    { Items must be published after OnGetImageIndex and OnGetSelectedIndex }
    property Items;
  end;



  {====================================}
  {== TRzCheckTree Class Declaration ==}
  {====================================}

  TRzCheckState = ( csUnknown, csUnchecked, csChecked, csPartiallyChecked );

  TRzCheckTreeChangingEvent = procedure( Sender: TObject; Node: TTreeNode; NewState: TRzCheckState;
                                         var AllowChange: Boolean ) of object;

  TRzCheckTreeChangeEvent = procedure( Sender: TObject; Node: TTreeNode; NewState: TRzCheckState ) of object;

  TRzCheckTreeCascadeCompleteEvent = procedure( Sender: TObject; Node: TTreeNode ) of object;

  TRzCheckTree = class( TRzCustomTreeView )
  private
    FSelectedItem: Integer;
    FBmpWidth: Integer;
    FImageWidth: Integer;
    FChangingState: Boolean;
    FSuspendCascades: Boolean;
    FCheckImages: TImageList;
    FCascadeChecks: Boolean;
    FSilentStateChanges: Boolean;
    FHighlightColor: TColor;

    FOnStateChanging: TRzCheckTreeChangingEvent;
    FOnStateChange: TRzCheckTreeChangeEvent;
    FOnUpdateChildren: TNotifyEvent;
    FOnCascadeComplete: TRzCheckTreeCascadeCompleteEvent;

    function GetItemState( AbsoluteIndex: Integer ): TRzCheckState;
    procedure SetItemState( AbsoluteIndex: Integer; Value: TRzCheckState );
    procedure SetNodeCheckState( Node:TTreeNode; NewState: TRzCheckState );
    procedure RecurseChildren( Node: TTreeNode; NodeChecked: Boolean );
    procedure SetAllChildren( Node: TTreeNode; NewState: TRzCheckState );

    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
  protected
    procedure Loaded; override;

    procedure UpdateItemHeight; override;
    procedure UpdateImageWidth; virtual;
    procedure InitStateImages; virtual;
    procedure UpdateParents( Node: TTreeNode; NodeChecked: Boolean ); virtual;
    procedure UpdateChildren( Node: TTreeNode; NodeChecked: Boolean ); virtual;
    procedure CascadeComplete( Node: TTreeNode ); dynamic;

    procedure LoadTreeFromList( List: TStrings ); override;
    procedure SaveTreeToList( List: TStrings ); override;
    { Event Dispatch Methods }
    function  CanChangeState( Node: TTreeNode; NewState: TRzCheckState ): Boolean; dynamic;
    procedure StateChange( Node: TTreeNode; NewState: TRzCheckState ); dynamic;

    procedure Delete( Node: TTreeNode ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure KeyUp( var Key: Word; Shift: TShiftState ); override;
    procedure WMChar( var Msg: TWMChar ); message wm_Char ;

    { Property Access Methods }
    function GetImages: TCustomImageList;
    procedure SetImages( Value: TCustomImageList );
    procedure SetFrameColor( Value: TColor ); override;
    procedure SetHighlightColor( Value: TColor );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ToggleCheckState( Node: TTreeNode );
    procedure ChangeNodeCheckState( Node: TTreeNode; NewState: TRzCheckState );
    procedure ForceCheckState( Node : TTreeNode; NewState: TRzCheckState );
    procedure SetAllNodes( NewState: TRzCheckState );

    procedure UpdateCascadingStates( Node: TTreeNode );
    procedure UpdateChildrenCascadingStates( ParentNode: TTreeNode );
    procedure UpdateStateFromChildren( ParentNode, DeletedNode: TTreeNode );

    property ItemState[ Index: Integer ]: TRzCheckState
      read GetItemState
      write SetItemState;

    property SilentCheckChanges: Boolean
      read FSilentStateChanges
      write FSilentStateChanges;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property CascadeChecks: Boolean
      read FCascadeChecks
      write FCascadeChecks
      default True;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property Images: TCustomImageList
      read GetImages
      write SetImages;

    property OnCascadeComplete: TRzCheckTreeCascadeCompleteEvent
      read FOnCascadeComplete
      write FOnCascadeComplete;

    property OnStateChanging: TRzCheckTreeChangingEvent
      read FOnStateChanging
      write FOnStateChanging;

    property OnStateChange: TRzCheckTreeChangeEvent
      read FOnStateChange
      write FOnStateChange;

    property OnUpdateChildren: TNotifyEvent
      read FOnUpdateChildren
      write FOnUpdateChildren;

    { Inherited Properties and Events }
    property Align;
    property Anchors;
    property AutoExpand;
    property AutoSelect;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
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
    property HotTrack;
    property Indent;
    property ItemHeight;
    property ItemHeightMargin;
    property MultiSelect;
    property MultiSelectStyle;
    property PathDelimiter;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property RightClickSelect;
    property RowSelect;
    property SelectionPen;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnAddition;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnCancelEdit;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    property OnContextPopup;
    property OnCreateNodeClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    {$IFDEF VCL140_OR_HIGHER}
    property OnHint;
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
    property OnNodeContextMenu;
    property OnStartDock;
    property OnStartDrag;

    // Items must be published after OnGetImageIndex and OnGetSelectedIndex
    property Items;
  end;


implementation

uses
  {&RAS}
  Themes,
  RzShellUtils,
  RzCommonBitmaps,
  TypInfo,
  ComStrs;


{&RT}
{===============================}
{== TRzCustomTreeView Methods ==}
{===============================}

constructor TRzCustomTreeView.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;

  FSelectionPen := TPen.Create;
  FSelectionPen.Color := clBtnShadow;
  FSelectionPen.Style := psSolid;
  FSelectionPen.OnChange := PenChanged;

  FDisabledColor := clBtnFace;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameControllerNotifications := fccAll;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
                           
  FPathDelimiter := PathDelim;
  
  TabStop := True;
  ParentColor := False;
end;


destructor TRzCustomTreeView.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  FSelectionPen.Free;

  inherited;
end;


procedure TRzCustomTreeView.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzCustomTreeView.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzCustomTreeView.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzCustomTreeView.Loaded;
begin
  inherited;
  UpdateItemHeight;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzCustomTreeView.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;



function DoAutoScroll( ctl: TWinControl; code, direction: Integer ): Boolean;
const
  conversion: array[ SB_HORZ..SB_VERT ] of Integer = ( WM_HSCROLL, WM_VSCROLL );
var
  info: TScrollInfo;
begin
  ZeroMemory( @info, Sizeof( info ) );
  info.cbSize := Sizeof( TScrollInfo );
  info.fMask := SIF_ALL;
  Windows.GetScrollInfo( ctl.Handle, code, info );
  if ( info.nMax - info.nMin - Integer( info.nPage ) ) > 0 then
    ctl.Perform( conversion[ code ], direction, 0 );
  Result := info.nPos <> Windows.GetScrollPos( ctl.Handle, code );
end;


function DoWheelScroll( aCtl: TWinControl; aSBar, aShort, aLong: DWORD ): Boolean;
var
  lines: DWORD;
begin
  if Mouse.WheelScrollLines = -1 then
    lines := WHEEL_PAGESCROLL
  else
    lines := Mouse.WheelScrollLines;

  if lines = WHEEL_PAGESCROLL then
    DoAutoScroll( aCtl, aSBar, aLong )
  else
  begin
    if aSBar = SB_HORZ then
      lines := 1;
    while ( ( lines and $FF ) <> 0 ) do     // the "and $FF" is just a sanity check
    begin
      DoAutoScroll( aCtl, aSBar, aShort );
      Dec( lines );
    end;
  end;
  Result := True;
end;                                        {DoWheelScroll}


function TRzCustomTreeView.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  Result := DoWheelScroll( Self, SB_VERT, SB_LINEDOWN, SB_PAGEDOWN );
end;


function TRzCustomTreeView.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  Result := DoWheelScroll( Self, SB_VERT, SB_LINEUP, SB_PAGEUP );
end;


function TRzCustomTreeView.GetAutoExpand: Boolean;
begin
  Result := inherited AutoExpand;
end;


procedure TRzCustomTreeView.SetAutoExpand( Value: Boolean );
begin
  inherited AutoExpand := Value;
  if AutoExpand then
    FAutoSelect := False;
end;


procedure TRzCustomTreeView.SetAutoSelect( Value: Boolean );
begin
  if FAutoSelect <> Value then
  begin
    FAutoSelect := Value;
    if FAutoSelect then
      AutoExpand := False;
  end;
end;


procedure TRzCustomTreeView.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if not FUpdatingColor then
  begin
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzCustomTreeView.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzCustomTreeView.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzCustomTreeView.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzCustomTreeView.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzCustomTreeView.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzCustomTreeView.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzCustomTreeView.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomTreeView.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomTreeView.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomTreeView.SetFrameHotTrack( Value: Boolean );
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
    RepaintFrame;
    Invalidate;
  end;
end;


procedure TRzCustomTreeView.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomTreeView.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomTreeView.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomTreeView.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    FRecreating := True;
    try
      RecreateWnd;              { Must recreate window so Ctl3D border reappears }
    finally
      FRecreating := False;
    end;
  end;
end;


procedure TRzCustomTreeView.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzCustomTreeView.UpdateItemHeight;
begin
  if HandleAllocated then
  begin
    if FItemHeight < 1 then
      FItemHeight := 0;

    if FItemHeight = 0 then
    begin
      if FItemHeightMargin = 0 then
        SendMessage( Handle, tvm_SetItemHeight, -1, 0 )
      else
        SendMessage( Handle, tvm_SetItemHeight, GetMinFontHeight( Font ) + 2 * FItemHeightMargin, 0 );
    end
    else
      SendMessage( Handle, tvm_SetItemHeight, FItemHeight, 0 );
  end;
end;


procedure TRzCustomTreeView.SetItemHeight( Value: Integer );
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    UpdateItemHeight;
  end;
end;


procedure TRzCustomTreeView.SetItemHeightMargin( Value: Integer );
begin
  if FItemHeightMargin <> Value then
  begin
    FItemHeightMargin := Value;
    UpdateItemHeight;
  end;
end;


procedure TRzCustomTreeView.SetSelectionPen( Value: TPen );
begin
  FSelectionPen.Assign( Value );
  Invalidate;
end;


procedure TRzCustomTreeView.PenChanged( Sender: TObject );
begin
  Invalidate;
end;


procedure TRzCustomTreeView.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, Rect( 0, 0, Width, Height ) );
end;


function TRzCustomTreeView.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzCustomTreeView.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzCustomTreeView.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
begin
  inherited;                       { Must call inherited so scroll bar show up }

  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( Focused or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzCustomTreeView.WMNCPaint =}


{===============================================================================
  TRzCustomTreeView.WMPaint

  Description
    The FCanvas "control canvas" is used to draw a solid rectangle around the
    text of the selected node when the tree view does *not* have the focus.
===============================================================================}

procedure TRzCustomTreeView.WMPaint( var Msg: TWMPaint );
var
  R: TRect;
  FocusWnd, EditWnd: HWND;
begin
  inherited;

  FocusWnd := GetFocus;
  EditWnd := TreeView_GetEditControl( Handle );

  if not HideSelection and not ( Focused or ( FocusWnd = EditWnd ) ) and
     ( Selected <> nil ) and not UsingSystemStyle then
  begin
    FCanvas.Handle := Msg.DC;                 { Map canvas onto device context }
    try
      R := Selected.DisplayRect( True );

      FCanvas.Pen := FSelectionPen;
      FCanvas.Brush.Style := bsClear;
      FCanvas.Rectangle( R.Left, R.Top, R.Right, R.Bottom );
      FCanvas.Pen.Width := 1;
      FCanvas.Pen.Style := psSolid;
    finally
      FCanvas.Handle := 0;
    end;
  end;
end;


procedure TRzCustomTreeView.UpdateColors;
begin
  if ( csLoading in ComponentState ) or ( not UsingSystemStyle ) then
    Exit;

  FUpdatingColor := True;
  try
    if not Enabled then
      Color := FDisabledColor
    else if Focused then
      Color := FFocusColor
    else
      Color := FNormalColor;
  finally
    FUpdatingColor := False;
  end;
end;


procedure TRzCustomTreeView.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzCustomTreeView.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;

procedure TRzCustomTreeView.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzCustomTreeView.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzCustomTreeView.CMMouseLeave( var Msg: TMessage );
begin
  {&RV}
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzCustomTreeView.Collapse( Node: TTreeNode );
begin
  if FAutoSelect then
    Node.Selected := True;
  inherited;
end;


procedure TRzCustomTreeView.Expand( Node: TTreeNode );
begin
  inherited;
  if FAutoSelect then
    Node.Selected := True;
end;


procedure TRzCustomTreeView.FullCollapse;
var
  SaveAutoSelect: Boolean;
begin
  SaveAutoSelect := FAutoSelect;
  FAutoSelect := False;
  inherited;
  FAutoSelect := SaveAutoSelect;
end;


procedure TRzCustomTreeView.FullExpand;
var
  SaveAutoSelect: Boolean;
begin
  SaveAutoSelect := FAutoSelect;
  FAutoSelect := False;
  inherited;
  FAutoSelect := SaveAutoSelect;
end;


procedure TRzCustomTreeView.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzCustomTreeView.PathFromNode( Node: TTreeNode ): string;
begin
  if Node <> nil then
  begin
    Result := Node.Text + FPathDelimiter;
    while Node.Parent <> nil do
    begin
      Node := Node.Parent;
      Result := Node.Text + FPathDelimiter + Result;
    end;
  end
  else
    Result := '';
end;


function TRzCustomTreeView.NodeFromPath( Path: string ): TTreeNode;
var
  OldCursor: TCursor;
  I: Integer;
  Found: Boolean;
  Node, SearchNode, MatchingNode: TTreeNode;
  FindPath: string;
  Delim: Char;
begin
  Result := nil;

  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if LastChar( Path ) <> FPathDelimiter then
      Path := Path + FPathDelimiter;
    Path := AnsiUpperCase( Path );

    Node := nil;
    if FPathDelimiter = #0 then
      Delim := PathDelim
    else
      Delim := FPathDelimiter;

    for I := 1 to CountChar( Delim, Path ) do
    begin
      FindPath := CopyEx( Path, 1, Delim, I );
      MatchingNode := nil;
      if Items.Count > 0 then
      begin
        if Node <> nil then
          SearchNode := Node.GetFirstChild
        else
          SearchNode := Items[ 0 ];

        Found := False;
        while not Found and ( SearchNode <> nil ) do
        begin
          if AnsiUpperCase( PathFromNode( SearchNode ) ) = FindPath then
          begin
            MatchingNode := SearchNode;
            Found := True;
          end;
          SearchNode := SearchNode.GetNextSibling;
        end;
      end;

      Node := MatchingNode;
      if Node = nil then
        Exit;
    end;
    Result := Node;
  finally
    Screen.Cursor := OldCursor;
  end;
end;


procedure TRzCustomTreeView.SelectByPath( const Path: string );
var
  Node: TTreeNode;
begin
  Node := NodeFromPath( Path );
  if Node <> nil then
    Node.Selected := True;
end;


procedure TRzCustomTreeView.UpdateStateIndexDisplay( Node: TTreeNode );
var
  I: Integer;
  Item: TTVItem;
  Value: Integer;
begin
  Value := Node.StateIndex;
  if Value >= 0 then
    Dec( Value );
  with Item do
  begin
    mask := TVIF_STATE or TVIF_HANDLE;
    stateMask := TVIS_STATEIMAGEMASK;
    hItem := Node.ItemId;
    state := IndexToStateImageMask( Value + 1 );
  end;
  TreeView_SetItem( Node.TreeView.Handle, Item );

  for I := 0 to Node.Count - 1 do
  begin
    if Node.Item[ I ].HasChildren then
      UpdateStateIndexDisplay( Node.Item[ I ] )
    else
    begin
      Value := Node.Item[ I ].StateIndex;
      if Value >= 0 then
        Dec( Value );
      with Item do
      begin
        mask := TVIF_STATE or TVIF_HANDLE;
        stateMask := TVIS_STATEIMAGEMASK;
        hItem := Node.Item[ I ].ItemId;
        state := IndexToStateImageMask( Value + 1 );
      end;
      TreeView_SetItem( Node.TreeView.Handle, Item );
    end;
  end;
end;


procedure TRzCustomTreeView.InvalidateNode( Node: TTreeNode; TextOnly: Boolean; EraseBkgnd: Boolean );
var
  R: TRect;
begin
  R := Node.DisplayRect( TextOnly );
  InvalidateRect( Handle, @R, EraseBkgnd );
end;


procedure TRzCustomTreeView.CMSysColorChange( var Msg: TMessage );
begin
  inherited;
  if Color < 0 then
    Perform( cm_ColorChanged, Msg.wParam, Msg.lParam );
end;


procedure TRzCustomTreeView.CNNotify( var Msg: TWMNotify );
var
  Node: TTreeNode;
  P: TPoint;
  Mnu: TPopupMenu;
  OldGetImageEvent: TTVExpandedEvent;
  OldGetSelectedImageEvent: TTVExpandedEvent;

  function GetNodeFromItem( const Item: TTVItem ): TTreeNode;
  begin
    with Item do
      if (state and TVIF_PARAM) <> 0 then
        Result := Pointer(lParam)
      else
        Result := Items.GetNode(hItem);
  end;

begin {= TRzCustomTreeView.CNNotify =}
  with Msg.NMHdr^ do
    case Code of

      tvn_GetDispInfo:
      begin
        with PTVDispInfo( Pointer( Msg.NMHdr ) )^ do
        begin
          Node := GetNodeFromItem( Item );

          if Assigned( Node ) then
          begin
            if (item.mask and TVIF_IMAGE) <> 0 then
            begin
              GetImageIndex( Node );
              Item.iImage := Node.ImageIndex;
            end;

            if ( Item.mask and tvif_SelectedImage ) <> 0 then
            begin
              GetSelectedIndex( Node );
              Item.iSelectedImage := Node.SelectedIndex;
            end;
          end;

          oldGetImageEvent := OnGetImageIndex;
          oldGetSelectedImageEvent := OnGetSelectedIndex;
          OnGetImageIndex:=nil;
          OnGetSelectedIndex:=nil;
          try
            inherited;
          finally
            OnGetImageIndex := oldGetImageEvent;
            OnGetSelectedIndex := oldGetSelectedImageEvent;
          end;
        end;
      end;

      nm_RClick:
      begin
        // Note: The RightClickSelect property introduced in Delphi 3 can do some of this. We don't use it
        //       in order to maintain Delphi 2 and C++Builder compatibility.
        if not (csDesigning in ComponentState) then
        begin
          GetCursorPos( p );
          p := ScreenToClient( p );
          FRClickNode := GetNodeAt( p.x, p.y );
          if not Assigned( FRClickNode ) then
            FRClickNode := inherited Selected;
          if Assigned( FRClickNode ) then
          begin
            mnu := PopupMenu; // Default is normal popup
            NodeContextMenu( FRClickNode, p, mnu );
            if Assigned( mnu ) then
              with ClientToScreen( p ) do
              begin
                SendCancelMode( nil );
                mnu.PopupComponent := self;
                mnu.Popup( x, y );
              end;
            FRClickNode := nil;
            FMenuAlreadyHandled := TRUE;
          end;
        end;

        inherited;
      end;

      else
        inherited;
    end; {case}
end; {TRzCustomTreeView.CNNotify}


procedure TRzCustomTreeView.WMRButtonUp( var Msg: TWMRButtonUp );
var
  OldAutoPopup: Boolean;
begin
  if FMenuAlreadyHandled and Assigned(PopupMenu) then
  begin
    OldAutoPopup := PopupMenu.AutoPopup;
    PopupMenu.AutoPopup := FALSE;
    try
      inherited;
    finally
      PopupMenu.AutoPopup := OldAutoPopup;
      FMenuAlreadyHandled := FALSE;
    end;
  end
  else
    inherited;
end;


procedure TRzCustomTreeView.WMContextMenu( var Msg: TMessage );
begin
  if not ( csDesigning in ComponentState ) and not Assigned( Selected ) and not FMenuAlreadyHandled then
  begin
    if Msg.lParam = -1 then
      DoPreNodeContextMenu
    else
      DoNodeContextMenu( Selected, ScreenToClient( Point( Msg.lParamLo, Msg.lParamHi ) ) );
  end;
end;


// Work around a bug with tooltips in NT4. We just disable them. The bug was fixed around v4.72 of
// comctl32.dll so we don't disable the tooltips for this and later versions.

procedure TRzCustomTreeView.CreateParams( var Params: TCreateParams );
const
  TVS_NOTOOLTIPS = $0080;  // comctl32.dll v4.70
begin
  inherited;

  if IsWinNT and ( RzShellUtils.COMCTL32_VER.version < COMCTL32_VER472 ) then
    Params.Style := Params.Style or TVS_NOTOOLTIPS;
end;


procedure TRzCustomTreeView.CreateWnd;
begin
  inherited;
  UpdateItemHeight;
end;


procedure TRzCustomTreeView.DoPreNodeContextMenu;
var
  P: TPoint;

  procedure DoDefault;
  begin
    if Assigned( PopupMenu ) then
    begin
      SendCancelMode( nil );
      PopupMenu.PopupComponent := Self;
      with ClientToScreen( Point( 0, 0 ) ) do
        PopupMenu.Popup( X, Y );
    end;
  end;

begin
  if Assigned( Selected ) then
  begin
    with Selected.DisplayRect( True ) do
      P := Point( ( Left + Right) div 2, ( Bottom + Top ) div 2 )
  end
  else
  begin
    DoDefault;
    Exit;
  end;
  DoNodeContextMenu( Selected, p );
end; {= TRzCustomTreeView.DoPreNodeContextMenu =}


procedure TRzCustomTreeView.DoNodeContextMenu( Node: TTreeNode; P: TPoint );
var
  Menu: TPopupMenu;
begin
  Menu := PopupMenu; // Default to normal popup
  NodeContextMenu( Node, P, Menu );
  if Menu <> PopupMenu then
    FMenuAlreadyHandled := True;
  if Assigned( Menu ) then
  begin
    SendCancelMode( nil );
    Menu.PopupComponent := Self;
    with ClientToScreen( P ) do
      Menu.Popup( X, Y );
  end;
end;


procedure TRzCustomTreeView.KeyDown( var Key: Word; ShiftState: TShiftState );
begin
  if ( ( Key = VK_APPS ) and ( ShiftState = [] ) ) or
     ( ( Key = VK_F10 ) and ( ShiftState = [ ssShift ] ) ) then
  begin
    Key := 0;
    DoPreNodeContextMenu;
  end;
  inherited;
end;


procedure TRzCustomTreeView.NodeContextMenu( Node: TTreeNode; var Pos: TPoint; var Menu: TPopupMenu );
begin
  if Assigned( FOnNodeContextMenu ) then
    FOnNodeContextMenu( Self, Node, Pos, Menu );
end;


function TRzCustomTreeView.GetSelected: TTreeNode;
begin
  if HandleAllocated then
  begin
    if RightClickSelect and Assigned( FRClickNode ) then
      Result := FRClickNode
    else
      Result := Items.GetNode( TreeView_GetSelection( Handle ) );
  end
  else
    Result := nil;
end;


procedure TRzCustomTreeView.SetSelected( Value: TTreeNode );
begin
  inherited Selected := Value;
end;


procedure TRzCustomTreeView.LoadTreeFromList( List: TStrings );
var
  Node, NextNode: TTreeNode;
  Level, I: Integer;
  CurrStr: string;

  function GetBufStart( Buffer: PChar; var Level: Integer ): PChar;
  begin
    Level := 0;
    while CharInSet( Buffer^, [ ' ', #9 ] ) do
    begin
      Inc( Buffer );
      Inc( Level );
    end;
    Result := Buffer;
  end;

begin
  Items.BeginUpdate;
  try
    try
      Items.Clear;
      Node := nil;
      for I := 0 to List.Count - 1 do
      begin
        CurrStr := GetBufStart( PChar( List[ I ] ), Level );
        if Node = nil then
          Node := Items.AddChild( nil, CurrStr )
        else if Node.Level = Level then
          Node := Items.AddChild( Node.Parent, CurrStr )
        else if Node.Level = ( Level - 1 ) then
          Node := Items.AddChild( Node, CurrStr )
        else if Node.Level > Level then
        begin
          NextNode := Node.Parent;
          while NextNode.Level > Level do
            NextNode := NextNode.Parent;
          Node := Items.AddChild( NextNode.Parent, CurrStr );
        end
        else
          raise ETreeViewError.CreateFmt( sInvalidLevelEx, [ Level, CurrStr ] );
      end;
    finally
      Items.EndUpdate;
    end;
  except
    Invalidate;  // force repaint on exception
    raise;
  end;
end;


procedure TRzCustomTreeView.LoadFromFile( const FileName: string );
{$IFNDEF UNICODE}
var
  List: TStringList;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  LoadFromFile( FileName, nil );
  {$ELSE}
  List := TStringList.Create;
  try
    List.LoadFromFile( FileName );
    LoadTreeFromList( List );
  finally
    List.Free;
  end;
  {$ENDIF}
end;


{$IFDEF UNICODE}

procedure TRzCustomTreeView.LoadFromFile( const FileName: string; Encoding: TEncoding );
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.LoadFromFile( FileName, Encoding );
    LoadTreeFromList( List );
  finally
    List.Free;
  end;
end;

{$ENDIF}


procedure TRzCustomTreeView.LoadFromStream( Stream: TStream );
{$IFNDEF UNICODE}
var
  List: TStringList;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  LoadFromStream( Stream, nil );
  {$ELSE}
  List := TStringList.Create;
  try
    List.LoadFromStream( Stream );
    LoadTreeFromList( List );
  finally
    List.Free;
  end;
  {$ENDIF}
end;


{$IFDEF UNICODE}

procedure TRzCustomTreeView.LoadFromStream( Stream: TStream; Encoding: TEncoding );
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.LoadFromStream( Stream, Encoding );
    LoadTreeFromList( List );
  finally
    List.Free;
  end;
end;

{$ENDIF}


procedure TRzCustomTreeView.SaveTreeToList( List: TStrings );
const
  TabChar = #9;
var
  I: Integer;
  Node: TTreeNode;
  NodeStr: string;
begin
  if Items.Count > 0 then
  begin
    Node := Items[ 0 ];
    while Node <> nil do
    begin
      NodeStr := '';
      for I := 0 to Node.Level - 1 do
        NodeStr := NodeStr + TabChar;
      NodeStr := NodeStr + Node.Text;
      List.Add( NodeStr );
      Node := Node.GetNext;
    end;
  end;
end;


procedure TRzCustomTreeView.SaveToFile( const FileName: string );
{$IFNDEF UNICODE}
var
  List: TStringList;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  SaveToFile( FileName, nil );
  {$ELSE}
  List := TStringList.Create;
  try
    SaveTreeToList( List );
    List.SaveToFile( FileName );
  finally
    List.Free;
  end;
  {$ENDIF}
end;


{$IFDEF UNICODE}

procedure TRzCustomTreeView.SaveToFile( const FileName: string; Encoding: TEncoding );
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    SaveTreeToList( List );
    List.SaveToFile( FileName, Encoding );
  finally
    List.Free;
  end;
end;

{$ENDIF}


procedure TRzCustomTreeView.SaveToStream( Stream: TStream );
{$IFNDEF UNICODE}
var
  List: TStringList;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  SaveToStream( Stream, nil );
  {$ELSE}
  List := TStringList.Create;
  try
    SaveTreeToList( List );
    List.SaveToStream( Stream );
  finally
    List.Free;
  end;
  {$ENDIF}
end;


{$IFDEF UNICODE}

procedure TRzCustomTreeView.SaveToStream( Stream: TStream; Encoding: TEncoding );
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    SaveTreeToList( List );
    List.SaveToStream( Stream, Encoding );
  finally
    List.Free;
  end;
end;

{$ENDIF}


{=======================================================}
{== TRzCheckTreeStrings Class Declaration and Methods ==}
{=======================================================}

procedure TreeViewError( const Msg: string );
begin
  raise ETreeViewError.Create( Msg );
end;

procedure TreeViewErrorFmt( const Msg: string; Format: array of const );
begin
  raise ETreeViewError.CreateFmt( Msg, Format );
end;


type
  TRzCheckTreeStrings = class( TStrings )
  private
    FOwner: TTreeNodes;
  protected
    function Get( Index: Integer ): string; override;
    function GetBufStart( Buffer: PChar; var Level: Integer ): PChar;
    function GetCount: Integer; override;
    function GetObject( Index: Integer ): TObject; override;
    procedure PutObject( Index: Integer; AObject: TObject ); override;
    procedure SetUpdateState( Updating: Boolean ); override;
  public
    constructor Create( AOwner: TTreeNodes );

    function Add( const S: string ): Integer; override;
    procedure Clear; override;
    procedure Delete( Index: Integer ); override;
    procedure Insert( Index: Integer; const S: string ); override;
    property Owner: TTreeNodes
      read FOwner;
  end;

constructor TRzCheckTreeStrings.Create( AOwner: TTreeNodes );
begin
  inherited Create;
  FOwner := AOwner;
end;


function TRzCheckTreeStrings.Get( Index: Integer ): string;
const
  TabChar = #9;
var
  Level, I: Integer;
  Node: TTreeNode;
begin
  Result := '';
  Node := Owner.Item[ Index ];
  Level := Node.Level;
  for I := 0 to Level - 1 do
    Result := Result + TabChar;
  Result := Result + Node.Text;
end;


function TRzCheckTreeStrings.GetBufStart( Buffer: PChar; var Level: Integer ): PChar;
begin
  Level := 0;
  while CharInSet( Buffer^, [ ' ', #9 ] ) do
  begin
    Inc( Buffer );
    Inc( Level );
  end;
  Result := Buffer;
end;


function TRzCheckTreeStrings.GetObject( Index: Integer ): TObject;
begin
  Result := Owner.Item[ Index ].Data;
end;


procedure TRzCheckTreeStrings.PutObject( Index: Integer; AObject: TObject );
begin
  Owner.Item[ Index ].Data := AObject;
end;


function TRzCheckTreeStrings.GetCount: Integer;
begin
  Result := Owner.Count;
end;


procedure TRzCheckTreeStrings.Clear;
begin
  Owner.Clear;
end;


procedure TRzCheckTreeStrings.Delete( Index: Integer );
begin
  Owner.Item[ Index ].Delete;
end;


procedure TRzCheckTreeStrings.SetUpdateState( Updating: Boolean );
begin
  SendMessage( Owner.Handle, WM_SETREDRAW, Ord( not Updating ), 0 );
  if not Updating then
    Owner.Owner.Refresh;
end;


function TRzCheckTreeStrings.Add( const S: string ): Integer;
var
  Level, OldLevel, I: Integer;
  NewStr: string;
  Node: TTreeNode;
begin
  Result := GetCount;
  if ( Length( S ) = 1 ) and ( S[ 1 ] = Chr( $1A ) ) then
    Exit;
  Node := nil;
  OldLevel := 0;
  NewStr := GetBufStart( PChar( S ), Level );
  if Result > 0 then
  begin
    Node := Owner.Item[ Result - 1 ];
    OldLevel := Node.Level;
  end;
  if ( Level > OldLevel ) or ( Node = nil ) then
  begin
    if Level - OldLevel > 1 then
      TreeViewError( sInvalidLevel );
  end
  else begin
    for I := OldLevel downto Level do
    begin
      Node := Node.Parent;
      if ( Node = nil ) and ( I - Level > 0 ) then
        TreeViewError( sInvalidLevel );
    end;
  end;
  Owner.AddChild( Node, NewStr );
end;


procedure TRzCheckTreeStrings.Insert( Index: Integer; const S: string );
begin
  Owner.Insert( Owner.Item[ Index ], S );
end;



{==========================}
{== TRzCheckTree Methods ==}
{==========================}

constructor TRzCheckTree.Create( AOwner: TComponent );
begin
  inherited;

  FAutoSelect := False;
  FHighlightColor := clHighlight;
  FCheckImages := TImageList.Create( Self );
  FCheckImages.Name := 'CheckImages';
  StateImages := FCheckImages;
  InitStateImages;
  FBmpWidth := FCheckImages.Width;

  ReadOnly := True;
  FSuspendCascades := False;
  FCascadeChecks := True;
  FSilentStateChanges := False;
  {&RCI}
end;


procedure TRzCheckTree.InitStateImages;
var
  R, CheckRect: TRect;
  ElementDetails: TThemedElementDetails;
  ChkBmp, ImgBmp: TBitmap;
  H, W, Y: Integer;

  function CheckColor( Value: TColor ): TColor;
  begin
    if ( ColorToRGB( Value ) = ColorToRGB( clOlive ) ) or
       ( ColorToRGB( Value ) = ColorToRGB( clGray ) ) then
    begin
      Result := ColorToRGB( Value ) + 1;
    end
    else
      Result := Value;
  end;


begin
  FCheckImages.Clear;

  H := Max( 16, FItemHeight );
  W := 20;
  FCheckImages.Height := H;
  FCheckImages.Width := W;


  ChkBmp := TBitmap.Create;
  try
    ChkBmp.Width := W;
    ChkBmp.Height := H;

    R := Rect( 0, 0, W, H );
    CheckRect := Rect( 0, 0, 14, 14 );

    if UsingSystemStyle then
      ChkBmp.Canvas.Brush.Color := Color
    else
      ChkBmp.Canvas.Brush.Color := ActiveStyleColor( scTreeView );

    ChkBmp.Canvas.FillRect( R );

    if ActiveStyleServicesEnabled then
    begin
      ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxUncheckedNormal );
      ActiveStyleServices.DrawElement( ChkBmp.Canvas.Handle, ElementDetails, R );
      FCheckImages.Add( ChkBmp, nil );

      ActiveStyleServices.DrawElement( ChkBmp.Canvas.Handle, ElementDetails, R );
      FCheckImages.Add( ChkBmp, nil );

      ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxCheckedNormal );
      ActiveStyleServices.DrawElement( ChkBmp.Canvas.Handle, ElementDetails, R );
      FCheckImages.Add( ChkBmp, nil );

      ElementDetails := ActiveStyleServices.GetElementDetails( tbCheckBoxMixedNormal );
      ActiveStyleServices.DrawElement( ChkBmp.Canvas.Handle, ElementDetails, R );
      FCheckImages.Add( ChkBmp, nil );

    end
    else // No Themes, but use HotTrack style check boxes
    begin
      ImgBmp := TBitmap.Create;
      try
        ImgBmp.Width := W;
        ImgBmp.Height := H;
        Y := ( H - 13 ) div 2;
        ImgBmp.Canvas.Brush.Color := clOlive;
        ImgBmp.Canvas.FillRect( R );

        DrawCheckBox( ChkBmp.Canvas, CheckRect, cbUnchecked, bdsNormal, False,
                      htsInterior, FFrameColor, FHighlightColor, clWindow,
                      clLime, DisabledColor, clLime, clRed, False, False, clWindow );

        ImgBmp.Canvas.Draw( 2, Y, ChkBmp );

        // Add once for No Image
        FCheckImages.AddMasked( ImgBmp, clOlive );
        // Add again for Unchecked
        FCheckImages.AddMasked( ImgBmp, clOlive );

        DrawCheckBox( ChkBmp.Canvas, CheckRect, cbChecked, bdsNormal, False,
                      htsInterior, FFrameColor, FHighlightColor, clWindow,
                      clLime, DisabledColor, clLime, clRed, False, False, clWindow );
        ImgBmp.Canvas.Draw( 2, Y, ChkBmp );
        FCheckImages.AddMasked( ImgBmp, clOlive );

        DrawCheckBox( ChkBmp.Canvas, CheckRect, cbGrayed, bdsNormal, False,
                      htsInterior, FFrameColor, FHighlightColor, clWindow,
                      clLime, DisabledColor, clLime, clRed, False, False, clWindow );
        ImgBmp.Canvas.Draw( 2, Y, ChkBmp );
        FCheckImages.AddMasked( ImgBmp, clOlive );
      finally
        ImgBmp.Free;
      end;
    end;
  finally
    ChkBmp.Free;
  end;
end; {= TRzCheckTree.InitStateImages =}



destructor TRzCheckTree.Destroy;
begin
  FCheckImages.Free;
  inherited;
end;


procedure TRzCheckTree.Loaded;
begin
  inherited;

  UpdateImageWidth;
  {&RV}
end;


procedure TRzCheckTree.UpdateItemHeight;
begin
  inherited;
  InitStateImages;
end;


procedure TRzCheckTree.UpdateImageWidth;
begin
  if Images = nil then
    FImageWidth := 0
  else
    FImageWidth := Images.Width;
end;


procedure TRzCheckTree.SetFrameColor( Value: TColor );
begin
  if FrameColor <> Value then
  begin
    inherited SetFrameColor( Value );
    InitStateImages;
    Invalidate;
  end;
end;


procedure TRzCheckTree.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    InitStateImages;
    Invalidate;
  end;
end;


procedure TRzCheckTree.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  InitStateImages;
end;


procedure TRzCheckTree.WMPaint( var Msg: TWMPaint );
var
  I: Integer;
begin
  // Since we cannot hook into the TreeNodes themselves, we will hook
  // into the paint processing to ensure that all nodes have their
  // StateIndex set to a valid value.
  for I := 0 to Items.Count - 1 do
  begin
    if Items[ I ].StateIndex = -1 then
      Items[ I ].StateIndex := Ord( csUnchecked );
  end;
  inherited;
end;


function TRzCheckTree.GetItemState( AbsoluteIndex: Integer ): TRzCheckState;
begin
  Result := TRzCheckState( Items[ AbsoluteIndex ].StateIndex );
end;


procedure TRzCheckTree.SetItemState( AbsoluteIndex: Integer; Value: TRzCheckState );
begin
  if TRzCheckState( Items[ AbsoluteIndex ].StateIndex ) <> Value then
    ChangeNodeCheckState( Items[ AbsoluteIndex ], Value );
end;


procedure TRzCheckTree.SetNodeCheckState( Node: TTreeNode; NewState: TRzCheckState );
begin
  if Node.StateIndex <> Ord( NewState ) then
  begin
    if CanChangeState( Node, NewState ) then
    begin
      Node.StateIndex := Ord( NewState );
      if not FSilentStateChanges then
        StateChange( Node, NewState );
    end;
  end;
end;


function TRzCheckTree.CanChangeState( Node: TTreeNode; NewState: TRzCheckState ): Boolean;
begin
  Result := True;
  if not FSilentStateChanges and Assigned( FOnStateChanging ) then
    FOnStateChanging( Self, Node, NewState, Result );
end;


procedure TRzCheckTree.StateChange( Node: TTreeNode; NewState: TRzCheckState );
begin
  if Assigned( FOnStateChange ) then
    FOnStateChange( Self, Node, NewState );
end;


// Public method used to set a node and potentially parents in code

procedure TRzCheckTree.ForceCheckState( Node: TTreeNode;
                                        NewState: TRzCheckState );
begin
  if Node.StateIndex <> Ord( NewState ) then
  begin
    Node.StateIndex := Ord( NewState );
    if not FSilentStateChanges then
      StateChange( Node, NewState );
  end;
end;


// Toggles state and cascades throughout tree
// The check state is actually stored in the StateIndex field

procedure TRzCheckTree.ToggleCheckState( Node: TTreeNode );
begin
  FChangingState := False;
  if Node.StateIndex = 0 then
    Exit;

  if Node.StateIndex = STATE_CHECKED then
    SetNodeCheckState( Node, csUnchecked )
  else
    SetNodeCheckState( Node, csChecked );

  if FCascadeChecks then
  begin
    UpdateChildren( Node, Node.StateIndex = STATE_CHECKED );
    UpdateParents( Node, Node.StateIndex = STATE_CHECKED );
  end;

  CascadeComplete( Node );
end;


procedure TRzCheckTree.CascadeComplete( Node: TTreeNode );
begin
  if Assigned( FOnCascadeComplete ) then
    FOnCascadeComplete( Self, Node );
end;


procedure TRzCheckTree.UpdateCascadingStates( Node: TTreeNode );
begin
  if FCascadeChecks then
  begin
    if ( Node.StateIndex = STATE_CHECKED ) or ( Node.StateIndex = STATE_UNCHECKED ) then
    begin
      UpdateChildren( Node, Node.StateIndex = STATE_CHECKED );
      UpdateParents( Node, Node.StateIndex = STATE_CHECKED );
    end;
  end;
end;


procedure TRzCheckTree.UpdateStateFromChildren( ParentNode, DeletedNode: TTreeNode );
var
  Node: TTreeNode;
  CheckedCount, UnCheckedCount, NewState: Integer;
begin
  if FCascadeChecks then
  begin
    // This method is called when a node in the tree is deleted.  In this case,
    // the check states of the nodes need to be re-evaluated to handle any
    // changes that need to be made up the tree.  Also, the parent node of the
    // node that is getting deleted may have its state change.

    NewState := STATE_UNCHECKED;

    Node := ParentNode;
    if Node.HasChildren then
    begin
      Node := Node.GetFirstChild;
      CheckedCount := 0;
      UnCheckedCount := 0;
      while True do
      begin
        if Node <> DeletedNode then
        begin
          Inc( UnCheckedCount, Ord( Node.StateIndex = STATE_UNCHECKED ) );
          Inc( CheckedCount, Ord( Node.StateIndex = STATE_CHECKED ) );

          if ( Node.StateIndex = STATE_PARTCHECKED ) or
             ( ( CheckedCount > 0 ) and ( UnCheckedCount > 0 ) ) then
          begin
            NewState := STATE_PARTCHECKED;
            Break;
          end;
        end;

        Node := Node.GetNextSibling;
        if Node = nil then
        begin
          if CheckedCount > 0 then
            NewState := STATE_CHECKED
          else
            NewState := STATE_UNCHECKED;
          Break;
        end;
      end;
    end;

    SetNodeCheckState( ParentNode, TRzCheckState( NewState ) );

    if ( ParentNode.StateIndex = STATE_CHECKED ) or
       ( ParentNode.StateIndex = STATE_UNCHECKED ) then
    begin
      UpdateChildren( ParentNode, ParentNode.StateIndex = STATE_CHECKED );
      UpdateParents( ParentNode, ParentNode.StateIndex = STATE_CHECKED );
    end;
  end;
end; {= TRzCheckTree.UpdateStateFromChildren =}


procedure TRzCheckTree.UpdateChildrenCascadingStates( ParentNode: TTreeNode );
var
  Node: TTreeNode;
begin
  if ( ParentNode = nil ) or not FCascadeChecks then
    Exit;

  Node := ParentNode.GetFirstChild;
  if Node = nil then
    UpdateCascadingStates( ParentNode )
  else
  begin
    while Node <> nil do
    begin
      if Node.HasChildren then
        UpdateChildrenCascadingStates( Node )
      else
        UpdateCascadingStates( Node );
      Node := Node.GetNextSibling;
    end;
  end;
end;



// Changes state and cascades throughout tree
// The check state is actually stored in the StateIndex field

procedure TRzCheckTree.ChangeNodeCheckState( Node: TTreeNode; NewState: TRzCheckState );
begin
  FChangingState := False;
  if Node.StateIndex <> Ord( NewState ) then
    SetNodeCheckState( Node, NewState );
  if FCascadeChecks then
  begin
    UpdateChildren( Node, Node.StateIndex = STATE_CHECKED );
    UpdateParents( Node, Node.StateIndex = STATE_CHECKED );
  end;
end;


procedure TRzCheckTree.UpdateParents( Node: TTreeNode; NodeChecked: Boolean );
var
  CheckedCount, UnCheckedCount, NewState: Integer;
begin
  {$IFDEF WIN32}
  {$IFNDEF VCL240_OR_HIGHER}
  // 64-bit compiler and 32-bit compiler (in RX 10.1 or higher) reports 
  // that value assigned to NewState not used. Without it, compiler reports
  // NewState may not be initialized.
  NewState := STATE_UNCHECKED;
  {$ENDIF}
  {$ENDIF}

  while ( Node <> nil ) and ( Node.Parent <> nil ) do
  begin
    Node := Node.Parent.GetFirstChild;
    CheckedCount := 0;
    UnCheckedCount := 0;
    while True do
    begin
      Inc( UnCheckedCount, Ord( Node.StateIndex = STATE_UNCHECKED ) );
      Inc( CheckedCount, Ord( Node.StateIndex = STATE_CHECKED ) );
      if ( Node.StateIndex = STATE_PARTCHECKED ) or
         ( ( CheckedCount > 0 ) and ( UnCheckedCount > 0 ) ) then
      begin
        NewState := STATE_PARTCHECKED;
        Break;
      end;
      if Node.GetNextSibling = nil then
      begin
        if CheckedCount > 0 then
          NewState := STATE_CHECKED
        else
          NewState := STATE_UNCHECKED;
        Break;
      end
      else
        Node := Node.GetNextSibling;
    end;
    Node := Node.Parent;
    if Node <> nil then
      SetNodeCheckState( Node, TRzCheckState( NewState ) );
  end;
end;


procedure TRzCheckTree.RecurseChildren( Node: TTreeNode; NodeChecked: Boolean );
begin
  while Node <> nil do
  begin
    if NodeChecked then
      SetNodeCheckState( Node, csChecked )
    else
      SetNodeCheckState( Node, csUnchecked );
    if Node.GetFirstChild <> nil then
      RecurseChildren( Node.GetFirstChild, NodeChecked );
    Node := Node.GetNextSibling;
  end;
end;


procedure TRzCheckTree.UpdateChildren( Node: TTreeNode; NodeChecked: Boolean );
var
  WasSuspended: Boolean;
begin
  WasSuspended := FSuspendCascades;
  FSuspendCascades := True;
  RecurseChildren( Node.GetFirstChild, NodeChecked );
  FSuspendCascades := WasSuspended;

  if Assigned( FOnUpdateChildren ) then
    FOnUpdateChildren( Self );
end;


procedure TRzCheckTree.Delete( Node: TTreeNode );
begin
  if FCascadeChecks and ( Node.Parent <> nil ) then
    UpdateStateFromChildren( Node.Parent, Node );
  inherited;
end;


procedure TRzCheckTree.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  R: TRect;
  Idx: Integer;
begin
  if Selected <> nil then
  begin
    if Selected.AbsoluteIndex > -1 then
    begin
      Idx := Selected.AbsoluteIndex;
      R := Selected.DisplayRect( True );

      if ( Button = mbLeft ) and ( X <= R.Left - FImageWidth ) and
         ( X > R.Left - FBmpWidth - FImageWidth ) and
         ( Y >= R.Top ) and ( Y <= R.Bottom ) then
      begin
        FChangingState := True;
        FSelectedItem := Idx;
      end;
    end;
  end;
  inherited;
end;


procedure TRzCheckTree.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if ( Button = mbLeft ) and FChangingState and ( Selected.AbsoluteIndex = FSelectedItem ) and
     PtInRect( ClientRect, Point( X, Y ) ) then
  begin
    ToggleCheckState( Selected );
  end;
  inherited;
end;


procedure TRzCheckTree.KeyUp( var Key: Word; Shift: TShiftState );
begin
  if ( Key = vk_Space ) and not IsEditing and ( Selected <> nil ) then
    ToggleCheckState( Selected );
  inherited;
end;


procedure TRzCheckTree.WMChar( var Msg: TWMChar );
begin
  if Msg.CharCode <> vk_Space then
    inherited;
end;


procedure TRzCheckTree.SetAllChildren( Node: TTreeNode; NewState: TRzCheckState );
begin
  while Node <> nil do
  begin
    Node.StateIndex := Ord( NewState );
    if Node.GetFirstChild <> nil then
      SetAllChildren( Node.GetFirstChild, NewState );          // Recursive call
    Node := Node.GetNextSibling;
  end;
end;


procedure TRzCheckTree.SetAllNodes( NewState: TRzCheckState );
begin
  if Items.Count > 0 then
    SetAllChildren( Items[ 0 ], NewState );
end;


function TRzCheckTree.GetImages: TCustomImageList;
begin
  Result := inherited Images;
end;


procedure TRzCheckTree.SetImages( Value: TCustomImageList );
begin
  inherited Images := Value;
  UpdateImageWidth;
end;



procedure TRzCheckTree.LoadTreeFromList( List: TStrings );
var
  Node, NextNode: TTreeNode;
  Level, I, P, NodeState, NodeImage, NodeImageSel: Integer;
  CurrStr: string;

  function GetBufStart( Buffer: PChar; var Level: Integer ): PChar;
  begin
    Level := 0;
    while CharInSet( Buffer^, [ ' ', #9 ] ) do
    begin
      Inc( Buffer );
      Inc( Level );
    end;
    Result := Buffer;
  end;

begin
  Items.BeginUpdate;
  try
    try
      Items.Clear;
      Node := nil;
      for I := 0 to List.Count - 1 do
      begin
        CurrStr := GetBufStart( PChar( List[ I ] ), Level );

        NodeState := -1;
        NodeImage := -1;
        NodeImageSel := -1;
        P := Pos( '|', CurrStr );
        if P > 0 then
        begin
          NodeState := StrToInt( Copy( CurrStr, 1, P - 1 ) );
          System.Delete( CurrStr, 1, P );

          P := Pos( '|', CurrStr );
          if P > 0 then
          begin
            NodeImage := StrToInt( Copy( CurrStr, 1, P - 1 ) );
            System.Delete( CurrStr, 1, P );

            P := Pos( '|', CurrStr );
            if P > 0 then
            begin
              NodeImageSel := StrToInt( Copy( CurrStr, 1, P - 1 ) );
              System.Delete( CurrStr, 1, P );
            end;
          end;
        end;

        if Node = nil then
          Node := Items.AddChild( nil, CurrStr )
        else if Node.Level = Level then
          Node := Items.AddChild( Node.Parent, CurrStr )
        else if Node.Level = ( Level - 1 ) then
          Node := Items.AddChild( Node, CurrStr )
        else if Node.Level > Level then
        begin
          NextNode := Node.Parent;
          while NextNode.Level > Level do
            NextNode := NextNode.Parent;
          Node := Items.AddChild( NextNode.Parent, CurrStr );
        end
        else
          raise ETreeViewError.CreateFmt( sInvalidLevelEx, [ Level, CurrStr ] );

        if Node <> nil then
        begin
          Node.StateIndex := NodeState;
          Node.ImageIndex := NodeImage;
          Node.SelectedIndex := NodeImageSel;
        end;
      end;
    finally
      Items.EndUpdate;
    end;
  except
    Invalidate;  // force repaint on exception
    raise;
  end;
end;


procedure TRzCheckTree.SaveTreeToList( List: TStrings );
const
  TabChar = #9;
var
  I: Integer;
  Node: TTreeNode;
  NodeState, NodeImage, NodeImageSel, NodeStr: string;
begin
  if Items.Count > 0 then
  begin
    Node := Items[ 0 ];
    while Node <> nil do
    begin
      NodeStr := '';
      for I := 0 to Node.Level - 1 do
        NodeStr := NodeStr + TabChar;
      NodeState := IntToStr( Node.StateIndex );
      NodeImage := IntToStr( Node.ImageIndex );
      NodeImageSel := IntToStr( Node.SelectedIndex );
      NodeStr := NodeStr + NodeState + '|' + NodeImage + '|' + NodeImageSel + '|' + Node.Text;
      List.Add( NodeStr );
      Node := Node.GetNext;
    end;
  end;
end;


{&RUIF}
end.
