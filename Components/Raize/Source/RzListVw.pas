{===============================================================================
  RzListVw Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzListView
    Enhanced list view components that supports Custom Framing, custom drawing,
    sorting, etc.


  Modification History
  ------------------------------------------------------------------------------
  6.1.10 (05 Sep 2014)
    * Fixed issue in TRzListView where the context menu for individual items
      would not honor the associated TPopupMenu's AutoPopup property.
  ------------------------------------------------------------------------------
  6.1.7  (07 Mar 2014)
    * Fixed flicker issues in TRzListView and descendants when ViewStyle is
      vsReport.
    * Updated the TRzListView with better support for 64-bit Windows.
  ------------------------------------------------------------------------------
  6.1.6  (15 Feb 2014)
    * Fixed issue in TRzListView and where the captions of columns (vsReport
      ViewStyle) would not be displayed at design-time if the FillLastColumn
      property was set to False.
    * Fixed flicker issue in TRzListView when moving the mouse rapidly over
      the column headers (ViewStyle = vsReport).
  ------------------------------------------------------------------------------
  6.1.5  (02 Oct 2013)
    * Fixed issue in TRzListView where re-ordering the Columns would cause the
      incorrect column to be sorted.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Fixed issue with displaying TRzListView headers when runtime themes are
      not enabled.
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Modified TRzListView and descendants to use HintWindowClass instead of
      THintWindow directly to display item hints.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Redesigned the header display painting methods used by the TRzListView.
    * Redesigned the code used to display the sort indicators in the headers.
    * Made necessary modifications to TRzListView to fully support VCL Styles
      introduced in RAD Studio XE2.
    * Made necessary modifications to TRzListView to support 64-bit development.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed issue where using the mouse wheel in a TRzListView that did not have
      any ImageList assigned to the control would raise and exception.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzListView control.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Fixed issue where editing an item in a TRzListView would result in an
      access violation when running under Vista.
  ------------------------------------------------------------------------------
  4.3.2  (04 Nov 2007)
    * Added OnDrawHeader event. This event is fired when the header needs to be
      drawn and HeaderDefaultDrawing is set to False.
    * The OnContextPopup event now correctly fires when right-clicking on an
      empty portion of the TRzListView.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Added custom code to handle displaying InfoTip hints in the TRzListView.
      This was necessary because the list view common control does not handle
      displaying hints with regions, which can occur if TRzBalloonHints is used
      with ShowBalloon set to True. The OnInfoTip events works the same way as
      before.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Loading a form containing a TRzListView in vsReport style that was placed
      on a TRzSplitter no longer causes the BDS IDE to lock up or experience
      painting issues.
    * Starting with BDS 2006, the base TListView class receives a cm_MouseLeave
      when the user moves the mouse onto the list view's header. If the user
      then moves the mouse upward out of the control, the list view does NOT
      receive notification of this. This change of behavior resulted in
      inconsistent painting in the TRzListView when using vsReport style and
      XP themes. The TRzListView now ensures that the control does receive a
      cm_MouseLeave notification message in this situation.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surfaced OnMouseWheel event in TRzListView and descendants.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where TRzListView header would be displayed in same font
      color as the list view's Font property. This is problematic if the color
      is very light. The control now draws the header in the same font, but uses
      the clBtnText font color.
    * Fixed problem where TRzListView would not get resized properly (e.g. Align
      set to alClient) when placed on a TRzSplitter.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzListView to
      account for changes introduced in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzListView when
      FrameVisible was set to True and changes were made to control's appearance
      within calls to LockWindowUpdate.
    * Added new FrameControllerNotifications property to TRzListView.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where header would not be repainted when
      HeaderDefaultDrawing property was modified.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem where incorrect image (via image index) was being displayed
      for column headers.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where TRzListView did not honor the TPopupMenu.AutoPopup
      property when invoking a context menu.
    * Moved the ResetHeaderHandle method to the public section.  This method is
      normally called in the Loaded method to take care of situations where the
      header handle has been changed. (Typically, when the list view is on a
      frame.)  However, if the frame is created dynamically and not loaded from
      a DFM file, the ResetHeaderHandle method will not be called and thus must
      be called manually by the user.  This is only necessary if custom framing
      is also being used by the list view.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where headers of list view would not display hot tracking
      when running under Windows XP Themes *and* the control was reparented.
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
    * Fixed problem where context menu associated with the PopupMenu property
      would not be invoked when user right-clicked on empty area of list view.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem where context menu would get redisplayed after selecting an
      nested menu item under certain circumstances (i.e. the list view structure
      was modified in some way within the event handler).
    * Fixed problem where placing an item into edit mode caused a stack
      overflow.
    * Fixed problem where display of header captions appear bolded when moving
      the mouse over the list view and ClearType was enabled on the system.
    * Reworked several aspects of the custom drawing of headers and the handling
      of sort indicators.
    * Fixed problem introduced in 3.0.8 where popup menus on items were not
      consistently being displayed when the user right-clicked on a list view
      item.
    * Fixed problem where last column was not getting sized correctly when a
      form containing the list view was loaded and FillLastCol was set to True.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where background of items in list view were not being
      displayed correctly when the component was disabled. That is, the
      background was not being drawn using DisabledColor.
    * Fixed problem where header captions would not get updated correctly after
      a drag operation.
    * Fixed problem where header would not display images assigned to the
      Columns[i] items.
    * Fixed problem where header was not drawn correctly under certain
      conditions when using Windows XP Themes.
    * When running under Windows XP, header no longer use HDF_SORTUP and
      HDF_SORTDOWN styles for displaying the sort indicator. Instead, the
      indicator is drawn using the implemented owner-draw code.  The reason for
      this change is that the standard list view control does not support
      displaying a sort indicator *and* an image.
    * Fixed problem where context menu was being displayed in the wrong
      position.
    * Modified the SortByColumn method such that it specifies a default
      parameter to control the direction of the sorting operation.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed problem where resizing columns would cause items covered by scroll
      bar not to be updated when the scroll bar is removed.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * When FillLastColumn = True, the last column is only resized when
      ViewStyle = vsReport.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
    * Add FocusColor and DisabledColor properties.
    * Added EditSelectedCaption and EditItemCaption methods.  These two methods
      allow a developer to programmatically place a list item into edit mode
      when the EditOnRowClick property is set to False.
    * SortColumn method was renamed to SortByColumn.
===============================================================================}

{$I RzComps.inc}

unit RzListVw;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Windows,
  Messages,
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ComCtrls,
  CommCtrl,
  Graphics,
  Menus,
  SysUtils,
  RzCommon;


type
  {=========================================}
  {== TRzCustomListView Class Declaration ==}
  {=========================================}

  TRzLVCheckStateChanging = procedure ( Sender: TObject; Item: TListItem; ToBeChecked: Boolean;
                                        var AllowChange: Boolean ) of object;

  TRzLVOnItemContextMenuEvent = procedure( Sender: TObject; Item: TListItem; var Pos: TPoint;
                                           var Menu: TPopupMenu ) of object;

  TRzLVDrawHeaderEvent = procedure( Sender: TObject; Canvas: TCanvas; Index: Integer;
                                    Rect: TRect ) of object;

  TRzLVSortDirection = ( sdAscending, sdDescending );
  TRzLVHeaderSortDisplayMode = ( hsdmNone, hsdmLeftAlign, hsdmRightOfText, hsdmRightAlign );

  TRzCustomListView = class( TCustomListView )
  private
    FAlphaSortAll: Boolean;
    FLastIndex: Integer;
    FFillLastColumn: Boolean;
    FEditOnRowClick: Boolean;
    FEditingCaption: Boolean;
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

    FOnCheckStateChanging: TRzLVCheckStateChanging;
    FOnCheckStateChange: TLVNotifyEvent;

    FOnItemContextMenu: TRzLVOnItemContextMenuEvent;
    FMenuAlreadyHandled: Boolean;
    FDragStarted: Boolean;

    FHeaderCanvas: TCanvas;
    FHeaderSortColumn: Integer;
    FHeaderSortDirection: TRzLVSortDirection;
    FHeaderSortDisplayMode: TRzLVHeaderSortDisplayMode;
    FHeaderDefaultDrawing: Boolean;
    FOnDrawHeader: TRzLVDrawHeaderEvent;

    FSetHeaderODStyleSem: Integer;

    FInternalHeaderHandle: HWND;
    FHeaderInstance: Pointer;
    FDefHeaderProc: Pointer;
    FRightClicked: Boolean;
    FMouseInNonClient: Boolean;

    FHintWnd: THintWindow;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CNNotify( var Msg: TWMNotify ); message cn_Notify;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure WMNotify( var Msg: TWMNotify ); message wm_Notify;
    procedure WMMouseMove( var Msg: TWMMouseMove ); message wm_MouseMove;

    procedure HeaderWndProc( var Msg: TMessage );

    function GetHeaderHandle: HWnd;

    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMSysColorChange( var Msg: TMessage ); message cm_SysColorChange;

    procedure AMInitStage2( var Msg: TMessage ); message wm_User + 100;

    procedure WMContextMenu( var Msg: TMessage ); message wm_ContextMenu;
    procedure WMParentNotify( var Msg: TWMParentNotify ); message wm_ParentNotify;
    procedure WMRButtonUp( var Msg: TWMRButtonUp ); message wm_RButtonUp;
  protected
    FAboutInfo: TRzAboutInfo;
    FLoading: Boolean;
    FCanvas: TControlCanvas;
    FOverControl: Boolean;
    FOverHeaderSection: Integer;
    FHeaderSectionDown: Boolean;
    FSettingFrameController: Boolean;

    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function IsCustomDrawn( Target: TCustomDrawTarget; Stage: TCustomDrawStage ): Boolean; override;
    function CustomDrawItem( Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage ): Boolean; override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    procedure ResizeLastColumn; virtual;

    procedure CreateWnd; override;

    procedure DrawHeader( Index: Integer; const Rect: TRect );
    procedure DrawHeaderSortGlyphs( Index: Integer; const Rect: TRect );
    procedure DefaultDrawHeader( Index: Integer; const Rect: TRect );
    procedure SetHeaderODStyle;

    function GetMappedColumnIndex( ColIndex: Integer ): Integer;

    procedure SetParent( Value: TWinControl ); override;

    function CalcHintRect( MaxWidth: Integer; const HintStr: string; HintWnd: THintWindow ): TRect;
    procedure DoHint( X, Y: Integer );
    procedure ReleaseHintWindow;

    { Event Dispatch Methods }
    procedure DoDrawHeader( Index: Integer; Rect: TRect ); dynamic;

    procedure ColClick( Column: TListColumn ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;

    procedure CheckStateChange( Item: TListItem ); dynamic;
    function CanCheckStateChange( Item: TListItem; ToBeChecked: Boolean ): Boolean; dynamic;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    procedure DoPreItemContextMenu( pt: TPoint ); dynamic;
    procedure DoItemContextMenu( p: TPoint ); dynamic;
    procedure ItemContextMenu( Item: TListItem; var Pos: TPoint; var Menu: TPopupMenu ); dynamic;
    procedure KeyDown( var Key: Word; ShiftState: TShiftState ); override;

    { Property Access Methods }
    procedure SetFillLastColumn( Value: Boolean ); virtual;
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

    procedure SetViewStyle( AValue: TViewStyle ); override;
    procedure SetHeaderSortColumn( Value: Integer );
    procedure SetHeaderSortDirection( Value: TRzLVSortDirection );
    procedure SetHeaderSortDisplayMode( Value: TRzLVHeaderSortDisplayMode );
    procedure SetHeaderDefaultDrawing( Value: Boolean );

    { Property Declarations }
    property AlphaSortAll: Boolean
      read FAlphaSortAll
      write FAlphaSortAll
      default False;

    property Color
      stored StoreColor
      default clWindow;

    property Enabled: Boolean
      read GetEnabled
      write SetEnabled
      default True;

    property EditOnRowClick: Boolean
      read FEditOnRowClick
      write FEditOnRowClick
      default False;

    property FillLastColumn: Boolean
      read FFillLastColumn
      write SetFillLastColumn
      default True;

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

    property HeaderCanvas: TCanvas
      read FHeaderCanvas;

    property HeaderHandle: HWnd
      read GetHeaderHandle;

    property HeaderDefaultDrawing: Boolean
      read FHeaderDefaultDrawing
      write SetHeaderDefaultDrawing
      default True;

    property HeaderSortDisplayMode: TRzLVHeaderSortDisplayMode
      read FHeaderSortDisplayMode
      write SetHeaderSortDisplayMode
      default hsdmRightOfText;

    property HeaderSortColumn: Integer
      read FHeaderSortColumn
      write SetHeaderSortColumn
      default -1;

    property HeaderSortDirection: TRzLVSortDirection
      read FHeaderSortDirection
      write SetHeaderSortDirection
      default sdAscending;

    property OnCheckStateChanging: TRzLVCheckStateChanging
      read FOnCheckStateChanging
      write FOnCheckStateChanging;

    property OnCheckStateChange: TLVNotifyEvent
      read FOnCheckStateChange
      write FOnCheckStateChange;

    property OnItemContextMenu: TRzLVOnItemContextMenuEvent
      read FOnItemContextMenu
      write FOnItemContextMenu;

    property OnDrawHeader: TRzLVDrawHeaderEvent
      read FOnDrawHeader
      write FOnDrawHeader;


    { Inherited Properties & Events }
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ResetHeaderHandle;

    function UseThemes: Boolean; virtual;

    function EditSelectedCaption: Boolean;
    function EditItemCaption( Item: TListItem ): Boolean;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure SortByColumn( Index: Integer; Direction: TRzLVSortDirection = sdAscending );
    procedure SetTopIndex( Index: Integer );
  end;


  TRzListView = class( TRzCustomListView )
  public
    property HeaderCanvas;
    property HeaderHandle;
    property HeaderSortColumn;
    property HeaderSortDirection;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Action;
    property Align;
    property AllocBy;
    property AlphaSortAll;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property Checkboxes;
    property Color;
    property Columns;
    property ColumnClick;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditOnRowClick;
    property Enabled;
    property FlatScrollBars;
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
    property FullDrag;
    property GridLines;
    property Groups;
    property GroupHeaderImages;
    property GroupView;
    property HeaderDefaultDrawing;
    property HeaderSortDisplayMode;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property IconOptions;
    property Items;
    property LargeImages;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property RowSelect;
    property ShowHint;
    property ShowColumnHeaders;
    property ShowWorkAreas;
    property SmallImages;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property ViewStyle;
    property FillLastColumn;
    property Visible;

    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnCheckStateChanging;
    property OnCheckStateChange;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnContextPopup;
    property OnCreateItemClass;
    property OnItemChecked;
    property OnGetSubItemImage;
    property OnInfoTip;
    property OnCompare;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawHeader;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetImageIndex;
    property OnInsert;
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
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnItemContextMenu;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnSelectItem;
  end;


function RzCustomSortDescendingProc( Item1, Item2, ColIndex: TRzNativeInt ): Integer; stdcall;
function RzCustomSortProc( Item1, Item2, ColIndex: TRzNativeInt ): Integer; stdcall;

procedure DrawArrow( Canvas: TCanvas; const Rect: TRect; ArrowColor: TColor; UpArrow: Boolean );


implementation

uses
  {&RAS}
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Themes,
  TypInfo,
  RzShellUtils;


const
  HDF_SORTUP = $0400;
  HDF_SORTDOWN = $0200;


{=====================}
{== Support Methods ==}
{=====================}

procedure DrawArrow( Canvas: TCanvas; const Rect: TRect; ArrowColor: TColor; UpArrow: Boolean );
var
  X, Y: Integer;
  OldBrushColor: TColor;
begin
  OldBrushColor := Canvas.Brush.Color;

  Canvas.Brush.Color := ArrowColor;
  Canvas.Pen.Style := psClear;

  X := Rect.Left + ( Rect.Right - Rect.Left ) div 2;
  Y := Rect.Top + ( Rect.Bottom - Rect.Top ) div 2;

  if UpArrow then
    Canvas.Polygon( [ Point( X, Y - 4 ), Point( X + 4, Y + 1 ), Point( X - 4, Y + 1 ) ] )
  else
    Canvas.Polygon( [ Point( X, Y ), Point( X - 3, Y - 4 ), Point( X + 4, Y - 4 ) ] );


  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Color := OldBrushColor;
end;



{&RT}
{===============================}
{== TRzCustomListView Methods ==}
{===============================}

constructor TRzCustomListView.Create( AOwner: TComponent );
begin
  inherited;

  FCanvas := TControlCanvas.Create;
  FCanvas.Control := Self;

  FAlphaSortAll := False;
  FEditOnRowClick := False;
  FFillLastColumn := True;

  FLastIndex := -1;
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

  FHeaderCanvas := TCanvas.Create;
  FHeaderSortDisplayMode := hsdmRightOfText;
  FHeaderDefaultDrawing := True;
  FHeaderSortColumn := -1;
  FHeaderSortDirection := sdAscending;
  FOverHeaderSection := -1;

  if not ( csDesigning in ComponentState ) then
    FHeaderSortColumn := -1;

  FHeaderInstance := MakeObjectInstance( HeaderWndProc );

  {&RCI}
end;


procedure TRzCustomListView.CreateWnd;
begin
  inherited;

  Perform( LVM_SETTEXTBKCOLOR, 0, ColorToRGB( Color ) );
  Perform( LVM_SETBKCOLOR, 0, ColorToRGB( Color ) );
  SetHeaderODStyle;
  PostMessage( Handle, WM_USER + 100, 0, 0 );
end;


destructor TRzCustomListView.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  FHeaderCanvas.Free;

  if HeaderHandle <> 0 then
    SetWindowLongPtr( HeaderHandle, GWL_WNDPROC, LONG_PTR( FDefHeaderProc ) );
  FreeObjectInstance( FHeaderInstance );

  inherited;
end;


procedure TRzCustomListView.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzCustomListView.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzCustomListView.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzCustomListView.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );

  if FFillLastColumn then
  begin
    FLoading := True;
    ResizeLastColumn;
    FLoading := False;
  end
  else if ( csDesigning in ComponentState ) and ( Columns.Count > 0 ) and ( ViewStyle = vsReport ) then
  begin
    // The following statements fix the issue where setting FillLastColumn to False would result
    // in the column captions to not be visible at design-time.
    SendMessage( Handle, wm_SetRedraw, 1, 0 );
    if HeaderHandle <> 0 then
      SendMessage( HeaderHandle, wm_SetRedraw, 1, 0 );
  end;

  ResetHeaderHandle;
end;


procedure TRzCustomListView.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzCustomListView.CNNotify( var Msg: TWMNotify );
var
  Item: TListItem;
  R: TRect;
  P: TPoint;
begin
  with Msg.NMHdr^ do
  begin
    Msg.Result := 0;

    case code of
      lvn_ItemChanging:
      begin
        inherited;  // Allow default OnChanging event to fire

        with PNMListView( Msg.NMHdr )^ do
        begin
          if CheckBoxes and
             ( ( uNewState = $2000 ) or ( uNewState = $1000 ) ) then
          begin
            if not CanCheckStateChange( Items[ iItem ], uNewState = $2000 ) then
              Msg.Result := 1;
          end;
        end;
      end;

      lvn_ItemChanged:
      begin
        inherited;  // Allow default OnChange event to fire

        with PNMListView( Msg.NMHdr )^ do
        begin
          if CheckBoxes and
             ( ( uNewState = $2000 ) or ( uNewState = $1000 ) ) then
          begin
            CheckStateChange( Items[ iItem ] );
          end;
        end;
      end;

      lvn_BeginLabelEdit:
      begin
        if not FEditingCaption and RowSelect and ( ViewStyle = vsReport ) and not FEditOnRowClick then
        begin
          // This notification message is received when the list view is about
          // to put the main item into edit mode.  Only go into edit mode if
          // the mouse was clicked on the main item.
          Item := Items[ ( PLVDispInfo( Pointer( Msg.NMHdr ) )^.item.iItem ) ];

          R := Item.DisplayRect( drLabel );

          GetCursorPos( P );
          P := ScreenToClient( P );

          if not PtInRect( R, P ) or not CanEdit( Item ) then
            Msg.Result := 1;
        end
        else
          inherited;
      end;

      lvn_BeginDrag, lvn_BeginRDrag:
      begin
        FDragStarted := True;
        inherited;
      end;

      nm_RClick:
      begin
        Windows.GetCursorPos( P );
        P := ScreenToClient( P );
        if htOnItem in GetHitTestInfoAt( P.X, P.Y ) then
          FRightClicked := True;
        try
          DoPreItemContextMenu( P );
        finally
          FRightClicked := False;
        end;
        inherited;
      end;

      lvn_GetInfoTip:
      begin
        // Do not use normal InfoTip hinting because if TRzBalloonHints are used
        // with the ShowBalloon property set to True, the region used causes the
        // list view to display empty hints.  The TRzListView handles displaying
        // the InfoTips itself using an approach similar to that of TRzListBox.
      end;

      else
        inherited;
    end; { case }
  end;
end; {= TRzCustomListView.CNNotify =}


function TRzCustomListView.CalcHintRect( MaxWidth: Integer; const HintStr: string; HintWnd: THintWindow ): TRect;
begin
  Result := HintWnd.CalcHintRect( Screen.Width, HintStr, nil );
end;


procedure TRzCustomListView.DoHint( X, Y: Integer );
var
  Offset: Integer;
  R, IR, WinRect: TRect;
  P: TPoint;
  Item: TListItem;
  InfoTip: string;
begin
  if not Assigned( OnInfoTip ) then
    Exit;

  Item := GetItemAt( X, Y );

  if Item <> nil then
  begin
    InfoTip := Item.Caption;
    DoInfoTip( Item, InfoTip );

    Canvas.Font := Font;

    if not ( csDesigning in ComponentState ) and ForegroundTask then
    begin
      if not Assigned( FHintWnd ) then
      begin
        FHintWnd := HintWindowClass.Create( Self );
        FHintWnd.Color := Application.HintColor;
      end;

      FHintWnd.Canvas.Font := Self.Font;

      R := CalcHintRect( Width, InfoTip, FHintWnd );
      IR := Item.DisplayRect( drBounds );

      Offset := IR.Bottom - IR.Top;
      P := ClientToScreen( Item.DisplayRect( drBounds ).TopLeft );
      OffsetRect( R, P.X + 5, P.Y + Offset - 3 );

      GetWindowRect( FHintWnd.Handle, WinRect );

      if not IsWindowVisible( FHintWnd.Handle ) or not ( ( R.Left = WinRect.Left ) and ( R.Top = WinRect.Top ) ) then
        FHintWnd.ActivateHint( R, InfoTip )
    end
    else
      ReleaseHintWindow;
  end
  else
    ReleaseHintWindow;
end;


procedure TRzCustomListView.WMMouseMove( var Msg: TWMMouseMove );
begin
  inherited;
  DoHint( Msg.XPos, Msg.YPos );
end;


procedure TRzCustomListView.ReleaseHintWindow;
begin
  if Assigned( FHintWnd ) then
    FHintWnd.ReleaseHandle;
end;


procedure TRzCustomListView.CheckStateChange( Item: TListItem );
begin
  if Assigned( FOnCheckStateChange ) then
    FOnCheckStateChange( Self, Item );
end;


function TRzCustomListView.CanCheckStateChange( Item: TListItem; ToBeChecked: Boolean ): Boolean;
begin
  Result := True;
  if Assigned( FOnCheckStateChanging ) then
    FOnCheckStateChanging( Self, Item, ToBeChecked, Result );
end;


procedure TRzCustomListView.ResizeLastColumn;
var
  ColWidths, W, BaseWidth, I: Integer;
begin
  if not FFillLastColumn or ( ViewStyle <> vsReport ) or
     ( csDestroying in ComponentState ) or ( csLoading in ComponentState ) then
  begin
    Exit;
  end;

  if Columns.Count >= 1 then
  begin
    ColWidths := 0;

    Items.BeginUpdate;

    for I := 0 to Columns.Count - 2 do
      ColWidths := ColWidths + Columns[ I ].Width;

    if ( Items.Count = 0 ) or FLoading then
      BaseWidth := ClientWidth - GetSystemMetrics( sm_CxVScroll )
    else
      BaseWidth := ClientWidth;

    if ColWidths < BaseWidth then
    begin
      W := BaseWidth - ColWidths;
      if W < 5 then
        W := 5;
      Columns[ Columns.Count - 1 ].Width := W;
    end;

    Items.EndUpdate;
  end;
end;


procedure TRzCustomListView.SetBounds( ALeft, ATop, AWidth, AHeight: Integer);
var
  NeedToResize: Boolean;
begin
  // Need to check to see if we need to call ResizeLastColumn.
  // But check must be made before calling inherited *and* the
  // call to ResizeLastColumn must be made after the inherited call
  // so that ResizeLastColumn can use the updated size of the control.

  NeedToResize := ( ALeft <> Left ) or ( ATop <> Top ) or
                  ( AWidth <> Width ) or ( AHeight <> Height );

  inherited;

  // Added check of FInternalHeaderHandle because resizing last column before
  // the control is properly reparented (e.g. when placed on a splitter)
  // prevents the list view from resizing correctly at start up.
  if NeedToResize and ( FInternalHeaderHandle <> 0 ) then
    ResizeLastColumn;
end;


procedure TRzCustomListView.WMSize( var Msg: TWMSize );
begin
  inherited;

  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzCustomListView.WMNotify( var Msg: TWMNotify );
begin
  if Msg.NMHdr.hwndFrom = HeaderHandle then
  begin
    if Msg.NMHdr.code = HDN_ITEMCLICKW then
      InvalidateRect( HeaderHandle, nil, True );

    inherited;

    case Msg.NMHdr.code of
      hdn_EndTrack, hdn_BeginTrack, hdn_ItemChanged:
      begin
        // After dragging a column, the ownerdraw state seems to be reset, so set it back.
        SetHeaderODStyle;
        if FFrameVisible then
          Invalidate;
      end;
    end;

    case Msg.NMHdr.code of
      hdn_EndTrack, hdn_DividerDblClick:
        ResizeLastColumn;
    end;
  end
  else
    inherited;
end; {= TRzCustomListView.WMNotify =}



procedure TRzCustomListView.SetFillLastColumn( Value: Boolean );
begin
  if FFillLastColumn <> Value then
  begin
    FFillLastColumn := Value;
    ResizeLastColumn;
  end;
end;


procedure TRzCustomListView.SetTopIndex( Index: Integer );
var
  L, T, H: Integer;
  R: TRect;
begin
  T := TopItem.Index;
  if T = Index then
    Exit;  // Nothing to do

  L := Index - T;
  R := Items[ T ].DisplayRect( drBounds );
  H := R.Bottom - R.Top;
  Scroll( 0, L * H );
end;


procedure TRzCustomListView.SortByColumn( Index: Integer; Direction: TRzLVSortDirection = sdAscending );
begin
  if Direction = sdAscending then
    FLastIndex := -1
  else
    FLastIndex := Index;
  ColClick( Columns[ Index ] );
end;


procedure TRzCustomListView.ColClick( Column: TListColumn );
var
  MappedColIndex: Integer;
begin
  MappedColIndex := GetMappedColumnIndex( Column.Index );
  if FAlphaSortAll then
  begin
    if FLastIndex = MappedColIndex then
    begin
      HeaderSortColumn := MappedColIndex;
      HeaderSortDirection := sdDescending;
      CustomSort( RzCustomSortDescendingProc, MappedColIndex );
      FLastIndex := -1;
    end
    else
    begin
      HeaderSortColumn := MappedColIndex;
      HeaderSortDirection := sdAscending;
      CustomSort( RzCustomSortProc, MappedColIndex );
      FLastIndex := MappedColIndex;
    end;
  end;
  inherited;
end;


function TRzCustomListView.GetMappedColumnIndex( ColIndex: Integer ): Integer;
var
  ColumnOrder: TIntegerDynArray;
begin
  SetLength( ColumnOrder, Columns.Count );
  ListView_GetColumnOrderArray( Handle, Columns.Count, PInteger( ColumnOrder ) );

  Result := ColumnOrder[ ColIndex ];
end;



function RzCustomSortDescendingProc( Item1, Item2, ColIndex: TRzNativeInt ): Integer; stdcall;
begin
  Result := RzCustomSortProc( Item1, Item2, ColIndex );
  Result := Result * -1;
end;



function RzCustomSortProc( Item1, Item2, ColIndex: TRzNativeInt ): Integer; stdcall;
var
  Item1Str: string;
  Item2Str: string;
  ListItem1, ListItem2: TListItem;
begin
  Result := 0;

  ListItem1 := TListItem( Item1 );
  ListItem2 := TListItem( Item2 );
  if ( ListItem1 = nil ) or ( ListItem2 = nil ) then
    Exit;

  Item1Str := '';
  Item2Str := '';

  if ColIndex < 0 then
    Exit;

  if ColIndex = 0 then
  begin
    Item1Str := ListItem1.Caption;
    Item2Str := ListItem2.Caption;
  end
  else
  begin
    if ColIndex <= ListItem1.SubItems.Count then
      Item1Str := ListItem1.SubItems[ ColIndex - 1 ];

    if ColIndex <= ListItem2.SubItems.Count then
      Item2Str := ListItem2.SubItems[ ColIndex - 1 ];
  end;

  Result := CompareText( Item1Str, Item2Str );

  // If one of the strings is empty, make the other string sort before it

  if ( Result > 0 ) and ( Item2Str = '' ) then
    Result := -1
  else if ( Result < 0 ) and ( Item1Str = '' ) then
    Result := 1;
end; {= RzCustomSortProc =}


(*
function RzCustomNumericSortProc( Item1, Item2, ColIndex: Integer ): Integer; stdcall;
var
  Item1Value: Integer;
  Item2Value: Integer;
  ListItem1, ListItem2: TListItem;
begin
  Result := 0;

  ListItem1 := TListItem( Item1 );
  ListItem2 := TListItem( Item2 );
  if ( ListItem1 = nil ) or ( ListItem2 = nil ) then
    Exit;

  Item1Value := 0;
  Item2Value := 0;

  if ColIndex < 0 then
    Exit;

  if ColIndex = 0 then
  begin
    Item1Value := StrToIntDef( ListItem1.Caption, 0 );
    Item2Value := StrToIntDef( ListItem2.Caption, 0 );
  end
  else
  begin
    if ColIndex <= ListItem1.SubItems.Count then
      Item1Value := StrToIntDef( ListItem1.SubItems[ ColIndex - 1 ], 0 );

    if ColIndex <= ListItem2.SubItems.Count then
      Item2Value := StrToIntDef( ListItem2.SubItems[ ColIndex - 1 ], 0 );
  end;

  if Item1Value < Item2Value then
    Result := -1
  else if Item1Value > Item2Value then
    Result := 1
  else
    Result := 0;
end;


function RzCustomDateTimeSortProc( Item1, Item2, ColIndex: Integer ): Integer; stdcall;
var
  Item1DateTime: TDateTime;
  Item2DateTime: TDateTime;
  ListItem1, ListItem2: TListItem;
begin
  Result := 0;

  ListItem1 := TListItem( Item1 );
  ListItem2 := TListItem( Item2 );
  if ( ListItem1 = nil ) or ( ListItem2 = nil ) then
    Exit;

  Item1DateTime := 0;
  Item2DateTime := 0;

  if ColIndex < 0 then
    Exit;

  if ColIndex = 0 then
  begin
    Item1DateTime := StrToDateDef( ListItem1.Caption, 0 );
    Item2DateTime := StrToDateDef( ListItem2.Caption, 0 );
  end
  else
  begin
    if ColIndex <= ListItem1.SubItems.Count then
      Item1DateTime := StrToDateDef( ListItem1.SubItems[ ColIndex - 1 ], 0 );

    if ColIndex <= ListItem2.SubItems.Count then
      Item2DateTime := StrToDateDef( ListItem2.SubItems[ ColIndex - 1 ], 0 );
  end;

  if Item1DateTime < Item2DateTime then
    Result := -1
  else if Item1DateTime > Item2DateTime then
    Result := 1
  else
    Result := 0;
end;

*)


function TRzCustomListView.EditSelectedCaption: Boolean;
begin
  Result := EditItemCaption( Selected );
end;


function TRzCustomListView.EditItemCaption( Item: TListItem ): Boolean;
begin
  Result := False;
  if Item <> nil then
  begin
    FEditingCaption := True;
    try
      Result := Item.EditCaption;
    finally
      FEditingCaption := True;
    end;
  end;
end;
                          

function TRzCustomListView.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  case ViewStyle of
    vsIcon:
    begin
      if ( LargeImages <> nil ) and ( LargeImages.Height > 0 ) then
        Scroll( 0, Mouse.WheelScrollLines * LargeImages.Height )
      else
        Scroll( 0, Mouse.WheelScrollLines * 32 );
    end;

    vsSmallIcon, vsList, vsReport:
    begin
      if ( SmallImages <> nil ) and ( SmallImages.Height > 0 ) then
        Scroll( 0, Mouse.WheelScrollLines * SmallImages.Height )
      else
        Scroll( 0, Mouse.WheelScrollLines * 16 );
    end;
  end;
  Result := True;
end;


function TRzCustomListView.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  case ViewStyle of
    vsIcon:
    begin
      if ( LargeImages <> nil ) and ( LargeImages.Height > 0 ) then
        Scroll( 0, -Mouse.WheelScrollLines * LargeImages.Height )
      else
        Scroll( 0, -Mouse.WheelScrollLines * 32 );
    end;

    vsSmallIcon, vsList, vsReport:
    begin
      if ( SmallImages <> nil ) and ( SmallImages.Height > 0 ) then
        Scroll( 0, -Mouse.WheelScrollLines * SmallImages.Height )
      else
        Scroll( 0, -Mouse.WheelScrollLines * 16 );
    end;
  end;
  Result := True;
end;


function TRzCustomListView.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzCustomListView.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzCustomListView.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzCustomListView.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzCustomListView.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzCustomListView.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzCustomListView.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomListView.SetFrameController( Value: TRzFrameController );
begin
  FSettingFrameController := True;
  try
    if FFrameController <> nil then
      FFrameController.RemoveControl( Self );
    FFrameController := Value;
    if Value <> nil then
    begin
      Value.AddControl( Self );
      Value.FreeNotification( Self );
    end;
  finally
    FSettingFrameController := False;
  end;
end;


procedure TRzCustomListView.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomListView.SetFrameHotTrack( Value: Boolean );
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


procedure TRzCustomListView.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomListView.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomListView.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzCustomListView.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    // Do not call RecreateWnd if setting the FrameController property--
    // if FrameController is on another form, calling
    // RecreateWnd causes and list index exception somewhere in the VCL
    if not FSettingFrameController then
      RecreateWnd;
  end;
end;


procedure TRzCustomListView.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzCustomListView.RepaintFrame;
begin
  if not ( csLoading in ComponentState ) then
  begin
    InvalidateWindowFrame( Handle, Rect( 0, 0, Width, Height ) );
  end;
end;


function TRzCustomListView.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzCustomListView.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzCustomListView.WMNCPaint( var Msg: TWMNCPaint );
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
end; {= TRzCustomListView.WMNCPaint =}


procedure TRzCustomListView.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FrameVisible then
    RepaintFrame;
end;


function TRzCustomListView.IsCustomDrawn( Target: TCustomDrawTarget; Stage: TCustomDrawStage ): Boolean;
begin
  if not ( csDesigning in ComponentState ) and not Enabled then
    Result := True
  else
    Result := inherited IsCustomDrawn( Target, Stage );
end;


function TRzCustomListView.CustomDrawItem( Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage ): Boolean;
begin
  Result := inherited CustomDrawItem( Item, State, Stage );

  // The following fixes the problem where the background of items were not
  // drawn using the DisabledColor value.
  if not ( csDesigning in ComponentState) and not Enabled then
  begin
    Canvas.Brush.Color := clNone;
    Canvas.Brush.Color := FDisabledColor;
  end;
end;


procedure TRzCustomListView.UpdateColors;
begin
  if csLoading in ComponentState then
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


procedure TRzCustomListView.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzCustomListView.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzCustomListView.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzCustomListView.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
  inherited;
  if ( ViewStyle = vsReport ) and (FOverHeaderSection <> -1 ) then
  begin
    FOverHeaderSection := -1;
    InvalidateRect( HeaderHandle, nil, False );
  end;
end;


procedure TRzCustomListView.CMMouseEnter( var Msg: TMessage );
var
  P: TPoint;
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
  {&RV}

  GetCursorPos( P );
  P := ScreenToClient( P );
  DoHint( P.X, P.Y );
end;


procedure TRzCustomListView.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  if ViewStyle = vsReport then
  begin
    FOverHeaderSection := -1;
    InvalidateRect( HeaderHandle, nil, False );
  end;

  UpdateFrame( True, False );
  ReleaseHintWindow;
end;


function TRzCustomListView.GetHeaderHandle: HWnd;
begin
  if HandleAllocated then
    Result := GetDlgItem( Handle, 0 )
  else
    Result := 0;
end;


procedure TRzCustomListView.SetViewStyle( AValue: TViewStyle );
begin
  if ( AValue <> ViewStyle ) then
  begin
    inherited;
    if AValue = vsReport then
      SetHeaderODStyle;
  end;
end;


procedure TRzCustomListView.SetHeaderSortColumn( Value: Integer );
var
  Header: HWND;
  TmpRect: TRect;
begin
  if FHeaderSortColumn <> Value then
  begin
    Header := HeaderHandle;
    if Header <> 0 then
    begin
      if FHeaderSortColumn >= 0 then
      begin
        if Bool( Header_GetItemRect( Header, FHeaderSortColumn, @TmpRect ) ) then
          InvalidateRect( Header, @TmpRect, True );
      end;

      if Value >= 0 then
      begin
        if Bool( Header_GetItemRect( Header, Value, @TmpRect ) ) then
          InvalidateRect( Header, @TmpRect, True );
      end;
    end;
    FHeaderSortColumn := Value;

    if ( RzShellUtils.COMCTL32_VER.version >= COMCTL32_VER600 ) and ( HeaderHandle <> 0 ) then
      SetHeaderODStyle;
  end;
end;


procedure TRzCustomListView.SetHeaderSortDirection( Value: TRzLVSortDirection );
var
  TmpRect: TRect;
begin
  if FHeaderSortDirection <> Value then
  begin
    if ( HeaderHandle <> 0 ) and ( FHeaderSortColumn >= 0 ) then
    begin
      if Bool( Header_GetItemRect( HeaderHandle, FHeaderSortColumn, @TmpRect ) ) then
        InvalidateRect( HeaderHandle, @TmpRect, True );
    end;
    FHeaderSortDirection := Value;

    if ( RzShellUtils.COMCTL32_VER.version >= COMCTL32_VER600 ) and
       ( HeaderHandle <> 0 ) and
       ( FHeaderSortColumn >= 0 ) then
    begin
      SetHeaderODStyle;
    end;
    Update;
  end;
end;


procedure TRzCustomListView.SetHeaderSortDisplayMode( Value: TRzLVHeaderSortDisplayMode );
begin
  if ( FHeaderSortDisplayMode <> Value ) then
  begin
    if ( COMCTL32_VER.version < COMCTL32_VER470 ) then
      FHeaderSortDisplayMode := hsdmNone
    else
      FHeaderSortDisplayMode := Value;
    SetHeaderODStyle;
    if HandleAllocated then
      InvalidateRect( HeaderHandle, nil, True );
  end;
end;


procedure TRzCustomListView.SetHeaderDefaultDrawing( Value: Boolean );
begin
  if FHeaderDefaultDrawing <> Value then
  begin
    FHeaderDefaultDrawing := Value;
    Invalidate;
  end;
end;


procedure TRzCustomListView.CMColorChanged( var Msg: TMessage );
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

  Perform( LVM_SETBKCOLOR, 0, ColorToRGB( Color ) );
  Perform( LVM_SETTEXTBKCOLOR, 0, ColorToRGB( Color ) );
end;


procedure TRzCustomListView.CMSysColorChange( var Msg: TMessage );
begin
  inherited;
  if Color < 0 then   // If color<0 then using sys colors, so need to reflect change to sys colors
    Perform( CM_COLORCHANGED, Msg.wParam, Msg.lParam );
end;


procedure TRzCustomListView.AMInitStage2( var Msg: TMessage );
begin
  SetHeaderODStyle;
end;


procedure TRzCustomListView.WMContextMenu( var Msg: TMessage );
begin
  if not ( csDesigning in ComponentState ) and not FMenuAlreadyHandled then
  begin
    if ( SelCount = 0 ) or not Assigned( ItemFocused ) then
      Exit;

    if ( Msg.lParam = -1 ) then
      DoPreItemContextMenu( Point( 0, 0 ) )
    else
      DoItemContextMenu( ScreenToClient( Point( Msg.lParamLo, Msg.lParamHi ) ) );
  end;
end;


procedure TRzCustomListView.SetParent( Value: TWinControl );
begin
  if Parent <> Value then
  begin
    ResetHeaderHandle;
  end;

  inherited;
end;


procedure TRzCustomListView.WMParentNotify( var Msg: TWMParentNotify );
begin
  inherited;

  if ( Msg.Event = wm_Create ) and ( FInternalHeaderHandle = 0 ) then
  begin
    FInternalHeaderHandle := HeaderHandle;
    FDefHeaderProc := Pointer( GetWindowLongPtr( HeaderHandle, GWL_WNDPROC ) );
    SetWindowLongPtr( HeaderHandle, GWL_WNDPROC, LONG_PTR( FHeaderInstance ) );
  end;

  SetHeaderODStyle;
end;


procedure TRzCustomListView.ResetHeaderHandle;
begin
  // Reset the internal HeaderHandle, which may have changed since the original
  // allocation if the list view is on a control that is part of a TFrame.

  if HeaderHandle <> 0 then
    SetWindowLongPtr( HeaderHandle, GWL_WNDPROC, LONG_PTR( FDefHeaderProc ) );
  FInternalHeaderHandle := HeaderHandle;
  FDefHeaderProc := Pointer( GetWindowLongPtr( HeaderHandle, GWL_WNDPROC ) );
  SetWindowLongPtr( HeaderHandle, GWL_WNDPROC, LONG_PTR( FHeaderInstance ) );
end;


// Since we do all the popup menu handling ourselves, we need to defeat Delphi's default handling. We do this by
// setting the popup menu's AutoPopup property false before allowing Delphi a shot at it.

procedure TRzCustomListView.WMRButtonUp( var Msg: TWMRButtonUp );
var
  OldPopup: TPopupMenu;
begin
  try
    if FMenuAlreadyHandled and Assigned( PopupMenu ) then
    begin
      OldPopup := PopupMenu;
      PopupMenu := nil;
      try
        inherited;
      finally
        PopupMenu := OldPopup;
      end;
    end
    else
      inherited;
  finally
    FMenuAlreadyHandled := FALSE;
  end;
end;


// Call this method when a menu is required, but you don't know if it should be
// the control or item one. If it should be the control one, then it is
// displayed. If it should be the item one, then the point at which it should
// popup is determined and passed to DoItemContextMenu.  'pt' is where the
// control menu should be placed if it is decided that it is the required menu.

procedure TRzCustomListView.DoPreItemContextMenu( pt: TPoint );
var
  p: TPoint;
  r: TRect;

  procedure DoDefault;
  var
    Handled: Boolean;
  begin
    Handled := False;
    DoContextPopup( pt, Handled );
    if Handled then
      Exit;

    if Assigned( PopupMenu ) and PopupMenu.AutoPopup then
    begin
      SendCancelMode( nil );
      PopupMenu.PopupComponent := self;
      with ClientToScreen( pt ) do
        PopupMenu.Popup( x, y );
    end;
  end;

begin {= TRzCustomListView.DoPreItemContextMenu =}
  if Assigned( ItemFocused ) and not FRightClicked then
  begin
    // The following code to determine the popup coordinate is derived from observation of Explorer (95/NT)
    // if the user presses the Shift+F10 key combination.
    if ItemFocused.Selected then
    begin
      r := ItemFocused.DisplayRect( drIcon );
      p := Point( ( r.Left + r.Right ) div 2, ( r.Bottom + r.Top ) div 2 );
      DoItemContextMenu( p );
    end
    else if ( SelCount > 0 ) and Assigned( Selected ) then // Some selections, but the current focus isn't one of them, second clause just paranoid safety level
    begin
      r := Selected.DisplayRect( drIcon );
      p := Point( ( r.Left + r.Right ) div 2, ( r.Bottom + r.Top ) div 2 );
      DoItemContextMenu( p );
    end
    else
    begin
      DoDefault;
    end;
  end
  else if not FRightClicked then
  begin
    DoDefault;
    FMenuAlreadyHandled := True;
  end
  else
  begin
    DoItemContextMenu( pt );
  end;
end; {= TRzCustomListView.DoPreItemContextMenu =}


// This method is called after it is known that the popup menu should be for an item.

procedure TRzCustomListView.DoItemContextMenu( p: TPoint );
var
  mnu: TPopupMenu;
begin
  mnu := PopupMenu;                         // Default to normal popup
  ItemContextMenu( ItemFocused, p, mnu );
  FMenuAlreadyHandled := True;
  if Assigned( mnu ) and PopupMenu.AutoPopup then
  begin
    mnu.PopupComponent := self;
    with ClientToScreen( p ) do
      mnu.Popup( x, y );
  end;
end;


procedure TRzCustomListView.ItemContextMenu( Item: TListItem; var Pos: TPoint; var Menu: TPopupMenu );
begin
  if Assigned( OnItemContextMenu ) then
    OnItemContextMenu( Self, Item, Pos, Menu );
end;


procedure TRzCustomListView.KeyDown( var Key: Word; ShiftState: TShiftState );
begin
  if ( ( Key = VK_APPS ) and ( ShiftState = [ ] ) ) or
     ( ( Key = VK_F10 ) and ( ShiftState = [ ssShift ] ) ) then
  begin
    Key := 0;
    DoPreItemContextMenu( Point( 0, 0 ) )
  end;
  inherited;
end;


procedure TRzCustomListView.DrawHeader( Index: Integer; const Rect: TRect );
var
  R: TRect;
begin
  HeaderCanvas.Font := Self.Font;
  HeaderCanvas.Font.Color := clBtnText;
  HeaderCanvas.Brush.Color := clBtnFace;

  R := Rect;
  if not ActiveStyleServicesEnabled then
  begin
    HeaderCanvas.FillRect( R );
    InflateRect( R, -2, -2 );
  end;

  if HeaderDefaultDrawing then
    DefaultDrawHeader( Index, R )
  else
    DoDrawHeader( Index, R );
end;


procedure TRzCustomListView.DoDrawHeader( Index: Integer; Rect: TRect );
begin
  if Assigned( FOnDrawHeader ) then
    FOnDrawHeader( Self, HeaderCanvas, Index, Rect );
end;


procedure TRzCustomListView.DrawHeaderSortGlyphs( Index: Integer; const Rect: TRect );
begin
  if Index = -1 then
    Exit;

  if ( Index = FHeaderSortColumn ) and ( Columns[ Index ].Width > ( Rect.Right - Rect.Left ) * 2 ) then
  begin
    if UsingSystemStyle then
    begin
      DrawArrow( HeaderCanvas, Rect, clBtnShadow, FHeaderSortDirection = sdAscending );
    end
    else // VCL Styles
    begin
      DrawArrow( HeaderCanvas, Rect, ActiveStyleFontColor( sfHeaderSectionTextNormal ), FHeaderSortDirection = sdAscending );
    end;
  end;
end;


procedure TRzCustomListView.DefaultDrawHeader( Index: Integer; const Rect: TRect );
const
  GLYPH_WIDTH = 9;
  TEXT_GLYPH_SPACING = 5;
var
  R, CaptionRect, SortGlyphRect: TRect;
  ElementDetails: TThemedElementDetails;


  function GetColumnAlignment: TAlignment;
  var
    Col: TLVColumn;
  begin
    Col.Mask := LVCF_FMT;
    if ListView_GetColumn( Handle, Index, Col ) then
    begin
      if ( Col.fmt and LVCFMT_RIGHT ) = LVCFMT_RIGHT then
        Result := taRightJustify
      else if ( Col.fmt and LVCFMT_CENTER ) = LVCFMT_CENTER then
        Result := taCenter
      else
        Result := taLeftJustify;
    end
    else
      Result := taLeftJustify;
  end;


  function GetColumnCaption: string;
  var
    Col: TLVColumn;
  begin
    Col.Mask := LVCF_TEXT;
    GetMem( Col.pszText, 255 );
    Col.cchTextMax := 255;
    try
      if ListView_GetColumn( Handle, Index, Col ) then
        Result := Col.pszText
      else
        Result := '';
    finally
      FreeMem( Col.pszText );
    end;
  end;


  procedure DoDrawText( var ARect: TRect; ACalcRect: Boolean );
  var
    A: TAlignment;
    S: string;
    Offset: Integer;
    C: TColor;
  begin
    if Index = -1 then
      Exit;

    // Do not use Columns[] property to access attributes of the column. When dragging columns that are displayed using
    // Owner-Drawn code, as the headers in the TRzListView do, the Columns property is not correctly sync'd up when this
    // method is called.
    // As a result, do not use Columns[ Index ].Caption or Columns[ Index ].Alignment
    // Use the ListView_GetColumn function instead.

    S := GetColumnCaption;
    A := GetColumnAlignment;

    if ( Columns[ Index ].ImageIndex <> -1 ) and ( SmallImages <> nil ) then
    begin
      Offset := ( ( ARect.Bottom - ARect.Top ) - SmallImages.Height ) div 2;
      SmallImages.Draw( HeaderCanvas, ARect.Left, ARect.Top + Offset, Columns[ Index ].ImageIndex );
      Inc( ARect.Left, SmallImages.Width + 4 );
    end;

    if UsingSystemStyle then
    begin
      if ActiveStyleServicesEnabled then
         HeaderCanvas.Brush.Style := bsClear
      else
        HeaderCanvas.Brush.Color := clBtnFace;
    end
    else // VCL Styles
    begin
      HeaderCanvas.Brush.Style := bsClear;
      if ( FOverHeaderSection = Index ) and ( Index <> -1 ) then
      begin
        if FHeaderSectionDown then
          C := ActiveStyleFontColor( sfHeaderSectionTextPressed )
        else
          C := ActiveStyleFontColor( sfHeaderSectionTextHot );
      end
      else
        C := ActiveStyleFontColor( sfHeaderSectionTextNormal );

      HeaderCanvas.Font.Color := C;
    end;

    DrawString( HeaderCanvas, S, ARect, dt_End_Ellipsis or dt_SingleLine or dt_VCenter or DrawTextAlignments[ A ] );

    if ACalcRect then
      DrawString( HeaderCanvas, S, ARect, dt_End_Ellipsis or dt_SingleLine or dt_VCenter or dt_CalcRect or dt_Right );
  end;


  function GetTextHeight( ARect: TRect ): Integer;
  var
    S: string;
  begin
    S := GetColumnCaption;

    DrawString( HeaderCanvas, S, ARect,
                DT_END_ELLIPSIS or DT_SINGLELINE or DT_CALCRECT or DT_RIGHT );

    Result := ARect.Bottom - ARect.Top;
  end;


  function CalcSortGlyphRect( Rect: TRect ): TRect;
  begin
    Result := Rect;
    Result.Right := Result.Left + 9;
  end;

begin {= TRzCustomListView.DefaultDrawHeader =}
  if ActiveStyleServicesEnabled then
  begin
    if ( FOverHeaderSection = Index ) and ( Index <> -1 ) then
    begin
      if FHeaderSectionDown then
        ElementDetails := ActiveStyleServices.GetElementDetails( thHeaderItemPressed )
      else
        ElementDetails := ActiveStyleServices.GetElementDetails( thHeaderItemHot );
    end
    else
      ElementDetails := ActiveStyleServices.GetElementDetails( thHeaderItemNormal );

    ActiveStyleServices.DrawElement( HeaderCanvas.Handle, ElementDetails, Rect );
  end
  else
  begin
    R := Rect;
    InflateRect( R, 2, 2 );
    if ( FOverHeaderSection = Index ) and ( Index <> -1 ) and FHeaderSectionDown then
      DrawButtonBorder( HeaderCanvas, R, True )
    else
      DrawButtonBorder( HeaderCanvas, R, False );
  end;


  R := Rect;
  if ActiveStyleServicesEnabled then
  begin
    InflateRect( R, -2, -2 );
    R.Left := R.Left + 4;
    R.Right := R.Right - 4;
  end
  else
  begin
    R.Left := R.Left + 4;
    R.Right := R.Right - 4;
  end;

  case HeaderSortDisplayMode of
    hsdmNone:
      DoDrawText( R, False );

    hsdmLeftAlign:
    begin
      if ( Index <> -1 ) and ( Index = FHeaderSortColumn ) and ( Columns[ Index ].Width > 15 ) then
      begin
        SortGlyphRect := CalcSortGlyphRect( R );
        DrawHeaderSortGlyphs( Index, SortGlyphRect );
        R.Left := SortGlyphRect.Right + TEXT_GLYPH_SPACING;
      end;
      DoDrawText( R, False );
    end;

    hsdmRightOfText:
    begin
      if GetColumnAlignment = taLeftJustify then
      begin
        CaptionRect := R;
        DoDrawText( CaptionRect, True );
        CaptionRect.Top := R.Top;
        CaptionRect.Bottom := R.Bottom;
        OffsetRect( CaptionRect, CaptionRect.Right - CaptionRect.Left + TEXT_GLYPH_SPACING, 0 );
        DrawHeaderSortGlyphs( Index, CalcSortGlyphRect( CaptionRect ) );
      end
      else
      begin
        // If text is right-justified then offset R by the glyph size
        if Index = FHeaderSortColumn then
        begin
          SortGlyphRect := CalcSortGlyphRect( R );
          OffsetRect( SortGlyphRect,
                      R.Right - R.Left - ( SortGlyphRect.Right - SortGlyphRect.Left ) - TEXT_GLYPH_SPACING, 0 );
          DrawHeaderSortGlyphs( Index, SortGlyphRect );

          if GetColumnAlignment = taRightJustify then
            R.Right := SortGlyphRect.Left - TEXT_GLYPH_SPACING
          else
            InflateRect( R, -( SortGlyphRect.Right - SortGlyphRect.Left + TEXT_GLYPH_SPACING ), 0 );
        end;
        DoDrawText( R, False );
      end;
    end;

    hsdmRightAlign:
    begin
      SortGlyphRect := CalcSortGlyphRect( R );
      OffsetRect( SortGlyphRect,
                  R.Right - R.Left - ( SortGlyphRect.Right - SortGlyphRect.Left ) - TEXT_GLYPH_SPACING, 0 );
      DrawHeaderSortGlyphs( Index, SortGlyphRect );

      if ( Index = FHeaderSortColumn ) then
        R.Right := SortGlyphRect.Left - TEXT_GLYPH_SPACING;

      DoDrawText( R, False );
    end;
  end;

end; {= TRzCustomListView.DefaultDrawHeader =}


procedure TRzCustomListView.SetHeaderODStyle;
var
  ColumnIndex: Integer;
  Header: THandle;
  HdrItemData: THDItem;
  OwnerDrawMode: Boolean;
begin
  if not HandleAllocated or ( ViewStyle <> vsReport ) then
  begin
    Exit;
  end;

  if FSetHeaderODStyleSem > 0 then          // Prevent reentrancy
  begin
    Exit;
  end;

  Inc( FSetHeaderODStyleSem );
  try
    OwnerDrawMode := ( HeaderSortDisplayMode <> hsdmNone );

    Header := HeaderHandle;
    if Header <> 0 then
    begin
      for ColumnIndex := Columns.Count - 1 downto 0 do
      begin
        {Set the HDF_OWNERDRAW flag while retaining the justify flag.}
        FillChar( HdrItemData, SizeOf( HdrItemData ), 0 );
        HdrItemData.Mask := HDI_FORMAT;
        Header_GetItem( Header, ColumnIndex, HdrItemData );

        if OwnerDrawMode then
        begin
          HdrItemData.fmt := ( HdrItemData.fmt and HDF_JUSTIFYMASK ) or HDF_OWNERDRAW;
        end
        else
        begin
          if ( Columns[ ColumnIndex ].ImageIndex <> -1 ) and ( SmallImages <> nil ) then
            HdrItemData.fmt := ( ( HdrItemData.fmt and HDF_JUSTIFYMASK ) and not HDF_OWNERDRAW ) or HDF_STRING or HDF_IMAGE
          else
            HdrItemData.fmt := ( ( HdrItemData.fmt and HDF_JUSTIFYMASK ) and not HDF_OWNERDRAW ) or HDF_STRING;
          if ( FHeaderSortColumn = ColumnIndex ) and ( HeaderSortDisplayMode <> hsdmNone ) then
          begin
            if FHeaderSortDirection = sdAscending then
              HdrItemData.fmt := HdrItemData.fmt or HDF_SORTUP
            else
              HdrItemData.fmt := HdrItemData.fmt or HDF_SORTDOWN;
          end;
        end;

        Header_SetItem( Header, ColumnIndex, HdrItemData );
      end;

      InvalidateRect( Header, nil, True );
    end;
  finally
    Dec( FSetHeaderODStyleSem );
  end;
end; {= TRzCustomListView.SetHeaderODStyle =}


procedure TRzCustomListView.HeaderWndProc( var Msg: TMessage );
var
  HTInfo: THDHitTestInfo;
  MouseEvent: TTrackMouseEvent;
  HeaderDC: HDC;
  PS: TPaintStruct;
  ColumnIndex, RightOffset, I, LastSection: Integer;
  R, HeaderR, SectionR, LastSectionR: TRect;
  SectionOrder: array of Integer;
  Item: THDItem;
  Buffer: array[ 0..255 ] of Char;
begin
  try
    case Msg.Msg of
      wm_NCHitTest:
      begin
        if csDesigning in ComponentState then
        begin
          Msg.Result := Windows.HTTRANSPARENT;
          Exit;
        end;
      end;

      wm_NCDestroy:
      begin
        Msg.Result := CallWindowProc( FDefHeaderProc, HeaderHandle, Msg.Msg, Msg.WParam, Msg.LParam );
        FDefHeaderProc := nil;
        Exit;
      end;


      wm_Paint:
      begin
        if TWMPaint( Msg ).DC = 0 then
          HeaderDC := BeginPaint( HeaderHandle, PS )
        else
          HeaderDC := TWMPaint( Msg ).DC;

        try
          try
            HeaderCanvas.Handle := HeaderDC;

            RightOffset := 0;

            for I := 0 to Header_GetItemCount( HeaderHandle ) - 1 do
            begin
              SetLength( SectionOrder, Header_GetItemCount( HeaderHandle ) );
              Header_GetOrderArray( HeaderHandle, Header_GetItemCount( HeaderHandle ), Pointer( SectionOrder ) );
              ColumnIndex := SectionOrder[ I ];
              Header_GETITEMRECT( HeaderHandle, ColumnIndex, @R );
              FillChar( Item, SizeOf( Item ), 0 );
              Item.Mask := HDI_TEXT;
              Item.pszText := @Buffer;
              Item.cchTextMax := Length( Buffer );
              Header_GetItem( HeaderHandle, ColumnIndex, Item );

              DrawHeader( ColumnIndex, R );

              if RightOffset < R.Right then
                RightOffset := R.Right;
            end;

            GetWindowRect( HeaderHandle, HeaderR );
            R := Rect( RightOffset, 0, HeaderR.Right - HeaderR.Left + 2, HeaderR.Bottom - HeaderR.Top );

            if not IsRectEmpty( R ) then
              DrawHeader( -1, R );

            if TWMPaint( Msg ).DC <> 0 then
              ReleaseDC( HeaderHandle, TWMPaint( Msg ).DC );

          finally
            HeaderCanvas.Handle := 0;
            Msg.Result := 1;
          end;

        finally
          if TWMPaint( Msg ).DC = 0  then
            EndPaint( HeaderHandle, PS )
        end;
      end;

      wm_MouseMove:
      begin
        // Handle moving over the header--needed for XP Theme support
        if not FHeaderSectionDown then
        begin
          HTInfo.Point.X := SmallInt( Msg.LParam and $FFFF );
          HTInfo.Point.Y := SmallInt( ( Msg.LParam and $FFFF0000 ) shr 16 );

          LastSection:= FOverHeaderSection;
          FOverHeaderSection := SendMessage( HeaderHandle, hdm_HitTest, 0, LParam( @HTInfo ) );

          if ( FOverHeaderSection <> LastSection ) and ActiveStyleServicesEnabled then
          begin
            // If over a different section and themes are being used, then invalidate previous and current section headers
            SendMessage( HeaderHandle, hdm_GetItemRect, LastSection, LParam( @LastSectionR ) );
            InvalidateRect( HeaderHandle, LastSectionR, False );
            SendMessage( HeaderHandle, hdm_GetItemRect, FOverHeaderSection, LParam( @SectionR ) );
            InvalidateRect( HeaderHandle, SectionR, False );
          end;
        end;


        if ActiveStyleServicesEnabled and not FMouseInNonClient then
        begin
          FMouseInNonClient := True;

          // Register for a WM_MOUSELEAVE message which we will use to ensure
          // that header gets repainted when the mouse leaves the header.
          MouseEvent.cbSize := SizeOf( MouseEvent );
          MouseEvent.dwFlags := TME_LEAVE;
          MouseEvent.hwndTrack := Handle;
          MouseEvent.dwHoverTime := HOVER_DEFAULT;
          _TrackMouseEvent( @MouseEvent );
        end;
      end;

      wm_MouseLeave:
      begin
        FMouseInNonClient := False;

        if ActiveStyleServicesEnabled then
        begin
          SendMessage( HeaderHandle, hdm_GetItemRect, FOverHeaderSection, LParam( @SectionR ) );
          InvalidateRect( HeaderHandle, SectionR, False );
        end;

        FOverHeaderSection := -1;
      end;

      wm_LButtonDown:
      begin
        FHeaderSectionDown := True;
      end;

      wm_LButtonUp:
      begin
        FHeaderSectionDown := False;
        if ActiveStyleServicesEnabled then
        begin
          SendMessage( HeaderHandle, hdm_GetItemRect, FOverHeaderSection, LParam( @SectionR ) );
          InvalidateRect( HeaderHandle, SectionR, False );
        end;
      end;
    end;
    Msg.Result := CallWindowProc( FDefHeaderProc, HeaderHandle, Msg.Msg, Msg.WParam, Msg.LParam );
  except
    Application.HandleException( Self );
  end;
end;




{&RUIF}

end.


