{===============================================================================
  RzPathBar Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzPathBar
    Navigational control that manages a set of path items that can be used to
    create a "you are here" path -- sometimes referred to as a
    "breadcrumb path".

  Modification History
  ------------------------------------------------------------------------------
  6.1.4  (29 May 2013)
    * Enhanced the TRzPathBar such that if there is a hint associated with the
      the last item, it is displayed when the mouse is hovered over the item
      and ShowHint is set to True as well.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzPathBar to fully support VCL Styles
      introduced in RAD Studio XE2.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzPathBar control.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added ItemHotStyle property to TRzPathBar. This property is used to change
      how an item appears when the mouse is positioned over the item. The
      default is ihsBox. The other options is ihsUnderline.
    * When running under right-to-left locales and the default Separator is used
      (i.e. '>'), the character is automatically switched to the '<' symbol.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surfaced GradientColorStart, GradientColorStop, GradientDirection
      properties in TRzPathBar from the TRzCustomPanel ancestor class.
    * Surfaced VisualStyle and GradientColorStyle properties in TRzPathBar.
    * Updated display of TRzPathBar items to match capabilities of toolbar
      buttons and menu items (using TRzMenuController).
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Fixed problem where TRzPathBar.OnClick event would also fire if an item
      in the path bar was clicked.
    * Fixed problem where the PopupMenu associated with a TRzPathBar control
      would also get displayed in addition to the ItemPopupMenu when the user
      invoked a context menu on an item.
  ------------------------------------------------------------------------------
  3.0.10  (26 Dec 2003)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzPathBar;

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
  RzPanel,
  RzGroupBar,
  RzCommon;


type
  TRzPathBar = class;
  TRzPathItem = class;

  {=============================================}
  {== TRzPathItemActionLink Class Declaration ==}
  {=============================================}

  TRzPathItemActionLink = class( TActionLink )
  protected
    FClient: TRzPathItem;
    procedure AssignClient( AClient: TObject ); override;
    function DoShowHint( var HintStr: string ): Boolean; virtual;

    function IsCaptionLinked: Boolean; override;
    function IsHintLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    function IsOnExecuteLinked: Boolean; override;

    procedure SetCaption( const Value: string ); override;
    procedure SetHint( const Value: string ); override;
    procedure SetImageIndex( Value: Integer ); override;
    procedure SetOnExecute( Value: TNotifyEvent ); override;
  end;

  TRzPathItemActionLinkClass = class of TRzPathItemActionLink;

  {===================================}
  {== TRzPathItem Class Declaration ==}
  {===================================}

  TRzPathItem = class( TCollectionItem )
  private
    FCaption: TCaption;
    FCaptionChanged: Boolean;
    FHighlightColor: TColor;
    FHint: string;
    FImageIndex: TImageIndex;
    FLinkInfo: string;
    FLinkObject: TObject;
    FTag: Longint;
    FOnClick: TNotifyEvent;

    FActionLink: TRzPathItemActionLink;
    FClickingCaption: Boolean;
    FCaptionState: TRzCaptionState;
    FHotRect: TRect;

    { Internal Event Handlers }
    procedure ActionChangeHandler( Sender: TObject );
  protected
    function GetDisplayName: string; override;

    procedure ActionChange( Sender: TObject; CheckDefaults: Boolean ); dynamic;

    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    function IsHelpContextStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsOnClickStored: Boolean;

    function GetAction: TBasicAction; virtual;
    procedure SetAction( Value: TBasicAction ); virtual;
    function GetActionLinkClass: TRzPathItemActionLinkClass; dynamic;
    procedure SetCaption( const Value: TCaption ); virtual;
    procedure SetHighlightColor( Value: TColor ); virtual;
    function GetPathBar: TRzPathBar; virtual;
    procedure SetImageIndex( Value: TImageIndex ); virtual;

    property ActionLink: TRzPathItemActionLink
      read FActionLink
      write FActionLink;
  public
    constructor Create( Collection: TCollection ); override;
    destructor Destroy; override;

    procedure Assign( Source: TPersistent ); override;
    procedure AssignTo( Dest: TPersistent ); override;

    procedure InitiateAction; virtual;
    procedure Click; dynamic;

    property HotRect: TRect
      read FHotRect
      write FHotRect;

    property CaptionState: TRzCaptionState
      read FCaptionState
      write FCaptionState;

    property ClickingCaption: Boolean
      read FClickingCaption
      write FClickingCaption;

    property LinkObject: TObject
      read FLinkObject
      write FLinkObject;

    property PathBar: TRzPathBar
      read GetPathBar;
  published
    property Action: TBasicAction
      read GetAction
      write SetAction;

    property Caption: TCaption
      read FCaption
      write SetCaption
      stored IsCaptionStored;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clNone;

    property Hint: string
      read FHint
      write FHint
      stored IsHintStored;

    property ImageIndex: TImageIndex
      read FImageIndex
      write SetImageIndex
      stored IsImageIndexStored
      default -1;

    property LinkInfo: string
      read FLinkInfo
      write FLinkInfo;

    property Tag: Longint
      read FTag
      write FTag
      default 0;

    property OnClick: TNotifyEvent
      read FOnClick
      write FOnClick
      stored IsOnClickStored;
  end;


  {====================================}
  {== TRzPathItems Class Declaration ==}
  {====================================}

  TRzPathItems = class( TCollection )
  private
    FPathBar: TRzPathBar;
    function GetItem( Index: Integer ): TRzPathItem;
    procedure SetItem( Index: Integer; Value: TRzPathItem );
  protected
    function GetOwner: TPersistent; override;
    procedure Update( Item: TCollectionItem ); override;
  public
    // Note: No override on constructor
    constructor Create( PathBar: TRzPathBar );

    function Add: TRzPathItem;

    // Array property provides access to collection items
    property Items[ Index: Integer ]: TRzPathItem
      read GetItem
      write SetItem; default;

    property PathBar: TRzPathBar
      read FPathBar;
  end;


  {==================================}
  {== TRzPathBar Class Declaration ==}
  {==================================}

  TRzMouseOverPathItemEvent = procedure( Sender: TObject; Item: TRzPathItem ) of object;

  TRzPathItemPopupMenuEvent = procedure( Sender: TObject; Item: TRzPathItem ) of object;

  TRzItemHotStyle = ( ihsBox, ihsUnderline );

  TRzPathBar = class( TRzCustomPanel )
  private
    FItemHotColor: TColor;
    FItemDownColor: TColor;
    FHighlightColor: TColor;
    FBoldLastItem: Boolean;
    FItemHotStyle: TRzItemHotStyle;

    FCaptionState: TRzCaptionState;
    FItemCursor: HCursor;

    FItems: TRzPathItems;
    FActionClientCount: Integer;

    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;

    FSeparator: string;
    FSeparatorFont: TFont;

    FOnMouseOverItem: TRzMouseOverPathItemEvent;

    FItemPopupMenu: TPopupMenu;
    FPopupItem: TRzPathItem;
    FOnItemPopupMenu: TRzPathItemPopupMenuEvent;

    { Internal Event Handlers }
    procedure ImagesChange( Sender: TObject );

    { Message Handling Methods }
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMHintShow( var Msg: TMessage ); message cm_HintShow;
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
  protected
    procedure DrawItems; virtual;

    procedure Paint; override;

    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateDownColors;

    procedure DragOver( Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean ); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;

    procedure DisplayItemPopupMenu( Index, X, Y: Integer );

    { Event Dispatch Methods }
    procedure Click; override;
    procedure DoContextPopup( MousePos: TPoint; var Handled: Boolean ); override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseOverItem( Item: TRzPathItem ); dynamic;

    { Property Access Methods }
    procedure SetBoldLastItem( Value: Boolean ); virtual;
    procedure SetItemHotColor( Value: TColor ); virtual;
    procedure SetHighlightColor( Value: TColor ); virtual;
    procedure SetItemHotStyle( Value: TRzItemHotStyle ); virtual;

    procedure SetItems( Value: TRzPathItems ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;

    procedure SetSeparator( const Value: string ); virtual;
    procedure SetSeparatorFont( Value: TFont ); virtual;
    procedure SetItemPopupMenu( Value: TPopupMenu ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure InitiateAction; override;
    procedure ActionClientConnect;
    procedure ActionClientDisconnect;
    procedure AssignActionList( ActionList: TCustomActionList; const Category: string = '' );

    function ItemAtPos( P: TPoint ): Integer;

    property PopupItem: TRzPathItem
      read FPopupItem;

    property Canvas;
    property DockManager;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property BoldLastItem: Boolean
      read FBoldLastItem
      write SetBoldLastItem
      default True;

    property Items: TRzPathItems
      read FItems
      write SetItems;

    property ItemHotColor: TColor
      read FItemHotColor
      write SetItemHotColor
      default clHotLight;

    property ItemPopupMenu: TPopupMenu
      read FItemPopupMenu
      write SetItemPopupMenu;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property ItemHotStyle: TRzItemHotStyle
      read FItemHotStyle
      write SetItemHotStyle
      default ihsBox;

    property Separator: string
      read FSeparator
      write SetSeparator;

    property SeparatorFont: TFont
      read FSeparatorFont
      write SetSeparatorFont;

    property OnMouseOverItem: TRzMouseOverPathItemEvent
      read FOnMouseOverItem
      write FOnMouseOverItem;

    property OnItemPopupMenu: TRzPathItemPopupMenuEvent
      read FOnItemPopupMenu
      write FOnItemPopupMenu;

    { Inherited Properties & Events }
    property Align default alTop;
    property Alignment;
    property AlignmentVertical;
    property Anchors;
    property AutoSize;
    property BevelWidth;
    property BiDiMode;
    property BorderInner;
    property BorderOuter default fsNone;
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
    property Height default 24;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
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



{&RT}
{===================================}
{== TRzPathItemActionLink Methods ==}
{===================================}

procedure TRzPathItemActionLink.AssignClient( AClient: TObject );
begin
  FClient := AClient as TRzPathItem;
end;


function TRzPathItemActionLink.DoShowHint( var HintStr: string ): Boolean;
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


function TRzPathItemActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked and ( FClient.Caption = ( Action as TCustomAction ).Caption );
end;


function TRzPathItemActionLink.IsHintLinked: Boolean;
begin
  Result := inherited IsHintLinked and ( FClient.Hint = ( Action as TCustomAction ).Hint );
end;


function TRzPathItemActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and ( FClient.ImageIndex = ( Action as TCustomAction ).ImageIndex );
end;


function TRzPathItemActionLink.IsOnExecuteLinked: Boolean;
begin
  Result := inherited IsOnExecuteLinked and ( @FClient.OnClick = @Action.OnExecute );
end;


procedure TRzPathItemActionLink.SetCaption( const Value: string );
begin
  if IsCaptionLinked then
    FClient.Caption := Value;
end;


procedure TRzPathItemActionLink.SetHint( const Value: string );
begin
  if IsHintLinked then
    FClient.Hint := Value;
end;


procedure TRzPathItemActionLink.SetImageIndex( Value: Integer );
begin
  if IsImageIndexLinked then
    FClient.ImageIndex := Value;
end;


procedure TRzPathItemActionLink.SetOnExecute( Value: TNotifyEvent );
begin
  if IsOnExecuteLinked then
    FClient.OnClick := Value;
end;



{=========================}
{== TRzPathItem Methods ==}
{=========================}

constructor TRzPathItem.Create( Collection: TCollection );
begin
  inherited;

  if ( csDesigning in PathBar.ComponentState ) and not ( csLoading in PathBar.ComponentState ) then
    FCaption := 'Item' + IntToStr( Index + 1 );

  FHighlightColor := clNone;
  // Call access method and not set FImageIndex b/c we want the Update method to be called on the Collection
  // after adding a new item.  Otherwise, the PathBar does not show the new item correctly.
  SetImageIndex( -1 );
end;


destructor TRzPathItem.Destroy;
begin
  FActionLink.Free;
  FActionLink := nil;
  inherited;
end;


procedure TRzPathItem.Assign( Source: TPersistent );
begin
  if Source is TRzPathItem then
  begin
    Action := TRzPathItem( Source ).Action;
    Caption := TRzPathItem( Source ).Caption;
    Hint := TRzPathItem( Source ).Hint;
    ImageIndex := TRzPathItem( Source ).ImageIndex;
    OnClick := TRzPathItem( Source ).OnClick;
  end
  else
    inherited;
end;


procedure TRzPathItem.AssignTo( Dest: TPersistent );
begin
  if Dest is TCustomAction then
  begin
    TCustomAction( Dest ).Caption := Self.Caption;
    TCustomAction( Dest ).Hint := Self.Hint;
    TCustomAction( Dest ).ImageIndex := Self.ImageIndex;
    TCustomAction( Dest ).OnExecute := Self.OnClick;
  end
  else
    inherited;
end;



function TRzPathItem.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := inherited GetDisplayName;
end;


procedure TRzPathItem.Click;
begin
  // Call OnClick if assigned and not equal to associated action's OnExecute.
  // If associated action's OnExecute assigned then call it, otherwise, call OnClick.

  if Assigned( FOnClick ) and ( Action <> nil ) and ( @FOnClick <> @Action.OnExecute ) then
    FOnClick( Self )
  else if not ( csDesigning in PathBar.ComponentState ) and ( ActionLink <> nil ) then
  begin
    ActionLink.Execute( PathBar );
  end
  else if Assigned( FOnClick ) then
    FOnClick( Self );
end;


function TRzPathItem.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action
  else
    Result := nil;
end;


procedure TRzPathItem.SetAction( Value: TBasicAction );
begin
  if Value = nil then
  begin
    ActionLink.Free;
    ActionLink := nil;
    PathBar.ActionClientDisconnect;
  end
  else
  begin
    PathBar.ActionClientConnect;
    if ActionLink = nil then
      ActionLink := GetActionLinkClass.Create( Self );
    ActionLink.Action := Value;
    ActionLink.OnChange := ActionChangeHandler;
    ActionChange( Value, csLoading in Value.ComponentState );

    Value.FreeNotification( PathBar );
  end;
end;


procedure TRzPathItem.ActionChange( Sender: TObject; CheckDefaults: Boolean );
var
  NewAction: TCustomAction;
begin
  if Sender is TCustomAction then
  begin
    NewAction := TCustomAction( Sender );

    if not CheckDefaults or ( Self.Caption = '' ) or not FCaptionChanged then
      Self.Caption := NewAction.Caption;

    if not CheckDefaults or ( Self.Hint = '' ) then
      Self.Hint := NewAction.Hint;

    if not CheckDefaults or ( Self.ImageIndex = -1 ) then
      Self.ImageIndex := NewAction.ImageIndex;

    if not CheckDefaults or not Assigned( Self.OnClick ) then
      Self.OnClick := NewAction.OnExecute;
  end;
end;


procedure TRzPathItem.ActionChangeHandler( Sender: TObject );
begin
  if Sender = Action then
    ActionChange( Sender, False );
end;


function TRzPathItem.GetActionLinkClass: TRzPathItemActionLinkClass;
begin
  Result := TRzPathItemActionLink;
end;


procedure TRzPathItem.InitiateAction;
begin
  if ActionLink <> nil then
    ActionLink.Update;
end;


function TRzPathItem.IsCaptionStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsCaptionLinked;
end;


function TRzPathItem.IsHintStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsHintLinked;
end;


function TRzPathItem.IsHelpContextStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsHelpContextLinked;
end;


function TRzPathItem.IsImageIndexStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsImageIndexLinked;
end;


function TRzPathItem.IsOnClickStored: Boolean;
begin
  Result := ( ActionLink = nil ) or not ActionLink.IsOnExecuteLinked;
end;


procedure TRzPathItem.SetCaption( const Value: TCaption );
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    FCaptionChanged := True;
    // Changed causes TRzPathItems.Update to be called.  Passing True is interpreted that ALL items have been changed.
    // For this component, this is sufficient.
    Changed( True );
  end;
end;


procedure TRzPathItem.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    Changed( True );
  end;
end;


function TRzPathItem.GetPathBar: TRzPathBar;
begin
  Result := TRzPathItems( Collection ).PathBar;
end;


procedure TRzPathItem.SetImageIndex( Value: TImageIndex );
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed( True );                                       // Causes TRzPathItems.Update to be called
  end;
end;



{==========================}
{== TRzPathItems Methods ==}
{==========================}

constructor TRzPathItems.Create( PathBar: TRzPathBar );
begin
  // Inherited constructor is passed the "type" of the collection
  // item that the collection will manage.
  inherited Create( TRzPathItem );
  FPathBar := PathBar;
end;


function TRzPathItems.Add: TRzPathItem;
begin
  Result := TRzPathItem( inherited Add );
end;


function TRzPathItems.GetItem( Index: Integer ): TRzPathItem;
begin
  Result := TRzPathItem( inherited GetItem( Index ) );
end;


procedure TRzPathItems.SetItem( Index: Integer; Value: TRzPathItem );
begin
  // Must specify SetItem b/c SetItem is not virtual
  inherited SetItem( Index, Value );
end;


function TRzPathItems.GetOwner: TPersistent;
begin
  Result := FPathBar;
end;


procedure TRzPathItems.Update( Item: TCollectionItem );
begin
  // If Item is nil, assume all items have changed
  // Otherwise, Item represents the item that has changed

  // Caption and other properties may have changed -- request that PathBar reposition the items
  FPathBar.Repaint;
end;



{========================}
{== TRzPathBar Methods ==}
{========================}

constructor TRzPathBar.Create( AOwner: TComponent );
begin
  inherited;

  DoubleBuffered := True;
  BorderOuter := fsNone;

  FBoldLastItem := True;
  FItemHotColor := clHotLight;
  UpdateDownColors;
  FHighlightColor := clHighlight;
  FItemHotStyle := ihsBox;

  {&RCI}

  FCaptionState := csNormal;

  if RunningUnder( WinNT ) or RunningUnder( Win95 ) then
    FItemCursor := LoadCursor( HInstance, 'RZCOMMON_HANDCURSOR' )
  else
    FItemCursor := LoadCursor( 0, IDC_HAND );

  Align := alTop;
  Height := 24;

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;

  FActionClientCount := 0;

  FItems := TRzPathItems.Create( Self );

  FSeparator := '>';
  FSeparatorFont := TFont.Create;
end; {= TRzPathBar.Create =}


destructor TRzPathBar.Destroy;
begin
  FItems.Free;
  FImagesChangeLink.Free;
  FSeparatorFont.Free;

  if RunningUnder( WinNT ) or RunningUnder( Win95 ) then
    DestroyCursor( FItemCursor );

  inherited;
end;


procedure TRzPathBar.Loaded;
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


procedure TRzPathBar.Notification( AComponent: TComponent; Operation: TOperation );
var
  I: Integer;
begin
  inherited;

  if Operation = opRemove then
  begin
    if AComponent = FImages then
      SetImages( nil )
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


procedure TRzPathBar.InitiateAction;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FItems.Count - 1 do
    FItems[ I ].InitiateAction;
end;


procedure TRzPathBar.ActionClientConnect;
begin
  Inc( FActionClientCount );
  ControlStyle := ControlStyle + [ csActionClient ];
end;


procedure TRzPathBar.ActionClientDisconnect;
begin
  Dec( FActionClientCount );
  if FActionClientCount = 0 then
    ControlStyle := ControlStyle - [ csActionClient ];
end;


procedure TRzPathBar.AssignActionList( ActionList: TCustomActionList; const Category: string = '' );
var
  I: Integer;
  Item: TRzPathItem;
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


procedure TRzPathBar.DrawItems;
var
  R, ItemRect: TRect;
  I, X, ImageXStart, ImageYStart, ImageWidth, TextXStart, TextYStart, TextWidth, TextHeight: Integer;
  ItemCaption, S: string;
  SelectionBorderColor, SelectionStartColor, SelectionStopColor, SelectionFontColor: TColor;
  ItemFontColor, ItemFontHotColor, ItemFontDownColor: TColor;

  procedure UpdateFontColorAndStyle( Index: Integer );
  begin
    case FItems[ Index ].CaptionState of
      csNormal:
      begin
        Canvas.Font.Color := ItemFontColor;

        if ( Index = FItems.Count - 1 ) and FBoldLastItem then
          Canvas.Font.Style := [ fsBold ]
        else
          Canvas.Font.Style := [];
      end;

      csHot:
      begin
        Canvas.Font.Color := ItemFontHotColor;

        if ItemHotStyle = ihsUnderline then
          Canvas.Font.Style := [ fsUnderline ];
      end;

      csDown:
      begin
        Canvas.Font.Color := ItemFontDownColor;

        if ItemHotStyle = ihsUnderline then
          Canvas.Font.Style := [ fsUnderline ];
      end;
    end;
  end; {= UpdateFontColorAndStyle =}


begin {= TRzPathBar.DrawItems =}

  Canvas.Brush.Style := bsClear;
  Canvas.Font := Self.Font;

  // Determine appropriate Font colors

  if UsingSystemStyle then
  begin
    ItemFontColor := Self.Font.Color;
    ItemFontHotColor := FItemHotColor;
  end
  else // VCL Styles
  begin
    ItemFontColor := ActiveStyleFontColor( sfButtonTextNormal );
    ItemFontHotColor := ActiveStyleSystemColor( clHighlight );
  end;
  ItemFontDownColor := BlendColors( clBlack, ItemFontColor, 50 );


  if GradientColorStyle <> gcsCustom then
  begin
    GetGradientSelectionColors( GradientColorStyle, SelectionBorderColor,
                                SelectionStartColor, SelectionStopColor );
  end;
  SelectionFontColor := ItemFontColor;



  X := BorderWidth + 2;
  TextHeight := Canvas.TextHeight( 'Pp' );
  TextYStart := BorderWidth + ( Height - 2 * BorderWidth - TextHeight ) div 2;
  if FImages <> nil then
  begin
    ImageYStart := BorderWidth + ( Height - 2 * BorderWidth - FImages.Height ) div 2;
    ImageWidth := FImages.Width;
  end
  else
  begin
    ImageYStart := 0;
    ImageWidth := 0;
  end;

  R := ClientRect;
  FixClientRect( R, True );
  IntersectClipRect( Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom );
  try
    // Draw each item in view

    for I := 0 to FItems.Count - 1 do
    begin
      Canvas.Font := Self.Font;
      UpdateFontColorAndStyle( I );
      Canvas.Brush.Style := bsClear;

      // Calc display size of item (includes both image and text)

      ItemRect := Rect( X, 0, X, 0 );

      if FItems[ I ].Caption <> '' then
      begin
        ItemCaption := RemoveAccelerators( FItems[ I ].Caption );
        TextWidth := Canvas.TextWidth( ItemCaption );
      end
      else
      begin
        ItemCaption := '';
        TextWidth := 0;
      end;

      if ( FImages <> nil ) and ( FItems[ I ].ImageIndex <> -1 ) then
        ItemRect := Rect( X, ImageYStart - 2, X + FImages.Width + 4 + TextWidth + 4, ImageYStart + FImages.Height + 2 )
      else
        ItemRect := Rect( X, TextYStart - 2, X + TextWidth + 4, TextYStart + TextHeight + 2 );

      if UseRightToLeftAlignment then
        ItemRect := Rect( Width - ItemRect.Right, ItemRect.Top, Width - ItemRect.Left, ItemRect.Bottom );

      // Display "hot" or "down" style background

      if ( FItemHotStyle = ihsBox ) and ( FItems[ I ].CaptionState in [ csHot, csDown ] ) then
      begin
        if UsingSystemStyle and ( GradientColorStyle = gcsCustom ) then
        begin
          if FItems[ I ].HighlightColor = clNone then
            SelectionBorderColor := FHighlightColor
          else
            SelectionBorderColor := FItems[ I ].HighlightColor;

          SelectionStartColor := LighterColor( SelectionBorderColor, 80 );
          SelectionStopColor := LighterColor( SelectionBorderColor, 40 );
        end;

        R := ItemRect;
        R := DrawBox( Canvas, R, SelectionBorderColor );
        if FItems[ I ].CaptionState = csHot then
          PaintGradient( Canvas, R, gdHorizontalEnd, SelectionStartColor, SelectionStopColor )
        else
          PaintGradient( Canvas, R, gdHorizontalEnd, SelectionStopColor, SelectionStartColor );
        Canvas.Font.Color := SelectionFontColor;
      end;

      // Draw image and caption

      if ( FImages <> nil ) and ( FItems[ I ].ImageIndex <> -1 ) then
      begin
        ImageXStart := ItemRect.Left + 2;

        if UseRightToLeftAlignment then
          ImageXStart := ItemRect.Right - FImages.Width - 2;

        FImages.Draw( Canvas, ImageXStart, ImageYStart, FItems[ I ].ImageIndex );
      end;

      if ItemCaption <> '' then
      begin
        if ( FImages <> nil ) and ( FItems[ I ].ImageIndex <> -1 ) and ( ImageWidth > 0 ) then
          TextXStart := ItemRect.Left + 2 + ImageWidth + 4
        else
          TextXStart := ItemRect.Left + 2;

        if not UseRightToLeftAlignment then
        begin
          R := Rect( TextXStart, TextYStart, ItemRect.Right, TextYStart + TextHeight );
          DrawString( Canvas, ItemCaption, R, dt_ExpandTabs or dt_NoPrefix );
        end
        else
        begin
          R := Rect( ItemRect.Left, TextYStart, ItemRect.Left + ItemRect.Right - TextXStart, TextYStart + TextHeight );
          DrawString( Canvas, ItemCaption, R, dt_ExpandTabs or dt_NoPrefix or dt_Right or dt_RtlReading );
        end;

        Canvas.Brush.Style := bsClear;   // Restore brush to clear in case just drew selected item.
      end;


      // Set item's HotRect for use in hit testing later
      FItems[ I ].HotRect := ItemRect;

      Inc( X, ItemRect.Right - ItemRect.Left );


      // Draw path separator if necessary
      if I < FItems.Count - 1 then
      begin
        Canvas.Font := FSeparatorFont;
        if UsingSystemStyle then
          Canvas.Font.Color := FSeparatorFont.Color
        else
          Canvas.Font.Color := ActiveStyleFontColor( sfButtonTextNormal );

        if not UseRightToLeftAlignment then
        begin
          R := Rect( ItemRect.Right, ItemRect.Top, ItemRect.Right + 20, ItemRect.Bottom );
          DrawString( Canvas, FSeparator, R, dt_Center or dt_VCenter or dt_SingleLine or
                                             dt_ExpandTabs or dt_NoPrefix )
        end
        else
        begin
          S := FSeparator;
          if S = '>' then
            S := '<'; // If using default separator and running in RTL, switch to '<'
          R := Rect( ItemRect.Left - 20, ItemRect.Top, ItemRect.Left, ItemRect.Bottom );
          DrawString( Canvas, S, R, dt_Center or dt_VCenter or dt_SingleLine or
                                          dt_ExpandTabs or dt_NoPrefix or dt_Right or dt_RtlReading );
        end;

        Inc( X, 20 );
      end;

    end; { for I }
  finally
    SelectClipRgn( Canvas.Handle, 0 );                     // Removing clipping region
    Canvas.Brush.Style := bsSolid;
  end;
end; {= TRzPathBar.DrawItems =}


procedure TRzPathBar.Paint;
begin
  inherited;
  DrawItems;
end;


procedure TRzPathBar.UpdateDownColors;
begin
  FItemDownColor := BlendColors( clBtnShadow, FItemHotColor, 128 );
end;


procedure TRzPathBar.SetBoldLastItem( Value: Boolean );
begin
  if FBoldLastItem <> Value then
  begin
    FBoldLastItem := Value;
    Invalidate;
  end;
end;


procedure TRzPathBar.SetItemHotColor( Value: TColor );
begin
  if FItemHotColor <> Value then
  begin
    FItemHotColor := Value;
    UpdateDownColors;
    Invalidate;
  end;
end;


procedure TRzPathBar.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    Invalidate;
  end;
end;


procedure TRzPathBar.SetItemHotStyle( Value: TRzItemHotStyle );
begin
  if FItemHotStyle <> Value then
  begin
    FItemHotStyle := Value;
    Invalidate;
  end;
end;


procedure TRzPathBar.SetItems( Value: TRzPathItems );
begin
  FItems.Assign( Value );
end;


procedure TRzPathBar.SetSeparator( const Value: string );
begin
  if FSeparator <> Value then
  begin
    FSeparator := Value;
    Invalidate;
  end;
end;


procedure TRzPathBar.SetSeparatorFont( Value: TFont );
begin
  FSeparatorFont.Assign( Value );
end;


procedure TRzPathBar.SetItemPopupMenu( Value: TPopupMenu );
begin
  if FItemPopupMenu <> Value then
  begin
    FItemPopupMenu := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


procedure TRzPathBar.SetImages( Value: TCustomImageList );
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


procedure TRzPathBar.ImagesChange( Sender: TObject );
begin
  Update;
end;


procedure TRzPathBar.DragOver( Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean );
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


procedure TRzPathBar.DoEndDrag(Target: TObject; X, Y: Integer);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    FItems[ I ].CaptionState := csNormal;
    FItems[ I ].ClickingCaption := False;
  end;
end;


function TRzPathBar.ItemAtPos( P: TPoint ): Integer;
var
  I: Integer;
begin
  Result := -1;

  if not PtInRect( ClientRect, P ) then
    Exit;

  for I := 0 to FItems.Count - 1 do
  begin
    if PtInRect( FItems[ I ].HotRect, P ) then
    begin
      Result := I;
      Break;
    end;
  end;
end;



procedure TRzPathBar.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  I: Integer;
begin
  inherited;

  if ( Button = mbLeft ) and Enabled and ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    // Check to see if user is clicking LEFT button on an enabled item at runtime

    for I := 0 to FItems.Count - 2 do
    begin
      if ( Assigned( FItems[ I ].OnClick ) or ( FItems[ I ].Action <> nil ) ) and
         PtInRect( FItems[ I ].HotRect, Point( X, Y ) ) then
      begin
        FItems[ I ].CaptionState := csDown;
        Invalidate;
        FItems[ I ].ClickingCaption := True;
        Break;
      end;
    end;
  end
  else if ( Button = mbRight ) and Enabled and ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    // Check to see if user is clicking RIGHT button on an enabled item at runtime

    for I := 0 to FItems.Count - 1 do
    begin
      if PtInRect( FItems[ I ].HotRect, Point( X, Y ) ) then
      begin
        FItems[ I ].ClickingCaption := True;
        Break;
      end;
    end;
  end;
end;


procedure TRzPathBar.MouseMove( Shift: TShiftState; X, Y: Integer );
var
  CapState: TRzCaptionState;
  ItemStates: array of TRzCaptionState;
  ItemStatesChanged: Boolean;
  I, OverItem: Integer;
begin
  inherited;

  CapState := csNormal;
  ItemStatesChanged := False;

  if ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    OverItem := -1;

    // Check to see if mouse is over one of the items
    SetLength( ItemStates, FItems.Count );
    for I := 0 to FItems.Count - 1 do
      ItemStates[ I ] := csNormal;

    for I := 0 to FItems.Count - 2 do
    begin
      if ( Assigned( FItems[ I ].OnClick ) or ( FItems[ I ].Action <> nil ) ) and
         PtInRect( FItems[ I ].HotRect, Point( X, Y ) ) then
      begin
        if FItems[ I ].ClickingCaption then
          ItemStates[ I ] := csDown
        else
          ItemStates[ I ] := csHot;
        OverItem := I;
      end;
    end;

    // Did any of the ItemStates change?

    for I := 0 to FItems.Count - 2 do
    begin
      if ItemStates[ I ] <> FItems[ I ].CaptionState then
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


procedure TRzPathBar.DisplayItemPopupMenu( Index, X, Y: Integer );
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


procedure TRzPathBar.Click;
var
  DoItemClick: Boolean;
  I: Integer;
  P: TPoint;
begin
  DoItemClick := False;
  if FItems.Count > 0 then
  begin
    P := CursorPosition;
    // Check to see if user clicked one of the items - last item can't be clicked
    for I := 0 to FItems.Count - 2 do
    begin
      if FItems[ I ].ClickingCaption then
      begin
        FItems[ I ].ClickingCaption := False;
        DoItemClick := PtInRect( FItems[ I ].HotRect, P );

        FItems[ I ].CaptionState := csHot;

        if DoItemClick and not ( FItems[ I ].CaptionState = csDown ) then
          Repaint;

        if DoItemClick then
        begin
          FItems[ I ].Click;
        end;
        Break;
      end
    end;
  end;

  if not DoItemClick then
    inherited;
end; {= TRzPathBar.Click =}


procedure TRzPathBar.DoContextPopup( MousePos: TPoint; var Handled: Boolean );
var
  DoItemClick: Boolean;
  I: Integer;
  P: TPoint;
begin
  DoItemClick := False;
  if FItems.Count > 0 then
  begin
    P := ScreenToClient( MousePos );
    // Check to see if user clicked one of the items
    for I := 0 to FItems.Count - 1 do
    begin
      if FItems[ I ].ClickingCaption then
      begin
        FItems[ I ].ClickingCaption := False;
        DoItemClick := PtInRect( FItems[ I ].HotRect, MousePos );

        FItems[ I ].CaptionState := csNormal;

        if DoItemClick and not ( FItems[ I ].CaptionState = csDown ) then
          Repaint;

        if DoItemClick then
        begin
          P := ClientToScreen( MousePos );
          DisplayItemPopupMenu( I, P.X, P.Y );
        end;
        Break;
      end
    end;
  end;

  Handled := DoItemClick;
  inherited;
end;


procedure TRzPathBar.CMMouseEnter( var Msg: TMessage );
begin
  inherited;
  Refresh;
end;


procedure TRzPathBar.CMMouseLeave( var Msg: TMessage );
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


procedure TRzPathBar.MouseOverItem( Item: TRzPathItem );
begin
  if Assigned( FOnMouseOverItem ) then
    FOnMouseOverItem( Self, Item );
end;


procedure TRzPathBar.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzPathBar.CMHintShow( var Msg: TMessage );
var
  I: Integer;
begin
  inherited;

  // Scan through all items and see if any of them are csHot (i.e. the mouse is over them).
  // If so, then set HintInfo^.HintStr and CursorRect
  Msg.Result := 1;

  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[ I ].CaptionState = csHot then
    begin
      TCMHintShow( Msg ).HintInfo^.HintStr := GetShortHint( FItems[ I ].Hint );
      TCMHintShow( Msg ).HintInfo^.CursorRect := FItems[ I ].FHotRect;
      Msg.Result := 0;
      Break;
    end;
  end;

  if ( Msg.Result = 1 ) and ( FItems.Count > 0 ) and PtInRect( FItems[ FItems.Count - 1 ].HotRect, CursorPosition ) then
  begin
    TCMHintShow( Msg ).HintInfo^.HintStr := GetShortHint( FItems[ FItems.Count - 1 ].Hint );
    TCMHintShow( Msg ).HintInfo^.CursorRect := FItems[ FItems.Count - 1 ].FHotRect;
    Msg.Result := 0;
  end;

end; {= TRzPathBar.CMHintShow =}


procedure TRzPathBar.WMSetCursor( var Msg: TWMSetCursor );
var
  I: Integer;
  OverItem: Boolean;
begin
  // If mouse is over an item, then use custom cursor
  if ( FItems.Count > 0 ) and not ( csDesigning in ComponentState ) then
  begin
    OverItem := False;
    for I := 0 to FItems.Count - 2 do
    begin
      if Assigned( FItems[ I ].OnClick ) and PtInRect( FItems[ I ].HotRect, CursorPosition ) then
      begin
        OverItem := True;
        Break;
      end;
    end;
    if OverItem then
      SetCursor( FItemCursor )
    else
      inherited;
  end
  else
    inherited;
end; {= TRzPathBar.WMSetCursor =}


{&RUIF}
end.

