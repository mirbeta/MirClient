{===============================================================================
  RzDBNav Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBNavigator
    Enhanced DBNavigator that provides new button images and also allows the 
    images to be replaced from images in an Image List.


  Modification History
  ------------------------------------------------------------------------------
  6.1.9  (21 Jun 2014)
    * Fixed issue in TRzDBNavigator where custom VCL Style images were overlaid
      on top of custom navigator images.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzDBNavigator to fully support VCL Styles
      introduced in RAD Studio XE2. Including displaying the navigation images
      defined in the VCL Style.
  ------------------------------------------------------------------------------
  5.1.2  (11 Jun 2009)
    * Added new EnablementStyle property to TRzDBNavigator, which affects the
      way certain buttons of the navigator are enabled and disabled. The default
      value for this property is nesTraditional, which is the standard style in
      which the browsing buttons (First, Prior, Next, Last) remain enabled when
      a dataset is put into Insert or Edit mode.  The new nesNoBrowseOnEdit
      style can be used to disable all buttons except for the Post and Cancel
      buttons when a dataset is in Insert or Edit mode.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Made the DataChanged and EditingChanged methods of TRzDBNavigator virtual.
  ------------------------------------------------------------------------------
  4.2    (29 May 2007)
    * Completely redesigned the TRzDBNavigator. Instead of descending from
      TDBNavigator, the TRzDBNavigator now descends from TRzCustomPanel and
      completely implements the functionality of the data-aware navigator strip.
      As such, the TRzDBNavigator inherits the advanced display features of
      TRzPanel (e.g. VisualStyle, Gradients, Transparency, etc.) In addition,
      the navigator buttons are instances of TRzToolButton components and thus
      fully support XP/Vista themes as well as gradient visual styles. The new
      ButtonSelectionColorStart, ButtonSelectionColorStop,
      ButtonSelectionFrameColor, ButtonGradientColorStyle, and ButtonVisualStyle
      properties allow controlling the appearance of the buttons.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Free FImageIndexes object in destructor of TRzDBNavigator.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzDBNav;

interface

uses
  {&RF}
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Messages,
  SysUtils,
  Windows,
  Graphics,
  Controls,
  ExtCtrls,
  DB,
  ImgList,
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Classes,
  RzCommon,
  RzPanel,
  RzButton;


type
  {==================================================}
  {== TRzDBNavigatorImageIndexes Class Declaration ==}
  {==================================================}

  TRzDBNavigator = class;

  TRzDBNavigatorImageIndexes = class( TPersistent )
  private
    FNavigator: TRzDBNavigator;

    FFirst: TImageIndex;
    FFirstDisabled: TImageIndex;
    FPrevious: TImageIndex;
    FPreviousDisabled: TImageIndex;
    FNext: TImageIndex;
    FNextDisabled: TImageIndex;
    FLast: TImageIndex;
    FLastDisabled: TImageIndex;
    FInsert: TImageIndex;
    FInsertDisabled: TImageIndex;
    FDelete: TImageIndex;
    FDeleteDisabled: TImageIndex;
    FEdit: TImageIndex;
    FEditDisabled: TImageIndex;
    FPost: TImageIndex;
    FPostDisabled: TImageIndex;
    FCancel: TImageIndex;
    FCancelDisabled: TImageIndex;
    FRefresh: TImageIndex;
    FRefreshDisabled: TImageIndex;
  protected
    function GetImageIndex( Index: Integer ): TImageIndex; virtual;
    procedure SetImageIndex( Index: Integer; Value: TImageIndex ); virtual;
  public
    constructor Create( Navigator: TRzDBNavigator );

    property Navigator: TRzDBNavigator
      read FNavigator;
  published
    property First: TImageIndex
      index 0
      read GetImageIndex
      write SetImageIndex
      default -1;

    property FirstDisabled: TImageIndex
      index 1
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Previous: TImageIndex
      index 2
      read GetImageIndex
      write SetImageIndex
      default -1;

    property PreviousDisabled: TImageIndex
      index 3
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Next: TImageIndex
      index 4
      read GetImageIndex
      write SetImageIndex
      default -1;

    property NextDisabled: TImageIndex
      index 5
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Last: TImageIndex
      index 6
      read GetImageIndex
      write SetImageIndex
      default -1;

    property LastDisabled: TImageIndex
      index 7
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Insert: TImageIndex
      index 8
      read GetImageIndex
      write SetImageIndex
      default -1;

    property InsertDisabled: TImageIndex
      index 9
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Delete: TImageIndex
      index 10
      read GetImageIndex
      write SetImageIndex
      default -1;

    property DeleteDisabled: TImageIndex
      index 11
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Edit: TImageIndex
      index 12
      read GetImageIndex
      write SetImageIndex
      default -1;

    property EditDisabled: TImageIndex
      index 13
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Post: TImageIndex
      index 14
      read GetImageIndex
      write SetImageIndex
      default -1;

    property PostDisabled: TImageIndex
      index 15
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Cancel: TImageIndex
      index 16
      read GetImageIndex
      write SetImageIndex
      default -1;

    property CancelDisabled: TImageIndex
      index 17
      read GetImageIndex
      write SetImageIndex
      default -1;

    property Refresh: TImageIndex
      index 18
      read GetImageIndex
      write SetImageIndex
      default -1;

    property RefreshDisabled: TImageIndex
      index 19
      read GetImageIndex
      write SetImageIndex
      default -1;
  end;


  {======================================}
  {== TRzDBNavigator Class Declaration ==}
  {======================================}

  TRzNavigatorToolButton = class;
  TRzNavigatorDataLink = class;

  TRzNavigatorButton = ( nbFirst, nbPrior, nbNext, nbLast,
                         nbInsert, nbDelete, nbEdit,
                         nbPost, nbCancel, nbRefresh );
  TRzNavigatorButtons = set of TRzNavigatorButton;
  TRzNavigatorButtonStyle = set of ( nbsAllowTimer, nbsFocusRect );

  TRzNavigatorButtonClickEvent = procedure( Sender: TObject; Button: TRzNavigatorButton ) of object;

  TRzNavigatorEnablementStyle = ( nesTraditional, nesNoBrowseOnEdit );

  TRzDBNavigator = class( TRzCustomPanel )
  private
    FAboutInfo: TRzAboutInfo;
    FDataLink: TRzNavigatorDataLink;
    FVisibleButtons: TRzNavigatorButtons;
    FHints: TStrings;
    FDefHints: TStrings;
    ButtonWidth: Integer;
    MinBtnSize: TPoint;
    FOnNavClick: TRzNavigatorButtonClickEvent;
    FBeforeAction: TRzNavigatorButtonClickEvent;
    FocusedButton: TRzNavigatorButton;
    FConfirmDelete: Boolean;
    FFlat: Boolean;
    FEnablementStyle: TRzNavigatorEnablementStyle;

    FInternalImages: TCustomImageList;
    FImageIndexes: TRzDBNavigatorImageIndexes;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;

    // Internal Event Handlers
    procedure BtnMouseDown( Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer );
    procedure ClickHandler( Sender: TObject );
    procedure HintsChanged( Sender: TObject );
    procedure ImagesChange( Sender: TObject );

    // Message Handling Methods
    procedure WMSize( var Msg: TWMSize );  message wm_Size;
    procedure WMSetFocus( var Msg: TWMSetFocus ); message wm_SetFocus;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMWindowPosChanging( var Msg: TWMWindowPosChanging ); message wm_WindowPosChanging;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
  protected
    Buttons: array[ TRzNavigatorButton ] of TRzNavigatorToolButton;

    procedure LoadImages;
    procedure InitButtons;
    procedure InitHints;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure DataChanged; virtual;
    procedure EditingChanged; virtual;
    procedure ActiveChanged;
    procedure UpdateImages( NavBtn: TRzNavigatorButton );

    // Event Dispatch Methods
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure CalcMinSize( var W, H: Integer );

    // Property Access Methods
    function GetButtonGradientColorStyle: TRzGradientColorStyle; virtual;
    procedure SetButtonGradientColorStyle( Value: TRzGradientColorStyle ); virtual;
    function GetButtonSelectionColorStart: TColor; virtual;
    procedure SetButtonSelectionColorStart( Value: TColor ); virtual;
    function GetButtonSelectionColorStop: TColor; virtual;
    procedure SetButtonSelectionColorStop( Value: TColor ); virtual;
    function GetButtonSelectionFrameColor: TColor; virtual;
    procedure SetButtonSelectionFrameColor( Value: TColor ); virtual;
    function GetButtonVisualStyle: TRzVisualStyle; virtual;
    procedure SetButtonVisualStyle( Value: TRzVisualStyle ); virtual;

    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    procedure SetFlat( Value: Boolean ); virtual;
    function GetHints: TStrings; virtual;
    procedure SetHints( Value: TStrings ); virtual;
    procedure SetImageIndexes( Value: TRzDBNavigatorImageIndexes ); virtual;
    procedure SetImages( Value: TCustomImageList ); virtual;
    procedure SetSize( var W, H: Integer ); virtual;
    procedure SetVisible( Value: TRzNavigatorButtons ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;
    procedure BtnClick( NavBtn: TRzNavigatorButton ); virtual;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ButtonGradientColorStyle: TRzGradientColorStyle
      read GetButtonGradientColorStyle
      write SetButtonGradientColorStyle
      default gcsSystem;

    property ButtonSelectionColorStart: TColor
      read GetButtonSelectionColorStart
      write SetButtonSelectionColorStart
      default clBtnFace;

    property ButtonSelectionColorStop: TColor
      read GetButtonSelectionColorStop
      write SetButtonSelectionColorStop
      default clBtnShadow;

    property ButtonSelectionFrameColor: TColor
      read GetButtonSelectionFrameColor
      write SetButtonSelectionFrameColor
      default cl3DDkShadow;

    property ButtonVisualStyle: TRzVisualStyle
      read GetButtonVisualStyle
      write SetButtonVisualStyle
      default vsWinXP;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    property ConfirmDelete: Boolean
      read FConfirmDelete
      write FConfirmDelete
      default True;

    property EnablementStyle: TRzNavigatorEnablementStyle
      read FEnablementStyle
      write FEnablementStyle
      default nesTraditional;

    property VisibleButtons: TRzNavigatorButtons
      read FVisibleButtons
      write SetVisible
      default [ nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete,
                nbEdit, nbPost, nbCancel, nbRefresh ];

    property Flat: Boolean
      read FFlat
      write SetFlat
      default True;

    property Hints: TStrings
      read GetHints
      write SetHints;

    property ImageIndexes: TRzDBNavigatorImageIndexes
      read FImageIndexes
      write SetImageIndexes;

    property Images: TCustomImageList
      read FImages
      write SetImages;

    property BeforeAction: TRzNavigatorButtonClickEvent
      read FBeforeAction
      write FBeforeAction;

    property OnClick: TRzNavigatorButtonClickEvent
      read FOnNavClick
      write FOnNavClick;

    // Inherited Properties & Events
    property Align;
    property Anchors;
    property BevelWidth;
    property BiDiMode;
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderColor;
    property BorderHighlight;
    property BorderShadow;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property FullRepaint;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property VisualStyle;

    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;


  {==============================================}
  {== TRzNavigatorToolButton Class Declaration ==}
  {==============================================}

  TRzNavigatorToolButton = class( TRzToolButton )
  private
    FNavBtn: TRzNavigatorButton;
    FNavStyle: TRzNavigatorButtonStyle;
    FRepeatTimer: TTimer;
    procedure TimerExpired( Sender: TObject );
  protected
    procedure Paint; override;
    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
  public
    destructor Destroy; override;

    property NavStyle: TRzNavigatorButtonStyle
      read FNavStyle
      write FNavStyle;

    property NavBtn: TRzNavigatorButton
      read FNavBtn
      write FNavBtn;
  end;


  {============================================}
  {== TRzNavigatorDataLink Class Declaration ==}
  {============================================}

  TRzNavigatorDataLink = class( TDataLink )
  private
    FNavigator: TRzDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create( Navigator: TRzDBNavigator );
    destructor Destroy; override;
  end;


resourcestring
  SRzFirstRecord = 'Go to First record';
  SRzPriorRecord = 'Go to Previous record';
  SRzNextRecord = 'Go to Next record';
  SRzLastRecord = 'Go to Last record';
  SRzInsertRecord = 'Insert a new record';
  SRzDeleteRecord = 'Delete selected record';
  SRzEditRecord = 'Edit selected record';
  SRzPostEdit = 'Post Changes';
  SRzCancelEdit = 'Cancel Changes';
  SRzRefreshRecord = 'Refresh record data';
  SRzDeleteRecordQuestion = 'Delete selected record?';


implementation

{$R RzDBNav.res}  // Link in default images for navigator buttons

uses
  {&RAS}
  Themes,
  Dialogs;

const
  InitRepeatPause = 400;  // pause before repeat timer (ms)
  RepeatPause     = 100;  // pause before hint window displays (ms)

  NavBmpNames: array[ TRzNavigatorButton, Boolean ] of PChar =
    ( ( 'RZDBNAV_FIRST',      'RZDBNAV_FIRST_DISABLED' ),
      ( 'RZDBNAV_PREVIOUS',   'RZDBNAV_PREVIOUS_DISABLED' ),
      ( 'RZDBNAV_NEXT',       'RZDBNAV_NEXT_DISABLED' ),
      ( 'RZDBNAV_LAST',       'RZDBNAV_LAST_DISABLED' ),
      ( 'RZDBNAV_INSERT',     'RZDBNAV_INSERT_DISABLED' ),
      ( 'RZDBNAV_DELETE',     'RZDBNAV_DELETE_DISABLED' ),
      ( 'RZDBNAV_EDIT',       'RZDBNAV_EDIT_DISABLED' ),
      ( 'RZDBNAV_POST',       'RZDBNAV_POST_DISABLED' ),
      ( 'RZDBNAV_CANCEL',     'RZDBNAV_CANCEL_DISABLED' ),
      ( 'RZDBNAV_REFRESH',    'RZDBNAV_REFRESH_DISABLED' ) );

var
  NavBtnHintIds: array[ TRzNavigatorButton ] of Pointer =
  ( @SRzFirstRecord, @SRzPriorRecord, @SRzNextRecord, @SRzLastRecord,
    @SRzInsertRecord, @SRzDeleteRecord, @SRzEditRecord,
    @SRzPostEdit, @SRzCancelEdit, @SRzRefreshRecord );



{&RT}
{========================================}
{== TRzDBNavigatorImageIndexes Methods ==}
{========================================}

constructor TRzDBNavigatorImageIndexes.Create( Navigator: TRzDBNavigator );
begin
  inherited Create;

  FNavigator := Navigator;

  FFirst := -1;
  FFirstDisabled := -1;
  FPrevious := -1;
  FPreviousDisabled := -1;
  FNext := -1;
  FNextDisabled := -1;
  FLast := -1;
  FLastDisabled := -1;
  FInsert := -1;
  FInsertDisabled := -1;
  FDelete := -1;
  FDeleteDisabled := -1;
  FEdit := -1;
  FEditDisabled := -1;
  FPost := -1;
  FPostDisabled := -1;
  FCancel := -1;
  FCancelDisabled := -1;
  FRefresh := -1;
  FRefreshDisabled := -1;
end;


function TRzDBNavigatorImageIndexes.GetImageIndex( Index: Integer ): TImageIndex;
begin
  Result := -1;
  case Index of
    0: Result := FFirst;
    1: Result := FFirstDisabled;
    2: Result := FPrevious;
    3: Result := FPreviousDisabled;
    4: Result := FNext;
    5: Result := FNextDisabled;
    6: Result := FLast;
    7: Result := FLastDisabled;
    8: Result := FInsert;
    9: Result := FInsertDisabled;
    10: Result := FDelete;
    11: Result := FDeleteDisabled;
    12: Result := FEdit;
    13: Result := FEditDisabled;
    14: Result := FPost;
    15: Result := FPostDisabled;
    16: Result := FCancel;
    17: Result := FCancelDisabled;
    18: Result := FRefresh;
    19: Result := FRefreshDisabled;
  end;
end;


procedure TRzDBNavigatorImageIndexes.SetImageIndex( Index: Integer; Value: TImageIndex );
begin
  case Index of
    0: FFirst             := Value;
    1: FFirstDisabled     := Value;
    2: FPrevious          := Value;
    3: FPreviousDisabled  := Value;
    4: FNext              := Value;
    5: FNextDisabled      := Value;
    6: FLast              := Value;
    7: FLastDisabled      := Value;
    8: FInsert            := Value;
    9: FInsertDisabled    := Value;
    10: FDelete           := Value;
    11: FDeleteDisabled   := Value;
    12: FEdit             := Value;
    13: FEditDisabled     := Value;
    14: FPost             := Value;
    15: FPostDisabled     := Value;
    16: FCancel           := Value;
    17: FCancelDisabled   := Value;
    18: FRefresh          := Value;
    19: FRefreshDisabled  := Value;
  end;
  FNavigator.UpdateImages( TRzNavigatorButton( Index div 2 ) );
end;



{============================}
{== TRzDBNavigator Methods ==}
{============================}

constructor TRzDBNavigator.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  ControlStyle := ControlStyle - [ csAcceptsControls, csSetCaption ] + [ csOpaque ];

  FDataLink := TRzNavigatorDataLink.Create( Self );

  FEnablementStyle := nesTraditional;

  FVisibleButtons := [ nbFirst, nbPrior, nbNext, nbLast, nbInsert,
                       nbDelete, nbEdit, nbPost, nbCancel, nbRefresh ];
  FHints := TStringList.Create;
  TStringList( FHints ).OnChange := HintsChanged;

  FInternalImages := TImageList.Create( nil );

  FImageIndexes := TRzDBNavigatorImageIndexes.Create( Self );

  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ImagesChange;

  LoadImages;

  InitButtons;
  InitHints;

  BorderOuter := fsNone;
  Width := 241;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := nbFirst;
  FConfirmDelete := True;
  Flat := True;
end;


destructor TRzDBNavigator.Destroy;
begin
  FInternalImages.Free;
  FImagesChangeLink.Free;
  FImageIndexes.Free;
  FDefHints.Free;
  FDataLink.Free;
  FHints.Free;
  FDataLink := nil;
  inherited Destroy;
end;


procedure TRzDBNavigator.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  SetSize( W, H );
  if ( W <> Width ) or ( H <> Height ) then
    inherited SetBounds( Left, Top, W, H );

  ImagesChange( nil );
  InitHints;
  ActiveChanged;
end;


procedure TRzDBNavigator.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited Notification( AComponent, Operation );

  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;

  if ( Operation = opRemove ) and ( AComponent = FImages ) then
    SetImages( nil );
end;


procedure TRzDBNavigator.LoadImages;
var
  B: TRzNavigatorButton;
  Bmp: TBitmap;
begin
  FInternalImages.Clear;

  Bmp := TBitmap.Create;
  try
    if UsingSystemStyle then
    begin
      for B := Low( Buttons ) to High( Buttons ) do
      begin
        Bmp.LoadFromResourceName( HInstance, NavBmpNames[ B, False ] );
        FInternalImages.AddMasked( Bmp, clFuchsia );

        Bmp.LoadFromResourceName( HInstance, NavBmpNames[ B, True ] );
        FInternalImages.AddMasked( Bmp, clFuchsia );
      end;
    end
    else // VCL Styles
    begin
      // When using VCL Styles the images to use come from the style and
      // are drawn on the buttons themselves using StyleServices.DrawElement,
      // which allows the alpha blending to work correctly.
    end;

  finally
    Bmp.Free;
  end;
  {&RCI}
end;


procedure TRzDBNavigator.InitButtons;
var
  B: TRzNavigatorButton;
  Btn: TRzNavigatorToolButton;
  X, ImageIdx: Integer;
begin
  MinBtnSize := Point( 20, 18 );
  X := 0;
  for B := Low( Buttons ) to High( Buttons ) do
  begin
    Btn := TRzNavigatorToolButton.Create( Self );
    Btn.Flat := Flat;
    Btn.NavBtn := B;

    Btn.Images := FInternalImages;
    ImageIdx := 2 * Ord( B );
    Btn.ImageIndex := ImageIdx;
    Btn.DisabledIndex := ImageIdx + 1;

    Btn.Visible := B in FVisibleButtons;
    Btn.Enabled := True;
    Btn.SetBounds( X, 0, MinBtnSize.X, MinBtnSize.Y );

    Btn.Enabled := False;
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;

    Buttons[ B ] := Btn;
    X := X + MinBtnSize.X;
  end;
  Buttons[ nbPrior ].NavStyle := Buttons[ nbPrior ].NavStyle + [ nbsAllowTimer ];
  Buttons[ nbNext ].NavStyle  := Buttons[ nbNext ].NavStyle + [ nbsAllowTimer ];
end;


procedure TRzDBNavigator.InitHints;
var
  I: Integer;
  B: TRzNavigatorButton;
begin
  if not Assigned( FDefHints ) then
  begin
    FDefHints := TStringList.Create;
    for B := Low( Buttons ) to High( Buttons ) do
      FDefHints.Add( LoadResString( NavBtnHintIds[ B ] ) );
  end;

  for B := Low( Buttons ) to High( Buttons ) do
    Buttons[ B ].Hint := FDefHints[ Ord( B ) ];

  B := Low( Buttons );
  for I := 0 to ( FHints.Count - 1 ) do
  begin
    if FHints.Strings[ I ] <> '' then
      Buttons[ B ].Hint := FHints.Strings[ I ];

    if B = High( Buttons ) then
      Exit;
    Inc( B );
  end;
end;


procedure TRzDBNavigator.HintsChanged( Sender: TObject );
begin
  InitHints;
end;


procedure TRzDBNavigator.SetFlat( Value: Boolean );
var
  B: TRzNavigatorButton;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].Flat := Value;
  end;
end;



function TRzDBNavigator.GetHints: TStrings;
begin
  if ( csDesigning in ComponentState ) and not ( csWriting in ComponentState ) and
     not ( csReading in ComponentState ) and ( FHints.Count = 0 ) then
    Result := FDefHints
  else
    Result := FHints;
end;


procedure TRzDBNavigator.SetHints( Value: TStrings );
begin
  if Value.Text = FDefHints.Text then
    FHints.Clear
  else
    FHints.Assign( Value );
end;


procedure TRzDBNavigator.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;


procedure TRzDBNavigator.SetVisible( Value: TRzNavigatorButtons );
var
  B: TRzNavigatorButton;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  FVisibleButtons := Value;

  for B := Low( Buttons ) to High( Buttons ) do
    Buttons[ B ].Visible := B in FVisibleButtons;

  SetSize( W, H );

  if ( W <> Width ) or ( H <> Height ) then
    inherited SetBounds( Left, Top, W, H );

  Invalidate;
end;


procedure TRzDBNavigator.CalcMinSize( var W, H: Integer );
var
  Count: Integer;
  B: TRzNavigatorButton;
begin
  if ( csLoading in ComponentState ) then
    Exit;

  if Buttons[ nbFirst ] = nil then
    Exit;

  Count := 0;
  for B := Low( Buttons ) to High( Buttons ) do
  begin
    if Buttons[ B ].Visible then
      Inc( Count );
  end;
  if Count = 0 then
    Inc( Count );

  W := Max( W, Count * MinBtnSize.X );
  H := Max( H, MinBtnSize.Y );

  if Align = alNone then
    W := ( W div Count ) * Count;
end;


procedure TRzDBNavigator.SetSize( var W, H: Integer );
var
  Count: Integer;
  B: TRzNavigatorButton;
  Space, Temp, Remain: Integer;
  X: Integer;
begin
  if ( csLoading in ComponentState ) then
    Exit;

  if Buttons[ nbFirst ] = nil then
    Exit;

  CalcMinSize( W, H );

  Count := 0;
  for B := Low( Buttons ) to High( Buttons ) do
  begin
    if Buttons[ B ].Visible then
      Inc( Count );
  end;
  if Count = 0 then
    Inc( Count );

  ButtonWidth := W div Count;
  Temp := Count * ButtonWidth;
  if Align = alNone then
    W := Temp;

  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for B := Low( Buttons ) to High( Buttons ) do
  begin
    if Buttons[ B ].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec( Temp, Remain );
        if Temp < 0 then
        begin
          Inc( Temp, Count );
          Space := 1;
        end;
      end;
      Buttons[ B ].SetBounds( X, 0, ButtonWidth + Space, Height );
      Inc( X, ButtonWidth + Space );
    end
    else
      Buttons[ B ].SetBounds( Width + 1, 0, ButtonWidth, Height );
  end;
end; {= TRzDBNavigator.SetSize =}


procedure TRzDBNavigator.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then
    SetSize( W, H );
  inherited SetBounds( ALeft, ATop, W, H );
end;


procedure TRzDBNavigator.WMSize( var Msg: TWMSize );
var
  W, H: Integer;
begin
  inherited;
  W := Width;
  H := Height;
  SetSize( W, H );
end;


procedure TRzDBNavigator.WMWindowPosChanging( var Msg: TWMWindowPosChanging );
begin
  inherited;
  if ( SWP_NOSIZE and Msg.WindowPos.Flags ) = 0 then
    CalcMinSize( Msg.WindowPos.cx, Msg.WindowPos.cy );
end;


procedure TRzDBNavigator.ClickHandler( Sender: TObject );
begin
  BtnClick( TRzNavigatorToolButton( Sender ).NavBtn );
end;


procedure TRzDBNavigator.BtnMouseDown( Sender: TObject; Button: TMouseButton;
                                       Shift: TShiftState; X, Y: Integer );
var
  OldFocus: TRzNavigatorButton;
begin
  OldFocus := FocusedButton;
  FocusedButton := TRzNavigatorToolButton( Sender ).NavBtn;
  if TabStop and ( GetFocus <> Handle ) and CanFocus then
  begin
    SetFocus;
    if GetFocus <> Handle then
      Exit;
  end
  else if TabStop and ( GetFocus = Handle ) and ( OldFocus <> FocusedButton ) then
  begin
    Buttons[ OldFocus ].Invalidate;
    Buttons[ FocusedButton ].Invalidate;
  end;
end;


procedure TRzDBNavigator.BtnClick( NavBtn: TRzNavigatorButton );
begin
  if ( DataSource <> nil ) and ( DataSource.State <> dsInactive ) then
  begin
    if not ( csDesigning in ComponentState ) and Assigned( FBeforeAction ) then
      FBeforeAction( Self, NavBtn );

    case NavBtn of
      nbPrior:
        DataSource.DataSet.Prior;
      nbNext:
        DataSource.DataSet.Next;
      nbFirst:
        DataSource.DataSet.First;
      nbLast:
        DataSource.DataSet.Last;
      nbInsert:
        DataSource.DataSet.Insert;
      nbEdit:
        DataSource.DataSet.Edit;
      nbCancel:
        DataSource.DataSet.Cancel;
      nbPost:
        DataSource.DataSet.Post;
      nbRefresh:
        DataSource.DataSet.Refresh;
      nbDelete:
      begin
        if not FConfirmDelete or
          ( MessageDlg( SRzDeleteRecordQuestion, mtConfirmation, mbOKCancel, 0 ) <> idCancel) then
        begin
          DataSource.DataSet.Delete;
        end;
      end;
    end;
  end;
  if not ( csDesigning in ComponentState ) and Assigned( FOnNavClick ) then
    FOnNavClick( Self, NavBtn );
end;


procedure TRzDBNavigator.SetImageIndexes( Value: TRzDBNavigatorImageIndexes );
begin
  {&RV}
  FImageIndexes.Assign( Value );
end;


procedure TRzDBNavigator.UpdateImages( NavBtn: TRzNavigatorButton );
var
  Idx: Integer;
begin
  Idx := Ord( NavBtn ) * 2;

  if ( FImageIndexes.GetImageIndex( Idx ) <> -1 ) and ( FImages <> nil ) then
  begin
    Buttons[ NavBtn ].Images := FImages;
    Buttons[ NavBtn ].ImageIndex := FImageIndexes.GetImageIndex( Idx );
    Buttons[ NavBtn ].DisabledIndex := FImageIndexes.GetImageIndex( Idx + 1 );
  end
  else
  begin
    Buttons[ NavBtn ].Images := FInternalImages;
    Buttons[ NavBtn ].ImageIndex := Idx;
    Buttons[ NavBtn ].DisabledIndex := Idx + 1;
  end;
end;


procedure TRzDBNavigator.SetImages( Value: TCustomImageList );
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


procedure TRzDBNavigator.ImagesChange( Sender: TObject );
var
  B: TRzNavigatorButton;
begin
  for B := Low( TRzNavigatorButton ) to High( TRzNavigatorButton ) do
    UpdateImages( B );
end;


procedure TRzDBNavigator.WMSetFocus( var Msg: TWMSetFocus );
begin
  Buttons[ FocusedButton ].Invalidate;
end;


procedure TRzDBNavigator.WMKillFocus( var Msg: TWMKillFocus );
begin
  Buttons[ FocusedButton ].Invalidate;
end;


procedure TRzDBNavigator.KeyDown( var Key: Word; Shift: TShiftState );
var
  NewFocus: TRzNavigatorButton;
  OldFocus: TRzNavigatorButton;
begin
  OldFocus := FocusedButton;
  case Key of
    vk_Right:
    begin
      if OldFocus < High( Buttons ) then
      begin
        NewFocus := OldFocus;
        repeat
          NewFocus := Succ( NewFocus );
        until ( NewFocus = High( Buttons ) ) or ( Buttons[ NewFocus ].Visible );

        if Buttons[ NewFocus ].Visible then
        begin
          FocusedButton := NewFocus;
          Buttons[ OldFocus ].Invalidate;
          Buttons[ NewFocus ].Invalidate;
        end;
      end;
    end;

    vk_Left:
    begin
      NewFocus := FocusedButton;
      repeat
        if NewFocus > Low( Buttons ) then
          NewFocus := Pred( NewFocus );
      until ( NewFocus = Low( Buttons ) ) or ( Buttons[ NewFocus ].Visible );

      if NewFocus <> FocusedButton then
      begin
        FocusedButton := NewFocus;
        Buttons[ OldFocus ].Invalidate;
        Buttons[ FocusedButton ].Invalidate;
      end;
    end;

    vk_Space:
    begin
      if Buttons[ FocusedButton ].Enabled then
        Buttons[ FocusedButton ].Click;
    end;
  end;
end; {= TRzDBNavigator.KeyDown =}


procedure TRzDBNavigator.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantArrows;
end;


procedure TRzDBNavigator.DataChanged;
var
  EnableUp, EnableDown, EnableDelete: Boolean;
begin
  EnableUp := Enabled and FDataLink.Active and not FDataLink.DataSet.BOF;
  EnableDown := Enabled and FDataLink.Active and not FDataLink.DataSet.EOF;

  if FEnablementStyle = nesNoBrowseOnEdit then
  begin
    Buttons[ nbFirst ].Enabled := EnableUp and not FDataLink.Editing;
    Buttons[ nbPrior ].Enabled := EnableUp and not FDataLink.Editing;
    Buttons[ nbNext ].Enabled := EnableDown and not FDataLink.Editing;
    Buttons[ nbLast ].Enabled := EnableDown and not FDataLink.Editing;
  end
  else // nesTraditional
  begin
    Buttons[ nbFirst ].Enabled := EnableUp;
    Buttons[ nbPrior ].Enabled := EnableUp;
    Buttons[ nbNext ].Enabled := EnableDown;
    Buttons[ nbLast ].Enabled := EnableDown;
  end;

  EnableDelete := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify and
                  not ( FDataLink.DataSet.BOF and FDataLink.DataSet.EOF );

  if FEnablementStyle = nesNoBrowseOnEdit then
    Buttons[ nbDelete ].Enabled := EnableDelete and not FDataLink.Editing
  else
    Buttons[ nbDelete ].Enabled := EnableDelete;
end;


procedure TRzDBNavigator.EditingChanged;
var
  CanModify: Boolean;
begin
  CanModify := Enabled and FDataLink.Active and FDataLink.DataSet.CanModify;

  if FEnablementStyle = nesNoBrowseOnEdit then
    Buttons[ nbInsert ].Enabled := CanModify and not FDataLink.Editing
  else
    Buttons[ nbInsert ].Enabled := CanModify;

  Buttons[ nbEdit ].Enabled := CanModify and not FDataLink.Editing;

  Buttons[ nbPost ].Enabled := CanModify and FDataLink.Editing;
  Buttons[ nbCancel ].Enabled := CanModify and FDataLink.Editing;

  Buttons[ nbRefresh ].Enabled := CanModify;

  if FEnablementStyle = nesNoBrowseOnEdit then
  begin
    Buttons[ nbFirst ].Enabled := not FDataLink.Editing;
    Buttons[ nbPrior ].Enabled := not FDataLink.Editing;
    Buttons[ nbNext ].Enabled := not FDataLink.Editing;
    Buttons[ nbLast ].Enabled := not FDataLink.Editing;
    Buttons[ nbDelete ].Enabled := not FDataLink.Editing;
  end;
end;


procedure TRzDBNavigator.ActiveChanged;
var
  B: TRzNavigatorButton;
begin
  if not ( Enabled and FDataLink.Active ) then
  begin
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].Enabled := False
  end
  else
  begin
    DataChanged;
    EditingChanged;
  end;
end;


procedure TRzDBNavigator.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  if not ( csLoading in ComponentState ) then
    ActiveChanged;
end;


function TRzDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBNavigator.SetDataSource( Value: TDataSource );
begin
  FDataLink.DataSource := Value;
  if not ( csLoading in ComponentState ) then
    ActiveChanged;
  if Value <> nil then
    Value.FreeNotification( Self );
end;


function TRzDBNavigator.GetButtonGradientColorStyle: TRzGradientColorStyle;
begin
  Result := Buttons[ nbFirst ].GradientColorStyle;
end;


procedure TRzDBNavigator.SetButtonGradientColorStyle( Value: TRzGradientColorStyle );
var
  B: TRzNavigatorButton;
begin
  if ButtonGradientColorStyle <> Value then
  begin
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].GradientColorStyle := Value;
  end;
end;


function TRzDBNavigator.GetButtonSelectionColorStart: TColor;
begin
  Result := Buttons[ nbFirst ].SelectionColorStart;
end;


procedure TRzDBNavigator.SetButtonSelectionColorStart( Value: TColor );
var
  B: TRzNavigatorButton;
begin
  if ButtonSelectionColorStart <> Value then
  begin
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].SelectionColorStart := Value;
  end;
end;


function TRzDBNavigator.GetButtonSelectionColorStop: TColor;
begin
  Result := Buttons[ nbFirst ].SelectionColorStop;
end;


procedure TRzDBNavigator.SetButtonSelectionColorStop( Value: TColor );
var
  B: TRzNavigatorButton;
begin
  if ButtonSelectionColorStop <> Value then
  begin
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].SelectionColorStop := Value;
  end;
end;


function TRzDBNavigator.GetButtonSelectionFrameColor: TColor;
begin
  Result := Buttons[ nbFirst ].SelectionFrameColor;
end;


procedure TRzDBNavigator.SetButtonSelectionFrameColor( Value: TColor );
var
  B: TRzNavigatorButton;
begin
  if ButtonSelectionFrameColor <> Value then
  begin
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].SelectionFrameColor := Value;
  end;
end;


function TRzDBNavigator.GetButtonVisualStyle: TRzVisualStyle;
begin
  Result := Buttons[ nbFirst ].VisualStyle;
end;


procedure TRzDBNavigator.SetButtonVisualStyle( Value: TRzVisualStyle );
var
  B: TRzNavigatorButton;
begin
  if ButtonVisualStyle <> Value then
  begin
    for B := Low( Buttons ) to High( Buttons ) do
      Buttons[ B ].VisualStyle := Value;
  end;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzDBNavigator.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  LoadImages;
end;

{$ENDIF}


{====================================}
{== TRzNavigatorToolButton Methods ==}
{====================================}

destructor TRzNavigatorToolButton.Destroy;
begin
  if FRepeatTimer <> nil then
    FRepeatTimer.Free;
  inherited Destroy;
end;

procedure TRzNavigatorToolButton.MouseDown( Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer );
begin
  inherited MouseDown( Button, Shift, X, Y );
  if nbsAllowTimer in FNavStyle then
  begin
    if FRepeatTimer = nil then
      FRepeatTimer := TTimer.Create( Self );

    FRepeatTimer.OnTimer := TimerExpired;
    FRepeatTimer.Interval := InitRepeatPause;
    FRepeatTimer.Enabled := True;
  end;
end;


procedure TRzNavigatorToolButton.MouseUp( Button: TMouseButton; Shift: TShiftState;
                                X, Y: Integer );
begin
  inherited MouseUp( Button, Shift, X, Y );
  if FRepeatTimer <> nil then
    FRepeatTimer.Enabled  := False;
end;


procedure TRzNavigatorToolButton.TimerExpired( Sender: TObject );
begin
  FRepeatTimer.Interval := RepeatPause;
  if ( FState = tbsDown ) and MouseCapture then
  begin
    try
      Click;
    except
      FRepeatTimer.Enabled := False;
      raise;
    end;
  end;
end;


procedure TRzNavigatorToolButton.Paint;
{$IFDEF VCL160_OR_HIGHER}
const
  NormalButtons: array[ TRzNavigatorButton ] of TThemedDataNavButtons =
    ( tdnbFirstNormal, tdnbPriorNormal, tdnbNextNormal, tdnbLastNormal,
      tdnbInsertNormal, tdnbDeleteNormal, tdnbEditNormal, tdnbPostNormal,
      tdnbCancelNormal, tdnbRefreshNormal );
  HotButtons: array[ TRzNavigatorButton ] of TThemedDataNavButtons =
    ( tdnbFirstHot, tdnbPriorHot, tdnbNextHot, tdnbLastHot,
      tdnbInsertHot, tdnbDeleteHot, tdnbEditHot, tdnbPostHot,
      tdnbCancelHot, tdnbRefreshHot );
  DisabledButtons: array[ TRzNavigatorButton ] of TThemedDataNavButtons =
    ( tdnbFirstDisabled, tdnbPriorDisabled, tdnbNextDisabled, tdnbLastDisabled,
      tdnbInsertDisabled, tdnbDeleteDisabled, tdnbEditDisabled, tdnbPostDisabled,
      tdnbCancelDisabled, tdnbRefreshDisabled );
  PressedButtons: array[ TRzNavigatorButton ] of TThemedDataNavButtons =
    ( tdnbFirstPressed, tdnbPriorPressed, tdnbNextPressed, tdnbLastPressed,
      tdnbInsertPressed, tdnbDeletePressed, tdnbEditPressed, tdnbPostPressed,
      tdnbCancelPressed, tdnbRefreshPressed );
{$ENDIF}
var
  R: TRect;
  Idx: Integer;
  {$IFDEF VCL160_OR_HIGHER}
  Element: TThemedDataNavButtons;
  {$ENDIF}
begin
  inherited Paint;

  Idx := Ord( NavBtn ) * 2;

  if not UsingSystemStyle and ( TRzDBNavigator( Parent ).FImageIndexes.GetImageIndex( Idx ) = -1 ) then
  begin
    {$IFDEF VCL160_OR_HIGHER}
    if not Enabled then
      Element := DisabledButtons[NavBtn]
    else if FState in [tbsDown, tbsExclusive] then
      Element := PressedButtons[NavBtn]
    else if MouseOverButton then
      Element := HotButtons[NavBtn]
    else
      Element := NormalButtons[NavBtn];

    R := Bounds(0, 0, Width, Height);
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(Element), R);
    {$ENDIF}
  end;


  if ( GetFocus = Parent.Handle ) and
     ( FNavBtn = TRzDBNavigator( Parent ).FocusedButton ) then
  begin
    R := Bounds( 0, 0, Width, Height );
    InflateRect( R, -3, -3 );
    if FState = tbsDown then
      OffsetRect( R, 1, 1 );
    Canvas.Brush.Style := bsSolid;
    Font.Color := clBtnShadow;
    DrawFocusRect( Canvas.Handle, R );
  end;
end;


{==================================}
{== TRzNavigatorDataLink Methods ==}
{==================================}

constructor TRzNavigatorDataLink.Create( Navigator: TRzDBNavigator );
begin
  inherited Create;
  FNavigator := Navigator;
  VisualControl := True;
end;


destructor TRzNavigatorDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;


procedure TRzNavigatorDataLink.EditingChanged;
begin
  if FNavigator <> nil then
    FNavigator.EditingChanged;
end;

procedure TRzNavigatorDataLink.DataSetChanged;
begin
  if FNavigator <> nil then
    FNavigator.DataChanged;
end;

procedure TRzNavigatorDataLink.ActiveChanged;
begin
  if FNavigator <> nil then
    FNavigator.ActiveChanged;
end;



{&RUIF}
end.
