{===============================================================================
  RzTrkBar Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzTrackBar
    Slider control for selecting integer position value.


  Modification History
  ------------------------------------------------------------------------------
  6.1.4  (29 May 2013)
    * Fixed issue where background of TRzTrackBar would not be painted correctly
      if the parent's DoubleBuffered property was True.
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Added new TrackClickBehavior property to TRzTrackBar. The default value is
      tcbMoveByPage, which means that when the user clicks in the track to the
      left/right (or above/below) the thumb, the thumbs moves in the direction
      of the click by the value specified in the PageSize property. When
      TrackClickBehavior is set to tcbMoveToTick and the user clicks in the
      track, the thumb is moved to the closest tick position where the user
      clicked. When TrackClickBehavior is set to tcbNone, the thumb is not moved
      at all.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzTrackBar to fully support VCL Styles
      introduced in RAD Studio XE2.
    * When XP/Vista/Win7 themes are used, the TRzTrackBar now displays the
      thumb as defined in the selected style. This change fixes the issue where
      the thumb would appear like a Windows XP even on Windows Vista or 7.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * The TRzTrackBar has been updated such that it will hide the focus
      rectangle until the user navigates on the form using the keyboard. The
      control honors the Windows system setting that affects this behavior.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed issue in TRzTrackBar where the thumb would not be displayed when a
      the Min to Max range was very, very large.
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzTrackBar and TRzDBTrackBar controls.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new OnDrawTrack event to TRzTrackBar. This event allows a developer
      to customize the appearance of the track used for the track bar. For
      example, a gradient fill could be used to display the track to indicate
      importance of the selected position. Event handlers are passed the
      the TCanvas to use, and the bounds of the track.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added ThemeAware property to TRzTrackBar.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzTrackBar to
      account for changes introduced in Borland Developer Studio 2006.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Created all new thumbs for the track bar.
    * Added TickColor and TickStep property.
    * Added HighlightColor and HotTrackColor properties.
    * Surfaced ParentColor property.
    * Added Transparent property.
===============================================================================}

{$I RzComps.inc}

unit RzTrkBar;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Messages,
  Windows,
  Classes,
  Graphics,
  Controls,
  Forms,
  Menus,
  Themes,
  StdCtrls,
  ExtCtrls,
  RzCommon;

type
  TBitmapType = ( btThumb, btHot, btDisabled, btMask );
  TThumbStyle = ( tsBox, tsCustom, tsMixer , tsPointer, tsFlat, tsXPPointer, tsXPBox );
  TTickStyle = ( tkStandard, tkOwnerDraw );
  TPointerType = ( ptStandard, ptWin95 );
  TTrackClickBehavior = ( tcbNone, tcbMoveByPage, tcbMoveToTick );

  TRzTrackBar = class;                               { Forward class reference }

  TDrawTickEvent = procedure ( TrackBar: TRzTrackBar; Canvas: TCanvas;
                               Location: TPoint; Index: Integer ) of object;

  TRzDrawTrackEvent = procedure ( TrackBar: TRzTrackBar; Canvas: TCanvas;
                                  Bounds: TRect ) of object;

  TRzTrackBar = class( TCustomControl )
  private
    FBorderWidth: Integer;
    FMax: Integer;
    FMin: Integer;
    FOrientation: TOrientation;
    FPageSize: Word;
    FPosition: Integer;
    FTickStyle: TTickStyle;
    FTickColor: TColor;
    FTickStep: TPositiveInteger;
    FHighlightColor: TColor;
    FHotTrackColor: TColor;
    FHotTrackColorType: TRzHotTrackColorType;
    FShowTicks: Boolean;
    FSliding: Boolean;
    FPaging: Boolean;
    FPagingDirInc: Boolean;
    FRepeatTimer: TTimer;
    FLastX: Integer;
    FLastY: Integer;
    FTabOnEnter: Boolean;

    FThumbCenterOffsetX: Integer;
    FThumbCenterOffsetY: Integer;
    FThumbHeight: Integer;
    FThumbRct: TRect;
    FThumbStyle: TThumbStyle;
    FThumbWidth: Integer;
    FHalfWidth: Integer;
    FPointerType: TPointerType;

    FTrackColor: TColor;                               { Attributes for track }
    FTrackOffset: Word;
    FTrackRct: TRect;
    FTrackFrame: TFrameStyle;
    FTrackFrameColor: TColor;
    FTrackWidth: Word;
    FShowFocusRect: Boolean;
    FTrackClickBehavior: TTrackClickBehavior;

    FThumbBmp: TBitmap;
    FHotThumbBmp: TBitmap;
    FMaskBmp: TBitmap;
    FDisabledBmp: TBitmap;
    FBackgroundBmp: TBitmap;
    FUpdateBg: Boolean;
    FCustomThumb: TBitmap;
    FUseHotThumb: Boolean;
    FTransparent: Boolean;
    FThemeAware: Boolean;

    FHorzCursor: HCursor;
    FVertCursor: HCursor;

    FOnChange: TNotifyEvent;                                  { Custom events }
    FOnChanging: TPositionChangingEvent;
    FOnDrawTrack: TRzDrawTrackEvent;
    FOnDrawTick: TDrawTickEvent;

    { Internal Event Handlers }
    procedure TimerExpired( Sender: TObject );
    procedure CustomThumbChanged( Sender: TObject );

    { Message Handling Methods }
    procedure WMEraseBkgnd( var Msg: TWMEraseBkgnd ); message wm_EraseBkgnd;
    procedure WMGetDlgCode( var Msg: TWMGetDlgCode ); message wm_GetDlgCode;
    procedure WMSize( var Msg: TWMSize); message wm_Size;
    procedure WMSetCursor( var Msg: TWMSetCursor ); message wm_SetCursor;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message cm_DesignHitTest;
    procedure WMSetFocus( var Msg: TWMSetFocus ); message wm_SetFocus;
    procedure WMKillFocus( var Msg: TWMKillFocus ); message wm_KillFocus;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
  protected
    FAboutInfo: TRzAboutInfo;

    procedure CreateWnd; override;
    procedure LoadThumbBitmaps;

    function ShowFocus: Boolean;
    function CursorPosition: TPoint;
    function UseThemes: Boolean;
    function GetThumbDetails: TThemedElementDetails;

    procedure DrawTrack( ACanvas: TCanvas ); virtual;
    procedure DrawTicks( ACanvas: TCanvas ); virtual;
    procedure DrawThumb( ACanvas: TCanvas ); virtual;
    procedure Paint; override;
    procedure UpdateBackground;
    procedure MoveThumbToPoint( X, Y: Integer );

    { Event Dispatch Methods }
    procedure Change; dynamic;
    function CanChange( NewPos: Integer ): Boolean; dynamic;
    function CanInternalChange( NewPos: Integer ): Boolean; virtual;

    procedure DoDrawTrack( Canvas: TCanvas; Bounds: TRect ); dynamic;
    procedure DrawTick( Canvas: TCanvas; Location: TPoint; Index: Integer ); dynamic;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress( var Key: Char ); override;

    procedure MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseMove( Shift: TShiftState; X, Y: Integer ); override;
    procedure MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); override;

    { Property Access Methods }
    procedure SetCustomThumb( Value: TBitmap ); virtual;
    procedure SetHighlightColor( Value: TColor ); virtual;
    procedure SetHotTrackColor( Value: TColor ); virtual;
    procedure SetHotTrackColorType( Value: TRzHotTrackColorType ); virtual;
    procedure SetMax( Value: Integer ); virtual;
    procedure SetMin( Value: Integer ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
    procedure SetPointerType( Value: TPointerType ); virtual;
    procedure SetPosition( Value: Integer ); virtual;
    procedure SetShowTicks( Value: Boolean ); virtual;
    procedure SetThumbStyle( Value: TThumbStyle ); virtual;
    procedure SetTickColor( Value: TColor ); virtual;
    procedure SetTickStep( Value: TPositiveInteger ); virtual;
    procedure SetTickStyle( Value: TTickStyle ); virtual;
    procedure SetTrackColor( Value: TColor ); virtual;
    procedure SetTrackOffset( Value: Word ); virtual;
    procedure SetTrackFrame( Value: TFrameStyle ); virtual;
    procedure SetTrackFrameColor( Value: TColor ); virtual;
    procedure SetTrackWidth( Value: Word ); virtual;
    procedure SetTransparent( Value: Boolean ); virtual;
    procedure SetThemeAware( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property About: TRzAboutInfo               { Read-Only Published Property }
      read FAboutInfo
      write FAboutInfo
      stored False;

    property CustomThumb: TBitmap
      read FCustomThumb
      write SetCustomThumb;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property HotTrackColor: TColor
      read FHotTrackColor
      write SetHotTrackColor
      default xpHotTrackColor;

    property HotTrackColorType: TRzHotTrackColorType
      read FHotTrackColorType
      write SetHotTrackColorType
      default htctActual;

    property Max: Integer
      read FMax
      write SetMax
      default 10;

    property Min: Integer
      read FMin
      write SetMin
      default 0;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    property PageSize: Word
      read FPageSize
      write FPageSize
      default 1;

    property PointerType: TPointerType
      read FPointerType
      write SetPointerType
      default ptStandard;

    property Position: Integer
      read FPosition
      write SetPosition;

    property ShowFocusRect: Boolean
      read FShowFocusRect
      write FShowFocusRect
      default True;

    property ShowTicks: Boolean
      read FShowTicks
      write SetShowTicks
      default True;

    property TabOnEnter: Boolean
      read FTabOnEnter
      write FTabOnEnter
      default False;

    property ThumbStyle: TThumbStyle
      read FThumbStyle
      write SetThumbStyle
      default tsPointer;

    property TickColor: TColor
      read FTickColor
      write SetTickColor
      default clBtnShadow;

    property TickStep: TPositiveInteger
      read FTickStep
      write SetTickStep
      default 1;

    property TickStyle: TTickStyle
      read FTickStyle
      write SetTickStyle
      default tkStandard;

    property TrackClickBehavior: TTrackClickBehavior
      read FTrackClickBehavior
      write FTrackClickBehavior
      default tcbMoveByPage;

    property TrackColor: TColor
      read FTrackColor
      write SetTrackColor
      default cl3DLight;

    property TrackOffset: Word
      read FTrackOffset
      write SetTrackOffset
      default 20;

    property TrackFrame: TFrameStyle
      read FTrackFrame
      write SetTrackFrame
      default fsStatus;

    property TrackFrameColor: TColor
      read FTrackFrameColor
      write SetTrackFrameColor
      default clBtnShadow;

    property TrackWidth: Word
      read FTrackWidth
      write SetTrackWidth
      default 8;

    property Transparent: Boolean
      read FTransparent
      write SetTransparent
      default False;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    property OnChanging: TPositionChangingEvent
      read FOnChanging
      write FOnChanging;

    property OnDrawTrack: TRzDrawTrackEvent
      read FOnDrawTrack
      write FOnDrawTrack;

    property OnDrawTick: TDrawTickEvent
      read FOnDrawTick
      write FOnDrawTick;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Height default 35;
    property HelpContext;
    property Hint;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;
    property Width default 200;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
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

// Link in bitmaps for Thumbs
{$R RzTrkBar.res}

uses
  {&RAS}
  SysUtils,
  RzGrafx;

{&RT}
{=========================}
{== TRzTrackBar Methods ==}
{=========================}

constructor TRzTrackBar.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
  Width := 200;
  Height := 35;
  FOrientation := orHorizontal;
  FTrackColor := cl3DLight;
  FTrackClickBehavior := tcbMoveByPage;
  FTrackOffset := 20;
  FTrackFrame := fsStatus;
  FTrackFrameColor := clBtnShadow;
  FTrackWidth := 8;
  FMin := 0;
  FMax := 10;
  FPosition := 0;
  FBorderWidth := 4;
  FPageSize := 1;
  TabStop := True;
  FShowTicks := True;
  FSliding := False;
  FPaging := False;
  FPointerType := ptStandard;
  FTickColor := clBtnShadow;
  FTickStep := 1;
  FHighlightColor := clHighlight;
  FHotTrackColor := xpHotTrackColor;
  FHotTrackColorType := htctActual;

  FRepeatTimer := TTimer.Create( Self );
  FRepeatTimer.OnTimer := TimerExpired;
  FRepeatTimer.Interval := 200;
  FRepeatTimer.Enabled := False;

  // Create internal bitmap objects
  FThumbBmp := TBitmap.Create;
  FHotThumbBmp := TBitmap.Create;
  FMaskBmp := TBitmap.Create;
  FDisabledBmp := TBitmap.Create;
  FBackgroundBmp := TBitmap.Create;
  FUpdateBg := True;
  FCustomThumb := TBitmap.Create;
  FCustomThumb.OnChange := CustomThumbChanged;
  FThumbStyle := tsPointer;
  FThemeAware := True;

  LoadThumbBitmaps;
  FShowFocusRect := True;
  FTabOnEnter := False;

  FHorzCursor := LoadCursor( HInstance, 'RZTRKBAR_H_CURSOR' );
  FVertCursor := LoadCursor( HInstance, 'RZTRKBAR_V_CURSOR' );
  {&RV}
end;


procedure TRzTrackBar.CreateWnd;
begin
  inherited;
  if RunningAtLeast( win2000 ) then
    Perform( wm_ChangeUIState, MakeWParam( UIS_INITIALIZE, UISF_HIDEACCEL or UISF_HIDEFOCUS ), 0 );
end;


destructor TRzTrackBar.Destroy;
begin
  FThumbBmp.Free;
  FHotThumbBmp.Free;
  FMaskBmp.Free;
  FDisabledBmp.Free;
  FBackgroundBmp.Free;
  FCustomThumb.Free;

  DestroyCursor( FHorzCursor );
  DestroyCursor( FVertCursor );

  FRepeatTimer.Free;
  inherited;
end;


procedure TRzTrackBar.CustomThumbChanged( Sender: TObject );
begin
  FThumbStyle := tsCustom;
  LoadThumbBitmaps;
  FUpdateBg := True;
  Invalidate;
end;


procedure TRzTrackBar.SetCustomThumb( Value: TBitmap );
begin
  FCustomThumb.Assign( Value );
end;


procedure TRzTrackBar.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetHotTrackColor( Value: TColor );
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetHotTrackColorType( Value: TRzHotTrackColorType );
begin
  if FHotTrackColorType <> Value then
  begin
    FHotTrackColorType := Value;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetMax( Value: Integer );
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMax <= FMin then
      FMax := FMin + 1;
    if FPosition > FMax then
      Position := FMax;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetMin( Value: Integer );
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMin >= FMax then
      FMin := FMax - 1;
    if FPosition < FMin then
      Position := FMin;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetOrientation( Value: TOrientation );
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetPointerType( Value: TPointerType );
begin
  if FPointerType <> Value then
  begin
    FPointerType := Value;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetPosition( Value: Integer );
begin
  if FPosition <> Value then
  begin
    { Range Checking }
    if Value < FMin then
      Value := FMin
    else if Value > FMax then
      Value := FMax;

    if CanChange( Value ) then
    begin
      FPosition := Value;

      if not FUpdateBg then
        DrawThumb( Canvas );
      Change;

      UpdateObjectInspector( Self );
    end;
  end;
end; {= TRzTrackBar.SetPosition =}


procedure TRzTrackBar.SetShowTicks( Value: Boolean );
begin
  if FShowTicks <> Value then
  begin
    FShowTicks := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetThumbStyle( Value: TThumbStyle );
begin
  if FThumbStyle <> Value then
  begin
    FThumbStyle := Value;
    if not ( csLoading in ComponentState ) then
    begin
      if ( FThumbStyle = tsMixer ) or ( FThumbStyle = tsFlat ) then
        FTrackWidth := 6
      else
        FTrackWidth := 8;
    end;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTickColor( Value: TColor );
begin
  if FTickColor <> Value then
  begin
    FTickColor := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTickStep( Value: TPositiveInteger );
begin
  if FTickStep <> Value then
  begin
    FTickStep := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTickStyle( Value: TTickStyle );
begin
  if FTickStyle <> Value then
  begin
    FTickStyle := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTrackColor( Value: TColor );
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTrackOffset( Value: Word );
begin
  if FTrackOffset <> Value then
  begin
    FTrackOffset := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTrackFrame( Value: TFrameStyle );
begin
  if FTrackFrame <> Value then
  begin
    FTrackFrame := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTrackFrameColor( Value: TColor );
begin
  if FTrackFrameColor <> Value then
  begin
    FTrackFrameColor := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTrackWidth( Value: Word );
begin
  if FTrackWidth <> Value then
  begin
    FTrackWidth := Value;
    if FTrackWidth < 4 then
      FTrackWidth := 4;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetTransparent( Value: Boolean );
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    FUpdateBg := True;
    Invalidate;
  end;
end;


procedure TRzTrackBar.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    LoadThumbBitmaps;
    FUpdateBg := True;
    Invalidate;
  end;
end;


function TRzTrackBar.UseThemes: Boolean;
begin
  Result := FThemeAware and ActiveStyleServicesEnabled and
            ( FThumbStyle in [ tsBox, tsXPBox, tsPointer, tsXPPointer ] );
end;


function TRzTrackBar.GetThumbDetails: TThemedElementDetails;
begin
  if FThumbStyle in [ tsBox, tsXPBox ] then
  begin
    if FOrientation = orHorizontal then
    begin
      if not Enabled then
        Result := ActiveStyleServices.GetElementDetails( ttbThumbDisabled )
      else if FUseHotThumb and not ( csDesigning in ComponentState ) then
        Result := ActiveStyleServices.GetElementDetails( ttbThumbHot )
      else
        Result := ActiveStyleServices.GetElementDetails( ttbThumbNormal );
    end
    else // Vertical
    begin
      if not Enabled then
        Result := ActiveStyleServices.GetElementDetails( ttbThumbVertDisabled )
      else if FUseHotThumb and not ( csDesigning in ComponentState ) then
        Result := ActiveStyleServices.GetElementDetails( ttbThumbVertHot )
      else
        Result := ActiveStyleServices.GetElementDetails( ttbThumbVertNormal );
    end;
  end
  else // Pointer
  begin

    if FOrientation = orHorizontal then
    begin
      if FPointerType = ptStandard then
      begin
        if not Enabled then
          Result := ActiveStyleServices.GetElementDetails( ttbThumbTopDisabled )
        else if FUseHotThumb and not ( csDesigning in ComponentState ) then
          Result := ActiveStyleServices.GetElementDetails( ttbThumbTopHot )
        else
          Result := ActiveStyleServices.GetElementDetails( ttbThumbTopNormal );
      end
      else // Win95
      begin
        if not Enabled then
          Result := ActiveStyleServices.GetElementDetails( ttbThumbBottomDisabled )
        else if FUseHotThumb and not ( csDesigning in ComponentState ) then
          Result := ActiveStyleServices.GetElementDetails( ttbThumbBottomHot )
        else
          Result := ActiveStyleServices.GetElementDetails( ttbThumbBottomNormal );
      end;
    end
    else // Vertical
    begin
      if not Enabled then
        Result := ActiveStyleServices.GetElementDetails( ttbThumbLeftDisabled )
      else if FUseHotThumb and not ( csDesigning in ComponentState ) then
        Result := ActiveStyleServices.GetElementDetails( ttbThumbLeftHot )
      else
        Result := ActiveStyleServices.GetElementDetails( ttbThumbLeftNormal );
    end;
  end;
end;


// Array Constants hold all bitmap resource names for easy access

const
  PointerBmpNames: array[ TOrientation, TBitmapType ] of PChar =
    ( ( 'RZTRKBAR_H_PTR', 'RZTRKBAR_H_PTR_HOT', 'RZTRKBAR_H_PTR_DISABLED', 'RZTRKBAR_H_PTR_MASK' ),
      ( 'RZTRKBAR_V_PTR', 'RZTRKBAR_V_PTR_HOT', 'RZTRKBAR_V_PTR_DISABLED', 'RZTRKBAR_V_PTR_MASK' ) );
  Win95PtrBmpNames: array[ TOrientation, TBitmapType ] of PChar =
    ( ( 'RZTRKBAR_WIN95', 'RZTRKBAR_WIN95_HOT', 'RZTRKBAR_WIN95_DISABLED', 'RZTRKBAR_WIN95_MASK' ),
      ( 'RZTRKBAR_V_PTR', 'RZTRKBAR_V_PTR_HOT', 'RZTRKBAR_V_PTR_DISABLED', 'RZTRKBAR_V_PTR_MASK' ) );
  MixerBmpNames: array[ TOrientation, TBitmapType ] of PChar =
    ( ( 'RZTRKBAR_H_MIXER', 'RZTRKBAR_H_MIXER_HOT', 'RZTRKBAR_H_MIXER_DISABLED', 'RZTRKBAR_H_MIXER_MASK' ),
      ( 'RZTRKBAR_V_MIXER', 'RZTRKBAR_V_MIXER_HOT', 'RZTRKBAR_V_MIXER_DISABLED', 'RZTRKBAR_V_MIXER_MASK' ) );
  BoxBmpNames: array[ TOrientation, TBitmapType ] of PChar =
    ( ( 'RZTRKBAR_H_BOX', 'RZTRKBAR_H_BOX_HOT', 'RZTRKBAR_H_BOX_DISABLED', 'RZTRKBAR_H_BOX_MASK' ),
      ( 'RZTRKBAR_V_BOX', 'RZTRKBAR_V_BOX_HOT', 'RZTRKBAR_V_BOX_DISABLED', 'RZTRKBAR_V_BOX_MASK' ) );
  FlatBmpNames: array[ TOrientation, TBitmapType ] of PChar =
    ( ( 'RZTRKBAR_H_FLAT', 'RZTRKBAR_H_FLAT_HOT', 'RZTRKBAR_H_FLAT_DISABLED', 'RZTRKBAR_H_FLAT_MASK' ),
      ( 'RZTRKBAR_V_FLAT', 'RZTRKBAR_V_FLAT_HOT', 'RZTRKBAR_V_FLAT_DISABLED', 'RZTRKBAR_V_FLAT_MASK' ) );


procedure TRzTrackBar.LoadThumbBitmaps;
const
  BaseColors: array[ 0..11 ] of TColor =
  (
    clSilver,      // Normal Fill
    clOlive,       // Normal Fill Dark
    clYellow,      // Normal Fill Light
    clGray,        // Normal Frame
    clGreen,       // Normal Frame Dark
    clBlue,        // Hot Fill
    clNavy,        // Hot Fill Dark
    clAqua,        // Hot Fill Light
    clTeal,        // Hot Frame
    clPurple,      // Hot Frame Dark
    clFuchsia,     // Hot Frame Extra Dark
    clMaroon       // Disabled Frame
  );
var
  W: Integer;
  SrcRct, DstRct: TRect;
  C, HotColor: TColor;
  ReplaceColors: array[ 0..11 ] of TColor;
begin
  if UseThemes then
  begin
    // Correct thumb images (from theme/style) will be drawn in DrawThumb, but
    // we still need to get the thumb height and width.

    FThumbHeight := 21;
    FThumbWidth := 11;
    FHalfWidth := FThumbWidth div 2;
  end
  else // No Themes
  begin

    // Define Color Mappings

    if UsingSystemStyle then
    begin
      ReplaceColors[ 0 ] := clBtnFace;                         // Normal Fill
      ReplaceColors[ 1 ] := DarkerColor( clBtnFace, 30 );      // Normal Fill Dark
      ReplaceColors[ 2 ] := LighterColor( clBtnFace, 30 );     // Normal Fill Light
      ReplaceColors[ 3 ] := clBtnShadow;                       // Normal Frame
      ReplaceColors[ 4 ] := DarkerColor( clBtnShadow, 30 );    // Normal Frame Dark

      HotColor := BlendColors( clWhite, FHighlightColor, 100 );

      ReplaceColors[ 5 ] := HotColor;                          // Hot Fill
      ReplaceColors[ 6 ] := DarkerColor( HotColor, 30 );       // Hot Fill Dark
      ReplaceColors[ 7 ] := LighterColor( HotColor, 30 );      // Hot Fill Light
      ReplaceColors[ 8 ] := DarkerColor( HotColor, 50 );       // Hot Frame
      ReplaceColors[ 9 ] := DarkerColor( HotColor, 60 );       // Hot Frame Dark
      ReplaceColors[ 10 ] := DarkerColor( HotColor, 80 );      // Hot Frame Extra Dark
      ReplaceColors[ 11 ] := LighterColor( clBtnShadow, 20 );  // Disabled Frame
    end
    else // VCL Styles
    begin
      C := BlendColors( clWhite, ActiveStyleColor( scGenericGradientBase ), 100 );
      ReplaceColors[ 0 ] := C;                                 // Normal Fill
      ReplaceColors[ 1 ] := DarkerColor( C, 30 );              // Normal Fill Dark
      ReplaceColors[ 2 ] := LighterColor( C, 30 );             // Normal Fill Light

      C := ActiveStyleColor( scGenericGradientEnd );
      ReplaceColors[ 3 ] := C;                                 // Normal Frame
      ReplaceColors[ 4 ] := DarkerColor( C, 30 );              // Normal Frame Dark

      HotColor := LighterColor( ActiveStyleColor( scGenericGradientBase ), 30 );

      ReplaceColors[ 5 ] := HotColor;                          // Hot Fill
      ReplaceColors[ 6 ] := DarkerColor( HotColor, 30 );       // Hot Fill Dark
      ReplaceColors[ 7 ] := LighterColor( HotColor, 30 );      // Hot Fill Light
      ReplaceColors[ 8 ] := DarkerColor( HotColor, 50 );       // Hot Frame
      ReplaceColors[ 9 ] := DarkerColor( HotColor, 60 );       // Hot Frame Dark
      ReplaceColors[ 10 ] := DarkerColor( HotColor, 80 );      // Hot Frame Extra Dark

      ReplaceColors[ 11 ] := LighterColor( ActiveStyleSystemColor( clBtnShadow ), 20 );  // Disabled Frame
    end;


    case FThumbStyle of
      tsBox, tsXPBox:
      begin
        FThumbBmp.Handle := CreateMappedRes( HInstance, BoxBmpNames[ FOrientation, btThumb ], BaseColors, ReplaceColors  );
        FHotThumbBmp.Handle := CreateMappedRes( HInstance, BoxBmpNames[ FOrientation, btHot ], BaseColors, ReplaceColors );
        FDisabledBmp.Handle := CreateMappedRes( HInstance, BoxBmpNames[ FOrientation, btDisabled ], BaseColors, ReplaceColors );
        FMaskBmp.Handle := LoadBitmap( HInstance, BoxBmpNames[ FOrientation, btMask ] );
      end;

      tsCustom:
      begin
        W := FCustomThumb.Width div 3;
        FThumbBmp.Width := W;
        FThumbBmp.Height := FCustomThumb.Height;
        SrcRct := Rect( 0, 0, W, FCustomThumb.Height );
        DstRct := SrcRct;
        FThumbBmp.Canvas.CopyRect( DstRct, FCustomThumb.Canvas, SrcRct );

        FHotThumbBmp.Width := W;
        FHotThumbBmp.Height := FCustomThumb.Height;
        FHotThumbBmp.Canvas.CopyRect( DstRct, FCustomThumb.Canvas, SrcRct );

        FMaskBmp.Width := W;
        FMaskBmp.Height := FCustomThumb.Height;
        SrcRct := Rect( W, 0, W + W, FCustomThumb.Height );
        FMaskBmp.Canvas.CopyRect( DstRct, FCustomThumb.Canvas, SrcRct );

        FDisabledBmp.Width := W;
        FDisabledBmp.Height := FCustomThumb.Height;
        SrcRct := Rect( W + W, 0, 3 * W, FCustomThumb.Height );
        FDisabledBmp.Canvas.CopyRect( DstRct, FCustomThumb.Canvas, SrcRct );
      end;

      tsMixer:
      begin
        FThumbBmp.Handle := CreateMappedRes( HInstance, MixerBmpNames[ FOrientation, btThumb ], BaseColors, ReplaceColors  );
        FHotThumbBmp.Handle := CreateMappedRes( HInstance, MixerBmpNames[ FOrientation, btHot ], BaseColors, ReplaceColors );
        FDisabledBmp.Handle := CreateMappedRes( HInstance, MixerBmpNames[ FOrientation, btDisabled ], BaseColors, ReplaceColors );
        FMaskBmp.Handle := LoadBitmap( HInstance, MixerBmpNames[ FOrientation, btMask ] );
      end;

      tsPointer, tsXPPointer:
      begin
        if FPointerType = ptStandard then
        begin
          FThumbBmp.Handle := CreateMappedRes( HInstance, PointerBmpNames[ FOrientation, btThumb ], BaseColors, ReplaceColors  );
          FHotThumbBmp.Handle := CreateMappedRes( HInstance, PointerBmpNames[ FOrientation, btHot ], BaseColors, ReplaceColors );
          FDisabledBmp.Handle := CreateMappedRes( HInstance, PointerBmpNames[ FOrientation, btDisabled ], BaseColors, ReplaceColors );
          FMaskBmp.Handle := LoadBitmap( HInstance, PointerBmpNames[ FOrientation, btMask ] );
        end
        else
        begin
          FThumbBmp.Handle := CreateMappedRes( HInstance, Win95PtrBmpNames[ FOrientation, btThumb ], BaseColors, ReplaceColors  );
          FHotThumbBmp.Handle := CreateMappedRes( HInstance, Win95PtrBmpNames[ FOrientation, btHot ], BaseColors, ReplaceColors );
          FDisabledBmp.Handle := CreateMappedRes( HInstance, Win95PtrBmpNames[ FOrientation, btDisabled ], BaseColors, ReplaceColors );
          FMaskBmp.Handle := LoadBitmap( HInstance, Win95PtrBmpNames[ FOrientation, btMask ] );
        end;
      end;

      tsFlat:
      begin
        FThumbBmp.Handle := CreateMappedRes( HInstance, FlatBmpNames[ FOrientation, btThumb ], BaseColors, ReplaceColors  );
        FHotThumbBmp.Handle := CreateMappedRes( HInstance, FlatBmpNames[ FOrientation, btHot ], BaseColors, ReplaceColors );
        FDisabledBmp.Handle := CreateMappedRes( HInstance, FlatBmpNames[ FOrientation, btDisabled ], BaseColors, ReplaceColors );
        FMaskBmp.Handle := LoadBitmap( HInstance, FlatBmpNames[ FOrientation, btMask ] );
      end;
    end; { case }

    if FOrientation = orVertical then
    begin
      FThumbHeight := FThumbBmp.Width;
      FThumbWidth := FThumbBmp.Height;
    end
    else
    begin
      FThumbHeight := FThumbBmp.Height;
      FThumbWidth := FThumbBmp.Width;
    end;
    FHalfWidth := FThumbWidth div 2;
  end;
end; {= TRzTrackBar.LoadThumbBitmaps =}


procedure TRzTrackBar.DrawTrack( ACanvas: TCanvas );
var
  C: TColor;
begin
  // Calculate the Size of the Track
  if FOrientation = orVertical then
  begin
    FTrackRct.Top := FHalfWidth + FBorderWidth;
    FTrackRct.Bottom := Height - FBorderWidth - FHalfWidth;
    FTrackRct.Left := FTrackOffset;
    FTrackRct.Right := FTrackRct.Left + FTrackWidth;
  end
  else
  begin
    FTrackRct.Top := FTrackOffset;
    FTrackRct.Bottom := FTrackRct.Top + FTrackWidth;
    FTrackRct.Left := FHalfWidth + FBorderWidth;
    FTrackRct.Right := Width - FBorderWidth - FHalfWidth;
  end;

  if Assigned( FOnDrawTrack ) then
    DoDrawTrack( ACanvas, FTrackRct )
  else
  begin
    // Draw the Track
    if Enabled then
      ACanvas.Brush.Color := ActiveStyleSystemColor( FTrackColor )
    else
      ACanvas.Brush.Color := ActiveStyleSystemColor( clBtnFace );

    ACanvas.FillRect( FTrackRct );

    if FTrackFrame = fsFlat then
    begin
      C := ActiveStyleSystemColor( FTrackFrameColor );
      DrawSides( ACanvas, FTrackRct, C, C, sdAllSides );
    end
    else
      DrawBorder( ACanvas, FTrackRct, FTrackFrame );
  end;
end; {= TRzTrackBar.DrawTrack =}


procedure TRzTrackBar.DrawTicks( ACanvas: TCanvas );
var
  Delta: Real;
  I, X, Y: Integer;
begin
  if UsingSystemStyle then
  begin
    ACanvas.Pen.Color := FTickColor;
  end
  else // VCL Styles
  begin
    ACanvas.Pen.Color := ActiveStyleSystemColor( clBtnText );
  end;


  if FOrientation = orVertical then
  begin
    // Delta is spacing between tick marks
    Delta := ( Height - FThumbWidth - 2 * FBorderWidth ) / ( FMax - FMin );

    for I := FMin to FMax do
    begin
      if I mod FTickStep = 0 then
      begin
        Y := Height - FThumbWidth - Trunc( Delta * ( I - FMin ) ) - FBorderWidth;

        if FTickStyle = tkStandard then
        begin
          ACanvas.MoveTo( FBorderWidth, Y + FHalfWidth );
          ACanvas.LineTo( 10, Y + FHalfWidth );
          if not ( FThumbStyle in [ tsPointer, tsXPPointer ] ) then
          begin
            // Draw Ticks on Other Side
            ACanvas.MoveTo( Width - 10, Y + FHalfWidth );
            ACanvas.LineTo( Width - FBorderWidth, Y + FHalfWidth );
          end;
        end
        else
        begin
          // Provide hook to owner draw ticks
          DrawTick( ACanvas, Point( 0, Y + FHalfWidth ), I );
        end;
      end;
    end;
  end
  else // FOrientation = orHorizontal
  begin
    Delta := ( Width - FThumbWidth - 2 * FBorderWidth ) / ( FMax - FMin );

    for I := FMin to FMax do
    begin
      if I mod FTickStep = 0 then
      begin
        X := Trunc( Delta * ( I - FMin ) ) + FBorderWidth;

        if FTickStyle = tkStandard then
        begin
          if ( FThumbStyle = tsPointer ) and ( FPointerType = ptWin95 ) then
          begin
            ACanvas.MoveTo( X + FHalfWidth, Height - 10 );
            ACanvas.LineTo( X + FHalfWidth, Height - FBorderWidth );
          end
          else
          begin
            ACanvas.MoveTo( X + FHalfWidth, FBorderWidth );
            ACanvas.LineTo( X + FHalfWidth, 10 );
            if not ( FThumbStyle in [ tsPointer, tsXPPointer ] ) then
            begin
              // Draw Ticks on Other Side
              ACanvas.MoveTo( X + FHalfWidth, Height - 10 );
              ACanvas.LineTo( X + FHalfWidth, Height - FBorderWidth );
            end;
          end;
        end
        else // Provide hook to owner draw ticks
          DrawTick( ACanvas, Point( X + FHalfWidth, 0 ), I );
      end;
    end;
  end;
end; {= TRzTrackBar.DrawTicks =}


procedure TRzTrackBar.DrawThumb( ACanvas: TCanvas );
var
  Offset: Longint;
  WorkBmp: TBitmap;
  R, WorkRct, OldThumbRct, RefreshRct: TRect;
  Delta: Integer;
  Details: TThemedElementDetails;
begin
  if ( FThumbStyle in [ tsPointer, tsXPPointer ] ) and
     ( ( FOrientation = orVertical ) or ( FPointerType = ptStandard ) ) then
    Delta := 2
  else
    Delta := 0;

  OldThumbRct := FThumbRct;

  // Calculate new location of thumb based on Position
  if FOrientation = orVertical then
  begin
    Offset := Trunc( Int( Height - FThumbWidth - 2 * FBorderWidth ) * ( FPosition - FMin ) / ( FMax - FMin ));
    FThumbRct.Left := FTrackOffset + (FTrackWidth - FThumbHeight) div 2 - Delta;
    FThumbRct.Right := FThumbRct.Left + FThumbHeight;
    FThumbRct.Bottom := Height - Offset - FBorderWidth;
    FThumbRct.Top := FThumbRct.Bottom - FThumbWidth;
  end
  else
  begin
    Offset := Trunc( Int(  Width - FThumbWidth - 2 * FBorderWidth ) * ( FPosition - FMin ) / ( FMax - FMin ));
    FThumbRct.Left := Offset + FBorderWidth;
    FThumbRct.Right := FThumbRct.Left + FThumbWidth;
    FThumbRct.Top := FTrackOffset + (FTrackWidth - FThumbHeight) div 2 - Delta;
    FThumbRct.Bottom := FThumbRct.Top + FThumbHeight;
  end;



  if FUpdateBg then
    UpdateBackground;

  // Calculate the area that needs to be refreshed, then restore that area using the background bitmap
  SubtractRect( RefreshRct, OldThumbRct, FThumbRct );
  ACanvas.CopyRect( RefreshRct, FBackgroundBmp.Canvas, RefreshRct );

  // Draw the thumb by displaying the thumb bitmap

  // WorkBmp is used to combine the Thumb bitmap and the background so that the
  // background of the track appears in the corners of the Thumb image.

  WorkBmp := TBitmap.Create;
  try
    if FOrientation = orHorizontal then
    begin
      WorkBmp.Height := FThumbHeight;
      WorkBmp.Width := FThumbWidth;
      WorkRct := Rect( 0, 0, FThumbWidth, FThumbHeight );
    end
    else // Vertical
    begin
      WorkBmp.Height := FThumbWidth;
      WorkBmp.Width := FThumbHeight;
      WorkRct := Rect( 0, 0, FThumbHeight, FThumbWidth );
    end;

    // Copy the Thumb area on the FBackgroundBmp image to the WorkBmp

    WorkBmp.Canvas.CopyMode := cmSrcCopy;
    WorkBmp.Canvas.CopyRect( WorkRct, FBackgroundBmp.Canvas, FThumbRct );

    if UseThemes then
    begin
      // Determine the correct element to display, then draw it on the WorkBmp

      Details := GetThumbDetails;
      R := Rect( 0, 0, WorkBmp.Width, WorkBmp.Height );
      ActiveStyleServices.DrawElement( WorkBmp.Canvas.Handle, Details, R );
    end
    else // No Themes
    begin
      // Combine the FBackgroundBmp and the FMaskBmp images using the cmSrcAnd CopyMode.
      // White pixels in mask have no effect. Background shows thru.
      WorkBmp.Canvas.CopyMode := cmSrcAnd;
      WorkBmp.Canvas.CopyRect( WorkRct, FMaskBmp.Canvas, WorkRct );

      // Copy the Thumb bitmap onto the Working bitmap using the cmSrcPaint mode.
      // Black pixels in Thumb bitmap let background show through.

      WorkBmp.Canvas.CopyMode := cmSrcPaint;

      if not Enabled then
        WorkBmp.Canvas.CopyRect( WorkRct, FDisabledBmp.Canvas, WorkRct )
      else
      begin
        // Only show HotThumb at runtime b/c @ design-time we don't get an event to restore the normal thumb.
        if FUseHotThumb and not ( csDesigning in ComponentState ) then
          WorkBmp.Canvas.CopyRect( WorkRct, FHotThumbBmp.Canvas, WorkRct )
        else
          WorkBmp.Canvas.CopyRect( WorkRct, FThumbBmp.Canvas, WorkRct );
      end;
    end;


    // Copy the working bitmap onto the control's ACanvas at thumb position
    ACanvas.CopyRect( FThumbRct, WorkBmp.Canvas, WorkRct );
  finally
    WorkBmp.Free;
  end;
end; {= TRzTrackBar.DrawThumb =}


procedure TRzTrackBar.Paint;
begin
  if ShowFocus and Focused and FShowFocusRect then
  begin
    if UsingSystemStyle then
      Canvas.DrawFocusRect( ClientRect )
    else
    begin
      DrawFocusBorder( Canvas, ClientRect, ActiveStyleSystemColor( clWindowText ) );
    end;
  end;

  DrawTrack( Canvas );
  if FShowTicks then
    DrawTicks( Canvas );
  DrawThumb( Canvas );
end;


procedure TRzTrackBar.UpdateBackground;
begin
  // Save background image of entire control
  FBackgroundBmp.Width := Width;
  FBackgroundBmp.Height := Height;

  if FTransparent then
  begin
    FBackgroundBmp.Canvas.CopyRect( ClientRect, Canvas, ClientRect );
  end
  else
  begin
    if UsingSystemStyle then
      FBackgroundBmp.Canvas.Brush.Color := Color
    else
      FBackgroundBmp.Canvas.Brush.Color := ActiveStyleSystemColor( clBtnFace );

    FBackgroundBmp.Canvas.FillRect( ClientRect );
  end;
  DrawTrack( FBackgroundBmp.Canvas );
  if FShowTicks then
    DrawTicks( FBackgroundBmp.Canvas );
  FUpdateBg := False;
end;


procedure TRzTrackBar.Change;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


function TRzTrackBar.CanChange( NewPos: Integer ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, NewPos, Result );
end;


function TRzTrackBar.CanInternalChange( NewPos: Integer ): Boolean;
begin
  Result := True;
end;


procedure TRzTrackBar.DoDrawTrack( Canvas: TCanvas; Bounds: TRect );
begin
  if Assigned( FOnDrawTrack ) then
    FOnDrawTrack( Self, Canvas, Bounds );
end;

{===============================================================================
  TRzTrackBar.DrawTick

  This method is the event dispatch method for the OnDrawTick event.
  The parameters are:
    Canvas - The Canvas for the TrackBar Control
    Location - Point record indicating X or Y coordinates of tick mark
    Index - Position index of tick mark to be drawn
===============================================================================}

procedure TRzTrackBar.DrawTick( Canvas: TCanvas; Location: TPoint; Index: Integer );
begin
  if Assigned( FOnDrawTick ) then
    FOnDrawTick( Self, Canvas, Location, Index );
end;


{ Process wm_SetFocus and wm_KillFocus messages instead of overriding DoEnter
and DoExit because the window messages are correctly sent when the form is
activated and deactivated }

procedure TRzTrackBar.WMSetFocus( var Msg: TWMSetFocus );
begin
  inherited;
  // When control gets focus, update display to show focus border
  Repaint;
end;

procedure TRzTrackBar.WMKillFocus( var Msg: TWMKillFocus );
begin
  inherited;
  // When control loses focus, update display to remove focus border
  Repaint;
end;


procedure TRzTrackBar.KeyDown( var Key: Word; Shift: TShiftState );
var
  NewPosition: Integer;
begin
  inherited;

  NewPosition := FPosition;
  case Key of
    vk_Prior: // PgUp Key - Increases Position
      NewPosition := FPosition + FPageSize;

    vk_Next:  // PgDn Key - Decreases Position
      NewPosition := FPosition - FPageSize;

    vk_End:
    begin
      // End is at right for horizontal trackbar, bottom for vertical trackbar
      if FOrientation = orVertical then
        NewPosition := FMin
      else
        NewPosition := FMax;
    end;

    vk_Home:
    begin
      // Home is at left for horizontal trackbar, top for vertical trackbar
      if FOrientation = orVertical then
        NewPosition := FMax
      else
        NewPosition := FMin;
    end;

    vk_Left, vk_Down, vk_Subtract:
      if FPosition > FMin then
        NewPosition := FPosition - 1;

    vk_Up, vk_Right, vk_Add:
      if FPosition < FMax then
        NewPosition := FPosition + 1;
  end; { case }

  if CanInternalChange( NewPosition ) then
    Position := NewPosition;
end; {= TRzTrackBar.KeyDown =}


procedure TRzTrackBar.KeyPress( var Key: Char );
begin
  if FTabOnEnter and ( Ord( Key ) = vk_Return ) then
  begin
    Key := #0;
    PostMessage( Handle, wm_KeyDown, vk_Tab, 0 );
  end
  else
    inherited;
end;



procedure TRzTrackBar.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
var
  PtX, PtY, NewPosition: Integer;
  Delta: Real;
  IncPos: Boolean;
begin
  inherited;
  if not ( csDesigning in ComponentState ) then
    SetFocus;

  if ( Button = mbLeft ) and PtInRect( FThumbRct, Point( X, Y ) ) {and not FPaging} then
  begin
    // User pressed the left mouse button while on the thumb
    if FPaging then
      MouseUp( Button, Shift, X, Y );

    SetCapture( Handle );
    FSliding := True;
    FThumbCenterOffsetX := ( X - FThumbRct.Left ) -
                           ( ( FThumbRct.Right - FThumbRct.Left ) div 2 );
    FThumbCenterOffsetY := ( Y - FThumbRct.Top ) -
                           ( ( FThumbRct.Bottom - FThumbRct.Top ) div 2 );
  end
  else if ( Button = mbLeft ) and PtInRect( FTrackRct, Point( X, Y ) ) then
  begin
    // User pressed left mouse button inside the track on either side of thumb.
    // Determine which side of thumb user clicked, and then update position
    FRepeatTimer.Enabled := True;
    FLastX := X;
    FLastY := Y;

    if FOrientation = orVertical then
    begin
      Delta := ( Height - FThumbWidth - 2 * FBorderWidth ) / ( FMax - FMin );
      PtY := Trunc( Delta * ( ( FMax - FPosition ) - FMin ) ) + FBorderWidth;
      IncPos := Y < PtY;
    end
    else
    begin
      Delta := ( Width - FThumbWidth - 2 * FBorderWidth ) / ( FMax - FMin );
      PtX := Trunc( Delta * ( FPosition - FMin ) ) + FBorderWidth;
      IncPos := X >= PtX;
    end;

    if not FPaging then
    begin
      FPagingDirInc := IncPos;
      FPaging := True;
    end;

    if IncPos = FPagingDirInc then
    begin
      case FTrackClickBehavior of
        tcbNone: ; // Do nothing
        tcbMoveByPage:
        begin
          if IncPos then
            NewPosition := FPosition + FPageSize
          else
            NewPosition := FPosition - FPageSize;

          if CanInternalChange( NewPosition ) then
            Position := NewPosition;
        end;

        tcbMoveToTick:
        begin
          MoveThumbToPoint( X, Y );
        end;
      end;
    end
    else
      MouseUp( Button, Shift, X, Y );
  end;
end; {= TRzTrackBar.MouseDown =}



function TRzTrackBar.ShowFocus: Boolean;
begin
  Result := ( Perform( wm_QueryUIState, 0, 0 ) and UISF_HIDEFOCUS ) = 0;
end;


function TRzTrackBar.CursorPosition: TPoint;
begin
  GetCursorPos( Result );
  Result := ScreenToClient( Result );
end;


procedure TRzTrackBar.WMSetCursor( var Msg: TWMSetCursor );
begin
  // If mouse is over thumb, then use custom cursors
  if PtInRect( FThumbRct, CursorPosition ) then
  begin
    if FOrientation = orVertical then
      SetCursor( FVertCursor )
    else
      SetCursor( FHorzCursor );
  end
  else
    inherited;
end;


procedure TRzTrackBar.MoveThumbToPoint( X, Y: Integer );
var
  P, W, H: Integer;
begin
  if FOrientation = orVertical then
  begin
    H := FTrackRct.Bottom - FTrackRct.Top;
    P := Round( ( ( H - Y + FTrackRct.Top + FThumbCenterOffsetY ) / H ) *
                ( FMax - FMin ) + FMin );
  end
  else
  begin
    W := FTrackRct.Right - FTrackRct.Left;
    P := Round( ( ( X - FTrackRct.Left - FThumbCenterOffsetX ) / W ) *
                ( FMax - FMin ) + FMin  );
  end;

  if P > FMax then
    P := FMax;
  if P < FMin then
    P := FMin;
  if CanInternalChange( P ) then
    Position := P;
end;


procedure TRzTrackBar.MouseMove( Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  // If in Sliding state, then move the thumb to the closest tick mark.
  if FSliding then
  begin
    MoveThumbToPoint( X, Y );
  end
  else if PtInRect( FThumbRct, CursorPosition ) then
  begin
    FUseHotThumb := True;
    DrawThumb( Canvas );
  end
  else
  begin
    FUseHotThumb := False;
    DrawThumb( Canvas );
  end;
end; {= TRzTrackBar.MouseMove =}


procedure TRzTrackBar.MouseUp( Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  inherited;

  FRepeatTimer.Enabled := False;
  FRepeatTimer.Interval := 200;

  FPaging := False;
  if FSliding and ( Button = mbLeft ) then
  begin
    FSliding := False;
    ReleaseCapture;

    FUseHotThumb := False;
    DrawThumb( Canvas );
  end;
end;


procedure TRzTrackBar.TimerExpired( Sender: TObject );
begin
  FRepeatTimer.Interval := 100;
  if MouseCapture then
  begin
    try
      MouseDown( mbLeft, [], FLastX, FLastY );
    except
      FRepeatTimer.Enabled := False;
      FRepeatTimer.Interval := 200;
      raise;
    end;
  end;
end;


procedure TRzTrackBar.WMEraseBkgnd( var Msg: TWMEraseBkgnd );
begin
  if FTransparent then
  begin
    if ( Parent <> nil ) and Parent.DoubleBuffered then
      PerformEraseBackground( Self, Msg.DC );
    DrawParentImage( Self, Msg.DC, True );

    // Do not call inherited -- prevents TWinControl.WMEraseBkgnd from
    // erasing background. Set Msg.Result to 1 to indicate background is painted
    // by the control.
    Msg.Result := 1;
  end
  else
  begin
    if UsingSystemStyle then
      inherited
    else
    begin
      Canvas.Brush.Color := ActiveStyleSystemColor( clBtnFace );
      Canvas.FillRect( ClientRect );
    end;
  end;
end;


procedure TRzTrackBar.WMGetDlgCode( var Msg: TWMGetDlgCode );
begin
  Msg.Result := dlgc_WantArrows;
end;


procedure TRzTrackBar.WMSize( var Msg: TWMSize );
begin
  inherited;
  if Height > Width then
    Orientation := orVertical
  else
    Orientation := orHorizontal;
  FUpdateBg := True;
end;


procedure TRzTrackBar.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  FUpdateBg := True;
  Invalidate;
end;


procedure TRzTrackBar.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;
  FUpdateBg := True;
  Invalidate;
end;


procedure TRzTrackBar.CMDesignHitTest( var Msg: TCMDesignHitTest );
begin
  // Allow thumb to be moved w/ LMB at design-time
  if FSliding or PtInRect( FThumbRct, CursorPosition ) then
    Msg.Result := 1
  else
    Msg.Result := 0;
end;


procedure TRzTrackBar.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;
end;



procedure TRzTrackBar.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  if not FSliding then
  begin
    FUseHotThumb := False;
    DrawThumb( Canvas );
  end;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzTrackBar.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  LoadThumbBitmaps;
end;

{$ENDIF}


{&RUIF}
end.
