{===============================================================================
  RzDTP Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDateTimePicker
    TDateTimePicker descendant--adds custom framing, Format property, calendar
    colors, etc.


  Modification History
  ------------------------------------------------------------------------------
  6.1    (20 Sep 2012)
    * Fixed display issue in TRzDateTimePicker where the content of the drop
      down calendar was getting clipped in Windows Vista or later when the
      ShowWeekNumbers properties was set to True.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzDateTimePicker to
      account for changes introduced in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzDateTimePicker
      when FrameVisible was set to True and changes were made to control's
      appearance within calls to LockWindowUpdate.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Added StoreColor and StoreFocusColor methods so that if control is
      disabled at design-time the Color and FocusColor properties are not
      streamed with the disabled color value.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Deprecated. This component has been replaced by the TRzDateTimeEdit.
    * Renamed FrameFlat property to FrameHotTrack.
    * Renamed FrameFocusStyle property to FrameHotStyle.
    * Removed FrameFlatStyle property.
    * Add FocusColor and DisabledColor properties.
    * Fixed memory leak.
===============================================================================}

{$I RzComps.inc}
{$WARN SYMBOL_DEPRECATED OFF}

unit RzDTP;

interface

uses
  {&RF}
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  CommCtrl,
  RzCommon;

type
  TRzDTPDropRange = 1..4;

  TRzDateTimePicker = class;

  {=========================================}
  {== TRzMonthCalColors Class Declaration ==}
  {=========================================}

  TRzMonthCalColors = class( TPersistent )
  private
    Owner: TRzDateTimePicker;
    FBackColor: TColor;
    FTextColor: TColor;
    FTitleBackColor: TColor;
    FTitleTextColor: TColor;
    FMonthBackColor: TColor;
    FTrailingTextColor: TColor;
  public
    constructor Create( AOwner: TRzDateTimePicker );
    procedure Assign( Source: TPersistent ); override;

    procedure SetColor( Index: Integer; Value: TColor );
    procedure SetAllColors;
  published
    property BackColor: TColor
      index 0
      read FBackColor
      write SetColor
      default clWindow;

    property TextColor: TColor
      index 1
      read FTextColor
      write SetColor
      default clWindowText;

    property TitleBackColor: TColor
      index 2
      read FTitleBackColor
      write SetColor
      default clActiveCaption;

    property TitleTextColor: TColor
      index 3
      read FTitleTextColor
      write SetColor
      default clCaptionText;

    property MonthBackColor: TColor
      index 4
      read FMonthBackColor
      write SetColor
      default clWindow;

    property TrailingTextColor: TColor
      index 5
      read FTrailingTextColor
      write SetColor
      default clGray;
  end;


  {=========================================}
  {== TRzDateTimePicker Class Declaration ==}
  {=========================================}

  TRzDateTimePicker = class( TDateTimePicker )
  private
    FFlatButtons: Boolean;
    FFlatButtonColor: TColor;
    FUpdatingColor: Boolean;
    FDisabledColor: TColor;
    FFocusColor: TColor;
    FNormalColor: TColor;
    FFrameColor: TColor;
    FFrameController: TRzFrameController;
    FFrameHotColor: TColor;
    FFrameHotTrack: Boolean;
    FFrameHotStyle: TFrameStyle;
    FFrameSides: TSides;
    FFrameStyle: TFrameStyle;
    FFrameVisible: Boolean;
    FFramingPreference: TFramingPreference;
    FShowToday: Boolean;
    FShowTodayCircle: Boolean;
    FShowWeekNumbers: Boolean;
    FDropColumns: TRzDTPDropRange;
    FDropRows: TRzDTPDropRange;
    FFormat: string;

    { We have to keep track of the CalColors ourselves because the default
      class does not allow us to change the default streaming values. }
    FInternalCalColors: TRzMonthCalColors;

    procedure ReadOldFrameFlatProp( Reader: TReader );
    procedure ReadOldFrameFocusStyleProp( Reader: TReader );

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CNNotify( var Msg: TWMNotify ); message cn_Notify;
  protected
    FAboutInfo: TRzAboutInfo;
    FCanvas: TCanvas;
    FInControl: Boolean;
    FOverControl: Boolean;
    FSaveFormat: string;

    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    function GetEditRect: TRect; virtual;
    function GetRightJustifiedText: string; virtual;

    { Property Access Methods }
    procedure SetCalColors( Value: TRzMonthCalColors );
    procedure SetFlatButtons( Value: Boolean ); virtual;
    procedure SetFlatButtonColor( Value: TColor ); virtual;
    procedure SetFormat( const Value: string ); virtual;
    function StoreColor: Boolean;
    function StoreFocusColor: Boolean;
    function NotUsingController: Boolean;
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

    { Property Declarations }
    property Canvas: TCanvas
      read FCanvas;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
    procedure PaintTo( DC: HDC; X, Y: Integer );

    function UpdateCalColors( ColorIndex: Integer; ColorValue: TColor ): Boolean;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property CalColors: TRzMonthCalColors
      read FInternalCalColors
      write SetCalColors;

    property Color
      stored StoreColor
      default clWindow;

    property DropColumns: TRzDTPDropRange
      read FDropColumns
      write FDropColumns
      default 1;

    property DropRows: TRzDTPDropRange
      read FDropRows
      write FDropRows
      default 1;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write SetFlatButtonColor
      stored NotUsingController
      default clBtnFace;

    property FlatButtons: Boolean
      read FFlatButtons
      write SetFlatButtons
      stored NotUsingController
      default False;

    property Format: string
      read FFormat
      write SetFormat;

    property DisabledColor: TColor
      read FDisabledColor
      write SetDisabledColor
      stored NotUsingController
      default clBtnFace;

    property FocusColor: TColor
      read FFocusColor
      write SetFocusColor
      stored StoreFocusColor
      default clWindow;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      stored NotUsingController
      default clBtnShadow;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameHotColor: TColor
      read FFrameHotColor
      write SetFrameHotColor
      stored NotUsingController
      default clBtnShadow;

    property FrameHotStyle: TFrameStyle
      read FFrameHotStyle
      write SetFrameHotStyle
      stored NotUsingController
      default fsFlatBold;

    property FrameHotTrack: Boolean
      read FFrameHotTrack
      write SetFrameHotTrack
      stored NotUsingController
      default False;

    property FrameSides: TSides
      read FFrameSides
      write SetFrameSides
      stored NotUsingController
      default sdAllSides;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      stored NotUsingController
      default fsFlat;

    property FrameVisible: Boolean
      read FFrameVisible
      write SetFrameVisible
      stored NotUsingController
      default False;

    property FramingPreference: TFramingPreference
      read FFramingPreference
      write SetFramingPreference
      default fpXPThemes;

    property ShowToday: Boolean
      read FShowToday
      write FShowToday
      default False;

    property ShowTodayCircle: Boolean
      read FShowTodayCircle
      write FShowTodayCircle
      default False;

    property ShowWeekNumbers: Boolean
      read FShowWeekNumbers
      write FShowWeekNumbers
      default False;

    { Inherited Properties & Events }
    property FirstDayOfWeek;
  end deprecated;


implementation

uses
  {&RAS}
  Types,
  Themes,
  TypInfo;


{&RT}
{===============================}
{== TRzMonthCalColors Methods ==}
{===============================}


const
  ColorIndex: array[0..5] of Integer = (MCSC_BACKGROUND, MCSC_TEXT,
    MCSC_TITLEBK, MCSC_TITLETEXT, MCSC_MONTHBK, MCSC_TRAILINGTEXT);

constructor TRzMonthCalColors.Create( AOwner: TRzDateTimePicker );
begin
  Owner := AOwner;
  FBackColor := clWindow;
  FTextColor := clWindowText;
  FTitleBackColor := clActiveCaption;
  FTitleTextColor := clCaptionText;
  FMonthBackColor := clWindow;
  FTrailingTextColor := clGray;
end;

procedure TRzMonthCalColors.Assign( Source: TPersistent );
begin
  if Source is TRzMonthCalColors then
  begin
    FBackColor := TRzMonthCalColors(Source).BackColor;
    FTextColor := TRzMonthCalColors(Source).TextColor;
    FTitleBackColor := TRzMonthCalColors(Source).TitleBackColor;
    FTitleTextColor := TRzMonthCalColors(Source).TitleTextColor;
    FMonthBackColor := TRzMonthCalColors(Source).MonthBackColor;
    FTrailingTextColor := TRzMonthCalColors(Source).TrailingTextColor;
  end
  else
    inherited;
end;

procedure TRzMonthCalColors.SetColor(Index: Integer; Value: TColor);
begin
  case Index of
    0: FBackColor := Value;
    1: FTextColor := Value;
    2: FTitleBackColor := Value;
    3: FTitleTextColor := Value;
    4: FMonthBackColor := Value;
    5: FTrailingTextColor := Value;
  end;
  if Owner.HandleAllocated then
    Owner.UpdateCalColors( ColorIndex[ Index ], ColorToRGB( Value ) );
end;


procedure TRzMonthCalColors.SetAllColors;
begin
  SetColor( 0, FBackColor );
  SetColor( 1, FTextColor );
  SetColor( 2, FTitleBackColor );
  SetColor( 3, FTitleTextColor );
  SetColor( 4, FMonthBackColor );
  SetColor( 5, FTrailingTextColor );
end;


{===============================}
{== TRzDateTimePicker Methods ==}
{===============================}

constructor TRzDateTimePicker.Create( AOwner: TComponent );
begin
  inherited;

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

  FShowToday := False;
  FShowTodayCircle := False;
  FShowWeekNumbers := False;
  FDropRows := 1;
  FDropColumns := 1;

  FInternalCalColors := TRzMonthCalColors.Create( Self );

  FFlatButtons := False;
  FFlatButtonColor := clBtnFace;
  FDisabledColor := clBtnFace;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FFrameColor := clBtnShadow;
  FFrameController := nil;
  FFrameHotColor := clBtnShadow;
  FFrameHotTrack := False;
  FFrameHotStyle := fsFlatBold;
  FFrameSides := sdAllSides;
  FFrameStyle := fsFlat;
  FFrameVisible := False;
  FFramingPreference := fpXPThemes;
  {&RCI}
end;


destructor TRzDateTimePicker.Destroy;
begin
  FInternalCalColors.Free;

  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;



procedure TRzDateTimePicker.CreateWnd;
begin
  inherited;
  FInternalCalColors.SetAllColors;
  if FSaveFormat <> '' then
    SetFormat( FSaveFormat );
end;


procedure TRzDateTimePicker.DestroyWnd;
begin
  FSaveFormat := FFormat;
  inherited;
end;


procedure TRzDateTimePicker.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FrameFlat and FrameFocusStyle properties were renamed to
  // FrameHotStyle and FrameHotStyle respectively in version 3.
  Filer.DefineProperty( 'FrameFlat', ReadOldFrameFlatProp, nil, False );
  Filer.DefineProperty( 'FrameFocusStyle', ReadOldFrameFocusStyleProp, nil, False );

  // Handle the fact that the FrameFlatStyle was published in version 2.x
  Filer.DefineProperty( 'FrameFlatStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
end;


procedure TRzDateTimePicker.ReadOldFrameFlatProp( Reader: TReader );
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


procedure TRzDateTimePicker.ReadOldFrameFocusStyleProp( Reader: TReader );
begin
  FFrameHotStyle := TFrameStyle( GetEnumValue( TypeInfo( TFrameStyle ), Reader.ReadIdent ) );
end;


procedure TRzDateTimePicker.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzDateTimePicker.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


function TRzDateTimePicker.UpdateCalColors( ColorIndex: Integer; ColorValue: TColor ): Boolean;
begin
  Result := True;
  if HandleAllocated then
    Result := DateTime_SetMonthCalColor( Handle, ColorIndex, ColorValue ) <> DWORD($FFFFFFFF);
end;

procedure TRzDateTimePicker.SetCalColors( Value: TRzMonthCalColors );
begin
  FInternalCalColors.Assign( Value );
end;


procedure TRzDateTimePicker.CMColorChanged( var Msg: TMessage );
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


procedure TRzDateTimePicker.SetFlatButtonColor( Value: TColor );
begin
  if FFlatButtonColor <> Value then
  begin
    FFlatButtonColor := Value;
    Invalidate;
  end;
end;


procedure TRzDateTimePicker.SetFlatButtons( Value: Boolean );
begin
  if FFlatButtons <> Value then
  begin
    FFlatButtons := Value;
    Invalidate;
  end;
end;


procedure TRzDateTimePicker.SetFormat( const Value: string );
begin
  FFormat := Value;
  SendTextMessage( Handle, dtm_SetFormat, 0, FFormat );
end;


function TRzDateTimePicker.StoreColor: Boolean;
begin
  Result := NotUsingController and Enabled;
end;


function TRzDateTimePicker.StoreFocusColor: Boolean;
begin
  Result := NotUsingController and ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzDateTimePicker.NotUsingController: Boolean;
begin
  Result := FFrameController = nil;
end;


procedure TRzDateTimePicker.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzDateTimePicker.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzDateTimePicker.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDateTimePicker.SetFrameController( Value: TRzFrameController );
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


procedure TRzDateTimePicker.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDateTimePicker.SetFrameHotTrack( Value: Boolean );
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


procedure TRzDateTimePicker.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDateTimePicker.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzDateTimePicker.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDateTimePicker.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzDateTimePicker.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzDateTimePicker.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;

procedure TRzDateTimePicker.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


function TRzDateTimePicker.GetEditRect: TRect;
begin
  Result := ClientRect;
end;

function TRzDateTimePicker.GetRightJustifiedText: string;
begin
  Result := Text;
end;


{ This method is needed in order to paint the custom framing when a DTP
  is used in a DBCtrlGrid or anywhere else PaintTo is to be used. }

procedure TRzDateTimePicker.PaintTo( DC: HDC; X, Y: Integer );
var
  SaveIndex: Integer;
  R: TRect;
begin
  if FFrameVisible and not UseThemes then
  begin
    ControlState := ControlState + [ csPaintCopy ];
    SaveIndex := SaveDC( DC );

    FCanvas.Handle := DC;
    try
      MoveWindowOrg( DC, X, Y );
      IntersectClipRect( DC, 0, 0, Width, Height );

      SetRect(R, 0, 0, Width, Height);
      DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );

      MoveWindowOrg(DC, R.Left + 2, R.Top + 2);
      IntersectClipRect(DC, 0, 0, R.Right - R.Left, R.Bottom - R.Top);

      Perform( WM_ERASEBKGND, WParam( DC ), 0 );
      Perform( WM_PAINT, WParam( DC ), 0 );
    finally
      FCanvas.Handle := 0;
    end;

    RestoreDC(DC, SaveIndex);
    ControlState := ControlState - [ csPaintCopy ];
  end
  else
    inherited;
end;



procedure TRzDateTimePicker.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzDateTimePicker.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzDateTimePicker.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzDateTimePicker.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
begin
  inherited;

  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( FInControl or FOverControl ) then
        DrawFrame( FCanvas, Width, Height, FFrameHotStyle, Color, FFrameHotColor, FFrameSides )
      else
        DrawFrame( FCanvas, Width, Height, FFrameStyle, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzDateTimePicker.WMNCPaint =}


procedure TRzDateTimePicker.CMParentColorChanged( var Msg: TMessage );
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


procedure TRzDateTimePicker.WMPaint( var Msg: TWMPaint );
var
  BtnRect, TempRect: TRect;
  X, Y: Integer;
  ElementDetails: TThemedElementDetails;
begin
  inherited;

  if FFrameVisible and not UseThemes and ( DateMode = dmComboBox ) then
  begin
    // Erase Ctl3D Border
    if csPaintCopy in ControlState then
      BtnRect := Rect( Width - GetSystemMetrics( sm_CxVScroll ) - 4, 0, Width - 4, Height - 4 )
    else
      BtnRect := Rect( Width - GetSystemMetrics( sm_CxVScroll ) - 4, 0, Width - 2, Height - 2 );

    if FFlatButtons then
    begin
      if  not ( FInControl or FOverControl ) then
      begin
        // Erase Button Border
        FCanvas.Brush.Color := Color;
        FCanvas.FillRect( BtnRect );

        if Enabled then
          FCanvas.Brush.Color := clBlack
        else
          FCanvas.Brush.Color := clBtnShadow;

        FCanvas.Pen.Color := Color;
        FCanvas.Brush.Color := clBlack;
        X := BtnRect.Left + GetSystemMetrics( sm_CxVScroll ) div 2 - 1;
        Y := BtnRect.Top + Height div 2;
        FCanvas.Polygon( [ Point( X, Y ), Point( X - 5, Y - 5 ), Point( X + 5, Y - 5 ) ] );
      end
      else
      begin
        // Erase Button Border
        if ActiveStyleServicesEnabled then
        begin
          if DroppedDown then
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonPressed )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( ttbButtonHot );

          ActiveStyleServices.DrawElement( FCanvas.Handle, ElementDetails, BtnRect );
        end
        else
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
        end;

        if Enabled then
          FCanvas.Brush.Color := clBlack
        else
          FCanvas.Brush.Color := clBtnShadow;

        FCanvas.Pen.Color := FFlatButtonColor;
        X := BtnRect.Left + GetSystemMetrics( sm_CxVScroll ) div 2;
        Y := BtnRect.Top + Height div 2;
        if DroppedDown then
        begin
          Inc( X );
          Inc( Y );
        end;
        FCanvas.Polygon( [ Point( X, Y ), Point( X - 5, Y - 5 ), Point( X + 5, Y - 5 ) ] );
      end;
    end;
  end;
end; {= TRzDateTimePicker.WMPaint =}


procedure TRzDateTimePicker.UpdateColors;
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


procedure TRzDateTimePicker.UpdateFrame( ViaMouse, InFocus: Boolean );
var
  PaintIt: Boolean;
  R: TRect;
begin
  if DroppedDown then
    Exit;                                { Do not update frame if list is down }

  if ViaMouse then
    FOverControl := InFocus
  else
    FInControl := InFocus;

  PaintIt := FFlatButtons or FFrameHotTrack;

  if PaintIt then
  begin
    R := ClientRect;
    if not FFrameHotTrack then
      R.Left := R.Right - GetSystemMetrics( sm_CxVScroll ) - 2;
    RedrawWindow( Handle, @R, 0, rdw_Frame or rdw_Invalidate or rdw_NoErase );
  end;

  UpdateColors;
end;


procedure TRzDateTimePicker.CMMouseEnter( var Msg: TMessage );
begin
  {&RV}
  if csDesigning in ComponentState then
    Exit;

  inherited;

  UpdateFrame( True, True );
end;


procedure TRzDateTimePicker.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  UpdateFrame( True, False );
end;


procedure TRzDateTimePicker.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzDateTimePicker.CNNotify( var Msg: TWMNotify );
var
  H: HWnd;
  R: TRect;
  UnitWidth, UnitHeight: Integer;
  DOWFlag: Integer;
  A: array[ 0..1 ] of Char;
  {$IFDEF VCL160_OR_HIGHER}
  NewWidth, NewHeight, BorderSize: Integer;
  Info: TDateTimePickerInfo;
  {$ELSE}
  DC: HDC;
  {$ENDIF}


  procedure SetComCtlStyle( H: HWnd; Value: Integer; UseStyle: Boolean );
  var
    Style: Integer;
  begin
    if H <> 0 then
    begin
      Style := GetWindowLong( H, gwl_Style );
      if not UseStyle then
        Style := Style and not Value
      else
        Style := Style or Value;
      SetWindowLong( H, gwl_Style, Style );
    end;
  end;

begin
  case Msg.NMHdr^.Code of
    dtn_DropDown:
    begin
      H := HWnd( SendMessage( Handle, dtm_GetMonthCal, 0, 0 ) );

      if H <> 0 then
      begin
        if FirstDayOfWeek = dowLocaleDefault then
        begin
          GetLocaleInfo( locale_User_Default, locale_IFirstDayOfWeek, A, SizeOf( A ) );
          DOWFlag := Ord( A[ 0 ] ) - Ord( '0' );
        end
        else
          DOWFlag := Ord( FirstDayOfWeek );
        MonthCal_SetFirstDayOfWeek( H, DOWFlag );

        SetComCtlStyle( H, mcs_NoToday, not FShowToday );
        SetComCtlStyle( H, mcs_NoTodayCircle, not FShowTodayCircle );
        SetComCtlStyle( H, mcs_WeekNumbers, FShowWeekNumbers );

        {$IFDEF VCL160_OR_HIGHER}
        UnitWidth := 0;
        UnitHeight:= 0;

        if MonthCal_GetMinReqRect( H, R ) then
        begin
          UnitWidth := R.Right - R.Left;
          UnitHeight:= R.Bottom - R.Top;
          if FShowToday then
            UnitWidth := Max( UnitWidth, MonthCal_GetMaxTodayWidth( H ) );
        end;

        // Get Border Size
        BorderSize := 0;
        if Win32MajorVersion >= 6 then
           BorderSize := MonthCal_GetCalendarBorder( H ) shl 1;
        if BorderSize = 0 then
           BorderSize:= 4;
        Inc( UnitWidth,  BorderSize );
        Inc( UnitHeight, BorderSize );

        NewWidth := FDropColumns * UnitWidth;
        NewHeight:= FDropRows * UnitHeight;

        SetWindowPos( H, 0, 0, 0, NewWidth, NewHeight, swp_NoMove or swp_NoZOrder );
        if Win32MajorVersion >= 6 then
        begin
          // If Windows Vista or higher, the dropdown calendar has a parent separate parent window
          FillChar( Info, SizeOf( Info ), #0 );
          Info.cbSize := SizeOf( Info );
          DateTime_GetDateTimePickerInfo( Handle, Info );
          if IsWindow( Info.hwndDropDown ) then
            SetWindowPos( Info.hwndDropDown, 0, 0, 0, NewWidth, NewHeight, swp_NoMove or swp_NoZOrder );
        end;
        
        {$ELSE}
        GetWindowRect( H, R );
        UnitWidth := R.Right - R.Left + 4;
        if FShowWeekNumbers then
        begin
          DC := GetDC( H );
          try
            Canvas.Handle := DC;
            Canvas.Font := Self.Font;
            Inc( UnitWidth, Canvas.TextWidth( ' 33   ' ) );
            Canvas.Handle := 0;
          finally
            ReleaseDC( H, DC );
          end;
        end;
        UnitHeight := R.Bottom - R.Top;
        SetWindowPos( H, 0, 0, 0, FDropColumns * UnitWidth, FDropRows * UnitHeight, swp_NoMove or swp_NoZOrder );
        {$ENDIF}
        
      end;
    end;
  end;
  inherited;
end;


{&RUIF}
end.
