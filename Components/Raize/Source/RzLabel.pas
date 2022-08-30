{===============================================================================
  RzLabel Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzLabel
    Enhanced label control that supports 3D text styles, angle rotation, fly-by
    colors, etc.

  TRzURLLabel
    TRzLabel descendant that supports linking to a URL.


  Modification History
  ------------------------------------------------------------------------------
  6.2    (16 Jul 2015)
    * Fixed issue where the TRzLabel would not pick up the correct font color
      value under certain condition when the VCL Style was dynamically changed.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzLabel and TRzURLLabel to fully support
      VCL Styles introduced in RAD Studio XE2.
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * Fixed issue where the TRzLabel would not auto-size correctly when one of
      the Border* properties were set to fsFlatBold or fsFlatRounded.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed problem where the TRzLabel would not get sized correctly when
      AutoSize was set to True and TextMargin was greater than zero.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Updated display of disabled text in TRzLabel and descendants.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where changes to borders of TRzLabel would not cause the
      control to auto size itself if the AutoSize property was True.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem where TRzLabel and descendants would not pick up the correct
      settings when connected to a TRzFrameController in Delphi 5.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzLabel to
      account for changes introduced in Borland Developer Studio 2006.
    * Fixed positioning of TRzLabel.Caption when Layout property is set to
      tlCenter or tlBottom.
    * Added new FrameControllerNotifications property to TRzLabel and descendant
      label controls.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Added new TextMargin property, which controls the amount of space between
      the inside border of the control (or edge of control if no borders are
      visible) and the Caption's display area.
    * Fixed positioning of text when borders are displayed.
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Applied dt_RtlReading flag only when UseRightToLeftAlignment is true.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem with alignment of Caption when running under RTL systems.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where changing Color to clWindow would not get streamed
      correctly.
    * Added FlatColorAdjustment property.
    * Added FrameController property.
    * System cursor (IDC_HAND) is now used in TRzURLLabel instead of the custom
      hand cursor when running under Windows 98 or higher.
    * Added CondenseCaption property to TRzLabel. When set to ccAtEnd, if the
      Caption does not fit within the control's bounds, the end of the string is
      replaced with an ellipsis (...). When set to ccWithinPath, if the Caption
      is condense so that the start and end of the path string is displayed
      within the bounds of the control.
    * Added fsFlatRounded to inner and outer border styles.
    * Refactored inner and outer border painting to common DrawInnerOuterBorders
      function.
    * Fixed problem where under certain cases the bounds of a TRzLabel component
      that had a BorderWidth > 0 would not be adjusted when AutoSize was True.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Added dt_RtlReading flag to DrawText function calls when
      UseRightToLeftAlignment = True.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Fixed problem with AdjustForLayout method where it was offsetting
      rectangle uncessarily.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * The FrameSides property has been removed from the TRzLabel component.
    * Fixed problem where rotated text becomes corrupted when AutoSize is True.
===============================================================================}

{$I RzComps.inc}

unit RzLabel;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  SysUtils,
  Windows,
  Classes,
  Graphics,
  StdCtrls,
  Controls,
  RzCommon,
  Menus,
  ExtCtrls;

type
  {== TRzLabel Class Declaration ==}

  TRotation = ( roNone, roFlat, roCurve );
  TCenterPoint = ( cpUpperLeft,  cpUpperCenter, cpUpperRight,
                   cpLeftCenter, cpCenter,      cpRightCenter,
                   cpLowerLeft,  cpLowerCenter, cpLowerRight );

  TRzCondenseCaption = ( ccNone, ccAtEnd, ccWithinPath );

  TRzLabel = class( TLabel, IRzCustomFramingNotification )
  private
    FAngle: Integer;
    FBlinkColor: TColor;
    FBlinkState: TBlinkState;
    FBlinking: Boolean;
    FBevelWidth: Integer;
    FBorderInner: TFrameStyleEx;
    FBorderOuter: TFrameStyleEx;
    FBorderSides: TSides;
    FBorderColor: TColor;
    FBorderHighlight: TColor;
    FBorderShadow: TColor;
    FBorderWidth: Integer;
    FFlatColor: TColor;
    FFlatColorAdjustment: Integer;
    FFontColor: TColor;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    FCenterPoint: TCenterPoint;
    FOrigColor: TColor;
    FInitColor: Boolean;

    FFlyByEnabled: Boolean;
    FFlyByColor: TColor;

    FLightTextStyle: Boolean;
    FTextStyle: TTextStyle;
    FRotation: TRotation;
    FHighlightColor: TColor;
    FShadowColor: TColor;
    FShadowDepth: Integer;
    FTextMargin: Integer;

    FCondenseCaption: TRzCondenseCaption;

    { Message Handling Methods }
    procedure CMGetBlinking( var Msg: TMessage ); message cm_GetBlinking;
    procedure CMBlink( var Msg: TMessage ); message cm_Blink;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
  protected
    FAboutInfo: TRzAboutInfo;
    FCurrentColor: TColor;
    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    procedure Paint; override;
    procedure CustomFramingChanged; virtual;

    procedure Blink( State: TBlinkState ); virtual;

    function GetLabelText: string; override;

    procedure AdjustForBorders( var R: TRect ); virtual;
    procedure AdjustBounds; override;

    function CalcCenterPoint: TPoint;
    function CalcRadius: TPoint;
    procedure FixClientRect(var Rect: TRect; ShrinkByBorder: Boolean); virtual;
    procedure DrawArcText;
    procedure AdjustForLayout( Flags: DWord; var R: TRect );
    procedure Draw3DText;

    { Property Access Methods }
    procedure SetAngle( Value: Integer ); virtual;
    procedure SetBlinking( Value: Boolean ); virtual;
    procedure SetBlinkColor( Value: TColor ); virtual;
    function GetBlinkIntervalOff: Word; virtual;
    procedure SetBlinkIntervalOff( Value: Word ); virtual;
    function GetBlinkIntervalOn: Word; virtual;
    procedure SetBlinkIntervalOn( Value: Word ); virtual;
    procedure SetBevelWidth( Value: Integer ); virtual;
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderInner( Value: TFrameStyleEx ); virtual;
    procedure SetBorderOuter( Value: TFrameStyleEx ); virtual;
    procedure SetBorderSides( Value: TSides ); virtual;
    procedure SetBorderHighlight( Value: TColor ); virtual;
    procedure SetBorderShadow( Value: TColor ); virtual;
    procedure SetBorderWidth( Value: Integer ); virtual;
    procedure SetCondenseCaption( Value: TRzCondenseCaption ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetCenterPoint( Value: TCenterPoint ); virtual;
    procedure SetFlyByColor( Value: TColor ); virtual;
    procedure SetFlyByEnabled( Value: Boolean ); virtual;
    function GetLayout: TTextLayout; virtual;
    procedure SetLayout( Value: TTextLayout ); virtual;
    procedure SetLightTextStyle( Value: Boolean ); virtual;
    procedure SetRotation( Value: TRotation ); virtual;
    procedure SetTextMargin( Value: Integer ); virtual;
    procedure SetTextStyle( Value: TTextStyle ); virtual;
    procedure SetHighlightColor( Value: TColor ); virtual;
    procedure SetShadowColor( Value: TColor ); virtual;
    procedure SetShadowDepth( Value: Integer ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property BlinkState: TBlinkState
      read FBlinkState;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Angle: Integer
      read FAngle
      write SetAngle
      default 0;

    property Blinking: Boolean
      read FBlinking
      write SetBlinking
      default False;

    property BlinkColor: TColor
      read FBlinkColor
      write SetBlinkColor
      default clHighlight;

    property BlinkIntervalOff: Word
      read GetBlinkIntervalOff
      write SetBlinkIntervalOff
      default 500;

    property BlinkIntervalOn: Word
      read GetBlinkIntervalOn
      write SetBlinkIntervalOn
      default 500;

    property BevelWidth: Integer
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

    property BorderWidth: Integer
      read FBorderWidth
      write SetBorderWidth
      default 0;

    property CondenseCaption: TRzCondenseCaption
      read FCondenseCaption
      write SetCondenseCaption
      default ccNone;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;

    property CenterPoint: TCenterPoint
      read FCenterPoint
      write SetCenterPoint
      default cpCenter;

    property FlyByColor: TColor
      read FFlyByColor
      write SetFlyByColor
      default clHighlight;

    property FlyByEnabled: Boolean
      read FFlyByEnabled
      write SetFlyByEnabled
      default False;

    property Layout: TTextLayout
      read GetLayout
      write SetLayout
      default tlTop;

    property LightTextStyle: Boolean
      read FLightTextStyle
      write SetLightTextStyle
      default False;

    property Rotation: TRotation
      read FRotation
      write SetRotation
      default roNone;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clBtnHighlight;

    property ShadowColor: TColor
      read FShadowColor
      write SetShadowColor
      default clBtnShadow;

    property ShadowDepth: Integer
      read FShadowDepth
      write SetShadowDepth
      default 2;

    property TextMargin: Integer
      read FTextMargin
      write SetTextMargin
      default 0;

    property TextStyle: TTextStyle
      read FTextStyle
      write SetTextStyle
      default tsNormal;

    { Inherited Properties & Events }
    property Color default clBtnFace;
  end;




const
  crRzHandPoint = 26000;
  CondenseCaptionFlags: array[ TRzCondenseCaption ] of Word = ( 0, dt_End_Ellipsis, dt_Path_Ellipsis );

type
  TRzURLLabel = class( TRzLabel )
  private
    FURL: string;
    FVisited: Boolean;
    FUnvisitedColor: TColor;
    FVisitedColor: TColor;
  protected
    procedure UpdateFontColor; virtual;

    { Property Access Methods }
    procedure SetVisited( Value: Boolean ); virtual;
    procedure SetUnvisitedColor( Value: TColor ); virtual;
    procedure SetVisitedColor( Value: TColor ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;

    { Event Dispatch Methods }
    procedure Click; override;
  published
    property URL: string
      read FURL
      write FURL;

    property UnvisitedColor: TColor
      read FUnvisitedColor
      write SetUnvisitedColor
      default clHighlight;

    property Visited: Boolean
      read FVisited
      write SetVisited
      default False;

    property VisitedColor: TColor
      read FVisitedColor
      write SetVisitedColor
      default clPurple;

    { Inherited Properties & Events }
    property Cursor default crRzHandPoint;
    property TextStyle default tsNormal;
  end;


implementation

uses
  {&RAS}
  Types,
  Forms,
  ShellAPI,
  Themes,
  RzCommonCursors;

{&RT}
{======================}
{== TRzLabel Methods ==}
{======================}

constructor TRzLabel.Create( AOwner: TComponent );
begin
  inherited;

  // VCL 70 csOpaque is removed when Themes are available. However, this means that you cannot use the Color property
  // so we always turn csOpaque on.
  ControlStyle := ControlStyle + [ csOpaque ];

  FAngle := 0;
  FFlyByEnabled := False;
  FFlyByColor := clHighlight;
  FRotation := roNone;
  FCenterPoint := cpCenter;
  FLightTextStyle := False;
  FTextStyle := tsNormal;                               // Used to be tsRecessed
  FShadowDepth := 2;
  FShadowColor := clBtnShadow;
  FHighlightColor := clBtnHighlight;
  FBlinkColor := clHighlight;
  Layout := tlTop;
  FTextMargin := 0;

  { If the BlinkingControls object has not already been created, then create it }
  if BlinkingControls = nil then
    BlinkingControls := TRzBlinkingControlsList.Create;
  BlinkingControls.Add( Self );

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FBorderSides := [ sdLeft, sdTop, sdRight, sdBottom ];
  FBorderColor := clBtnFace;
  FBorderHighlight := clBtnHighlight;
  FBorderShadow := clBtnShadow;
  FBorderOuter := fsNone;
  FBevelWidth := 1;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;

  FInitColor := True;
  try
    Color := clBtnFace;
    ParentColor := True;
  finally
    FInitColor := False;
  end;
  {&RCI}
end;


destructor TRzLabel.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  BlinkingControls.Remove( Self );
  FreeBlinkingControlsListIfEmpty;
  inherited;
end;


procedure TRzLabel.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzLabel.Loaded;
begin
  inherited;
  if ( FRotation = roNone ) and ( Angle <> 0 ) then
    FRotation := roFlat;

  if ( FRotation <> roNone ) and not IsTrueTypeFont( Font ) then
    Font.Name := 'Verdana';  { Switch to Verdana if current font is not TrueType }

  FFontColor := Font.Color;
  FCurrentColor := ActiveStyleSystemColor( FFontColor );
  if csDesigning in ComponentState then
    Repaint;
  {&RV}
end;


procedure TRzLabel.DefineProperties( Filer: TFiler );
begin
  inherited;

  // Handle the fact that the FrameSides property was published in version 2.x
  Filer.DefineProperty( 'FrameSides', TRzOldPropReader.ReadOldSetProp, nil, False );
end;


procedure TRzLabel.SetAngle( Value: Integer );
begin
  if FAngle <> Value then
  begin
    if Value < 0 then
      FAngle := 360 - Abs( Value )
    else
      FAngle := Value mod 360;

    if not ( csLoading in ComponentState ) and
       ( Value <> 0 ) and
       not IsTrueTypeFont( Font ) then
    begin
      Font.Name := 'Verdana';  { Switch to Verdana if current font is not TrueType }
    end;

    if ( Angle <> 0 ) and ( FRotation = roNone ) then
      FRotation := roFlat;
    AdjustBounds;
    Repaint;
  end;
end;


procedure TRzLabel.SetBlinking( Value: Boolean );
begin
  if FBlinking <> Value then
  begin
    FBlinking := Value;
    if not FBlinking then
    begin
      FBlinkState := bsOff;
      FCurrentColor := ActiveStyleSystemColor( FFontColor );
      Repaint;
    end;
  end;
end;


procedure TRzLabel.SetBlinkColor( Value: TColor );
begin
  if FBlinkColor <> Value then
  begin
    FBlinkColor := Value;
    Invalidate;
  end;
end;


function TRzLabel.GetBlinkIntervalOff: Word;
begin
  Result := BlinkingControls.IntervalOff;
end;

procedure TRzLabel.SetBlinkIntervalOff( Value: Word );
begin
  BlinkingControls.IntervalOff := Value;
end;


function TRzLabel.GetBlinkIntervalOn: Word;
begin
  Result := BlinkingControls.IntervalOn;
end;

procedure TRzLabel.SetBlinkIntervalOn( Value: Word );
begin
  BlinkingControls.IntervalOn := Value;
end;


procedure TRzLabel.Blink( State: TBlinkState );
begin
  if State = bsOn then
  begin
    FBlinkState := bsOff;
    FCurrentColor := ActiveStyleSystemColor( FFontColor );
  end
  else
  begin
    FBlinkState := bsOn;
    FCurrentColor := ActiveStyleSystemColor( FBlinkColor );
  end;
  Refresh;
end;


procedure TRzLabel.SetBevelWidth( Value: Integer );
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    AdjustBounds;
  end;
end;

procedure TRzLabel.SetBorderSides( Value: TSides );
begin
  if FBorderSides <> Value then
  begin
    FBorderSides := Value;
    AdjustBounds;
  end;
end;


procedure TRzLabel.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TRzLabel.SetBorderHighlight( Value: TColor );
begin
  if FBorderHighlight <> Value then
  begin
    FBorderHighlight := Value;
    Invalidate;
  end;
end;

procedure TRzLabel.SetBorderShadow( Value: TColor );
begin
  if FBorderShadow <> Value then
  begin
    FBorderShadow := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetBorderInner( Value: TFrameStyleEx );
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    AdjustBounds;
  end;
end;


procedure TRzLabel.SetBorderOuter( Value: TFrameStyleEx );
begin
  if FBorderOuter <> Value then
  begin
    FBorderOuter := Value;
    AdjustBounds;
  end;
end;


procedure TRzLabel.SetBorderWidth( Value: Integer );
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    AdjustBounds;
  end;
end;


procedure TRzLabel.SetCondenseCaption( Value: TRzCondenseCaption );
begin
  if FCondenseCaption <> Value then
  begin
    FCondenseCaption := Value;
    if FCondenseCaption <> ccNone then
      AutoSize := False;
    Invalidate;
  end;
end;


procedure TRzLabel.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetFrameController( Value: TRzFrameController );
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


procedure TRzLabel.SetCenterPoint( Value: TCenterPoint );
begin
  if FCenterPoint <> Value then
  begin
    FCenterPoint := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetFlyByColor( Value: TColor );
begin
  if FFlyByColor <> Value then
  begin
    FFlyByColor := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetFlyByEnabled( Value: Boolean );
begin
  if FFlyByEnabled <> Value then
  begin
    FFlyByEnabled := Value;
    if not FFlyByEnabled then
      Font.Color := FOrigColor;
    Invalidate;
  end;
end;


function TRzLabel.GetLayout: TTextLayout;
begin
  Result := inherited Layout;
end;

procedure TRzLabel.SetLayout( Value: TTextLayout );
begin
  inherited Layout := Value;
end;


procedure TRzLabel.SetRotation( Value: TRotation );
begin
  if FRotation <> Value then
  begin
    if not ( csLoading in ComponentState ) and
       ( Value <> roNone ) and
       not IsTrueTypeFont( Font ) then
    begin
      Font.Name := 'Verdana';  { Switch to Verdana if current font is not TrueType }
    end;

    FRotation := Value;
    if FRotation = roNone then
      FAngle := 0
    else if FRotation = roFlat then
    begin
      FAngle := 0;
      FCenterPoint := cpCenter;
    end;
    Invalidate;
  end;
end;


procedure TRzLabel.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetShadowColor( Value: TColor );
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetShadowDepth( Value: Integer );
begin
  if FShadowDepth <> Value then
  begin
    FShadowDepth := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetLightTextStyle( Value: Boolean );
begin
  if FLightTextStyle <> Value then
  begin
    FLightTextStyle := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetTextMargin( Value: Integer );
begin
  if FTextMargin <> Value then
  begin
    FTextMargin := Value;
    Invalidate;
  end;
end;


procedure TRzLabel.SetTextStyle( Value: TTextStyle );
begin
  {&RV}
  if FTextStyle <> Value then
  begin
    FTextStyle := Value;
    Invalidate;
  end;
end;


function TRzLabel.CalcCenterPoint: TPoint;
begin
  if FRotation = roFlat then
    Result := Point( Width div 2, Height div 2 )
  else
  begin
    case FCenterPoint of
      cpUpperLeft:
        Result := Point( FBorderWidth, FBorderWidth );

      cpUpperCenter:
        Result := Point( Width div 2, FBorderWidth );

      cpUpperRight:
        Result := Point( Width, FBorderWidth );

      cpLeftCenter:
        Result := Point( FBorderWidth, Height div 2 );

      cpCenter:
        Result := Point( Width div 2, Height div 2 );

      cpRightCenter:
        Result := Point( Width - FBorderWidth, Height div 2 );

      cpLowerLeft:
        Result := Point( FBorderWidth, Height - FBorderWidth );

      cpLowerCenter:
        Result := Point( Width div 2, Height - FBorderWidth );

      cpLowerRight:
        Result := Point( Width - FBorderWidth, Height - FBorderWidth );
    end;
  end;
end;


function TRzLabel.CalcRadius: TPoint;
var
  BorderOffset: Integer;
begin
  BorderOffset := 2 * FBorderWidth;
  case FCenterPoint of
    cpUpperLeft, cpUpperRight, cpLowerLeft, cpLowerRight:
      Result := Point( Width - BorderOffset, Height - BorderOffset );

    cpUpperCenter, cpLowerCenter:
      Result := Point( Width div 2 - BorderOffset, Height - BorderOffset );

    cpLeftCenter, cpRightCenter:
      Result := Point( Width - BorderOffset, Height div 2 - BorderOffset );

    cpCenter:
      Result := Point( Width div 2 - BorderOffset, Height div 2 - BorderOffset );
  end;

end;


function TRzLabel.GetLabelText: string;
begin
  Result := Caption
end;


procedure TRzLabel.DrawArcText;
var
  LogFont: TLogFont;
  Rads: Double;
  I, X, Y: Integer;
  Center, Radius: TPoint;
  ULColor, LRColor: TColor;
  CaptionStr: string;
begin
  CaptionStr := GetLabelText;

  Center := CalcCenterPoint;
  Radius := CalcRadius;

  with Canvas do
  begin
    Font := Self.Font;

    Rads  := FAngle * Pi / 180;
    SetTextAlign( Canvas.Handle, ta_Center or ta_Baseline );
    GetObject( Font.Handle, SizeOf( TLogFont ), @LogFont );

    for I := 1 to Length( CaptionStr ) do
    begin
      X := Center.X - Round( ( {Center}Radius.X - TextHeight( 'Yy' ) ) * Sin( Rads ) );
      Y := Center.Y - Round( ( {Center}Radius.Y - TextHeight( 'Yy' ) ) * Cos( Rads ) );

      LogFont.lfEscapement := Round( Rads * 1800 / Pi );

      Font.Handle := CreateFontIndirect( LogFont );

      if FTextStyle in [ tsRecessed, tsRaised ] then
      begin
        if FTextStyle = tsRaised then
        begin
          ULColor := FHighlightColor;
          LRColor := FShadowColor;
        end
        else
        begin
          ULColor := FShadowColor;
          LRColor := FHighlightColor;
        end;

        if ( FTextStyle = tsRecessed ) or not FLightTextStyle then
        begin
          Font.Color := LRColor;
          TextOut( X + 1, Y + 1, CaptionStr[ I ] );
        end;

        if ( FTextStyle = tsRaised ) or not FLightTextStyle then
        begin
          Font.Color := ULColor;
          TextOut( X - 1, Y - 1, CaptionStr[ I ] );
        end;
      end
      else if FTextStyle = tsShadow then
      begin
        Font.Color := FShadowColor;
        TextOut( X + FShadowDepth, Y + FShadowDepth, CaptionStr[ I ] );
      end;

      Font.Color := FCurrentColor;
      if not Enabled then
      begin
        Font.Color := clBtnHighlight;
        TextOut( X + 1, Y + 1, CaptionStr[ I ] );
        Font.Color := clBtnShadow;
      end;

      TextOut( X, Y, CaptionStr[ I ] );

      Rads := Rads - ( ( ( TextWidth( 'w' ) + 2 ) ) / Radius.X );
    end;
  end;
end; {= TRzLabel.DrawArcText =}


procedure TRzLabel.AdjustForBorders( var R: TRect );
var
  Rad: Extended;
  X, Y: Integer;

  procedure AdjustRect( var R: TRect; Sides: TSides; N: Integer );
  begin
    if sdLeft in Sides then
      Inc( R.Right, N );
    if sdTop in Sides then
      Inc( R.Bottom, N );
    if sdRight in Sides then
      Inc( R.Right, N );
    if sdBottom in Sides then
      Inc( R.Bottom, N );
  end;

begin
  if FBorderOuter = fsFlat then
    AdjustRect( R, FBorderSides, 1 )
  else if FBorderOuter in [ fsStatus, fsPopup ] then
    AdjustRect( R, FBorderSides, BevelWidth )
  else if FBorderOuter in fsDoubleBorders then
    AdjustRect( R, FBorderSides, 2 );

  AdjustRect( R, FBorderSides, FBorderWidth );
  AdjustRect( R, sdAllSides, FTextMargin );

  if FBorderInner = fsFlat then
    AdjustRect( R, FBorderSides, 1 )
  else if FBorderInner in [ fsStatus, fsPopup ] then
    AdjustRect( R, FBorderSides, BevelWidth )
  else if FBorderInner in fsDoubleBorders then
    AdjustRect( R, FBorderSides, 2 );

  if ( FRotation = roNone ) and ( FTextStyle = tsShadow ) then
  begin
    Inc( R.Right, FShadowDepth );
    Inc( R.Bottom, FShadowDepth );
  end;

  if ( FRotation = roFlat ) and ( FAngle <> 0 ) and ( FAngle <> 180 ) then
  begin
    if FTextStyle = tsShadow then
    begin
      Inc( R.Right, FShadowDepth div 2 );
      Inc( R.Bottom, FShadowDepth div 2 );
    end;

    X := R.Right;
    Y := R.Bottom;
    if ( FAngle = 90 ) or ( FAngle = 270 ) then
      Rad := ( ( FAngle mod 90 ) * Pi / 180 ) + ( Pi / 2 )
    else
      Rad := ( ( FAngle mod 90 ) * Pi / 180 );
    R.Right := Round( X * Cos( Rad ) + Y * Sin( Rad ) );

    // X needs to be positive here otherwise, the Height becomes negative
    R.Bottom := Round( X * Sin( Rad ) + Y * Cos( Rad ) );
  end;
end;


procedure TRzLabel.AdjustBounds;
const
  WordWraps: array[ Boolean ] of Word = ( 0, dt_WordBreak );
var
  DC: HDC;
  X: Integer;
  R: TRect;
  TempAlignment: TAlignment;
begin
  if not ( csReading in ComponentState ) and AutoSize then
  begin
    R := ClientRect;
    FixClientRect( R, True );
    OffsetRect( R, -R.Left, -R.Top );

    DC := GetDC( 0 );
    Canvas.Handle := DC;
    DoDrawText( R, dt_ExpandTabs or dt_CalcRect or WordWraps[ WordWrap ] or
                   CondenseCaptionFlags[ FCondenseCaption ] );

    Canvas.Handle := 0;
    ReleaseDC( 0, DC );

    AdjustForBorders( R );

    X := Left;
    TempAlignment := Alignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment( TempAlignment );
    if TempAlignment = taRightJustify then
      Inc( X, Width - R.Right );

    SetBounds( X, Top, R.Right, R.Bottom );
  end;
end;


procedure TRzLabel.AdjustForLayout( Flags: DWord; var R: TRect );
var
  CalcRect: TRect;
begin
  if Layout <> tlTop then
  begin
    CalcRect := R;

    DrawString( Canvas, GetLabelText, CalcRect, Flags or dt_CalcRect or CondenseCaptionFlags[ FCondenseCaption ] );

    if Layout = tlBottom then
      OffsetRect( R, 0, ( R.Bottom - R.Top ) - ( CalcRect.Bottom - CalcRect.Top ) )
    else // Layout = tlCenter
      OffsetRect( R, 0, ( ( R.Bottom - R.Top ) - ( CalcRect.Bottom - CalcRect.Top ) ) div 2 );
  end;

  if ( Flags and dt_Right ) = dt_Right then
  begin
    if FTextStyle = tsShadow then
      OffsetRect( R, -FShadowDepth, 0 );
  end;

  if Layout = tlBottom then
  begin
    if FTextStyle = tsShadow then
      OffsetRect( R, 0, -FShadowDepth );
  end;
end;


procedure TRzLabel.FixClientRect( var Rect: TRect; ShrinkByBorder: Boolean );

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

begin
  if ShrinkByBorder then
    InflateRect( Rect, -BorderWidth, -BorderWidth );

  if FBorderOuter = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderOuter in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderOuter in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );

  if FBorderInner = fsFlat then
    AdjustRect( Rect, FBorderSides, 1 )
  else if FBorderInner in [ fsStatus, fsPopup ] then
    AdjustRect( Rect, FBorderSides, BevelWidth )
  else if FBorderInner in [ fsGroove..fsButtonUp, fsFlatBold, fsFlatRounded ] then
    AdjustRect( Rect, FBorderSides, 2 );
end;


procedure TRzLabel.Draw3DText;
var
  R, TempRct: TRect;
  ULColor, LRColor: TColor;
  Center: TPoint;
  Radius, Rad: Extended;
  Flags: DWord;
  H, W: Integer;
  HalfShadow, ShadowOffset: Integer;
  TempAlignment: TAlignment;

  function TextAligned( A: DWord ): Boolean;
  begin
    Result := ( Flags and A ) = A;
  end;

begin
  HalfShadow := FShadowDepth div 2;

  R := ClientRect;
  FixClientRect( R, True );
  InflateRect( R, -FTextMargin, -FTextMargin );

  TempAlignment := Alignment;
  if UseRightToLeftAlignment then
    ChangeBiDiModeAlignment( TempAlignment );

  Flags := dt_ExpandTabs or DrawTextAlignments[ TempAlignment ];

  if TempAlignment = taRightJustify then
    Flags := Flags or dt_Right;

  if UseRightToLeftAlignment then
    Flags := Flags or dt_RtlReading;

  if WordWrap then
    Flags := Flags or dt_WordBreak;

  if not ShowAccelChar then
    Flags := Flags or dt_NoPrefix;

  Canvas.Font := Self.Font;

  Center := CalcCenterPoint;

  if FRotation = roFlat then
  begin
    Rad := ( FAngle * Pi / 180 ) + ( Pi / 2 );
    Radius := Canvas.TextHeight( 'Pp' ) / 4;

    W := Canvas.TextWidth( Caption );
    H := Canvas.TextHeight( 'Pp' );
    if FTextStyle = tsShadow then
      ShadowOffset := {FShadowDepth} HalfShadow
    else
      ShadowOffset := 0;

    W := W + ShadowOffset;
    H := H + ShadowOffset;

    case FAngle of
      0, 360:
      begin
        if TextAligned( dt_Center ) then
          SetTextAlign( Canvas.Handle, ta_Center )
        else if TextAligned( dt_Right ) then
          Center.X := R.Right - W - ShadowOffset
        else
          Center.X := R.Left + ShadowOffset;
        Center.Y := Center.Y - H div 2;
      end;

      90:
      begin
        Center.X := Center.X - Round( Radius * Cos( Rad ) );
        if TextAligned( dt_Center ) then
          Center.Y := R.Bottom - ( R.Bottom - W ) div 2
        else if TextAligned( dt_Right ) then
          Center.Y := R.Top + W + ShadowOffset
        else
          Center.Y := R.Bottom - ShadowOffset;
        SetTextAlign( Canvas.Handle, ta_Left or ta_Baseline );
      end;

      180:
      begin
        if TextAligned( dt_Center ) then
          Center.X := R.Right - ( R.Right - W ) div 2
        else if TextAligned( dt_Right ) then
          Center.X := R.Left + W
        else
          Center.X := R.Right - ShadowOffset;
        Center.Y := Center.Y - ( H div 4 );
        SetTextAlign( Canvas.Handle, ta_Left or ta_Baseline );
      end;

      270:
      begin
        Center.X := Center.X - Round( Radius * Cos( Rad ) );
        if TextAligned( dt_Center ) then
          Center.Y := ( R.Bottom - W ) div 2
        else if TextAligned( dt_Right ) then
          Center.Y := R.Bottom - W - ShadowOffset
        else
          Center.Y := R.Left + ShadowOffset;
        SetTextAlign( Canvas.Handle, ta_Left or ta_Baseline );
      end;

      else
      begin
        Center.X := Center.X - Round( Radius * Cos( Rad ) );
        Center.Y := Center.Y + Round( Radius * Sin( Rad ) );
        SetTextAlign( Canvas.Handle, ta_Center or ta_Baseline );
      end;
    end; { case }

    Canvas.Font.Handle := RotateFont( Self.Font, FAngle );
  end; { if Rotation = roFlat }

  TempRct := R;
  if Enabled then
  begin
    if FTextStyle in [ tsRecessed, tsRaised ] then
    begin
      Canvas.Brush.Style := bsClear;

      if FTextStyle = tsRaised then
      begin
        ULColor := ActiveStyleSystemColor( FHighlightColor );
        LRColor := ActiveStyleSystemColor( FShadowColor );
      end
      else
      begin
        ULColor := ActiveStyleSystemColor( FShadowColor );
        LRColor := ActiveStyleSystemColor( FHighlightColor );
      end;

      if ( FTextStyle = tsRecessed ) or not FLightTextStyle then
      begin
        Canvas.Font.Color := LRColor;

        if FRotation = roFlat then
        begin
          OffsetRect( TempRct, 1, 1 );
          Canvas.TextRect( TempRct, Center.X + 1, Center.Y + 1, Caption );
        end
        else
        begin
          AdjustForLayout( Flags, TempRct );
          OffsetRect( TempRct, 1, 1 );
          DrawString( Canvas, GetLabelText, TempRct, Flags or CondenseCaptionFlags[ FCondenseCaption ] );
        end;
      end;

      if ( FTextStyle = tsRaised ) or not FLightTextStyle then
      begin
        TempRct := R;
        Canvas.Font.Color := ULColor;

        if FRotation = roFlat then
        begin
          OffsetRect( TempRct, -1, -1 );
          Canvas.TextRect( TempRct, Center.X - 1, Center.Y - 1, Caption );
        end
        else
        begin
          AdjustForLayout( Flags, TempRct );
          OffsetRect( TempRct, -1, -1 );
          DrawString( Canvas, GetLabelText, TempRct, Flags or CondenseCaptionFlags[ FCondenseCaption ] );
        end;
      end;
    end
    else if FTextStyle = tsShadow then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Color := ActiveStyleSystemColor( FShadowColor );

      if FRotation = roFlat then
      begin
        Canvas.TextRect( TempRct, Center.X + HalfShadow, Center.Y + HalfShadow, Caption );
      end
      else
      begin
        AdjustForLayout( Flags, TempRct );
        OffsetRect( TempRct, FShadowDepth, FShadowDepth );
        DrawString( Canvas, GetLabelText, TempRct, Flags or CondenseCaptionFlags[ FCondenseCaption ] );
      end;
    end;

    if not FBlinking and ActiveStyleServicesEnabled and not UsingSystemStyle then
      Canvas.Font.Color := ActiveStyleFontColor( sfTextLabelNormal )
    else
      Canvas.Font.Color := FCurrentColor;

    if FRotation = roFlat then
    begin
      if FTextStyle = tsShadow then
        Canvas.TextRect( TempRct, Center.X - HalfShadow, Center.Y - HalfShadow, Caption )
      else
        Canvas.TextRect( TempRct, Center.X, Center.Y, Caption )
    end
    else
    begin
      AdjustForLayout( Flags, R );
      DrawString( Canvas, GetLabelText, R, Flags or CondenseCaptionFlags[ FCondenseCaption ] );
    end;

  end
  else { if not Enabled }
  begin
    if UsingSystemStyle then
      Canvas.Font.Color := clGrayText
    else
      Canvas.Font.Color := ActiveStyleFontColor( sfWindowTextDisabled );

    if FRotation = roFlat then
    begin
      Canvas.TextRect( TempRct, Center.X, Center.Y, Caption );
    end
    else
    begin
      AdjustForLayout( Flags, R );
      DrawString( Canvas, GetLabelText, R, Flags or CondenseCaptionFlags[ FCondenseCaption ] );
    end;

  end;
end; {= TRzLabel.Draw3DText =}


type
  TWinControlAccess = class( TWinControl );


procedure TRzLabel.Paint;
var
  R: TRect;
  FrameColor, BdrColor, BdrHighlight, BdrShadow, PanelColor, ParentColor: TColor;
  FrameColorAdjustment: Integer;
  {$IFDEF VCL160_OR_HIGHER}
  C: TColor;
  Style: TCustomStyleServices;
  Details: TThemedElementDetails;
  {$ENDIF}
begin
  R := Rect( 0, 0, Width, Height );

  if UsingSystemStyle then
  begin
    FrameColor := FFlatColor;
    FrameColorAdjustment := FFlatColorAdjustment;
    if Parent <> nil then
      ParentColor := TWinControlAccess( Parent ).Color
    else
      ParentColor := clBtnFace;
    BdrColor := FBorderColor;
    BdrHighlight := FBorderHighlight;
    BdrShadow := FBorderShadow;
    PanelColor := Color;
  end
  else // VCL Styles
  begin
    {$IFDEF VCL160_OR_HIGHER}
    Style := StyleServices;

    FrameColor := Style.GetSystemColor( FFlatColor );
    FrameColorAdjustment := FFlatColorAdjustment;

    Details := Style.GetElementDetails( tpPanelBackground );
    if Style.GetElementColor( Details, ecFillColor, C ) and ( C <> clNone ) then
      PanelColor := C
    else
      PanelColor := Color;
    ParentColor := PanelColor;

    Details := Style.GetElementDetails( tpPanelBevel );
    if Style.GetElementColor( Details, ecEdgeHighLightColor, C ) and ( C <> clNone ) then
      BdrHighlight := C
    else
      BdrHighlight := FBorderHighlight;

    if Style.GetElementColor( Details, ecEdgeShadowColor, C ) and ( C <> clNone ) then
      BdrShadow := C
    else
      BdrShadow := FBorderShadow;

    BdrColor := Style.GetSystemColor( FBorderColor );

    {$ELSE}

    // To eliminate warnings in earlier versions of Delphi -- this code will not be called
    FrameColor := clNone;
    FrameColorAdjustment := 0;
    ParentColor := clNone;
    BdrColor := clNone;
    BdrHighlight := clNone;
    BdrShadow := clNone;
    PanelColor := clNone;
    {$ENDIF}
  end;


//  R := DrawInnerOuterBorders( Canvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, FBevelWidth,
//                              BdrColor, BdrHighlight, BdrShadow,
//                              FrameColor, FrameColorAdjustment, PanelColor, ParentColor,
//                              not ( csOpaque in ControlStyle ) {i.e. Transparent} );


  R := DrawInnerOuterBorders( Canvas, R, FBorderOuter, FBorderInner, BorderWidth, FBorderSides, BevelWidth,
                              BdrColor, BdrHighlight, BdrShadow,
                              FrameColor, FrameColorAdjustment, PanelColor, ParentColor, Transparent );

  if Transparent then
    Canvas.Brush.Style := bsClear
  else
  begin
    Canvas.Brush.Color := ActiveStyleSystemColor( Color );
    Canvas.FillRect( R );
  end;

  // Draw Caption

  if FRotation = roCurve then
    DrawArcText
  else
    Draw3DText;
end; {= TRzLabel.Paint =}


procedure TRzLabel.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameStyle in FFrameControllerNotifications then
      FBorderOuter := FFrameController.FrameStyle;
    if fcpFrameSides in FFrameControllerNotifications then
      FBorderSides := FFrameController.FrameSides;
    if fcpFrameColor in FFrameControllerNotifications then
    begin
      FFlatColor := FFrameController.FrameColor;
      FFlatColorAdjustment := 0;
    end;
    if fcpColor in FFrameControllerNotifications then
      Color := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;

    if fcpColor in FFrameControllerNotifications then
    begin
      FBorderHighlight := LighterColor( Color, 100 );
      FBorderShadow := DarkerColor( Color, 50 );
    end;

    Invalidate;
  end;
end;


procedure TRzLabel.CMGetBlinking( var Msg: TMessage );
begin
  inherited;
  if FBlinking then
    Msg.Result := 1
  else
    Msg.Result := 0;
end;


procedure TRzLabel.CMBlink( var Msg: TMessage );
begin
  inherited;
  if FBlinking then
    Blink( TBlinkState( Msg.WParam ) );
end;

procedure TRzLabel.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  if csLoading in ComponentState then
    Exit;

  FFontColor := Font.Color;
  if not FBlinking then
    FCurrentColor := ActiveStyleSystemColor( FFontColor );

  { Only TrueType fonts can be rotated }
  if ( FAngle <> 0 ) and not IsTrueTypeFont( Font ) then
    FAngle := 0;
end;


procedure TRzLabel.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  Repaint;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzLabel.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  if ActiveStyleServicesEnabled then
    FCurrentColor := ActiveStyleFontColor( sfTextLabelNormal )
  else
    FCurrentColor := FFontColor;
  Invalidate;
end;

{$ENDIF}


procedure TRzLabel.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  if FFlyByEnabled then
  begin
    FOrigColor := Font.Color;
    Font.Color := FFlyByColor;
  end;

  inherited;
end;


procedure TRzLabel.CMMouseLeave( var Msg: TMessage );
begin
  if FFlyByEnabled then
    Font.Color := FOrigColor;

  inherited;
end;


procedure TRzLabel.CMParentColorChanged( var Msg: TMessage );
begin
  // Do not allow side-effect of changing ParentColor to True when initializing the Color property from within the
  // constructor.
  if not FInitColor then
    inherited;
end;


{=========================}
{== TRzURLLabel Methods ==}
{=========================}

constructor TRzURLLabel.Create( AOwner: TComponent );
begin
  inherited;

  if RunningUnder( WinNT ) or RunningUnder( Win95 ) then
    Screen.Cursors[ crRzHandPoint ] := LoadCursor( HInstance, 'RZCOMMON_HANDCURSOR' )
  else
    Screen.Cursors[ crRzHandPoint ] := LoadCursor( 0, IDC_HAND );
  Cursor := crRzHandPoint;

  TextStyle := tsNormal;

  FVisited := False;
  FVisitedColor := clPurple;
  FUnvisitedColor := clHighlight;

  Font.Color := FUnvisitedColor;
  Font.Style := [ fsUnderline ];
  {&RCI}
end;


procedure TRzURLLabel.Click;
var
  S: string;
  ShellInfo: TShellExecuteInfo;
begin
  {&RV}
  inherited;

  if Trim( FURL ) = '' then
    S := Caption
  else
    S := FURL;

  if Trim( S ) <> '' then
  begin
    FillChar( ShellInfo, SizeOf( TShellExecuteInfo ), 0 );
    ShellInfo.cbSize := SizeOf( TShellExecuteInfo );
    ShellInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_FLAG_NO_UI or
                       SEE_MASK_FLAG_DDEWAIT;
    ShellInfo.Wnd := HWnd_Desktop;
    ShellInfo.lpVerb := 'Open';
    ShellInfo.lpFile := PChar( S );
    ShellInfo.lpParameters := nil;
    ShellInfo.lpDirectory := nil;
    ShellInfo.nShow := sw_ShowNormal;

    if ShellExecuteEx( @ShellInfo ) then
      Visited := True
    else
    begin
      if UpperCase( ExtractFileExt( S ) ) = '.HTM' then
        WinExec( 'RunDLL32.exe Shell32.dll,OpenAs_RunDLL *.htm', sw_ShowNormal );
    end;
  end;
end;


procedure TRzURLLabel.UpdateFontColor;
begin
  if FVisited then
    Font.Color := FVisitedColor
  else
    Font.Color := FUnvisitedColor;
end;


procedure TRzURLLabel.SetVisited( Value: Boolean );
begin
  if FVisited <> Value then
  begin
    FVisited := Value;
    UpdateFontColor;
    Invalidate;
  end;
end;


procedure TRzURLLabel.SetUnvisitedColor( Value: TColor );
begin
  if FUnvisitedColor <> Value then
  begin
    FUnvisitedColor := Value;
    UpdateFontColor;
    Invalidate;
  end;
end;

procedure TRzURLLabel.SetVisitedColor( Value: TColor );
begin
  if FVisitedColor <> Value then
  begin
    FVisitedColor := Value;
    UpdateFontColor;
    Invalidate;
  end;
end;

{&RUIF}
end.
