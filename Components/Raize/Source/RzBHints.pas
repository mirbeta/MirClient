{===============================================================================
  TRzBHints Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components            Description
  ------------------------------------------------------------------------------
  TRzBalloonHints       Application hints are displayed in a customizable
                          balloon hint window


  Modification History
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * Fixed display issue that would occur when creating a new instance of
      TRzCustomHintWindow and passing nil as the owner.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed display issue with setting Color property of TRzHintWindow.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Added new CenterThreshold property to TRzBalloonHints. This property is
      used to determine if a control is small enough so that balloon hints
      shown for the control will be centered to the control as opposed to being
      shown near the mouse cursor. The default value of 80 should be sufficient
      for general use as toolbar buttons and other small controls will meet
      this threshold and have their hints centered.
    * Fixed issue where balloon hints would span monitors in a multi-monitor
      system.
  ------------------------------------------------------------------------------
  4.1.2  (17 Apr 2007)
    * Fixed GDI Object leak in TRzBalloonHints when ShowBalloon is set to True.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Redesigned the TRzBalloonHints components and the supporting
      TRzCustomHintWindow class.
    * The TRzBalloonHints.Shadow property has been removed because with the
      new redesigned hint window the shadow is handle by the operating system.
      The major benefit of this is that the shadow is drawn using the alpha-
      blending capabilities of the operating system.
    * The shadow effect also works with balloon style hints with or without the
      pointer, as well as with rectangular hint windows.
    * The redesign of the custom hint window also fixed display issues that
      occurred when moving the mouse between controls while the hint window was
      still visible.
    * The hint positioning has also been refined in this release.
    * Fixed issue with incorrect word wrapping under certain circumstances.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * TRzBalloonHints now correctly displays hints when running under
      right-to-left (RTL) systems.
  ------------------------------------------------------------------------------
  3.1    (04 Aug 2005)
    * Fixed problem where the pointer area of the balloon hint would not get
      displayed in the same color specified in the Color property.
    * Added new FrameColor property to allow user to change the color of the
      line (i.e. frame) drawn around the hint window.
    * Added new enumerated value to TRzHintCorner type (hcNone). When the
      TRzBalloonHints.Corner property is set to hcNone, then hint window is
      displayed without the pointer area.  In other words, the hint window
      appears as a rounded rectangle.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where displaying a TRzCustomHintWindow would cause the
      window to be repeated displayed, and thus making it appear that the hint
      window was not being displayed at all.
    * Fixed problem where changing HintPause or HintShortPause at design-time
      changed the Application object for the Delphi IDE.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Set TRzBalloonHints.Font.Color to clInfoText as default.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Removed the BalloonStyle property.
    * The hint window displayed by the component is much cleaner and is similar
      to the BalloonHint window implemented in version 5 of the Shell32.dll
      for Windows.
===============================================================================}

{$I RzComps.inc}

unit RzBHints;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Forms,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  RzCommon,
  RzGrafx;

const
  MinWindowWidth = 55;
  MouseAdj = 2;

type
  TRzHintCorner = ( hcLowerRight, hcLowerLeft, hcUpperLeft, hcUpperRight, hcNone );

  TSetHintWinSizeEvent = procedure( Sender: TObject; Canvas: TCanvas; var Width, Height: Integer;
                                    Hint: string; Corner: TRzHintCorner ) of object;
  TSetHintRectEvent = procedure( Sender: TObject; Canvas: TCanvas; var Rect: TRect;
                                 Hint: string; Corner: TRzHintCorner ) of object;

  {=========================================}
  {== TRzBalloonBitmaps Class Declaration ==}
  {=========================================}

  PRzBalloonBitmaps = ^TRzBalloonBitmaps;

  TRzBalloonBitmaps = class( TPersistent )
  private
    FLowerRight: TBitmap;
    FLowerLeft: TBitmap;
    FUpperLeft: TBitmap;
    FUpperRight: TBitmap;
    FTransparentColor: TColor;
    FOnChange: TNotifyEvent;
  protected
    { Property Access Methods }
    procedure SetLowerRight( Value: TBitmap ); virtual;
    procedure SetLowerLeft( Value: TBitmap ); virtual;
    procedure SetUpperLeft( Value: TBitmap ); virtual;
    procedure SetUpperRight( Value: TBitmap ); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property LowerRight: TBitmap
      read FLowerRight
      write SetLowerRight;

    property LowerLeft: TBitmap
      read FLowerLeft
      write SetLowerLeft;

    property UpperLeft: TBitmap
      read FUpperLeft
      write SetUpperLeft;

    property UpperRight: TBitmap
      read FUpperRight
      write SetUpperRight;

    property TransparentColor: TColor
      read FTransParentColor
      write FTransParentColor;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;
  end;

  TRzBalloonHints = class;

  {===========================================}
  {== TRzCustomHintWindow Class Declaration ==}
  {===========================================}

  TRzCustomHintWindow = class( THintWindow )
  private
    FHintActive: Boolean;
    FApplication: TApplication;
    FBalloonHints: TRzBalloonHints;
    FBitmaps: PRzBalloonBitmaps;
    FCaption: string;
    FCaptionWidth: Integer;
    FColor: TColor;
    FFrameColor: TColor;
    FHintInfo: THintInfo;
    FFont: TFont;
    FAlignment: TAlignment;
    FCorner: TRzHintCorner;
    FShowBalloon: Boolean;
    FCenterThreshold: Integer;
    FDrawCorner: TRzHintCorner;
    FHintRect: TRect;

    FOnSetHintWinSize: TSetHintWinSizeEvent;
    FOnSetHintRect: TSetHintRectEvent;

    { Message Handling Methods }
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
    procedure WMNCCalcSize( var Msg: TWMNCCalcSize ); message wm_NCCalcSize;
  protected
    procedure Paint; override;
    procedure NCPaint( DC: HDC ); override;

    function GetHintRegion: HRgn;

    procedure AdjustBoundsForPointer( var Bounds: TRect; Shrink: Boolean );
    procedure DrawHintText( Canvas: TCanvas; Bounds: TRect );
    procedure GetPointerCoordinates( Bounds, HintRect: TRect;
                                     var Pt1, Pt2, Pt3: TPoint;
                                     var FillLineRect: TRect );
    procedure DrawHint( Canvas: TCanvas; Bounds: TRect );
    procedure DrawBitmapBalloon( Canvas: TCanvas );

    procedure WndProc( var msg: TMessage ); override;

    { Property Access Methods }
    procedure SetColor( Value: TColor );
    procedure SetFont( Value: TFont );
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure ActivateHint( Rect: TRect; const AHint: string ); override;
    procedure DoShowHint( var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo );
    procedure BitmapChanged( Sender: TObject );

    property Bitmaps: PRzBalloonBitmaps
      read FBitmaps
      write FBitmaps;
  published
    property Color: TColor
      read FColor
      write SetColor;

    property CaptionWidth: Integer
      read FCaptionWidth
      write FCaptionWidth;

    property Caption: string
      read FCaption
      write FCaption;

    property CenterThreshold: Integer
      read FCenterThreshold
      write FCenterThreshold
      default 80;

    property Font: TFont
      read FFont
      write SetFont;

    property FrameColor: TColor
      read FFrameColor
      write FFrameColor;

    property Alignment: TAlignment
      read FAlignment
      write FAlignment;

    property Corner: TRzHintCorner
      read FCorner
      write FCorner;

    property ShowBalloon: Boolean
      read FShowBalloon
      write FShowBalloon;
      
    property OnSetHintWinSize: TSetHintWinSizeEvent
      read FOnSetHintWinSize
      write FOnSetHintWinSize;

    property OnSetHintRect: TSetHintRectEvent
      read FOnSetHintRect
      write FOnSetHintRect;

    property Canvas;
  end;

  {=======================================}
  {== TRzBalloonHints Class Declaration ==}
  {=======================================}

  TRzBalloonHints = class( TComponent )
  private
    FAboutInfo: TRzAboutInfo;
    FHintWindow: TRzCustomHintWindow;
    FOrigHintWindowClass: THintWindowClass;

    FBitmaps: TRzBalloonBitmaps;
    FCaptionWidth: Integer;
    FColor: TColor;
    FFrameColor: TColor;
    FFont: TFont;
    FAlignment: TAlignment;
    FCorner: TRzHintCorner;
    FShowBalloon: Boolean;
    FHintPause: Integer;
    FHintShortPause: Integer;
    FCenterThreshold: Integer;

    FOnSetHintWinSize: TSetHintWinSizeEvent;
    FOnSetHintRect: TSetHintRectEvent;
    FOnShowHint: TShowHintEvent;

    { Internal Event Handlers }
    procedure FontChanged( Sender: TObject );
  protected
    procedure DefineProperties( Filer: TFiler ); override;

    { Property Access Methods }
    procedure SetFont( Value: TFont ); virtual;
    procedure SetCenterThreshold( Value: Integer ); virtual;
    procedure SetColor( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetCaptionWidth( Value: Integer ); virtual;
    procedure SetAlignment( Value: TAlignment ); virtual;
    procedure SetCorner( Value: TRzHintCorner ); virtual;
    procedure SetShowBalloon( Value: Boolean ); virtual;
    procedure SetOnSetHintWinSize( Value: TSetHintWinSizeEvent ); virtual;
    procedure SetOnSetHintRect( Value: TSetHintRectEvent ); virtual;

    function GetHintPause: Integer; virtual;
    procedure SetHintPause( Value: Integer ); virtual;
    function GetHintShortPause: Integer; virtual;
    procedure SetHintShortPause( Value: Integer ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure MakeConnection( HintWindow: TRzCustomHintWindow );
    procedure BreakConnection( HintWindow: TRzCustomHintWindow );

    property HintWindow: TRzCustomHintWindow
      read FHintWindow;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Alignment: TAlignment
      read FAlignment
      write SetAlignment
      default taLeftJustify;

    property Bitmaps: TRzBalloonBitmaps
      read FBitmaps
      write FBitmaps;

    property CaptionWidth: Integer
      read FCaptionWidth
      write SetCaptionWidth
      default 100;

    property CenterThreshold: Integer
      read FCenterThreshold
      write SetCenterThreshold
      default 80;

    property Color: TColor
      read FColor
      write SetColor
      default clInfoBk;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      default clInfoText;

    property Corner: TRzHintCorner
      read FCorner
      write SetCorner
      default hcUpperRight;

    property Font: TFont
      read FFont
      write SetFont;

    property HintPause: Integer
      read GetHintPause
      write SetHintPause
      default 500;

    property HintShortPause: Integer
      read GetHintShortPause
      write SetHintShortPause
      default 0;

    property ShowBalloon: Boolean
      read FShowBalloon
      write SetShowBalloon
      default True;

    property OnSetHintWinSize: TSetHintWinSizeEvent
      read FOnSetHintWinSize
      write SetOnSetHintWinSize;

    property OnSetHintRect: TSetHintRectEvent
      read FOnSetHintRect
      write SetOnSetHintRect;

    property OnShowHint: TShowHintEvent
      read FOnShowHint
      write FOnShowHint;
  end;


implementation

uses
  {&RAS}
  Types;


{&RT}
{=================================}
{== TRzCustomHintWindow Methods ==}
{=================================}

constructor TRzCustomHintWindow.Create( AOwner: TComponent );
var
  I, J: Integer;
  F: TForm;
  {$IFNDEF UNICODE}
  S: string[ 255 ];
  {$ELSE}
  S: string;
  {$ENDIF}
begin
  inherited;

  if AOwner = nil then
    AOwner := Application;

  if AOwner is TApplication then
  begin
    FApplication := TApplication( AOwner );
    FApplication.OnShowHint := DoShowHint;
  end;

  FHintActive := False;
  FCaptionWidth := 100;
  FCorner := hcUpperRight;
  FCenterThreshold := 80;

  FFont := TFont.Create;
  FFont.Name := 'Tohoma';
  FFont.Size := 8;

  Canvas.Font := FFont;
  Canvas.Brush.Style := bsClear;

  FFrameColor := clInfoText;

  FBalloonHints := nil;
  FBitmaps := nil;

  // Find The TRzBalloonHints component and make a connection with it so we can use it's properties

  if AOwner is TApplication then
  begin
    if not ( csDesigning in ComponentState ) then
    begin
      for I := 0 to FApplication.ComponentCount - 1 do
      begin
        S := FApplication.Components[ I ].ClassName;
        if FApplication.Components[ I ] is TForm then
        begin
          F := TForm( FApplication.Components[ I ] );
          for J := 0 to F.ComponentCount - 1 do
          begin
            S := F.Components[ J ].ClassName;
            if F.Components[ J ] is TRzBalloonHints then
            begin
              FBalloonHints := TRzBalloonHints( F.Components[ J ] );
              FBalloonHints.MakeConnection( Self );
            end;
          end;
        end;
      end;
    end;
  end;
end; {= TRzCustomHintWindow.Create =}


destructor TRzCustomHintWindow.Destroy;
begin
  FFont.Free;

  if FBalloonHints <> nil then
    FBalloonHints.BreakConnection( self );
  FBalloonHints := nil;

  inherited;
end;

procedure TRzCustomHintWindow.WndProc( var Msg: TMessage );
var
  R: TRect;
  P: TPoint;
begin
  case Msg.Msg of
    wm_SetFocus:
      Windows.SetFocus( Msg.WParam );

    wm_MouseMove:
    begin
      if FHintActive then
      begin
        SetRect( R, FHintInfo.CursorPos.X - MouseAdj, FHintInfo.CursorPos.Y - MouseAdj,
                    FHintInfo.CursorPos.X + MouseAdj, FHintInfo.CursorPos.Y + MouseAdj );

        R.TopLeft := FHintInfo.HintControl.ClientToScreen( R.TopLeft );
        R.BottomRight := FHintInfo.HintControl.ClientToScreen( R.BottomRight );

        R.TopLeft := ScreenToClient( R.TopLeft );
        R.BottomRight := ScreenToClient( R.BottomRight );
        P.X := TWMMouseMove( Msg ).XPos;
        P.Y := TWMMouseMove( Msg ).YPos;
        if not PtInRect( R, P ) then
        begin
          FHintActive := False;
          Application.CancelHint;
        end;
      end;

      FHintInfo.HintControl.Dispatch( Msg );
    end;

    wm_LButtonDblClk, wm_LButtonDown, wm_LButtonUp,
    wm_MButtonDblClk, wm_MButtonDown, wm_MButtonUp,
    wm_RButtonDblClk, wm_RButtonDown, wm_RButtonUp:
      FHintInfo.HintControl.Dispatch( Msg );

    else
      Dispatch( Msg );
  end;
end;{= TRzCustomHintWindow.WndProc =}



procedure TRzCustomHintWindow.CMTextChanged( var Msg: TMessage );
var
  R: TRect;
  H: Integer;
  W: Integer;
begin
  inherited;
  R := Bounds( 0, 0, FCaptionWidth, 0 );

  DrawString( Canvas, Caption, R, dt_CalcRect or dt_Left or dt_WordBreak or
                                  dt_NoPrefix or DrawTextBiDiModeFlagsReadingOnly);

  if Assigned( FOnSetHintRect ) then
    FOnSetHintRect( Self, Canvas, R, Caption, FDrawCorner );

  if ( R.Right - R.Left ) >= MinWindowWidth then
    W := R.Right - R.Left
  else
    W := MinWindowWidth;

  H := R.Bottom - R.Top;

  if Assigned( FOnSetHintWinSize ) then
    FOnSetHintWinSize( Self, Canvas, W, H, Caption, FDrawCorner );

  if W < MinWindowWidth then
    W := MinWindowWidth;

  Height := H;
  Width := W;
end;{= TRzCustomHintWindow.CMTextChanged =}


procedure TRzCustomHintWindow.DoShowHint( var HintStr: string; var CanShow: Boolean;
                                          var HintInfo: THintInfo );
var
  OriginalColor: TColor;
begin
  OriginalColor := FColor;

  if FBalloonHints <> nil then
  begin
    OriginalColor := FBalloonHints.Color;
    HintInfo.HintColor := OriginalColor;
  end;

  CanShow := True;
  HintInfo.HintMaxWidth := FCaptionWidth;

  if FBalloonHints <> nil then
  begin
    if Assigned( FBalloonHints.FOnShowHint ) then
      FBalloonHints.FOnShowHint( HintStr, CanShow, HintInfo );
  end;

  FHintInfo := HintInfo;
  if HintInfo.HintColor <> OriginalColor then
    Color := HintInfo.HintColor
  else
    Color := OriginalColor;
end;


procedure TRzCustomHintWindow.ActivateHint( Rect: TRect; const AHint: string );
var
  P: TPoint;
  TopLeft: TPoint;
  W: Integer;
  H: Integer;
  Rgn: HRgn;
  M: TMonitor;
  WorkArea: TRect;
begin
  FDrawCorner := FCorner;
  Caption := AHint;

  if Assigned( FOnSetHintRect ) then
    FOnSetHintRect( Self, Canvas, Rect, Caption, FDrawCorner );

  UpdateBoundsRect( Rect );

  if ShowBalloon and ( FDrawCorner <> hcNone ) then
    InflateRect( Rect, 4, 4 )
  else
    InflateRect( Rect, 2, 2 );

  AdjustBoundsForPointer( Rect, False );

  if FHintInfo.HintControl <> nil then
  begin
    if ShowBalloon and
       ( FHintInfo.HintControl.Width <= FCenterThreshold ) and
       ( FHintInfo.HintControl.Height <= FCenterThreshold ) then
    begin
      P := Point( FHintInfo.HintControl.Width div 2, FHintInfo.HintControl.Height div 2 );
      P := FHintInfo.HintControl.ClientToScreen( P );
    end
    else
      P := FHintInfo.HintControl.ClientToScreen( FHintInfo.CursorPos );
  end
  else
    P := BoundsRect.TopLeft;

  if ShowBalloon and ( FDrawCorner <> hcNone ) then
  begin
    OffsetRect( Rect, -Rect.Left, -Rect.Top );
    OffsetRect( Rect, P.X, P.Y );
  end;

  if ( Rect.Right - Rect.Left ) >= MinWindowWidth then
    W := Rect.Right - Rect.Left
  else
    W := MinWindowWidth;

  H := Rect.Bottom - Rect.Top;

  if Assigned( FOnSetHintWinSize ) then
    FOnSetHintWinSize( Self, Canvas, W, H, Caption, FDrawCorner );

  if W < MinWindowWidth then
    W := MinWindowWidth;

  if not ShowBalloon then
    FDrawCorner := hcLowerRight;

  case FDrawCorner of
    hcLowerRight, hcNone:
    begin
      TopLeft.X := Rect.Left - 10;
      TopLeft.Y := Rect.Top;
    end;

    hcUpperRight:
    begin
      TopLeft.X := Rect.Left - 10;
      TopLeft.Y := Rect.Top - H;
    end;

    hcLowerLeft:
    begin
      TopLeft.X := Rect.Left - W + 10;
      TopLeft.Y := Rect.Top;
    end;

    hcUpperLeft:
    begin
      TopLeft.X := Rect.Left - W + 10;
      TopLeft.Y := Rect.Top - H;
    end;
  end; { case }


  M := GetMonitorContainingPoint( TopLeft );
  if M <> nil then
    WorkArea := GetMonitorWorkArea( M )
  else
  begin
    WorkArea := Screen.WorkAreaRect;
  end;

  if TopLeft.Y < WorkArea.Top then
  begin
    if FDrawCorner = hcUpperRight then
      FDrawCorner := hcLowerRight
    else if FDrawCorner = hcUpperLeft then
      FDrawCorner := hcLowerLeft;

    TopLeft.Y := Rect.Top;
  end;

  if TopLeft.Y + H > WorkArea.Bottom then
  begin
    if FDrawCorner = hcLowerRight then
      FDrawCorner := hcUpperRight
    else if FDrawCorner = hcLowerLeft then
      FDrawCorner := hcUpperLeft;

    TopLeft.Y := Rect.Top - H;
  end;

  if ( TopLeft.X + W ) > WorkArea.Right then
  begin
    if FDrawCorner = hcLowerRight then
      FDrawCorner := hcLowerLeft
    else if FDrawCorner = hcUpperRight then
      FDrawCorner := hcUpperLeft;

    TopLeft.X := Rect.Left - W - 1;
  end;

  if TopLeft.X < WorkArea.Left then
  begin
    if FDrawCorner = hcLowerLeft then
      FDrawCorner := hcLowerRight
    else if FDrawCorner = hcUpperLeft then
      FDrawCorner := hcUpperRight;

    TopLeft.X := Rect.Left + 1;
  end;

  FHintRect := Types.Rect( TopLeft.X, TopLeft.Y, TopLeft.X + W, TopLeft.Y + H );

  UpdateBoundsRect( FHintRect );
  OffsetRect( FHintRect, -FHintRect.Left, -FHintRect.Top );

  ParentWindow := Application.Handle;
  SetWindowPos( Handle, hwnd_TopMost, TopLeft.X, TopLeft.Y, W, H,
                swp_ShowWindow or swp_NoActivate );

  if ShowBalloon then
  begin
    Rgn := GetHintRegion;
    SetWindowRgn( Handle, Rgn, True );
  end;
  Invalidate;

  FHintActive := True;
end;{= TRzCustomHintWindow.ActivateHint =}


procedure TRzCustomHintWindow.Paint;
begin
  Canvas.Brush.Color := FColor;
  Canvas.Pen.Color := clBlack;
  Canvas.Font := FFont;
  Canvas.Pen.Width := 1;

  DrawHint( Canvas, ClientRect );
end; {= TRzCustomHintWindow.Paint =}


procedure TRzCustomHintWindow.WMNCCalcSize( var Msg: TWMNCCalcSize );
begin
  inherited;
  if ShowBalloon then
    InflateRect( Msg.CalcSize_Params^.rgrc[0], 1, 1 );
end;

    
procedure TRzCustomHintWindow.NCPaint( DC: HDC );
var
  R: TRect;
  TempCanvas: TCanvas;
begin
  if not ShowBalloon then
  begin
    TempCanvas := TCanvas.Create;
    try
      TempCanvas.Handle := DC;
      R := Rect( 0, 0, Width, Height );
      RzCommon.DrawSides( TempCanvas, R, FFrameColor, FFrameColor, sdAllSides );
    finally
      TempCanvas.Free;
    end;
  end;
end;


function TRzCustomHintWindow.GetHintRegion: HRgn;
var
  R: TRect;
  Pts: array[ 0..2 ] of TPoint;
  FillLineRect: TRect;
  Rgn, PointerRgn: HRgn;
begin
  R := ClientRect;
  AdjustBoundsForPointer( R, True );

  // Create region for main part of hint
  Result := CreateRectRgn( R.Left, R.Top + 4, R.Right, R.Bottom - 4 );
  Rgn := CreateRectRgn( R.Left + 4, R.Top, R.Right - 4, R.Bottom );
  CombineRgn( Result, Result, Rgn, RGN_OR );
  DeleteObject( Rgn );

  Rgn := CreateRectRgn( R.Left + 2, R.Top + 1, R.Right - 2, R.Bottom - 1 );
  CombineRgn( Result, Result, Rgn, RGN_OR );
  DeleteObject( Rgn );

  Rgn := CreateRectRgn( R.Left + 1, R.Top + 2, R.Right - 1, R.Bottom - 2 );
  CombineRgn( Result, Result, Rgn, RGN_OR );
  DeleteObject( Rgn );

  if FDrawCorner <> hcNone then
  begin
    // Add region for pointer
    GetPointerCoordinates( ClientRect, R, Pts[ 0 ], Pts[ 1 ], Pts[ 2 ], FillLineRect );

    // Adjust Pts to account for regions not accepting right/bottom most pixels
    case FDrawCorner of
      hcUpperRight:
      begin
        Inc( Pts[ 1 ].Y );
        Inc( Pts[ 2 ].X );
      end;

      hcUpperLeft:
      begin
        Inc( Pts[ 0 ].X );
        Inc( Pts[ 1 ].X );
        Inc( Pts[ 1 ].Y );
      end;

      hcLowerRight:
      begin
        Dec( Pts[ 1 ].Y );
        Inc( Pts[ 2 ].X );
      end;

      hcLowerLeft:
      begin
        Inc( Pts[ 0 ].X );
        Inc( Pts[ 1 ].X );
        Dec( Pts[ 1 ].Y );
      end;
    end; { case }

    PointerRgn := CreatePolygonRgn( Pts, 3, WINDING );

    CombineRgn( Result, Result, PointerRgn, RGN_OR );
    DeleteObject( PointerRgn );
  end;
end;


procedure TRzCustomHintWindow.AdjustBoundsForPointer( var Bounds: TRect; Shrink: Boolean );
var
  Offset: Integer;
begin
  if ShowBalloon then
  begin
    if Shrink then
      Offset := 20
    else
      Offset := -20;

    case FDrawCorner of
      hcUpperRight, hcUpperLeft:
        Dec( Bounds.Bottom, Offset );

      hcLowerRight, hcLowerLeft:
        Inc( Bounds.Top, Offset );
    end;
  end;
end;


procedure TRzCustomHintWindow.DrawHintText( Canvas: TCanvas; Bounds: TRect );
var
  OldBkMode: Integer;
begin
  if ShowBalloon and ( FDrawCorner <> hcNone ) then
  begin
    case Alignment of
      taLeftJustify: OffsetRect( Bounds, 8, 4 );
      taRightJustify: OffsetRect( Bounds, -8, 4 );
      taCenter:
      begin
        if Bounds.Right - Bounds.Left > CaptionWidth then
          OffsetRect( Bounds, 8, 4 )
        else
          OffsetRect( Bounds, 0, 4 );
      end;
    end;
  end
  else
  begin
    case Alignment of
      taLeftJustify: OffsetRect( Bounds, 2, 2 );
      taRightJustify: OffsetRect( Bounds, -2, 2 );
      taCenter: OffsetRect( Bounds, 0, 2 )
    end;
  end;


  OldBkMode := SetBkMode( Canvas.Handle, Transparent );

  if Bounds.Right - Bounds.Left > CaptionWidth then
  begin
    if ( Alignment = taLeftJustify ) or ( Alignment = taCenter ) then
    begin
      // Need to adjust the Bounds slightly because Bounds represents the rectangle
      // for the entire hint.  We need to restrict the width to CaptionWidth.
      Bounds.Right := Bounds.Left + CaptionWidth;
    end
    else if Alignment = taRightJustify then
    begin
      Bounds.Left := Bounds.Right - CaptionWidth;
    end;
  end;

  DrawString( Canvas, Caption, Bounds,
              DrawTextAlignments[ Alignment ] or dt_NoPrefix or dt_WordBreak or
              DrawTextBiDiModeFlagsReadingOnly );

  SetBkMode( Canvas.Handle, OldBkMode );
end;


procedure TRzCustomHintWindow.GetPointerCoordinates( Bounds, HintRect: TRect;
                                                     var Pt1, Pt2, Pt3: TPoint;
                                                     var FillLineRect: TRect );
begin
  case FDrawCorner of
    hcUpperRight:
    begin
      Pt1 := Point( HintRect.Left + 10, HintRect.Bottom - 2 );
      Pt2 := Point( HintRect.Left + 10, Bounds.Bottom - 2 );
      Pt3 := Point( HintRect.Left + 30, HintRect.Bottom - 2 );

      SetRect( FillLineRect, HintRect.Left + 10, HintRect.Bottom - 3,
                             HintRect.Left + 31, HintRect.Bottom - 1 );
    end;

    hcUpperLeft:
    begin
      Pt1 := Point( HintRect.Right - 10, HintRect.Bottom - 2 );
      Pt2 := Point( HintRect.Right - 10, Bounds.Bottom - 2 );
      Pt3 := Point( HintRect.Right - 30, HintRect.Bottom - 2 );

      SetRect( FillLineRect, HintRect.Right - 9, HintRect.Bottom - 3,
                             HintRect.Right - 30, HintRect.Bottom - 1 );
    end;

    hcLowerRight:
    begin
      Pt1 := Point( HintRect.Left + 10, HintRect.Top + 1 );
      Pt2 := Point( HintRect.Left + 10, Bounds.Top + 1 );
      Pt3 := Point( HintRect.Left + 30, HintRect.Top + 1 );

      SetRect( FillLineRect, HintRect.Left + 10, HintRect.Top + 2,
                             HintRect.Left + 31, HintRect.Top + 1 );
    end;

    hcLowerLeft:
    begin
      Pt1 := Point( HintRect.Right - 10, HintRect.Top + 1 );
      Pt2 := Point( HintRect.Right - 10, Bounds.Top + 1 );
      Pt3 := Point( HintRect.Right - 30, HintRect.Top + 1 );

      SetRect( FillLineRect, HintRect.Right - 9, HintRect.Top + 2,
                             HintRect.Right - 30, HintRect.Top + 1 );
    end;
  end; { case FDrawCorner =}
end;


procedure TRzCustomHintWindow.DrawHint( Canvas: TCanvas; Bounds: TRect );
var
  HintRect, FillLineRect: TRect;
  Pt1, Pt2, Pt3: TPoint;
begin
  HintRect := Bounds;
  AdjustBoundsForPointer( HintRect, True );

  // Allow user to change text location
  if Assigned( FOnSetHintRect ) then
    FOnSetHintRect( Self, Canvas, HintRect, Caption, FDrawCorner );


  if ( FBitmaps <> nil ) and
     ( ( FBitmaps^.FLowerRight.Handle <> 0 ) or ( FBitmaps^.FLowerLeft.Handle <> 0 ) or
       ( FBitmaps^.FUpperLeft.Handle <> 0 ) or ( FBitmaps^.FUpperRight.Handle <> 0 ) ) then
  begin
    DrawBitmapBalloon( Canvas );
  end
  else if ShowBalloon then
  begin
    Canvas.Pen.Color := FFrameColor;
    Canvas.Brush.Color := FColor;
    DrawBox( Canvas, HintRect, FFrameColor );
    // Round the Upper Left corner
    Canvas.Pixels[ HintRect.Left + 1, HintRect.Top + 3 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Left + 1, HintRect.Top + 2 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Left + 2, HintRect.Top + 1 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Left + 3, HintRect.Top + 1 ] := FFrameColor;
    // Round the Upper Right corner
    Canvas.Pixels[ HintRect.Right - 4, HintRect.Top + 1 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Right - 3, HintRect.Top + 1 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Right - 2, HintRect.Top + 2 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Right - 2, HintRect.Top + 3 ] := FFrameColor;
    // Round the Lower Right corner
    Canvas.Pixels[ HintRect.Right - 2, HintRect.Bottom - 4 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Right - 2, HintRect.Bottom - 3 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Right - 3, HintRect.Bottom - 2 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Right - 4, HintRect.Bottom - 2 ] := FFrameColor;
    // Round the Lower Left corner
    Canvas.Pixels[ HintRect.Left + 3, HintRect.Bottom - 2 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Left + 2, HintRect.Bottom - 2 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Left + 1, HintRect.Bottom - 3 ] := FFrameColor;
    Canvas.Pixels[ HintRect.Left + 1, HintRect.Bottom - 4 ] := FFrameColor;

    if FDrawCorner <> hcNone then
    begin
      GetPointerCoordinates( Bounds, HintRect, Pt1, Pt2, Pt3, FillLineRect );

      Canvas.Pen.Color := FFrameColor;
      Canvas.Polygon( [ Pt1, Pt2, Pt3 ] );
      Canvas.FillRect( FillLineRect );
    end;
  end;

  DrawHintText( Canvas, HintRect );
end;{= TRzCustomHintWindow.DrawHint =}


procedure TRzCustomHintWindow.DrawBitmapBalloon( Canvas: TCanvas );
var
  TmpBitmap: TBitmap;
  Src: TRect;

  function GetTmpBitmap( var Src: TRect ): TBitmap;
  begin
    Result := nil;
    if FBitmaps^.UpperRight.Handle <> 0 then
    begin
      Result := FBitmaps^.UpperRight;
      case FDrawCorner of
        hcUpperLeft: Src := Rect( Result.Width - 1, 0, 0, Result.Height );
        hcLowerRight: Src := Rect( 0, Result.Height - 1, Result.Width, 0 );
        hcLowerLeft: Src := Rect( Result.Width - 1, Result.Height - 1, 0, 0 );
      end;
    end
    else if FBitmaps^.UpperLeft.Handle <> 0 then
    begin
      Result := FBitmaps^.UpperLeft;
      case FDrawCorner of
        hcUpperRight: Src := Rect( Result.Width - 1, 0, 0, Result.Height );
        hcLowerRight: Src := Rect( Result.Width - 1, Result.Height - 1, 0, 0 );
        hcLowerLeft: Src := Rect( 0, Result.Height - 1, Result.Width, 0 );
      end;
    end
    else if FBitmaps^.LowerRight.Handle <> 0 then
    begin
      Result := FBitmaps^.LowerRight;
      case FDrawCorner of
        hcUpperRight: Src := Rect( 0, Result.Height - 1, Result.Width, 0 );
        hcUpperLeft: Src := Rect( Result.Width - 1, Result.Height - 1, 0, 0 );
        hcLowerLeft: Src := Rect( Result.Width - 1, 0, 0, Result.Height );
      end;
    end
    else if FBitmaps^.LowerLeft.Handle <> 0 then
    begin
      Result := FBitmaps^.LowerLeft;
      case FDrawCorner of
        hcUpperRight: Src := Rect( Result.Width - 1, Result.Height - 1, 0, 0 );
        hcUpperLeft: Src := Rect( 0, Result.Height - 1, Result.Width, 0 );
        hcLowerRight: Src := Rect( Result.Width - 1, 0, 0, Result.Height );
      end;
    end;
  end; {= GetTmpBitmap =}


begin
  TmpBitmap := nil;

  case FDrawCorner of
    hcUpperRight:
    begin
      if FBitmaps^.UpperRight.Handle <> 0 then
      begin
        TmpBitmap := FBitmaps^.UpperRight;
        Src := Bounds( 0, 0, TmpBitmap.width, TmpBitmap.Height );
      end
      else
        TmpBitmap := GetTmpBitmap( Src );
    end;

    hcUpperLeft:
    begin
      if FBitmaps^.UpperLeft.Handle <> 0 then
      begin
        TmpBitmap := FBitmaps^.UpperLeft;
        Src := Bounds( 0, 0, TmpBitmap.width, TmpBitmap.Height );
      end
      else
        TmpBitmap := GetTmpBitmap( Src );
    end;

    hcLowerRight:
    begin
      if FBitmaps^.LowerRight.Handle <> 0 then
      begin
        TmpBitmap := FBitmaps^.LowerRight;
        Src := Bounds( 0, 0, TmpBitmap.width, TmpBitmap.Height );
      end
      else
        TmpBitmap := GetTmpBitmap( Src );
    end;

    hcLowerLeft:
    begin
      if FBitmaps^.LowerLeft.Handle <> 0 then
      begin
        TmpBitmap := FBitmaps^.LowerLeft;
        Src := Bounds( 0, 0, TmpBitmap.width, TmpBitmap.Height );
      end
      else
        TmpBitmap := GetTmpBitmap( Src );
    end;
  end; { case FDrawCorner }

  Canvas.CopyMode := SrcCopy;
  SetBkMode( Canvas.Handle, OPAQUE );
  SetBkColor( Canvas.Handle, clWhite );

  DrawTransparentBitmap( Canvas, TmpBitmap, ClientRect, Src, FBitmaps^.TransparentColor );

end;{= TRzCustomHintWindow.DrawBitmapBalloon =}


procedure TRzCustomHintWindow.SetColor( Value: TColor );
begin
  FColor := Value;
  inherited Color := Value;
end;


procedure TRzCustomHintWindow.SetFont( Value: TFont );
begin
  FFont.Assign( Value );
  Canvas.Font := Value;
end;


procedure TRzCustomHintWindow.BitmapChanged( Sender: TObject );
begin
  if Visible then
    Invalidate;
end;


{===============================}
{== TRzBalloonBitmaps Methods ==}
{===============================}

constructor TRzBalloonBitmaps.Create;
begin
  FLowerRight := TBitmap.Create;
  FLowerLeft := TBitmap.Create;
  FUpperLeft := TBitmap.Create;
  FUpperRight := TBitmap.Create;
  FTransparentColor := clOlive;
end;

destructor TRzBalloonBitmaps.Destroy;
begin
  FLowerRight.Free;
  FLowerLeft.Free;
  FUpperLeft.Free;
  FUpperRight.Free;
  inherited;
end;

procedure TRzBalloonBitmaps.SetLowerRight( Value: TBitmap );
begin
  if FLowerRight <> Value then
  begin
    FLowerRight.Assign( Value );
    if Assigned( FOnChange ) then
      FOnChange( Self );
  end;
end;

procedure TRzBalloonBitmaps.SetLowerLeft( Value: TBitmap );
begin
  if FLowerLeft <> Value then
  begin
    FLowerLeft.Assign( Value );
    if Assigned( FOnChange ) then
      FOnChange( Self );
  end;
end;

procedure TRzBalloonBitmaps.SetUpperLeft( Value: TBitmap );
begin
  if FUpperLeft <> Value then
  begin
    FUpperLeft.Assign( Value );
    if Assigned( FOnChange ) then
      FOnChange( Self );
  end;
end;

procedure TRzBalloonBitmaps.SetUpperRight( Value: TBitmap );
begin
  if FUpperRight <> Value then
  begin
    FUpperRight.Assign( Value );
    if Assigned( FOnChange ) then
      FOnChange( Self );
  end;
end;


{=============================}
{== TRzBalloonHints Methods ==}
{=============================}

constructor TRzBalloonHints.Create( AOwner: TComponent );
var
  OrigVal: Boolean;
begin
  inherited;

  OrigVal := Application.ShowHint;
  FOrigHintWindowClass := HintWindowClass;
  FCaptionWidth := 100;

  FColor := clInfoBk;
  FFrameColor := cl3DDkShadow;

  FAlignment := taLeftJustify;
  FCorner := hcUpperRight;
  FShowBalloon := True;
  FBitmaps := TRzBalloonBitmaps.Create;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FFont.Size := 8;
  FFont.Color := clInfoText;
  FFont.OnChange := FontChanged;

  FHintPause := Application.HintPause;
  FHintShortPause := Application.HintShortPause;

  if not ( csDesigning in ComponentState ) then
  begin
    Application.ShowHint := False;
    HintWindowClass := TRzCustomHintWindow;
    Application.ShowHint := OrigVal;
  end;

  {&RCI}
end; {= TRzBalloonHints.Create =}


destructor TRzBalloonHints.Destroy;
var
  OrigVal: Boolean;
begin
  if not ( csDestroying in Application.ComponentState ) then
  begin
    OrigVal := Application.ShowHint;
    Application.ShowHint := False;
    HintWindowClass := FOrigHintWindowClass;
    Application.ShowHint := OrigVal;
  end;
  FBitmaps.Free;
  FFont.Free;
  inherited;
end;


procedure TRzBalloonHints.MakeConnection( HintWindow: TRzCustomHintWindow );
begin
  FHintWindow := HintWindow;
  FHintWindow.CaptionWidth := FCaptionWidth;
  FHintWindow.Color := FColor;
  FHintWindow.Corner := FCorner;
  FHintWindow.ShowBalloon := FShowBalloon;
  FHintWindow.Alignment := Alignment;
  FHintWindow.Bitmaps := @FBitmaps;

  FHintWindow.FOnSetHintWinSize := FOnSetHintWinSize;
  FHintWindow.FOnSetHintRect := FOnSetHintRect;

  if FFont <> nil then
    FHintWindow.Font := FFont;
  {&RV}
end;


procedure TRzBalloonHints.BreakConnection( HintWindow: TRzCustomHintWindow );
begin
  FHintWindow := nil;
end;


procedure TRzBalloonHints.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the BalloonStyle was published in version 2.x
  Filer.DefineProperty( 'BalloonStyle', TRzOldPropReader.ReadOldEnumProp, nil, False );
  // Handle the fact that the Shadow was published in version 4.0.3 and earlier
  Filer.DefineProperty( 'Shadow', TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzBalloonHints.FontChanged( Sender: TObject );
begin
  if FHintWindow <> nil then
    FHintWindow.Font := FFont;
end;

procedure TRzBalloonHints.SetFont( Value: TFont );
begin
  FFont.Assign( Value );
  if FHintWindow <> nil then
    FHintWindow.Font := Value;
end;


procedure TRzBalloonHints.SetCenterThreshold( Value: Integer );
begin
  FCenterThreshold := Value;
  if FHintWindow <> nil then
    FHintWindow.CenterThreshold := Value;
end;


procedure TRzBalloonHints.SetColor( Value: TColor );
begin
  FColor := Value;
  if FHintWindow <> nil then
    FHintWindow.Color := Value;
end;


procedure TRzBalloonHints.SetFrameColor( Value: TColor );
begin
  FFrameColor := Value;
  if FHintWindow <> nil then
    FHintWindow.FrameColor := Value;
end;


procedure TRzBalloonHints.SetCaptionWidth( Value: Integer );
begin
  FCaptionWidth := Value;
  if FHintWindow <> nil then
    FHintWindow.CaptionWidth := Value;
end;

procedure TRzBalloonHints.SetAlignment( Value: TAlignment );
begin
  FAlignment := Value;
  if FHintWindow <> nil then
    FHintWindow.Alignment := Value;
end;


procedure TRzBalloonHints.SetCorner( Value: TRzHintCorner );
begin
  FCorner := Value;
  if FHintWindow <> nil then
    FHintWindow.Corner := Value;
end;

procedure TRzBalloonHints.SetShowBalloon( Value: Boolean );
begin
  FShowBalloon := Value;
  if FHintWindow <> nil then
    FHintWindow.ShowBalloon := Value;
end;


procedure TRzBalloonHints.SetOnSetHintWinSize( Value: TSetHintWinSizeEvent );
begin
  FOnSetHintWinSize := Value;
  if FHintWindow <> nil then
    FHintWindow.OnSetHintWinSize := Value;
end;

procedure TRzBalloonHints.SetOnSetHintRect( Value: TSetHintRectEvent );
begin
  FOnSetHintRect := Value;
  if FHintWindow <> nil then
    FHintWindow.OnSetHintRect := Value;
end;


function TRzBalloonHints.GetHintPause: Integer;
begin
  if not ( csDesigning in ComponentState ) then
    Result := Application.HintPause
  else
    Result := FHintPause;
end;

procedure TRzBalloonHints.SetHintPause( Value: Integer );
begin
  if not ( csDesigning in ComponentState ) then
    Application.HintPause := Value
  else
    FHintPause := Value;
end;


function TRzBalloonHints.GetHintShortPause: Integer;
begin
  if not ( csDesigning in ComponentState ) then
    Result := Application.HintShortPause
  else
    Result := FHintShortPause;
end;

procedure TRzBalloonHints.SetHintShortPause( Value: Integer );
begin
  if not ( csDesigning in ComponentState ) then
    Application.HintShortPause := Value
  else
    FHintShortPause := Value;;
end;


{&RUIF}
end.

