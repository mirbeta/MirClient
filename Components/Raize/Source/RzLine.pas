{===============================================================================
  RzLine Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzLine
    Graphic control that display a single line between opposite corners of the
    bounding rectangle. Arrows on either end optional.  OnClick event only
    generated when user clicks on the line, not the bounding rectangle.


  Modification History
  ------------------------------------------------------------------------------
  6.1.2  (22 Feb 2013)
    * Fixed issue in TRzLine where changing the caption may not immediately
      update the display of the control.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzLine to fully support VCL Styles 
      introduced in RAD Studio XE2.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzLine control.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new Caption property to TRzLine. The Caption is displayed over the
      center of the line.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzLine to
      account for changes introduced in Borland Developer Studio 2006.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Added Align, Anchors, and Constraints properties.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Initial release.
===============================================================================}

{$I RzComps.inc}

unit RzLine;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Classes,
  {&RF}
  Controls,
  Graphics,
  Messages,
  SysUtils,
  Windows,
  RzCommon;

type
  {===============================}
  {== TRzLine Class Declaration ==}
  {===============================}

  TRzLineSlope = ( lsDown, lsUp );
  TRzShowArrows = ( saNone, saStart, saEnd, saBoth );

  TRzLine = class( TGraphicControl )
  private
    FAboutInfo: TRzAboutInfo;
    FBorderWidth: Integer;
    FStartPoint: TPoint;
    FEndPoint: TPoint;
    FLineColor: TColor;
    FLineSlope: TRzLineSlope;
    FLineStyle: TPenStyle;
    FLineWidth: Integer;
    FArrowLength: Integer;
    FShowArrows: TRzShowArrows;

    procedure CMHitTest( var Msg: TCMHitTest ); message cm_HitTest;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure CMTextChanged( var Msg: TMessage ); message cm_TextChanged;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure UpdateSize;
    procedure Resize; override;

    function GetYFromX( X: Integer; Offset: TPoint ): Integer;
    function PointOnLine( P: TPoint ): Boolean;
    procedure SetEndPoints;


    { Property Access Methods }
    procedure SetArrowLength( Value: Integer ); virtual;
    procedure SetLineColor( Value: TColor ); virtual;
    procedure SetLineSlope( Value: TRzLineSlope ); virtual;
    procedure SetLineStyle( Value: TPenStyle ); virtual;
    procedure SetLineWidth( Value: Integer ); virtual;
    procedure SetShowArrows( Value: TRzShowArrows ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property ArrowLength: Integer
      read FArrowLength
      write SetArrowLength
      default 10;

    property LineColor: TColor
      read FLineColor
      write SetLineColor
      default clWindowText;

    property LineSlope: TRzLineSlope
      read FLineSlope
      write SetLineSlope
      default lsDown;

    property LineStyle: TPenStyle
      read FLineStyle
      write SetLineStyle
      default psSolid;

    property LineWidth: Integer
      read FLineWidth
      write SetLineWidth
      default 1;

    property ShowArrows: TRzShowArrows
      read FShowArrows
      write SetShowArrows
      default saNone;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Caption;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
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
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

{&RUAS}

{&RT}
{=====================}
{== TRzLine Methods ==}
{=====================}

constructor TRzLine.Create( AOwner: TComponent );
begin
  inherited;

  FBorderWidth := 10;
  FLineSlope := lsDown;
  SetEndPoints;
  {&RCI}

  FLineColor := clWindowText;
  FLineWidth := 1;
  FLineStyle := psSolid;
  FArrowLength := 10;
  FShowArrows := saNone;
end;


procedure TRzLine.Loaded;
begin
  inherited;
  SetEndPoints;
end;


procedure TRzLine.Paint;
var
  Theta, Alpha, Beta: Extended;
  A, B, SP, EP: TPoint;
begin
  // Theta is the slope of the line
  SP := FStartPoint;
  EP := FEndPoint;

  if EP.X <> SP.X then
    Theta := ArcTan( ( EP.Y - SP.Y ) / ( EP.X - SP.X ) )
  else
    Theta := Pi / 2;

  if FShowArrows <> saNone then
  begin
    // Adjust End Points if there are Arrows
    A.X := Round( ( FArrowLength div 2 ) * Cos( Theta ) );
    A.Y := Round( ( FArrowLength div 2 ) * Sin( Theta ) );

    if ( FLineSlope = lsUp ) and ( Theta = Pi / 2 ) then
      A := Point( -A.X, -A.Y );

    if ( FShowArrows = saStart ) or ( FShowArrows = saBoth ) then
    begin
      Inc( SP.X, A.X );
      Inc( SP.Y, A.Y );
    end;

    if ( FShowArrows = saEnd ) or ( FShowArrows = saBoth ) then
    begin
      Dec( EP.X, A.X );
      Dec( EP.Y, A.Y );
    end;
  end;

  // This is needed so that correct background color shows through for non-solid LineStyles
  Canvas.Brush.Style := bsClear;

  Canvas.Pen.Color := ActiveStyleSystemColor( FLineColor );
  Canvas.Pen.Style := FLineStyle;
  Canvas.Pen.Width := FLineWidth;

  // Draw Line
  Canvas.MoveTo( SP.X, SP.Y );
  Canvas.LineTo( EP.X, EP.Y );

  // Draw Arrows
  if FShowArrows <> saNone then
  begin
    Alpha := Theta - ( Pi / 8 );
    Beta := Theta + ( Pi / 8 );

    A.X := Round( FArrowLength * Cos( Alpha ) );
    A.Y := Round( FArrowLength * Sin( Alpha ) );
    B.X := Round( FArrowLength * Cos( Beta ) );
    B.Y := Round( FArrowLength * Sin( Beta ) );

    if ( FLineSlope = lsUp ) and ( Theta = Pi / 2 ) then
    begin
      A := Point( -A.X, -A.Y );
      B := Point( -B.X, -B.Y );
    end;

    Canvas.Brush.Color := ActiveStyleSystemColor( FLineColor );
    Canvas.Pen.Width := 1;

    if ( FShowArrows = saStart ) or ( FShowArrows = saBoth ) then
    begin
      Canvas.Polygon( [ Point( FStartPoint.X, FStartPoint.Y ),
                        Point( FStartPoint.X + B.X, FStartPoint.Y + B.Y ),
                        Point( FStartPoint.X + A.X, FStartPoint.Y + A.Y ) ] );
    end;

    if ( FShowArrows = saEnd ) or ( FShowArrows = saBoth ) then
    begin
      Canvas.Polygon( [ Point( FEndPoint.X, FEndPoint.Y ),
                        Point( FEndPoint.X - B.X, FEndPoint.Y - B.Y ),
                        Point( FEndPoint.X - A.X, FEndPoint.Y - A.Y ) ] );
    end;
  end; { if FShowArrows <> saNone }

  if Caption <> '' then
  begin
    Canvas.Font := Font;
    Canvas.Font.Color := ActiveStyleSystemColor( Font.Color );
    Canvas.Brush.Color := ActiveStyleSystemColor( Color );
    DrawStringCentered( Canvas, Caption, ClientRect );
  end;
end; {= TRzLine.Paint =}


function TRzLine.GetYFromX( X: Integer; Offset: TPoint ): Integer;
begin
  Result := Round( ( ( FEndPoint.Y - FStartPoint.Y ) / ( FEndPoint.X - FStartPoint.X ) * ( X - FStartPoint.X + Offset.X ) ) +
                   ( FStartPoint.Y + Offset.Y ) );
end;


function TRzLine.PointOnLine( P: TPoint ): Boolean;
var
  Y1, Y2, Threshold: Integer;
  R: TRect;

  procedure Swap( var A, B: Integer );
  var
    Temp: Integer;
  begin
    Temp := A;
    A := B;
    B := Temp;
  end;

begin {= TRzLine.PointOnLine =}
  Threshold := FLineWidth div 2;
  if Threshold < 4 then
    Threshold := 4;

  // Check to see if P is in the border area
  R := ClientRect;
  InflateRect( R, -FBorderWidth + Threshold, -FBorderWidth + Threshold );
  if not PtInRect( R, P ) then
  begin
    Result := False;
    Exit;
  end;

  if FStartPoint.X <> FEndPoint.X then
  begin
    case FLineSlope of
      lsDown:
      begin
        Y1 := GetYFromX( P.X, Point( Threshold, -Threshold ) );
        Y2 := GetYFromX( P.X, Point( -Threshold, Threshold ) );

      end;

      lsUp:
      begin
        Y1 := GetYFromX( P.X, Point( -Threshold, -Threshold ) );
        Y2 := GetYFromX( P.X, Point( Threshold, Threshold ) );
      end;
    end;

    if Y2 < Y1 then
      Swap( Y1, Y2 );

    Result := ( P.Y >= Y1 ) and ( P.Y <= Y2 );
  end
  else
  begin
    // Must be a vertical line
    Result := Abs( P.X - FStartPoint.X ) <= Threshold;
  end;
end; {= TRzLine.PointOnLine =}


procedure TRzLine.CMHitTest( var Msg: TCMHitTest );
begin
  // Need to determine if P is on the actual line
  if PointOnLine( Point( Msg.XPos, Msg.YPos ) ) then
    Msg.Result := HTCLIENT
  else
    Msg.Result := HTNOWHERE;
end;


procedure TRzLine.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  {&RV}
end;


procedure TRzLine.CMMouseLeave( var Msg: TMessage );
begin
  inherited;
end;


procedure TRzLine.CMTextChanged( var Msg: TMessage );
begin
  inherited;
  Invalidate;
end;


procedure TRzLine.UpdateSize;
begin
  if Width < ( 2 * FBorderWidth ) then
    Width := 2 * FBorderWidth;
  if Height < ( 2 * FBorderWidth ) then
    Height := 2 * FBorderWidth;
end;


procedure TRzLine.Resize;
begin
  inherited;
  UpdateSize;
  {&RV}
  SetEndPoints;
end;


procedure TRzLine.SetEndPoints;
begin
  case FLineSlope of
    lsDown:
    begin
      FStartPoint := Point( FBorderWidth, FBorderWidth );
      FEndPoint := Point( Width - FBorderWidth, Height - FBorderWidth );
    end;

    lsUp:
    begin
      FStartPoint := Point( FBorderWidth, Height - FBorderWidth );
      FEndPoint := Point( Width - FBorderWidth, FBorderWidth );
    end;
  end;
end;


procedure TRzLine.SetArrowLength( Value: Integer );
begin
  if FArrowLength <> Value then
  begin
    FArrowLength := Value;
    FBorderWidth := Value div 2;
    UpdateSize;
    SetEndPoints;
    Invalidate;
  end;
end;


procedure TRzLine.SetLineColor( Value: TColor );
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;


procedure TRzLine.SetLineSlope( Value: TRzLineSlope );
begin
  if FLineSlope <> Value then
  begin
    FLineSlope := Value;
    SetEndPoints;
    Invalidate;
  end;
end;


procedure TRzLine.SetLineStyle( Value: TPenStyle );
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Invalidate;
  end;
end;


procedure TRzLine.SetLineWidth( Value: Integer );
begin
  if FLineWidth <> Value then
  begin
    if FShowArrows <> saNone then
    begin
      if Value mod 2 = 0 then
      begin
        if FLineWidth < Value then
          Inc( Value )
        else
          Dec( Value );
      end;
    end;
    FLineWidth := Value;
    Invalidate;
  end;
end;


procedure TRzLine.SetShowArrows( Value: TRzShowArrows );
begin
  if FShowArrows <> Value then
  begin
    FShowArrows := Value;
    Invalidate;
  end;
end;


{&RUIF}
end.
