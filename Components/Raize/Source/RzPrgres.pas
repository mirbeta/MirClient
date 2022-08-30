{===============================================================================
  RzPrgres Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzProgressBar
    Custom graphic control used to display progress information (percentage)


  Modification History
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzProgressBar control.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Added new ShowParts property to TRzProgressBar. When this property is set
      to True and ShowPercent is set to True, the text representation of the
      percentage is displayed as a ratio of the values of PartsComplete and
      TotalParts properties.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Fixed problem where TRzProgressBar and descendants would not pick up the
      correct settings when connected to a TRzFrameController in Delphi 5.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new bsGradient value to TRzProgressBar.BarStyle enumeration. When
      selected, the progress bar and background are drawn using gradients. The
      colors of the gradients are determined by the BarColor, BarColorStop,
      BackColor, and BackColorStop properties.  The style of gradient is
      controlled by the new GradientDirection property.
    * Added new FrameControllerNotifications property to TRzProgressBar.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where ThemeAware property would not be honored unless
      component was parented by a TRzStatusBar.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Added FrameController property, which can be used to control the frame
      that appears around the progress bar.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Fixed problem where fsFlatBold borders were not drawn with FlatColor
      value.
    * Added fsFlatRounded to inner and outer border styles.
    * Refactored inner and outer border painting to common DrawInnerOuterBorders
      function.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Fixed painting problem of rounded corners of progress bar when used as a
      status pane.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added FlatColor and FlatColorAdjustment property.
===============================================================================}

{$I RzComps.inc}

unit RzPrgres;

interface

uses
  {&RF}
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Classes,
  Graphics,
  Controls,
  Menus,
  ExtCtrls,
  RzCommon;

type
  TSegmentRange = 1..100;
  TUnsignedLongint = 0..MaxLongint;
  TProgressChangeEvent = procedure( Sender: TObject; Percent: Integer ) of object;

  TRzCustomProgressBar = class( TGraphicControl, IRzCustomFramingNotification )
  private
    FBackColor: TColor;
    FBackColorStop: TColor;
    FBarColor: TColor;
    FBarColorStop: TColor;
    FBarStyle: TBarStyle;
    FBevelWidth: TBevelWidth;
    FBorderColor: TColor;
    FBorderInner: TFrameStyleEx;
    FBorderOuter: TFrameStyleEx;
    FBorderWidth: TBorderWidth;
    FFlatColor: TColor;
    FFlatColorAdjustment: Integer;
    FGradientDirection: TGradientDirection;
    FInteriorOffset: Byte;
    FNumSegments: TSegmentRange;
    FOrientation: TOrientation;
    FPartsComplete: TUnsignedLongint;
    FPercent: Word;
    FShowPercent: Boolean;
    FShowParts: Boolean;
    FTotalParts: TUnsignedLongint;
    FThemeAware: Boolean;
    FFrameController: TRzFrameController;
    FFrameControllerNotifications: TRzFrameControllerNotifications;

    FOnChange: TProgressChangeEvent;

    { Message Handling Methods }
    procedure CMHitTest( var Msg: TMessage ); message cm_HitTest;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure Paint; override;

    procedure CustomFramingChanged; virtual;

    { Event Dispatch Methods }
    procedure PercentChanged; dynamic;

    { Property Access Methods }
    procedure SetBackColor( Value: TColor ); virtual;
    procedure SetBackColorStop( Value: TColor );
    procedure SetBarColor( Value: TColor ); virtual;
    procedure SetBarColorStop( Value: TColor );
    procedure SetBarStyle( Value: TBarStyle ); virtual;
    procedure SetBevelWidth( Value: TBevelWidth ); virtual;
    procedure SetBorderColor( Value: TColor ); virtual;
    procedure SetBorderInner( Value: TFrameStyleEx ); virtual;
    procedure SetBorderOuter( Value: TFrameStyleEx ); virtual;
    procedure SetBorderWidth( Value: TBorderWidth ); virtual;
    procedure SetFlatColor( Value: TColor ); virtual;
    procedure SetFlatColorAdjustment( Value: Integer ); virtual;
    procedure SetFrameController( Value: TRzFrameController ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection );
    procedure SetInteriorOffset( Value: Byte ); virtual;
    procedure SetNumSegments( Value: TSegmentRange ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
    procedure SetPartsComplete( Value: TUnsignedLongint ); virtual;
    procedure SetPercent( Value: Word ); virtual;
    procedure SetShowPercent( Value: Boolean ); virtual;
    procedure SetShowParts( Value: Boolean ); virtual;
    procedure SetTotalParts( Value: TUnsignedLongint ); virtual;
    procedure SetThemeAware( Value: Boolean ); virtual;

    { Property Declarations }
    property BackColor: TColor
      read FBackColor
      write SetBackColor
      default clWhite;

    property BackColorStop: TColor
      read FBackColorStop
      write SetBackColorStop
      default clBtnFace;

    property BarColor: TColor
      read FBarColor
      write SetBarColor
      default clHighlight;

    property BarColorStop: TColor
      read FBarColorStop
      write SetBarColorStop
      default clHotLight;

    property BarStyle: TBarStyle
      read FBarStyle
      write SetBarStyle
      default bsTraditional;

    property BevelWidth: TBevelWidth
      read FBevelWidth
      write SetBevelWidth
      default 1;

    property BorderColor: TColor
      read FBorderColor
      write SetBorderColor
      default clBtnFace;

    property BorderInner: TFrameStyleEx
      read FBorderInner
      write SetBorderInner
      default fsNone;

    property BorderOuter: TFrameStyleEx
      read FBorderOuter
      write SetBorderOuter
      default fsLowered;

    property BorderWidth: TBorderWidth
      read FBorderWidth
      write SetBorderWidth;

    property FlatColor: TColor
      read FFlatColor
      write SetFlatColor
      default clBtnShadow;

    property FlatColorAdjustment: Integer
      read FFlatColorAdjustment
      write SetFlatColorAdjustment
      default 30;

    property FrameControllerNotifications: TRzFrameControllerNotifications
      read FFrameControllerNotifications
      write FFrameControllerNotifications
      default fccAll;
      
    property FrameController: TRzFrameController
      read FFrameController
      write SetFrameController;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalCenter;

    property InteriorOffset: Byte
      read FInteriorOffset
      write SetInteriorOffset;

    property NumSegments: TSegmentRange
      read FNumSegments
      write SetNumSegments
      default 20;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    property PartsComplete: TUnsignedLongint
      read FPartsComplete
      write SetPartsComplete;

    property Percent: Word
      read FPercent
      write SetPercent;

    property ShowPercent: Boolean
      read FShowPercent
      write SetShowPercent
      default True;

    property ShowParts: Boolean
      read FShowParts
      write SetShowParts
      default False;

    property TotalParts: TUnsignedLongint
      read FTotalParts
      write SetTotalParts;

    property ThemeAware: Boolean
      read FThemeAware
      write SetThemeAware
      default True;

    property OnChange: TProgressChangeEvent
      read FOnChange
      write FOnChange;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

    function UseThemes: Boolean;

    { Inherited Properties & Events }
    property Align;
    property Font;
    property Height default 24;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 200;
    property OnClick;
    property OnDblClick;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  TRzProgressBar = class( TRzCustomProgressBar )
  private
    FAboutInfo: TRzAboutInfo;
  public
    procedure IncPartsByOne;
    procedure IncParts( N: TUnsignedLongint );
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property BackColor;
    property BackColorStop;
    property BarColor;
    property BarColorStop;
    property BarStyle;
    property BevelWidth;
    property BorderColor;
    property BorderInner;
    property BorderOuter;
    property BorderWidth;
    property Constraints;
    property DragKind;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property GradientDirection;
    property Height;
    property InteriorOffset;
    property NumSegments;
    property Orientation;
    property ParentFont;
    property ParentShowHint;
    property PartsComplete;
    property Percent;
    property PopupMenu;
    property ShowHint;
    property ShowParts;
    property ShowPercent;
    property ThemeAware;
    property TotalParts;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;
    property Width;

    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEndDock;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
  end;

implementation

uses
  {&RAS}
  Types,
  Themes,
  UxTheme,
  RzPanel,
  SysUtils;

{&RT}
{==================================}
{== TRzCustomProgressBar Methods ==}
{==================================}

constructor TRzCustomProgressBar.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle + [ csOpaque ];
  FPercent := 0;
  FShowParts := False;
  FShowPercent := True;
  FNumSegments := 20;
  FOrientation := orHorizontal;
  FBackColor := clWhite;
  FBackColorStop := clBtnFace;
  FBarColor := clHighlight;
  FBarColorStop := clHotLight;

  FFrameController := nil;
  FFrameControllerNotifications := fccAll;

  FBorderColor := clBtnFace;
  FBorderInner := fsNone;
  FBorderOuter := fsLowered;
  FBevelWidth := 1;
  FBorderWidth := 0;
  FFlatColor := clBtnShadow;
  FFlatColorAdjustment := 30;
  FGradientDirection := gdHorizontalCenter;
  Width := 200;
  Height := 24;
  FThemeAware := True;
  {&RCI}
end;



destructor TRzCustomProgressBar.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );

  inherited;
end;


procedure TRzCustomProgressBar.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


procedure TRzCustomProgressBar.SetBackColor( Value: TColor );
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBackColorStop( Value: TColor );
begin
  if FBackColorStop <> Value then
  begin
    FBackColorStop := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBarColor( Value: TColor );
begin
  if FBarColor <> Value then
  begin
    FBarColor := Value;
    Font.Color := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBarColorStop( Value: TColor );
begin
  if FBarColorStop <> Value then
  begin
    FBarColorStop := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBarStyle( Value: TBarStyle );
begin
  if FBarStyle <> Value then
  begin
    FBarStyle := Value;
    FShowPercent := ( FBarStyle = bsTraditional ) or ( FBarStyle = bsGradient );
    if FBarStyle = bsLED then
      FInteriorOffset := 1
    else
      FInteriorOffset := 0;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBevelWidth( Value: TBevelWidth );
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBorderColor( Value: TColor );
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBorderInner( Value: TFrameStyleEx );
begin
  if FBorderInner <> Value then
  begin
    FBorderInner := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBorderOuter( Value: TFrameStyleEx );
begin
  if FBorderOuter <> Value then
  begin
    FBorderOuter := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetBorderWidth( Value: TBorderWidth );
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetFlatColor( Value: TColor );
begin
  if FFlatColor <> Value then
  begin
    FFlatColor := Value;
    Invalidate;
  end;
end;


procedure TRzCustomProgressBar.SetFlatColorAdjustment( Value: Integer );
begin
  if FFlatColorAdjustment <> Value then
  begin
    FFlatColorAdjustment := Value;
    Invalidate;
  end;
end;



procedure TRzCustomProgressBar.SetFrameController( Value: TRzFrameController );
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


procedure TRzCustomProgressBar.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.CustomFramingChanged;
begin
  if FFrameController.FrameVisible then
  begin
    if fcpFrameStyle in FFrameControllerNotifications then
      FBorderOuter := FFrameController.FrameStyle;
    if fcpFrameColor in FFrameControllerNotifications then
    begin
      FFlatColor := FFrameController.FrameColor;
      FFlatColorAdjustment := 0;
    end;
    if fcpColor in FFrameControllerNotifications then
      BackColor := FFrameController.Color;
    if fcpParentColor in FFrameControllerNotifications then
      ParentColor := FFrameController.ParentColor;
    Invalidate;
  end;
end;


procedure TRzCustomProgressBar.SetInteriorOffset( Value: Byte );
begin
  if FInteriorOffset <> Value then
  begin
    FInteriorOffset := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetNumSegments( Value: TSegmentRange );
begin
  if FNumSegments <> Value then
  begin
    FNumSegments := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetOrientation( Value: TOrientation );
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetPartsComplete( Value: TUnsignedLongint );
begin
  if FPartsComplete <> Value then
  begin
    if Value > FTotalParts then
      FPartsComplete := FTotalParts
    else
      FPartsComplete := Value;

    // Setting the Percent property causes the SetPercent method to get called
    // which will force a repaint
    if not ( csLoading in ComponentState ) and ( FTotalParts <> 0 ) then
      Percent := Round( FPartsComplete / FTotalParts * 100 );
  end;
end;


procedure TRzCustomProgressBar.SetPercent( Value: Word );
begin
  {&RV}
  if FPercent <> Value then
  begin
    FPercent := Value;
    PercentChanged;

    // Call Repaint rather than Repaint so that the view of the component
    // does not get erased.  This prevents flicker.
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetShowPercent( Value: Boolean );
begin
  if FShowPercent <> Value then
  begin
    FShowPercent := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetShowParts( Value: Boolean );
begin
  if FShowParts <> Value then
  begin
    FShowParts := Value;
    Repaint;
  end;
end;


procedure TRzCustomProgressBar.SetTotalParts( Value: TUnsignedLongint );
begin
  if FTotalParts <> Value then
  begin
    FTotalParts := Value;
    if not ( csLoading in ComponentState ) then
    begin
      FPartsComplete := 0;
      Percent := 0;
    end;
  end;
end;


procedure TRzCustomProgressBar.SetThemeAware( Value: Boolean );
begin
  if FThemeAware <> Value then
  begin
    FThemeAware := Value;
    Invalidate;
  end;
end;


procedure TRzCustomProgressBar.PercentChanged;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self, FPercent );
end;


function TRzCustomProgressBar.UseThemes: Boolean;
begin
  Result := ActiveStyleServicesEnabled and FThemeAware;
  if ( Parent <> nil ) and ( Parent is TRzStatusBar ) then
    Result := Result and ( TRzStatusBar( Parent ).VisualStyle = vsWinXP );
end;


type
  TWinControlAccess = class( TWinControl );


procedure TRzCustomProgressBar.Paint;
var
  R, SrcRect, ThemeRect: TRect;
  ElementDetails: TThemedElementDetails;
  MemImage, Bmp: TBitmap;
  BarColor, BarColorStop, BackColor, BackColorStop: TColor;
begin
  MemImage := TBitmap.Create;
  try
    MemImage.Width := Width;
    MemImage.Height := Height;

    MemImage.Canvas.Font := Font;

    R := ClientRect;

    if UseThemes then
    begin
      if ( Parent <> nil ) and ( Parent is TRzStatusBar ) then
      begin
        // Draw as status
        Bmp := TBitmap.Create;
        try
          Bmp.Width := TRzStatusBar( Parent ).Width;
          Bmp.Height := TRzStatusBar( Parent ).Height;

          // Draw background as status bar
          ElementDetails := ActiveStyleServices.GetElementDetails( tsStatusRoot );
          ThemeRect := Rect( 0, 0, Bmp.Width, Bmp.Height );
          ActiveStyleServices.DrawElement( Bmp.Canvas.Handle, ElementDetails, ThemeRect );
          SrcRect := Rect( BoundsRect.Left + R.Left, BoundsRect.Top + R.Top,
                           BoundsRect.Left + R.Right, BoundsRect.Top + R.Bottom );
          MemImage.Canvas.CopyRect( R, Bmp.Canvas, SrcRect );

          // Draw Status Pane
          ElementDetails := ActiveStyleServices.GetElementDetails( tsPane );
          ActiveStyleServices.DrawElement( MemImage.Canvas.Handle, ElementDetails, R );
        finally
          Bmp.Free;
        end;
        if FInteriorOffset = 0 then
          InflateRect( R, -3, -2 )
        else
          InflateRect( R, -FInteriorOffset - 2, -FInteriorOffset - 1 );

        if FThemeAware then
        begin
          if FOrientation = orHorizontal then
            ElementDetails := ActiveStyleServices.GetElementDetails( tpBar )
          else
            ElementDetails := ActiveStyleServices.GetElementDetails( tpBarVert );

          ActiveStyleServices.DrawElement( MemImage.Canvas.Handle, ElementDetails, R );
        end;

        if FThemeAware then
        begin
          if ( FBarStyle = bsTraditional ) or ( FBarStyle = bsGradient ) then
            InflateRect( R, -FInteriorOffset - 3, -FInteriorOffset - 3 )
          else
            InflateRect( R, -FInteriorOffset - 1, -FInteriorOffset - 1 );
        end
        else
        begin
          InflateRect( R, -FInteriorOffset - 1, -FInteriorOffset - 1 );
        end;
      end
      else // Draw as stand-alone progress bar
      begin
        if FOrientation = orHorizontal then
          ElementDetails := ActiveStyleServices.GetElementDetails( tpBar )
        else
          ElementDetails := ActiveStyleServices.GetElementDetails( tpBarVert );

        ActiveStyleServices.DrawElement( MemImage.Canvas.Handle, ElementDetails, R );

        if ( FBarStyle = bsTraditional ) or ( FBarStyle = bsGradient ) then
          InflateRect( R, -FInteriorOffset - 3, -FInteriorOffset - 3 )
        else if not FThemeAware then
          InflateRect( R, -FInteriorOffset - 2, -FInteriorOffset - 2 )
        else
          InflateRect( R, -FInteriorOffset - 1, -FInteriorOffset - 1 );
      end;
    end
    else // No Themes
    begin
      MemImage.Canvas.Brush.Color := FBorderColor;
      MemImage.Canvas.FillRect( R );

      R := DrawInnerOuterBorders( MemImage.Canvas, R, FBorderOuter, FBorderInner, BorderWidth, sdAllSides, BevelWidth,
                                  FBorderColor, clBtnHighlight, clBtnShadow,
                                  FlatColor, FlatColorAdjustment, FBackColor, TWinControlAccess( Parent ).Color,
                                  False, True );

      // Draw Interior Offset Region

      if FInteriorOffset > 0 then
      begin
        MemImage.Canvas.Brush.Style := bsClear;
        MemImage.Canvas.Pen.Color := FBackColor;
        MemImage.Canvas.Pen.Style := psInsideFrame;
        MemImage.Canvas.Pen.Width := FInteriorOffset;
        MemImage.Canvas.Rectangle( R.Left, R.Top, R.Right, R.Bottom );
        MemImage.Canvas.Pen.Width := 1;                    // Be sure to restore Pen width
        MemImage.Canvas.Pen.Style := psSolid;
        MemImage.Canvas.Brush.Style := bsSolid;
      end;
      InflateRect( R, -FInteriorOffset, -FInteriorOffset );
    end;


    if UsingSystemStyle then
    begin
      BarColor := FBarColor;
      BarColorStop := FBarColorStop;
      BackColor := FBackColor;
      BackColorStop := FBackColorStop;
    end
    else // VCL Styles
    begin
      BarColor := ActiveStyleSystemColor( clHighlight );
      BarColorStop := DarkerColor( BarColor, 20 );
      BackColor := ActiveStyleSystemColor( clWindow );
      BackColorStop := DarkerColor( BackColor, 20 );
    end;


    case FBarStyle of
      bsTraditional:
        DrawPercentBar( MemImage.Canvas, R, FOrientation, BarColor, BackColor,
                        FPercent, FShowPercent, False, FShowParts,
                        FPartsComplete, FTotalParts );
      bsLED:
        DrawLEDBar( MemImage.Canvas, R, FOrientation, BarColor, BackColor,
                    FNumSegments, FPercent, FThemeAware, False );
      bsGradient:
        DrawGradientPercentBar( MemImage.Canvas, R, FOrientation,
                                BarColor, BarColorStop, BackColor, BackColorStop,
                                FGradientDirection,
                                FPercent, FShowPercent, False, FShowParts,
                                FPartsComplete, FTotalParts );
    end;

    Canvas.Draw( 0, 0, MemImage );
  finally
    MemImage.Free;
  end;
end; {= TRzCustomProgressBar.Paint =}


procedure TRzCustomProgressBar.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
  inherited;
  if Height > Width then
    Orientation := orVertical
  else
    Orientation := orHorizontal;
end;


{===============================================================================
  TRzCustomProgressBar.CMHitTest

  Description
    This message is handled in this component so that when progress bars are
    used in an RzStatusBar component, the size grip can force the mouse message
    all the way down to the form.
===============================================================================}

procedure TRzCustomProgressBar.CMHitTest( var Msg: TMessage );
var
  R: TRect;
begin
  inherited;

  if ( Parent <> nil ) and ( Parent is TRzStatusBar ) then
  begin
    R := ClientRect;
    R.Left := R.Right - 12;

    if PtInRect( R, Point( Msg.LParamLo, Msg.LParamHi ) ) then
      Msg.Result := Parent.Perform( wm_NCHitTest, Msg.WParam, Msg.LParam );
  end;
end;


{============================}
{== TRzProgressBar Methods ==}
{============================}

procedure TRzProgressBar.IncPartsByOne;
begin
  IncParts( 1 );
end;


procedure TRzProgressBar.IncParts( N: TUnsignedLongint );
begin
  PartsComplete := PartsComplete + N;
end;

{&RUIF}
end.

