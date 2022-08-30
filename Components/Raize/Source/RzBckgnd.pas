{===============================================================================
  RzBckgnd Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzBackground
    Graphic control to display gradients, bitmap tiling, and textures.

  TRzSeparator
    Graphic control to display a separator - supports gradient fills


  Modification History
  ------------------------------------------------------------------------------
  6.1.7  (07 Mar 2014)
    * Updated the TRzBackground with better support for 64-bit Windows.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzSeparator to fully support VCL Styles 
      introduced in RAD Studio XE2.
    * Fixed display issue in TRzSeparator where no separator would be displayed
      if ShowGradient was set to False.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzBackground control.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed problem where the TRzBackground component displaying a gradient
      would get into a repaint loop if the control was placed on a container
      that forced the component to occupy a space smaller than 5 pixels
      horizontally or vertically.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Fixed problem where TRzSeparator would not honor the ParentColor property.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where TRzBackground would display a white rectangle if the
      starting and stopping colors were the same and the gradient direction was
      one of the "box" styles.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Test FullColorSupport before all calls to PaintGradient in TRzSeparator.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Surfaced Anchors and Constraints properties.
    * Moved gradient drawing code to the RzGrafx unit (PaintGradient).
===============================================================================}

{$I RzComps.inc}

{$R-} { Turn off range checking }

unit RzBckgnd;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Menus,
  Graphics,
  StdCtrls,
  ExtCtrls,
  Forms,
  Buttons,
  RzCommon,
  Dialogs,
  RzGrafx;

type
  TImageStyle = ( isCenter, isClip, isFill, isStretch, isTiled );

  {===========================================}
  {== TRzCustomBackground Class Declaration ==}
  {===========================================}

  TRzCustomBackground = class( TGraphicControl )
  private
    FActive: Boolean;
    FMDIActive: Boolean;

    FFrameStyle: TFrameStyle;
    FFrameColor: TColor;
    FColorStart: TColor;
    FColorStartDisp: TColor;
    FStartRed, FStartGreen, FStartBlue: Byte;
    FColorStop: TColor;
    FColorStopDisp: TColor;
    FStopRed, FStopGreen, FStopBlue: Byte;
    FGradientDirection: TGradientDirection;
    FGradientSmoothFactor: TSmoothFactor;
    FShowGradient: Boolean;

    FImage: TPicture;
    FShowImage: Boolean;
    FImageStyle: TImageStyle;

    FTexture: TPicture;
    FShowTexture: Boolean;

    FClientInstance: TFarProc;
    FClientHandle: HWND;
    FDefClientProc: TFarProc;

    procedure EraseMDIClientBkgrnd( var Msg: TMessage );
    procedure SubClassMDIClient( AOwner: TForm );

    { Internal Event Handlers }
    procedure TextureChangeHandler( Sender: TObject );
    procedure ImageChangeHandler( Sender: TObject );
  protected
    FAboutInfo: TRzAboutInfo;

    procedure Loaded; override;
    procedure SetParent( AParent: TWinControl ); override;

    procedure InvalidateBackground; virtual;
    procedure Paint; override;
    procedure PaintGradient( ACanvas: TCanvas ); virtual;
    procedure PaintImage( ACanvas: TCanvas; Picture: TPicture ); virtual;
    procedure PaintTexture( ACanvas: TCanvas; Picture: TPicture ); virtual;

    { Property Access Methods }
    procedure SetActive( Value: Boolean ); virtual;
    function GetAlign: TAlign; virtual;
    procedure SetAlign( Value: TAlign ); virtual;
    procedure SetColorStart( Value: TColor ); virtual;
    procedure SetColorStop( Value: TColor ); virtual;
    procedure SetFrameColor( Value: TColor ); virtual;
    procedure SetFrameStyle( Value: TFrameStyle ); virtual;
    procedure SetGradientDirection( Value: TGradientDirection ); virtual;
    procedure SetGradientSmoothFactor( Value: TSmoothFactor ); virtual;
    procedure SetImage( Value: TPicture ); virtual;
    procedure SetImageStyle( Value: TImageStyle ); virtual;
    procedure SetShowGradient( Value: Boolean ); virtual;
    procedure SetShowImage( Value: Boolean ); virtual;
    procedure SetShowTexture( Value: Boolean ); virtual;
    procedure SetTexture( Value: TPicture ); virtual;
    function GetVisible: Boolean; virtual;
    procedure SetVisible( Value: Boolean ); virtual;

    { Property Declarations }
    property Active: Boolean
      read FActive
      write SetActive;

    property Align: TAlign
      read GetAlign
      write SetAlign;

    property FrameColor: TColor
      read FFrameColor
      write SetFrameColor
      default clBtnShadow;

    property FrameStyle: TFrameStyle
      read FFrameStyle
      write SetFrameStyle
      default fsNone;

    property GradientColorStart: TColor
      read FColorStartDisp
      write SetColorStart
      default clHotLight;

    property GradientColorStop: TColor
      read FColorStopDisp
      write SetColorStop
      default clActiveCaption;

    property GradientDirection: TGradientDirection
      read FGradientDirection
      write SetGradientDirection
      default gdHorizontalEnd;

    property GradientSmoothFactor: TSmoothFactor
      read FGradientSmoothFactor
      write SetGradientSmoothFactor
      default 1;

    property Image: TPicture
      read FImage
      write SetImage;

    property ImageStyle: TImageStyle
      read FImageStyle
      write SetImageStyle
      stored True;

    property ShowGradient: Boolean
      read FShowGradient
      write SetShowGradient
      stored True;

    property ShowImage: Boolean
      read FShowImage
      write SetShowImage
      stored True;

    property ShowTexture: Boolean
      read FShowTexture
      write SetShowTexture
      stored True;

    property Texture: TPicture
      read FTexture
      write SetTexture
      stored True;

    property Visible: Boolean
      read GetVisible
      write SetVisible
      default True;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  end;


  TRzBackground = class( TRzCustomBackground )
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Active;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FrameColor;
    property FrameStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GradientSmoothFactor;
    property Image;
    property ImageStyle;
    property ParentShowHint;
    property PopupMenu;
    property ShowGradient;
    property ShowHint;
    property ShowImage;
    property ShowTexture;
    property Texture;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  {====================================}
  {== TRzSeparator Class Declaration ==}
  {====================================}

  TRzHighlightLocation = ( hlCenter, hlUpperLeft, hlLowerRight );

  TRzSeparator = class( TGraphicControl )
  private
    FAboutInfo: TRzAboutInfo;
    FHighlightColor: TColor;
    FHighlightLocation: TRzHighlightLocation;
    FOrientation: TOrientation;
    FShowGradient: Boolean;

    FGradientDirection: TGradientDirection;
    FStartColor: TColor;
    FStopColor: TColor;

    FSetInitialSizeRequired: Boolean;

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    {$IFDEF VCL160_OR_HIGHER}
    procedure CMStyleChanged( var Msg: TMessage ); message cm_StyleChanged;
    {$ENDIF}
  protected
    procedure Loaded; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure UpdateGradientDetails;

    { Property Access Methods }
    procedure SetHighlightColor( Value: TColor ); virtual;
    procedure SetHighlightLocation( Value: TRzHighlightLocation ); virtual;
    procedure SetShowGradient( Value: Boolean ); virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property HighlightColor: TColor
      read FHighlightColor
      write SetHighlightColor
      default clHighlight;

    property HighlightLocation: TRzHighlightLocation
      read FHighlightLocation
      write SetHighlightLocation
      default hlCenter;

    property Orientation: TOrientation
      read FOrientation
      write SetOrientation
      default orHorizontal;

    property ShowGradient: Boolean
      read FShowGradient
      write SetShowGradient
      stored True;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Color stored True;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Height default 2;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property Width default 200;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation

uses
  {&RAS}
  Types;


{&RT}
{=================================}
{== TRzCustomBackground Methods ==}
{=================================}

constructor TRzCustomBackground.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csOpaque ];

  FMDIActive := False;

  FImage := TPicture.Create;
  FImage.OnChange := ImageChangeHandler;
  FTexture := TPicture.Create;
  FTexture.OnChange := TextureChangeHandler;

  Height := 100;
  Width := 200;

  FFrameColor := clBtnShadow;
  FFrameStyle := fsNone;

  { Must call methods to ensure other fields are setup correctly }
  SetColorStart( clHotLight );
  SetColorStop( clActiveCaption );

  FGradientDirection := gdHorizontalEnd;
  FGradientSmoothFactor := 1;
  FActive := True;
  FShowGradient := True;
  FShowTexture := False;
  FShowImage := False;
  {&RCI}
end; {= TRzCustomBackground.Create =}


destructor TRzCustomBackground.Destroy;
begin
  FImage.Free;
  FTexture.Free;
  if FMDIActive then
  begin
    Destroying;
    SetWindowLongPtr( FClientHandle, gwl_WndProc, LONG_PTR( FDefClientProc ) );
    Classes.FreeObjectInstance( FClientInstance );
  end;
  inherited;
end;


procedure TRzCustomBackground.Loaded;
begin
  inherited;

  if FMDIActive then
  begin
    if not ( csDesigning in ComponentState ) then
      SubClassMDIClient( Owner as TForm );
  end;
  {&RV}
end;


procedure TRzCustomBackground.SetParent( AParent: TWinControl );
begin
  inherited;

  if AParent <> nil then
  begin
    { Check for MDI parent for proper subclassing }
    if ( AParent is TForm ) and ( TForm( AParent ).FormStyle = fsMDIForm ) then
    begin
      inherited Align := alClient;
      FMDIActive := True;
    end;
  end;
end;


procedure TRzCustomBackground.SubClassMDIClient( AOwner: TForm );
begin
  FClientHandle := AOwner.ClientHandle;
  FClientInstance := Classes.MakeObjectInstance( EraseMDIClientBkgrnd );
  FDefClientProc := Pointer( GetWindowLong( FClientHandle, gwl_WndProc ) );
  SetWindowLongPtr( FClientHandle, gwl_WndProc, LONG_PTR( FClientInstance ) );
end;


procedure TRzCustomBackground.EraseMDIClientBkgrnd( var Msg: TMessage );
var
  DestRect, R: TRect;
  SrcDC, DestDC: THandle;
  TempBitmap: TBitmap;
  Offset: Integer;
  ACanvas: TCanvas;
begin
  if Visible then
  begin
    case Msg.Msg of
      wm_EraseBkgnd:
      begin
        DestDC := TWMEraseBkgnd( Msg ).DC;
        ACanvas := TCanvas.Create;
        ACanvas.Handle := DestDC;

        GetClipBox( DestDC, DestRect );
        TempBitmap := TBitmap.Create;
        try
          if Width < 5 then
          begin
            Width := 5;
          end;
          if Height < 5 then
          begin
            Height := 5;
          end;
          TempBitmap.Width := Width;
          TempBitmap.Height := Height;
          SrcDC := TempBitmap.Canvas.Handle;

          if FShowTexture then
            PaintTexture( TempBitmap.Canvas, FTexture );

          if FShowGradient then
            PaintGradient( TempBitmap.Canvas );

          if FShowImage then
            PaintImage( TempBitmap.Canvas, FImage );

          case FFrameStyle of
            fsNone:
              Offset := 0;
            fsFlat, fsPopup, fsStatus:
              Offset := 1;
            else
              Offset := 2;
          end;

          BitBlt( DestDC, Offset, Offset, Width - Offset, Height - Offset, SrcDC, 0, 0, SRCCOPY );

          if FFrameStyle = fsFlat then
            DrawSides( ACanvas, ClientRect, FFrameColor, FFrameColor, sdAllSides )
          else
            RzCommon.DrawBorder( ACanvas, ClientRect, FFrameStyle );
        finally
          TempBitmap.Free;
          ACanvas.Free;
        end;
        Msg.Result := 1;
      end;

      wm_WindowPosChanging:
      begin
        // It is necessary to manually invalidate client area if background in
        // on a MDIForm because Win32 does not invalidate the entire client area.
        R := ClientRect;
        InvalidateRect( FClientHandle, @R, True );
        Msg.Result := CallWindowProc( FDefClientProc, FClientHandle, Msg.Msg, Msg.WParam, Msg.LParam );
      end;

      wm_Paint:
      begin
        // It is necessary to manually invalidate client area if background in
        // on a MDIForm because Win32 does not invalidate the entire client area.
        R := ClientRect;
        InvalidateRect( FClientHandle, @R, True );
        Msg.Result := CallWindowProc( FDefClientProc, FClientHandle, Msg.Msg, Msg.WParam, Msg.LParam );
      end;

      else
        Msg.Result := CallWindowProc( FDefClientProc, FClientHandle, Msg.Msg, Msg.WParam, Msg.LParam );
    end; { case }
  end
  else
    Msg.Result := CallWindowProc( FDefClientProc, FClientHandle, Msg.Msg, Msg.WParam, Msg.LParam );

end;{= TRzCustomBackground.EraseMDIClientBkgrnd =}



procedure TRzCustomBackground.PaintGradient( ACanvas: TCanvas );
var
  R: TRect;
begin
  if csDesigning in ComponentState then
  begin
    ACanvas.Pen.Style := psDash;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle( 0, 0, Width, Height );
  end;

  if Active and not ( FShowImage and ( FImageStyle in [ isStretch, isTiled ] ) ) then
  begin
    R := Rect( 0, 0, Width, Height );
    RzGrafx.PaintGradient( ACanvas, R, FGradientDirection, FColorStart, FColorStop, FGradientSmoothFactor );
  end;
end;


procedure TRzCustomBackground.PaintImage( ACanvas: TCanvas; Picture: TPicture );
var
  Dest: TRect;
  XPos, YPos: Integer;
  PicRatio, ImageRatio: Extended;
begin
  if csDesigning in ComponentState then
  begin
    ACanvas.Pen.Style := psDash;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle( 0, 0, Width, Height );
  end;

  if Picture.Graphic <> nil then
  begin
    case ImageStyle of
      isClip:
        ACanvas.Draw( 0, 0, Picture.Graphic );

      isCenter:
      begin
        XPos := ( Width - Picture.Width ) div 2;
        YPos := ( Height - Picture.Height ) div 2;
        ACanvas.Draw( Xpos, YPos, Picture.Graphic );
      end;

      isFill:
      begin
        if ( Picture.Width > 0 ) and ( Picture.Height > 0 ) then
        begin
          PicRatio := Picture.Height / Picture.Width;
          ImageRatio := Height / Width;
          if PicRatio > ImageRatio then
          begin
            XPos := Trunc( Height / PicRatio );
            YPos := Height;
          end
          else
          begin
            XPos := Width;
            YPos := Trunc( Width * PicRatio );
          end;
          Dest := Rect( 0, 0, XPos, YPos );
          ACanvas.StretchDraw( Dest, Picture.Graphic );
        end;
      end;

      isStretch:
      begin
        Dest := ClientRect;
        ACanvas.StretchDraw( Dest, Picture.Graphic );
      end;

      isTiled:
      begin
        ACanvas.Draw( 0, 0, Picture.Graphic );                      { 1st copy }
        BitFillBlit( ACanvas.Handle, 0, 0, Width, Height, Picture.Width, Picture.Height, SRCCOPY );
      end;
    end; { case }
  end;
end; {= TRzCustomBackground.PaintImage =}


procedure TRzCustomBackground.PaintTexture( ACanvas: TCanvas; Picture: TPicture );
begin
  if csDesigning in ComponentState then
  begin
    ACanvas.Pen.Style := psDash;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle( 0, 0, Width, Height );
  end;

  if Picture.Graphic <> nil then
  begin
    if Picture.Graphic.ClassType <> TBitmap then
    begin
      ACanvas.Draw( 0, 0, Picture.Graphic );                          { 1st copy }
      BitFillBlit( ACanvas.Handle, 0, 0, Width, Height, Picture.Width, Picture.Height, SRCCOPY );
    end
    else
      TileBitmap( ACanvas, Picture.Bitmap, ClientRect );
  end;
end; {= TRzCustomBackground.PaintTexture =}


procedure TRzCustomBackground.Paint;
var
  TempBitmap: TBitmap;
  Offset: Integer;
begin
  if FMDIActive and not ( csDesigning in ComponentState ) then
    Exit; // EraseMDIClientBackground will take care of this, but only at runtime.

  TempBitmap := TBitmap.Create;
  try
    if FShowGradient and ( ( Width < 5 ) or ( Height < 5 ) ) then
    begin
      Canvas.Brush.Color := FColorStart;
      Canvas.FillRect( ClientRect );
    end
    else
    begin
      TempBitmap.Width := Width;
      TempBitmap.Height := Height;

      if FShowTexture then
        PaintTexture( TempBitmap.Canvas, FTexture );

      if FShowGradient then
        PaintGradient( TempBitmap.Canvas );

      if FShowImage then
        PaintImage( TempBitmap.Canvas, FImage );

      case FFrameStyle of
        fsNone:
          Offset := 0;
        fsFlat, fsPopup, fsStatus:
          Offset := 1;
        else
          Offset := 2;
      end;

      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw( Offset, Offset, TempBitmap );
    end;

    if FFrameStyle = fsFlat then
      DrawSides( Canvas, ClientRect, FFrameColor, FFrameColor, sdAllSides )
    else
      RzCommon.DrawBorder( Canvas, ClientRect, FFrameStyle );
  finally
    TempBitmap.Free;
  end;
end; {= TRzCustomBackground.Paint =}


procedure TRzCustomBackground.ImageChangeHandler( Sender: TObject );
begin
  if FShowImage then
    InvalidateBackground;
end;

procedure TRzCustomBackground.TextureChangeHandler( Sender: TObject );
begin
  if FShowTexture then
    InvalidateBackground;
end;


procedure TRzCustomBackground.SetActive( Value: Boolean );
begin
  if FActive <> Value then
  begin
    FActive:= Value;
    InvalidateBackground;
  end;
end;


function TRzCustomBackground.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TRzCustomBackground.SetAlign( Value: TAlign );
begin
  if FMDIActive then
    inherited Align := alClient
  else
    inherited Align := Value;
end;


procedure TRzCustomBackground.SetColorStart( Value: TColor );
begin
  if FColorStart <> Value then
  begin
    FColorStart := ColorToRGB( Value );
    FColorStartDisp := Value;
    FStartRed := GetRValue( FColorStart );
    FStartGreen := GetGValue( FColorStart );
    FStartBlue := GetBValue( FColorStart );
    InvalidateBackground;
  end;
end;

procedure TRzCustomBackground.SetColorStop( Value: TColor );
begin
  if FColorStop <> Value then
  begin
    FColorStop := ColorToRGB( Value );
    FColorStopDisp := Value;
    FStopRed := GetRValue( FColorStop );
    FStopGreen := GetGValue( FColorStop );
    FStopBlue := GetBValue( FColorStop );
    InvalidateBackground;
  end;
end;


procedure TRzCustomBackground.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    InvalidateBackground;
  end;
end;

procedure TRzCustomBackground.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    InvalidateBackground;
  end;
end;


procedure TRzCustomBackground.InvalidateBackground;
var
  R: TRect;
begin
  if csLoading in ComponentState then
    Exit;

  if FMDIActive then
  begin
    R := ClientRect;
    InvalidateRect( FClientHandle, @R, True );
  end
  else
    Invalidate;
end;

procedure TRzCustomBackground.SetShowImage( Value: Boolean );
begin
  if FShowImage <> Value then
  begin
    FShowImage := Value;
    InvalidateBackground;
  end;
end;


procedure TRzCustomBackground.SetImage( Value: TPicture );
begin
  FImage.Assign( Value );
end;

procedure TRzCustomBackground.SetImageStyle( Value: TImageStyle );
begin
  if FImageStyle <> Value then
  begin
    FImageStyle := Value;
    if FShowImage then
      InvalidateBackground;
  end;
end;


procedure TRzCustomBackground.SetTexture( Value: TPicture );
begin
  FTexture.Assign( Value );
end;


procedure TRzCustomBackground.SetShowTexture( Value: Boolean );
begin
  if FShowTexture <> Value then
  begin
    FShowTexture := Value;
    if FShowTexture then
      SetShowGradient( False )
    else
      SetShowGradient( True );
    InvalidateBackground;
  end;
end;

procedure TRzCustomBackground.SetGradientDirection( Value: TGradientDirection );
begin
  if FGradientDirection <> Value then
  begin
    FGradientDirection := Value;
    InvalidateBackground;
  end;
end;

procedure TRzCustomBackground.SetGradientSmoothFactor( Value: TSmoothFactor );
begin
  if FGradientSmoothFactor <> Value then
  begin
    FGradientSmoothFactor := Value;
    InvalidateBackground;
  end;
end;


procedure TRzCustomBackground.SetShowGradient( Value: Boolean );
begin
  if FShowGradient <> Value then
  begin
    FShowGradient := Value;

    if FShowGradient then
      SetShowTexture( False )
    else if not FShowImage then
      SetShowTexture( True );

    InvalidateBackground;
  end;
end;

function TRzCustomBackground.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TRzCustomBackground.SetVisible( Value: Boolean );
begin
  if Visible <> Value then
  begin
    inherited Visible := Value;
    InvalidateBackground;
  end;
end;


{==========================}
{== TRzSeparator Methods ==}
{==========================}

constructor TRzSeparator.Create( AOwner: TComponent );
begin
  inherited;

  Width := 200;
  Height := 2;

  // The FSetInitialSizeRequired flag is used to work around a problem with the 
  // Form Designer in Delphi 6 and above. The Form Designer does not the Width 
  // or Height of a control to be smaller than 8 pixels.
  FSetInitialSizeRequired := csDesigning in ComponentState;

  FOrientation := orHorizontal;
  FHighlightColor := clHighlight;
  FHighlightLocation := hlCenter;
  UpdateGradientDetails;

  FShowGradient := True;

  {&RCI}
end;


procedure TRzSeparator.Loaded;
begin
  inherited;
  // If the component is loaded from a form/stream, no need to initialize the size
  FSetInitialSizeRequired := False;
  UpdateGradientDetails;
end;


procedure TRzSeparator.Resize;
begin
  if FSetInitialSizeRequired then
  begin
    FSetInitialSizeRequired := False;
    Width := 200;
    Height := 2;
  end;
  inherited;
end;


procedure TRzSeparator.Paint;
var
  TempBitmap: TBitmap;
  R: TRect;
begin
  if FShowGradient and FullColorSupported then
  begin
    TempBitmap := TBitmap.Create;
    try
      if Width < 1 then
        Width := 1;
      if Height < 1 then
        Height := 1;

      TempBitmap.Width := Width;
      TempBitmap.Height := Height;

      if csDesigning in ComponentState then
      begin
        TempBitmap.Canvas.Pen.Style := psDash;
        TempBitmap.Canvas.Brush.Style := bsClear;
        TempBitmap.Canvas.Rectangle( 0, 0, Width, Height );
      end;

      R := Rect( 0, 0, Width, Height );
      RzGrafx.PaintGradient( TempBitmap.Canvas, R, FGradientDirection, FStartColor, FStopColor );

      Canvas.CopyMode := cmSrcCopy;
      Canvas.Draw( 0, 0, TempBitmap );
    finally
      TempBitmap.Free;
    end;
  end
  else
  begin
    inherited;
    Canvas.Brush.Color := ActiveStyleSystemColor( HighlightColor );
    Canvas.FillRect( ClientRect );
  end;
end; {= TRzSeparator.Paint =}


procedure TRzSeparator.UpdateGradientDetails;
var
  C, HC: TColor;
begin
  C := ActiveStyleSystemColor( Color );
  HC := ActiveStyleSystemColor( FHighlightColor );

  case FOrientation of
    orHorizontal:
    begin
      case FHighlightLocation of
        hlCenter:
        begin
          FGradientDirection := gdVerticalCenter;
          FStartColor := C;
          FStopColor := HC;
        end;

        hlUpperLeft:
        begin
          FGradientDirection := gdVerticalEnd;
          FStartColor := HC;
          FStopColor := C;
        end;

        hlLowerRight:
        begin
          FGradientDirection := gdVerticalEnd;
          FStartColor := C;
          FStopColor := HC;
        end;
      end;
    end;

    orVertical:
    begin
      case FHighlightLocation of
        hlCenter:
        begin
          FGradientDirection := gdHorizontalCenter;
          FStartColor := C;
          FStopColor := HC;
        end;

        hlUpperLeft:
        begin
          FGradientDirection := gdHorizontalEnd;
          FStartColor := HC;
          FStopColor := C;
        end;

        hlLowerRight:
        begin
          FGradientDirection := gdHorizontalEnd;
          FStartColor := C;
          FStopColor := HC;
        end;
      end;
    end;
  end;
end; {= TRzSeparator.UpdateGradientDetails =}


procedure TRzSeparator.SetHighlightColor( Value: TColor );
begin
  if FHighlightColor <> Value then
  begin
    FHighlightColor := Value;
    UpdateGradientDetails;
    Invalidate;
  end;
end;


procedure TRzSeparator.SetHighlightLocation( Value: TRzHighlightLocation );
begin
  if FHighlightLocation <> Value then
  begin
    FHighlightLocation := Value;
    UpdateGradientDetails;
    Invalidate;
  end;
end;


procedure TRzSeparator.SetOrientation( Value: TOrientation );
var
  W: Integer;
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    if not ( csLoading in ComponentState ) then
    begin
      // Swap Width and Height
      W := Width;
      Width := Height;
      Height := W;
      UpdateGradientDetails;
      Invalidate;
    end;
  end;
end;


procedure TRzSeparator.SetShowGradient( Value: Boolean );
begin
  if FShowGradient <> Value then
  begin
    FShowGradient := Value;
    Invalidate;
  end;
end;


procedure TRzSeparator.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  UpdateGradientDetails;
  Invalidate;
end;


{$IFDEF VCL160_OR_HIGHER}

procedure TRzSeparator.CMStyleChanged( var Msg: TMessage );
begin
  inherited;
  UpdateGradientDetails;
  Invalidate;
end;

{$ENDIF}


{&RUIF}
end.

