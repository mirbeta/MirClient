{===============================================================================
  RzGrids Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzStringGrid
    Enhanced StringGrid that supports Custom Framing and additional display
    options for grid cells.


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Added new UseDrawingStyle property to TRzStringGrid. This property only
      applies when using RAD Studio 2010 or higher. When it is True (the
      default) the appearance of the grid is controlled by the built-in 
      DrawingStyle property. When UseDrawingStyle is set to False, the 
      appearance of the grid is controlled by the various properties such as 
      LineColor.
    * Made necessary modifications to TRzStringGrid to support 64-bit.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Updated the TRzStringGrid to take advantage of the new TStringGrid display
      capabilities added in RAD Studio 2010.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where focus color of the grid would change if the inplace
      editor was invoked using the mouse.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzStringGri to
      account for changes introduced in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzStringGrid
      when FrameVisible was set to True and changes were made to control's
      appearance within calls to LockWindowUpdate.
    * Added new FrameControllerNotifications property to TRzStringGrid.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where inplace edit would not match the Color property of
      the grid itself.
  ------------------------------------------------------------------------------
  3.0.10 (26 Dec 2003)
    * Fixed problem where changing ParentColor to True in a control using Custom
      Framing did not reset internal color fields used to manage the color of
      the control at various states.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Surfaced OnResize event in TRzStringGrid.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Initial release.
    * The TRzStringGrid is a direct descendant of the VCL TStringGrid that
      supports Custom Framing and additional display options for grid cells.
===============================================================================}

{$I RzComps.inc}

unit RzGrids;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Messages,
  Windows,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  Grids,
  RzCommon;

type
  {=====================================}
  {== TRzStringGrid Class Declaration ==}
  {=====================================}

  TRzStringGrid = class( TStringGrid )
  private
    FLineColor: TColor;
    FFixedLineColor: TColor;
    FUseDrawingStyle: Boolean;

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

    { Message Handling Methods }
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure WMNCPaint( var Msg: TWMNCPaint ); message wm_NCPaint;
    procedure CMParentColorChanged( var Msg: TMessage ); message cm_ParentColorChanged;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMMouseEnter( var Msg: TMessage ); message cm_MouseEnter;
    procedure CMMouseLeave( var Msg: TMessage ); message cm_MouseLeave;
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
  protected
    FAboutInfo: TRzAboutInfo;
    FCanvas: TCanvas;
    FOverControl: Boolean;

    procedure Loaded; override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    function CreateEditor: TInplaceEdit; override;

    procedure UpdateColors; virtual;
    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); virtual;
    procedure RepaintFrame; virtual;

    function IsActiveControl: Boolean;
    procedure Paint; override;
    procedure GridRectToScreenRect( GridRect: TGridRect;
              var ScreenRect: TRect; IncludeLine: Boolean );
    function GetFocusSelection: TGridRect; virtual;

    { Property Access Methods }
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
    procedure SetFixedLineColor( Value: TColor ); virtual;
    procedure SetLineColor( Value: TColor ); virtual;
    procedure SetUseDrawingStyle( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UseThemes: Boolean; virtual;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property Color
      stored StoreColor
      default clWindow;

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

    property FixedLineColor: TColor
      read FFixedLineColor
      write SetFixedLineColor
      default clBtnShadow;

    property LineColor: TColor
      read FLineColor
      write SetLineColor
      default clBtnFace;

    property UseDrawingStyle: Boolean
      read FUseDrawingStyle
      write SetUseDrawingStyle
      default True;

    { Inherited Properties & Events }
    property DefaultColWidth default 60;
    property DefaultRowHeight default 18;
    property Height default 125;
    property Width default 325;

    property OnResize;
  end;


implementation

uses
  {&RAS}
  {$IFDEF VCL170_OR_HIGHER}
  UITypes,
  {$ENDIF}
  Themes,
  UxTheme,
  Forms,
  Math,
  RTLConsts,
  Consts;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..MaxCustomExtents] of Integer;


{=======================}
{== Support Functions ==}
{=======================}

function PointInGridRect( Col, Row: Longint; const Rect: TGridRect ): Boolean;
begin
  Result := ( Col >= Rect.Left ) and ( Col <= Rect.Right ) and ( Row >= Rect.Top ) and ( Row <= Rect.Bottom );
end;


{$IFDEF CPUX64}

procedure FillDWord( var Dest; Count, Value: Integer );
{$POINTERMATH ON}
var
  I: Integer;
  P: PInteger;
begin
  P := PInteger( @Dest );
  for I := 0 to Count - 1 do
    P[ I ] := Value;
end;

{$ELSE}

procedure FillDWord( var Dest; Count, Value: Integer ); register;
asm
  XCHG  EDX, ECX
  PUSH  EDI
  MOV   EDI, EAX
  MOV   EAX, EDX
  REP   STOSD
  POP   EDI
end;

{$ENDIF}



{$IFDEF CPUX64}

function StackAlloc( Size: Integer ): Pointer;
begin
  GetMem( Result, Size );
end;

{$ELSE}

function StackAlloc( Size: Integer ): Pointer; register;
asm
  POP   ECX          // return address
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          // make sure we touch guard page, to grow stack
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     // function result = low memory address of block
  PUSH  EDX          // save original SP, for cleanup
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          // save current SP, for sanity check  (sp = [sp])
  PUSH  ECX          // return to caller
end;

{$ENDIF}


{$IFDEF CPUX64}

procedure StackFree( P: Pointer );
begin
  FreeMem( P );
end;

{$ELSE}

procedure StackFree( P: Pointer ); register;
asm
  POP   ECX                     // return address
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                // sanity check #1 (SP = [SP])
  JNE   @@1
  CMP   EDX, EAX                // sanity check #2 (P = this stack block)
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  // restore previous SP
@@1:
  PUSH  ECX                     // return to caller
end;

{$ENDIF}


{&RT}
{===========================}
{== TRzStringGrid Methods ==}
{===========================}

constructor TRzStringGrid.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}

  FCanvas := TControlCanvas.Create;
  TControlCanvas( FCanvas ).Control := Self;

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

  FLineColor := clBtnFace;
  FFixedLineColor := clBtnShadow;
  FUseDrawingStyle := True;

  DefaultRowHeight := 18;
  DefaultColWidth := 60;
  Height := 125;
  Width := 325;
  {&RV}
end;


destructor TRzStringGrid.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FCanvas.Free;
  inherited;
end;


procedure TRzStringGrid.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzStringGrid.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


function TRzStringGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  TEdit( Result ).Color := Color;
end;


function TRzStringGrid.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzStringGrid.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzStringGrid.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzStringGrid.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzStringGrid.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzStringGrid.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzStringGrid.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzStringGrid.SetFrameController( Value: TRzFrameController );
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


procedure TRzStringGrid.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzStringGrid.SetFrameHotTrack( Value: Boolean );
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


procedure TRzStringGrid.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzStringGrid.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzStringGrid.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzStringGrid.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzStringGrid.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzStringGrid.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzStringGrid.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzStringGrid.CMColorChanged( var Msg: TMessage );
begin
  if InplaceEditor <> nil then
    TEdit( InplaceEditor ).Color := Color;

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


procedure TRzStringGrid.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzStringGrid.SetFixedLineColor( Value: TColor );
begin
  if FFixedLineColor <> Value then
  begin
    FFixedLineColor := Value;
    Invalidate;
  end;
end;


procedure TRzStringGrid.SetLineColor( Value: TColor );
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;


procedure TRzStringGrid.SetUseDrawingStyle( Value: Boolean );
begin
  if FUseDrawingStyle <> Value then
  begin
    FUseDrawingStyle := Value;
    Invalidate;
  end;
end;




procedure TRzStringGrid.WMNCPaint( var Msg: TWMNCPaint );
var
  DC: HDC;
  C: TColor;

  {---------------------------------------------------------------------------------------------------------------------
    DrawGridFrame

    Draws Custom Framing frame around the grid. This version is different than the RzCommon.DrawFrame in that for the
    fsFlat style, the upper-left erase color needs to be a different color than the lower-right erase color.
  ---------------------------------------------------------------------------------------------------------------------}
  procedure DrawGridFrame( Canvas: TCanvas; FrameStyle: TFrameStyle; ULEraseColor, LREraseColor, FrameColor: TColor;
                           FrameSides: TSides );
  var
    R: TRect;
  begin
    R := Rect( 0, 0, Width, Height );
    DrawBevel( Canvas, R, ULEraseColor, LREraseColor, 2, sdAllSides );

    if FrameStyle = fsFlat then
      DrawSides( Canvas, R, FrameColor, FrameColor, FrameSides )
    else if FrameStyle = fsFlatBold then
      DrawBevel( Canvas, R, FrameColor, FrameColor, 2, FrameSides )
    else
      DrawBorderSides( Canvas, R, FrameStyle, FrameSides );
  end;

begin {= TRzStringGrid.WMNCPaint =}
  inherited;                       { Must call inherited so scroll bar show up }

  if BorderStyle = bsNone then
    Exit;
  
  if FFrameVisible and not UseThemes then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      if FFrameHotTrack and ( Focused or FOverControl ) then
        DrawGridFrame( FCanvas, FFrameHotStyle, FixedColor, Color, FFrameHotColor, FFrameSides )
      else
        DrawGridFrame( FCanvas, FFrameStyle, FixedColor, Color, FFrameColor, FFrameSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end
  else if ActiveStyleServicesEnabled then
  begin
    DC := GetWindowDC( Handle );
    FCanvas.Handle := DC;
    try
      C := GetXPThemeColor( xptcEditBorder );
      DrawGridFrame( FCanvas, fsFlat, FixedColor, Color, C, sdAllSides );
    finally
      FCanvas.Handle := 0;
      ReleaseDC( Handle, DC );
    end;
    Msg.Result := 0;
  end;
end; {= TRzStringGrid.WMNCPaint =}


procedure TRzStringGrid.CMParentColorChanged( var Msg: TMessage );
begin
  inherited;

  if ParentColor then
  begin
    // If ParentColor set to True, must reset FNormalColor and FFocusColor
    if FFocusColor = FNormalColor then
      FFocusColor := Color;
    FNormalColor := Color;
  end;

  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


procedure TRzStringGrid.UpdateColors;
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


procedure TRzStringGrid.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzStringGrid.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzStringGrid.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzStringGrid.CMMouseEnter( var Msg: TMessage );
begin
  if csDesigning in ComponentState then
    Exit;

  inherited;

  // When the inplace editor is shown with the mouse, the mouse technically
  // leaves the grid because the mouse is over the inplace editor. However,
  // for purposes of the custom framing and focus color changes, we can
  // consider the mouse to be still over the grid.
  if ( InplaceEditor = nil ) or
     ( ( InplaceEditor <> nil ) and not InplaceEditor.Focused ) then
  begin
    UpdateFrame( True, True );
  end;
end;


procedure TRzStringGrid.CMMouseLeave( var Msg: TMessage );
begin
  inherited;

  // When the inplace editor is shown with the mouse, the mouse technically
  // leaves the grid because the mouse is over the inplace editor. However,
  // for purposes of the custom framing and focus color changes, we can
  // consider the mouse to be still over the grid.
  if ( InplaceEditor = nil ) or
     ( ( InplaceEditor <> nil ) and not InplaceEditor.Focused ) then
  begin
    UpdateFrame( True, False );
  end;
end;


procedure TRzStringGrid.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzStringGrid.IsActiveControl: Boolean;
var
  H: Hwnd;
  ParentForm: TCustomForm;
begin
  Result := False;
  ParentForm := GetParentForm( Self );
  if Assigned( ParentForm ) then
  begin
    if ParentForm.ActiveControl = Self then
      Result := True
  end
  else
  begin
    H := GetFocus;
    while IsWindow( H ) and ( Result = False ) do
    begin
      if H = WindowHandle then
        Result := True
      else
        H := GetParent( H );
    end;
  end;
end; {= TRzStringGrid.IsActiveControl =}


procedure TRzStringGrid.Paint;
var
  LC: TColor;
  DrawInfo: TGridDrawInfo;
  Sel, FocusSelection: TGridRect;
  UpdateRect: TRect;
  AFocRect, FocRect: TRect;
  PointsList: PIntArray;
  StrokeList: PIntArray;
  MaxStroke: Integer;

  {---------------------------------------------------------
    DrawLines

    CellBounds is 4 integers: StartX, StartY, StopX, StopY
    Horizontal lines:         MajorIndex = 0
    Vertical lines:           MajorIndex = 1
  ---------------------------------------------------------}
  procedure DrawLines( DoHorz, DoVert: Boolean; Col, Row: Longint; const CellBounds: array of Integer;
                       OnColor, OffColor: TColor);
  const
    FlatPenStyle = ps_Geometric or ps_Solid or ps_EndCap_Flat or ps_Join_Miter;

    procedure DrawAxisLines( const AxisInfo: TGridAxisDrawInfo; Cell, MajorIndex: Integer; UseOnColor: Boolean );
    var
      Line: Integer;
      LogBrush: TLogBrush;
      Index: Integer;
      Points: PIntArray;
      StopMajor, StartMinor, StopMinor, StopIndex: Integer;
      LineIncr: Integer;
    begin
      if AxisInfo.EffectiveLineWidth <> 0 then
      begin
        Canvas.Pen.Width := GridLineWidth;
        if UseOnColor then
          Canvas.Pen.Color := OnColor
        else
          Canvas.Pen.Color := OffColor;

        if Canvas.Pen.Width > 1 then
        begin
          LogBrush.lbStyle := bs_Solid;
          LogBrush.lbColor := Canvas.Pen.Color;
          LogBrush.lbHatch := 0;
          Canvas.Pen.Handle := ExtCreatePen( FlatPenStyle, Canvas.Pen.Width, LogBrush, 0, nil );
        end;

        Points := PointsList;
        Line := CellBounds[ MajorIndex ] + AxisInfo.EffectiveLineWidth shr 1 + AxisInfo.GetExtent( Cell );

        if UseRightToLeftAlignment and ( MajorIndex = 0 ) then
          Inc( Line );

        StartMinor := CellBounds[ MajorIndex xor 1 ];
        StopMinor := CellBounds[ 2 + ( MajorIndex xor 1 ) ];
        StopMajor := CellBounds[ 2 + MajorIndex ] + AxisInfo.EffectiveLineWidth;
        StopIndex := MaxStroke * 4;

        Index := 0;
        repeat
          Points^[ Index + MajorIndex ] := Line;           // MoveTo
          Points^[ Index + ( MajorIndex xor 1 ) ] := StartMinor;
          Inc( Index, 2 );
          Points^[ Index + MajorIndex ] := Line;           // LineTo
          Points^[ Index + ( MajorIndex xor 1 ) ] := StopMinor;
          Inc( Index, 2 );

          // Skip hidden columns/rows.  We don't have stroke slots for them
          // A column/row with an extent of -EffectiveLineWidth is hidden
          repeat
            Inc( Cell );
            LineIncr := AxisInfo.GetExtent( Cell ) + AxisInfo.EffectiveLineWidth;
          until ( LineIncr > 0 ) or ( Cell > AxisInfo.LastFullVisibleCell );
          Inc( Line, LineIncr );
        until ( Line > StopMajor ) or ( Cell > AxisInfo.LastFullVisibleCell ) or ( Index > StopIndex );

        // 2 integers per point, 2 points per line -> Index div 4
        PolyPolyLine( Canvas.Handle, Points^, StrokeList^, Index shr 2 );
      end;
    end; {= DrawAxisLines =}

  begin {= DrawLines =}
    if ( CellBounds[ 0 ] = CellBounds[ 2 ] ) or ( CellBounds[ 1 ] = CellBounds[ 3 ] ) then
      Exit;

    if not DoHorz then
    begin
      DrawAxisLines( DrawInfo.Vert, Row, 1, DoHorz );
      DrawAxisLines( DrawInfo.Horz, Col, 0, DoVert );
    end
    else
    begin
      DrawAxisLines( DrawInfo.Horz, Col, 0, DoVert );
      DrawAxisLines( DrawInfo.Vert, Row, 1, DoHorz );
    end;
  end; {= DrawLines =}


  procedure DrawCells( ACol, ARow: Longint; StartX, StartY, StopX, StopY: Integer; Color: TColor;
                       IncludeDrawState: TGridDrawState );
  var
    CurCol, CurRow: Longint;
    AWhere, Where: TRect;
    DrawState: TGridDrawState;
    Focused: Boolean;
  begin
    CurRow := ARow;
    Where.Top := StartY;
    while ( Where.Top < StopY ) and ( CurRow < RowCount ) do
    begin
      CurCol := ACol;
      Where.Left := StartX;
      Where.Bottom := Where.Top + RowHeights[ CurRow ];
      while ( Where.Left < StopX ) and ( CurCol < ColCount ) do
      begin
        Where.Right := Where.Left + ColWidths[ CurCol ];
        if ( Where.Right > Where.Left ) and RectVisible( Canvas.Handle, Where ) then
        begin
          DrawState := IncludeDrawState;
          Focused := IsActiveControl;
          if Focused and ( CurRow = Row ) and ( CurCol = Col ) then
            Include( DrawState, gdFocused );

          if PointInGridRect( CurCol, CurRow, Sel ) then
            Include( DrawState, gdSelected );

          if not ( gdFocused in DrawState ) or
             not ( goEditing in Options ) or
             not EditorMode or
             ( csDesigning in ComponentState ) then
          begin
            if DefaultDrawing or ( csDesigning in ComponentState ) then
            begin
              Canvas.Font := Self.Font;
              if ( gdSelected in DrawState ) and
                 ( not ( gdFocused in DrawState ) or
                   ( [ goDrawFocusSelected, goRowSelect ] * Options <> [] ) ) then
              begin
                Canvas.Brush.Color := clHighlight;
                Canvas.Font.Color := clHighlightText;
              end
              else
                Canvas.Brush.Color := Color;
              Canvas.FillRect( Where );
            end;

            DrawCell( CurCol, CurRow, Where, DrawState );

            (*
            The last step performed by the call to DrawCell above is the following. This is what gives the fixed cells
            a raised look in the inherited grid. We simply paint over this to remove the raised appearance.

            if ( gdFixed in AState ) and ( [ dgRowLines, dgColLines ] * Options = [ dgRowLines, dgColLines ] ) then
            begin
              InflateRect( ARect, 1, 1 );
              DrawEdge( Canvas.Handle, ARect, BDR_RAISEDINNER, BF_BOTTOMRIGHT );
              DrawEdge( Canvas.Handle, ARect, BDR_RAISEDINNER, BF_TOPLEFT );
            end;
            *)

            (*
            // The following block was commented out in 3.0.11 b/c it was getting
            // in the way of drawing the header with a gradient fill. However,
            // removing it does not appear to have any affect on normal operation.

            if ( gdFixed in DrawState ) then
            begin
              // Remove raised appearance from fixed cells
              Canvas.Brush.Style := bsClear;
              Canvas.Pen.Color := FixedColor;
              Canvas.Rectangle( Where );
              Canvas.Brush.Style := bsSolid;
            end;
            *)

            if DefaultDrawing and not ( csDesigning in ComponentState ) and ( gdFocused in DrawState ) and
               ( [ goEditing, goAlwaysShowEditor ] * Options <> [ goEditing, goAlwaysShowEditor ] )
               and not ( goRowSelect in Options ) then
            begin
              if not UseRightToLeftAlignment then
                DrawFocusRect( Canvas.Handle, Where )
              else
              begin
                AWhere := Where;
                AWhere.Left := Where.Right;
                AWhere.Right := Where.Left;
                DrawFocusRect( Canvas.Handle, AWhere );
              end;
            end;
          end;
        end;
        Where.Left := Where.Right + DrawInfo.Horz.EffectiveLineWidth;
        Inc( CurCol );
      end;
      Where.Top := Where.Bottom + DrawInfo.Vert.EffectiveLineWidth;
      Inc( CurRow );
    end;
  end; {= DrawCells =}

begin {= TRzStringGrid.Paint =}

  {$IFDEF VCL140_OR_HIGHER}

  if UseDrawingStyle then
  begin
    // In Delphi 2010, the grid's interior appearance has been updated with DrawingStyle property.
    inherited;
    Exit;
  end;
  {$ENDIF}

  
  if UseRightToLeftAlignment then
    ChangeGridOrientation( True );

  UpdateRect := Canvas.ClipRect;
  CalcDrawInfo( DrawInfo );

  if ( DrawInfo.Horz.EffectiveLineWidth > 0 ) or ( DrawInfo.Vert.EffectiveLineWidth > 0 ) then
  begin
    // Draw the grid line in the four areas (fixed, fixed), (variable, fixed), (fixed, variable) and
    // (variable, variable).

    LC := FLineColor;
    MaxStroke := Max( DrawInfo.Horz.LastFullVisibleCell - LeftCol + FixedCols,
                      DrawInfo.Vert.LastFullVisibleCell - TopRow + FixedRows ) + 3;

    PointsList := StackAlloc( MaxStroke * SizeOf( TPoint ) * 2 );
    StrokeList := StackAlloc( MaxStroke * SizeOf( Integer ) );
    FillDWord( StrokeList^, MaxStroke, 2 );

    if ColorToRGB( LineColor ) = ColorToRGB( Color ) then
      LC := DarkerColor( LineColor, 50 );

    DrawLines( goFixedHorzLine in Options,
               goFixedVertLine in Options,
               0, 0,
               [ 0, 0, DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.FixedBoundary ],
               FFixedLineColor, FixedColor );

    DrawLines( goFixedHorzLine in Options,
               goFixedVertLine in Options,
               LeftCol, 0,
               [ DrawInfo.Horz.FixedBoundary, 0, DrawInfo.Horz.GridBoundary, DrawInfo.Vert.FixedBoundary],
               FFixedLineColor, FixedColor );

    DrawLines( goFixedHorzLine in Options,
               goFixedVertLine in Options,
               0, TopRow,
               [ 0, DrawInfo.Vert.FixedBoundary, DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.GridBoundary ],
               FFixedLineColor, FixedColor );

    DrawLines( goHorzLine in Options,
               goVertLine in Options,
               LeftCol, TopRow,
               [ DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.FixedBoundary, DrawInfo.Horz.GridBoundary,
                 DrawInfo.Vert.GridBoundary ],
               LC, Color );

    StackFree( StrokeList );
    StackFree( PointsList );
  end;

  // Draw the cells in the four areas
  Sel := Selection;

  DrawCells( 0, 0, 0, 0, DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.FixedBoundary, FixedColor, [ gdFixed ] );

  DrawCells( LeftCol, 0, DrawInfo.Horz.FixedBoundary {- FColOffset}, 0, DrawInfo.Horz.GridBoundary,
             DrawInfo.Vert.FixedBoundary, FixedColor, [ gdFixed ] );

  DrawCells( 0, TopRow, 0, DrawInfo.Vert.FixedBoundary, DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.GridBoundary,
             FixedColor, [ gdFixed ] );

  DrawCells( LeftCol, TopRow, DrawInfo.Horz.FixedBoundary {- FColOffset},
             DrawInfo.Vert.FixedBoundary, DrawInfo.Horz.GridBoundary, DrawInfo.Vert.GridBoundary, Color, [] );

  if not ( csDesigning in ComponentState ) and ( goRowSelect in Options ) and
     DefaultDrawing and Focused then
  begin
    FocusSelection := GetFocusSelection;
    GridRectToScreenRect( FocusSelection, FocRect, False );

    if not UseRightToLeftAlignment then
      Canvas.DrawFocusRect( FocRect )
    else
    begin
      AFocRect := FocRect;
      AFocRect.Left := FocRect.Right;
      AFocRect.Right := FocRect.Left;
      DrawFocusRect( Canvas.Handle, AFocRect );
    end;
  end;

  // Fill in area not occupied by cells
  if DrawInfo.Horz.GridBoundary < DrawInfo.Horz.GridExtent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect( Rect( DrawInfo.Horz.GridBoundary, 0,
                           DrawInfo.Horz.GridExtent,
                           DrawInfo.Vert.GridBoundary ) );
  end;

  if DrawInfo.Vert.GridBoundary < DrawInfo.Vert.GridExtent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect( Rect( 0, DrawInfo.Vert.GridBoundary,
                           DrawInfo.Horz.GridExtent,
                           DrawInfo.Vert.GridExtent ) );
  end;

  if UseRightToLeftAlignment then
    ChangeGridOrientation( False );

end; {= TRzStringGrid.Paint =}


function TRzStringGrid.GetFocusSelection: TGridRect;
begin
  // This method can be overridden by descendant classes to adjust which cells
  // of the grid show the focus rect.
  Result := Selection;
end;


procedure TRzStringGrid.GridRectToScreenRect( GridRect: TGridRect;
                                              var ScreenRect: TRect;
                                              IncludeLine: Boolean );

  function LinePos( const AxisInfo: TGridAxisDrawInfo; Line: Integer ): Integer;
  var
    Start, I: Longint;
  begin
    Result := 0;
    if Line < AxisInfo.FixedCellCount then
      Start := 0
    else
    begin
      if Line >= AxisInfo.FirstGridCell then
        Result := AxisInfo.FixedBoundary;
      Start := AxisInfo.FirstGridCell;
    end;
    for I := Start to Line - 1 do
    begin
      Inc( Result, AxisInfo.GetExtent( I ) + AxisInfo.EffectiveLineWidth );
      if Result > AxisInfo.GridExtent then
      begin
        Result := 0;
        Exit;
      end;
    end;
  end; {= LinePos =}


  function CalcAxis( const AxisInfo: TGridAxisDrawInfo;
                     GridRectMin, GridRectMax: Integer;
                     var ScreenRectMin, ScreenRectMax: Integer ): Boolean;
  begin
    Result := False;
    if ( GridRectMin >= AxisInfo.FixedCellCount ) and ( GridRectMin < AxisInfo.FirstGridCell ) then
    begin
      if GridRectMax < AxisInfo.FirstGridCell then
      begin
        FillChar( ScreenRect, SizeOf( ScreenRect ), 0 ); // Erase partial results
        Exit;
      end
      else
        GridRectMin := AxisInfo.FirstGridCell;
    end;

    if GridRectMax > AxisInfo.LastFullVisibleCell then
    begin
      GridRectMax := AxisInfo.LastFullVisibleCell;
      if GridRectMax < AxisInfo.GridCellCount - 1 then
        Inc( GridRectMax );

      if LinePos( AxisInfo, GridRectMax ) = 0 then
        Dec( GridRectMax );
    end;

    if GridRectMin > GridRectMax then
    begin
      FillChar( ScreenRect, SizeOf( ScreenRect ), 0 ); // Erase partial results
      Exit;
    end;

    ScreenRectMin := LinePos( AxisInfo, GridRectMin );
    ScreenRectMax := LinePos( AxisInfo, GridRectMax );

    if ScreenRectMax = 0 then
      ScreenRectMax := ScreenRectMin + AxisInfo.GetExtent( GridRectMin )
    else
      Inc( ScreenRectMax, AxisInfo.GetExtent( GridRectMax ) );

    if ScreenRectMax > AxisInfo.GridExtent then
      ScreenRectMax := AxisInfo.GridExtent;
    if IncludeLine then
      Inc( ScreenRectMax, AxisInfo.EffectiveLineWidth );

    Result := True;
  end; {= CalcAxis =}

var
  DrawInfo: TGridDrawInfo;
  Hold: Integer;
begin {= TRzStringGrid.GridRectToScreenRect =}
  FillChar( ScreenRect, SizeOf( ScreenRect ), 0 );
  if ( GridRect.Left > GridRect.Right ) or ( GridRect.Top > GridRect.Bottom ) then
    Exit;

  CalcDrawInfo( DrawInfo );
  if GridRect.Left > DrawInfo.Horz.LastFullVisibleCell + 1 then
    Exit;

  if GridRect.Top > DrawInfo.Vert.LastFullVisibleCell + 1 then
    Exit;

  if CalcAxis( DrawInfo.Horz, GridRect.Left, GridRect.Right,
               ScreenRect.Left, ScreenRect.Right ) then
  begin
    CalcAxis( DrawInfo.Vert, GridRect.Top, GridRect.Bottom,
              ScreenRect.Top, ScreenRect.Bottom );
  end;

  if UseRightToLeftAlignment and ( Canvas.CanvasOrientation = coLeftToRight ) then
  begin
    Hold := ScreenRect.Left;
    ScreenRect.Left := ClientWidth - ScreenRect.Right;
    ScreenRect.Right := ClientWidth - Hold;
  end;
end; {= TRzStringGrid.GridRectToScreenRect =}


{&RUIF}
end.
