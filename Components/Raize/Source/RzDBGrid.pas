{===============================================================================
  RzDBGrid Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBGrid
    Enhanced DBGrid that supports Custom Framing and additional display options
    for grid cells.


  Modification History
  ------------------------------------------------------------------------------
  6.1.3  (01 May 2013)
    * Fixed issue where adding a column to the TRzDBGrid.Columns collection
      would cause an access violation.
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to TRzDBGrid to fully support VCL Styles
      introduced in RAD Studio XE2. Including automatic color adjustment for
      alternate rows when the AltRowShading property is set to True.
    * Made necessary modifications to TRzDBGrid to support 64-bit development.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Fixed issue where highlighted cells in the TRzDBGrid would not appear
      correctly in RAD Studio 2010.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed issue where setting the TRzDBGrid.QuickCompare.FieldValue to a non
      integer value would raise and exception.
    * Updated the TRzDBGrid to take advantage of the new TDBGrid display
      capabilities added in RAD Studio 2010.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed problem where setting TRzDBGrid.AltRowShading to True at design-time
      would not get picked up at runtime.
    * If the total number of records in the dataset can be displayed completely
      in a TRzDBGrid, then the vertical scroll bar is not displayed.
    * If a vertical scroll bar is displayed in a TRzDBGrid, the UpArrow and
      DownArrow buttons on the scroll bar move the current record up or down by
      1 rather than jumping to the opposite side (top or bottom) of the grid
      as is done in the base TDBGrid. The result is a much more natural
      scrolling experience.
  ------------------------------------------------------------------------------
  4.3    (13 Sep 2007)
    * Added new QuickCompare property to TRzDBGrid, which provides a fast and
      simple way to highlight specific records in the dataset by comparing a
      specified field in each record with a value. The QuickCompare property
      has several sub-properties for specifying the criteria of the comparison
      and the resulting affect on the grid's display:
        QuickCompare.Active
          Controls whether QuickCompare is performed.
        QuickCompare.FieldName
          Name of field in dataset used in comparision.
        QuickCompare.FieldValue
          Value used in comparison
        QuickCompare.Operation
          Specifies the type of operation:
            qcoEqual, qcoLessThan, qcoLessThanOrEqual,
            qcoGreaterThan, qcoGreaterThanOrEqual, qcoNotEqual
        QuickCompare.Color
          Color of record if comparison is successful
        QuickCompare.FontColor
          Font color of record if comparison is successful
    * Added new AltRowShading, AltRowShadingColor, and AltRowShadingFixed
      properties to TRzDBGrid.  When AltRowShading is set to True, the
      background color of alternate rows in the grid are shaded using the
      AltRowShadingColor. The AltRowShadingFixed property controls whether or
      not the shading remains fixed as you scroll through the grid.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed problem where focus color of the grid would change if the inplace
      editor was invoked using the mouse.
    * Redesigned OnMouseEnter and OnMouseLeave events in TRzDBGrid to account
      for changes introduced in Borland Developer Studio 2006.
    * Fixed custom framing display problem that would occur in TRzDBGrid when
      FrameVisible was set to True and changes were made to control's appearance
      within calls to LockWindowUpdate.
    * Added new FrameControllerNotifications property to TRzDBGrid.
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
    * Surfaced OnResize event in TRzDBGrid.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Initial release.
    * The TRzDBGrid is a direct descendant of the VCL TDBGrid that supports
      Custom Framing and additional display options for grid cells.
===============================================================================}

{$I RzComps.inc}

unit RzDBGrid;

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
  DBGrids,
  RzCommon;

const
  clFadedGreen  = TColor( $00EBF5EB );
  clBlueGray = TColor( $00996666 );

type
  {=================================}
  {== TRzDBGrid Class Declaration ==}
  {=================================}

  TRzDBGrid = class;

  TRzQuickCompareOperation = ( qcoEqual, qcoLessThan, qcoLessThanOrEqual,
                               qcoGreaterThan, qcoGreaterThanOrEqual, qcoNotEqual );

  TRzDBGridQuickCompare = class( TPersistent )
  private
    FGrid: TRzDBGrid;

    FActive: Boolean;
    FFieldName: string;
    FFieldValue: Variant;
    FOperation: TRzQuickCompareOperation;
    FColor: TColor;
    FFontColor: TColor;
  protected
    procedure SetActive( Value: Boolean );
    procedure SetFieldName( const Value: string );
    procedure SetFieldValue( Value: Variant );
    function StoreFieldValue: Boolean;
    procedure SetOperation( Value: TRzQuickCompareOperation );
    procedure SetColor( Value: TColor );
    procedure SetFontColor( Value: TColor );
  public
    constructor Create;

    property Grid: TRzDBGrid
      read FGrid;
  published
    property Active: Boolean
      read FActive
      write SetActive
      default False;

    property FieldName: string
      read FFieldName
      write SetFieldName;

    property FieldValue: Variant
      read FFieldValue
      write SetFieldValue
      stored StoreFieldValue;

    property Operation: TRzQuickCompareOperation
      read FOperation
      write SetOperation
      default qcoEqual;

    property Color: TColor
      read FColor
      write SetColor
      default clBlueGray;

    property FontColor: TColor
      read FFontColor
      write SetFontColor
      default clWhite;
  end;


  TRzDBGrid = class( TDBGrid )
  private
    FLineColor: TColor;
    FFixedLineColor: TColor;

    FQuickCompare: TRzDBGridQuickCompare;
    FAltRowShading: Boolean;
    FAltRowShadingColor: TColor;
    FAltRowShadingFixed: Boolean;
    FLocalDefaultDrawing: Boolean;

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

    // Re-implementation of private methods in base class to handle scrolling
    function AcquireFocus: Boolean;
    procedure WMVScroll( var Msg: TWMVScroll ); message wm_VScroll;

    // Message Handling Methods
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

    procedure DrawColumnCell( const Rect: TRect; DataCol: Integer;
                              Column: TColumn; State: TGridDrawState ); override;
    procedure Scroll( Distance: Integer ); override;
                             
    procedure UpdateScrollBar; override;
    procedure Resize; override;

    function IsActiveControl: Boolean;
    procedure Paint; override;
    procedure GridRectToScreenRect( GridRect: TGridRect; var ScreenRect: TRect; IncludeLine: Boolean );

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
    procedure SetQuickCompare( Value: TRzDBGridQuickCompare ); virtual;
    procedure SetAltRowShading( Value: Boolean );
    procedure SetAltRowShadingColor( Value: TColor );
    procedure SetAltRowShadingFixed( Value: Boolean );
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

    property QuickCompare: TRzDBGridQuickCompare
      read FQuickCompare
      write SetQuickCompare;

    property AltRowShading: Boolean
      read FAltRowShading
      write SetAltRowShading
      default False;

    property AltRowShadingColor: TColor
      read FAltRowShadingColor
      write SetAltRowShadingColor
      default clFadedGreen;

    property AltRowShadingFixed: Boolean
      read FAltRowShadingFixed
      write SetAltRowShadingFixed
      default True;

    // Redefined for AltRowShading and QuickCompare
    property DefaultDrawing: Boolean
      read FLocalDefaultDrawing
      write FLocalDefaultDrawing
      default True;

    { Inherited Properties & Events }
    property Height default 125;
    property Width default 325;

    property OnResize;
  end;


implementation

uses
  {&RAS}
  Themes,
  UxTheme,
  DB,
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

{===================================}
{== TRzDBGridQuickCompare Methods ==}
{===================================}

constructor TRzDBGridQuickCompare.Create;
begin
  FActive := False;
  FFieldName := '';
  FFieldValue := varNull;
  FOperation := qcoEqual;
  FColor := clBlueGray;
  FFontColor := clWhite;
end;


procedure TRzDBGridQuickCompare.SetActive( Value: Boolean );
begin
  if FActive <> Value then
  begin
    FActive := Value;
    FGrid.Invalidate;
  end;
end;


procedure TRzDBGridQuickCompare.SetFieldName( const Value: string );
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    FGrid.Invalidate;
  end;
end;


procedure TRzDBGridQuickCompare.SetFieldValue( Value: Variant );
begin
  FFieldValue := Value;
  FGrid.Invalidate;
end;


function TRzDBGridQuickCompare.StoreFieldValue: Boolean;
begin
  Result := FFieldValue <> varNull;
end;


procedure TRzDBGridQuickCompare.SetOperation( Value: TRzQuickCompareOperation );
begin
  if FOperation <> Value then
  begin
    FOperation := Value;
    FGrid.Invalidate;
  end;
end;


procedure TRzDBGridQuickCompare.SetColor( Value: TColor );
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGrid.Invalidate;
  end;
end;


procedure TRzDBGridQuickCompare.SetFontColor( Value: TColor );
begin
  if FFontColor <> Value then
  begin
    FFontColor := Value;
    FGrid.Invalidate;
  end;
end;


{=======================}
{== TRzDBGrid Methods ==}
{=======================}

constructor TRzDBGrid.Create( AOwner: TComponent );
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

  Height := 125;
  Width := 325;

  FLocalDefaultDrawing := True;

  FQuickCompare := TRzDBGridQuickCompare.Create;
  FQuickCompare.FGrid := Self;

  FAltRowShading := False;
  FAltRowShadingColor := clFadedGreen;
  FAltRowShadingFixed := True;

  inherited DefaultDrawing := False;
  {&RV}
end;


destructor TRzDBGrid.Destroy;
begin
  if FFrameController <> nil then
    FFrameController.RemoveControl( Self );
  FQuickCompare.Free;
  FCanvas.Free;
  inherited;
end;


procedure TRzDBGrid.Loaded;
begin
  inherited;
  UpdateColors;
  UpdateFrame( False, False );
end;


procedure TRzDBGrid.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFrameController ) then
    FFrameController := nil;
end;


function TRzDBGrid.CreateEditor: TInplaceEdit;
begin
  Result := inherited CreateEditor;
  TEdit( Result ).Color := Color;
end;


function TRzDBGrid.StoreColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpColor in FFrameControllerNotifications ) ) )
            and
            Enabled;
end;


function TRzDBGrid.StoreFocusColor: Boolean;
begin
  Result := ( ( FFrameController = nil ) or
              ( ( FFrameController <> nil ) and
                not ( fcpFocusColor in FFrameControllerNotifications ) ) )
            and
            ( ColorToRGB( FFocusColor ) <> ColorToRGB( Color ) );
end;


function TRzDBGrid.StoreDisabledColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpDisabledColor in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreParentColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpParentColor in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFlatButtonColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtonColor in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFlatButtons: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFlatButtons in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameColor in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameHotColor: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotColor in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameHotTrack: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotTrack in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameHotStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameHotStyle in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameSides: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameSides in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameStyle: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameStyle in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFrameVisible: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFrameVisible in FFrameControllerNotifications ) );
end;


function TRzDBGrid.StoreFramingPreference: Boolean;
begin
  Result := ( FFrameController = nil ) or
            ( ( FFrameController <> nil ) and
              not ( fcpFramingPreference in FFrameControllerNotifications ) );
end;


procedure TRzDBGrid.SetDisabledColor( Value: TColor );
begin
  FDisabledColor := Value;
  if not Enabled then
    UpdateColors;
end;


procedure TRzDBGrid.SetFocusColor( Value: TColor );
begin
  FFocusColor := Value;
  if Focused then
    UpdateColors;
end;


procedure TRzDBGrid.SetFrameColor( Value: TColor );
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBGrid.SetFrameController( Value: TRzFrameController );
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


procedure TRzDBGrid.SetFrameHotColor( Value: TColor );
begin
  if FFrameHotColor <> Value then
  begin
    FFrameHotColor := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBGrid.SetFrameHotTrack( Value: Boolean );
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


procedure TRzDBGrid.SetFrameHotStyle( Value: TFrameStyle );
begin
  if FFrameHotStyle <> Value then
  begin
    FFrameHotStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBGrid.SetFrameSides( Value: TSides );
begin
  if FFrameSides <> Value then
  begin
    FFrameSides := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBGrid.SetFrameStyle( Value: TFrameStyle );
begin
  if FFrameStyle <> Value then
  begin
    FFrameStyle := Value;
    RepaintFrame;
  end;
end;


procedure TRzDBGrid.SetFrameVisible( Value: Boolean );
begin
  if FFrameVisible <> Value then
  begin
    FFrameVisible := Value;
    if FFrameVisible then
      Ctl3D := True;
    RecreateWnd;
  end;
end;


procedure TRzDBGrid.SetFramingPreference( Value: TFramingPreference );
begin
  if FFramingPreference <> Value then
  begin
    FFramingPreference := Value;
    if FFramingPreference = fpCustomFraming then
      RepaintFrame;
  end;
end;


procedure TRzDBGrid.RepaintFrame;
begin
  InvalidateWindowFrame( Handle, ClientRect );
end;


function TRzDBGrid.UseThemes: Boolean;
begin
  Result := ( FFramingPreference = fpXPThemes ) and ActiveStyleServicesEnabled;
end;


procedure TRzDBGrid.CMColorChanged( var Msg: TMessage );
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


procedure TRzDBGrid.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  UpdateColors;
end;


procedure TRzDBGrid.SetFixedLineColor( Value: TColor );
begin
  if FFixedLineColor <> Value then
  begin
    FFixedLineColor := Value;
    Invalidate;
  end;
end;


procedure TRzDBGrid.SetLineColor( Value: TColor );
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;


procedure TRzDBGrid.SetQuickCompare( Value: TRzDBGridQuickCompare );
begin
  FQuickCompare.Assign( Value );
end;


procedure TRzDBGrid.SetAltRowShading( Value: Boolean );
begin
  if FAltRowShading <> Value then
  begin
    FAltRowShading := Value;
    Invalidate;
  end;
end;


procedure TRzDBGrid.SetAltRowShadingColor( Value: TColor );
begin
  if FAltRowShadingColor <> Value then
  begin
    FAltRowShadingColor := Value;
    Invalidate;
  end;
end;


procedure TRzDBGrid.SetAltRowShadingFixed( Value: Boolean );
begin
  if FAltRowShadingFixed <> Value then
  begin
    FAltRowShadingFixed := Value;
    Invalidate;
  end;
end;


procedure TRzDBGrid.WMNCPaint( var Msg: TWMNCPaint );
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

begin {= TRzDBGrid.WMNCPaint =}
  inherited;                       { Must call inherited so scroll bar show up }

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
end; {= TRzDBGrid.WMNCPaint =}


procedure TRzDBGrid.CMParentColorChanged( var Msg: TMessage );
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


procedure TRzDBGrid.UpdateColors;
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


procedure TRzDBGrid.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  if ViaMouse then
    FOverControl := InFocus;

  if FFrameHotTrack then
    RepaintFrame;

  UpdateColors;
end;


procedure TRzDBGrid.CMEnter( var Msg: TCMEnter );
begin
  UpdateFrame( False, True );
  inherited;
end;


procedure TRzDBGrid.CMExit( var Msg: TCMExit );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzDBGrid.CMMouseEnter( var Msg: TMessage );
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


procedure TRzDBGrid.CMMouseLeave( var Msg: TMessage );
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


procedure TRzDBGrid.WMSize( var Msg: TWMSize );
begin
  inherited;
  if FFrameVisible and not UseThemes then
    RepaintFrame;
end;


function TRzDBGrid.IsActiveControl: Boolean;
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
end; {= TRzDBGrid.IsActiveControl =}



// This access class is needed in order to access the Options property from the TCustomGrid class, which is a different
// type from the Options property in the TDBGrid class.

type
  TCustomGridAccess = class( TCustomGrid );


procedure TRzDBGrid.Paint;
{$IFNDEF VCL140_OR_HIGHER}
var
  LC: TColor;
  DrawInfo: TGridDrawInfo;
  Sel: TGridRect;
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
             not ( goEditing in TCustomGridAccess( Self ).Options ) or
             not EditorMode or
             ( csDesigning in ComponentState ) then
          begin
            if DefaultDrawing or ( csDesigning in ComponentState ) then
            begin
              Canvas.Font := Self.Font;
              if ( gdSelected in DrawState ) and
                 ( not ( gdFocused in DrawState ) or
                   ( [ goDrawFocusSelected, goRowSelect ] * TCustomGridAccess( Self ).Options <> [] ) ) then
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

            if ( gdFixed in DrawState ) then
            begin
              // Remove raised appearance from fixed cells
              Canvas.Brush.Style := bsClear;
              Canvas.Pen.Color := FixedColor;
              Canvas.Rectangle( Where );
              Canvas.Brush.Style := bsSolid;
            end;

            if DefaultDrawing and not ( csDesigning in ComponentState ) and ( gdFocused in DrawState ) and
               ( [goEditing, goAlwaysShowEditor] * TCustomGridAccess(Self).Options <> [goEditing, goAlwaysShowEditor] )
               and not ( goRowSelect in TCustomGridAccess( Self ).Options ) then
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
{$ENDIF}

begin {= TRzDBGrid.Paint =}
  {$IFDEF VCL140_OR_HIGHER}

  inherited;

  {$ELSE}

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

    DrawLines( goFixedHorzLine in TCustomGridAccess( Self ).Options,
               goFixedVertLine in TCustomGridAccess( Self ).Options,
               0, 0,
               [ 0, 0, DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.FixedBoundary ],
               FFixedLineColor, FixedColor );

    DrawLines( goFixedHorzLine in TCustomGridAccess( Self ).Options,
               goFixedVertLine in TCustomGridAccess( Self ).Options,
               LeftCol, 0,
               [ DrawInfo.Horz.FixedBoundary, 0, DrawInfo.Horz.GridBoundary, DrawInfo.Vert.FixedBoundary],
               FFixedLineColor, FixedColor );

    DrawLines( goFixedHorzLine in TCustomGridAccess( Self ).Options,
               goFixedVertLine in TCustomGridAccess( Self ).Options,
               0, TopRow,
               [ 0, DrawInfo.Vert.FixedBoundary, DrawInfo.Horz.FixedBoundary, DrawInfo.Vert.GridBoundary ],
               FFixedLineColor, FixedColor );

    DrawLines( goHorzLine in TCustomGridAccess( Self ).Options,
               goVertLine in TCustomGridAccess( Self ).Options,
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

  if not ( csDesigning in ComponentState ) and ( goRowSelect in TCustomGridAccess( Self ).Options ) and
     DefaultDrawing and Focused then
  begin
    GridRectToScreenRect( Selection, FocRect, False );
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
    Canvas.FillRect( Rect( DrawInfo.Horz.GridBoundary, 0, DrawInfo.Horz.GridExtent, DrawInfo.Vert.GridBoundary ) );
  end;

  if DrawInfo.Vert.GridBoundary < DrawInfo.Vert.GridExtent then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect( Rect( 0, DrawInfo.Vert.GridBoundary, DrawInfo.Horz.GridExtent, DrawInfo.Vert.GridExtent ) );
  end;

  if UseRightToLeftAlignment then
    ChangeGridOrientation( False );

  {$ENDIF}
end; {= TRzDBGrid.Paint =}


procedure TRzDBGrid.GridRectToScreenRect( GridRect: TGridRect; var ScreenRect: TRect; IncludeLine: Boolean );

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


  function CalcAxis( const AxisInfo: TGridAxisDrawInfo; GridRectMin, GridRectMax: Integer;
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
begin {= TRzDBGrid.GridRectToScreenRect =}
  FillChar( ScreenRect, SizeOf( ScreenRect ), 0 );
  if ( GridRect.Left > GridRect.Right ) or ( GridRect.Top > GridRect.Bottom ) then
    Exit;

  CalcDrawInfo( DrawInfo );
  if GridRect.Left > DrawInfo.Horz.LastFullVisibleCell + 1 then
    Exit;

  if GridRect.Top > DrawInfo.Vert.LastFullVisibleCell + 1 then
    Exit;

  if CalcAxis( DrawInfo.Horz, GridRect.Left, GridRect.Right, ScreenRect.Left, ScreenRect.Right ) then
  begin
    CalcAxis( DrawInfo.Vert, GridRect.Top, GridRect.Bottom, ScreenRect.Top, ScreenRect.Bottom );
  end;

  if UseRightToLeftAlignment and ( Canvas.CanvasOrientation = coLeftToRight ) then
  begin
    Hold := ScreenRect.Left;
    ScreenRect.Left := ClientWidth - ScreenRect.Right;
    ScreenRect.Right := ClientWidth - Hold;
  end;
end; {= TRzDBGrid.GridRectToScreenRect =}


procedure TRzDBGrid.Scroll( Distance: Integer );
begin
  if FAltRowShading and FAltRowShadingFixed then
  begin
    inherited Scroll( 0 );     // In this case we have to repaint all the grid
    Invalidate;
  end
  else
    inherited Scroll( Distance );
end;


procedure TRzDBGrid.DrawColumnCell( const Rect: TRect; DataCol: Integer;
                                    Column: TColumn; State: TGridDrawState );
var
  Success: Boolean;
  {$IFDEF VCL140_OR_HIGHER}
  S: string;
  {$ENDIF}
begin
  // Set cell background color the right way before calling user event
  if FAltRowShading then
  begin
    if ( FAltRowShadingFixed and ( ( DataLink.ActiveRecord mod 2 ) = 0 ) ) or                // Scrolling background
       ( ( not FAltRowShadingFixed ) and ( ( Column.Field.DataSet.RecNo mod 2 ) = 1 ) ) then // Fixed background
    begin
      if HighlightCell( DataCol, DataLink.ActiveRecord, Column.Field.DisplayText, State ) then
        Canvas.Brush.Color := clHighlight
      else
      begin
        if UsingSystemStyle then
          Canvas.Brush.Color := FAltRowShadingColor
        else
        begin
          Canvas.Brush.Color := BlendColors( ActiveStyleSystemColor( clHighlight ), ActiveStyleSystemColor( clBtnFace ), 50 );
        end;
      end;
    end;
  end;

  if FQuickCompare.Active and ( FQuickCompare.FieldName <> '' ) Then
  begin
    case FQuickCompare.Operation of
      qcoEqual:
        Success := DataSource.DataSet.FieldByName( FQuickCompare.FieldName ).AsVariant = FQuickCompare.FieldValue;

      qcoLessThan:
        Success := DataSource.DataSet.FieldByName( FQuickCompare.FieldName ).AsVariant < FQuickCompare.FieldValue;

      qcoLessThanOrEqual:
        Success := DataSource.DataSet.FieldByName( FQuickCompare.FieldName ).AsVariant <= FQuickCompare.FieldValue;

      qcoGreaterThan:
        Success := DataSource.DataSet.FieldByName( FQuickCompare.FieldName ).AsVariant > FQuickCompare.FieldValue;

      qcoGreaterThanOrEqual:
        Success := DataSource.DataSet.FieldByName( FQuickCompare.FieldName ).AsVariant >= FQuickCompare.FieldValue;

      qcoNotEqual:
        Success := DataSource.DataSet.FieldByName( FQuickCompare.FieldName ).AsVariant <> FQuickCompare.FieldValue;
    else
      Success := False;
    end;

    if Success then
    begin
      Canvas.Brush.Color := FQuickCompare.Color;
      Canvas.Font.Color := FQuickCompare.FontColor;
    end;
  end;


  // If assigned and enabled, call the user drawing, otherwise do default drawing of TDBGrid
  if ( not FLocalDefaultDrawing ) and Assigned( OnDrawColumnCell ) then
    OnDrawColumnCell( Self, Rect, DataCol, Column, State )
  else
  begin
    {$IFDEF VCL140_OR_HIGHER}
    if Column.Field <> nil then
      S := Column.Field.DisplayText
    else
      S := '';
    if HighlightCell( DataCol, DataLink.ActiveRecord, S, State ) and FLocalDefaultDrawing then
      DrawCellHighlight( Rect, State, DataCol, DataLink.ActiveRecord );
    {$ENDIF}
    DefaultDrawColumnCell( Rect, DataCol, Column, State );
  end;
end;


type  // Provides access to protected MoveBy method
  TGridDataLinkAccess = class( TGridDataLink )
  end;

function TRzDBGrid.AcquireFocus: Boolean;
begin
  Result := True;
  if FAcquireFocus and CanFocus and not ( csDesigning in ComponentState ) then
  begin
    SetFocus;
    Result := Focused or ( InplaceEditor <> nil ) and InplaceEditor.Focused;
  end;
end;


procedure TRzDBGrid.WMVScroll( var Msg: TWMVScroll );
var
  SI: TScrollInfo;
begin
  if not AcquireFocus then
    Exit;

  if Datalink.Active then
  begin
    with Msg, DataLink.DataSet do
    begin
      case ScrollCode of
        SB_LINEUP:
          TGridDataLinkAccess( DataLink ).MoveBy( -1 );

        SB_LINEDOWN:
          //TGridDataLinkAccess( DataLink ).MoveBy(Datalink.RecordCount - Datalink.ActiveRecord);
          TGridDataLinkAccess( DataLink ).MoveBy( 1 );

        SB_PAGEUP:
          TGridDataLinkAccess( DataLink ).MoveBy( -VisibleRowCount );

        SB_PAGEDOWN:
          TGridDataLinkAccess( DataLink ).MoveBy( VisibleRowCount );

        SB_THUMBPOSITION:
        begin
          if IsSequenced then
          begin
            SI.cbSize := SizeOf( SI );
            SI.fMask := SIF_ALL;
            GetScrollInfo( Self.Handle, SB_VERT, SI );
            if SI.nTrackPos <= 1 then
              First
            else if SI.nTrackPos >= RecordCount then
              Last
            else
              RecNo := SI.nTrackPos;
          end
          else
          begin
            case Pos of
              0: First;
              1: TGridDataLinkAccess( DataLink ).MoveBy( -VisibleRowCount );
              2: Exit;
              3: TGridDataLinkAccess( DataLink ).MoveBy( VisibleRowCount );
              4: Last;
            end;
          end;
        end;

        SB_BOTTOM:
          Last;

        SB_TOP:
          First;
      end;
    end;
  end;
end;

                                   
procedure TRzDBGrid.UpdateScrollBar;
var
  SIOld, SINew: TScrollInfo;
  H, VRows: Integer;
begin
  if Datalink.Active and HandleAllocated then
  begin
    with Datalink.DataSet do
    begin
      SIOld.cbSize := SizeOf( SIOld );
      SIOld.fMask := SIF_ALL;
      GetScrollInfo( Self.Handle, SB_VERT, SIOld );
      SINew := SIOld;
      if IsSequenced then
      begin
        H := Height - 4;
        // Adjust H if title row is visible
        if dgTitles in Options then
          Dec( H, RowHeights[ 0 ] );

        // Adjust H if horizontal scroll bar is visible
        if ( GetWindowLong( Handle, GWL_STYLE ) and WS_HSCROLL ) = WS_HSCROLL then
          Dec( H, GetSystemMetrics( SM_CYHSCROLL ) );

        VRows := H div ( DefaultRowHeight + 1 );

        if RecordCount > VRows then
        begin
          SINew.nMin := 1;
          SINew.nPage := Self.VisibleRowCount;
          SINew.nMax := Integer( DWORD( RecordCount ) + SINew.nPage - 1 );
          if State in [ dsInactive, dsBrowse, dsEdit ] then
            SINew.nPos := RecNo;  // else keep old pos
        end
        else
        begin
          SINew.nMin := 1;
          SINew.nPage := 3;
          SINew.nMax := 2;
          SINew.nPos := 1;  // else keep old pos
        end;
      end
      else
      begin
        SINew.nMin := 0;
        SINew.nPage := 0;
        SINew.nMax := 4;
        if DataLink.BOF then
          SINew.nPos := 0
        else if DataLink.EOF then
          SINew.nPos := 4
        else
          SINew.nPos := 2;
      end;
      if ( SINew.nMin <> SIOld.nMin ) or ( SINew.nMax <> SIOld.nMax ) or
         ( SINew.nPage <> SIOld.nPage ) or ( SINew.nPos <> SIOld.nPos ) then
        SetScrollInfo( Self.Handle, SB_VERT, SINew, True );
    end;
  end;
end;


procedure TRzDBGrid.Resize;
begin
  inherited;
  UpdateScrollBar;
end;


{&RUIF}
end.



