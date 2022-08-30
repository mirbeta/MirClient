{===============================================================================
  RzDBSpin Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBSpinEdit
    Data-Aware TRzSpinEdit

  TRzDBSpinner
    Data-Aware TRzSpinner


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Cleaned up the appearance of embedded buttons in TRzDBSpinEdit under
      Windows Vista and Windows 7.
    * Made necessary modifications to TRzDBSpinEdit and TRzDBSpinner to fully
      support VCL Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzDBSpinEdit and TRzDBSpinner to support
      64-bit development.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed positioning of TRzDBSpinEdit buttons between XP, Vista, and
      Windows 7 (and when used on a TDBCtrlGrid).
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Changed the TRzDBSpinEdit.IntValue property to be of type Int64.
    * Fixed issue where setting the TRzDBSpinEdit.Max property to 0 would not
      get saved to the DFM file even though the default value of Max is 100.0.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Fixed issue where the TRzDBSpinEdit would not perform range checking
      unless the AllowBlank property was set to False.
    * Fixed issue where underlying database field object would get updated
      before range checking was applied.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new TextHint and TextHintVisibleOnFocus properties to TRzDBSpinEdit.
      The TextHint property allows a developer to specify a prompt that appears
      inside the edit area of the control, when the control is empty. The text
      prompt appears grayed. By default, the prompt is automatically hidden when
      the control receives focus. To keep the prompt visible until the user
      enters a value, set the TextHintVisibleOnFocus property to True.
      NOTES:
        - Please note that for the TextHint to appear, the AllowBlank property
          has to be set to True *and* the Text property has to be empty, which
          occurs during an insert.
        - Please see the comments in RzEdit.pas for more details on TextHint
          and TextHintVisibleOnFocus.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added BeepOnInvalidKey property to TRzDBSpinEdit and TRzDBSpinner.
    * Adjusted positioning and size of spin buttons in TRzDBSpinEdit.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surfaced OnMouseWheel event in TRzDBSpinEdit.
  ------------------------------------------------------------------------------
  4.0.2  (13 Jan 2006)
    * Surfaced new ReadOnlyColorOnFocus proeprty.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Fixed button placement issue in TRzDBSpinEdit when running under RTL
      systems.
    * Added ReadOnlyColor property to TRzDBSpinEdit. This color property is used
      to change the color of the control when the ReadOnly property is set to
      True.
    * When the ReadOnly property for a TRzDBSpinEdit is set to True, the spin
      buttons are hidden.
    * Added new FrameControllerNotifications property to TRzDBSpinEdit.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Fixed problem in CMFontChanged method where SetEditRect could be called
      before a window handle has been allocated for the TRzDBSpinEdit.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * The color of flat buttons in TRzDBSpinEdit are now adjusted appropriately
      when the control is disabled and re-enabled.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzSpinEdit.
    * OnChange event is no longer fired when TRzDBSpinEdit receives the focus.
===============================================================================}

{$I RzComps.inc}

unit RzDBSpin;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Forms,
  StdCtrls,
  DB,
  DBCtrls,
  Graphics,
  SysUtils,
  Classes,
  Buttons,
  Controls,
  ExtCtrls,
  RzCommon,
  RzButton,
  RzDBEdit,
  RzSpnEdt;

type
  {=====================================}
  {== TRzDBSpinEdit Class Declaration ==}
  {=====================================}

  TRzDBSpinEdit = class( TRzDBEdit )
  private
    FAllowKeyEdit: Boolean;
    FAllowBlank: Boolean;
    FBlankValue: Extended;
    FButtons: TRzSpinButtons;
    FButtonWidth: Integer;
    FCheckRange: Boolean;
    FDecimals: Byte;
    FIncrement: Extended;
    FIntegersOnly: Boolean;
    FMin: Extended;
    FMax: Extended;
    FPageSize: Extended;
    FFlatButtonColor: TColor;
    FInternalUpdate: Boolean;

    FOnChanging: TSpinChangingEvent;
    FOnButtonClick: TSpinButtonEvent;

    { Message Handling Methods }
    procedure WMSize( var Msg: TWMSize ); message wm_Size;
    procedure CMEnter( var Msg: TCMEnter ); message cm_Enter;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure WMPaste( var Msg: TWMPaste ); message wm_Paste;
    procedure WMCut( var Msg: TWMCut ); message wm_Cut;
    procedure CMEnabledChanged( var Msg: TMessage ); message cm_EnabledChanged;
    procedure CMFontChanged( var Msg: TMessage ); message cm_FontChanged;
    procedure CMColorChanged( var Msg: TMessage ); message cm_ColorChanged;
  protected
    procedure CreateParams( var Params: TCreateParams ); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); override;
    function CanEditData: Boolean;

    function IsCustomDownGlyph: Boolean;
    function IsCustomUpGlyph: Boolean;

    procedure ReadOnlyChanged; override;
    procedure ResizeButtons; virtual;
    procedure AdjustEditRect; override;
    function GetEditRect: TRect; override;
    procedure SetEditRect; virtual;

    function CleanUpText: string;
    function IsValidChar( Key: Char ): Boolean; virtual;
    procedure UpRightClickHandler( Sender: TObject ); virtual;
    procedure DownLeftClickHandler( Sender: TObject ); virtual;

    { Event Dispatch Methods }
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress(var Key: Char); override;
    function CanChange( NewValue: Extended ): Boolean; dynamic;
    procedure Change; override;
    procedure DoButtonClick( S: TSpinButtonType ); dynamic;
    procedure IncValue( const Amount: Extended ); virtual;
    procedure DecValue( const Amount: Extended ); virtual;

    function DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean; override;
    function DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean; override;

    { Property Access Methods }
    procedure SetFrameStyle( Value: TFrameStyle ); override;
    procedure SetButtonWidth( Value: Integer ); virtual;
    procedure SetDecimals( Value: Byte ); virtual;
    procedure SetIntegersOnly( Value: Boolean ); virtual;

    function GetButton( Index: Integer ): TRzControlButton; virtual;

    function GetButtonUpGlyph: TBitmap; virtual;
    procedure SetButtonUpGlyph( Value: TBitmap ); virtual;
    function GetButtonUpNumGlyphs: TNumGlyphs; virtual;
    procedure SetButtonUpNumGlyphs( Value: TNumGlyphs ); virtual;
    function GetButtonDownGlyph: TBitmap; virtual;
    procedure SetButtonDownGlyph( Value: TBitmap ); virtual;
    function GetButtonDownNumGlyphs: TNumGlyphs; virtual;
    procedure SetButtonDownNumGlyphs( Value: TNumGlyphs ); virtual;

    function GetDirection: TSpinDirection; virtual;
    procedure SetDirection( Value: TSpinDirection ); virtual;
    procedure SetFlatButtons( Value: Boolean ); override;
    function GetOrientation: TOrientation; virtual;
    procedure SetOrientation( Value: TOrientation ); virtual;

    procedure SetCheckRange( Value: Boolean ); virtual;
    procedure SetMin( const Value: Extended ); virtual;
    procedure SetMax( const Value: Extended ); virtual;

    function GetIntValue: Int64; virtual;
    procedure SetIntValue( Value: Int64 ); virtual;
    function GetValue: Extended; virtual;
    function CheckValue( const Value: Extended ): Extended; virtual;
    procedure SetValue( const Value: Extended); virtual;

    function StoreIncrement: Boolean;
    function StorePageSize: Boolean;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;

    property Buttons: TRzSpinButtons
      read FButtons;

    property DownLeftButton: TRzControlButton
      index 1
      read GetButton;

    property UpRightButton: TRzControlButton
      index 2
      read GetButton;

    property IntValue: Int64
      read GetIntValue
      write SetIntValue;
  published
    property AllowBlank: Boolean
      read FAllowBlank
      write FAllowBlank
      default True;

    property BlankValue: Extended
      read FBlankValue
      write FBlankValue;

    property AllowKeyEdit: Boolean
      read FAllowKeyEdit
      write FAllowKeyEdit
      default False;

    property ButtonDownGlyph: TBitmap
      read GetButtonDownGlyph
      write SetButtonDownGlyph
      stored IsCustomDownGlyph;

    property ButtonDownNumGlyphs: TNumGlyphs
      read GetButtonDownNumGlyphs
      write SetButtonDownNumGlyphs
      stored IsCustomDownGlyph;

    property ButtonUpGlyph: TBitmap
      read GetButtonUpGlyph
      write SetButtonUpGlyph
      stored IsCustomUpGlyph;

    property ButtonUpNumGlyphs: TNumGlyphs
      read GetButtonUpNumGlyphs
      write SetButtonUpNumGlyphs
      stored IsCustomUpGlyph;

    property ButtonWidth: Integer
      read FButtonWidth
      write SetButtonWidth
      default 17;

    property CheckRange: Boolean
      read FCheckRange
      write SetCheckRange
      default False;

    property Decimals: Byte
      read FDecimals
      write SetDecimals
      default 0;

    property Direction: TSpinDirection
      read GetDirection
      write SetDirection
      default sdUpDown;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write FFlatButtonColor
      default clBtnFace;

    property Increment: Extended
      read FIncrement
      write FIncrement
      stored StoreIncrement;

    property IntegersOnly: Boolean
      read FIntegersOnly
      write SetIntegersOnly
      default True;

    property Max: Extended
      read FMax
      write SetMax
      stored True;

    property Min: Extended
      read FMin
      write SetMin
      stored True;

    property Orientation: TOrientation
      read GetOrientation
      write SetOrientation
      default orVertical;

    property PageSize: Extended
      read FPageSize
      write FPageSize
      stored StorePageSize;

    property Value: Extended
      read GetValue
      write SetValue;

    property OnChanging: TSpinChangingEvent
      read FOnChanging
      write FOnChanging;

    property OnButtonClick: TSpinButtonEvent
      read FOnButtonClick
      write FOnButtonClick;

    { Inherited Properties & Events }
    property Enabled;
    property FlatButtons;
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
  end;


  {====================================}
  {== TRzDBSpinner Class Declaration ==}
  {====================================}

  TRzDBSpinner = class( TRzSpinner )
  private
    FDataLink: TFieldDataLink;
    FPaintControl: TRzSpinner;
    FBeepOnInvalidKey: Boolean;

    { Internal Event Handlers }
    procedure DataChangeHandler( Sender: TObject );
    procedure UpdateDataHandler( Sender: TObject );
    procedure ActiveChangeHandler( Sender: TObject );

    { Message Handling Methods }
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMDesignHitTest( var Msg: TCMDesignHitTest ); message cm_DesignHitTest;
    procedure CMGetDataLink( var Msg: TMessage ); message cm_GetDataLink;
  protected
    procedure Notification( AComponent : TComponent; Operation : TOperation ); override;
    procedure CheckFieldType( const Value: string ); virtual;

    procedure DecValue( Amount: Integer ); override;
    procedure IncValue( Amount: Integer ); override;

    procedure DataChange; virtual;
    procedure UpdateData; virtual;
    procedure ActiveChange; virtual;

    procedure InvalidKeyPressed;

    { Event Dispatch Methods }
    procedure Change; override;
    procedure KeyPress( var Key : Char ); override;

    { Property Access Methods }
    function GetField: TField; virtual;
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); virtual;

    property Field: TField
      read GetField;

    property DataLink: TFieldDataLink
      read FDataLink;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property BeepOnInvalidKey: Boolean
      read FBeepOnInvalidKey
      write FBeepOnInvalidKey
      default True;

    property DataField: string
      read GetDataField
      write SetDataField;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    { This property controls the ReadOnly state of the DataLink }
    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;
  end;


resourcestring
  sRzInvalidFieldType = 'DataField can only be connected to columns of type Integer, Smallint, Word, and Float';

implementation

uses
  {&RAS}
  Themes;


const
  DefaultIncrement: Extended = 1.0;
  DefaultPageSize: Extended  = 10.0;
  DefaultMin: Extended       = 0.0;
  DefaultMax: Extended       = 100.0;


{&RT}
{===========================}
{== TRzDBSpinEdit Methods ==}
{===========================}

constructor TRzDBSpinEdit.Create( AOwner: TComponent );
begin
  inherited;

  FButtons := TRzSpinButtons.Create( Self );
  FButtons.Parent := Self;
  FButtons.Width := 17;
  FButtons.Height := 17;
  FButtons.Visible := True;
  FButtons.FocusControl := Self;
  FButtons.OnUpRightClick := UpRightClickHandler;
  FButtons.OnDownLeftClick := DownLeftClickHandler;

  ControlStyle := ControlStyle - [ csSetCaption ];
  ControlStyle := ControlStyle + [ csReplicatable ];
  FButtons.ControlStyle := FButtons.ControlStyle + [ csReplicatable ];
  FButtons.UpRightButton.ControlStyle := FButtons.UpRightButton.ControlStyle + [ csReplicatable ];
  FButtons.DownLeftButton.ControlStyle := FButtons.DownLeftButton.ControlStyle + [ csReplicatable ];

  FFlatButtonColor := clBtnFace;

  FButtonWidth := 17;
  Width := 47;
  FIntegersOnly := True;
  FAllowBlank := True;
  FBlankValue := 0;
  FAllowKeyEdit := False;
  FIncrement := DefaultIncrement;
  FPageSize := DefaultPageSize;
  FDecimals := 0;
  FCheckRange := False;
  FMin := DefaultMin;
  FMax := DefaultMax;

  Alignment := taRightJustify;
end;


procedure TRzDBSpinEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or ws_ClipChildren;
end;


procedure TRzDBSpinEdit.CreateWnd;
begin
  inherited;
  SetEditRect;
  {&RCI}
end;


procedure TRzDBSpinEdit.Loaded;
begin
  inherited;
  ResizeButtons;
end;


procedure TRzDBSpinEdit.GetChildren( Proc: TGetChildProc; Root: TComponent );
begin
end;


procedure TRzDBSpinEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  // Handle the fact that the FlatButtonParentColor was published in version 2.x
  Filer.DefineProperty( 'FlatButtonParentColor', TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzDBSpinEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
begin
  inherited;

  if FlatButtons then
  begin
    if ActiveStyleServicesEnabled then
    begin
      if InFocus or Focused then
        FButtons.Flat := False
      else
        FButtons.Flat := True;
      FButtons.Color := Color;
    end
    else // No Themes
    begin
      if InFocus or Focused then
        FButtons.Color := FFlatButtonColor
      else
        FButtons.Color := Color;
    end;
  end;
end;


function TRzDBSpinEdit.CanChange( NewValue: Extended ): Boolean;
begin
  Result := True;
  if Assigned( FOnChanging ) then
    FOnChanging( Self, NewValue, Result );
end;


procedure TRzDBSpinEdit.Change;
begin
  if not FInternalUpdate then
    inherited;
end;


procedure TRzDBSpinEdit.DoButtonClick( S: TSpinButtonType );
begin
  if Assigned( FOnButtonClick ) then
    FOnButtonClick( Self, S );
end;


function TRzDBSpinEdit.CanEditData: Boolean;
begin
  if not ReadOnly and ( Field <> nil ) and Field.CanModify and ( DataSource <> nil ) then
    DataSource.Edit;
  Result := DataSource.State in dsEditModes;
end;


procedure TRzDBSpinEdit.IncValue( const Amount: Extended );
var
  TempValue: Extended;
begin
  {&RV}
  if ReadOnly or ( Field = nil ) then
    InvalidKeyPressed                { Prevent change if FDataLink is ReadOnly }
  else
  begin
    TempValue := Value + Amount;
    if CanChange( TempValue ) then
    begin
      if CanEditData then           { Put corresponding Dataset into Edit mode }
        Value := TempValue;                   { Increment only if in Edit mode }
      DoButtonClick( sbUp );
    end;
  end;
end;


procedure TRzDBSpinEdit.DecValue( const Amount: Extended );
var
  TempValue: Extended;
begin
  if ReadOnly or ( Field = nil ) then
    InvalidKeyPressed                { Prevent change if FDataLink is ReadOnly }
  else
  begin
    TempValue := Value - Amount;
    if CanChange( TempValue ) then
    begin
      if CanEditData then           { Put corresponding Dataset into Edit mode }
        Value := TempValue;                   { Increment only if in Edit mode }
      DoButtonClick( sbDown );
    end;
  end;
end;


function TRzDBSpinEdit.DoMouseWheelDown( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  inherited DoMouseWheelDown( Shift, MousePos );
  if ssCtrl in Shift then
    DecValue( FPageSize )
  else
    DecValue( FIncrement );
  Result := True;
end;


function TRzDBSpinEdit.DoMouseWheelUp( Shift: TShiftState; MousePos: TPoint ): Boolean;
begin
  inherited DoMouseWheelUp( Shift, MousePos );
  if ssCtrl in Shift then
    IncValue( FPageSize )
  else
    IncValue( FIncrement );
  Result := True;
end;


procedure TRzDBSpinEdit.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;

  if not FAllowKeyEdit and ( Key = vk_Delete ) then
  begin
    Key := 0;
    InvalidKeyPressed;
  end;

  case Key of
    vk_Prior:
      IncValue( FPageSize );
    vk_Next:
      DecValue( FPageSize );
    vk_Up:
      IncValue( FIncrement );
    vk_Down:
      DecValue( FIncrement );
  end;
end;


procedure TRzDBSpinEdit.KeyPress( var Key: Char );
begin
  inherited;

  if not IsValidChar( Key ) then
  begin
    Key := #0;
    InvalidKeyPressed;
  end;
  {&RV}
end;


function TRzDBSpinEdit.IsValidChar( Key: Char ): Boolean;
var
  ValidCharSet: TSysCharSet;
begin
  if FIntegersOnly then
    ValidCharSet := [ '+', '-', '0'..'9' ]
  else
    ValidCharSet := [ FormatSettings.DecimalSeparator, '+', '-', '0'..'9' ];

  Result := CharInSet( Key, ValidCharSet ) or ( ( Key < #32 ) and ( Key <> Chr( vk_Return ) ) );

  if Result then
  begin
    if Key = FormatSettings.DecimalSeparator then
    begin
      if SelLength = 0 then
        Result := Pos( FormatSettings.DecimalSeparator, Text ) = 0
      else
      begin
        Result := Pos( FormatSettings.DecimalSeparator, Text ) = 0;
        if not Result then
          Result := Pos( FormatSettings.DecimalSeparator, SelText ) <> 0;
      end;
    end
    else if ( Key = '+' ) or ( Key = '-' ) then
      Result := ( ( SelStart = 0 ) and
                  ( Pos( '+', Text ) = 0 ) and
                  ( Pos( '-', Text ) = 0 ) ) or
                ( SelLength = Length( Text ) );
  end;

  if not FAllowKeyEdit and Result and
     ( ( Key >= #32 ) or
       ( Key = Char( vk_Back ) ) or
       ( Key = Char( vk_Delete ) ) ) then
    Result := False;
end;


procedure TRzDBSpinEdit.UpRightClickHandler( Sender: TObject );
begin
  IncValue( FIncrement );
end;


procedure TRzDBSpinEdit.DownLeftClickHandler( Sender: TObject );
begin
  DecValue( FIncrement )
end;


procedure TRzDBSpinEdit.SetFrameStyle( Value: TFrameStyle );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzDBSpinEdit.SetButtonWidth( Value: Integer );
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    if FButtonWidth < 0 then
      FButtonWidth := 0;

    if Orientation = orVertical then
      FButtons.Width := FButtonWidth
    else
      FButtons.Width := 2 * FButtonWidth;
    ResizeButtons;
    Invalidate;
  end;
end;


procedure TRzDBSpinEdit.SetDecimals( Value: Byte );
begin
  if FDecimals <> Value then
  begin
    FDecimals := Value;
    SetValue( GetValue );
  end;
end;


function TRzDBSpinEdit.GetButton( Index: Integer ): TRzControlButton;
begin
  if Index = 1 then
    Result := FButtons.DownLeftButton
  else
    Result := FButtons.UpRightButton;
end;


function TRzDBSpinEdit.IsCustomUpGlyph: Boolean;
begin
  Result := FButtons.CustomUpRightGlyph;
end;

function TRzDBSpinEdit.GetButtonUpGlyph: TBitmap;
begin
    Result := FButtons.GlyphUpRight;
end;

procedure TRzDBSpinEdit.SetButtonUpGlyph( Value: TBitmap );
begin
  FButtons.GlyphUpRight := Value;
end;


function TRzDBSpinEdit.GetButtonUpNumGlyphs: TNumGlyphs;
begin
  Result := FButtons.NumGlyphsUpRight;
end;

procedure TRzDBSpinEdit.SetButtonUpNumGlyphs( Value: TNumGlyphs );
begin
  FButtons.NumGlyphsUpRight := Value;
end;


function TRzDBSpinEdit.IsCustomDownGlyph: Boolean;
begin
  Result := FButtons.CustomDownLeftGlyph;
end;

function TRzDBSpinEdit.GetButtonDownGlyph: TBitmap;
begin
  Result := FButtons.GlyphDownLeft;
end;

procedure TRzDBSpinEdit.SetButtonDownGlyph( Value: TBitmap );
begin
  FButtons.GlyphDownLeft := Value;
end;


function TRzDBSpinEdit.GetButtonDownNumGlyphs: TNumGlyphs;
begin
  Result := FButtons.NumGlyphsDownLeft;
end;

procedure TRzDBSpinEdit.SetButtonDownNumGlyphs( Value: TNumGlyphs );
begin
  FButtons.NumGlyphsDownLeft := Value;
end;


function TRzDBSpinEdit.GetDirection: TSpinDirection;
begin
  Result := FButtons.Direction;
end;

procedure TRzDBSpinEdit.SetDirection( Value: TSpinDirection );
begin
  FButtons.Direction := Value;
end;


procedure TRzDBSpinEdit.SetFlatButtons( Value: Boolean );
begin
  inherited;
  FButtons.Flat := Value;
  ResizeButtons;
end;


function TRzDBSpinEdit.GetOrientation: TOrientation;
begin
  Result := FButtons.Orientation;
end;

procedure TRzDBSpinEdit.SetOrientation( Value: TOrientation );
begin
  FButtons.Orientation := Value;
  ResizeButtons;
  Invalidate;
end;


procedure TRzDBSpinEdit.SetIntegersOnly( Value: Boolean );
begin
  if FIntegersOnly <> Value then
  begin
    FIntegersOnly := Value;
    if FIntegersOnly then
    begin
      Decimals := 0;
      SetValue( Round( GetValue ) );
    end;
  end;
end;


procedure TRzDBSpinEdit.SetCheckRange( Value: Boolean );
begin
  if FCheckRange <> Value then
  begin
    FCheckRange := Value;
    SetValue( GetValue );
  end;
end;


procedure TRzDBSpinEdit.SetMin( const Value: Extended );
begin
  if FMin <> Value then
  begin
    FMin := Value;
    if FMax < FMin then
      FMax := FMin;
    SetValue( GetValue ); // Reapply range
    Invalidate;
  end;
end;


procedure TRzDBSpinEdit.SetMax( const Value: Extended );
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMin > FMax then
      FMin := FMax;
    SetValue( GetValue ); // Reapply range
    Invalidate;
  end;
end;


function TRzDBSpinEdit.GetIntValue: Int64;
begin
  Result := Round( GetValue );
end;


procedure TRzDBSpinEdit.SetIntValue( Value: Int64 );
begin
  SetValue( Value );
end;


function TRzDBSpinEdit.CleanUpText: string;
var
  I: Integer;
  ValidCharSet: TSysCharSet;
begin
  ValidCharSet := [ FormatSettings.DecimalSeparator, '+', '-', '0'..'9' ];

  Result := '';
  for I := 1 to Length( Text ) do
  begin
    if CharInSet( Text[ I ], ValidCharSet ) then
      Result := Result + Text[ I ];
  end;
end;


function TRzDBSpinEdit.GetValue: Extended;
var
  S: string;
begin
  try
    if Text = '' then
    begin
      if FAllowBlank then
        Result := FBlankValue
      else
      begin
        S := FloatToStr( FMin );
        Result := StrToFloat( S );
      end;
    end
    else
      Result := StrToFloat( CleanUpText );
  except
    Result := FMin;
  end;
end;


function TRzDBSpinEdit.CheckValue( const Value: Extended ): Extended;
begin
  Result := Value;
  if ( FMax <> FMin ) or FCheckRange then
  begin
    if Value < FMin then
      Result := FMin
    else if Value > FMax then
      Result := FMax;
  end;
end;

procedure TRzDBSpinEdit.SetValue( const Value: Extended );
var
  S: string;
begin
  if ( DataSource = nil ) or ( DataSource.Dataset = nil ) then
    Exit;
  if not ( DataSource.Dataset.State in [ dsEdit, dsInsert ] ) then
    Exit;

  S := FloatToStrF( CheckValue( Value ), ffFixed, 7, FDecimals );
  if CleanUpText <> S then
    Text := S;
end;


function TRzDBSpinEdit.StoreIncrement: Boolean;
begin
  Result := FIncrement <> DefaultIncrement;
end;



function TRzDBSpinEdit.StorePageSize: Boolean;
begin
  Result := FPageSize <> DefaultPageSize;
end;



procedure TRzDBSpinEdit.WMPaste( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;

procedure TRzDBSpinEdit.WMCut( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;


procedure TRzDBSpinEdit.CMEnter( var Msg: TCMEnter );
begin
  // Moved inherited to beginning b/c any changes made in OnEnter event handler
  // that recreate the window does not cause the EditRect to be updated.
  inherited;
  SetEditRect;
  if AutoSelect and not ( csLButtonDown in ControlState ) then
    SelectAll;
end;


procedure TRzDBSpinEdit.CMExit( var Msg: TCMExit );
var
  N: Extended;
  S: string;
begin
  SetEditRect;

  if Field <> nil then
  begin
    S := CleanUpText;
    if FAllowBlank and ( S = '' ) then
    begin
      inherited;
      Exit;
    end;

    try
      N := StrToFloat( S );
    except
      N := FMin;
    end;
    SetValue( N );
  end;
  inherited;
end;




procedure TRzDBSpinEdit.AdjustEditRect;
begin
  inherited;

  if ( Parent <> nil ) and Parent.ClassNameIs( 'TDBCtrlPanel' ) then
  begin
    // Need to refresh buttons so they appear when control in on a DBCtrlGrid
    FButtons.Refresh;
  end;
end;


function TRzDBSpinEdit.GetEditRect: TRect;
begin
  Result := inherited GetEditRect;
  if not ReadOnlyValue then
    Dec( Result.Right, FButtons.Width + 2 );
end;


procedure TRzDBSpinEdit.SetEditRect;
begin
  if ReadOnlyValue then
  begin
    SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
    SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
    Exit;
  end;

  if not ( csLoading in ComponentState ) then
  begin
    if not UseRightToLeftLayout then
    begin
      SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
      SendMessage( Handle, em_SetMargins, ec_RightMargin, MakeLParam( 0, FButtons.Width + 2 ) );
    end
    else
    begin
      SendMessage( Handle, em_SetMargins, ec_LeftMargin, MakeLParam( FButtons.Width + 2, 0 ) );
      SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
    end;
  end;
end;


procedure TRzDBSpinEdit.ResizeButtons;
var
  W, MinHeight: Integer;
begin
  if not ( csLoading in ComponentState ) then
  begin
    MinHeight := GetMinFontHeight( Font );
    if Height < MinHeight then
      Height := MinHeight
    else if FButtons <> nil then
    begin
      W := FButtons.Width;

      if not UseRightToLeftLayout then
      begin
        if not FrameVisible then
        begin
          if Ctl3D then
          begin
            if ActiveStyleServicesEnabled then
            begin
              if UsingSystemStyle then
              begin
                if RunningAtLeast( winVista ) then
                begin
                  if FButtons.Orientation = orVertical then
                    FButtons.SetBounds( Width - W - 3, 0, W, Height - 3 )
                  else
                    FButtons.SetBounds( Width - W - 3, -1, W, Height - 3 );
                end
                else
                  FButtons.SetBounds( Width - W - 3, -1, W, Height - 2 );
              end
              else // VCL Style
              begin
                FButtons.SetBounds( Width - W - 3, -1, W, Height - 2 );
              end;
            end
            else
              FButtons.SetBounds( Width - W - 4, 0, W, Height - 4 );
          end
          else
            FButtons.SetBounds( Width - W - 1, 1, W, Height - 2 );
        end
        else
        begin
          if ( Parent <> nil ) and Parent.ClassNameIs( 'TDBCtrlPanel' ) then
          begin
            if RunningAtLeast( winVista ) then
              FButtons.SetBounds( Width - W - 1, 2, W, Height - 3 )
            else
              FButtons.SetBounds( Width - W - 1, 1, W, Height - 2 );
          end
          else if ActiveStyleServicesEnabled then
            FButtons.SetBounds( Width - W - 3, -1, W, Height - 2 )
          else
            FButtons.SetBounds( Width - W - 4, 0, W, Height - 4 );
        end;
      end
      else
      begin
        if ( Parent <> nil ) and Parent.ClassNameIs( 'TDBCtrlPanel' ) then
          FButtons.SetBounds( 0, 2, W, Height - 4 )
        else
          FButtons.SetBounds( 0, 0, W, Height - 4 );
      end;

      SetEditRect;
    end;
  end;
end; {= TRzDBSpinEdit.ResizeButtons =}


procedure TRzDBSpinEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzDBSpinEdit.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect;
  FButtons.Enabled := Enabled;
  if FlatButtons then
    FButtons.Color := Color;
end;


procedure TRzDBSpinEdit.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  if HandleAllocated then
    SetEditRect;
  FButtons.Enabled := Enabled;
end;


procedure TRzDBSpinEdit.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if FButtons <> nil then
    FButtons.Color := Color;
end;


procedure TRzDBSpinEdit.ReadOnlyChanged;
begin
  inherited;
  if FButtons <> nil then
    FButtons.Visible := not ReadOnlyValue;
  ResizeButtons;
end;


{==========================}
{== TRzDBSpinner Methods ==}
{==========================}

constructor TRzDBSpinner.Create( AOwner: TComponent );
begin
  inherited;
  {&RCI}
  // Add csReplicatable style so control can be used in DBCtrlGrids
  ControlStyle := ControlStyle + [ csReplicatable ];

  FDataLink := TFieldDataLink.Create;

  // To support the TField.FocusControl method, set the FDataLink.Control property to point to the spinner.
  // The Control property requires a TWinControl component.

  FDataLink.Control := Self;

  FDataLink.OnDataChange := DataChangeHandler;
  FDataLink.OnUpdateData := UpdateDataHandler;
  FDataLink.OnActiveChange := ActiveChangeHandler;

  FPaintControl := TRzSpinner.Create( Self );
  FPaintControl.Parent := Self;
  FPaintControl.Visible := False;
  FPaintControl.ControlStyle := FPaintControl.ControlStyle + [ csReplicatable ];

  FBeepOnInvalidKey := True;
  {&RV}
end;


destructor TRzDBSpinner.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBSpinner.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = FDataLink.DataSource ) then
    SetDataSource( nil );
end;


function TRzDBSpinner.GetField: TField;
begin
  Result := FDataLink.Field;
end;


function TRzDBSpinner.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBSpinner.SetDataField( const Value: string );
begin
  CheckFieldType( Value );
  FDataLink.FieldName := Value;
end;


function TRzDBSpinner.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBSpinner.SetDataSource( Value: TDataSource );
begin
  if FDatalink.DataSource <> Value then
  begin
    FDataLink.DataSource := Value;

    // FreeNotification must be called b/c DataSource may be located on another form or data module.
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


function TRzDBSpinner.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;


procedure TRzDBSpinner.SetReadOnly( Value: Boolean );
begin
  FDataLink.ReadOnly := Value;
end;


procedure TRzDBSpinner.CheckFieldType( const Value: string );
var
  FieldType: TFieldType;
begin
  // Make sure the field type corresponding to the column referenced by Value is either ftInteger, ftSmallInt, ftWord,
  // or ftFloat.  If it is not, an EInvalidFieldType exception is  raised.

  if ( Value <> '' ) and ( FDataLink <> nil ) and ( FDataLink.Dataset <> nil ) and ( FDataLink.Dataset.Active ) then
  begin
    FieldType := FDataLink.Dataset.FieldByName( Value ).DataType;
    if not ( FieldType in [ ftInteger, ftSmallInt, ftWord, ftFloat ] ) then
      raise EInvalidFieldType.Create( sRzInvalidFieldType );
  end;
end;


procedure TRzDBSpinner.Change;
begin
  // Tell the FDataLink that the data has changed
  if FDataLink <> nil then
    FDataLink.Modified;
  inherited;
end;


procedure TRzDBSpinner.InvalidKeyPressed;
begin
  if FBeepOnInvalidKey then
    Beep;
end;


procedure TRzDBSpinner.KeyPress( var Key: Char );
begin
  inherited;

  if Key = #27 then
  begin
    FDataLink.Reset;                                          // Esc key pressed
    Key := #0;                            // Set to #0 so Esc won't close dialog
  end;
end;


procedure TRzDBSpinner.DecValue( Amount: Integer );
begin
  if ReadOnly or not FDataLink.CanModify then
    InvalidKeyPressed                 // Prevent change if FDataLink is ReadOnly
  else
  begin
    if FDataLink.Edit then                  // Try to put Dataset into edit mode
      inherited;                               // Decrement only if in edit mode
  end;
end;


procedure TRzDBSpinner.IncValue( Amount: Integer );
begin
  if ReadOnly or not FDataLink.CanModify then
    InvalidKeyPressed                 // Prevent change if FDataLink is ReadOnly
  else
  begin
    if FDataLink.Edit then                  // Try to put Dataset into edit mode
      inherited;                               // Increment only if in edit mode
  end;
end;


{-----------------------------------------------------------------------------------------------------------------------
  TRzDBSpinner.DataChangeHandler

  This method gets called as a result of a number of different events:

  1. The underlying field value changes.  Occurs when changing the value of the column tied to this control and then
     move to a new column or a new record.
  2. The corresponding Dataset goes into Edit mode.
  3. The corresponding Dataset referenced by DataSource changes.
  4. The current cursor is scrolled to a new record in the table.
  5. The record is reset through a Cancel call.
  6. The DataField property changes to reference another column.
-----------------------------------------------------------------------------------------------------------------------}

procedure TRzDBSpinner.DataChangeHandler( Sender: TObject );
begin
  DataChange;
end;


procedure TRzDBSpinner.DataChange;
begin
  if FDataLink.Field <> nil then
    Value := FDataLink.Field.AsInteger;
end;


{-----------------------------------------------------------------------------------------------------------------------
  TRzDBSpinner.UpdateData

  This method gets called when the corresponding field value and the contents of the Spinner need to be synchronized.
  Note that this method only gets called if this control was responsible for altering the data.
-----------------------------------------------------------------------------------------------------------------------}

procedure TRzDBSpinner.UpdateDataHandler( Sender: TObject );
begin
  UpdateData;
end;


procedure TRzDBSpinner.UpdateData;
begin
  FDataLink.Field.AsInteger := Value;
end;


{-----------------------------------------------------------------------------------------------------------------------
  TRzDBSpinner.ActiveChange

  This method gets called whenever the Active property of the attached Dataset changes.

  NOTE: You can use the FDataLink.Active property to determine the *new* state of the Dataset.
-----------------------------------------------------------------------------------------------------------------------}

procedure TRzDBSpinner.ActiveChangeHandler( Sender: TObject );
begin
  ActiveChange;
end;


procedure TRzDBSpinner.ActiveChange;
begin
  // If the Dataset is becoming Active, then check to make sure the field type of the DataField property is a
  // valid type.

  if ( FDataLink <> nil ) and FDataLink.Active then
    CheckFieldType( DataField );
end;


procedure TRzDBSpinner.WMPaint( var Msg: TWMPaint );
begin
  if csPaintCopy in ControlState then
  begin
    if Field <> nil then
      FPaintControl.Value := Field.AsInteger
    else
      FPaintControl.Value := 0;
    FPaintControl.Color := Color;
    FPaintControl.SetBounds( BoundsRect.Left, BoundsRect.Top,
                             BoundsRect.Right - BoundsRect.Left,
                             BoundsRect.Bottom - BoundsRect.Top );
    SendMessage( FPaintControl.Handle, wm_Paint, WParam( Msg.DC ), 0 );
    FPaintControl.PaintTo( Msg.DC, 0, 0 );
  end
  else
  begin
    FPaintControl.SetBounds( 0, 0, 0, 0 );
    inherited;
  end;
end; {= TRzDBSpinner.WMPaint =}


procedure TRzDBSpinner.CMExit( var Msg: TCMExit );
begin
  try                                                      // Attempt to update the record if focus leaves the spinner
    FDataLink.UpdateRecord;
  except
    SetFocus;                                              // Keep the focus on the control if Update fails
    raise;                                                 // Re-raise the exception
  end;
  inherited;
end;


procedure TRzDBSpinner.CMDesignHitTest( var Msg: TCMDesignHitTest );
begin
  // Ancestor component allows Value to be changed at design-time.  This is not valid in a data-aware component because
  // it would put the connected dataset into edit mode.
  Msg.Result := 0;
end;


procedure TRzDBSpinner.CMGetDataLink( var Msg: TMessage );
begin
  // The cm_GetDataLink message is sent to the component by the DBCtrlGrid whenever it needs access to the data link
  Msg.Result := Integer( FDataLink );
end;

{&RUIF}
end.
