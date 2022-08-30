{===============================================================================
  RzBtnEdt Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzButtonEdit
    Descendant of TRzEdit with two embedded buttons


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Cleaned up the appearance of embedded buttons in TRzButtonEdit under
      Windows Vista and Windows 7.
    * Made necessary modifications to TRzButtonEdit to fully support VCL Styles 
      introduced in RAD Studio XE2.
    * Made necessary modifications to TRzButtonEdit to support 64-bit.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Updated the display of embedded buttons in TRzButtonEdit when running
      under Windows Vista and Windows 7.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzButtonEdit control.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed problem in TRzButtonEdit where trapping keys in an OnKeyDown event
      handler (by setting Key := 0) would result in the OnButtonClick event or
      the OnAltBtnClick event being fired if the ShortCut for either button was
      set to scNone.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Fixed problem with hot track custom framing not getting updated correctly
      in TRzButtonEdit.
  ------------------------------------------------------------------------------
  4.1    (15 Dec 2006)
    * Added BeepOnInvalidKey property to TRzButtonEdit.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * The embedded buttons of a TRzButtonEdit (and descendants) now match in
      appearance to combo box buttons and the buttons in TRzColorEdit and
      TRzDateTimeEdit.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Added HideButtonsOnReadOnly property to TRzButtonEdit. When this property
      is True (the default), the buttons embedded in the control are hidden when
      the ReadOnly property is set to True.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added ReadOnlyColor property to TRzButtonEdit. This color property is used
      to change the color of the control when the ReadOnly property is set to
      True.
    * When the ReadOnly property for a TRzButtonEdit is set to True, the
      embedded buttons are hidden.
    * Added new FrameControllerNotifications property to TRzButtonEdit.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem when changing the ButtonKind or AltBtnKind properties to
      bkDropDown would prevent future changes to the ButtonKind (AltBtnKind)
      property to take affect.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * The color of flat buttons in TRzButtonEdit are now adjusted appropriately
      when the control is disabled and re-enabled.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Fixed display problem when button clicked and focus taken away by an
      exception.
    * The OnChange event is no longer fired when the TRzButtonEdit control
      receives the keyboard focus.
    * The Buttons are now placed on the left side of the control for
      Right-To-Left locales.
    * Inherits XP visual style support from TRzCustomEdit.
===============================================================================}

{$I RzComps.inc}

unit RzBtnEdt;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Classes,
  StdCtrls,
  ExtCtrls,
  Controls,
  SysUtils,
  Forms,
  Graphics,
  Menus,
  Buttons,
  RzEdit,
  RzCommon,
  RzButton;

const
  um_PopupListKeyDown = wm_User + $B900;
  um_PopupListKeyUP   = um_PopupListKeyDown + 1;

type
  TButtonKind = ( bkCustom, bkLookup, bkDropDown, bkCalendar, bkAccept, bkReject, bkFolder, bkFind, bkSearch );


  {=====================================}
  {== TRzButtonPair Class Declaration ==}
  {=====================================}

  TRzButtonPair = class( TWinControl )
  private
    FAltBtn: TRzControlButton;
    FButton: TRzControlButton;
    FFocusControl: TWinControl;
    FFlat: Boolean;

    FOnAltBtnClick: TNotifyEvent;
    FOnButtonClick: TNotifyEvent;

    { Message Handling Methods }
    procedure WMSize( var Msg: TWMSize );  message wm_Size;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function CreateButton: TRzControlButton; virtual;

    { Internal Event Handlers }
    procedure BtnClickHandler( Sender: TObject ); virtual;
    procedure BtnMouseDownHandler( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ); virtual;

    procedure RepositionButtons; virtual;

    { Event Dispatch Methods }
    procedure AltBtnClick; dynamic;
    procedure ButtonClick; dynamic;

    { Property Access Methods }
    function GetAllEnabled: Boolean; virtual;
    procedure SetAllEnabled( Value: Boolean ); virtual;
    function GetColor: TColor; virtual;
    procedure SetColor( Value: TColor ); virtual;
    procedure SetFlat( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    procedure SetBounds( ALeft, ATop, AWidth, AHeight: Integer ); override;

    property AltBtn: TRzControlButton
      read FAltBtn;

    property Button: TRzControlButton
      read FButton;
  published
    property Color: TColor
      read GetColor
      write SetColor
      default clBtnFace;

    property Enabled: Boolean
      read GetAllEnabled
      write SetAllEnabled
      default True;

    property Flat: Boolean
      read FFlat
      write SetFlat
      default False;

    property FocusControl: TWinControl
      read FFocusControl
      write FFocusControl;

    property OnAltBtnClick: TNotifyEvent
      read FOnAltBtnClick
      write FOnAltBtnClick;

    property OnButtonClick: TNotifyEvent
      read FOnButtonClick
      write FOnButtonClick;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDock;
    property OnStartDrag;
  end;


  {===========================================}
  {== TRzCustomButtonEdit Class Declaration ==}
  {===========================================}

  TRzCustomButtonEdit = class( TRzEdit )
  private
    FAboutInfo: TRzAboutInfo;
    FAllowKeyEdit: Boolean;
    FButtons: TRzButtonPair;
    FFlatButtonColor: TColor;

    FAltBtnKind: TButtonKind;
    FButtonKind: TButtonKind;
    FAltBtnShortCut: TShortCut;
    FButtonShortCut: TShortCut;
    FShortCutPressed: Boolean;
    FHideButtonsOnReadOnly: Boolean;

    FOnAltBtnClick: TNotifyEvent;
    FOnButtonClick: TNotifyEvent;

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
    procedure CreateWnd; override; //!! remove
    procedure Loaded; override;
    procedure DefineProperties( Filer: TFiler ); override;

    procedure UpdateFrame( ViaMouse, InFocus: Boolean ); override;

    function IsCustomAltBtnGlyph: Boolean;
    function IsCustomButtonGlyph: Boolean;
    procedure ReadButtonShortCutText( Reader: TReader ); virtual;
    procedure ReadAltBtnShortCutText( Reader: TReader ); virtual;
    procedure WriteShortCutText( Writer: TWriter ); virtual;

    procedure ReadOnlyChanged; override;
    procedure ResizeButtons; virtual;
    function GetEditRect: TRect; override;
    procedure SetEditRect; virtual;

    procedure AltBtnClickHandler( Sender: TObject ); virtual;
    procedure ButtonClickHandler( Sender: TObject ); virtual;

    { Event Dispatch Methods }
    procedure AltBtnClick; dynamic;
    procedure ButtonClick; dynamic;

    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress(var Key: Char); override;

    { Property Access Methods }
    procedure SetAlignment( Value: TAlignment ); override;
    procedure SetFrameStyle( Value: TFrameStyle ); override;
    procedure SetFlatButtons( Value: Boolean ); override;
    procedure SetHideButtonsOnReadOnly( Value: Boolean ); virtual;

    function GetButton( Index: Integer ): TRzControlButton; virtual;
    function GetAltBtnGlyph: TBitmap; virtual;
    procedure SetAltBtnGlyph( Value: TBitmap ); virtual;
    function GetButtonGlyph: TBitmap; virtual;
    procedure SetButtonGlyph( Value: TBitmap ); virtual;
    function GetButtonNumGlyphs( Index: Integer ): TNumGlyphs; virtual;
    procedure SetButtonNumGlyphs( Index: Integer; Value: TNumGlyphs ); virtual;
    function GetButtonHint( Index: Integer ): string; virtual;
    procedure SetButtonHint( Index: Integer; const Value: string ); virtual;
    function GetButtonKind( Index: Integer ): TButtonKind; virtual;
    procedure SetButtonKind( Index: Integer; Value: TButtonKind ); virtual;
    function GetButtonShortCut( Index: Integer ): TShortCut; virtual;
    procedure SetButtonShortCut( Index: Integer; Value: TShortCut ); virtual;
    function GetButtonVisible( Index: Integer ): Boolean; virtual;
    procedure SetButtonVisible( Index: Integer; Value: Boolean ); virtual;
    function GetButtonWidth( Index: Integer ): Integer; virtual;
    procedure SetButtonWidth( Index: Integer; Value: Integer ); virtual;
  public
    constructor Create(AOwner: TComponent); override;

    procedure GetChildren( Proc: TGetChildProc; Root: TComponent ); override;

    property Buttons: TRzButtonPair
      read FButtons;

    property AltBtn: TRzControlButton
      index 1
      read GetButton;

    property Button: TRzControlButton
      index 2
      read GetButton;

    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property AllowKeyEdit: Boolean
      read FAllowKeyEdit
      write FAllowKeyEdit
      default True;

    property AltBtnGlyph: TBitmap
      read GetAltBtnGlyph
      write SetAltBtnGlyph
      stored IsCustomAltBtnGlyph;

    property ButtonGlyph: TBitmap
      read GetButtonGlyph
      write SetButtonGlyph
      stored IsCustomButtonGlyph;

    property AltBtnNumGlyphs: TNumGlyphs
      index 1
      read GetButtonNumGlyphs
      write SetButtonNumGlyphs
      default 2;

    property ButtonNumGlyphs: TNumGlyphs
      index 2
      read GetButtonNumGlyphs
      write SetButtonNumGlyphs
      default 2;

    property AltBtnHint: string
      index 1
      read GetButtonHint
      write SetButtonHint;

    property ButtonHint: string
      index 2
      read GetButtonHint
      write SetButtonHint;

    property AltBtnKind: TButtonKind
      index 1
      read GetButtonKind
      write SetButtonKind
      default bkLookup;

    property ButtonKind: TButtonKind
      index 2
      read GetButtonKind
      write SetButtonKind
      default bkLookup;

    property AltBtnShortCut: TShortCut
      index 1
      read GetButtonShortCut
      write SetButtonShortCut
      default scNone;

    property ButtonShortCut: TShortCut
      index 2
      read GetButtonShortCut
      write SetButtonShortCut
      default vk_F4;

    property AltBtnVisible: Boolean
      index 1
      read GetButtonVisible
      write SetButtonVisible
      default False;

    property ButtonVisible: Boolean
      index 2
      read GetButtonVisible
      write SetButtonVisible
      default True;

    property AltBtnWidth: Integer
      index 1
      read GetButtonWidth
      write SetButtonWidth
      default 17;

    property ButtonWidth: Integer
      index 2
      read GetButtonWidth
      write SetButtonWidth
      default 17;

    property FlatButtonColor: TColor
      read FFlatButtonColor
      write FFlatButtonColor
      default clBtnFace;

    property HideButtonsOnReadOnly: Boolean
      read FHideButtonsOnReadOnly
      write SetHideButtonsOnReadOnly
      default True;

    property OnAltBtnClick: TNotifyEvent
      read FOnAltBtnClick
      write FOnAltBtnClick;

    property OnButtonClick: TNotifyEvent
      read FOnButtonClick
      write FOnButtonClick;

    { Inherited Properties & Events }
    property Alignment;
    property Enabled;
  end;

  {=====================================}
  {== TRzButtonEdit Class Declaration ==}
  {=====================================}

  TRzButtonEdit = class( TRzCustomButtonEdit )
  private
    FAboutInfo: TRzAboutInfo;

  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    { Inherited Properties & Events }
    property Alignment;
    property AllowKeyEdit;
    property AltBtnGlyph;
    property ButtonGlyph;
    property AltBtnNumGlyphs;
    property ButtonNumGlyphs;
    property AltBtnHint;
    property ButtonHint;
    property AltBtnKind;
    property ButtonKind;
    property AltBtnShortCut;
    property ButtonShortCut;
    property AltBtnVisible;
    property ButtonVisible;
    property AltBtnWidth;
    property ButtonWidth;
    property BeepOnInvalidKey;
    property Enabled;
    property FlatButtons;
    property FlatButtonColor;
    property HideButtonsOnReadOnly;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}

    property OnAltBtnClick;
    property OnButtonClick;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
  end;


function GetBtnEdtGlyph( Kind: TButtonKind ): TBitmap;


implementation

uses
  {&RAS}
  Themes;

// Link in bitmaps for button glyphs
{$R RzBtnEdt.res}

const
  BtnEdtResNames: array[ TButtonKind ] of PChar = ( nil,
                                                    'RZBTNEDT_LOOKUP',
                                                    'RZBTNEDT_DROPDOWN',
                                                    'RZBTNEDT_CALENDAR',
                                                    'RZBTNEDT_ACCEPT',
                                                    'RZBTNEDT_REJECT',
                                                    'RZBTNEDT_FOLDER',
                                                    'RZBTNEDT_FIND',
                                                    'RZBTNEDT_SEARCH' );
var
  BtnEdtGlyphs: array[ TButtonKind ] of TBitmap;



function GetBtnEdtGlyph( Kind: TButtonKind ): TBitmap;
begin
  if BtnEdtGlyphs[ Kind ] = nil then
  begin
    BtnEdtGlyphs[ Kind ] := TBitmap.Create;
    BtnEdtGlyphs[ Kind ].LoadFromResourceName( HInstance, BtnEdtResNames[ Kind ] );
  end;
  Result := BtnEdtGlyphs[ Kind ];
end;


{&RT}
{===========================}
{== TRzButtonPair Methods ==}
{===========================}

constructor TRzButtonPair.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle - [ csAcceptsControls, csSetCaption ];

//  FFlat := ActiveStyleServicesEnabled;

  FButton := CreateButton;
  FButton.Visible := True;
  FAltBtn := CreateButton;

  Width := 34;
  Height := 21;
  {&RCI}
end;


function TRzButtonPair.CreateButton: TRzControlButton;
begin
  Result := TRzControlButton.Create( Self );
  Result.Parent := Self;
  Result.ControlStyle := Result.ControlStyle + [ csNoDesignVisible ];
  Result.OnClick := BtnClickHandler;
  Result.OnMouseDown := BtnMouseDownHandler;
  Result.Visible := False;
  Result.Enabled := True;
  if ActiveStyleServicesEnabled then
    Result.Width := 15
  else
    Result.Width := 17;
  {&RV}
end;


procedure TRzButtonPair.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( AComponent = FFocusControl ) then
    FFocusControl := nil;
end;


procedure TRzButtonPair.RepositionButtons;
begin
  if ( FButton = nil ) or ( csLoading in ComponentState ) then
    Exit;

  if FButton.Visible and not FAltBtn.Visible then
    FButton.SetBounds( 0, 0, FButton.Width, Height - 1 )
  else if FButton.Visible and FAltBtn.Visible then
  begin
    FButton.SetBounds( 0, 0, FButton.Width, Height - 1 );
    FAltBtn.SetBounds( FButton.Width, 0, FAltBtn.Width, Height - 1 );
  end
  else if not FButton.Visible and FAltBtn.Visible then
    FAltBtn.SetBounds( 0, 0, FAltBtn.Width, Height - 1 );
end; {= TRzButtonPair.RepositionButtons =}


procedure TRzButtonPair.SetBounds( ALeft, ATop, AWidth, AHeight: Integer );
begin
  inherited;
  RepositionButtons;
end;


procedure TRzButtonPair.BtnMouseDownHandler( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
begin
  if Button = mbLeft then
  begin
    if ( FFocusControl <> nil ) and FFocusControl.TabStop and FFocusControl.CanFocus and
       ( GetFocus <> FFocusControl.Handle ) then
    begin
      Windows.SetFocus( FFocusControl.Handle );
      if not FFocusControl.Focused then
      begin
        // If FFocusControl is not focused, which may happen as the result
        // of a validation exception on a data-aware control, then abort
        // the clicking process of this button.  The call to Abort will
        // prevent the button from being drawn in the down state and not
        // being repainted after the exception message is closed.
        Abort;
      end;
    end
    else if TabStop and ( GetFocus <> Handle ) and CanFocus then
    begin
      Windows.SetFocus( Handle );
      if not Focused then
        Abort;
    end;
  end;
end;


procedure TRzButtonPair.AltBtnClick;
begin
  if Assigned( FOnAltBtnClick ) then
    FOnAltBtnClick( Self );
end;

procedure TRzButtonPair.ButtonClick;
begin
  if Assigned( FOnButtonClick ) then
    FOnButtonClick( Self );
end;


procedure TRzButtonPair.BtnClickHandler( Sender: TObject );
begin
  if Sender = FAltBtn then
    AltBtnClick
  else
    ButtonClick;
end;


function TRzButtonPair.GetAllEnabled: Boolean;
begin
  Result := inherited Enabled;
end;


procedure TRzButtonPair.SetAllEnabled( Value: Boolean );
begin
  if inherited Enabled <> Value then
  begin
    inherited Enabled := Value;
    FAltBtn.Enabled := Value;
    FButton.Enabled := Value;
  end;
end;


function TRzButtonPair.GetColor: TColor;
begin
  Result := inherited Color;
end;


procedure TRzButtonPair.SetColor( Value: TColor );
begin
  inherited Color := Value;
  FAltBtn.Color := Value;
  FButton.Color := Value;
end;


procedure TRzButtonPair.SetFlat( Value: Boolean );
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    FAltBtn.Flat := Value;
    FButton.Flat := Value;
  end;
end;


procedure TRzButtonPair.WMSize( var Msg: TWMSize );
begin
  inherited;
  RepositionButtons;
  Msg.Result := 0;
end;


{=================================}
{== TRzCustomButtonEdit Methods ==}
{=================================}

constructor TRzCustomButtonEdit.Create( AOwner: TComponent );
begin
  inherited;

  FButtons := TRzButtonPair.Create( Self );
  FButtons.Parent := Self;
  FButtons.Width := 17;
  FButtons.Height := 10;
  FButtons.Visible := True;
  FButtons.FocusControl := Self;
  FButtons.OnAltBtnClick := AltBtnClickHandler;
  FButtons.OnButtonClick := ButtonClickHandler;

  ControlStyle := ControlStyle - [ csSetCaption ];

  FFlatButtonColor := clBtnFace;

  Width := 121;
  FButtonShortCut := vk_F4;
  FAltBtnShortCut := scNone;
  FAllowKeyEdit := True;

  FAltBtnKind := bkLookup;
  FButtonKind := bkLookup;

  FHideButtonsOnReadOnly := True;
  
  FButtons.AltBtn.Visible := False;
  FButtons.AltBtn.Glyph.Assign( GetBtnEdtGlyph( bkLookup ) );
  FButtons.AltBtn.NumGlyphs := 2;
  FButtons.Button.Glyph.Assign( GetBtnEdtGlyph( bkLookup ) );
  FButtons.Button.NumGlyphs := 2;
  {&RCI}
end;


procedure TRzCustomButtonEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or ws_ClipChildren;
  {&RV}
end;


(*!! remove *)
procedure TRzCustomButtonEdit.CreateWnd;
begin
  inherited;
  SetEditRect;
end;


procedure TRzCustomButtonEdit.Loaded;
begin
  inherited;

  { Reset any custom glyphs in the Loaded method instead of inside the
    SetButtonKind method to ensure component works correctly in a TFrame. }
  if FButtonKind <> bkCustom then
  begin
    Button.Glyph.Assign( GetBtnEdtGlyph( FButtonKind ) );
    Button.NumGlyphs := 2;
  end;
  if FAltBtnKind <> bkCustom then
  begin
    AltBtn.Glyph.Assign( GetBtnEdtGlyph( FAltBtnKind ) );
    AltBtn.NumGlyphs := 2;
  end;

  ResizeButtons;
end;


procedure TRzCustomButtonEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineProperty( 'ButtonShortCutText', ReadButtonShortCutText, WriteShortCutText, False );
  Filer.DefineProperty( 'AltBtnShortCutText', ReadAltBtnShortCutText, WriteShortCutText, False );
  // Handle the fact that the FlatButtonParentColor was published in version 2.x
  Filer.DefineProperty( 'FlatButtonParentColor', TRzOldPropReader.ReadOldBooleanProp, nil, False );
end;


procedure TRzCustomButtonEdit.ReadButtonShortCutText( Reader: TReader );
begin
  FButtonShortCut := TextToShortCut( Reader.ReadString );
end;


procedure TRzCustomButtonEdit.ReadAltBtnShortCutText( Reader: TReader );
begin
  FAltBtnShortCut := TextToShortCut( Reader.ReadString );
end;


procedure TRzCustomButtonEdit.WriteShortCutText( Writer: TWriter );
begin
end;


procedure TRzCustomButtonEdit.GetChildren( Proc: TGetChildProc; Root: TComponent );
begin
end;


procedure TRzCustomButtonEdit.KeyDown( var Key: Word; Shift: TShiftState );
var
  ShortCut: TShortCut;
begin
  inherited;

  if not FAllowKeyEdit and ( Key = vk_Delete ) then
  begin
    Key := 0;
    InvalidKeyPressed;
    Exit;
  end;

  if Key = 0 then
    Exit;

  ShortCut := Byte( Key );
  if ssShift in Shift then
    Inc( ShortCut, scShift );
  if ssCtrl in Shift then
    Inc( ShortCut, scCtrl );
  if ssAlt in Shift then
    Inc( ShortCut, scAlt );

  FShortCutPressed := False;
  if Button.Visible and Button.Enabled and ( ShortCut = FButtonShortCut ) then
  begin
    Button.Click;
    FShortCutPressed := True;
  end
  else if AltBtn.Visible and AltBtn.Enabled and ( ShortCut = FAltBtnShortCut ) then
  begin
    AltBtn.Click;
    FShortCutPressed := True;
  end;
end; {= TRzCustomButtonEdit.KeyDown =}


procedure TRzCustomButtonEdit.KeyPress( var Key: Char );
begin
  inherited;

  if not FAllowKeyEdit and ( ( Key >= #32 ) or ( Key = Char( vk_Back ) ) or ( Key = Char( vk_Delete ) ) ) then
  begin
    Key := #0;
    InvalidKeyPressed;
  end;

  if FShortCutPressed then
  begin
    Key := #0;
  end;
end;


procedure TRzCustomButtonEdit.AltBtnClick;
begin
  if Assigned( FOnAltBtnClick ) then
    FOnAltBtnClick( Self );
end;

procedure TRzCustomButtonEdit.AltBtnClickHandler( Sender: TObject );
begin
  AltBtnClick;
end;

procedure TRzCustomButtonEdit.ButtonClick;
begin
  if Assigned( FOnButtonClick ) then
    FOnButtonClick( Self );
end;

procedure TRzCustomButtonEdit.ButtonClickHandler( Sender: TObject );
begin
  ButtonClick;
end;


procedure TRzCustomButtonEdit.SetAlignment( Value: TAlignment );
begin
  inherited;
  SetEditRect;
end;


procedure TRzCustomButtonEdit.SetFrameStyle( Value: TFrameStyle );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzCustomButtonEdit.SetFlatButtons( Value: Boolean );
begin
  inherited;
  FButtons.Flat := Value;
  ResizeButtons;
end;


procedure TRzCustomButtonEdit.SetHideButtonsOnReadOnly( Value: Boolean );
begin
  if FHideButtonsOnReadOnly <> Value then
  begin
    FHideButtonsOnReadOnly := Value;
    ReadOnlyChanged;
  end;
end;


function TRzCustomButtonEdit.GetButton( Index: Integer ): TRzControlButton;
begin
  if Index = 1 then
    Result := FButtons.AltBtn
  else
    Result := FButtons.Button;
end;


function TRzCustomButtonEdit.IsCustomAltBtnGlyph: Boolean;
begin
  Result := FAltBtnKind = bkCustom;
end;

function TRzCustomButtonEdit.GetAltBtnGlyph: TBitmap;
begin
  Result := AltBtn.Glyph
end;

procedure TRzCustomButtonEdit.SetAltBtnGlyph( Value: TBitmap );
begin
  AltBtn.Glyph := Value;
  FAltBtnKind := bkCustom;
end;


function TRzCustomButtonEdit.IsCustomButtonGlyph: Boolean;
begin
  Result := FButtonKind = bkCustom;
end;

function TRzCustomButtonEdit.GetButtonGlyph: TBitmap;
begin
  Result := Button.Glyph;
end;

procedure TRzCustomButtonEdit.SetButtonGlyph( Value: TBitmap );
begin
  Button.Glyph := Value;
  FButtonKind := bkCustom;
end;


function TRzCustomButtonEdit.GetButtonHint( Index: Integer ): string;
begin
  if Index = 1 then
    Result := AltBtn.Hint
  else
    Result := Button.Hint;
end;

procedure TRzCustomButtonEdit.SetButtonHint( Index: Integer; const Value: string );
begin
  if Index = 1 then
    AltBtn.Hint := Value
  else
    Button.Hint := Value;
end;


function TRzCustomButtonEdit.GetButtonKind( Index: Integer ): TButtonKind;
begin
  if Index = 1 then
    Result := FAltBtnKind
  else
    Result := FButtonKind;
end;

procedure TRzCustomButtonEdit.SetButtonKind( Index: Integer; Value: TButtonKind );
begin
  if Index = 1 then
  begin
    if FAltBtnKind <> Value then
    begin
      FAltBtnKind := Value;
      if ( FAltBtnKind <> bkCustom ) and not ( csLoading in ComponentState ) then
      begin
        AltBtn.Glyph.Assign( GetBtnEdtGlyph( FAltBtnKind ) );
        AltBtn.NumGlyphs := 2;
      end;
    end;
  end
  else
  begin
    if FButtonKind <> Value then
    begin
      FButtonKind := Value;
      if ( FButtonKind <> bkCustom ) and not ( csLoading in ComponentState ) then
      begin
        Button.Glyph.Assign( GetBtnEdtGlyph( FButtonKind ) );
        Button.NumGlyphs := 2;
      end;
    end;
  end;
end; {= TRzCustomButtonEdit.SetButtonKind =}


function TRzCustomButtonEdit.GetButtonNumGlyphs( Index: Integer ): TNumGlyphs;
begin
  if Index = 1 then
    Result := AltBtn.NumGlyphs
  else
    Result := Button.NumGlyphs;
end;

procedure TRzCustomButtonEdit.SetButtonNumGlyphs( Index: Integer; Value: TNumGlyphs );
begin
  if Index = 1 then
    AltBtn.NumGlyphs := Value
  else
    Button.NumGlyphs := Value;
end;


function TRzCustomButtonEdit.GetButtonShortCut( Index: Integer ): TShortCut;
begin
  if Index = 1 then
    Result := FAltBtnShortCut
  else
    Result := FButtonShortCut;
end;

procedure TRzCustomButtonEdit.SetButtonShortCut( Index: Integer; Value: TShortCut );
begin
  if Index = 1 then
    FAltBtnShortCut := Value
  else
    FButtonShortCut := Value;
end;


function TRzCustomButtonEdit.GetButtonVisible( Index: Integer ): Boolean;
begin
  if Index = 1 then
    Result := AltBtn.Visible
  else
    Result := Button.Visible;
end;

procedure TRzCustomButtonEdit.SetButtonVisible( Index: Integer; Value: Boolean );
begin
  if Index = 1 then
    AltBtn.Visible := Value
  else
    Button.Visible := Value;
  ResizeButtons;
end;


function TRzCustomButtonEdit.GetButtonWidth( Index: Integer ): Integer;
begin
  if Index = 1 then
    Result := AltBtn.Width
  else
    Result := Button.Width;
end;

procedure TRzCustomButtonEdit.SetButtonWidth( Index: Integer; Value: Integer );
begin
  if Index = 1 then
    AltBtn.Width := Value
  else
    Button.Width := Value;
  ResizeButtons;
end;


procedure TRzCustomButtonEdit.WMPaste( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;


procedure TRzCustomButtonEdit.WMCut( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;


procedure TRzCustomButtonEdit.CMEnter( var Msg: TCMEnter );
begin
  SetEditRect; //!! remove
  if AutoSelect and not ( csLButtonDown in ControlState ) then
    SelectAll;
  inherited;
end;


procedure TRzCustomButtonEdit.CMExit( var Msg: TCMExit );
begin
  inherited;
  SetEditRect; //!! remove
end;


function TRzCustomButtonEdit.GetEditRect: TRect;
begin
  Result := inherited GetEditRect;
  if not ( ReadOnlyValue and FHideButtonsOnReadOnly ) then
    Dec( Result.Right, FButtons.Width + 2 );
end;


procedure TRzCustomButtonEdit.SetEditRect;
begin
  if ReadOnlyValue and FHideButtonsOnReadOnly then
  begin
    SendMessage( Handle, em_SetMargins, ec_LeftMargin, 0 );
    SendMessage( Handle, em_SetMargins, ec_RightMargin, 0 );
    Exit;
  end;

  if not UseRightToLeftAlignment then
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


procedure TRzCustomButtonEdit.ResizeButtons;
var
  W, MinHeight: Integer;
begin
  if not ( csLoading in ComponentState ) then
  begin
    MinHeight := GetMinFontHeight( Font );
      { text edit bug: if size to less than minheight, then edit ctrl does
        not display the text }
    if Height < MinHeight then
      Height := MinHeight
    else if FButtons <> nil then
    begin
      if Button.Visible and not AltBtn.Visible then
        W := Button.Width
      else if Button.Visible and AltBtn.Visible then
      begin
        W := Button.Width + AltBtn.Width;
      end
      else if not Button.Visible and AltBtn.Visible then
        W := AltBtn.Width
      else { Neither one visible... }
        W := 0;

      if not UseRightToLeftAlignment then
      begin
        if not FrameVisible then
        begin
          if Ctl3D then
            FButtons.SetBounds( Width - W - 4, 0, W, Height - 3 )
          else
            FButtons.SetBounds( Width - W - 1, 1, W, Height - 3 );
        end
        else
          FButtons.SetBounds( Width - W - 4, 0, W, Height - 3 );
      end
      else
      begin
        FButtons.SetBounds( 0, 0, W, Height - 3 );
      end;

      SetEditRect;
      Invalidate;
    end;
  end;
end; {= TRzCustomButtonEdit.ResizeButtons =}


procedure TRzCustomButtonEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzCustomButtonEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
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
  end
end;


procedure TRzCustomButtonEdit.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect; //!! remove
  FButtons.Enabled := Enabled;
  if FlatButtons then
    FButtons.Color := Color;
end;


procedure TRzCustomButtonEdit.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect; //!! remove
  FButtons.Enabled := Enabled;
end;


procedure TRzCustomButtonEdit.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if FButtons <> nil then
    FButtons.Color := Color;
end;


procedure TRzCustomButtonEdit.ReadOnlyChanged;
begin
  inherited;
  if FButtons <> nil then
    FButtons.Visible := not ( ReadOnlyValue and FHideButtonsOnReadOnly );
  ResizeButtons;
end;



{=================================}
{== Initialization/Finalization ==}
{=================================}

procedure FreeBitmaps; far;
var
  I: TButtonKind;
begin
  for I := Low( TButtonKind ) to High( TButtonKind ) do
    BtnEdtGlyphs[ I ].Free;
end;


initialization
  FillChar( BtnEdtGlyphs, SizeOf( BtnEdtGlyphs ), 0 );
  {&RUI}

finalization
  FreeBitmaps;
end.
