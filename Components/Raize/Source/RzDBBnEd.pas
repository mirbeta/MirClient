{===============================================================================
  RzDBBnEd Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBButtonEdit
    Data-Aware TRzButtonEdit


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Cleaned up the appearance of embedded buttons in TRzDBButtonEdit under
      Windows Vista and Windows 7.
    * Made necessary modifications to TRzDBButtonEdit to fully support VCL 
      Styles introduced in RAD Studio XE2.
    * Made necessary modifications to TRzDBButtonEdit to support 64-bit.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Updated the display of embedded buttons in TRzDBButtonEdit when running
      under Windows Vista and Windows 7.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzDBButtonEdit control.
  ------------------------------------------------------------------------------
  5.1    (16 Mar 2009)
    * Fixed problem in TRzButtonEdit where trapping keys in an OnKeyDown event
      handler (by setting Key := 0) would result in the OnButtonClick event or
      the OnAltBtnClick event being fired if the ShortCut for either button was
      set to scNone.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Added HideButtonsOnReadOnly property to TRzDBButtonEdit. When this
      property is True (the default), the buttons embedded in the control are
      hidden when the ReadOnly property is set to True.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added ReadOnlyColor property to TRzDBButtonEdit. This color property is
      used to change the color of the control when the ReadOnly property is set
      to True.
    * When the ReadOnly property for a TRzDBButtonEdit is set to True, the
      embedded buttons are hidden.
    * Added new FrameControllerNotifications property to TRzDBButtonEdit.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * The color of flat buttons in TRzDBButtonEdit are now adjusted
      appropriately when the control is disabled and re-enabled.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Fixed display problem when button clicked and focus taken away by an
      exception.
    * The OnChange event is no longer fired when the TRzDBButtonEdit control
      receives the keyboard focus.
    * The Buttons are now placed on the left side of the control for Right-To-
      Left locales.
    * Inherits XP visual style support from TRzCustomEdit.
===============================================================================}

{$I RzComps.inc}

unit RzDBBnEd;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Buttons,
  RzBtnEdt,
  StdCtrls,
  DB,
  DBCtrls,
  Graphics,
  Classes,
  Controls,
  Menus,
  RzCommon,
  RzButton,
  RzDBEdit;

type
  {=======================================}
  {== TRzDBButtonEdit Class Declaration ==}
  {=======================================}

  TRzDBButtonEdit = class( TRzDBEdit )
  private
    FInternalUpdate: Boolean;
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
    procedure CreateWnd; override;
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
    procedure AdjustEditRect; override;
    function GetEditRect: TRect; override;
    procedure SetEditRect; virtual;

    procedure AltBtnClickHandler( Sender: TObject ); virtual;
    procedure ButtonClickHandler( Sender: TObject ); virtual;

    { Event Dispatch Methods }
    procedure AltBtnClick; dynamic;
    procedure ButtonClick; dynamic;

    procedure Change; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState ); override;
    procedure KeyPress(var Key: Char); override;

    { Property Access Methods }
    procedure SetFrameStyle( Value: TFrameStyle ); override;
    procedure SetFrameVisible( Value: Boolean ); override;
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
  published
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
    property Enabled;
    property FlatButtons;
  end;


implementation

uses
  {&RAS}
  Themes,
  SysUtils,
  Forms;

{&RT}
{=============================}
{== TRzDBButtonEdit Methods ==}
{=============================}

constructor TRzDBButtonEdit.Create( AOwner: TComponent );
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
  FButtons.ControlStyle := FButtons.ControlStyle + [ csReplicatable ];
  FButtons.Button.ControlStyle := FButtons.Button.ControlStyle + [ csReplicatable ];
  FButtons.AltBtn.ControlStyle := FButtons.AltBtn.ControlStyle + [ csReplicatable ];

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


procedure TRzDBButtonEdit.CreateParams( var Params: TCreateParams );
begin
  inherited;
  Params.Style := Params.Style or ws_ClipChildren;
end;


procedure TRzDBButtonEdit.CreateWnd;
begin
  inherited;
  SetEditRect;
  {&RV}
end;


procedure TRzDBButtonEdit.Loaded;
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


procedure TRzDBButtonEdit.DefineProperties( Filer: TFiler );
begin
  inherited;
  Filer.DefineProperty( 'ButtonShortCutText', ReadButtonShortCutText, WriteShortCutText, False );
  Filer.DefineProperty( 'AltBtnShortCutText', ReadAltBtnShortCutText, WriteShortCutText, False );
  // Handle the fact that the FlatButtonParentColor was published in version 2.x
  Filer.DefineProperty( 'FlatButtonParentColor', TRzOldPropReader.ReadOldBooleanProp, nil, False );
  { Handle the fact that the Text property was published in version 1.6 }
  Filer.DefineProperty( 'Text', TRzOldPropReader.ReadOldStringProp, nil, False );
end;


procedure TRzDBButtonEdit.ReadButtonShortCutText( Reader: TReader );
begin
  FButtonShortCut := TextToShortCut( Reader.ReadString );
end;


procedure TRzDBButtonEdit.ReadAltBtnShortCutText( Reader: TReader );
begin
  FAltBtnShortCut := TextToShortCut( Reader.ReadString );
end;


procedure TRzDBButtonEdit.WriteShortCutText( Writer: TWriter );
begin
end;


procedure TRzDBButtonEdit.GetChildren( Proc: TGetChildProc; Root: TComponent );
begin
end;


procedure TRzDBButtonEdit.Change;
begin
  if not FInternalUpdate then
    inherited;
end;


procedure TRzDBButtonEdit.KeyDown( var Key: Word; Shift: TShiftState );
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
end; {= TRzDBButtonEdit.KeyDown =}


procedure TRzDBButtonEdit.KeyPress( var Key: Char );
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


procedure TRzDBButtonEdit.AltBtnClick;
begin
  if Assigned( FOnAltBtnClick ) then
    FOnAltBtnClick( Self );
end;

procedure TRzDBButtonEdit.AltBtnClickHandler( Sender: TObject );
begin
  AltBtnClick;
end;

procedure TRzDBButtonEdit.ButtonClick;
begin
  if Assigned( FOnButtonClick ) then
    FOnButtonClick( Self );
end;

procedure TRzDBButtonEdit.ButtonClickHandler( Sender: TObject );
begin
  ButtonClick;
end;


procedure TRzDBButtonEdit.SetFrameStyle( Value: TFrameStyle );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzDBButtonEdit.SetFrameVisible( Value: Boolean );
begin
  inherited;
  UpdateFrame( False, False );
end;


procedure TRzDBButtonEdit.SetFlatButtons( Value: Boolean );
begin
  inherited;
  FButtons.Flat := Value;
  ResizeButtons;
end;


procedure TRzDBButtonEdit.SetHideButtonsOnReadOnly( Value: Boolean );
begin
  if FHideButtonsOnReadOnly <> Value then
  begin
    FHideButtonsOnReadOnly := Value;
    ReadOnlyChanged;
  end;
end;


function TRzDBButtonEdit.GetButton( Index: Integer ): TRzControlButton;
begin
  if Index = 1 then
    Result := FButtons.AltBtn
  else
    Result := FButtons.Button;
end;


function TRzDBButtonEdit.IsCustomAltBtnGlyph: Boolean;
begin
  Result := FAltBtnKind = bkCustom;
end;

function TRzDBButtonEdit.GetAltBtnGlyph: TBitmap;
begin
  Result := AltBtn.Glyph
end;

procedure TRzDBButtonEdit.SetAltBtnGlyph( Value: TBitmap );
begin
  AltBtn.Glyph := Value;
  FAltBtnKind := bkCustom;
end;


function TRzDBButtonEdit.IsCustomButtonGlyph: Boolean;
begin
  Result := FButtonKind = bkCustom;
end;

function TRzDBButtonEdit.GetButtonGlyph: TBitmap;
begin
  Result := Button.Glyph;
end;

procedure TRzDBButtonEdit.SetButtonGlyph( Value: TBitmap );
begin
  Button.Glyph := Value;
  FButtonKind := bkCustom;
end;


function TRzDBButtonEdit.GetButtonHint( Index: Integer ): string;
begin
  if Index = 1 then
    Result := AltBtn.Hint
  else
    Result := Button.Hint;
end;

procedure TRzDBButtonEdit.SetButtonHint( Index: Integer; const Value: string );
begin
  if Index = 1 then
    AltBtn.Hint := Value
  else
    Button.Hint := Value;
end;


function TRzDBButtonEdit.GetButtonKind( Index: Integer ): TButtonKind;
begin
  if Index = 1 then
    Result := FAltBtnKind
  else
    Result := FButtonKind;
end;

procedure TRzDBButtonEdit.SetButtonKind( Index: Integer; Value: TButtonKind );
begin
  if Index = 1 then
  begin
    if FAltBtnKind <> Value then
    begin
      FAltBtnKind := Value;
      if ( Value <> bkCustom ) and not ( csLoading in ComponentState ) then
      begin
        AltBtn.Glyph.Assign( GetBtnEdtGlyph( Value ) );
        AltBtn.NumGlyphs := 2;
      end;
    end;
  end
  else
  begin
    if FButtonKind <> Value then
    begin
      FButtonKind := Value;
      if ( Value <> bkCustom ) and not ( csLoading in ComponentState ) then
      begin
        Button.Glyph.Assign( GetBtnEdtGlyph( Value ) );
        Button.NumGlyphs := 2;
      end;
    end;
  end;
end; {= TRzDBButtonEdit.SetButtonKind =}


function TRzDBButtonEdit.GetButtonNumGlyphs( Index: Integer ): TNumGlyphs;
begin
  if Index = 1 then
    Result := AltBtn.NumGlyphs
  else
    Result := Button.NumGlyphs;
end;

procedure TRzDBButtonEdit.SetButtonNumGlyphs( Index: Integer; Value: TNumGlyphs );
begin
  if Index = 1 then
    AltBtn.NumGlyphs := Value
  else
    Button.NumGlyphs := Value;
end;


function TRzDBButtonEdit.GetButtonShortCut( Index: Integer ): TShortCut;
begin
  if Index = 1 then
    Result := FAltBtnShortCut
  else
    Result := FButtonShortCut;
end;

procedure TRzDBButtonEdit.SetButtonShortCut( Index: Integer; Value: TShortCut );
begin
  if Index = 1 then
    FAltBtnShortCut := Value
  else
    FButtonShortCut := Value;
end;


function TRzDBButtonEdit.GetButtonVisible( Index: Integer ): Boolean;
begin
  if Index = 1 then
    Result := AltBtn.Visible
  else
    Result := Button.Visible;
end;

procedure TRzDBButtonEdit.SetButtonVisible( Index: Integer; Value: Boolean );
begin
  if Index = 1 then
    AltBtn.Visible := Value
  else
    Button.Visible := Value;
  ResizeButtons;
end;


function TRzDBButtonEdit.GetButtonWidth( Index: Integer ): Integer;
begin
  if Index = 1 then
    Result := AltBtn.Width
  else
    Result := Button.Width;
end;

procedure TRzDBButtonEdit.SetButtonWidth( Index: Integer; Value: Integer );
begin
  if Index = 1 then
    AltBtn.Width := Value
  else
    Button.Width := Value;
  ResizeButtons;
end;


procedure TRzDBButtonEdit.WMPaste( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;


procedure TRzDBButtonEdit.WMCut( var Msg: TWMPaste );
begin
  if not FAllowKeyEdit or ReadOnly then
    Exit;
  inherited;
end;


procedure TRzDBButtonEdit.CMEnter( var Msg: TCMEnter );
var
  S: string;
begin
  SetEditRect;
  { Set the control's text manually, so new margins take effect }
  S := Text;
  FInternalUpdate := True;
  try
    SendTextMessage( Handle, wm_SetText, 0, S );
  finally
    FInternalUpdate := False;
  end;

  if AutoSelect and not ( csLButtonDown in ControlState ) then
    SelectAll;
  inherited;
end;


procedure TRzDBButtonEdit.CMExit( var Msg: TCMExit );
begin
  inherited;
  SetEditRect;
end;


procedure TRzDBButtonEdit.AdjustEditRect;
begin
  inherited;

  if ( Parent <> nil ) and Parent.ClassNameIs( 'TDBCtrlPanel' ) then
  begin
    // Need to refresh buttons so they appear when control in on a DBCtrlGrid
    FButtons.Refresh;
  end;
end;


function TRzDBButtonEdit.GetEditRect: TRect;
begin
  Result := inherited GetEditRect;
  if not ( ReadOnlyValue and FHideButtonsOnReadOnly ) then
    Dec( Result.Right, FButtons.Width + 2 );
end;


procedure TRzDBButtonEdit.SetEditRect;
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


procedure TRzDBButtonEdit.ResizeButtons;
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
        begin
          if ( Parent <> nil ) and Parent.ClassNameIs( 'TDBCtrlPanel' ) then
            FButtons.SetBounds( Width - W - 2, 2, W, Height - 3 )
          else
            FButtons.SetBounds( Width - W - 4, 0, W, Height - 3 );
        end;
      end
      else
      begin
        if ( Parent <> nil ) and Parent.ClassNameIs( 'TDBCtrlPanel' ) then
          FButtons.SetBounds( 0, 2, W, Height - 4 )
        else
          FButtons.SetBounds( 0, 0, W, Height - 3 );
      end;

      SetEditRect;
    end;
  end;
end; {= TRzDBButtonEdit.ResizeButtons =}


procedure TRzDBButtonEdit.WMSize( var Msg: TWMSize );
begin
  inherited;
  ResizeButtons;
end;


procedure TRzDBButtonEdit.UpdateFrame( ViaMouse, InFocus: Boolean );
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


procedure TRzDBButtonEdit.CMEnabledChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect;
  FButtons.Enabled := Enabled;
  if FlatButtons then
    FButtons.Color := Color;
end;


procedure TRzDBButtonEdit.CMFontChanged( var Msg: TMessage );
begin
  inherited;
  SetEditRect;
  FButtons.Enabled := Enabled;
end;


procedure TRzDBButtonEdit.CMColorChanged( var Msg: TMessage );
begin
  inherited;
  if FButtons <> nil then
    FButtons.Color := Color;
end;


procedure TRzDBButtonEdit.ReadOnlyChanged;
begin
  inherited;
  if FButtons <> nil then
    FButtons.Visible := not ( ReadOnlyValue and FHideButtonsOnReadOnly );
  ResizeButtons;
end;


{&RUIF}
end.
