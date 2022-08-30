{===============================================================================
  RzDBChk Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBCheckBox
    Data-Aware TRzCheckBox


  Modification History
  ------------------------------------------------------------------------------
  5.5.1  (31 Mar 2011)
    * Surfaced DoubleBuffered property in TRzDBCheckBox.
  ------------------------------------------------------------------------------
  5.4    (14 Sep 2010)
    * Fixed issue where CustomGlyphImages would not appear on replicated
      TRzDBCheckBox controls on a TDBCtrlGrid.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Added new WordWrap property to TRzDBCheckBox.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new AutoSize property to TRzDBCheckBox. When this property is True
      (the default), the bounds of the control are automatically adjusted so
      that the entire caption is visible. This also allows the focus rectangle
      to surround just the caption of the control rather than just the entire
      client area.
    * Added new CustomGlyphImages property to TRzRadioButton and TRzCheckBox.
      This property is used to reference an ImageList that contains the glyphs
      to be used for the various states of the control. This new property should
      be used instead of the deprecated CustomGlyphs property, which is still
      available strictly for backward compatibility. By referencing an ImageList
      that holds the custom glyphs rather than an embedded bitmap, the actual
      glyph images are stored only once in the application instead of inside
      each instance of the control. When populating a TImageList for use with
      CustomGlyphImages, each index in the ImageList represents a different
      state.  Please see the RzRadChk.pas comments for details on how states
      are mapped to images.
    * Registered custom property editor for the Caption property of
      TRzDBCheckBox and TRzRadioButton. The editor allows the developer to
      display a dialog box that can be used to enter multi-line captions at
      design-time.
    * Added new ThemeAware property to TRzDBCheckBox. When this property is set
      to True (the default) the control will use XP/Vista themes for display (if
      available).  When set to False, the control will be drawn using the visual
      property setttings of the controls even if themes are available.
  ------------------------------------------------------------------------------
  4.0.1  (07 Jan 2006)
    * Added FillColor and FocusColor properties to TRzDBCheckbox. These
      properties affect the interior of the check box glyph. When the control is
      focused, the interior is filled with the FocusColor otherwise the
      FillColor is used. These properties can also be controlled through a
      connected TRzFrameController.
    * Added HotTrackStyle property to TRzDBCheckBox, which determines the
      appearance of hot track highlighting when the HotTrack property is set to
      True. The default of htsInterior is identical to previous versions where
      the interior is highlighted.  When set to htsFrame the frame of the box is
      highlighted to be thicker--the same appearance as setting FrameHotStyle to
      fsFlatBold in TRzEdit and others. The TRzFrameController can also be used
      to manage the this new property and apperance.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Surfaced Align property in TRzDBCheckBox.
    * Added new FrameControllerNotifications property to TRzDBCheckBox.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem in TRzDBCheckBox.WMPaint where Msg.DC was being passed to
      SendMessage as a WParam and was not casted to a WParam (i.e. Longint).
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzCheckBox.
===============================================================================}

{$I RzComps.inc}

unit RzDBChk;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  DB,
  Menus,
  RzCommon,
  RzButton,
  DBCtrls,
  RzRadChk;

type
  TRzDBCheckBox = class( TRzCustomCheckBox )
  private
    FAboutInfo: TRzAboutInfo;
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;
    FNullAsUnchecked: Boolean;
    FPaintControl: TRzCheckBox;

    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );
    procedure UpdateData( Sender: TObject );

    { Message Handling Methods }
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMGetDataLink( var Msg: TMessage ); message cm_GetDataLink;
  protected
    procedure ChangeState; override;
    procedure KeyPress( var Key: Char ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function ValueMatch( const ValueList, Value: string ): Boolean;

    { Property Access Methods }
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetField: TField; virtual;
    function GetFieldState: TCheckBoxState; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); override;
    procedure SetValueCheck( const Value: string ); virtual;
    procedure SetValueUncheck( const Value: string ); virtual;
    procedure SetNullAsUnchecked( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Checked;
    property Field: TField
      read GetField;
    property State;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property DataField: string
      read GetDataField
      write SetDataField;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    property NullAsUnchecked: Boolean
      read FNullAsUnchecked
      write SetNullAsUnchecked
      default True;

    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;

    property ValueChecked: string
      read FValueCheck
      write SetValueCheck;

    property ValueUnchecked: string
      read FValueUncheck
      write SetValueUncheck;


    { Inherited Properties & Events }
    property Action;
    property Align;
    property Alignment;
    property AlignmentVertical default avTop;
    property AllowGrayed;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property CustomGlyphs;
    property CustomGlyphImages;
    property DisabledColor;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FillColor;
    property FocusColor;
    property FrameColor;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property Height;
    property HelpContext;
    property HighlightColor;
    property Hint;
    property HotTrack;
    property HotTrackColor;
    property HotTrackColorType;
    property HotTrackStyle;
    property LightTextStyle;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TextHighlightColor;
    property TextShadowColor;
    property TextShadowDepth;
    property ShowHint;
    property TabOnEnter;
    property TabOrder;
    property TabStop default True;
    property TextStyle;
    property ThemeAware;
    property Transparent;
    property TransparentColor;
    property UseCustomGlyphs;
    property Visible;
    property WinMaskColor;
    property WordWrap;

    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

uses
  {&RAS}
  DBConsts;

{&RT}
constructor TRzDBCheckBox.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle + [ csReplicatable ];
  State := cbUnchecked;

  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;

  FNullAsUnchecked := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  {&RCI}

  FPaintControl := TRzCheckBox.Create( Self );
  FPaintControl.Parent := Self;
  FPaintControl.Visible := False;
end;


destructor TRzDBCheckBox.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBCheckBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


function TRzDBCheckBox.GetFieldState: TCheckBoxState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      if FNullAsUnchecked then
        Result := cbUnchecked
      else
        Result := cbGrayed
    end
    else if FDataLink.Field.DataType = ftBoolean then
      if FDataLink.Field.AsBoolean then
        Result := cbChecked
      else
        Result := cbUnchecked
    else
    begin
      Result := cbGrayed;
      Text := FDataLink.Field.Text;
      if ValueMatch( FValueCheck, Text ) then
        Result := cbChecked
      else if ValueMatch( FValueUncheck, Text ) then
        Result := cbUnchecked;
    end;
  end
  else
    Result := cbUnchecked;
end;


procedure TRzDBCheckBox.DataChange( Sender: TObject );
begin
  State := GetFieldState;
end;


procedure TRzDBCheckBox.UpdateData( Sender: TObject );
var
  Pos: Integer;
  S: string;
begin
  if State = cbGrayed then
    FDataLink.Field.Clear
  else
  begin
    if FDataLink.Field.DataType = ftBoolean then
      FDataLink.Field.AsBoolean := Checked
    else
    begin
      if Checked then
        S := FValueCheck
      else
        S := FValueUncheck;
      Pos := 1;
      FDataLink.Field.Text := ExtractFieldName( S, Pos );
    end;
  end;
end;


function TRzDBCheckBox.ValueMatch( const ValueList, Value: string ): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  Pos := 1;
  while Pos <= Length( ValueList ) do
    if AnsiCompareText( ExtractFieldName( ValueList, Pos ), Value ) = 0 then
    begin
      Result := True;
      Break;
    end;
end;


procedure TRzDBCheckBox.ChangeState;
begin
  {&RV}
  if FDataLink.Edit then
  begin
    inherited;
    FDataLink.Modified;
  end;
end;


function TRzDBCheckBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRzDBCheckBox.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;

function TRzDBCheckBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRzDBCheckBox.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;

function TRzDBCheckBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRzDBCheckBox.SetReadOnly( Value: Boolean );
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

function TRzDBCheckBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TRzDBCheckBox.KeyPress( var Key: Char );
begin
  inherited;
  case Key of
    #8, ' ':
      FDataLink.Edit;

    #27:
    begin
      FDataLink.Reset;
    end;
  end;
end;

procedure TRzDBCheckBox.SetValueCheck( const Value: string );
begin
  FValueCheck := Value;
  DataChange( Self );
end;

procedure TRzDBCheckBox.SetValueUncheck( const Value: string );
begin
  FValueUncheck := Value;
  DataChange( Self );
end;

procedure TRzDBCheckBox.SetNullAsUnchecked( Value: Boolean );
begin
  if FNullAsUnchecked <> Value then
  begin
    FNullAsUnchecked := Value;
    Invalidate;
  end;
end;



procedure TRzDBCheckBox.WMPaint( var Msg: TWMPaint );
var
  S: string;
begin
  if csPaintCopy in ControlState then
  begin
    if Field <> nil then
      S := Field.Text
    else
      S := '';

    FPaintControl.SetBounds( BoundsRect.Left, BoundsRect.Top,
                             BoundsRect.Right - BoundsRect.Left,
                             BoundsRect.Bottom - BoundsRect.Top );

    FPaintControl.Alignment := Alignment;
    FPaintControl.Caption := Caption;
    if UseCustomGlyphs then
    begin
      FPaintControl.CustomGlyphImages := CustomGlyphImages;
      FPaintControl.CustomGlyphs := CustomGlyphs;
      FPaintControl.CustomGlyphs := CustomGlyphs;
      FPaintControl.WinMaskColor := WinMaskColor;
      FPaintControl.TransparentColor := TransparentColor;
    end;

    if Field <> nil then
    begin
      if Field.IsNull then
      begin
        if FNullAsUnchecked then
          FPaintControl.State := cbUnchecked
        else
          FPaintControl.State := cbGrayed
      end
      else if Field.DataType = ftBoolean then
        if Field.AsBoolean then
          FPaintControl.State := cbChecked
        else
          FPaintControl.State := cbUnchecked
      else
      begin
        FPaintControl.State := cbGrayed;
        if ValueMatch( FValueCheck, S ) then
          FPaintControl.State := cbChecked
        else if ValueMatch( FValueUncheck, S ) then
          FPaintControl.State := cbUnchecked;
      end;
    end
    else
      FPaintControl.State := cbUnchecked;

    SendMessage( FPaintControl.Handle, wm_Paint, WParam( Msg.DC ), 0 );
  end
  else
  begin
    FPaintControl.SetBounds( 0, 0, 0, 0 );
    inherited;
  end;
end; {= TRzDBCheckBox.WMPaint =}


procedure TRzDBCheckBox.CMExit( var Msg: TCMExit );
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;

procedure TRzDBCheckBox.CMGetDataLink( var Msg: TMessage );
begin
  Msg.Result := Integer( FDataLink );
end;

{&RUIF}
end.
