{===============================================================================
  RzDBRGrp Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBRadioGroup
    Data-Aware TRzRadioGroup


  Modification History
  ------------------------------------------------------------------------------
  5.5    (06 Mar 2011)
    * Added new TRzDBCheckBoxGroup control. This specialized group box has a
      check box embedded next to the group's Caption. The check box state is
      controlled from the DataSource and DataField properties. The ValueCheck
      and ValueUncheck properties allow non Boolean values to be used to control
      the state of the check box. The TRzDBCheckBoxGroup inherits the
      EnableControlsOnCheck property, which allows child controls to be
      automatically enabled/disabled based on the state of the check box.
    * Fixed issue in TRzDBRadioGroup where using the arrow keys to change the
      selected radio button would not place the dataset into edit mode.
    * Fixed issue in TRzDBRadioGroup where pressing an accelerator character
      defined for an Item's caption would not result in the dataset being placed
      into edit mode.
  ------------------------------------------------------------------------------
  5.3    (07 Feb 2010)
    * Added new CaptionFont property to TRzDBRadioGroup.
    * Added ReadOnlyColor, and ReadOnlyColorOnFocus properties to the
      TRzDBRadioGroup.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzDBRadioGroup control.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Added new CustomGlyphImages property to TRzDBRadioGroup. This property is
      used to reference an ImageList that contains the glyphs to be used for the
      various states of the control. This new property should be used instead of
      the deprecated CustomGlyphs property, which is still available strictly
      for backward compatibility. By referencing an ImageList that holds the
      custom glyphs rather than an embedded bitmap, the actual glyph images are
      stored only once in the application instead of inside each instance of the
      control. When populating a TImageList for use with CustomGlyphImages, each
      index in the ImageList represents a different state.  The following tables
      describe the mapping:

      TRzRadioGroup CustomGlyphImages Index Mapping
        Index  State
          0    Unchecked
          1    Checked
          2    Unchecked - Pressed
          3    Checked   - Pressed
          4    Unchecked - Disabled
          5    Checked   - Disabled
          6    Unchecked - Hot       (Optional)
          7    Checked   - Hot       (Optional)
    * As noted in the above table, with the new CustomGlyphImages property, it
      is now possible to specify "hot" glyphs. That is, separate images to be
      displayed for a given state when the mouse is positioned over the control.
    * Added new BannerHeight property to TRzDBRadioGroup, which is used to
      control the height of the banner area when the gsBanner GroupStyle is
      used. By default, this property is 0, which instructs the control to
      determine the height of the Banner based on the font size. If BannerHeight
      is set to a non-zero value, the height of the banner is sized accordingly.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * The TRzDBRadioGroup inherits all of the new features and functionality 
      added to the TRzCustomPanel and TRzGroupBox components including the new 
      ViewStyle property, and the picking up of XP themes for Captions for all 
      GroupStyle values.
    * Fixed problem in TRzDBRadioGroup where radio buttons would take up extra 
      space if their captions contained accelerator characters (&).
    * Fixed positioning of radio buttons when group caption is empty and 
      GroupStyle is gsFlat.
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * Surfaced the OnPaint event in TRzDBRadioGroup. This is useful when
      GroupStyle is set to gsCustom.
    * When a TRzDBRadioGroup is connected to a dataset that is not active, the
      radio-group disables itself.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzRadioGroup.
===============================================================================}

{$I RzComps.inc}

unit RzDBRGrp;

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
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  RzRadGrp,
  Menus,
  DB,
  RzPanel,
  DBCtrls,
  ExtCtrls,
  RzCommon;

type
  TRzDBRadioGroup = class( TRzCustomRadioGroup )
  private
    FAboutInfo: TRzAboutInfo;
    FDataLink: TFieldDataLink;
    FValue: string;
    FValues: TStrings;
    FInSetValue: Boolean;
    FOnChange: TNotifyEvent;

    { Internal Event Handlers }
    procedure ActiveChange( Sender: TObject );
    procedure DataChange( Sender: TObject );
    procedure UpdateData( Sender: TObject );

    { Message Handling Methods }
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Event Dispatch Methods }
    procedure Change; dynamic;
    procedure Click; override;
    procedure KeyPress( var Key: Char ); override;
    function CanModify: Boolean; override;

    { Property Access Methods }
    function GetField: TField; virtual;
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); override;
    function GetButtonValue( Index: Integer ): string; virtual;
    procedure SetValue( const Value: string ); virtual;
    procedure SetItems( Value: TStrings ); override;
    procedure SetValues( Value: TStrings ); virtual;

    { Property Declarations }
    property DataLink: TFieldDataLink
      read FDataLink;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function ExecuteAction( Action: TBasicAction ): Boolean; override;
    function UpdateAction( Action: TBasicAction ): Boolean; override;

    procedure AddItemValue( const Item, Value: string );

    property Field: TField
      read GetField;

    property Value: string
      read FValue
      write SetValue;

    property Buttons;
    property ItemEnabled;
    property ItemIndex;
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

    property Items
      write SetItems;

    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;

    property Values: TStrings
      read FValues
      write SetValues;

    property OnChange: TNotifyEvent
      read FOnChange
      write FOnChange;

    { Inherited Properties & Events }
    property Align;
    property Alignment;
    property Anchors;
    property BannerHeight;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderWidth;
    property Caption;
    property CaptionFont;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property CustomGlyphs;
    property CustomGlyphImages;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameController;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GroupStyle;
    property Height;
    property ItemFrameColor;
    property ItemHighlightColor;
    property ItemHotTrack;
    property ItemHotTrackColor;
    property ItemHotTrackColorType;
    property ItemFont;
    property ItemHeight;
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
    property ReadOnlyColor;
    property ReadOnlyColorOnFocus;
    property ShowHint;
    property SpaceEvenly;
    property StartXPos;
    property StartYPos;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TextStyle;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Transparent;
    property TransparentColor;
    property UseCustomGlyphs;
    property VerticalSpacing;
    property Visible;
    property VisualStyle;
    property WinMaskColor;

    property OnChanging;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;



  TRzDBCheckBoxGroup = class( TRzCustomGroupBox )
  private
    FAboutInfo: TRzAboutInfo;
    FDataLink: TFieldDataLink;
    FValueCheck: string;
    FValueUncheck: string;

    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );
    procedure UpdateData( Sender: TObject );
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMGetDataLink( var Msg: TMessage ); message cm_GetDataLink;

  protected
    procedure KeyPress( var Key: Char ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    function ValueMatch( const ValueList, Value: string ): Boolean;
    procedure ChangeState; override;

    { Property Access Methods }
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetField: TField; virtual;
    function GetFieldState: TCheckBoxState; virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); virtual;
    procedure SetValueCheck( const Value: string ); virtual;
    procedure SetValueUncheck( const Value: string ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Checked default False;
    property Field: TField
      read GetField;
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
    property Align;
    property Alignment;
    property Anchors;
    property BannerHeight;
    property BevelWidth;
    property BiDiMode;
    property BorderColor;
    property BorderInner;
    property BorderOuter;
    property BorderSides;
    property BorderWidth;
    property Caption;
    property CaptionFont;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property EnableControlsOnCheck;
    property FlatColor;
    property FlatColorAdjustment;
    property Font;
    property FrameControllerNotifications;
    property FrameController;
    property GradientColorStyle;
    property GradientColorStart;
    property GradientColorStop;
    property GradientDirection;
    property GroupStyle;
    property Height;
    property Padding;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowDockClientCaptions;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF VCL140_OR_HIGHER}
    property Touch;
    {$ENDIF}
    property Transparent;
    property Visible;
    property VisualStyle;

    property OnCheckBoxClick;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF VCL140_OR_HIGHER}
    property OnGesture;
    {$ENDIF}
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;




implementation

uses
  {&RAS}
  DBConsts;


{&RT}
{=============================}
{== TRzDBRadioGroup Methods ==}
{=============================}

constructor TRzDBRadioGroup.Create( AOwner: TComponent );
begin
  inherited;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
  FValues := TStringList.Create;
  {&RCI}
end;


destructor TRzDBRadioGroup.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  inherited;
end;


procedure TRzDBRadioGroup.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


procedure TRzDBRadioGroup.AddItemValue( const Item, Value: string );
begin
  Items.Add( Item );
  Values.Add( Value );
end;


procedure TRzDBRadioGroup.DataChange( Sender: TObject );
begin
  if not ( csDestroying in ComponentState ) then
  begin
    if FDataLink.Field <> nil then
      Value := FDataLink.Field.Text
    else
      Value := '';
  end;
end;


procedure TRzDBRadioGroup.UpdateData( Sender: TObject );
begin
  if FDataLink.Field <> nil then
    FDataLink.Field.Text := Value;
end;


procedure TRzDBRadioGroup.ActiveChange( Sender: TObject );
begin
  Enabled := ( FDataLink <> nil ) and FDataLink.Active;
end;


function TRzDBRadioGroup.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBRadioGroup.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


function TRzDBRadioGroup.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBRadioGroup.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;


function TRzDBRadioGroup.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;


procedure TRzDBRadioGroup.SetReadOnly( Value: Boolean );
begin
  inherited SetReadOnly( Value );
  FDataLink.ReadOnly := Value;
end;


function TRzDBRadioGroup.GetField: TField;
begin
  Result := FDataLink.Field;
end;


function TRzDBRadioGroup.GetButtonValue( Index: Integer ): string;
begin
  if ( Index < FValues.Count ) and ( FValues[ Index ] <> '' ) then
    Result := FValues[ Index ]
  else if Index < Items.Count then
    Result := Items[ Index ]
  else
    Result := '';
end;


procedure TRzDBRadioGroup.SetValue( const Value: string );
var
  WasFocused: Boolean;
  I, Index: Integer;
begin
  if FValue <> Value then
  begin
    FInSetValue := True;
    try
      WasFocused := ( ItemIndex > -1 ) and ( Buttons[ ItemIndex ].Focused );
      Index := -1;
      for I := 0 to Items.Count - 1 do
        if Value = GetButtonValue( I ) then
        begin
          Index := I;
          Break;
        end;
      ItemIndex := Index;
      // Move the focus rect along with the selected index
      if WasFocused and ( ItemIndex <> -1 ) then
        Buttons[ ItemIndex ].SetFocus;
    finally
      FInSetValue := False;
    end;
    FValue := Value;
    Change;
  end;
end;


procedure TRzDBRadioGroup.CMExit( var Msg: TCMExit );
begin
  try
    FDataLink.UpdateRecord;
  except
    if ItemIndex >= 0 then
      TRadioButton( Controls[ ItemIndex ] ).SetFocus
    else
      TRadioButton( Controls[ 0 ] ).SetFocus;
    raise;
  end;
  inherited;
end;


procedure TRzDBRadioGroup.Click;
begin
  if not FInSetValue then
  begin
    inherited;
    if ItemIndex >= 0 then
      Value := GetButtonValue( ItemIndex );
    if FDataLink.Editing then
      FDataLink.Modified;
  end;
  {&RV}
end;


procedure TRzDBRadioGroup.SetItems( Value: TStrings );
begin
  Items.Assign( Value );
  DataChange( Self );
end;


procedure TRzDBRadioGroup.SetValues( Value: TStrings );
begin
  FValues.Assign( Value );
  DataChange( Self );
end;


procedure TRzDBRadioGroup.Change;
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;


procedure TRzDBRadioGroup.KeyPress( var Key: Char );
begin
  inherited;
  case Key of
    #8, ' ':
      FDataLink.Edit;
    #27:
      FDataLink.Reset;
  end;
end;


function TRzDBRadioGroup.CanModify: Boolean;
begin
  Result := FDataLink.Edit;
end;


function TRzDBRadioGroup.ExecuteAction( Action: TBasicAction ): Boolean;
begin
  Result := inherited ExecuteAction( Action ) or ( DataLink <> nil ) and DataLink.ExecuteAction( Action );
end;


function TRzDBRadioGroup.UpdateAction( Action: TBasicAction ): Boolean;
begin
  Result := inherited UpdateAction( Action ) or ( DataLink <> nil ) and DataLink.UpdateAction( Action );
end;


{================================}
{== TRzDBCheckBoxGroup Methods ==}
{================================}

constructor TRzDBCheckBoxGroup.Create( AOwner: TComponent );
begin
  inherited;
  Checked := False;
  ShowCheckBox := True;

  FValueCheck := STextTrue;
  FValueUncheck := STextFalse;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  {&RCI}
end;


destructor TRzDBCheckBoxGroup.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBCheckBoxGroup.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


function TRzDBCheckBoxGroup.GetFieldState: TCheckBoxState;
var
  Text: string;
begin
  if FDatalink.Field <> nil then
  begin
    if FDataLink.Field.IsNull then
    begin
      Result := cbUnchecked
    end
    else if FDataLink.Field.DataType = ftBoolean then
    begin
      if FDataLink.Field.AsBoolean then
        Result := cbChecked
      else
        Result := cbUnchecked;
    end
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


procedure TRzDBCheckBoxGroup.DataChange( Sender: TObject );
begin
  if GetFieldState = cbChecked then
    Checked := True
  else
    Checked := False;
end;


procedure TRzDBCheckBoxGroup.UpdateData( Sender: TObject );
var
  Pos: Integer;
  S: string;
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


function TRzDBCheckBoxGroup.ValueMatch( const ValueList, Value: string ): Boolean;
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


procedure TRzDBCheckBoxGroup.ChangeState;
begin
  {&RV}
  if FDataLink.Edit then
  begin
    inherited;
    FDataLink.Modified;
  end;
end;


function TRzDBCheckBoxGroup.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRzDBCheckBoxGroup.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;

function TRzDBCheckBoxGroup.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRzDBCheckBoxGroup.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;

function TRzDBCheckBoxGroup.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRzDBCheckBoxGroup.SetReadOnly( Value: Boolean );
begin
  inherited;
  FDataLink.ReadOnly := Value;
end;

function TRzDBCheckBoxGroup.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TRzDBCheckBoxGroup.KeyPress( var Key: Char );
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

procedure TRzDBCheckBoxGroup.SetValueCheck( const Value: string );
begin
  FValueCheck := Value;
  DataChange( Self );
end;

procedure TRzDBCheckBoxGroup.SetValueUncheck( const Value: string );
begin
  FValueUncheck := Value;
  DataChange( Self );
end;


procedure TRzDBCheckBoxGroup.CMExit( var Msg: TCMExit );
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;


procedure TRzDBCheckBoxGroup.CMGetDataLink( var Msg: TMessage );
begin
  Msg.Result := Integer( FDataLink );
end;


{&RUIF}
end.
