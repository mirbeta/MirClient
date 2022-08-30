{===============================================================================
  RzDBList Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBListBox
    Data-Aware TRzListBox


  Modification History
  ------------------------------------------------------------------------------
  6.1.11 (08 Apr 2015)
    * Surface the Sorted property in TRzDBListBox.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Surfaced OnMouseWheel event in TRzDBListBox.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new FrameControllerNotifications property to TRzDBListBox.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzCustomListBox.
===============================================================================}

{$I RzComps.inc}

unit RzDBList;

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
  StdCtrls,
  Menus,
  RzLstBox,
  DBCtrls,
  DB,
  RzCommon;

type
  TRzDBListBox = class( TRzCustomListBox )
  private
    FAboutInfo: TRzAboutInfo;
    FJumpingToEdit: Boolean;
    FDataLink: TFieldDataLink;

    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );
    procedure UpdateData( Sender: TObject );

    { Message Handling Methods }
    procedure WMLButtonDown( var Msg: TWMLButtonDown ); message wm_LButtonDown;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
  protected
    { Event Dispatch Methods }
    procedure Click; override;
    procedure KeyDown( var Key: Word; Shift: TShiftState); override;
    procedure KeyPress( var Key: Char ); override;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Property Access Methods }
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetField: TField; virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); virtual;
    procedure SetItems( Value: TStrings ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

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

    property Items
      write SetItems;

    { Inherited Properties & Events }
    property Align;
    property Anchors;
    property BeepOnInvalidKey;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DisabledColor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FocusColor;
    property FrameColor;
    property FrameControllerNotifications;
    property FrameController;
    property FrameHotColor;
    property FrameHotTrack;
    property FrameHotStyle;
    property FrameSides;
    property FrameStyle;
    property FrameVisible;
    property FramingPreference;
    property GroupColor;
    property GroupFont;
    property GroupPrefix;
    property HorzScrollBar;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowGroups;
    property ShowHint;
    property ShowItemHints;
    property Sorted;
    property Style;
    property TabOnEnter;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property UseGradients;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMatch;
    property OnMeasureItem;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnStartDrag;
  end;


implementation

{&RUAS}

{&RT}
{==========================}
{== TRzDBListBox Methods ==}
{==========================}

constructor TRzDBListBox.Create( AOwner: TComponent );
begin
  inherited;
  FJumpingToEdit := False;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  {&RCI}
end;


destructor TRzDBListBox.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBListBox.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


procedure TRzDBListBox.DataChange( Sender: TObject );
begin
  if not FJumpingToEdit then
  begin
    if FDataLink.Field <> nil then
      ItemIndex := Items.IndexOf( FDataLink.Field.Text )
    else
      ItemIndex := -1;
  end;
end;


procedure TRzDBListBox.UpdateData( Sender: TObject );
begin
  if ItemIndex >= 0 then
    FDataLink.Field.Text := Items[ ItemIndex ]
  else
    FDataLink.Field.Text := '';
end;


procedure TRzDBListBox.Click;
begin
  {&RV}
  if FDataLink.Edit then
  begin
    inherited;
    FDataLink.Modified;
  end;
end;


function TRzDBListBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBListBox.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


function TRzDBListBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBListBox.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;


function TRzDBListBox.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;


procedure TRzDBListBox.SetReadOnly( Value: Boolean );
begin
  FDataLink.ReadOnly := Value;
end;


function TRzDBListBox.GetField: TField;
begin
  Result := FDataLink.Field;
end;


procedure TRzDBListBox.KeyDown( var Key: Word; Shift: TShiftState );
begin
  inherited;
  if Key in [ vk_Prior, vk_Next, vk_End, vk_Home, vk_Left, vk_Up, vk_Right, vk_Down ] then
    if not FDataLink.Edit then
      Key := 0;
end;


procedure TRzDBListBox.KeyPress( var Key: Char );
begin
  inherited;
  case Key of
    #32..#255:
    begin
      FJumpingToEdit := True;
      if not FDataLink.Edit then
        Key := #0;
      FJumpingToEdit := False;
    end;

    #27:
      FDataLink.Reset;
  end;
end;


procedure TRzDBListBox.WMLButtonDown( var Msg: TWMLButtonDown );
begin
  if FDataLink.Edit then
    inherited
  else
  begin
    SetFocus;
    with Msg do
      MouseDown( mbLeft, KeysToShiftState( Keys ), XPos, YPos );
  end;
end;


procedure TRzDBListBox.CMExit( var Msg: TCMExit );
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;


procedure TRzDBListBox.SetItems( Value: TStrings );
begin
  Items.Assign( Value );
  DataChange( Self );
end;

{&RUIF}
end.

