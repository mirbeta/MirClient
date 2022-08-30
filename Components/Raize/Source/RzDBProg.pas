{===============================================================================
  RzDBProg Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBProgressBar
    Data-Aware TRzProgressBar.  Control can be connected to 1) a DataField, 
    2) a DataField and a BaseValue, or 3) a DataField and a BaseField.


  Modification History
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * For RAD Studio 2010, surfaced Touch property and OnGesture event in the
      TRzDBProgressBar control.
  ------------------------------------------------------------------------------
  4.0    (23 Dec 2005)
    * Added new FrameControllerNotifications property to TRzDBProgessBar.
      The FrameControllerNotifications set property defines which
      TRzFrameController properties will be handled by the control.
      By default all TRzFrameController properties will be handled.
  ------------------------------------------------------------------------------
  3.0.9  (22 Sep 2003)
    * Surface FrameController property introduced in TRzProgressBar.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzProgressBar.
===============================================================================}

{$I RzComps.inc}

unit RzDBProg;

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
  ExtCtrls,
  Forms,
  Dialogs,
  Menus,
  StdCtrls,
  RzPrgres,
  DB,
  DBCtrls,
  RzCommon;

type
  EInvalidBaseValue = class( Exception );

  TRzDBProgressBar = class( TRzCustomProgressBar )
  private
    FAboutInfo: TRzAboutInfo;
    FBaseValue: Double;
    FDataLink: TFieldDataLink;
    FBaseDataLink: TFieldDataLink;

    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );
  protected
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation ); override;

    procedure SetBaseValue( Value: Double ); virtual;
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetBaseField: string; virtual;
    procedure SetBaseField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetPercent: Integer; virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Percent: Integer
      read GetPercent;
  published
    property About: TRzAboutInfo
      read FAboutInfo
      write FAboutInfo
      stored False;

    property BaseField: string
      read GetBaseField
      write SetBaseField;

    property BaseValue: Double
      read FBaseValue
      write SetBaseValue;

    property DataField: string
      read GetDataField
      write SetDataField;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

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
    property PopupMenu;
    property ShowHint;
    property ShowPercent;
    property ThemeAware;
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

{&RUAS}

resourcestring
  sRzInvalidBaseValue = 'Base Value cannot be zero';


{&RT}
{==============================}
{== TRzDBProgressBar Methods ==}
{==============================}

constructor TRzDBProgressBar.Create( AOwner: TComponent );
begin
  inherited;

  FBaseValue := 0;
  FDataLink := TFieldDataLink.Create;
  FDataLink.OnDataChange := DataChange;

  FBaseDataLink := TFieldDataLink.Create;
  FBaseDataLink.OnDataChange := DataChange;
  {&RCI}
end;


destructor TRzDBProgressBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FBaseDataLink.Free;
  FBaseDataLink := nil;
  inherited;
end;


procedure TRzDBProgressBar.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( FBaseDataLink <> nil ) and
     ( AComponent = DataSource ) then
    DataSource := nil;
end;


function TRzDBProgressBar.GetPercent: Integer;
begin
  Result := inherited Percent;
end;


procedure TRzDBProgressBar.SetBaseValue( Value: Double );
begin
  if Value = 0 then
    raise EInvalidBaseValue.Create( sRzInvalidBaseValue );

  if FBaseValue <> Value then
  begin
    FBaseValue := Value;
    DataChange( Self );
  end;
end;


function TRzDBProgressBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBProgressBar.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
  {&RV}
end;


function TRzDBProgressBar.GetBaseField: string;
begin
  Result := FBaseDataLink.FieldName;
end;


procedure TRzDBProgressBar.SetBaseField( const Value: string );
begin
  FBaseDataLink.FieldName := Value;
end;


function TRzDBProgressBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBProgressBar.SetDataSource( Value: TDataSource );
begin
  {&RV}
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    FBaseDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


{===============================================================================
  TRzDBProgressBar.DataChange
    This method gets called as a result of a number of different events:

    1. The underlying field value changes.  Occurs when changing the value
       of the column tied to this control and then move to a new column or a
       new record.
    2. The corresponding Dataset goes into Edit mode.
    3. The corresponding Dataset referenced by DataSource changes.
    4. The current cursor is scrolled to a new record in the table.
    5. The record is reset through a Cancel call.
    6. The DataField property changes to reference another column.
===============================================================================}

procedure TRzDBProgressBar.DataChange( Sender: TObject );
begin
  try
    if ( FDataLink.Field <> nil ) and ( FBaseDataLink.Field <> nil ) then
      inherited Percent := Round( ( FDataLink.Field.AsFloat / FBaseDataLink.Field.AsFloat ) * 100 )
    else if ( FDataLink.Field <> nil ) and
            ( FBaseDataLink.Field = nil ) and
            ( FBaseValue <> 0 ) then
      inherited Percent := Round( ( FDataLink.Field.AsFloat / FBaseValue ) * 100 )
    else if FDataLink.Field <> nil then
      inherited Percent := FDataLink.Field.AsInteger
    else
      inherited Percent := 0;
  except
    on EMathError do
      inherited Percent := 0;
  end;
end;

{&RUIF}
end.
