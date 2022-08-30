{===============================================================================
  RzDBLbl Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBLabel
    Data-Aware TRzLabel


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzLabel.
===============================================================================}

{$I RzComps.inc}

unit RzDBLbl;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Messages,
  Classes,
  Graphics,
  Controls,
  DBConsts,
  VDBConsts,
  Windows,
  Forms,
  Dialogs,
  StdCtrls,
  RzLabel,
  DB,
  DBCtrls,
  RzCommon;

type
  TRzDBLabel = class( TRzLabel )
  private
    FDataLink: TFieldDataLink;

    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );

    procedure CMGetDataLink( var Msg: TMessage ); message cm_GetDataLink;
  protected
    procedure Loaded; override;
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation ); override;
    procedure Paint; override;

    function GetFieldText: string;
    function GetLabelText: string; override;

    { Property Access Methods }
    procedure SetAutoSize( Value: Boolean ); override;
    function GetField: TField; virtual;
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Field: TField
      read GetField;
  published
    property DataField: string
      read GetDataField
      write SetDataField;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    { Inherited Properties & Events }
    property AutoSize default False;
    property Caption stored False;
  end;


implementation


{&RUAS}
{&RT}
{========================}
{== TRzDBLabel Methods ==}
{========================}

constructor TRzDBLabel.Create( AOwner: TComponent );
begin
  inherited;
  ControlStyle := ControlStyle + [ csReplicatable ];
  {&RCI}
  AutoSize := False;
  FDataLink := TFieldDataLink.Create;                        { Create DataLink }
  FDataLink.OnDataChange := DataChange;                 { Assign Event Handler }
end;


destructor TRzDBLabel.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBLabel.Loaded;
begin
  inherited;

  if csDesigning in ComponentState then
    DataChange( Self );
end;


procedure TRzDBLabel.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


procedure TRzDBLabel.Paint;
begin
  if Field <> nil then
    Alignment := Field.Alignment;

  inherited;
end;


procedure TRzDBLabel.SetAutoSize( Value: Boolean );
begin
  if AutoSize <> Value then
  begin
    if Value and FDataLink.DataSourceFixed then
      DatabaseError( SDataSourceFixed );
    inherited;
  end;
end;


function TRzDBLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

function TRzDBLabel.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRzDBLabel.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;


function TRzDBLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBLabel.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


{===============================================================================
  TRzDBLabel.DataChange
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

procedure TRzDBLabel.DataChange( Sender: TObject );
begin
  Caption := GetFieldText;
  {&RV}
end;


function TRzDBLabel.GetFieldText: string;
begin
  if FDataLink.Field <> nil then
    Result := FDataLink.Field.DisplayText
  else if csDesigning in ComponentState then
    Result := Name
  else
    Result := '';
end;


function TRzDBLabel.GetLabelText: string;
begin
  if csPaintCopy in ControlState then
    Result := GetFieldText
  else
    Result := Caption;
end;

procedure TRzDBLabel.CMGetDataLink( var Msg: TMessage );
begin
  Msg.Result := Integer( FDataLink );
end;


{&RUIF}
end.
