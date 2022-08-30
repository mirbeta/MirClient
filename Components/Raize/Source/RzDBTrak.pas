{===============================================================================
  RzDBTrak Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBTrackBar
    Data-Aware TRzTrackBar


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzTrackBar.
===============================================================================}

{$I RzComps.inc}

unit RzDBTrak;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  {&RF}
  Messages,
  Classes,
  Controls,
  RzTrkBar,
  DBCtrls,
  DB,
  RzCommon;

type
  TRzDBTrackBar = class;

  TRzTrackValueList = class( TStringList )
  private
    FTrackBar: TRzDBTrackBar;
  public
    function Add( const Value: string ): Integer; override;
    procedure Clear; override;
    procedure Delete( Index: Integer ); override;
    procedure Insert( Index: Integer; const Value: string ); override;
  end;

  TRzDBTrackBar = class( TRzTrackBar )
  private
    FDataLink: TFieldDataLink;
    FValue: string;
    FValues: TStrings;

    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );
    procedure UpdateData( Sender: TObject );

    { Message Handling Methods }
    procedure CMExit( var Msg: TCMExit ); message CM_EXIT;
  protected
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Event Dispatch Methods }
    procedure Change; override;
    procedure KeyPress( var Key: Char ); override;
    function CanInternalChange( NewPos: Integer ): Boolean; override;

    { Property Access Methods }
    function GetField: TField; virtual;
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); virtual;
    function GetPositionValue( Index: Integer ): string; virtual;
    function GetValue: string; virtual;
    procedure SetValue( const Value: string ); virtual;
    procedure SetValues( Value: TStrings ); virtual;

    { Property Declarations }
    property DataLink: TFieldDataLink
      read FDataLink;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Field: TField
      read GetField;

    property Value: string
      read GetValue
      write SetValue;
  published
    property DataField: string
      read GetDataField
      write SetDataField;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    { This property controls the ReadOnly State of the DataLink }
    property ReadOnly: Boolean
      read GetReadOnly
      write SetReadOnly
      default False;

    property Values: TStrings
      read FValues
      write SetValues;
  end;



implementation

uses
  {&RAS}
  SysUtils,
  Windows;


{===============================}
{== TRzTrackValueList Methods ==}
{===============================}


function TRzTrackValueList.Add( const Value: string ): Integer;
begin
  Result := inherited Add( Value );
  if FTrackBar <> nil then
  begin
    FTrackBar.Min := 0;
    FTrackBar.Max := Count - 1;
  end;
end;


procedure TRzTrackValueList.Clear;
begin
  inherited;
  if ( FTrackBar <> nil ) and
     not ( csDestroying in FTrackBar.ComponentState ) then
  begin
    FTrackBar.Min := 0;
    FTrackBar.Max := 1;
  end;
end;


procedure TRzTrackValueList.Delete( Index: Integer );
begin
  inherited;
  if FTrackBar <> nil then
  begin
    FTrackBar.Min := 0;
    FTrackBar.Max := Count - 1;
  end;
end;


procedure TRzTrackValueList.Insert( Index: Integer; const Value: string );
begin
  inherited;
  if FTrackBar <> nil then
  begin
    FTrackBar.Min := 0;
    FTrackBar.Max := Count - 1;
  end;
end;

{&RT}
{===========================}
{== TRzDBTrackBar Methods ==}
{===========================}

constructor TRzDBTrackBar.Create( AOwner: TComponent );
begin
  inherited;

  FDataLink := TFieldDataLink.Create;

  { To support the TField.FocusControl method, set the FDataLink.Control       }
  { property to point to the track bar.  The Control property requires a       }
  { TWinControl component.                                                     }
  FDataLink.Control := Self;
  {&RCI}
  FDataLink.OnDataChange := DataChange;                { Assign Event Handlers }
  FDataLink.OnUpdateData := UpdateData;

  FValue := '';
  FValues := TRzTrackValueList.Create;
  TRzTrackValueList( FValues ).FTrackBar := Self;
end;


destructor TRzDBTrackBar.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  FValues.Free;
  inherited;
end;


procedure TRzDBTrackBar.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


function TRzDBTrackBar.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBTrackBar.SetDataField( const Value: string );
begin
  FDataLink.FieldName := Value;
end;


function TRzDBTrackBar.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBTrackBar.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


function TRzDBTrackBar.GetField: TField;
begin
  Result := FDataLink.Field;
end;


function TRzDBTrackBar.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;


procedure TRzDBTrackBar.SetReadOnly( Value: Boolean );
begin
  FDataLink.ReadOnly := Value;
end;


function TRzDBTrackBar.GetPositionValue( Index: Integer ): string;
begin
  if ( Index < FValues.Count ) and ( FValues[ Index ] <> '' ) then
    Result := FValues[ Index ]
  else if ( Index >= Min ) and ( Index <= Max ) then
    Result := IntToStr( Index )
  else
    Result := '';
end;


function TRzDBTrackBar.GetValue: string;
begin
  Result := FValue;
end;


procedure TRzDBTrackBar.SetValue( const Value: string );
var
  I: Integer;
begin
  FValue := Value;
  if GetPositionValue( Position ) <> Value then
  begin
    for I := Min to Max do
    begin
      if GetPositionValue( I ) = Value then
      begin
        Position := I;
        Break;
      end;
    end;
  end;
end;


procedure TRzDBTrackBar.SetValues( Value: TStrings );
begin
  FValues.Assign( Value );
  Min := 0;
  Max := FValues.Count - 1;
  DataChange( Self );
end;


procedure TRzDBTrackBar.Change;
begin
  {&RV}
  Value := GetPositionValue( Position );
  if FDataLink.Editing then
    FDataLink.Modified;
  inherited;
end;


function TRzDBTrackBar.CanInternalChange( NewPos: Integer ): Boolean;
begin
  if csDesigning in ComponentState then
  begin
    Result := False;
    Exit;
  end;
  
  if ReadOnly then
    SysUtils.Beep
  else
    FDataLink.Edit;                  { Prevent change if FDataLink is ReadOnly }
  Result := not ReadOnly;
end;


procedure TRzDBTrackBar.DataChange( Sender: TObject );
begin
  if FDataLink.Field <> nil then
    Value := FDataLink.Field.Text
  else
    Value := EmptyStr;
end;


procedure TRzDBTrackBar.UpdateData(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    FDataLink.Field.Text := Value;
end;


procedure TRzDBTrackBar.KeyPress( var Key: Char );
begin
  inherited;
  if Key = #27 then                                    { If Escape key pressed }
    FDataLink.Reset;
end;


procedure TRzDBTrackBar.CMExit( var Msg: TCMExit );
begin
  try             { Attempt to Update the record if focus leaves the track bar }
    FDataLink.UpdateRecord;
  except
    SetFocus;                  { Keep the focus on the control if Update fails }
    raise;                                             { Reraise the exception }
  end;

  inherited;
end;

{&RUIF}
end.
