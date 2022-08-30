{===============================================================================
  RzDBDTP Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBDateTimePicker   
    Data-Aware TRzDateTimePicker


  Modification History
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Deprecated. This component has been replaced with the new
      TRzDBDateTimeEdit.
===============================================================================}

{$I RzComps.inc}
{$WARN SYMBOL_DEPRECATED OFF}

unit RzDBDTP;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  {&RF}
  SysUtils,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  RzDTP,
  RzCommon,
  DB,
  DBCtrls,
  ExtCtrls;

type
  {===========================================}
  {== TRzDBDateTimePicker Class Declaration ==}
  {===========================================}

  TRzDBDateTimePicker = class( TRzDateTimePicker )
  private
    FDataLink: TFieldDataLink;
    FPaintControl: TRzDateTimePicker;

    { Internal Event Handlers }
    procedure ActiveChangeHandler( Sender: TObject );
    procedure DataChangeHandler( Sender: TObject );
    procedure UpdateDataHandler( Sender: TObject );

    { Message Handling Methods }
    procedure WMPaint( var Msg: TWMPaint ); message wm_Paint;
    procedure WMLButtonDown( var Msg: TWMLButtonDown ); message wm_LButtonDown;
    procedure WMChar( var Msg: TWMChar ); message wm_Char;
    procedure CNNotify( var Msg: TWMNotify); message cn_Notify;
    procedure CMExit( var Msg: TCMExit ); message cm_Exit;
    procedure CMGetDataLink( var Msg: TMessage ); message cm_GetDataLink;
  protected
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation ); override;
    procedure CheckFieldType( const Value: string ); virtual;

    procedure DataChange; virtual;
    procedure UpdateData; virtual;
    procedure ActiveChange; virtual;

    { Event Dispatch Methods }
    procedure Change; override;

    procedure KeyPress( var Key: Char ); override;

    { Property Access Methods }
    function GetField: TField; virtual;
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetReadOnly: Boolean; virtual;
    procedure SetReadOnly( Value: Boolean ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function UpdateAction( Action: TBasicAction ): Boolean; override;

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
  end; // removed deprecated here b/c stopped working in RS2009


implementation

uses
  {&RAS}
  ComCtrls,
  CommCtrl;

{&RT}
{=================================}
{== TRzDBDateTimePicker Methods ==}
{=================================}

constructor TRzDBDateTimePicker.Create( AOwner: TComponent );
begin
  inherited;

  ControlStyle := ControlStyle + [ csReplicatable ];

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChangeHandler;
  FDataLink.OnDataChange := DataChangeHandler;
  FDataLink.OnUpdateData := UpdateDataHandler;
  {&RCI}


  FPaintControl := TRzDateTimePicker.Create( Self );
  FPaintControl.Parent := Self;
  FPaintControl.Visible := False;
  FPaintControl.ControlStyle := FPaintControl.ControlStyle + [ csReplicatable ];
end;


destructor TRzDBDateTimePicker.Destroy;
begin
  FPaintControl.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBDateTimePicker.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


procedure TRzDBDateTimePicker.CheckFieldType( const Value: string );
var
  FieldType: TFieldType;
begin
  if ( Value <> '' ) and
     ( FDataLink <> nil ) and
     ( FDataLink.Dataset <> nil ) and
     ( FDataLink.Dataset.Active ) then
  begin
    FieldType := FDataLink.Dataset.FieldByName( Value ).DataType;

    if FieldType in [ ftDate, ftTime, ftDateTime ] then
    begin
      if FieldType = ftDate then
        Kind := dtkDate
      else if FieldType = ftTime then
        Kind := dtkTime;
      { Otherwise, don't change Kind property--both settings will work }
    end
    else
    begin
      raise EInvalidFieldType.Create( 'RkDBDateTimePicker.DataField can only ' +
                                      'be connected to columns of type ' +
                                      'Date, Time, or DateTime' );
    end;
  end;
end;


procedure TRzDBDateTimePicker.ActiveChangeHandler( Sender: TObject );
begin
  ActiveChange;
end;

procedure TRzDBDateTimePicker.ActiveChange;
begin
  { If the Dataset is becoming Active, then check to make sure the
    field type of the DataField property is a valid type. }

  if ( FDataLink <> nil ) and FDataLink.Active then
    CheckFieldType( DataField );
end;


procedure TRzDBDateTimePicker.DataChangeHandler( Sender: TObject );
begin
  DataChange;
end;



procedure TRzDBDateTimePicker.DataChange;
begin
  if FDataLink.Field <> nil then
  begin
    if ShowCheckBox then
    begin
      if FDataLink.Field.IsNull then
      begin
        DateTime := Now;
        // Make sure the Checked assignment occurs after the DateTime
        // value is assigned, setting the DateTime value sets Checked := True
        Checked := False;
      end
      else
      begin
        DateTime := FDataLink.Field.AsDateTime;
        Checked := True;
      end;
    end
    else
      DateTime := FDataLink.Field.AsDateTime
  end
  else
    DateTime := Now;
end;


procedure TRzDBDateTimePicker.UpdateDataHandler( Sender: TObject );
begin
  UpdateData;
end;


procedure TRzDBDateTimePicker.UpdateData;
begin
  if ShowCheckBox then
  begin
    if Checked then
      FDataLink.Field.AsDateTime := DateTime
    else
      FDataLink.Field.Clear;
  end
  else
    FDataLink.Field.AsDateTime := DateTime;
end;



function TRzDBDateTimePicker.GetField: TField;
begin
  Result := FDataLink.Field;
end;


function TRzDBDateTimePicker.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TRzDBDateTimePicker.SetDataField( const Value: string );
begin
  CheckFieldType( Value );
  FDataLink.FieldName := Value;
end;


function TRzDBDateTimePicker.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TRzDBDateTimePicker.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


function TRzDBDateTimePicker.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

procedure TRzDBDateTimePicker.SetReadOnly( Value: Boolean );
begin
  FDataLink.ReadOnly := Value;
end;



procedure TRzDBDateTimePicker.Change;
begin
  FDataLink.Edit;
  inherited;
  FDataLink.Modified;
end;


procedure TRzDBDateTimePicker.KeyPress( var Key: Char );
begin
  inherited;
  case Key of
    #27:
    begin
      FDataLink.Reset;
    end;
  end;
end;


procedure TRzDBDateTimePicker.WMPaint( var Msg: TWMPaint );
begin
  if csPaintCopy in ControlState then
  begin
    FPaintControl.Kind := Kind;
    FPaintControl.ShowCheckBox := ShowCheckBox;
    if Field <> nil then
    begin
      if Field.IsNull then
      begin
        FPaintControl.DateTime := Now;
        FPaintControl.Checked := False;
      end
      else
      begin
        FPaintControl.DateTime := Field.AsDateTime;
        FPaintControl.Checked := True;
      end;
    end;
    FPaintControl.Color := Color;
    FPaintControl.FlatButtons := FlatButtons;
    FPaintControl.FrameVisible := FrameVisible;
    if FrameVisible then
    begin
      FPaintControl.FrameColor := FrameColor;
      FPaintControl.FrameHotTrack := FrameHotTrack;
      FPaintControl.FrameHotStyle := FrameHotStyle;
      FPaintControl.FrameSides := FrameSides;
      FPaintControl.FrameStyle := FrameStyle;
    end;

    FPaintControl.SetBounds( BoundsRect.Left, BoundsRect.Top,
                             BoundsRect.Right - BoundsRect.Left,
                             BoundsRect.Bottom - BoundsRect.Top );

    SendMessage( FPaintControl.Handle, wm_Paint, WParam( Msg.DC ), 0 );
    FPaintControl.PaintTo( Msg.DC, -2, -2 );
  end
  else
  begin
    FPaintControl.SetBounds( 0, 0, 0, 0 );
    inherited;
  end;
end; {= TRzDBDateTimePicker.WMPaint =}


procedure TRzDBDateTimePicker.WMLButtonDown( var Msg: TWMLButtonDown );
begin
  if not ReadOnly then
    inherited;
end;


procedure TRzDBDateTimePicker.WMChar( var Msg: TWMChar );
begin
  { This method is called when the user uses the keyboard to enter a date/time }
  if ReadOnly or not FDataLink.CanModify then
    SysUtils.Beep
  else if FDataLink.Edit then
    inherited;
end;


procedure TRzDBDateTimePicker.CNNotify( var Msg: TWMNotify );
begin
  with Msg do
  begin
    case NMHdr^.Code of
      dtn_DateTimeChange:
      begin
        if not ReadOnly and FDataLink.CanModify and FDataLink.Edit then
        begin
          inherited;
          FDataLink.Modified;
        end;
      end;
    else
      inherited;
    end;
  end;
end;

procedure TRzDBDateTimePicker.CMExit( var Msg: TCMExit );
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  inherited;
end;


procedure TRzDBDateTimePicker.CMGetDataLink( var Msg: TMessage );
begin
  Msg.Result := Integer( FDataLink );
end;


function TRzDBDateTimePicker.UpdateAction( Action: TBasicAction ): Boolean;
begin
  Result := inherited UpdateAction( Action ) or ( FDataLink <> nil ) and FDataLink.UpdateAction( Action );
end;


{&RUIF}
end.

