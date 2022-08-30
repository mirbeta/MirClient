{===============================================================================
  RzDBStat Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Components
  ------------------------------------------------------------------------------
  TRzDBStatusPane
    Data-Aware TRzStatusPane

  TRzDBStateStatus
    Displays the state of an associated TDataSource component.


  Modification History
  ------------------------------------------------------------------------------
  3.0.8  (29 Aug 2003)
    * TRzDBStatusPane now descends from the new TRzCustomFieldStatus component.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Inherits changes from TRzCustomStatusPane.
    * Published inherited AutoSize property.
    * Published inherited Blinking property.
    * Published inherited BlinkColor property.
    * Published inherited BlinkIntervalOff property.
    * Published inherited BlinkIntervalOn property.
===============================================================================}

{$I RzComps.inc}

unit RzDBStat;

interface

uses
  {&RF}
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Messages,
  Windows,
  Classes,
  Graphics,
  Controls,
  RzStatus,
  DB,
  DBCtrls,
  RzCommon;

type
  TRzDBStatusPane = class( TRzCustomFieldStatus )
  private
    { Internal Event Handlers }
    procedure DataChange( Sender: TObject );
  protected
    FDataLink: TFieldDataLink;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;

    { Property Access Methods }
    function GetDataField: string; virtual;
    procedure SetDataField( const Value: string ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property DataField: string
      read GetDataField
      write SetDataField;

    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    { Inherited Properties }
    property Alignment;          { No Need to surface Caption property b/c the }
    property AutoSize;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property CaptionOffset;        { DataSource will supply the display string }
    property ShowHint default True;
  end;


  TRzDBStateStatus = class;                          { Forward class reference }

  TRzDBStateDataLink = class( TDataLink )
  private
    FStatusControl: TRzDBStateStatus;
  protected
    procedure EditingChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create( AStatusControl: TRzDBStateStatus );
    destructor Destroy; override;
  end;


  TRzStateCaptions = class( TPersistent )
  private
    FCaptions: array[ 0..6 ] of string;                 { dsInactive..dsFilter }
    FStatusControl: TRzDBStateStatus;
  protected
    { Property Access Methods }
    function GetStateCaption( Index: Integer ): string; virtual;
    procedure SetStateCaption( Index: Integer; const Value: string ); virtual;
  public
    constructor Create( AStatusControl: TRzDBStateStatus );
    destructor Destroy; override;
    { Property Declarations }
    property StausControl: TRzDBStateStatus
      read FStatusControl;

    property Captions[ Index: Integer ]: string
      read GetStateCaption
      write SetStateCaption;
  published
    { Property Declarations }
    property Inactive: string
      index 0
      read GetStateCaption
      write SetStateCaption;

    property Browse: string
      index 1
      read GetStateCaption
      write SetStateCaption;

    property Edit: string
      index 2
      read GetStateCaption
      write SetStateCaption;

    property Insert: string
      index 3
      read GetStateCaption
      write SetStateCaption;

    property Search: string
      index 4
      read GetStateCaption
      write SetStateCaption;

    property Calculate: string
      index 5
      read GetStateCaption
      write SetStateCaption;

    property Filter: string
      index 6
      read GetStateCaption
      write SetStateCaption;
  end;


  TRzDBStateStatus = class( TRzCustomGlyphStatus )
  private
    FBitmaps: TBitmap;
    FStateCaptions: TRzStateCaptions;
    FStoreStateCaptions: Boolean;
    FShowCaption: Boolean;
  protected
    FGlyphHeight: Integer;
    FGlyphWidth: Integer;
    FDataLink: TRzDBStateDataLink;
    procedure Notification( AComponent: TComponent; Operation: TOperation ); override;
    procedure UpdateState; virtual;

    procedure ExtractGlyph( Index: Integer; Bitmap, Source: TBitmap; W, H: Integer ); virtual;
    procedure SelectGlyph( Glyph: TBitmap ); virtual;

    { Property Access Methods }
    procedure SetStateCaptions( Value: TRzStateCaptions ); virtual;
    function GetDataSource: TDataSource; virtual;
    procedure SetDataSource( Value: TDataSource ); virtual;
    function GetDatasetState: TDatasetState; virtual;
    procedure SetShowCaption( Value: Boolean ); virtual;
    procedure SetShowGlyph( Value: Boolean ); override;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property DatasetState: TDatasetState
      read GetDatasetState;
  published
    property DataSource: TDataSource
      read GetDataSource
      write SetDataSource;

    property ShowCaption: Boolean
      read FShowCaption
      write SetShowCaption
      default True;

    property StateCaptions: TRzStateCaptions
      read FStateCaptions
      write SetStateCaptions
      stored FStoreStateCaptions;

    { Inherited Properties }
    property Alignment;          { No Need to surface Caption property b/c the }
    property AutoSize;
    property Blinking;
    property BlinkColor;
    property BlinkIntervalOff;
    property BlinkIntervalOn;
    property CaptionOffset;        { DataSource will supply the display string }
    property GlyphAlignment;
    property GlyphOffset default 5;
    property ShowGlyph;
  end;



implementation

// Link in glyphs for TRzDBStateStatus
{$R RzDBStat.res}

uses
  {&RAS}
  Themes,
  SysUtils;

resourcestring
  sRzInactive    = 'Inactive';
  sRzBrowse      = 'Browse';
  sRzEdit        = 'Edit';
  sRzInsert      = 'Insert';
  sRzSetKey      = 'Search';
  sRzCalcFields  = 'Calculate';
  sRzFilter      = 'Filter';

{&RT}
{=============================}
{== TRzDBStatusPane Methods ==}
{=============================}

constructor TRzDBStatusPane.Create( AOwner: TComponent );
begin
  inherited;
  FDataLink := TFieldDataLink.Create;                        { Create DataLink }
  FDataLink.OnDataChange := DataChange;                 { Assign Event Handler }
  ShowHint := True;
  {&RCI}
end;


destructor TRzDBStatusPane.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBStatusPane.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;

  if ( Operation = opRemove ) and ( FDataLink <> nil ) and ( AComponent = DataSource ) then
    DataSource := nil;
end;


function TRzDBStatusPane.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;


procedure TRzDBStatusPane.SetDataField( const Value: string );
begin
  {&RV}
  FDataLink.FieldName := Value;
  if not ( csLoading in ComponentState ) and ( FDataLink.Field <> nil ) then
    Hint := FDataLink.Field.DisplayLabel;
end;


function TRzDBStatusPane.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBStatusPane.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
end;


{-----------------------------------------------------------------------------------------------------------------------
  TRzDBStatusPane.DataChange

  This method gets called as a result of a number of different events:

  1. The underlying field value changes.  Occurs when changing the value of the column tied to this control and then
     move to a new column or a new record.
  2. The corresponding Dataset goes into Edit mode.
  3. The corresponding Dataset referenced by DataSource changes.
  4. The current cursor is scrolled to a new record in the table.
  5. The record is reset through a Cancel call.
  6. The DataField property changes to reference another column.
-----------------------------------------------------------------------------------------------------------------------}

procedure TRzDBStatusPane.DataChange( Sender: TObject );
begin
  if FDataLink.Field = nil then
    Caption := ''
  else
  begin
    Alignment := FDataLink.Field.Alignment;
    Caption := FDataLink.Field.DisplayText;
  end;
end;



{================================}
{== TRzDBStateDataLink Methods ==}
{================================}

constructor TRzDBStateDataLink.Create( AStatusControl: TRzDBStateStatus );
begin
  inherited Create;
  FStatusControl := AStatusControl;
end;


destructor TRzDBStateDataLink.Destroy;
begin
  FStatusControl := nil;
  inherited;
end;


procedure TRzDBStateDataLink.EditingChanged;
begin
  if FStatusControl <> nil then
    FStatusControl.UpdateState;          { To reflect dsEdit, dsInsert changes }
end;


procedure TRzDBStateDataLink.ActiveChanged;
begin
  if FStatusControl <> nil then
    FStatusControl.UpdateState;          { To reflect dsInactive state changes }
end;



{==============================}
{== TRzStateCaptions Methods ==}
{==============================}

constructor TRzStateCaptions.Create( AStatusControl: TRzDBStateStatus );
begin
  inherited Create;
  FStatusControl := AStatusControl;

  FCaptions[ 0 ] := sRzInactive;
  FCaptions[ 1 ] := sRzBrowse;
  FCaptions[ 2 ] := sRzEdit;
  FCaptions[ 3 ] := sRzInsert;
  FCaptions[ 4 ] := sRzSetKey;
  FCaptions[ 5 ] := sRzCalcFields;
  FCaptions[ 6 ] := sRzFilter;
end;


destructor TRzStateCaptions.Destroy;
begin
  FStatusControl := nil;
  inherited;
end;

function TRzStateCaptions.GetStateCaption( Index: Integer ): string;
begin
  if Index <= 6 then
    Result := FCaptions[ Index ]
  else
    Result := '';
end;

procedure TRzStateCaptions.SetStateCaption( Index: Integer; const Value: string );
begin
  if ( Index <= 6 ) and ( FCaptions[ Index ] <> Value ) then
  begin
    FCaptions[ Index ] := Value;
    FStatusControl.UpdateState;
    FStatusControl.FStoreStateCaptions := True;
  end;
end;


var
  DBStatesBmp: TBitmap;


{==============================}
{== TRzDBStateStatus Methods ==}
{==============================}

constructor TRzDBStateStatus.Create( AOwner: TComponent );
begin
  inherited;
  FDataLink := TRzDBStateDataLink.Create( Self );            { Create DataLink }

  FBitmaps := TBitmap.Create;
  FBitmaps.Assign( DBStatesBmp );
  FGlyphWidth := 12;
  FGlyphHeight := 12;

  GlyphOffset := 5;
  FShowCaption := True;

  FStateCaptions := TRzStateCaptions.Create( Self );
  FStoreStateCaptions := False;
  {&RCI}
end;


destructor TRzDBStateStatus.Destroy;
begin
  FStateCaptions.Free;
  FBitmaps.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited;
end;


procedure TRzDBStateStatus.Notification( AComponent: TComponent; Operation: TOperation );
begin
  inherited;
  if ( Operation = opRemove ) and
     ( FDataLink <> nil ) and
     ( AComponent = DataSource ) then
    DataSource := nil;
end;


procedure TRzDBStateStatus.ExtractGlyph( Index: Integer; Bitmap, Source: TBitmap; W, H: Integer );
var
  DestRct, SrcRct: TRect;
begin
  DestRct := Rect( 0, 0, W, H );

  Bitmap.Width := W;
  Bitmap.Height := H;
  SrcRct := Rect( Index * W, 0, (Index + 1 ) * W, H );

  Bitmap.Canvas.Brush.Color := FillColor;
  Bitmap.Canvas.BrushCopy( DestRct, Source, SrcRct, Source.TransparentColor );

end;


procedure TRzDBStateStatus.SelectGlyph( Glyph: TBitmap );
var
  DestBmp: TBitmap;
begin
  DestBmp := TBitmap.Create;
  try
    ExtractGlyph( Ord( DatasetState ), DestBmp, FBitmaps, FGlyphWidth, FGlyphHeight );
    Glyph.Assign( DestBmp );
  finally
    DestBmp.Free;
  end;
end;


procedure TRzDBStateStatus.UpdateState;
begin
  if FShowCaption and ( Ord( DatasetState ) <= 6 ) then
    Caption := FStateCaptions.Captions[ Ord( DatasetState ) ]
  else
    Caption := '';

  if ShowGlyph then
  begin
    GlyphOffset := 5;
    if Ord( DatasetState ) <= 6 then
      SelectGlyph( Glyph )
    else
      Glyph := nil;
  end
  else
  begin
    GlyphOffset := 0;
    Glyph := nil;
  end;
end;



function TRzDBStateStatus.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;


procedure TRzDBStateStatus.SetDataSource( Value: TDataSource );
begin
  if not ( FDataLink.DataSourceFixed and ( csLoading in ComponentState ) ) then
  begin
    FDataLink.DataSource := Value;
    UpdateState;
    if Value <> nil then
      Value.FreeNotification( Self );
  end;
  {&RV}
end;


function TRzDBStateStatus.GetDatasetState: TDatasetState;
begin
  if FDataLink.DataSource <> nil then
    Result := FDataLink.DataSource.State
  else
    Result := dsInactive;
end;


procedure TRzDBStateStatus.SetShowCaption( Value: Boolean );
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    UpdateState;
  end;
end;


procedure TRzDBStateStatus.SetShowGlyph( Value: Boolean );
begin
  inherited;
  UpdateState;
end;


procedure TRzDBStateStatus.SetStateCaptions( Value: TRzStateCaptions );
begin
  FStateCaptions.Assign( Value );
end;




procedure FreeBitmaps; far;
begin
  DBStatesBmp.Free;
end;

initialization
  DBStatesBmp := TBitmap.Create;
  DBStatesBmp.LoadFromResourceName( HInstance, 'RZDBSTAT_DBSTATEBMPS' );
  {&RUI}

finalization
  FreeBitmaps;

end.
