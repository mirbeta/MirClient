{===============================================================================
  RzIntLst Unit

  Raize Components - Component Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Classes
  ------------------------------------------------------------------------------
  TRzIntegerList
    Class to manage a dynamic list of integer values.


  Modification History
  ------------------------------------------------------------------------------
  3.0.13 (15 May 2005)
    * Fixed problem where TRzIntegerList was not recognizing the dupIgnore
      setting for the Duplicates property.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Fixed problem where Sorted property could not be turned off once it was
      turned on.
===============================================================================}

{$I RzComps.inc}

unit RzIntLst;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Classes;

type
  EOutOfRange   = class( EListError );

  TRzIntegerList = class( TPersistent )
  private
    FList: TList;
    FDuplicates: TDuplicates;
    FMin: Longint;
    FMax: Longint;
    FSorted: Boolean;
    procedure ReadMin( Reader: TReader );
    procedure WriteMin( Writer: TWriter );
    procedure ReadMax( Reader: TReader );
    procedure WriteMax( Writer: TWriter );
    procedure ReadIntegers( Reader: TReader );
    procedure WriteIntegers( Writer: TWriter );
    procedure SetSorted( Value: Boolean );
    procedure QuickSort( L, R: Integer );
  protected
    procedure DefineProperties( Filer: TFiler ); override;
    function Find( N: Longint; var Index: Integer ): Boolean; virtual;
    function GetCount: Integer;
    function GetItem( Index: Integer ): Longint;
    procedure SetItem( Index: Integer; Value: Longint ); virtual;
    procedure SetMin( Value: Longint );
    procedure SetMax( Value: Longint );
    procedure Sort; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Add( Value: Longint ): Integer; virtual;
    procedure AddIntegers( List: TRzIntegerList ); virtual;
    procedure Assign( Source: TPersistent ); override;
    procedure Clear; virtual;
    procedure Delete( Index: Integer ); virtual;
    function Equals( List: TRzIntegerList ): Boolean; reintroduce;
    procedure Exchange( Index1, Index2: Integer); virtual;
    function IndexOf( N: Longint ): Integer; virtual;
    procedure Insert( Index: Integer; Value: Longint ); virtual;
    procedure InsertNumber( Index: Integer; Value: Longint ); virtual;
    procedure Move( CurIndex, NewIndex: Integer ); virtual;

    property Duplicates: TDuplicates
      read FDuplicates
      write FDuplicates;

    property Count: Integer
      read GetCount;

    property Items[ Index: Integer ]: Longint
      read GetItem
      write SetItem; default;

    property Min: Longint
      read FMin
      write SetMin;

    property Max: Longint
      read FMax
      write SetMax;

    property Sorted: Boolean
      read FSorted
      write SetSorted;
  end;



implementation

uses
  RTLConsts,
  Consts,
  Windows;



{============================}
{== TRzIntegerList Methods ==}
{============================}

constructor TRzIntegerList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FSorted := False;
end;


destructor TRzIntegerList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;


procedure TRzIntegerList.Assign( Source: TPersistent );
begin
  if Source is TRzIntegerList then
  begin
    Clear;
    AddIntegers( TRzIntegerList( Source ) );
  end
  else
    inherited;
end;


procedure TRzIntegerList.DefineProperties( Filer: TFiler );

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      // If form inheritance is being used, then only store integers
      // if they differ from the ancestor property list.
      if Filer.Ancestor is TRzIntegerList then
        Result := not Equals( TRzIntegerList( Filer.Ancestor ) );
    end
    else
      Result := Count > 0;
  end;

begin
  inherited;
  
  Filer.DefineProperty( 'Min', ReadMin, WriteMin, FMin <> 0 );
  Filer.DefineProperty( 'Max', ReadMax, WriteMax, FMax <> 0 );
  Filer.DefineProperty( 'Integers', ReadIntegers, WriteIntegers, DoWrite );
end;


procedure TRzIntegerList.ReadMin( Reader: TReader );
begin
  FMin := Reader.ReadInteger;
end;


procedure TRzIntegerList.WriteMin( Writer: TWriter );
begin
  Writer.WriteInteger( FMin );
end;


procedure TRzIntegerList.ReadMax( Reader: TReader );
begin
  FMax := Reader.ReadInteger;
end;


procedure TRzIntegerList.WriteMax( Writer: TWriter );
begin
  Writer.WriteInteger( FMax );
end;


procedure TRzIntegerList.ReadIntegers( Reader: TReader );
begin
  Reader.ReadListBegin;                     { Read in the Start of List Marker }
  Clear;                                                  { Clear Current List }
  while not Reader.EndOfList do
    Add( Reader.ReadInteger );                   { Add Stored Integers to List }
  Reader.ReadListEnd;                         { Read in the End of List Marker }
end;


procedure TRzIntegerList.WriteIntegers( Writer: TWriter );
var
  I: Integer;
begin
  Writer.WriteListBegin;           { Be sure to Write the Start of List Marker }
  for I := 0 to Count - 1 do
    Writer.WriteInteger( GetItem( I ) );        { Write All Integers to Writer }
  Writer.WriteListEnd;                          { Write the End of List Marker }
end;


procedure TRzIntegerList.SetSorted( Value: Boolean );
begin
  if FSorted <> Value then
  begin
    if Value then
      Sort;
    FSorted := Value;
  end;
end;


function TRzIntegerList.GetCount: Integer;
begin
  Result := FList.Count;
end;


function TRzIntegerList.GetItem( Index: Integer ): Longint;
begin
  Result := PLongint( FList.Items[ Index ] )^;
end;


procedure TRzIntegerList.SetItem( Index: Integer; Value: Longint );
begin
  if ( FMin <> FMax ) and ( ( Value < FMin ) or ( Value > FMax ) ) then
    raise EOutOfRange.CreateFmt( 'Value must be within %d..%d', [ FMin, FMax ]);

  PLongint( FList.Items[ Index ] )^ := Value;
end;


procedure TRzIntegerList.SetMin( Value: Longint );
var
  I: Integer;
begin
  if Value <> FMin then
  begin
    for I := 0 to Count - 1 do
    begin
      if GetItem( I ) < Value then
        raise EOutOfRange.CreateFmt( 'Unable to set new minimum value.'#13 +
                                     'List contains values below %d',[ Value ]);
    end;
    FMin := Value;
    if FMin > FMax then
      FMax := FMin;
  end;
end; {= TRzIntegerList.SetMin =}


procedure TRzIntegerList.SetMax( Value: Longint );
var
  I: Integer;
begin
  if Value <> FMax then
  begin
    for I := 0 to Count - 1 do
    begin
      if GetItem( I ) > Value then
        raise EOutOfRange.CreateFmt( 'Unable to set new maximum value.'#13 +
                                     'List contains values above %d',[ Value ]);
    end;
    FMax := Value;
    if FMax < FMin then
      FMin := FMax;
  end;
end; {= TRzIntegerList.SetMax =}


procedure TRzIntegerList.AddIntegers( List: TRzIntegerList );
var
  I: Integer;
begin
  for I := 0 to Pred( List.Count ) do
    Add( List[ I ] );
end;


function TRzIntegerList.Add( Value: Longint ): Integer;
begin
  if not Sorted then
    Result := Count
  else if Find( Value, Result ) then
  begin
    case Duplicates of
      dupIgnore:
        Exit;

      dupError:
        raise EListError.Create( 'Duplicates not allowed' );
    end;
  end;

  InsertNumber( Result, Value );
end;


procedure TRzIntegerList.Clear;
var
  I: Integer;
begin
  for I := 0 to Pred( FList.Count ) do
    Dispose( PLongint( FList.Items[ I ] ) );
  FList.Clear;
end;


procedure TRzIntegerList.Delete( Index: Integer );
begin
  Dispose( PLongint( FList.Items[ Index ] ) );
  FList.Delete( Index );
end;


function TRzIntegerList.Equals( List: TRzIntegerList ): Boolean;
var
  I, Count: Integer;
begin
  Count := GetCount;
  if Count <> List.GetCount then
    Result := False
  else
  begin
    I := 0;
    while ( I < Count ) and ( GetItem( I ) = List.GetItem( I ) ) do
      Inc( I );
    Result := I = Count;
  end;
end; {= TRzIntegerList.Equals =}


procedure TRzIntegerList.Exchange( Index1, Index2: Integer );
begin
  FList.Exchange( Index1, Index2 );
end;


{= Find - Implements a binary search which is called by IndexOf only if the   =}
{=        list is sorted.                                                     =}

function TRzIntegerList.Find( N: Longint; var Index: Integer ): Boolean;
var
  L, H, I: Integer;
begin
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := ( L + H ) shr 1;
    if PLongint( FList[ I ] )^ < N then
      L := I + 1
    else
    begin
      H := I - 1;
      if PLongint( FList[ I ] )^ = N then
      begin
        Result := True;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  Index := L;
end; {= TRzIntegerList.Find =}


function TRzIntegerList.IndexOf( N: Longint ): Integer;
var
  I: Integer;
begin
  Result := -1;

  if not Sorted then
  begin
    I := 0;
    while ( I < GetCount ) and ( GetItem( I ) <> N ) do
      Inc( I );
    if I <> GetCount then
      Result := I;
  end
  else if Find( N, I ) then
    Result := I;
end; {= TRzIntegerList.IndexOf =}


procedure ListError;
begin
  raise EListError.Create( SSortedListError );
end;

procedure ListIndexError;
begin
  raise EListError.Create( SListIndexError );
end;


procedure TRzIntegerList.Insert( Index: Integer; Value: Longint );
begin
  if Sorted then
    ListError;

  if ( Index < 0 ) or ( Index > Count ) then
    ListIndexError;

  InsertNumber( Index, Value );                { Insert Integer onto Internal List }
end;


procedure TRzIntegerList.InsertNumber( Index: Integer; Value: Longint );
var
  P: PLongint;
begin
  if ( FMin <> FMax ) and ( ( Value < FMin ) or ( Value > FMax ) ) then
    raise EOutOfRange.CreateFmt( 'Value must be within %d..%d', [ FMin, FMax ]);

  if ( Duplicates = dupIgnore ) and ( IndexOf( Value ) >= 0 ) then
    Exit;

  New( P );                                      { Allocate Memory for Integer }
  P^ := Value;
  FList.Insert( Index, P );                { Insert Integer onto Internal List }
end;


procedure TRzIntegerList.Move( CurIndex, NewIndex: Integer );
begin
  FList.Move( CurIndex, NewIndex );
end;


procedure TRzIntegerList.QuickSort( L, R: Integer );
var
  I, J: Integer;
  P: PLongint;
begin                                          {= Generic QuickSort Procedure =}
  I := L;
  J := R;
  P := PLongint( FList[ ( L + R ) shr 1 ] );
  repeat
    while PLongint( FList[ I ] )^ < P^  do
      Inc( I );

    while PLongint( FList[ J ] )^ > P^ do
      Dec( J );

    if I <= J then
    begin
      FList.Exchange( I, J );
      Inc( I );
      Dec( J );
    end;
  until I > J;
  if L < J then
    QuickSort( L, J );
  if I < R then
    QuickSort( I, R );
end; {= TRzIntegerList.QuickSort =}


procedure TRzIntegerList.Sort;
begin
  if not Sorted and ( FList.Count > 1 ) then
    QuickSort( 0, FList.Count - 1 );
end;


end.
