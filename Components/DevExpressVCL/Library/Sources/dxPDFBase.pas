{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxPDFBase;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections, cxVariants, dxCore;

const
  dxPDFInvalidValue = -MaxInt;

type
  TdxPDFBase = class;

  TdxPDFBaseType = (otArray, otBoolean, otDouble, otCommand, otCommandName, otDictionary, otIndirectReference,
    otIndirectObject, otInteger, otName, otNull, otStream, otString, otComment, otStreamElement, otObjectStream);

  TdxPDFIntegersDictionary = class(TDictionary<Integer, Integer>);
  TdxPDFStringsDictionary = class(TDictionary<string, string>);
  TdxPDFDoubleList = class(TList<Double>);
  TdxPDFWordDictionary = class(TDictionary<string, Word>);

  { IdxPDFDocumentSharedObjectListener }

  IdxPDFDocumentSharedObjectListener = interface
  ['{430AD1E4-E008-46D1-B211-A00F56023C8E}']
    procedure DestroyHandler(Sender: TdxPDFBase);
  end;

  { TdxPDFReferencedObject }

  TdxPDFReferencedObject = class(TObject)
  strict private
    FReferenceCount: Integer;
  strict protected
    property ReferenceCount: Integer read FReferenceCount;
  public
    procedure Reference;
    procedure Release;
  end;

  { TdxPDFBase }

  TdxPDFBase = class(TdxPDFReferencedObject)
  strict private
    FGeneration: Integer;
    FNumber: Integer;
  protected
    class function GetObjectType: TdxPDFBaseType; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(ANumber, AGeneration: Integer); overload; virtual;

    property Generation: Integer read FGeneration write FGeneration;
    property Number: Integer read FNumber write FNumber;
    property ObjectType: TdxPDFBaseType read GetObjectType;
  end;

  { TdxPDFFactory<T> }

  TdxPDFFactory<T> = class
  strict private
    FClasses: TDictionary<string, T>;
  protected
    function ContainsKey(const AKey: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetClass(const AKey: string; out AClass: T): Boolean;
    procedure Register(const AKey: string; AClass: T);
    procedure UnregisterClass(const AKey: string);
  end;

  { TdxPDFObjectList<T> }

  TdxPDFObjectList<T: TdxPDFReferencedObject> = class(TList<T>)
  protected
    procedure Notify(const Item: T; Action: TCollectionNotification); override;
  end;
  TdxPDFReferencedObjects = class(TdxPDFObjectList<TdxPDFReferencedObject>);

  { TdxPDFBaseReferences }

  TdxPDFBaseReferences = class
  strict private
    FItems: TDictionary<Integer, TdxPDFReferencedObject>;
    function GetCount: Integer;
    function GetKeys: TEnumerable<Integer>;
    function GetItem(AKey: Integer): TdxPDFReferencedObject;
  protected
    property Items: TDictionary<Integer, TdxPDFReferencedObject> read FItems;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(const AKey: Integer): Boolean;
    function TryGetValue(const AKey: Integer; out AValue: TdxPDFReferencedObject): Boolean;
    procedure Add(const AKey: Integer; AValue: TdxPDFReferencedObject);
    procedure Clear;
    procedure Remove(const AKey: Integer);
    procedure TrimExcess;

    property Count: Integer read GetCount;
    property Keys: TEnumerable<Integer> read GetKeys;
    property Item[Key: Integer]: TdxPDFReferencedObject read GetItem; default;
  end;

  { TdxPDFBaseList }

  TdxPDFBaseList = class(TdxPDFObjectList<TdxPDFBase>);

  { TdxPDFUniqueReferences }

  TdxPDFUniqueReferences = class
  strict private
    FLock: TRtlCriticalSection;
    FDictionary: TDictionary<string, TdxPDFBase>;
    FNumbers: TDictionary<Integer, TdxPDFBase>;
    FReferences: TdxPDFBaseList;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(const AID: string; ANumber: Integer): Boolean;
    function ContainsValue(AValue: TdxPDFBase): Boolean;
    function TryGetValue(AKey: Integer; out AValue: TdxPDFBase): Boolean;
    procedure Add(const AID: string; ANumber: Integer; AObject: TdxPDFBase);
    procedure Remove(const AID: string); overload;
    procedure Clear;
  end;

  { TdxPDFReferencedObjectDictionary }

  TdxPDFReferencedObjectDictionary = class
  strict private
    FItems: TDictionary<string, TdxPDFReferencedObject>;
    function GetCount: Integer;
    procedure Changed(Sender: TObject; const Item: TdxPDFReferencedObject; Action: TCollectionNotification);
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsKey(const AKey: string): Boolean;
    function TryGetValue(const AKey: string; out AValue: TdxPDFReferencedObject): Boolean;
    procedure Add(const AKey: string; AValue: TdxPDFReferencedObject);
    procedure AddOrSetValue(const AKey: string; AValue: TdxPDFReferencedObject);
    procedure Clear;
    procedure Remove(const AKey: string);
    procedure TrimExcess;

    property Count: Integer read GetCount;
    property Items: TDictionary<string, TdxPDFReferencedObject> read FItems;
  end;

  { TdxPDFBytesStream }

  TdxPDFBytesStream = class
  strict private
    FReader: TcxReader;
    FStream: TBytesStream;
    FWriter: TcxWriter;
    function GetData: TBytes; inline;
    function GetPosition: Int64;
    function GetSize: Int64; inline;
    procedure SetPosition(const AValue: Int64);
  protected
    property Reader: TcxReader read FReader;
    property Writer: TcxWriter read FWriter;
  public
    constructor Create; overload;
    constructor Create(const AData: TBytes); overload;
    destructor Destroy; override;

    function ReadArray(ALength: Integer): TBytes; inline;
    function ReadByte: SmallInt; inline;
    function ReadFixed: Single; inline;
    function ReadInt: Integer; inline;
    function ReadLong: Int64; inline;
    function ReadOffset(ALength: Integer): Integer; inline;
    function ReadShort: Smallint;
    function ReadShortArray(ALength: Integer): TSmallIntDynArray; inline;
    function ReadShortArrayEx(ALength: Integer): TSmallIntDynArray; inline;
    function ReadString(ALength: Integer): string; inline;
    function ReadUShort: Word;
    function ToAlignedArray: TBytes;

    procedure WriteArray(const AArray: TBytes);
    procedure WriteByte(AValue: Byte);
    procedure WriteEmptyArray(ALength: Integer);
    procedure WriteFixed(AValue: Single);
    procedure WriteDouble(AValue: Double);
    procedure WriteInt(AValue: Integer);
    procedure WriteLong(AValue: Int64);
    procedure WriteShort(AValue: Smallint);
    procedure WriteShortArray(const AArray: TSmallIntDynArray);
    procedure WriteShortArrayEx(const AArray: TSmallIntDynArray);
    procedure WriteSpace;
    procedure WriteString(const AStr: string);


    property Data: TBytes read GetData;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
  end;

  { TdxPDFWriterStream }

  TdxPDFWriterStream = class(TdxPDFBytesStream)
  public
    procedure WriteCloseBracket;
    procedure WriteHexadecimalString(const AData: TBytes; ANumber: Integer);
    procedure WriteOpenBracket;
  end;

function dxPDFIsDoubleValid(AValue: Double): Boolean;
function dxPDFIsIntegerValid(AValue: Integer): Boolean;

implementation

uses
  Math, dxTypeHelpers;

function dxPDFIsDoubleValid(AValue: Double): Boolean;
begin
  Result := not SameValue(AValue, dxPDFInvalidValue, 1);
end;

function dxPDFIsIntegerValid(AValue: Integer): Boolean;
begin
  Result := dxPDFIsDoubleValid(AValue);
end;

{ TdxPDFReferencedObject }

procedure TdxPDFReferencedObject.Reference;
begin
  InterlockedIncrement(FReferenceCount);
end;

procedure TdxPDFReferencedObject.Release;
begin
  if FReferenceCount <= 1 then
    Free
  else
    InterlockedDecrement(FReferenceCount);
end;

{ TdxPDFBase }

constructor TdxPDFBase.Create;
begin
  inherited Create;
  FGeneration := dxPDFInvalidValue;
  FNumber := dxPDFInvalidValue;
end;

constructor TdxPDFBase.Create(ANumber, AGeneration: Integer);
begin
  Create;
  FNumber := ANumber;
  FGeneration := AGeneration;
end;

class function TdxPDFBase.GetObjectType: TdxPDFBaseType;
begin
  Result := otNull;
end;

{ TdxPDFFactory<T> }

constructor TdxPDFFactory<T>.Create;
begin
  inherited Create;
  FClasses := TDictionary<string, T>.Create;
end;

destructor TdxPDFFactory<T>.Destroy;
begin
  FreeAndNil(FClasses);
  inherited Destroy;
end;

function TdxPDFFactory<T>.TryGetClass(const AKey: string; out AClass: T): Boolean;
begin
  Result := FClasses.TryGetValue(AKey, AClass);
end;

procedure TdxPDFFactory<T>.Register(const AKey: string; AClass: T);
begin
  if not ContainsKey(AKey) then
    FClasses.Add(AKey, AClass);
end;

procedure TdxPDFFactory<T>.UnregisterClass(const AKey: string);
begin
  if FClasses.ContainsKey(AKey) then
    FClasses.Remove(AKey);
end;

function TdxPDFFactory<T>.ContainsKey(const AKey: string): Boolean;
begin
  Result := FClasses.ContainsKey(AKey);
end;

{ TdxPDFObjectList }

procedure TdxPDFObjectList<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  case Action of
    cnAdded:
      Item.Reference;
    cnRemoved:
      Item.Release;
  end;
end;

{ TdxPDFBaseReferences }

constructor TdxPDFBaseReferences.Create;
begin
  inherited Create;
  FItems := TDictionary<Integer, TdxPDFReferencedObject>.Create;
end;

destructor TdxPDFBaseReferences.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxPDFBaseReferences.ContainsKey(const AKey: Integer): Boolean;
begin
  Result := FItems.ContainsKey(AKey);
end;

function TdxPDFBaseReferences.TryGetValue(const AKey: Integer; out AValue: TdxPDFReferencedObject): Boolean;
begin
  AValue := nil;
  Result := FItems.TryGetValue(AKey, AValue);
end;

procedure TdxPDFBaseReferences.Add(const AKey: Integer; AValue: TdxPDFReferencedObject);
begin
  FItems.Add(AKey, AValue);
  AValue.Reference;
end;

procedure TdxPDFBaseReferences.Clear;
var
  AValue: TdxPDFReferencedObject;
begin
  for AValue in FItems.Values do
    AValue.Release;
  FItems.Clear;
end;

procedure TdxPDFBaseReferences.Remove(const AKey: Integer);
begin
  FItems.Items[AKey].Release;
  FItems.Remove(AKey);
end;

procedure TdxPDFBaseReferences.TrimExcess;
begin
  FItems.TrimExcess;
end;

function TdxPDFBaseReferences.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPDFBaseReferences.GetKeys: TEnumerable<Integer>;
begin
  Result := FItems.Keys;
end;

function TdxPDFBaseReferences.GetItem(AKey: Integer): TdxPDFReferencedObject;
begin
  if not FItems.TryGetValue(AKey, Result) then
    Result := nil;
end;

{ TdxPDFUniqueReferences }

constructor TdxPDFUniqueReferences.Create;
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FDictionary := TDictionary<string, TdxPDFBase>.Create;
  FNumbers := TDictionary<Integer, TdxPDFBase>.Create;
  FReferences := TdxPDFBaseList.Create;
end;

destructor TdxPDFUniqueReferences.Destroy;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FNumbers);
  FreeAndNil(FReferences);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxPDFUniqueReferences.ContainsKey(const AID: string; ANumber: Integer): Boolean;
begin
  Result := FDictionary.ContainsKey(AID) or FNumbers.ContainsKey(ANumber);
end;

function TdxPDFUniqueReferences.ContainsValue(AValue: TdxPDFBase): Boolean;
begin
  Result := FDictionary.ContainsValue(AValue);
end;

function TdxPDFUniqueReferences.TryGetValue(AKey: Integer; out AValue: TdxPDFBase): Boolean;
begin
  Result := FNumbers.TryGetValue(AKey, AValue);
end;

procedure TdxPDFUniqueReferences.Add(const AID: string; ANumber: Integer; AObject: TdxPDFBase);
begin
  EnterCriticalSection(FLock);
  try
    if not FDictionary.ContainsKey(AID) then
    begin
      FDictionary.Add(AID, AObject);
      if dxPDFIsIntegerValid(ANumber) then
        FNumbers.Add(ANumber, AObject);
      FReferences.Add(AObject);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxPDFUniqueReferences.Remove(const AID: string);
var
  AObject: TdxPDFBase;
begin
  if FDictionary.TryGetValue(AID, AObject) then
  begin
    FNumbers.Remove(AObject.Number);
    FDictionary.Remove(AID);
    FReferences.Remove(AObject)
  end;
end;

procedure TdxPDFUniqueReferences.Clear;
begin
  FDictionary.Clear;
  FNumbers.Clear;
  FReferences.Clear;
end;

{ TdxPDFReferencedObjectDictionary }

constructor TdxPDFReferencedObjectDictionary.Create;
begin
  inherited Create;
  FItems := TDictionary<string, TdxPDFReferencedObject>.Create;
  FItems.OnValueNotify := Changed;
end;

destructor TdxPDFReferencedObjectDictionary.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxPDFReferencedObjectDictionary.ContainsKey(const AKey: string): Boolean;
begin
  Result := FItems.ContainsKey(AKey);
end;

function TdxPDFReferencedObjectDictionary.TryGetValue(const AKey: string; out AValue: TdxPDFReferencedObject): Boolean;
begin
  Result := FItems.TryGetValue(AKey, AValue);
end;

procedure TdxPDFReferencedObjectDictionary.Add(const AKey: string; AValue: TdxPDFReferencedObject);
begin
  FItems.Add(AKey, AValue);
end;

procedure TdxPDFReferencedObjectDictionary.AddOrSetValue(const AKey: string; AValue: TdxPDFReferencedObject);
begin
  FItems.AddOrSetValue(AKey, AValue);
end;

procedure TdxPDFReferencedObjectDictionary.Clear;
begin
  FItems.Clear;
end;

procedure TdxPDFReferencedObjectDictionary.Remove(const AKey: string);
begin
  FItems.Remove(AKey);
end;

procedure TdxPDFReferencedObjectDictionary.TrimExcess;
begin
  FItems.TrimExcess;
end;

function TdxPDFReferencedObjectDictionary.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TdxPDFReferencedObjectDictionary.Changed(Sender: TObject; const Item: TdxPDFReferencedObject;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      Item.Reference;
    cnRemoved:
      Item.Release;
  end;
end;

{ TdxPDFBytesStream }

constructor TdxPDFBytesStream.Create;
var
  AData: TBytes;
begin
  Create(TdxByteArray.Resize(AData, 0));
end;

constructor TdxPDFBytesStream.Create(const AData: TBytes);
begin
  inherited Create;
  FStream := TBytesStream.Create(AData);
  FReader := TcxReader.Create(FStream);
  FWriter := TcxWriter.Create(FStream);
end;

destructor TdxPDFBytesStream.Destroy;
begin
  FreeAndNil(FWriter);
  FreeAndNil(FReader);
  FreeAndNil(FStream);
  inherited Destroy;
end;

function TdxPDFBytesStream.GetData: TBytes;
begin
  SetLength(Result, FStream.Size);
  cxCopyData(FStream.Bytes, Result, FStream.Size);
end;

function TdxPDFBytesStream.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

function TdxPDFBytesStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

procedure TdxPDFBytesStream.SetPosition(const AValue: Int64);
begin
  FStream.Position := AValue;
end;

function TdxPDFBytesStream.ReadByte: SmallInt;
begin
  if Reader.Stream.Position < Size then
    Result := Reader.ReadByte
  else
    Result := -1;
end;

function TdxPDFBytesStream.ReadShort: Smallint;
begin
  Result := (ReadByte shl 8) + ReadByte;
end;

function TdxPDFBytesStream.ReadUShort: Word;
begin
  Result := ReadShort;
end;

function TdxPDFBytesStream.ToAlignedArray: TBytes;
var
  ALength, AFactor: Integer;
begin
  FStream.Position := 0;
  ALength := FStream.Size;
  AFactor := ALength mod 4;
  SetLength(Result, ALength + IfThen(AFactor > 0, 4 - AFactor, 0));
  FStream.Read(Result[0], ALength);
end;

function TdxPDFBytesStream.ReadInt: Integer;
begin
  Result := (ReadByte shl 24) + (ReadByte shl 16) + (ReadByte shl 8) + ReadByte;
end;

function TdxPDFBytesStream.ReadLong: Int64;
begin
  Result := Reader.ReadLargeInt;
end;

function TdxPDFBytesStream.ReadOffset(ALength: Integer): Integer;
begin
  case ALength of
    2:
      Result := ReadUShort;
    3:
      Result := (ReadByte shl 16) + (ReadByte shl 8) + ReadByte;
    4:
      Result := ReadInt;
  else
    Result := ReadByte;
  end;
end;

function TdxPDFBytesStream.ReadArray(ALength: Integer): TBytes;
var
  AArray: TBytes;
begin
  SetLength(AArray, ALength);
  FStream.Read(AArray[0], ALength);
  Result := AArray;
end;

function TdxPDFBytesStream.ReadShortArray(ALength: Integer): TSmallIntDynArray;
var
  I: Integer;
begin
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := ReadShort;
end;

function TdxPDFBytesStream.ReadShortArrayEx(ALength: Integer): TSmallIntDynArray;
var
  I: Integer;
begin
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := ReadShort;
end;

function TdxPDFBytesStream.ReadString(ALength: Integer): string;
var
  ASb: TStringBuilder;
  I: Integer;
begin
  ASb := TStringBuilder.Create;
  try
    for I := 0 to ALength - 1 do
      ASb.Append(Char(ReadByte));
    Result := ASb.ToString;
  finally
    ASb.Free;
  end;
end;

function TdxPDFBytesStream.ReadFixed: Single;
begin
  Result := ReadInt / 65536;
end;

procedure TdxPDFBytesStream.WriteFixed(AValue: Single);
begin
  WriteInt(Trunc(AValue * 65536));
end;

procedure TdxPDFBytesStream.WriteDouble(AValue: Double);
var
  ADoubleMask: string;
  AZero: SmallInt;
  V: Integer;
begin
  ADoubleMask := '0.################';
  AZero := Byte('0');
  WriteSpace;

  if Abs(AValue) < 0.000015 then
  begin
    WriteByte(Byte('0'));
    Exit;
  end;
  if AValue < 0 then
  begin
    WriteByte(Byte('-'));
    AValue := -AValue;
  end;
  if AValue < 1.0 then
  begin
    AValue := AValue + 0.000005;
    if AValue >= 1 then
    begin
      WriteByte(Byte('1'));
      Exit;
    end;
    V := Trunc(AValue * 100000);

    WriteByte(Byte('0'));
    WriteByte(Byte('.'));
    WriteByte(Byte(V div 10000 + AZero));
    if V mod 10000 <> 0 then
    begin
      WriteByte(Byte((V div 1000) mod 10 + AZero));
      if V mod 1000 <> 0 then
      begin
        WriteByte(Byte((V div 100) mod 10 + AZero));
        if V mod 100 <> 0 then
        begin
          WriteByte(Byte((V div 10) mod 10 + AZero));
          if V mod 10 <> 0 then
            WriteByte(Byte(V mod 10 + AZero));
        end;
      end;
    end;
    Exit;
  end
  else
    if AValue <= 32767 then
    begin
      AValue := AValue + 0.00005;
      V := Trunc(AValue * 10000);
      if V >= 100000000 then
        WriteByte(Byte(((V div 100000000) mod 10 + AZero)));
      if V >= 10000000 then
        WriteByte(Byte(((V div 10000000) mod 10 + AZero)));
      if V >= 1000000 then
        WriteByte(Byte(((V div 1000000) mod 10 + AZero)));
      if V >= 100000 then
        WriteByte(Byte(((V div 100000) mod 10 + AZero)));
      if V >= 10000 then
        WriteByte(Byte(((V div 10000) mod 10 + AZero)));
      if V mod 10000 <> 0 then
      begin
        WriteByte(Byte('.'));
        WriteByte(Byte(((V div 1000) mod 10 + AZero)));
        if V mod 1000 <> 0 then
        begin
          WriteByte(Byte(((V div 100) mod 10 + AZero)));
          if V mod 100 <> 0 then
          begin
            WriteByte(Byte(((V div 10) mod 10 + AZero)));
            if V mod 10 <> 0 then
              WriteByte(Byte((V mod 10 + AZero)));
          end;
        end;
      end;
      Exit;
    end;
  WriteString(FormatFloat(ADoubleMask, AValue));
end;

procedure TdxPDFBytesStream.WriteByte(AValue: Byte);
begin
  Writer.WriteByte(AValue);
end;

procedure TdxPDFBytesStream.WriteShort(AValue: Smallint);
begin
  WriteByte((AValue and $FF00) shr 8);
  WriteByte(AValue and $FF);
end;

procedure TdxPDFBytesStream.WriteInt(AValue: Integer);
begin
  WriteByte((AValue and $FF000000) shr 24);
  WriteByte((AValue and $FF0000) shr 16);
  WriteByte((AValue and $FF00) shr 8);
  WriteByte(AValue and $FF);
end;

procedure TdxPDFBytesStream.WriteLong(AValue: Int64);
begin
  WriteByte((AValue and $FF00000000000000) shr 56);
  WriteByte((AValue and $FF000000000000) shr 48);
  WriteByte((AValue and $FF00000000) shr 32);
  WriteByte((AValue and $FF000000) shr 24);
  WriteByte((AValue and $FF0000) shr 16);
  WriteByte((AValue and $FF0000) shr 16);
  WriteByte((AValue and $FF00) shr 8);
  WriteByte(AValue and $FF);
end;

procedure TdxPDFBytesStream.WriteEmptyArray(ALength: Integer);
var
  I: Integer;
begin
  for I := 0 to ALength - 1 do
    WriteByte(0);
end;

procedure TdxPDFBytesStream.WriteArray(const AArray: TBytes);
begin
  FStream.WriteBuffer(AArray[0], Length(AArray));
end;

procedure TdxPDFBytesStream.WriteShortArray(const AArray: TSmallIntDynArray);
var
  AValue: SmallInt;
begin
  for AValue in AArray do
    WriteShort(AValue);
end;

procedure TdxPDFBytesStream.WriteShortArrayEx(const AArray: TSmallIntDynArray);
var
  I: Integer;
begin
  for I := 0 to Length(AArray) - 1 do
    WriteShort(AArray[I]);
end;

procedure TdxPDFBytesStream.WriteSpace;
begin
  WriteByte(Byte(' '));
end;

procedure TdxPDFBytesStream.WriteString(const AStr: string);
var
  C: Char;
begin
  for C in AStr do
    WriteByte(Byte(C));
end;

{ TdxPDFWriterStream }

procedure TdxPDFWriterStream.WriteCloseBracket;
begin
  WriteByte(Byte(']'));
end;

procedure TdxPDFWriterStream.WriteHexadecimalString(const AData: TBytes; ANumber: Integer);
var
  B: Byte;
  L, ALength, I, J, K: Integer;
  ABytes: TBytes;
begin
  L := Length(AData);
  ALength := L * 2 + 2;
  SetLength(ABytes, ALength);
  ABytes[0] := 60;
  ABytes[ALength - 1] := 62;
  J := 0;
  for I := 0 to L - 1 do
  begin
    B := AData[I];
    K := B shr 4;
    Inc(J);
    ABytes[J] := IfThen(k > 9 , k + $37, k + $30);
    K := B and 15;
    Inc(J);
    ABytes[J] := IfThen(k > 9 , k + $37, k + $30);
  end;
  WriteArray(ABytes);
end;

procedure TdxPDFWriterStream.WriteOpenBracket;
begin
  WriteByte(Byte('['));
end;

end.
