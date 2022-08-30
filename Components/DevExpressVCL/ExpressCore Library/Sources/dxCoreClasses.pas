{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxCoreClasses;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  ObjAuto, TypInfo, dxCore, SyncObjs;

const
  cxE_NOINTERFACE = HResult($80004002);

type
  { TcxIUnknownObject }

  TcxIUnknownObject = class(TObject, IUnknown)
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  { TcxInterfacedPersistent }

  TcxInterfacedPersistentClass = class of TcxInterfacedPersistent;

  TcxInterfacedPersistent = class(TInterfacedPersistent)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    property Owner: TPersistent read FOwner;
  end;

  { TcxOwnedPersistent }

  TcxOwnedPersistent = class(TPersistent)
  private
    FOwner: TPersistent;
  protected
    procedure DoAssign(Source: TPersistent); virtual;
    function GetOwner: TPersistent; override;
    property Owner: TPersistent read FOwner write FOwner;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
  end;

  { TcxOwnedInterfacedPersistent }

  TcxOwnedInterfacedPersistent = class(TcxOwnedPersistent, IUnknown)
  protected
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;

  { TcxLockablePersistent }

  TcxLockablePersistent = class(TcxOwnedInterfacedPersistent)
  strict private
    FHasChanges: Boolean;
    FLockCount: Integer;
  protected
    procedure Changed;
    procedure DoChanged; virtual; abstract;
  public
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
  end;

  { TcxFreeNotificator }

  TcxFreeNotificationEvent = procedure(Sender: TComponent) of object;

  TcxFreeNotificator = class(TComponent)
  private
    FOnFreeNotification: TcxFreeNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure AddSender(ASender: TComponent);
    procedure RemoveSender(ASender: TComponent);
    property OnFreeNotification: TcxFreeNotificationEvent read FOnFreeNotification write FOnFreeNotification;
  end;

  { TcxCollection }

  TcxCollection = class(TCollection)
  public
    procedure Assign(Source: TPersistent); override;
  end;

  { TcxDoublyLinkedObject }

  TcxDoublyLinkedObjectClass = class of TcxDoublyLinkedObject;
  TcxDoublyLinkedObject = class
  private
    FPrev: TcxDoublyLinkedObject;
    FNext: TcxDoublyLinkedObject;
  public
    property Prev: TcxDoublyLinkedObject read FPrev write FPrev;
    property Next: TcxDoublyLinkedObject read FNext write FNext;
  end;

  { TcxDoublyLinkedObjectList }

  TcxDoublyLinkedObjectList = class
  strict private
    function GetCount: Integer;
  protected
    FFirst: TcxDoublyLinkedObject;
    FLast: TcxDoublyLinkedObject;

    function CreateLinkedObject: TcxDoublyLinkedObject; virtual;
    procedure DoAdd(ASource: TcxDoublyLinkedObject); virtual;
    function GetLinkedObjectClass: TcxDoublyLinkedObjectClass; virtual;
  public
    destructor Destroy; override;

    function Add: TcxDoublyLinkedObject; virtual;
    procedure Clear; virtual;
    procedure InsertBefore(ADestination, ASource: TcxDoublyLinkedObject);
    procedure InsertAfter(ADestination, ASource: TcxDoublyLinkedObject);
    procedure Delete(ALinkedObject: TcxDoublyLinkedObject); virtual;
    procedure Extract(ALinkedObject: TcxDoublyLinkedObject); virtual;
    procedure Remove(ALinkedObject: TcxDoublyLinkedObject); virtual; //obsolete;

    property First: TcxDoublyLinkedObject read FFirst;
    property Last: TcxDoublyLinkedObject read FLast;
    property Count: Integer read GetCount;
  end;

  TcxDoublyLinkedData = class(TcxDoublyLinkedObject)
  private
    FData: Pointer;
    FNextSorted: TcxDoublyLinkedData;
    FPrevSorted: TcxDoublyLinkedData;
  public
    property Data: Pointer read FData write FData;
  end;

  TcxDoublyLinkedDataList = class(TcxDoublyLinkedObjectList)
  public
    function Add(AData: Pointer): TcxDoublyLinkedData; reintroduce;
    function GetLinkedObjectClass: TcxDoublyLinkedObjectClass; override;

    procedure PopulateFromList(AList: TList); virtual;
    procedure PopulateToList(AList: TList); virtual;
    procedure Sort(ACompareProc: TListSortCompare);

  end;

  TcxDoublyLinkedIndexedData = class(TcxDoublyLinkedData)
  private
    FIndex: Integer;
  public
    property Index: Integer read FIndex write FIndex;
  end;

  TcxDoublyLinkedDataIndexedList = class(TcxDoublyLinkedDataList)
  public
    function Add(AData: Pointer; AIndex: Integer): TcxDoublyLinkedIndexedData;
    function GetLinkedObjectClass: TcxDoublyLinkedObjectClass; override;

    procedure PopulateFromList(AList: TList); override;
  end;

  { TdxFastList }

  TdxPointerList = array[0..0] of Pointer;
  PdxPointerList = {$IFDEF DELPHI16}^TdxPointerList{$ELSE}PPointerList{$ENDIF};
  TCompareItems = function (AItem1, AItem2: Pointer): Integer of object;
  TdxListDirection = (ldFromBeginning, ldFromEnd);

  TdxFastList = class
  private
    FCapacity: Integer;
    FCount: Integer;
    FList: PdxPointerList;
    procedure DoDelete(AIndex: Integer); inline;
  protected
    procedure CheckDeleteRange(AIndex, ACount: Integer); inline;
    procedure DoDeleteRange(AIndex, ACount: Integer);
    function GetItem(AIndex: Integer): Pointer; inline;
    procedure Grow;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer); virtual;
    procedure SetItem(AIndex: Integer; AItem: Pointer); inline;
  public
    constructor Create(ACapacity: Integer = 0);
    destructor Destroy; override;
    function Add(AItem: Pointer): Integer; inline;
    procedure Assign(ASource: TdxFastList); overload;
    procedure Assign(ASource: TList); overload;
    procedure Assign(ASource: TdxFastList; AOperator: TListAssignOp); overload;
    procedure Clear; virtual;
    procedure Delete(AIndex: Integer);
    procedure DeleteRange(AIndex, ACount: Integer); inline;
    function Empty: Boolean; inline;
    procedure Exchange(AIndex1, AIndex2: Integer);
    function Expand: TdxFastList;
    function Extract(AItem: Pointer; ADirection: TdxListDirection = ldFromBeginning): Pointer;
    function ExtractByIndex(AIndex: Integer): Pointer;
    function First: Pointer;
    function GetTList: TList;
    function IndexOf(AItem: Pointer; ADirection: TdxListDirection = ldFromBeginning): Integer;
    procedure Insert(AIndex: Integer; AItem: Pointer); overload;
    procedure Insert(AIndex: Integer; AList: TdxFastList); overload;
    function Last: Pointer;
    procedure Move(ACurrentIndex, ANewIndex: Integer);
    procedure Pack(APattern: Pointer = nil);
    function Remove(AItem: Pointer): Integer;
    procedure Reverse;
    procedure Sort(ACompare: TCompareItems; AMultiThreaded: Boolean = False); overload;
    procedure Sort(ACompare: TListSortCompare; AMultiThreaded: Boolean = False); overload;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read GetItem write SetItem; default;
    property List: PdxPointerList read FList;
  end;

  { TdxFastObjectList }

  TdxFastObjectList = class(TdxFastList)
  private
    FOwnsObjects: Boolean;
  protected
    function GetItem(AIndex: Integer): TObject; inline;
    procedure SetItem(AIndex: Integer; AObject: TObject); inline;
    procedure SetCount(AValue: Integer); override;
  public
    constructor Create(AOwnsObjects: Boolean = True; ACapacity: Integer = 0);
    function Add(AObject: TObject): Integer; inline;
    procedure Clear; override;
    procedure Delete(AIndex: Integer);
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(AItem: TObject): TObject; inline;
    procedure ExtractRange(AIndex, ACount: Integer);
    function Remove(AObject: TObject): Integer; inline;
    function IndexOf(AObject: TObject; ADirection: TdxListDirection = ldFromBeginning): Integer; inline;
    procedure Insert(AIndex: Integer; AObject: TObject); inline;
    function First: TObject; inline;
    function Last: TObject; inline;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
  end;

  { TdxMulticastMethod<T> }

  IdxMulticastMethod<T> = interface
  ['{BCD2B104-F540-4470-B0E9-5A15EAF6FE71}']
    function GetInvoke: T;
    procedure Add(const AMethod: T); overload;
    procedure Clear;
    function GetCount: Integer;
    procedure Remove(const AMethod: T); overload;

    property Count: Integer read GetCount;
    property Invoke: T read GetInvoke;
  end;

  TdxEventHandlerInstance = class
  strict private
    type
      PParameterInfos = ^TParameterInfos;
      TParameterInfos = array[0..255] of ^PTypeInfo;
  {$IFNDEF CPUX64}
    function GetTypeSize(ATypeInfo: PTypeInfo): Integer;
  {$ENDIF CPUX64}
    class function GetMethodAdditionalInfo(ATypeData: PTypeData): Pointer; static;
    class procedure NextParameter(var P: PByte); static; inline;
  strict protected
    FParamInfos: PParameterInfos;
    FStackSize: Integer;
  {$IFDEF CPUX64}
    FRegisterFlag: Word;
  {$ENDIF CPUX64}
    FDynamicInvokeEvent: TDynamicInvokeEvent;
    procedure InternalHandler(AParams: Pointer);
    procedure Handler(AParams: Pointer); virtual;
    procedure RegisterStub;
  public
    constructor Create(const ADynamicInvokeEvent: TDynamicInvokeEvent; ATypeData: PTypeData);
    class function CreateMethodPointer(const ADynamicInvokeEvent: TDynamicInvokeEvent; ATypeData: PTypeData): TMethod; static;
    class function GetMethodCallingConvention(ATypeData: PTypeData): TCallConv; static; inline;
    class function GetMethodParametersTypeInfo(ATypeData: PTypeData): Pointer; static; inline;
  end;

  TdxMulticastImplementator = class(TInterfacedObject)
  strict private
    FLock: TCriticalSection;
    FHandlers: array of TMethod;
    FInternalDispatcher: TMethod;
    class procedure InvokeMethod(const AMethod: TMethod; AParams: Pointer; AStackSize: Integer); static;
    procedure InternalInvoke(AParams: PParameters; AStackSize: Integer);
  protected
    function GetCount: Integer;
    procedure InternalClear;
    procedure InternalAdd(const AValue);
    procedure InternalRemove(const AValue);
    procedure InternalSetDispatcher(var ADispatcher; ATypeData: PTypeData);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TdxMulticastImplementator<T> = class(TdxMulticastImplementator, IdxMulticastMethod<T>)
  strict private
    FInvoke: T;
    function GetInvoke: T;
    procedure SetEventDispatcher(var ADispatcher: T; ATypeData: PTypeData);
  public
    constructor Create;
    procedure Add(const AMethod: T);
    procedure Remove(const AMethod: T);
    procedure Clear;
    property Invoke: T read FInvoke;
  end;

  TdxMulticastMethod<T> = record
  strict private
    FImplementator: IdxMulticastMethod<T>;
    function GetCount: Integer;
    function GetEmpty: Boolean;
    function GetInvoke: T;
    procedure Validate;
  public
    procedure Add(const AMethod: T);
    procedure Remove(const AMethod: T);
    procedure Clear;
    function Clone: TdxMulticastMethod<T>;

    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property Invoke: T read GetInvoke;
  end;

  TdxNotifyEventHandler = TdxMulticastMethod<TNotifyEvent>;

  { TdxNullableValue<T> }

  TdxNullableValue<T> = packed record
  strict private
    FHasValue: LongBool;
    FValue: T;
    function GetHasValue: Boolean; inline;
    function GetIsNull: Boolean; inline;
    function GetValue: T; inline;
    procedure SetValue(const Value: T); inline;
    class function GetNull: TdxNullableValue<T>; static;
  public
    constructor Create(AUse: Boolean; const AValue: T); overload;
    procedure Reset;

    class operator Implicit(const A: T): TdxNullableValue<T>;
    class operator Implicit(const A: TdxNullableValue<T>): T;
    class operator Explicit(const A: TdxNullableValue<T>): T;
    class operator Equal(const A, B: TdxNullableValue<T>): Boolean;
    class operator Equal(const A: TdxNullableValue<T>; const B: T): Boolean;
    class operator GreaterThan(const A: TdxNullableValue<T>; const B: T): Boolean;
    class operator LessThan(const A: TdxNullableValue<T>; const B: T): Boolean;
    class operator LogicalAnd(const A, B: TdxNullableValue<T>): TdxNullableValue<T>;
    class operator LogicalNot(const A: TdxNullableValue<T>): TdxNullableValue<T>;
    class operator LogicalOr(const A, B: TdxNullableValue<T>): TdxNullableValue<T>;
    class operator NotEqual(const A, B: TdxNullableValue<T>): Boolean;
    class operator NotEqual(const A: TdxNullableValue<T>; const B: T): Boolean;

    class function IfThen(AValue: Boolean; const ATrue, AFalse: TdxNullableValue<T>): TdxNullableValue<T>; overload; static;
    class function IfThen(AValue: Boolean; const ATrue: TdxNullableValue<T>): TdxNullableValue<T>; overload; static;
    class function IfThen(AValue: Boolean; const ATrue, AFalse: T): TdxNullableValue<T>; overload; static;
    class function IfThen(AValue: Boolean; const ATrue: T): TdxNullableValue<T>; overload; static;

    function IsNullOrEmpty: Boolean;
  {$IFNDEF DELPHIXE}
    function IsEqual(const AValue: TdxNullableValue<T>): Boolean;
  {$ENDIF}

    property HasValue: Boolean read GetHasValue;
    property IsNull: Boolean read GetIsNull;
    property Value: T read GetValue write SetValue;
    class property Null: TdxNullableValue<T> read GetNull;
  end;

  TdxNullableBoolean = TdxNullableValue<Boolean>;
  TdxNullableString = TdxNullableValue<string>;
  TdxNullableInteger = TdxNullableValue<Integer>;
  TdxNullableSingle = TdxNullableValue<Single>;
  TdxNullableRect = TdxNullableValue<TRect>;

  { TdxStringComparer }

  TdxStringComparer = class(TCustomComparer<string>)
  private class var
    FOrdinal: TCustomComparer<string>;
  protected
    class procedure Finalize;
  public
    class function Ordinal: TCustomComparer<string>;
  end;

  { TdxIStringComparer }

  TdxIStringComparer = class(TCustomComparer<string>)
  private class var
    FOrdinal: TCustomComparer<string>;
  protected
    class procedure Finalize;
  public
    class function Ordinal: TCustomComparer<string>;
  end;

  { TdxOrdinalIStringComparer }

  TdxOrdinalIStringComparer = class(TCustomComparer<string>)
  public
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const ALeft, ARight: string): Boolean; override;
    function GetHashCode(const AValue: string): Integer; override;
  end;

  { TdxOrdinalStringEqualityComparer }

  TdxOrdinalStringComparer = class(TCustomComparer<string>)
  public
    function Compare(const ALeft, ARight: string): Integer; override;
    function Equals(const ALeft, ARight: string): Boolean; override;
    function GetHashCode(const AValue: string): Integer; override;
  end;

  { TdxMemoryStream }

  TdxMemoryStream = class(TMemoryStream)
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    constructor Create(const AFileName: string; AMode: Word = fmOpenRead or fmShareDenyWrite); overload;
    constructor Create(const AContent: TArray<Byte>); overload;
    constructor Create(const AContent: TArray<Byte>; ACount: Integer); overload;
    constructor Create(const AContent: TArray<Byte>; AOffset, ACount: Integer); overload;
    procedure LoadFromFile(const AFileName: string); reintroduce; overload;
    procedure LoadFromFile(const AFileName: string; AMode: Word); reintroduce; overload;
    function ReadByte: Byte;
    function ReadInteger: Integer;
    procedure WriteByte(const Value: Byte);
    procedure WriteByteArray(const Value: TArray<Byte>);
    procedure WriteInteger(const Value: Integer);
    function ToArray: TArray<Byte>;
  end;

  { TdxRectList }

  TdxRectList = class(TList<TRect>)
  public
    function Union: TRect;
  end;

procedure dxLiaisonSort(AList: TList; ACompareProc: TListSortCompare = nil);
procedure dxQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TCompareItems; AMultiThreaded: Boolean = False); overload;
procedure dxQuickSortList(AList: TList; ACompareFunc: TCompareItems;
  AMultiThreaded: Boolean = False); overload; inline;
procedure dxInternalQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TCompareItems); overload;
procedure dxQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TListSortCompare; AMultiThreaded: Boolean = False); overload;
procedure dxQuickSortList(AList: TList; ACompareFunc: TListSortCompare;
  AMultiThreaded: Boolean = False); overload; inline;
procedure dxInternalQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TListSortCompare); overload;


implementation

uses
  RTLConsts, Rtti, Math,
{$IFDEF MSWINDOWS}
  Windows,
  dxThreading,
{$ENDIF}
  cxGeometry;


const
  dxInsertionSortCount = 35;
  dxInsertionSortLast  = dxInsertionSortCount - 1;


procedure InsertionSort(ASortList: PdxPointerList; ALast: Integer; ACompareFunc: TCompareItems); overload;
var
  I, J: Integer;
  ATemp: Pointer;
begin;
  I := 0;
  J := ALast;
  if J > dxInsertionSortLast then
    J := dxInsertionSortLast;
  repeat
    if ACompareFunc(ASortList[J], ASortList[I]) < 0 then
      I := J;
    Dec(J);
  until J <= 0;
  if I > 0 then
  begin;
    ATemp := ASortList[0];
    ASortList[0] := ASortList[I];
    ASortList[I] := ATemp;
  end;
  J := 1;
  while True do
  begin
    if J >= ALast then
      Break;
    Inc(J);
    if ACompareFunc(ASortList[J], ASortList[J - 1]) < 0 then
    begin;
      ATemp := ASortList[J];
      I := J;
      repeat
        ASortList[I] := ASortList[I - 1];
        Dec(I);
      until not (ACompareFunc(ATemp, ASortList[I - 1]) < 0);
      ASortList[I] := ATemp;
    end;
  end;
end;

procedure ModifiedQuickSort(ASortList: PdxPointerList; ARight: Integer; ACompareFunc: TCompareItems); overload;
var
  I, J: Integer;
  APivot, ATemp: Pointer;
begin;
  while True do
  begin;
    J := ARight;
    if J <= dxInsertionSortLast then
      Break;
    I := J shr 1;
    APivot := ASortList[I];
    if ACompareFunc(ASortList[J], ASortList[0]) < 0 then
    begin;
      ATemp := ASortList[0];
      ASortList[0] := ASortList[J];
      ASortList[J] := ATemp;
    end;
    if ACompareFunc(APivot, ASortList[0]) < 0 then
    begin;
      APivot := ASortList[0];
      ASortList[0] := ASortList[I];
      ASortList[I] := APivot;
    end
    else if ACompareFunc(ASortList[J], APivot) < 0 then
    begin;
      APivot := ASortList[J];
      ASortList[J] := ASortList[I];
      ASortList[I] := APivot;
    end;
    I := 0;
    repeat
      Inc(I);
    until not (ACompareFunc(ASortList[I], APivot) < 0);
    repeat
      Dec(J);
    until not (ACompareFunc(APivot, ASortList[J]) < 0);
    if I < J then
      repeat
        ATemp := ASortList[I];
        ASortList[I] := ASortList[J];
        ASortList[J] := ATemp;
        repeat
          Inc(I);
        until not (ACompareFunc(ASortList[I], APivot) < 0);
        repeat
          Dec(J);
        until not (ACompareFunc(APivot, ASortList[J]) < 0);
      until I >= J;
    Dec(I);
    Inc(J);
    if I + J <= ARight then
    begin;
      if I > dxInsertionSortLast then
        ModifiedQuickSort(ASortList, I, ACompareFunc);
      ARight := ARight - J;
      Pointer(ASortList) := Pointer(@ASortList[J]);
    end
    else
    begin;
      if J + dxInsertionSortLast < ARight then
        ModifiedQuickSort(Pointer(@ASortList[J]), ARight - J, ACompareFunc);
      ARight := I;
    end;
  end;
end;

procedure dxQuickSortList(AList: TList; ACompareFunc: TCompareItems;
  AMultiThreaded: Boolean = False);
begin;
  dxQuickSortList(Pointer(AList.List), AList.Count, ACompareFunc, AMultiThreaded);
end;

procedure dxQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TCompareItems; AMultiThreaded: Boolean = False);
begin;
  if (ASortList <> nil) and (ACount > 1) then
{$IFDEF MSWINDOWS}
    if AMultiThreaded and dxCanUseMultiThreading and (ACount > dxMaxUserWorkItems) then
      dxSortHelper.Sort(ASortList, ACount, ACompareFunc)
    else
{$ENDIF}
      dxInternalQuickSortList(ASortList, ACount, ACompareFunc);
end;

procedure dxInternalQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TCompareItems);
begin;
  if (ASortList <> nil) and (ACount > 1) then
  begin
    Dec(ACount);
    if ACount > dxInsertionSortLast then
      ModifiedQuickSort(ASortList, ACount, ACompareFunc);
    InsertionSort(ASortList, ACount, ACompareFunc);
  end;
end;


procedure InsertionSort(ASortList: PdxPointerList; ALast: Integer; ACompareFunc: TListSortCompare); overload;
var
  I, J: Integer;
  ATemp: Pointer;
begin;
  I := 0;
  J := ALast;
  if J > dxInsertionSortLast then
    J := dxInsertionSortLast;
  repeat
    if ACompareFunc(ASortList[J], ASortList[I]) < 0 then
      I := J;
    Dec(J);
  until J <= 0;
  if I > 0 then
  begin;
    ATemp := ASortList[0];
    ASortList[0] := ASortList[I];
    ASortList[I] := ATemp;
  end;
  J := 1;
  while True do
  begin
    if J >= ALast then
      Break;
    Inc(J);
    if ACompareFunc(ASortList[J], ASortList[J - 1]) < 0 then
    begin;
      ATemp := ASortList[J];
      I := J;
      repeat
        ASortList[I] := ASortList[I - 1];
        Dec(I);
      until not (ACompareFunc(ATemp, ASortList[I - 1]) < 0);
      ASortList[I] := ATemp;
    end;
  end;
end;

procedure ModifiedQuickSort(ASortList: PdxPointerList; ARight: Integer; ACompareFunc: TListSortCompare); overload;
var
  I, J: Integer;
  APivot, ATemp: Pointer;
begin;
  while True do
  begin;
    J := ARight;
    if J <= dxInsertionSortLast then
      Break;
    I := J shr 1;
    APivot := ASortList[I];
    if ACompareFunc(ASortList[J], ASortList[0]) < 0 then
    begin;
      ATemp := ASortList[0];
      ASortList[0] := ASortList[J];
      ASortList[J] := ATemp;
    end;
    if ACompareFunc(APivot, ASortList[0]) < 0 then
    begin;
      APivot := ASortList[0];
      ASortList[0] := ASortList[I];
      ASortList[I] := APivot;
    end
    else if ACompareFunc(ASortList[J], APivot) < 0 then
    begin;
      APivot := ASortList[J];
      ASortList[J] := ASortList[I];
      ASortList[I] := APivot;
    end;
    I := 0;
    repeat
      Inc(I);
    until not (ACompareFunc(ASortList[I], APivot) < 0);
    repeat
      Dec(J);
    until not (ACompareFunc(APivot, ASortList[J]) < 0);
    if I < J then
      repeat
        ATemp := ASortList[I];
        ASortList[I] := ASortList[J];
        ASortList[J] := ATemp;
        repeat
          Inc(I);
        until not (ACompareFunc(ASortList[I], APivot) < 0);
        repeat
          Dec(J);
        until not (ACompareFunc(APivot, ASortList[J]) < 0);
      until I >= J;
    Dec(I);
    Inc(J);
    if I + J <= ARight then
    begin;
      if I > dxInsertionSortLast then
        ModifiedQuickSort(ASortList, I, ACompareFunc);
      ARight := ARight - J;
      Pointer(ASortList) := Pointer(@ASortList[J]);
    end
    else
    begin;
      if J + dxInsertionSortLast < ARight then
        ModifiedQuickSort(Pointer(@ASortList[J]), ARight - J, ACompareFunc);
      ARight := I;
    end;
  end;
end;

procedure dxQuickSortList(AList: TList; ACompareFunc: TListSortCompare;
  AMultiThreaded: Boolean = False);
begin;
  dxQuickSortList(Pointer(AList.List), AList.Count, ACompareFunc, AMultiThreaded);
end;

procedure dxQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TListSortCompare; AMultiThreaded: Boolean = False);
begin;
  if (ASortList <> nil) and (ACount > 1) then
{$IFDEF MSWINDOWS}
    if AMultiThreaded and dxCanUseMultiThreading and (ACount > dxMaxUserWorkItems) then
      dxSortHelper.Sort(ASortList, ACount, ACompareFunc)
    else
{$ENDIF}
      dxInternalQuickSortList(ASortList, ACount, ACompareFunc);
end;

procedure dxInternalQuickSortList(ASortList: PdxPointerList; ACount: Integer;
  ACompareFunc: TListSortCompare);
begin;
  if (ASortList <> nil) and (ACount > 1) then
  begin;
    Dec(ACount);
    if ACount > dxInsertionSortLast then
      ModifiedQuickSort(ASortList, ACount, ACompareFunc);
    InsertionSort(ASortList, ACount, ACompareFunc);
  end;
end;

{ TcxIUnknownObject }

function TcxIUnknownObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TcxIUnknownObject._AddRef: Integer;
begin
  Result := -1;
end;

function TcxIUnknownObject._Release: Integer;
begin
  Result := -1;
end;

{ TcxInterfacedPersistent }

constructor TcxInterfacedPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxInterfacedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TcxOwnedPersistent }

constructor TcxOwnedPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TcxOwnedPersistent.Assign(Source: TPersistent);
begin
  if Source is TcxOwnedPersistent then
    DoAssign(Source)
  else
    inherited;
end;

procedure TcxOwnedPersistent.DoAssign(Source: TPersistent);
begin

end;

function TcxOwnedPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TcxOwnedInterfacedPersistent }

function TcxOwnedInterfacedPersistent._AddRef: Integer;
begin
  Result := -1;
end;

function TcxOwnedInterfacedPersistent._Release: Integer;
begin
  Result := -1;
end;

function TcxOwnedInterfacedPersistent.QueryInterface(
  const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := cxE_NOINTERFACE;
end;

{ TcxLockablePersistent }

procedure TcxLockablePersistent.Assign(Source: TPersistent);
begin
  BeginUpdate;
  try
    inherited;
  finally
    EndUpdate;
  end;
end;

procedure TcxLockablePersistent.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxLockablePersistent.CancelUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    FHasChanges := False;
end;

procedure TcxLockablePersistent.Changed;
begin
  if FLockCount = 0 then
  begin
    FHasChanges := False;
    DoChanged;
  end
  else
    FHasChanges := True;
end;

procedure TcxLockablePersistent.EndUpdate;
begin
  Dec(FLockCount);
  if (FLockCount = 0) and FHasChanges then
    Changed;
end;

{ TcxFreeNotificator }

procedure TcxFreeNotificator.AddSender(ASender: TComponent);
begin
  if ASender <> nil then
    ASender.FreeNotification(Self);
end;

procedure TcxFreeNotificator.RemoveSender(ASender: TComponent);
begin
  if ASender <> nil then
    ASender.RemoveFreeNotification(Self);
end;

procedure TcxFreeNotificator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and Assigned(FOnFreeNotification) then
    FOnFreeNotification(AComponent);
end;

{ TcxCollection }

procedure TcxCollection.Assign(Source: TPersistent);
var
  I: Integer;
  AItem: TCollectionItem;
begin
  if Source is TCollection then
  begin
    if (Count = 0) and (TCollection(Source).Count = 0) then Exit;
    BeginUpdate;
    try
      for I := 0 to TCollection(Source).Count - 1 do
      begin
        if I > Count - 1 then
          AItem := Add
        else
          AItem := Items[I];
        AItem.Assign(TCollection(Source).Items[I]);
      end;
      for I := Count - 1 downto TCollection(Source).Count do
        Delete(I);
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

{ TcxDoublyLinkedObject }

procedure dxLiaisonSort(AList: TList; ACompareProc: TListSortCompare);
var
  ADataList: TcxDoublyLinkedDataList;
begin
  ADataList := TcxDoublyLinkedDataList.Create;
  try
    ADataList.PopulateFromList(AList);
    if not Assigned(ACompareProc) then
      ADataList.Sort(dxCompareValues)
    else
      ADataList.Sort(ACompareProc);
    ADataList.PopulateToList(AList);
  finally
    ADataList.Free;
  end;
end;

{ TcxDoublyLinkedObjectList }

destructor TcxDoublyLinkedObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TcxDoublyLinkedObjectList.Add: TcxDoublyLinkedObject;
begin
  Result := CreateLinkedObject;
  DoAdd(Result);
end;

procedure TcxDoublyLinkedObjectList.Clear;
var
  ACurrent: TcxDoublyLinkedObject;
begin
  while FLast <> nil do
  begin
    ACurrent := FLast;
    FLast := ACurrent.Prev;
    ACurrent.Free;
  end;
  FFirst := nil;
end;

function TcxDoublyLinkedObjectList.GetLinkedObjectClass: TcxDoublyLinkedObjectClass;
begin
  Result := TcxDoublyLinkedObject;
end;

function TcxDoublyLinkedObjectList.CreateLinkedObject: TcxDoublyLinkedObject;
begin
  Result := GetLinkedObjectClass.Create;
end;

procedure TcxDoublyLinkedObjectList.DoAdd(ASource: TcxDoublyLinkedObject);
begin
  ASource.Prev := FLast;
  if FFirst = nil then
    FFirst := ASource;
  if FLast <> nil then
    FLast.Next := ASource;
  FLast := ASource;
end;

procedure TcxDoublyLinkedObjectList.Remove(ALinkedObject: TcxDoublyLinkedObject);
begin
  Delete(ALinkedObject);
end;

procedure TcxDoublyLinkedObjectList.Delete(ALinkedObject: TcxDoublyLinkedObject);
begin
  Extract(ALinkedObject);
  ALinkedObject.Free;
end;

procedure TcxDoublyLinkedObjectList.Extract(ALinkedObject: TcxDoublyLinkedObject);


begin
  if ALinkedObject.Next <> nil then
    ALinkedObject.Next.Prev := ALinkedObject.Prev
  else
    FLast := ALinkedObject.Prev;

  if ALinkedObject.Prev <> nil then
    ALinkedObject.Prev.FNext := ALinkedObject.Next
  else
    FFirst := ALinkedObject.Next;
end;

procedure TcxDoublyLinkedObjectList.InsertBefore(ADestination, ASource: TcxDoublyLinkedObject);
begin
  ASource.Prev := ADestination.Prev;
  if ADestination.Prev <> nil then
    ADestination.Prev.Next := ASource
  else
    FFirst := ASource;
  ADestination.Prev := ASource;
  ASource.Next := ADestination;
end;

procedure TcxDoublyLinkedObjectList.InsertAfter(ADestination, ASource: TcxDoublyLinkedObject);
begin
  ASource.Next := ADestination.Next;
  if ADestination.Next <> nil then
    ADestination.Next.Prev := ASource
  else
    FLast := ASource;
  ADestination.Next := ASource;
  ASource.Prev := ADestination;
end;

function TcxDoublyLinkedObjectList.GetCount: Integer;
var
  AItem: TcxDoublyLinkedObject;
begin
  Result := 0;
  AItem := Last;
  while AItem <> nil do
  begin
    Inc(Result);
    AItem := AItem.Prev;
  end;
end;

{ TcxDoublyLinkedDataList }

function TcxDoublyLinkedDataList.Add(AData: Pointer): TcxDoublyLinkedData;
begin
  Result := TcxDoublyLinkedData(inherited Add);
  Result.Data := AData;
end;

function TcxDoublyLinkedDataList.GetLinkedObjectClass: TcxDoublyLinkedObjectClass;
begin
  Result := TcxDoublyLinkedData;
end;

procedure TcxDoublyLinkedDataList.PopulateFromList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    Add(AList.List[I]);
end;

procedure TcxDoublyLinkedDataList.PopulateToList(AList: TList);
var
  I, ACount: Integer;
  AItem: TcxDoublyLinkedData;
begin
  ACount := Count;
  AList.Capacity := ACount;
  AItem := TcxDoublyLinkedData(Last);
  for I := ACount - 1 downto 0 do
  begin
    AList.List[I] := AItem.Data;
    AItem := TcxDoublyLinkedData(AItem.Prev);
  end;
end;

procedure TcxDoublyLinkedDataList.Sort(ACompareProc: TListSortCompare);

{
  procedure CompareAndMove(var S, A, B: TcxDoublyLinkedData); inline;
  begin
    if ACompareProc(A, B) > 0 then
    begin // B wins
      if B.Prev <> S then
      begin
        S.Next := B;
        B.Prev := S;
      end;
      S := B;
      B := TcxDoublyLinkedData(S.Next);
    end
    else
    begin
      if A.Prev <> S then
      begin
        S.Next := A;
        A.Prev := S;
      end;
      S := A;
      A := TcxDoublyLinkedData(S.Next);
    end;
  end;
  }

  function Glue(A: TcxDoublyLinkedData): TcxDoublyLinkedData;
  var
    B, ANextSorted, S: TcxDoublyLinkedData;
    ALastA: TcxDoublyLinkedObject;
  begin
    B := A.FNextSorted;
    if B <> nil then
    begin
      ANextSorted := B.FNextSorted;
      Result := ANextSorted;

      ALastA := B.Prev;
      ALastA.Next := ANextSorted;

      if ACompareProc(A.Data, B.Data) > 0 then
      begin  // B wins
        if A.FPrevSorted <> nil then
          A.FPrevSorted.FNextSorted := B;
        B.FPrevSorted := A.FPrevSorted;

        S := TcxDoublyLinkedData(A.Prev);
        if S <> nil then
          S.Next := B;
        B.Prev := S;
        S := B;
        B := TcxDoublyLinkedData(S.Next);
      end
      else
      begin
        if ANextSorted <> nil then
          ANextSorted.FPrevSorted := A;
        A.FNextSorted := ANextSorted;

        S := A;
        A := TcxDoublyLinkedData(S.Next);
      end;

      while (A <> ANextSorted) and (B <> ANextSorted) do
      begin
      //       CompareAndMove(S, A, B);

        if ACompareProc(A.Data, B.Data) > 0 then
        begin // B wins
          if B.Prev <> S then
          begin
            S.Next := B;
            B.Prev := S;
          end;
          S := B;
          B := TcxDoublyLinkedData(S.Next);
        end
        else
        begin
          if A.Prev <> S then
          begin
            S.Next := A;
            A.Prev := S;
          end;
          S := A;
          A := TcxDoublyLinkedData(S.Next);
        end;
      end;

      if A = ALastA.Next then
      begin
        S.Next := B;
        B.Prev := S;
      end
      else
      begin
        S.Next := A;
        A.Prev := S;
        if ANextSorted <> nil then
          ANextSorted.Prev := ALastA;
      end;
    end
    else
      Result := nil;
  end;

  procedure ChangeDirection(AFirst, ALast: TcxDoublyLinkedObject);
  var
    ALastNext, AFirstPrev, AItem1, AItem2, AItem3: TcxDoublyLinkedObject;
  begin
    AFirstPrev := AFirst.Prev;
    ALastNext := ALast.Next;

    if AFirstPrev <> nil then
      AFirstPrev.Next := ALast;
    if ALastNext <> nil then
      ALastNext.Prev := AFirst;

    AItem2 := ALast;
    Aitem1 := ALast.Prev;

    repeat
      AItem3 := AItem2;
      AItem2 := AItem1;
      AItem1 := AItem1.Prev;

      AItem3.Next := AItem2;
      AItem2.Prev := AItem3;
    until AItem2 = AFirst;

    AFirst.Next := ALastNext;
    ALast.Prev := AFirstPrev;
  end;

var
  AItem, AFirst, AFirstInQueue, ANextSorted: TcxDoublyLinkedData;
  ADirection, ACompare: Integer;
begin
  AItem := TcxDoublyLinkedData(Last);
  if AItem <> nil then
  begin
    ANextSorted := nil;
    ADirection := 0;
    while AItem <> nil do
    begin
      if AItem.Prev <> nil then
        ACompare := ACompareProc(TcxDoublyLinkedData(AItem.Prev).Data, AItem.Data)
      else
        ACompare := 0;

      if (AItem.Prev <> nil) and
        ((ADirection < 0) and (ACompare < 0) or (ADirection > 0) and (ACompare > 0) or
         (ACompare = 0) or (ADirection = 0)) then
      begin
        if ADirection = 0 then
          ADirection := ACompare;
      end
      else
      begin
        if ADirection > 0 then
        begin
          if ANextSorted <> nil then
            AFirstInQueue := TcxDoublyLinkedData(ANextSorted.Prev)
          else
            AFirstInQueue := TcxDoublyLinkedData(Last);

          ChangeDirection(AItem, AFirstInQueue);
          AItem := AFirstInQueue;
        end;
        ADirection := 0;

        if ANextSorted <> nil then
          ANextSorted.FPrevSorted := AItem;
        AItem.FNextSorted := ANextSorted;
        ANextSorted := AItem;
      end;
      AItem := TcxDoublyLinkedData(AItem.Prev);
    end;

    if ANextSorted <> nil then
    begin
      AFirst := ANextSorted;
      repeat
        AItem := AFirst;
        repeat
          AItem := Glue(AItem);
        until AItem = nil;
        if (AFirst.FNextSorted <> nil) and (AFirst.FNextSorted.FPrevSorted = nil) then
          AFirst := AFirst.FNextSorted;
      until AFirst.FNextSorted = nil;

      while Last.Next <> nil do
        FLast := Last.Next;
    end;
  end;
end;


function TcxDoublyLinkedDataIndexedList.Add(AData: Pointer; AIndex: Integer): TcxDoublyLinkedIndexedData;
begin
  Result := TcxDoublyLinkedIndexedData(inherited Add(AData));
  Result.Index := AIndex;
end;

function TcxDoublyLinkedDataIndexedList.GetLinkedObjectClass: TcxDoublyLinkedObjectClass;
begin
  Result := TcxDoublyLinkedIndexedData;
end;

procedure TcxDoublyLinkedDataIndexedList.PopulateFromList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    Add(AList.List[I], I);
end;


{ TdxFastList }

constructor TdxFastList.Create(ACapacity: Integer = 0);
begin
  inherited Create;
  Capacity := ACapacity;
end;

destructor TdxFastList.Destroy;
begin
  Clear;
  Capacity := 0;
  inherited Destroy;
end;

function TdxFastList.Add(AItem: Pointer): Integer;
begin
  Result := Count;
  if Result = Capacity then
    Grow;
  FList[Result] := AItem;
  Inc(FCount);
end;

procedure TdxFastList.Assign(ASource: TdxFastList);
begin
  FCount := ASource.Count;
  Capacity := ASource.Capacity;
  if Count > 0 then
    System.Move(ASource.List[0], List[0], ASource.Count * SizeOf(Pointer));
end;

procedure TdxFastList.Assign(ASource: TList);
begin
  FCount := ASource.Count;
  Capacity := ASource.Capacity;
  if Count > 0 then
    System.Move(ASource.List[0], List[0], ASource.Count * SizeOf(Pointer));
end;

procedure TdxFastList.Assign(ASource: TdxFastList; AOperator: TListAssignOp);
var
  I: Integer;
  ATemp: TdxFastList;
begin
  case AOperator of
    laCopy:
      Assign(ASource);
    laAnd:
      for I := Count - 1 downto 0 do
        if ASource.IndexOf(Items[I]) = -1 then
          Delete(I);
    laOr:
      for I := 0 to ASource.Count - 1 do
        if IndexOf(ASource.Items[I]) = -1 then
          Add(ASource.Items[I]);
    laXor:
      begin
        ATemp := TdxFastList.Create;
        try
          ATemp.Capacity := Count + ASource.Count;
          for I := 0 to ASource.Count - 1 do
            if IndexOf(ASource[I]) = -1 then
              ATemp.Add(ASource[I]);
          for I := Count - 1 downto 0 do
            if ASource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := Count;
          Count := Count + ATemp.Count;
          if ATemp.Count > 0 then
            System.Move(ATemp.List[0], List[I], ATemp.Count * SizeOf(Pointer));
        finally
          ATemp.Free;
        end;
      end;
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if ASource.IndexOf(Items[I]) <> -1 then
          Delete(I);
    laDestUnique:
      begin
        ATemp := TdxFastList.Create;
        try
          ATemp.Capacity := ASource.Count;
          for I := ASource.Count - 1 downto 0 do
            if IndexOf(ASource[I]) = -1 then
              ATemp.Add(ASource[I]);
          Assign(ATemp);
        finally
          ATemp.Free;
        end;
      end;
  end;
end;

procedure TdxFastList.Clear;
begin
  FCount := 0;
  Capacity := 0;
end;

procedure TdxFastList.Delete(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);
  DoDelete(AIndex);
end;

procedure TdxFastList.DeleteRange(AIndex, ACount: Integer);
begin
  CheckDeleteRange(AIndex, ACount);
  DoDeleteRange(AIndex, ACount);
end;

function TdxFastList.Empty: Boolean;
begin
  Result := (Self = nil) or (Count = 0);
end;

procedure TdxFastList.Exchange(AIndex1, AIndex2: Integer);
var
  ATempItem: Pointer;
begin
  if (AIndex1 < 0) or (AIndex1 >= Count) then
    TList.Error(@SListIndexError, AIndex1);
  if (AIndex2 < 0) or (AIndex2 >= Count) then
    TList.Error(@SListIndexError, AIndex2);
  ATempItem := List[AIndex1];
  List[AIndex1] := List[AIndex2];
  List[AIndex2] := ATempItem;
end;

function TdxFastList.Expand: TdxFastList;
begin
  if Count = Capacity then
    Grow;
  Result := Self;
end;

function TdxFastList.First: Pointer;
begin
  Result := GetItem(0);
end;

function TdxFastList.GetTList: TList;
begin
  Result := TList.Create;
  if Count > 0 then
  begin
    Result.Count := Count;
    System.Move(List[0], Result.List[0], Count * SizeOf(Pointer));
  end;
end;

procedure TdxFastList.CheckDeleteRange(AIndex, ACount: Integer);
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > Count) or (AIndex + ACount < 0) then
    TList.Error(@SArgumentOutOfRange, AIndex);
end;

procedure TdxFastList.DoDeleteRange(AIndex, ACount: Integer);
var
  ANextIndex: Integer;
begin
  if ACount = 0 then
    Exit;

  ANextIndex := AIndex + ACount;
  if ANextIndex < FCount then
    System.Move(FList[ANextIndex], FList[AIndex], (FCount - ANextIndex) * SizeOf(Pointer));

  Dec(FCount, ACount);
end;

procedure TdxFastList.DoDelete(AIndex: Integer);
begin
  Dec(FCount);
  if AIndex < FCount then
    System.Move(FList[AIndex + 1], FList[AIndex], (FCount - AIndex) * SizeOf(Pointer));
end;

function TdxFastList.GetItem(AIndex: Integer): Pointer;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);
  Result := List[AIndex];
end;

procedure TdxFastList.Grow;
var
  ADelta: Integer;
begin
  if Capacity > 128 then
    ADelta := Capacity shr 1
  else
    if Capacity > 16 then
      ADelta := 32
    else
      ADelta := 16;
  SetCapacity(Capacity + ADelta);
end;

function TdxFastList.IndexOf(AItem: Pointer; ADirection: TdxListDirection = ldFromBeginning): Integer;
var
  ARegCount: Integer;
  ARegList: PdxPointerList;
begin
  ARegCount := Count;
  ARegList  := List;
  if ADirection = ldFromBeginning then
  begin
    for Result := 0 to ARegCount - 1 do
      if AItem = ARegList[Result]  then
        Exit;
  end
  else
    for Result := ARegCount - 1 downto 0 do
      if AItem = ARegList[Result]  then
        Exit;
  Result := -1;
end;

procedure TdxFastList.Insert(AIndex: Integer; AItem: Pointer);
begin
  if (AIndex < 0) or (AIndex > Count) then
    TList.Error(@SListIndexError, AIndex);
  if Count = Capacity then
    Grow;
  if AIndex < Count then
    System.Move(List[AIndex], List[AIndex + 1], (Count - AIndex) * SizeOf(Pointer));
  List[AIndex] := AItem;
  Inc(FCount);
end;

procedure TdxFastList.Insert(AIndex: Integer; AList: TdxFastList);
var
  ANewCount: Integer;
begin
  if (AIndex < 0) or (AIndex > Count) then
    TList.Error(@SListIndexError, AIndex);
  if AList.Count = 0 then
    Exit;
  ANewCount := Count + AList.Count;
  if ANewCount > Capacity then
    SetCapacity(ANewCount);
  if AIndex < Count then
    System.Move(List[AIndex], List[AIndex + AList.Count], (Count - AIndex) * SizeOf(Pointer));
  System.Move(AList.List[0], List[AIndex], AList.Count * SizeOf(Pointer));
  FCount := ANewCount;
end;

function TdxFastList.Last: Pointer;
begin
  Result := GetItem(Count - 1);
end;

procedure TdxFastList.Move(ACurrentIndex, ANewIndex: Integer);
var
  AMovedItem: Pointer;
begin
  if ACurrentIndex <> ANewIndex then
  begin
    if (ANewIndex < 0) or (ANewIndex >= Count) then
      TList.Error(@SListIndexError, ANewIndex);
    AMovedItem := GetItem(ACurrentIndex);
    DoDelete(ACurrentIndex);
    Insert(ANewIndex, AMovedItem);
  end;
end;

procedure TdxFastList.Pack(APattern: Pointer = nil);
var
  AStartIndex, AEndIndex, ANewCount: Integer;
begin
  if Count = 0 then
    Exit;
  ANewCount := 0;
  AStartIndex := 0;
  repeat
    while (AStartIndex < Count) and (List[AStartIndex] = APattern) do
      Inc(AStartIndex);
    if AStartIndex < Count then
    begin
      AEndIndex := AStartIndex;
      while (AEndIndex < Count) and (List[AEndIndex] <> APattern) do
        Inc(AEndIndex);
      Dec(AEndIndex);
      if AStartIndex > ANewCount then
        System.Move(List[AStartIndex], List[ANewCount],
          (AEndIndex - AStartIndex + 1) * SizeOf(Pointer));
      Inc(ANewCount, AEndIndex - AStartIndex + 1);
      AStartIndex := AEndIndex + 1;
    end;
  until AStartIndex >= Count;
  FCount := ANewCount;
end;

function TdxFastList.Remove(AItem: Pointer): Integer;
begin
  Result := IndexOf(AItem);
  if Result >= 0 then
    DoDelete(Result);
end;

procedure TdxFastList.Reverse;
var
  ATempItem: Pointer;
  ALeft, ARight: PPointer;
begin
  if Count < 2 then
    Exit;
  ALeft  := @List[0];
  ARight := ALeft;
  Inc(ARight, Count - 1);
  while TdxNativeUInt(ARight) > TdxNativeUInt(ALeft) do
  begin
    ATempItem := ALeft^;
    ALeft^ := ARight^;
    ARight^ := ATempItem;
    Inc(ALeft);
    Dec(ARight);
  end;
end;

procedure TdxFastList.SetCapacity(AValue: Integer);
begin
  if (AValue < Count) {$IFNDEF DELPHI16} or (AValue > MaxListSize) {$ENDIF} then
    TList.Error(@SListCapacityError, AValue);
  if AValue <> Capacity then
  begin
    ReallocMem(FList, AValue * SizeOf(Pointer));
    FCapacity := AValue;
  end;
end;

procedure TdxFastList.SetCount(AValue: Integer);
begin
  if (AValue < 0) {$IFNDEF DELPHI16} or (AValue > MaxListSize) {$ENDIF} then
    TList.Error(@SListCountError, AValue);
  if AValue > Capacity then
    SetCapacity(AValue);
  if AValue > Count then
    FillChar(List[Count], (AValue - Count) * SizeOf(Pointer), 0);
  FCount := AValue;
end;

procedure TdxFastList.SetItem(AIndex: Integer; AItem: Pointer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);
  List[AIndex] := AItem;
end;

procedure TdxFastList.Sort(ACompare: TCompareItems; AMultiThreaded: Boolean = False);
begin
  dxQuickSortList(List, Count, ACompare, AMultiThreaded);
end;

procedure TdxFastList.Sort(ACompare: TListSortCompare; AMultiThreaded: Boolean = False);
begin
  dxQuickSortList(List, Count, ACompare, AMultiThreaded);
end;

function TdxFastList.Extract(AItem: Pointer; ADirection: TdxListDirection = ldFromBeginning): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(AItem, ADirection);
  if I >= 0 then
  begin
    Result := AItem;
    DoDelete(I);
  end;
end;

function TdxFastList.ExtractByIndex(AIndex: Integer): Pointer;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);
  Result := FList[AIndex];
  DoDelete(AIndex);
end;

{ TdxFastObjectList }

constructor TdxFastObjectList.Create(AOwnsObjects: Boolean = True; ACapacity: Integer = 0);
begin
  inherited Create(ACapacity);
  FOwnsObjects := AOwnsObjects;
end;

function TdxFastObjectList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
end;

procedure TdxFastObjectList.Clear;
var
  I: Integer;
begin
  if OwnsObjects then
    for I := 0 to Count - 1 do
      TObject(List[I]).Free;
  inherited Clear;
end;

procedure TdxFastObjectList.Delete(AIndex: Integer);
begin
  if OwnsObjects then
    GetItem(AIndex).Free;
  inherited Delete(AIndex);
end;

procedure TdxFastObjectList.DeleteRange(AIndex, ACount: Integer);
var
  I: Integer;
begin
  CheckDeleteRange(AIndex, ACount);
  if OwnsObjects then
    for I := AIndex to AIndex + ACount - 1 do
      GetItem(I).Free;
  DoDeleteRange(AIndex, ACount);
end;

function TdxFastObjectList.Extract(AItem: TObject): TObject;
begin
  Result := inherited Extract(AItem);
end;

procedure TdxFastObjectList.ExtractRange(AIndex, ACount: Integer);
begin
  CheckDeleteRange(AIndex, ACount);
  DoDeleteRange(AIndex, ACount);
end;

function TdxFastObjectList.Remove(AObject: TObject): Integer;
begin
  Result := IndexOf(AObject);
  if Result >= 0 then
    Delete(Result);
end;

function TdxFastObjectList.IndexOf(AObject: TObject;
  ADirection: TdxListDirection = ldFromBeginning): Integer;
begin
  Result := inherited IndexOf(AObject, ADirection);
end;

procedure TdxFastObjectList.Insert(AIndex: Integer; AObject: TObject);
begin
  inherited Insert(AIndex, AObject);
end;

function TdxFastObjectList.First: TObject;
begin
  Result := inherited First;
end;

function TdxFastObjectList.Last: TObject;
begin
  Result := inherited Last;
end;

procedure TdxFastObjectList.SetCount(AValue: Integer);
var
  I: Integer;
begin
  if (AValue < 0) {$IFNDEF DELPHI16} or (AValue > MaxListSize) {$ENDIF} then
    TList.Error(@SListCountError, AValue);
  if AValue > Capacity then
    SetCapacity(AValue);
  if AValue > Count then
    FillChar(List[Count], (AValue - Count) * SizeOf(Pointer), 0)
  else
    if (AValue < Count) and OwnsObjects then
      for I := AValue to Count - 1 do
        TObject(List[I]).Free;
  FCount := AValue;
end;

function TdxFastObjectList.GetItem(AIndex: Integer): TObject;
begin
  Result := inherited GetItem(AIndex);
end;

procedure TdxFastObjectList.SetItem(AIndex: Integer; AObject: TObject);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);
  if AObject <> List[AIndex] then
  begin
    if OwnsObjects then
      TObject(List[AIndex]).Free;
    List[AIndex] := AObject;
  end;
end;

{ TdxEventHandlerInstance }

class procedure TdxEventHandlerInstance.NextParameter(var P: PByte);
begin
  Inc(P, 1);
  Inc(P, P[0] + 1);
  Inc(P, P[0] + 1);
end;

constructor TdxEventHandlerInstance.Create(const ADynamicInvokeEvent: TDynamicInvokeEvent; ATypeData: PTypeData);
{$IFNDEF CPUX64}
var
  P: PByte;
  ACurReg: Integer;
  I: Integer;
  Size: Integer;
begin
  FParamInfos := GetMethodParametersTypeInfo(ATypeData);

  // Calculate stack size
  ACurReg := paEDX;
  P := @ATypeData^.ParamList;
  FStackSize := 0;
  for I := 0 to ATypeData^.ParamCount - 1 do
  begin
    if TypInfo.TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] <> [] then
      Size := 4
    else
      Size := GetTypeSize(FParamInfos^[I]^);
    if (Size <= 4) and (ACurReg <= paECX) then
      Inc(ACurReg)
    else
    begin
      if Size < 4 then
        Size := 4;
      Inc(FStackSize, Size);
    end;
    NextParameter(P);
  end;
  FDynamicInvokeEvent := ADynamicInvokeEvent;
end; // of CPUX86 implementation
{$ELSE}
var
  P: PByte;
  I: Integer;
begin
  FRegisterFlag := $00;
  FParamInfos := GetMethodParametersTypeInfo(ATypeData);

  P := @ATypeData^.ParamList;
  FStackSize := 8;
  for I := 0 to ATypeData^.ParamCount - 1 do
  begin
    if I < 3 then
    begin
      if (System.TypInfo.TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] = [])
          and (FParamInfos^[I]^.Kind = tkFloat) then
        FRegisterFlag := FRegisterFlag or (1 shl (I + 1));
    end;
    Inc(FStackSize, 8);
    NextParameter(P);
  end;
  if FStackSize < 32 then
    FStackSize := 32;
  FDynamicInvokeEvent := ADynamicInvokeEvent;
end;
{$ENDIF}

class function TdxEventHandlerInstance.CreateMethodPointer(const ADynamicInvokeEvent: TDynamicInvokeEvent; ATypeData: PTypeData): TMethod;
begin
  TObject(Result.Data) := TdxEventHandlerInstance.Create(ADynamicInvokeEvent, ATypeData);
  Result.Code := @TdxEventHandlerInstance.RegisterStub;
end;

class function TdxEventHandlerInstance.GetMethodAdditionalInfo(ATypeData: PTypeData): Pointer;
var
  P: PByte;
  I: Integer;
begin
  P := @ATypeData^.ParamList;

  for I := 1 to ATypeData^.ParamCount do
    NextParameter(P);
  if ATypeData^.MethodKind = mkFunction then
  begin
    Inc(P, P[0] + 1);        // skip ResultType name
    Inc(P, SizeOf(Pointer)); // skip ResultTypeRef
  end;
  Result := P;
end;

class function TdxEventHandlerInstance.GetMethodCallingConvention(ATypeData: PTypeData): TCallConv;
begin
  Result := TCallConv(PByte(GetMethodAdditionalInfo(ATypeData))^);
end;

class function TdxEventHandlerInstance.GetMethodParametersTypeInfo(ATypeData: PTypeData): Pointer;
var
  P: PByte;
begin
  P := GetMethodAdditionalInfo(ATypeData);
  Inc(P);
  Result := P;
end;

{$IFNDEF CPUX64}
function TdxEventHandlerInstance.GetTypeSize(ATypeInfo: PTypeInfo): Integer;
var
  ATypeData: PTypeData;
begin
  case ATypeInfo^.Kind of
    tkChar:
      Result := 1;
    tkWChar:
      Result := 2;
    tkInteger, tkEnumeration:
      begin
        ATypeData := GetTypeData(ATypeInfo);
        if ATypeData^.MinValue >= 0 then
          if Cardinal(ATypeData^.MaxValue) > $FFFF then
            Result := 4
          else if ATypeData^.MaxValue > $FF then
            Result := 2
          else
            Result := 1
        else
          if (ATypeData^.MaxValue > $7FFF) or (ATypeData^.MinValue < -$7FFF - 1) then
            Result := 4
          else if (ATypeData^.MaxValue > $7F) or (ATypeData^.MinValue < -$7F - 1) then
            Result := 2
          else
            Result := 1;
      end;
    tkFloat:
      begin
        ATypeData := GetTypeData(ATypeInfo);
        case ATypeData^.FloatType of
          ftSingle: Result := 4;
          ftComp, ftCurr, ftDouble: Result := 8;
          ftExtended:
            Result := (SizeOf(Extended) + SizeOf(Pointer) - 1) and not (SizeOf(Pointer) - 1);
        else
          Result := -1;
        end;
      end;
    tkString, tkLString, tkUString, tkWString, tkInterface, tkClass:
      Result := SizeOf(Pointer);
    tkMethod:
      Result := SizeOf(TMethod);
    tkInt64:
      Result := 8;
    tkVariant:

{$IFDEF CPUX64}
      Result := 24;
{$ELSE !CPUX64}
      Result := 16;
{$ENDIF !CPUX64}
    tkSet:
      begin
        Result := 4;
      end;

  else
    Assert(False);
    Result := -1;
  end;
end;
{$ENDIF}

procedure TdxEventHandlerInstance.InternalHandler(AParams: Pointer);
{$IFNDEF CPUX64}
asm
        MOV     ECX,[EAX]
        JMP     DWORD PTR [ECX] + VMTOFFSET TdxEventHandlerInstance.Handler
end;
{$ELSE}
asm
        MOV     RAX,[RAX]
        JMP     QWORD PTR [RAX] + VMTOFFSET TdxEventHandlerInstance.Handler
end;
{$ENDIF}


procedure TdxEventHandlerInstance.RegisterStub;
{$IFNDEF CPUX64}
const
  PtrSize = SizeOf(Pointer);
asm
        PUSH    EAX
        PUSH    ECX
        PUSH    EDX
        MOV     EDX,ESP
        CALL    InternalHandler
        MOV     [ESP+4],EAX
        POP     EAX
        POP     EAX
        POP     ECX             // Self
        MOV     ECX,[ECX].TdxEventHandlerInstance.FStackSize

        TEST    ECX,ECX
        JZ      @@SimpleRet

        ADD     ECX, PtrSize - 1
        AND     ECX, NOT (PtrSize - 1)
        AND     ECX, $FFFF

        PUSH    EAX                         // we need this register, so save it
        MOV     EAX,[ESP + 4]               // Load the return address
        MOV     [ESP + ECX + 4], EAX        // Just blast it over the first param on the stack
        POP     EAX
        ADD     ESP,ECX                     // This will move the stack back to where the moved
                                            // return address is now located. The next RET
                                            // instruction will do the final stack cleanup
@@SimpleRet:
end;
{$ELSE}
asm
        MOV     AX, WORD PTR [RCX].TdxEventHandlerInstance.FRegisterFlag
@@FIRST:
        TEST    AX, $01
        JZ      @@SAVE_RCX
@@SAVE_XMM0:
        MOVSD   QWORD PTR [RSP+$08], XMM0
        JMP     @@SECOND
@@SAVE_RCX:
        MOV     QWORD PTR [RSP+$08], RCX

@@SECOND:
        TEST    AX, $02
        JZ      @@SAVE_RDX
@@SAVE_XMM1:
        MOVSD   QWORD PTR [RSP+$10], XMM1
        JMP     @@THIRD
@@SAVE_RDX:
        MOV     QWORD PTR [RSP+$10], RDX

@@THIRD:
        TEST    AX, $04
        JZ      @@SAVE_R8
@@SAVE_XMM2:
        MOVSD   QWORD PTR [RSP+$18], XMM2
        JMP     @@FORTH
@@SAVE_R8:
        MOV     QWORD PTR [RSP+$18], R8

@@FORTH:
        TEST    AX, $08
        JZ      @@SAVE_R9
@@SAVE_XMM3:
        MOVSD   QWORD PTR [RSP+$20], XMM3
        JMP     @@1
@@SAVE_R9:
        MOV     QWORD PTR [RSP+$20], R9

@@1:    LEA     RDX, QWORD PTR [RSP+$08]
        MOV     RAX, RCX
        SUB     RSP, $28
        CALL    InternalHandler
        ADD     RSP, $28
end;
{$ENDIF}

procedure TdxEventHandlerInstance.Handler(AParams: Pointer);
begin
  if Assigned(FDynamicInvokeEvent) then
    FDynamicInvokeEvent(AParams, FStackSize);
end;

{ TdxMulticastImplementator }

constructor TdxMulticastImplementator.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TdxMulticastImplementator.Destroy;
begin
  InternalClear;
  ReleaseMethodPointer(FInternalDispatcher);
  FLock.Free;
  inherited Destroy;
end;

procedure TdxMulticastImplementator.InternalSetDispatcher(var ADispatcher; ATypeData: PTypeData);
var
  AMethod: TMethod absolute ADispatcher;
begin
  if Assigned(FInternalDispatcher.Code) and Assigned(FInternalDispatcher.Data) then
    TObject(FInternalDispatcher.Data).Free;
  TMethod(FInternalDispatcher) := TdxEventHandlerInstance.CreateMethodPointer(InternalInvoke, ATypeData);
  AMethod := FInternalDispatcher;
end;

function TdxMulticastImplementator.GetCount: Integer;
begin
  Result := Length(FHandlers);
end;

procedure TdxMulticastImplementator.InternalClear;
begin
  FHandlers := nil;
end;

procedure TdxMulticastImplementator.InternalAdd(const AValue);
var
  AMethod: TMethod absolute AValue;
begin
  FLock.Enter;
  try
    SetLength(FHandlers, Length(FHandlers) + 1);
    FHandlers[High(FHandlers)] := AMethod;
  finally
    FLock.Leave;
  end;
end;

procedure TdxMulticastImplementator.InternalRemove(const AValue);
var
  AMethod: TMethod absolute AValue;
  I: Integer;
begin
  FLock.Enter;
  try
    for I := Low(FHandlers) to High(FHandlers) do
      if dxSameMethods(FHandlers[I], AMethod) then
      begin
        if I <> High(FHandlers) then
          System.Move(FHandlers[I + 1], FHandlers[I], (High(FHandlers) - I) * SizeOf(TMethod));
        SetLength(FHandlers, Length(FHandlers) - 1);
        Break;
      end;
  finally
    FLock.Leave;
  end;
end;

class procedure TdxMulticastImplementator.InvokeMethod(const AMethod: TMethod; AParams: Pointer; AStackSize: Integer);
const
  PointerSize = SizeOf(Pointer);
asm
{$IFNDEF CPUX64}
  PUSH  EBP
  MOV   EBP,ESP
  PUSH  EAX
  PUSH  EBX
  MOV   EBX, EDX

  TEST  ECX, ECX
  JNZ   @@1

  NOP
  JMP   @@2

//@@1:
//{$IFDEF ALIGN_STACK}
//      MOV   ECX, ECX
//      AND   ECX, $F
//      JZ    @@no_align
//      SUB   ECX, 16
//      ADD   ESP, ECX
//@@no_align:
//{$ENDIF ALIGN_STACK}
@@1:
  ADD   ECX, PointerSize - 1
  AND   ECX, NOT (PointerSize-1)
  AND   ECX, $FFFF
  SUB   ESP, ECX

  MOV   EDX, ESP
{$IFNDEF DELPHIXE2}
  LEA   EAX, [EBX].TParameters.Stack[8]
{$ELSE}
  LEA   EAX, [EBX].TParameters.Stack
{$ENDIF}

  CALL  System.Move
@@2:
  MOV   ECX, [EBX].TParameters.Registers.DWORD[4]
  MOV   EDX, [EBX].TParameters.Registers.DWORD[0]
  MOV   EBX, [EBP - 4]

  MOV   EAX, [EBX].TMethod.Data
  CALL  [EBX].TMethod.Code
  POP   EBX
  POP   EAX
  MOV   ESP,EBP
  POP   EBP
{$ELSE}
  .PARAMS 64
  TEST  R8,R8
  JZ    @@1
  MOV   R9,   RSP
  PUSH  RCX
  PUSH  RDX
  LEA   RCX,  [RDX].TParameters.Stack
  MOV   RDX,  R9
  CALL  Move
  POP   RDX
  POP   RCX
@@1:
  LEA   RAX,  [RCX]
  MOV   RCX,  [RDX].TParameters.Stack.QWORD[0]
  MOV   R8,   [RDX].TParameters.Stack.QWORD[16]
  MOV   R9,   [RDX].TParameters.Stack.QWORD[24]
  MOVSD XMM0, [RDX].TParameters.Stack.QWORD[0]
  MOVSD XMM1, [RDX].TParameters.Stack.QWORD[8]
  MOVSD XMM2, [RDX].TParameters.Stack.QWORD[16]
  MOVSD XMM3, [RDX].TParameters.Stack.QWORD[24]
  MOV   RDX,  [RDX].TParameters.Stack.QWORD[8]
  MOV   RCX,  [RAX].TMethod.Data
  CALL  [RAX].TMethod.Code
{$ENDIF}
end;

procedure TdxMulticastImplementator.InternalInvoke(AParams: PParameters; AStackSize: Integer);
var
  I: Integer;
begin
  for I := Low(FHandlers) to High(FHandlers) do
    InvokeMethod(FHandlers[I], AParams, AStackSize);
end;

{ TdxMulticastImplementator<T> }

constructor TdxMulticastImplementator<T>.Create;
var
  ATypeInfo: PTypeInfo;
  ATypeData: PTypeData;
begin
  ATypeInfo := TypeInfo(T);
  ATypeData := GetTypeData(ATypeInfo);
  inherited Create;
  SetEventDispatcher(FInvoke, ATypeData);
end;

procedure TdxMulticastImplementator<T>.Add(const AMethod: T);
begin
  InternalAdd(AMethod);
end;

procedure TdxMulticastImplementator<T>.Remove(const AMethod: T);
begin
  InternalRemove(AMethod);
end;

procedure TdxMulticastImplementator<T>.Clear;
begin
  InternalClear;
end;

procedure TdxMulticastImplementator<T>.SetEventDispatcher(var ADispatcher: T; ATypeData: PTypeData);
begin
  InternalSetDispatcher(ADispatcher, ATypeData);
end;

function TdxMulticastImplementator<T>.GetInvoke: T;
begin
  Result := FInvoke;
end;

{ TdxMulticastMethod<T> }

procedure TdxMulticastMethod<T>.Clear;
begin
  if FImplementator <> nil then
    FImplementator.Clear;
end;

function TdxMulticastMethod<T>.Clone: TdxMulticastMethod<T>;
begin
  Validate;
  Result := Self;
end;

procedure TdxMulticastMethod<T>.Add(const AMethod: T);
begin
  Validate;
  FImplementator.Add(AMethod);
end;

procedure TdxMulticastMethod<T>.Remove(const AMethod: T);
begin
  if FImplementator <> nil then
    FImplementator.Remove(AMethod);
end;

procedure TdxMulticastMethod<T>.Validate;
begin
  if FImplementator = nil then
    FImplementator := TdxMulticastImplementator<T>.Create;
end;

function TdxMulticastMethod<T>.GetCount: Integer;
begin
  if FImplementator <> nil then
    Result := FImplementator.Count
  else
    Result := 0;
end;

function TdxMulticastMethod<T>.GetEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TdxMulticastMethod<T>.GetInvoke: T;
begin
  Validate;
  Result := FImplementator.Invoke;
end;

{ TdxNullableValue<T> }

constructor TdxNullableValue<T>.Create(AUse: Boolean; const AValue: T);
begin
  if AUse then
    Value := AValue
  else
    Reset;
end;

function TdxNullableValue<T>.GetHasValue: Boolean;
begin
  Result := FHasValue;
end;

function TdxNullableValue<T>.GetIsNull: Boolean;
begin
  Result := not FHasValue;
end;

procedure TdxNullableValue<T>.Reset;
begin
  FHasValue := False;
  FValue := Default(T);
end;

class operator TdxNullableValue<T>.Equal(const A, B: TdxNullableValue<T>): Boolean;
begin
  Result := (A.IsNull and B.IsNull) or
    (not (A.IsNull or B.IsNull) and (TComparer<T>.Default.Compare(A.Value, B.Value) = 0));
end;

class function TdxNullableValue<T>.IfThen(AValue: Boolean; const ATrue, AFalse: T): TdxNullableValue<T>;
begin
  if AValue then
    Result := TdxNullableValue<T>.Create(True, ATrue)
  else
    Result := TdxNullableValue<T>.Create(True, AFalse);
end;

class function TdxNullableValue<T>.IfThen(AValue: Boolean; const ATrue: T): TdxNullableValue<T>;
begin
  if AValue then
    Result := TdxNullableValue<T>.Create(True, ATrue)
  else
    Result := TdxNullableValue<T>.Null;
end;

class function TdxNullableValue<T>.IfThen(AValue: Boolean; const ATrue,
  AFalse: TdxNullableValue<T>): TdxNullableValue<T>;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

class function TdxNullableValue<T>.IfThen(AValue: Boolean; const ATrue: TdxNullableValue<T>): TdxNullableValue<T>;
begin
  if AValue then
    Result := ATrue
  else
    Result := TdxNullableValue<T>.Null;
end;

class operator TdxNullableValue<T>.Implicit(const A: T): TdxNullableValue<T>;
begin
  Result := TdxNullableValue<T>.Create(True, A);
end;

class operator TdxNullableValue<T>.Implicit(const A: TdxNullableValue<T>): T;
begin
  Result := A.Value;
end;

class operator TdxNullableValue<T>.Explicit(const A: TdxNullableValue<T>): T;
begin
  Result := A.Value;
end;

class operator TdxNullableValue<T>.LogicalAnd(const A, B: TdxNullableValue<T>): TdxNullableValue<T>;
var
  AValueA, AValueB: TValue;
begin
  if A.IsNull and B.IsNull then
    Result := TdxNullableValue<T>.Null
  else
  begin
    if A.IsNull then
      AValueA := TValue.From<T>(B.Value)
    else
      AValueA := TValue.From<T>(A.Value);
    if (AValueA.TypeInfo <> TypeInfo(Boolean)) then
      Assert(False, 'Not applicable')
    else
      if A.IsNull or B.IsNull then
        if not AValueA.AsBoolean then
          Result.Value := AValueA.AsType<T>
        else
          Result := TdxNullableValue<T>.Null
      else
        begin
          AValueB := TValue.From<T>(B.Value);
          AValueA := AValueA.AsBoolean and AValueB.AsBoolean;
          Result.Value := AValueA.AsType<T>;
        end;
  end;
end;

class operator TdxNullableValue<T>.LogicalNot(const A: TdxNullableValue<T>): TdxNullableValue<T>;
var
  AValue: TValue;
begin
  Result.FHasValue := A.FHasValue;
  if A.FHasValue then
  begin
    AValue := TValue.From<T>(A.Value);
    if (AValue.TypeInfo = TypeInfo(Boolean)) then
    begin
      AValue := not AValue.AsBoolean;
      Result.Value := AValue.AsType<T>;
    end
    else
      Assert(False, 'Not applicable');
  end
end;

class operator TdxNullableValue<T>.LogicalOr(const A, B: TdxNullableValue<T>): TdxNullableValue<T>;
var
  AValueA, AValueB: TValue;
begin
  if A.IsNull and B.IsNull then
    Result := TdxNullableValue<T>.Null
  else
  begin
    if A.IsNull then
      AValueA := TValue.From<T>(B.Value)
    else
      AValueA := TValue.From<T>(A.Value);
    if (AValueA.TypeInfo <> TypeInfo(Boolean)) then
      Assert(False, 'Not applicable')
    else
      if A.IsNull or B.IsNull then
        if AValueA.AsBoolean then
          Result.Value := AValueA.AsType<T>
        else
          Result := TdxNullableValue<T>.Null
      else
        begin
          AValueB := TValue.From<T>(B.Value);
          AValueA := AValueA.AsBoolean or AValueB.AsBoolean;
          Result.Value := AValueA.AsType<T>;
        end;
  end;
end;

class operator TdxNullableValue<T>.NotEqual(const A: TdxNullableValue<T>; const B: T): Boolean;
begin
  Result := A.IsNull or (TComparer<T>.Default.Compare(A.Value, B) <> 0);
end;

class operator TdxNullableValue<T>.Equal(const A: TdxNullableValue<T>; const B: T): Boolean;
begin
  if A.IsNull then
    Result := False
  else
    Result := TComparer<T>.Default.Compare(A.Value, B) = 0
end;

class operator TdxNullableValue<T>.GreaterThan(const A: TdxNullableValue<T>; const B: T): Boolean;
begin
  if A.IsNull then
    Result := False
  else
    Result := TComparer<T>.Default.Compare(A.Value, B) > 0
end;

class operator TdxNullableValue<T>.LessThan(const A: TdxNullableValue<T>; const B: T): Boolean;
begin
  if A.IsNull then
    Result := False
  else
    Result := TComparer<T>.Default.Compare(A.Value, B) < 0
end;

class function TdxNullableValue<T>.GetNull: TdxNullableValue<T>;
begin
  Result := TdxNullableValue<T>.Create(False, Default(T));
end;

function TdxNullableValue<T>.GetValue: T;
begin
  if HasValue then
    Result := FValue
  else
    Result := Default(T);
end;

class operator TdxNullableValue<T>.NotEqual(const A, B: TdxNullableValue<T>): Boolean;
begin
  Result := (A.IsNull <> B.IsNull) or
    (not A.IsNull and (TComparer<T>.Default.Compare(A.Value, B.Value) <> 0));
end;

procedure TdxNullableValue<T>.SetValue(const Value: T);
begin
  FHasValue := True;
  FValue := Value;
end;

function TdxNullableValue<T>.IsNullOrEmpty: Boolean;
begin
  Result := IsNull or (TComparer<T>.Default.Compare(FValue, Default(T)) = 0);
end;

{$IFNDEF DELPHIXE}
function TdxNullableValue<T>.IsEqual(const AValue: TdxNullableValue<T>): Boolean;
begin
  Result := (Self.IsNull and AValue.IsNull) or
    (not (Self.IsNull or AValue.IsNull) and (TComparer<T>.Default.Compare(Self.Value, AValue.Value) = 0));
end;
{$ENDIF}

{ TdxIStringComparer }

class procedure TdxIStringComparer.Finalize;
begin
  FreeAndNil(FOrdinal);
end;

class function TdxIStringComparer.Ordinal: TCustomComparer<string>;
begin
  if FOrdinal = nil then
    FOrdinal := TdxOrdinalIStringComparer.Create;
  Result := FOrdinal;
end;

function GetHashCodeOfString(const ASource: string): Integer;
var
  S: PChar;
  AHash1, AHash2, C: Integer;
begin
  AHash1 := 5381;
  AHash2 := AHash1;
  S := PChar(ASource);
  while Ord(S[0]) <> 0 do
  begin
    C := Ord(S[0]);
    AHash1 := ((AHash1 shl 5) + AHash1) xor C;
    C := Ord(S[1]);
    if C = 0 then
      Break;
    AHash2 := ((AHash2 shl 5) + AHash2) xor C;
    Inc(S, 2);
  end;
  Result := AHash1 + AHash2 * 1566083941;
end;

{ TdxOrdinalIStringComparer }

function TdxOrdinalIStringComparer.Compare(const Left, Right: string): Integer;
var
  L, R: string;
  ALen, ALenDiff: Integer;
begin
  L := AnsiLowerCase(Left);
  R := AnsiLowerCase(Right);
  ALen := Length(L);
  ALenDiff := ALen - Length(R);
  if Length(R) < ALen then
    ALen := Length(R);
  Result := BinaryCompare(PChar(L), PChar(R), ALen * SizeOf(Char));
  if Result = 0 then
    Result := ALenDiff;
end;

function TdxOrdinalIStringComparer.Equals(const ALeft, ARight: string): Boolean;
var
  ALength: Integer;
  L, R: string;
begin
  L := AnsiLowerCase(ALeft);
  R := AnsiLowerCase(ARight);
  ALength := Length(L);
  Result := (ALength - Length(R) = 0) and CompareMem(PChar(L), PChar(R), ALength * SizeOf(Char));
end;

function TdxOrdinalIStringComparer.GetHashCode(const AValue: string): Integer;
var
  S: string;
begin
  S := AnsiLowerCase(AValue);
  Result := GetHashCodeOfString(S);
end;

{ TdxOrdinalStringEqualityComparer }

function TdxOrdinalStringComparer.Compare(const ALeft, ARight: string): Integer;
var
  ALength, ALengthDiff: Integer;
begin
  ALength := Length(ALeft);
  ALengthDiff := ALength - Length(ARight);
  if Length(ARight) < ALength then
    ALength := Length(ARight);
  Result := BinaryCompare(PChar(ALeft), PChar(ARight), ALength * SizeOf(Char));
  if Result = 0 then
    Result := ALengthDiff;
end;

function TdxOrdinalStringComparer.Equals(const ALeft, ARight: string): Boolean;
var
  ALength: Integer;
begin
  ALength := Length(ALeft);
  Result := (ALength - Length(ARight) = 0) and CompareMem(PChar(ALeft), PChar(ARight), ALength * SizeOf(Char));
end;

function TdxOrdinalStringComparer.GetHashCode(const AValue: string): Integer;
begin
  Result := GetHashCodeOfString(AValue);
end;

{ TdxStringComparer }

class procedure TdxStringComparer.Finalize;
begin
  FreeAndNil(FOrdinal);
end;

class function TdxStringComparer.Ordinal: TCustomComparer<string>;
begin
  if FOrdinal = nil then
    FOrdinal := TdxOrdinalStringComparer.Create;
  Result := FOrdinal;
end;

{ TdxMemoryStream }

constructor TdxMemoryStream.Create(const AFileName: string; AMode: Word = fmOpenRead or fmShareDenyWrite);
begin
  inherited Create;
  LoadFromFile(AFileName, AMode);
end;

constructor TdxMemoryStream.Create(const AContent: TArray<Byte>);
begin
  Create(AContent, 0, Length(AContent));
end;

constructor TdxMemoryStream.Create(const AContent: TArray<Byte>; ACount: Integer);
begin
  Create(AContent, 0, ACount);
end;

constructor TdxMemoryStream.Create(const AContent: TArray<Byte>; AOffset, ACount: Integer);
begin
  inherited Create;
  Write((@AContent[AOffset])^, ACount);
  Position := 0;
end;

procedure TdxMemoryStream.LoadFromFile(const AFileName: string);
begin
  LoadFromFile(AFileName, fmOpenRead or fmShareDenyNone);
end;

procedure TdxMemoryStream.LoadFromFile(const AFileName: string; AMode: Word);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, AMode);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

function TdxMemoryStream.ReadByte: Byte;
begin
  ReadBuffer(Result, SizeOf(Byte));
end;

function TdxMemoryStream.ReadInteger: Integer;
begin
  ReadBuffer(Result, SizeOf(Integer));
end;

function TdxMemoryStream.Realloc(var NewCapacity: Integer): Pointer;
const
  MemoryDelta = $4000;
begin
  if (NewCapacity > 0) and (NewCapacity <> Size) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Memory;
  if NewCapacity <> Capacity then
  begin
    if NewCapacity = 0 then
    begin
      FreeMem(Memory);
      Result := nil;
    end
    else
    begin
      if Capacity = 0 then
        Result := AllocMem(NewCapacity)
      else
        ReallocMem(Result, NewCapacity);
      if Result = nil then raise EStreamError.Create('@SMemoryStreamError');
    end;
  end;
end;

function TdxMemoryStream.ToArray: TArray<Byte>;
var
  ASize: Integer;
begin
  ASize  := Size;
  SetLength(Result, ASize);
  if ASize = 0 then
    Exit;
  Move(Memory^, Result[0], ASize);
end;

procedure TdxMemoryStream.WriteByte(const Value: Byte);
begin
  WriteBuffer(Value, SizeOf(Byte));
end;

procedure TdxMemoryStream.WriteByteArray(const Value: TArray<Byte>);
begin
  WriteBuffer(PByte(Value)^, Length(Value));
end;

procedure TdxMemoryStream.WriteInteger(const Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Integer));
end;

{ TdxRectList }

function TdxRectList.Union: TRect;
var
  I: Integer;
begin
  if Count = 0 then
    raise EInvalidOperation.Create(ClassName);

  Result := First;
  for I := 1 to Count - 1 do
    Result := cxRectUnion(Result, Items[I]);
end;

initialization

finalization
  TdxStringComparer.Finalize;
  TdxIStringComparer.Finalize;
end.
