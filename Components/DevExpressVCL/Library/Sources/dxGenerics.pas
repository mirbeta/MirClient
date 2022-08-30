{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCoreLibrary                                       }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORELIBRARY AND ALL            }
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

unit dxGenerics;

{$I cxVer.inc}

{.$DEFINE DXLOGGING}

interface

uses
  TypInfo, Types, Classes, Generics.Defaults, Generics.Collections, Contnrs,
  dxCore, dxCoreClasses, dxCoreGraphics;

type
{$IFNDEF DELPHIXE}
  PNativeInt = ^NativeInt;
  PNativeUInt = ^NativeUInt;
{$ENDIF}
  TdxCharStringDictionary = class(TDictionary<Char, string>);
  TdxIntegersDictionary = class(TDictionary<Integer, Integer>);
  TdxIntegersPair = TPair<Integer, Integer>;
  TdxStringsDictionary = class(TDictionary<string, string>);
  TdxStringColorDictionary = class(TDictionary<string, TdxAlphaColor>);
  TdxCharList = class(TList<Char>);
  TdxListMaxIntegerAction<T> = reference to function(const AItem: T): Integer;

  { TdxStringList }

  TdxStringList = class(TStringList)
  public
    procedure AddRange(const Values: TStrings); overload;
    procedure AddRange(const Values: TArray<string>); overload;
    function Contains(const Value: string): Boolean;
    procedure DeleteRange(AIndex, ACount: Integer);
    procedure InsertRange(Index: Integer; const Values: TStrings); overload;
    procedure InsertRange(Index: Integer; const Values: TArray<string>); overload;
    function Remove(const Value: string): Integer;
    function ToArray: TArray<string>;
  end;

  TdxList<T: class> = class(TObjectList)
  private
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    constructor Create; reintroduce; overload;
    constructor Create(AOwnsObjects: Boolean); reintroduce; overload;
    procedure AddRange(const Collection: TdxList<T>); overload;
    procedure AddRange(const Collection: array of T); overload;
    function BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean;
    function Contains(const AItem: T): Boolean;
    procedure DeleteRange(AIndex, ACount: Integer);
    function Extract(const AItem: T): T; reintroduce;
    function First: T; reintroduce;
    function Last: T; reintroduce;
    function Max(AAction: TdxListMaxIntegerAction<T>): Integer;
    procedure Sort(const AComparer: IComparer<T>); overload;
    procedure Sort; overload;
    function ToArray: TArray<T>;

    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  { TdxNotificationCollection }

  TdxNotificationCollectionChangedEvent = procedure(const AItem: TObject; Action: TListNotification) of object;
  TdxNotificationCollectionChangedEventHandler = TdxMulticastMethod<TdxNotificationCollectionChangedEvent>;

  TdxNotificationCollection<T: class> = class(TdxList<T>)
  strict private
    FOnChanged: TdxNotificationCollectionChangedEventHandler;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    procedure RaiseChanged(const AItem: TObject; Action: TListNotification);
  public
    procedure AddChangedHandler(AHandler: TdxNotificationCollectionChangedEvent);
    procedure RemoveChangedHandler(AHandler: TdxNotificationCollectionChangedEvent);
  end;

  { TdxObjectList }

  TdxObjectList<T: class> = class(TdxList<T>)
  public
    constructor Create; reintroduce; overload;
  end;

  { TdxNotificationObjectCollection }

  TdxNotificationObjectCollection<T: class> = class(TdxNotificationCollection<T>)
  public
    constructor Create; reintroduce; overload;
  end;

  { TdxOrdinalHelper }

  TdxOrdinalHelper = class
  public const
    SizeMask: array[1..SizeOf(NativeInt)] of NativeUInt = (
    {$IFDEF CPUX64}
      $00000000000000FF, // 1
      $000000000000FFFF, // 2
      $0000000000FFFFFF, // 3
      $00000000FFFFFFFF, // 4
      $000000FFFFFFFFFF, // 5
      $0000FFFFFFFFFFFF, // 6
      $00FFFFFFFFFFFFFF, // 7
      $FFFFFFFFFFFFFFFF);// 8
    {$ELSE}
      $000000FF, // 1
      $0000FFFF, // 2
      $00FFFFFF, // 3
      $FFFFFFFF);// 4
    {$ENDIF CPUX64}
  {$REGION 'internal types'}
  public type
    TSetter = procedure (AValue: NativeInt; ADest: Pointer);
    class procedure Set2(AValue: NativeInt; ADest: Pointer); static;
    class procedure Set3(AValue: NativeInt; ADest: Pointer); static;
  {$IFDEF CPUX64}
    class procedure Set4(AValue: NativeInt; ADest: Pointer); static;
    class procedure Set5(AValue: NativeInt; ADest: Pointer); static;
    class procedure Set6(AValue: NativeInt; ADest: Pointer); static;
    class procedure Set7(AValue: NativeInt; ADest: Pointer); static;
  {$ENDIF}
  strict private const
    Setters: array[2..SizeOf(NativeInt) - 1] of TSetter = (
    {$IFDEF CPUX64}
      TdxOrdinalHelper.Set2,
      TdxOrdinalHelper.Set3,
      TdxOrdinalHelper.Set4,
      TdxOrdinalHelper.Set5,
      TdxOrdinalHelper.Set6,
      TdxOrdinalHelper.Set7
    {$ELSE}
      TdxOrdinalHelper.Set2,
      TdxOrdinalHelper.Set3
    {$ENDIF}
      );
  {$ENDREGION}
  public
    class procedure SetValue(ASize: Integer; AValue: NativeInt; ADest: Pointer); static; inline;
  end;

  { TdxStringToOrdinalMap }

  TdxStringToOrdinalMap = class
  {$REGION 'internal types'}
  public const
    DefaultHashTableSize = 691;
  public type
    PItem = ^TItem;
    TItem = record
      Key: string;
      Hash: Cardinal;
      Value: NativeInt;
      Next: PItem;
    end;
    TTable = array of PItem;
  {$ENDREGION}
  strict private
    FCount: Cardinal;
    FTable: TTable;
    FTableSize: Cardinal;
    FValueSize: Integer;
  strict protected
    function NewItem(const AKey: string; AHash: Cardinal; const AValue: NativeInt): PItem;
    procedure InternalAdd(const AKey: string; AValue: NativeInt; ACheckDuplicates: Boolean);
    procedure InternalRemove(AItem: PItem);
    function InternalTryGetValue(const AKey: string; out AValue: NativeInt): Boolean;
    procedure RemoveValue(AValue: NativeInt); virtual;
  protected
    property Table: TTable read FTable;
  public
    constructor Create(AValueSize: Integer; ATableSize: Cardinal = DefaultHashTableSize);
    destructor Destroy; override;
    procedure Add(const AKey: string; AValue: Pointer);
    procedure AddOrSetValue(const AKey: string; AValue: Pointer);
    procedure Clear;
    function ContainsKey(const AKey: string): Boolean;
    class function Hash(const S: string): Cardinal; static; inline;
    function Remove(const AKey: string): Boolean;
    function TryGetValue(const AKey: string; AValue: Pointer): Boolean;

    function Keys: TArray<string>;

    property Count: Cardinal read FCount;
    property TableSize: Cardinal read FTableSize;
  end;

  { TdxNamedOrdinalDictionary }

  TdxNamedOrdinalDictionary = class
  strict private
    FMap: TdxStringToOrdinalMap;
    function GetCount: Integer;
  strict protected
    function CreateMap: TdxStringToOrdinalMap; virtual; abstract;
  protected
    property Map: TdxStringToOrdinalMap read FMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ContainsKey(const AKey: string): Boolean; inline;
    procedure Remove(const AKey: string); inline;
    function Keys: TArray<string>;

    property Count: Integer read GetCount;
  end;

  { TdxNamedOrdinalDictionary }

  TdxNamedOrdinalDictionary<T> = class(TdxNamedOrdinalDictionary)
  strict private
    function GetItem(const AKey: string): T;
    procedure SetItem(const AKey: string; const Value: T);
  protected
    function CreateMap: TdxStringToOrdinalMap; override;
  public
    procedure Add(const AKey: string; AValue: T); inline;
    procedure AddOrSetValue(const AKey: string; AValue: T); inline;
    function ContainsValue(const Value: T): Boolean;
    function GetValueDef(const AKey: string; ADefaultValue: T): T; inline;
    function TryGetValue(const AKey: string; out AValue: T): Boolean; inline;

    property Items[const AKey: string]: T read GetItem write SetItem; default;
  end;

  TdxStringBooleanDictionary = class(TdxNamedOrdinalDictionary<Boolean>);

  { TdxNamedOrdinalDictionary }

  TdxNamedObjectDictionary<T: class> = class(TdxNamedOrdinalDictionary)
  {$REGION 'internal types'}
  strict protected type
    TMap = class(TdxStringToOrdinalMap)
    strict private
      FOwnsObjects: Boolean;
    strict protected
      procedure RemoveValue(AValue: NativeInt); override;
    public
      property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    end;
  {$ENDREGION}
  strict private
    function GetItem(const AKey: string): T;
    function GetOwnsObjects: Boolean;
    procedure SetItem(const AKey: string; const Value: T);
    procedure SetOwnsObjects(const Value: Boolean);
  strict protected
    function CreateMap: TdxStringToOrdinalMap; override;
  public
    constructor Create(AOwnsObjects: Boolean);

    procedure Add(const AKey: string; AValue: T); inline;
    procedure AddOrSetValue(const AKey: string; AValue: T); inline;
    function TryGetValue(const AKey: string; out AValue: T): Boolean; inline;

    property Items[const AKey: string]: T read GetItem write SetItem; default;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    function Values: TArray<T>;
  end;

  { TdxNamedDelegateDictionary }

  TdxNamedDelegateDictionary<TValue> = class
  public const
    DefaultHashTableSize = Cardinal(467);
  {$REGION 'internal types'}
  strict protected type
    TItem = class
    strict private
      FKey: string;
      FHash: Cardinal;
      FValue: TValue;
      FNext: TItem;
    public
      constructor Create(const AKey: string; AHash: Cardinal; const AValue: TValue);
      function IsEqualTo(AItem: TItem): Boolean;

      property Key: string read FKey;
      property Hash: Cardinal read FHash;
      property Next: TItem read FNext write FNext;
      property Value: TValue read FValue write FValue;
    end;
    TTable = TArray<TItem>;
  {$ENDREGION}
  strict private
    FCount: Integer;
    FTable: TTable;
  strict protected
    class function Hash(const AKey: string): Cardinal; static; inline;
    procedure InternalAdd(const AKey: string; const AValue: TValue; ACheckDuplicates: Boolean);
    property Table: TTable read FTable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const AKey: string; AValue: TValue);
    procedure AddOrSetValue(const AKey: string; AValue: TValue);
    procedure Clear;
    function ContainsKey(const AKey: string): Boolean;
    function Keys: TArray<string>;
    function TryGetValue(const AKey: string; out AValue: TValue): Boolean;

    property Count: Integer read FCount;
  end;

  { TdxEnumeratedMap }

  TdxEnumeratedMap<TValue> = class
  strict protected type
    TKeyTable = array of Boolean;
    TValueTable = array of TValue;
  strict private
    FCount: Integer;
    FKeyTable: TKeyTable;
    FValueTable: TValueTable;
    FTableSize: Cardinal;
    function GetValues: TArray<TValue>;
  strict protected
    procedure Clear;
    procedure InternalAdd(AKey: Cardinal; const AValue: TValue; ACheckDuplicates: Boolean);
    procedure InternalRemove(AKey: Cardinal); virtual;
  public
    constructor Create(ATableSize: Cardinal);
    destructor Destroy; override;
    procedure Add(AKey: Cardinal; const AValue: TValue);
    procedure AddOrSetValue(AKey: Cardinal; const AValue: TValue);
    function ContainsKey(AKey: Cardinal): Boolean;
    procedure Remove(AKey: Cardinal);
    function TryGetValue(AKey: Cardinal; out AValue: TValue): Boolean;

    property Count: Integer read FCount;
    property TableSize: Cardinal read FTableSize;
    property KeyTable: TKeyTable read FKeyTable;
    property ValueTable: TValueTable read FValueTable;
    property Values: TArray<TValue> read GetValues;
  end;

  { TdxEnumeratedDictionary }

  TdxEnumeratedDictionary<TValue> = class
  strict private
    FKeySize: Cardinal;
    FMap: TdxEnumeratedMap<TValue>;
    FMinValue: Integer;
    FMaxValue: Integer;
    function GetCount: Integer;
    function GetValues: TArray<TValue>;
  strict protected
    function CreateMap(ATableSize: Cardinal): TdxEnumeratedMap<TValue>; virtual;

    property KeySize: Cardinal read FKeySize;
    property MinValue: Integer read FMinValue;
    property Map: TdxEnumeratedMap<TValue> read FMap;
    property MaxValue: Integer read FMaxValue;
  public
    constructor Create(AKeySize: Cardinal; ATypeInfo: PTypeInfo);
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Values: TArray<TValue> read GetValues;
  end;

  { TdxEnumeratedDictionary }

  TdxEnumeratedDictionary<TEnumeration, TValue> = class(TdxEnumeratedDictionary<TValue>)
  strict private
    function GetItem(AKey: TEnumeration): TValue; inline;
    function GetKeys: TArray<TEnumeration>;
    procedure SetItem(AKey: TEnumeration; const Value: TValue); inline;
  public
    constructor Create;
    procedure Add(AKey: TEnumeration; const AValue: TValue);
    procedure AddOrSetValue(AKey: TEnumeration; const AValue: TValue);
    function TryGetValue(AKey: TEnumeration; out AValue: TValue): Boolean;
    function ContainsKey(AKey: TEnumeration): Boolean; inline;
    procedure Remove(AKey: TEnumeration); inline;

    property Items[AKey: TEnumeration]: TValue read GetItem write SetItem; default;
    property Keys: TArray<TEnumeration> read GetKeys;
  end;

  { TdxEnumeratedObjectDictionary }

  TdxEnumeratedObjectDictionary<TEnumeration; TValue: class> = class(TdxEnumeratedDictionary<TValue>)
  {$REGION 'internal types'}
  strict protected type
    TMap = class(TdxEnumeratedMap<TObject>)
    strict private
      FOwnsObjects: Boolean;
    strict protected
      procedure InternalRemove(AKey: Cardinal); override;
    public
      property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    end;
  {$ENDREGION}
  strict private
    function GetItem(AKey: TEnumeration): TValue; inline;
    function GetKeys: TArray<TEnumeration>;
    function GetMap: TMap;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    procedure SetItem(AKey: TEnumeration; const Value: TValue); inline;
  strict protected
    function CreateMap(ATableSize: Cardinal): TdxEnumeratedMap<TValue>; override;
    property Map: TMap read GetMap;
  public
    constructor Create(AOwnsObjects: Boolean);
    procedure Add(AKey: TEnumeration; AValue: TValue);
    procedure AddOrSetValue(AKey: TEnumeration; AValue: TValue);
    function TryGetValue(AKey: TEnumeration; out AValue: TValue): Boolean;
    function ContainsKey(AKey: TEnumeration): Boolean; inline;
    procedure Remove(AKey: TEnumeration); inline;

    property Items[AKey: TEnumeration]: TValue read GetItem write SetItem; default;
    property Keys: TArray<TEnumeration> read GetKeys;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  { TdxOrdinalList }

  TdxOrdinalList = class
  strict private
    FCapacity: Integer;
    FCount: Integer;
    FOnNotify: TCollectionNotifyEvent<NativeUInt>;
  strict protected
    FList: TArray<NativeUInt>;
    procedure DoDelete(AIndex: Integer); inline;
    procedure CheckDeleteRange(AIndex, ACount: Integer); inline;
    procedure DoDeleteRange(AIndex, ACount: Integer);
    function GetItem(AIndex: Integer): NativeUInt; inline;
    procedure Grow;
    procedure Notify(const AItem: NativeUInt; AAction: TCollectionNotification); virtual;
    procedure SetCapacity(AValue: Integer);
    procedure SetCount(AValue: Integer); virtual;
    procedure SetItem(AIndex: Integer; AItem: NativeUInt); inline;

    function Add(AItem: NativeUInt): Integer; inline;
    function BinarySearch(AItem: NativeUInt; out AIndex: Integer; const AComparer: IComparer<NativeUInt>): Boolean;
    function Contains(AItem: NativeUInt): Boolean; inline;
    function Extract(AItem: NativeUInt; ADirection: TList.TDirection = FromBeginning): NativeUInt;
    function ExtractByIndex(AIndex: Integer): NativeUInt;
    function First: NativeUInt; inline;
    function IndexOf(AItem: NativeUInt): Integer;
    function LastIndexOf(AItem: NativeUInt): Integer;
    function Last: NativeUInt; inline;
    function Remove(AItem: NativeUInt): Integer;

    property OnNotify: TCollectionNotifyEvent<NativeUInt> read FOnNotify write FOnNotify;
  public
    constructor Create(ACapacity: Integer = 0);
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(AIndex: Integer);
    procedure DeleteRange(AIndex, ACount: Integer); inline;
    function Empty: Boolean; inline;
    procedure Exchange(AIndex1, AIndex2: Integer);
    procedure Insert(AIndex: Integer; AItem: NativeUInt);
    procedure Move(ACurrentIndex, ANewIndex: Integer);
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(ACompare: TCompareItems; AMultiThreaded: Boolean = False); overload;
    procedure Sort(ACompare: TListSortCompare; AMultiThreaded: Boolean = False); overload;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
  end;

  { TdxOrdinalList<T> }

  TdxOrdinalList<T> = class(TdxOrdinalList)
  strict private
    FItemSize: Integer;
    FValueMask: NativeUInt;
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; AValue: T);
  strict protected
    property ItemSize: Integer read FItemSize;
    property ValueMask: NativeUInt read FValueMask;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(ACollection: TdxOrdinalList<T>); overload;
    function Add(AItem: T): Integer; inline;
    procedure AddRange(AList: TdxOrdinalList<T>); overload;
    procedure AddRange(const AArray: TArray<T>); overload;
    function BinarySearch(AItem: T; out AIndex: Integer): Boolean; overload;
    function Contains(AItem: T): Boolean;
    function Extract(AItem: T; ADirection: TList.TDirection = FromBeginning): T;
    function ExtractByIndex(AIndex: Integer): T;
    function First: T; inline;
    procedure Insert(AIndex: Integer; AItem: NativeUInt); inline;
    function IndexOf(AItem: T): Integer;
    function LastIndexOf(AItem: T): Integer;
    function Last: T; inline;
    function Remove(AItem: T): Integer;

    function ToArray: TArray<T>;

    property Items[Index: Integer]: T read GetItem write SetItem; default;
  end;

  TdxIntegerList = class(TdxOrdinalList<Integer>);
  TdxAlphaColorList = class(TdxOrdinalList<TdxAlphaColor>);

  TdxObjectStack<T: class> = class(TObjectStack)
  strict private
    FOwnsObjects: Boolean;
  public
    constructor Create(AOwnsObjects: Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Extract: T; inline;
    procedure Pop; inline;
    function Peek: T; inline;

    function ToArray: TArray<T>;

    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

  { TdxIntegerStack }

  TdxIntegerStack = class(TStack)
  public
    procedure Clear;
    function Push(AItem: NativeInt): Pointer; inline;
    function Pop: NativeInt; inline;
    function Peek: NativeInt; inline;
  end;

  TdxTopologicalSorterIsDependOnDelegate = reference to function (ALeft, ARight: Integer): Boolean;

  { TdxTopologicalSorterNode }

  TdxTopologicalSorterNode = class
  private
    FOneBasedSuccessorIndex: Integer;
    FNext: TdxTopologicalSorterNode;
  public
    constructor Create(AOneBasedSuccessorIndex: Integer; ANext: TdxTopologicalSorterNode);
    property OneBasedSuccessorIndex: Integer read FOneBasedSuccessorIndex;
    property Next: TdxTopologicalSorterNode read FNext;
  end;

  { TdxTopologicalSorter<T> }

  TdxTopologicalSorter<T: class> = class
  private type
    TDefaultDependencyCalculator = record
    private
      FSourceObjects: TdxList<T>;
      FComparer: IComparer<T>;
    public
      constructor Create(ASourceObjects: TdxList<T>; AComparer: IComparer<T>);

      function IsDependOn(Left, Right: Integer): Boolean;
    end;
  strict private
    FQLink: TArray<Integer>;
    FNodeList: TdxObjectList<TdxTopologicalSorterNode>;
    FNodes: TArray<TdxTopologicalSorterNode>;
    FSuccess: Boolean;
  protected
    procedure AppendRelation(ASuccessorIndex, APredecessorIndex: Integer);
    procedure Initialize(N: Integer);
    procedure CalculateRelations(ASourceObjects: TdxList<T>; const AIsDependOn: TdxTopologicalSorterIsDependOnDelegate);
    function CreateVirtualNoPredecessorsItemList: Integer;
    function ProcessNodes(ALastNoPredecessorItemIndex: Integer; ASourceObjects: TdxList<T>): TdxList<T>;
    function RemoveRelation(ANode: TdxTopologicalSorterNode; var ALastNoPredecessorItemIndex: Integer): TdxTopologicalSorterNode;

    function DoSort(ASourceObjects: TdxList<T>; AComparer: IComparer<T>): TdxList<T>; overload;
    function DoSort(ASourceObjects: TdxList<T>; const AIsDependOn: TdxTopologicalSorterIsDependOnDelegate): TdxList<T>; overload;

    property Nodes: TArray<TdxTopologicalSorterNode> read FNodes;
    property QLink: TArray<Integer> read FQLink;
  public
    constructor Create;
    destructor Destroy; override;

    class function Sort(ASourceObjects: TdxList<T>; AComparer: IComparer<T>): TdxList<T>;

    property Success: Boolean read FSuccess;
  end;

  { TdxComparer }

  TdxComparer<T> = class(TcxIUnknownObject, IComparer<T>)
  public
    class function Default: IComparer<T>;
    class function Construct(const Comparison: TComparison<T>): IComparer<T>;
    function Compare(const Left, Right: T): Integer; virtual; abstract;
  end;

  IdxComparable<T> = interface
    function CompareTo(const Value: T): Integer;
  end;

  { TdxDefaultComparable }

  TdxDefaultComparable<T> = class(TInterfacedObject, IdxComparable<T>)
  strict private
    FValue: T;
    FComparer: IComparer<T>;
  protected
    //IdxComparable
    function CompareTo(const AValue: T): Integer;
  public
    constructor Create(const AValue: T);

    property Value: T read FValue;
  end;

  { TdxAlgorithms }

  TdxAlgorithms<T> = class
  public
    class function BinarySearch(AList: TList<T>; const AValue: T; out AIndex: Integer): Boolean; overload;

    class function BinarySearch(AList: TList<T>; const APredicate: IdxComparable<T>;
      AStartIndex, AEndIndex: Integer; out AIndex: Integer): Boolean; overload;
    class function BinarySearch(AList: TList<T>; const APredicate: IdxComparable<T>; out AIndex: Integer): Boolean; overload;
  end;

  TdxAlgorithms1<T: class> = class
  public
    class function BinarySearch(AList: TdxList<T>; const APredicate: IdxComparable<T>;
      AStartIndex, AEndIndex: Integer; out AIndex: Integer): Boolean; overload;
    class function BinarySearch(AList: TdxList<T>; const APredicate: IdxComparable<T>; out AIndex: Integer): Boolean; overload;
    class function BinarySearchReverseOrder(AList: TdxList<T>; const APredicate: IdxComparable<T>;
      AStartIndex, AEndIndex: Integer; out AIndex: Integer): Boolean; overload;
    class function BinarySearchReverseOrder(AList: TdxList<T>; const APredicate: IdxComparable<T>; out AIndex: Integer): Boolean; overload;
  end;

  { TdxSortedList }

  TdxSortedList<T> = class
  private
    FComparer: IComparer<T>;
    function GetItem(Index: Integer): T;
  protected
    FInnerList: TList<T>;
    function CreateInnerList: TList<T>; virtual;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    destructor Destroy; override;

    function Count: Integer;
    function First: T;
    function Last: T;

    procedure Add(const AValue: T);
    procedure Insert(AIndex: Integer; const AValue: T);
    function Contains(const AValue: T): Boolean; virtual;
    function BinarySearch(const AValue: T): Integer; overload;
    function BinarySearch(const APredicate: IdxComparable<T>): Integer; overload;
    procedure Delete(AIndex: Integer);
    function Extract(const Value: T): T;
    procedure Remove(const AValue: T); virtual;
    procedure RemoveFrom(const AValue: T); virtual;
    procedure Clear; virtual;
    procedure CopyCore(ADestination: TdxSortedList<T>); virtual;

    property Self[Index: Integer]: T read GetItem; default;
  end;

  { TdxObjectSortedList<T> }

  TdxObjectSortedList<T: class> = class(TdxSortedList<T>)
  protected
    function CreateInnerList: TList<T>; override;
  public
    function ExtractByIndex(AIndex: Integer): T;
  end;

  TdxLayoutUnitSortedList = class sealed(TdxSortedList<Integer>)
  public
    function Clone: TdxLayoutUnitSortedList;
  end;

  { TdxHashSet }

  TdxHashSet<T> = class(TEnumerable<T>)
  strict private
    FDictionary: TDictionary<T, Integer>;
    function GetCount: Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    constructor Create(ACapacity: Integer = 0); overload;
    constructor Create(const AComparer: IEqualityComparer<T>); overload;
    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<T>); overload;
    destructor Destroy; override;
    function Contains(const AValue: T): Boolean;
    procedure Clear;
    procedure Include(const AValue: T); overload;
    procedure Include(const AValue: TdxHashSet<T>); overload;
    procedure Include(const AValue: TList<T>); overload;
    procedure Include(const AValue: TArray<T>); overload;
    procedure Exclude(const AValue: T); overload;
    procedure Exclude(const AValue: TdxHashSet<T>); overload;
    procedure Exclude(const AValue: TList<T>); overload;
    procedure Exclude(const AValue: TArray<T>); overload;
    function ToArray: TArray<T>;{$IFDEF DELPHIXE}override;{$ENDIF}
    function ToList: TList<T>;

    property Count: Integer read GetCount;
  end;

  { TdxSmartPointer<T> }

  TdxSmartPointer<T: class> = record
  private type
    TLifetimeWatcher = class(TInterfacedObject)
    private
      FObject: TObject;
    public
      constructor Create(AObject: TObject);
      destructor Destroy; override;
    end;
  private
    FValue: T;
    FLifetime: IInterface;
  public
    constructor Create(const AValue: T);
    class operator Implicit(const AValue: T): TdxSmartPointer<T>;
    class operator Implicit(const AValue: TdxSmartPointer<T>): T;
    property Value: T read FValue;
  end;

  { TdxMap }

  TdxMap<TKey, TValue> = class
  strict private
    FKeyToValue: TDictionary<TKey, TValue>;
    FValueToKey: TDictionary<TValue, TKey>;
  public
    constructor Create; overload;
    constructor Create(
      AComparerKey: IEqualityComparer<TKey>;
      AComparerValue: IEqualityComparer<TValue>;
      AOwnerships: TDictionaryOwnerships = []; ACapacity: Integer = 0); overload;
    constructor Create(ACapacity: Integer; AOwnerships: TDictionaryOwnerships = []); overload;
    constructor Create(AOwnerships: TDictionaryOwnerships); overload;
    destructor Destroy; override;
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure Clear;
    procedure DeleteByKey(const AKey: TKey);
    procedure DeleteByValue(const AValue: TValue);
    function GetKey(const AValue: TValue): TKey;
    function GetValue(const AKey: TKey): TValue;
    function TryGetKey(const AValue: TValue; out AKey: TKey): Boolean;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
  end;

implementation

uses
  Math, SysUtils, RTLConsts, dxHash;


{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

{ TdxStringList }

procedure TdxStringList.AddRange(const Values: TStrings);
begin
  InsertRange(Count, Values);
end;

procedure TdxStringList.AddRange(const Values: TArray<string>);
begin
  InsertRange(Count, Values);
end;

function TdxStringList.Contains(const Value: string): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

procedure TdxStringList.DeleteRange(AIndex, ACount: Integer);
var
  I: Integer;
begin
  if ACount = 0 then
    Exit;
  for I := AIndex + ACount - 1 downto AIndex do
    Delete(I);
end;

procedure TdxStringList.InsertRange(Index: Integer; const Values: TStrings);
var
  I: Integer;
begin
  for I := 0 to Values.Count - 1 do
    Insert(I + Index, Values[I]);
end;

procedure TdxStringList.InsertRange(Index: Integer;
  const Values: TArray<string>);
var
  I: Integer;
begin
  for I := 0 to Length(Values) - 1 do
    Insert(I + Index, Values[I]);
end;

function TdxStringList.Remove(const Value: string): Integer;
begin
  Result := IndexOf(Value);
  if Result >= 0 then
    Delete(Result);
end;

function TdxStringList.ToArray: TArray<string>;
{$IFNDEF DELPHIXE}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  Result := ToStringArray;
{$ELSE}
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
{$ENDIF}
end;

{ TdxList<T> }

constructor TdxList<T>.Create;
begin
  Create(False);
end;

constructor TdxList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
end;

procedure TdxList<T>.AddRange(const Collection: TdxList<T>);
var
  I, ACount: Integer;
begin
  ACount := Collection.Count;
  if ACount = 0 then
    Exit;
  Capacity := Capacity + ACount;
  for I := 0 to ACount - 1 do
    Add(Collection[I]);
end;

procedure TdxList<T>.AddRange(const Collection: array of T);
var
  I, ACount: Integer;
begin
  ACount := Length(Collection);
  if ACount = 0 then
    Exit;
  Capacity := Capacity + ACount;
  for I := 0 to ACount - 1 do
    Add(Collection[I]);
end;

function TdxList<T>.BinarySearch(const Item: T; out Index: Integer; const AComparer: IComparer<T>): Boolean;
var
  L, H: Integer;
  mid, cmp: Integer;
begin
  Index := 0;
  if Count = 0 then
    Exit(False);

  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    mid := L + (H - L) shr 1;
    cmp := AComparer.Compare(Items[mid], Item);
    if cmp < 0 then
      L := mid + 1
    else
    begin
      H := mid - 1;
      if cmp = 0 then
        Result := True;
    end;
  end;
  Index := L;
end;

function TdxList<T>.Contains(const AItem: T): Boolean;
begin
  Result := IndexOf(AItem) >= 0;
end;

procedure TdxList<T>.DeleteRange(AIndex, ACount: Integer);
var
  I: Integer;
begin
  ACount := Min(ACount, Count - AIndex);
  for I := ACount + AIndex - 1 downto AIndex do
    Delete(I);
end;

function TdxList<T>.Extract(const AItem: T): T;
begin
  Result := T(inherited Extract(AItem));
end;

function TdxList<T>.First: T;
begin
  Result := T(inherited First);
end;

function TdxList<T>.Last: T;
begin
  Result := T(inherited Last);
end;

function TdxList<T>.GetItem(Index: Integer): T;
begin
  Result := T(inherited Items[Index]);
end;

function TdxList<T>.Max(AAction: TdxListMaxIntegerAction<T>): Integer;
var
  I: Integer;
begin
  Result := -MaxInt;
  for I := 0 to Count - 1 do
    Result := Math.Max(Result, AAction(Self[I]));
end;

procedure TdxList<T>.SetItem(Index: Integer; const Value: T);
begin
  inherited Items[Index] := Value;
end;

procedure TdxList<T>.Sort(const AComparer: IComparer<T>);
var
  AFunc: TListSortCompareFunc;
begin
  AFunc := function (Item1, Item2: Pointer): Integer
    begin
      Result := AComparer.Compare(T(TObject(Item1)), T(TObject(Item2)));
    end;

  inherited SortList(AFunc);
end;

procedure TdxList<T>.Sort;
var
  AComparer: IComparer<T>;
begin
  AComparer := TComparer<T>.Default;
  Sort(AComparer);
end;

function TdxList<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Items[I];
end;

{ TdxNotificationCollection }

procedure TdxNotificationCollection<T>.AddChangedHandler(
  AHandler: TdxNotificationCollectionChangedEvent);
begin
  FOnChanged.Add(AHandler);
end;

procedure TdxNotificationCollection<T>.Notify(Ptr: Pointer;
  Action: TListNotification);
begin
  RaiseChanged(TObject(Ptr), Action);
  inherited Notify(Ptr, Action);
end;

procedure TdxNotificationCollection<T>.RaiseChanged(const AItem: TObject;
  Action: TListNotification);
begin
  if FOnChanged.Empty then
    Exit;
  FOnChanged.Invoke(AItem, Action);
end;

procedure TdxNotificationCollection<T>.RemoveChangedHandler(
  AHandler: TdxNotificationCollectionChangedEvent);
begin
  FOnChanged.Remove(AHandler);
end;

{ TdxObjectList<T> }

constructor TdxObjectList<T>.Create;
begin
  inherited Create(True);
end;

{ TdxNotificationObjectCollection }

constructor TdxNotificationObjectCollection<T>.Create;
begin
  inherited Create(True);
end;

{ TdxOrdinalHelper }

class procedure TdxOrdinalHelper.Set2(AValue: NativeInt; ADest: Pointer);
begin
  PWord(ADest)^ := Word(AValue);
end;

class procedure TdxOrdinalHelper.Set3(AValue: NativeInt; ADest: Pointer);
begin
  Move(AValue, ADest^, 3);
end;

{$IFDEF CPUX64}
class procedure TdxOrdinalHelper.Set4(AValue: NativeInt; ADest: Pointer);
begin
  PCardinal(ADest)^ := Cardinal(AValue);
end;

class procedure TdxOrdinalHelper.Set5(AValue: NativeInt; ADest: Pointer);
begin
  Move(AValue, ADest^, 5);
end;

class procedure TdxOrdinalHelper.Set6(AValue: NativeInt; ADest: Pointer);
begin
  Move(AValue, ADest^, 6);
end;

class procedure TdxOrdinalHelper.Set7(AValue: NativeInt; ADest: Pointer);
begin
  Move(AValue, ADest^, 7);
end;
{$ENDIF}

{ TdxOrdinalHelper }

class procedure TdxOrdinalHelper.SetValue(ASize: Integer; AValue: NativeInt; ADest: Pointer);
begin
  if ASize = 1 then
    PByte(ADest)^ := Byte(AValue)
  else
    if ASize = SizeOf(NativeInt) then
      PNativeInt(ADest)^ := AValue
    else
      Setters[ASize](AValue, ADest);
end;

{ TdxStringToOrdinalMap }

constructor TdxStringToOrdinalMap.Create(AValueSize: Integer; ATableSize: Cardinal = DefaultHashTableSize);
begin
  FValueSize := AValueSize;
  FTableSize := ATableSize;
  SetLength(FTable, ATableSize);
end;

destructor TdxStringToOrdinalMap.Destroy;
begin
  Clear;
  FTable := nil;
  inherited Destroy;
end;

procedure TdxStringToOrdinalMap.Add(const AKey: string; AValue: Pointer);
begin
  InternalAdd(AKey, PNativeInt(AValue)^ and TdxOrdinalHelper.SizeMask[FValueSize], True);
end;

procedure TdxStringToOrdinalMap.AddOrSetValue(const AKey: string; AValue: Pointer);
begin
  InternalAdd(AKey, PNativeInt(AValue)^ and TdxOrdinalHelper.SizeMask[FValueSize], False);
end;

function TdxStringToOrdinalMap.TryGetValue(const AKey: string; AValue: Pointer): Boolean;
var
  AValueContainer: NativeInt;
begin
  if FCount = 0 then
    Exit(False);

  Result := InternalTryGetValue(AKey, AValueContainer);
  if Result then
    TdxOrdinalHelper.SetValue(FValueSize, AValueContainer, AValue);
end;

function TdxStringToOrdinalMap.Keys: TArray<string>;
var
  I: Integer;
  AIndex: Integer;
  AItem: PItem;
begin
  SetLength(Result, Count);
  AIndex := 0;
  for I := 0 to Length(FTable) - 1 do
  begin
    AItem := FTable[I];
    while AItem <> nil do
    begin
      Result[AIndex] := AItem.Key;
      Inc(AIndex);
      AItem := AItem.Next;
    end;
  end;
end;

function TdxStringToOrdinalMap.ContainsKey(const AKey: string): Boolean;
var
  AEntry: PItem;
  AHash: Cardinal;
begin
  if FCount = 0 then
    Exit(False);

  AHash := Hash(AKey);
  AEntry := FTable[AHash mod TableSize];
  if AEntry = nil then
    Exit(False);

  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
      Exit(True);
    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(False);
  until False;
end;

class function TdxStringToOrdinalMap.Hash(const S: string): Cardinal;
begin
  Result := dxElfHash(S);
end;

procedure TdxStringToOrdinalMap.InternalAdd(const AKey: string; AValue: NativeInt; ACheckDuplicates: Boolean);
var
  AIndex: Cardinal;
  AEntry, ATemp: PItem;
  AHash: Cardinal;
begin
  AHash := Hash(AKey);
  AIndex := AHash mod TableSize;
  AEntry := FTable[AIndex];
  if AEntry = nil then
  begin
    AEntry := NewItem(AKey, AHash, AValue);
    FTable[AIndex] := AEntry;
    Exit;
  end;

  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
    begin
      if ACheckDuplicates then
        raise EListError.CreateRes(@SGenericDuplicateItem)
      else
      begin
        RemoveValue(AEntry.Value);
        AEntry.Value := AValue;
        Exit;
      end;
    end;
    ATemp := AEntry.Next;
    if ATemp = nil then
    begin
      ATemp := NewItem(AKey, AHash, AValue);
      AEntry.Next := ATemp;
      Exit;
    end;
    AEntry := ATemp;
  until False;
end;

procedure TdxStringToOrdinalMap.InternalRemove(AItem: PItem);
begin
  RemoveValue(AItem.Value);
  Dispose(AItem);
end;

function TdxStringToOrdinalMap.InternalTryGetValue(const AKey: string; out AValue: NativeInt): Boolean;
var
  AEntry: PItem;
  AHash: Cardinal;
begin
  AHash := Hash(AKey);
  AEntry := FTable[AHash mod TableSize];
  if AEntry = nil then
    Exit(False);

  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
    begin
      AValue := AEntry.Value;
      Exit(True);
    end;

    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(False);
  until False;
end;

procedure TdxStringToOrdinalMap.RemoveValue(AValue: NativeInt);
begin
end;

procedure TdxStringToOrdinalMap.Clear;
var
  I: Integer;
  AItem, ATemp: PItem;
begin
  if FCount = 0 then
    Exit;

  for I := Low(FTable) to High(FTable) do
  begin
    AItem := FTable[I];
    while AItem <> nil do
    begin
      ATemp := AItem;
      AItem := AItem.Next;
      InternalRemove(ATemp);
    end;
    FTable[I] := nil;
  end;
  FCount := 0;
end;

function TdxStringToOrdinalMap.NewItem(const AKey: string; AHash: Cardinal; const AValue: NativeInt): PItem;
begin
  New(Result);
  Result.Key := AKey;
  Result.Hash := AHash;
  Result.Value := AValue;
  Result.Next := nil;
  Inc(FCount);
end;

function TdxStringToOrdinalMap.Remove(const AKey: string): Boolean;
var
  AHash: Cardinal;
  AIndex: Integer;
  AEntry: PItem;
  ALink: PPointer;
begin
  if FCount = 0 then
    Exit(False);

  AHash := Hash(AKey);
  AIndex := AHash mod TableSize;
  AEntry := FTable[AIndex];
  if AEntry = nil then
    Exit(False);

  ALink := @FTable[AIndex];
  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
    begin
      ALink^ := AEntry.Next;
      InternalRemove(AEntry);
      Dec(FCount);
      Exit(True);
    end;
    ALink := @AEntry.Next;

    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(False);

  until False;
end;

{ TdxNamedOrdinalDictionary }

constructor TdxNamedOrdinalDictionary.Create;
begin
  FMap := CreateMap;
end;

destructor TdxNamedOrdinalDictionary.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

procedure TdxNamedOrdinalDictionary.Clear;
begin
  FMap.Clear;
end;

function TdxNamedOrdinalDictionary.ContainsKey(const AKey: string): Boolean;
begin
  Result := FMap.ContainsKey(AKey);
end;

function TdxNamedOrdinalDictionary.GetCount: Integer;
begin
  Result := FMap.Count;
end;


procedure TdxNamedOrdinalDictionary.Remove(const AKey: string);
begin
  FMap.Remove(AKey);
end;

function TdxNamedOrdinalDictionary.Keys: TArray<string>;
begin
  Result := Map.Keys;
end;

{ TdxNamedOrdinalDictionary<T> }

function TdxNamedOrdinalDictionary<T>.GetItem(const AKey: string): T;
begin
  if not TryGetValue(AKey, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);
end;

function TdxNamedOrdinalDictionary<T>.GetValueDef(const AKey: string; ADefaultValue: T): T;
begin
  if not TryGetValue(AKey, Result) then
    Result := ADefaultValue;
end;

procedure TdxNamedOrdinalDictionary<T>.SetItem(const AKey: string;
  const Value: T);
begin
  AddOrSetValue(AKey, Value);
end;

function TdxNamedOrdinalDictionary<T>.TryGetValue(const AKey: string; out AValue: T): Boolean;
begin
  Result := Map.TryGetValue(AKey, @AValue);
end;

procedure TdxNamedOrdinalDictionary<T>.Add(const AKey: string; AValue: T);
begin
  Map.Add(AKey, @AValue);
end;

procedure TdxNamedOrdinalDictionary<T>.AddOrSetValue(const AKey: string; AValue: T);
begin
  Map.AddOrSetValue(AKey, @AValue);
end;

function TdxNamedOrdinalDictionary<T>.ContainsValue(const Value: T): Boolean;
var
  AComparer: IEqualityComparer<T>;
  AKeys: TArray<string>;
  AKey: string;
begin
  AComparer := TEqualityComparer<T>.Default;
  AKeys := Keys;
  for AKey in AKeys do
    if AComparer.Equals(Value, Items[AKey]) then
      Exit(True);
  Result := False;
end;

function TdxNamedOrdinalDictionary<T>.CreateMap: TdxStringToOrdinalMap;
begin
  Result := TdxStringToOrdinalMap.Create(SizeOf(T));
end;

{ TdxNamedObjectDictionary<T>.TMap }

procedure TdxNamedObjectDictionary<T>.TMap.RemoveValue(AValue: NativeInt);
begin
  if OwnsObjects then
    TObject(AValue).Free;
end;

{ TdxNamedObjectDictionary<T> }

procedure TdxNamedObjectDictionary<T>.Add(const AKey: string; AValue: T);
begin
  Map.Add(AKey, @AValue);
end;

procedure TdxNamedObjectDictionary<T>.AddOrSetValue(const AKey: string; AValue: T);
begin
  Map.AddOrSetValue(AKey, @AValue);
end;

constructor TdxNamedObjectDictionary<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  OwnsObjects := AOwnsObjects;
end;

function TdxNamedObjectDictionary<T>.CreateMap: TdxStringToOrdinalMap;
begin
  Result := TMap.Create(SizeOf(T));
end;

procedure TdxNamedObjectDictionary<T>.SetItem(const AKey: string; const Value: T);
begin
  AddOrSetValue(AKey, Value);
end;

procedure TdxNamedObjectDictionary<T>.SetOwnsObjects(const Value: Boolean);
begin
  TMap(Map).OwnsObjects := Value;
end;

function TdxNamedObjectDictionary<T>.TryGetValue(const AKey: string;
  out AValue: T): Boolean;
begin
  Result := Map.TryGetValue(AKey, @AValue);
end;

function TdxNamedObjectDictionary<T>.Values: TArray<T>;
var
  I, AIndex: Integer;
begin
  SetLength(Result, Count);
  AIndex := 0;
  I := 0;
  while AIndex < Length(Map.Table) - 1 do
  begin
    Inc(AIndex);
    if Map.Table[AIndex] <> nil then
    begin
      Result[I] := T(TObject(Map.Table[AIndex].Value));
      Inc(I);
    end;
  end;
end;

function TdxNamedObjectDictionary<T>.GetItem(const AKey: string): T;
begin
  if not TryGetValue(AKey, Result) then
    Result := Default(T);
end;

function TdxNamedObjectDictionary<T>.GetOwnsObjects: Boolean;
begin
  Result := TMap(Map).OwnsObjects;
end;

{ TdxNamedDelegateDictionary<TValue>.TItem }

constructor TdxNamedDelegateDictionary<TValue>.TItem.Create(const AKey: string; AHash: Cardinal; const AValue: TValue);
begin
  FKey := AKey;
  FValue := AValue;
  FHash := AHash
end;

function TdxNamedDelegateDictionary<TValue>.TItem.IsEqualTo(AItem: TItem): Boolean;
begin
  Result := (FHash = AItem.Hash) and (FKey = AItem.Key);
end;

{ TdxNamedDelegateDictionary }

constructor TdxNamedDelegateDictionary<TValue>.Create;
begin
  SetLength(FTable, DefaultHashTableSize);
end;

destructor TdxNamedDelegateDictionary<TValue>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxNamedDelegateDictionary<TValue>.Add(const AKey: string; AValue: TValue);
begin
  InternalAdd(AKey, AValue, True);
end;

procedure TdxNamedDelegateDictionary<TValue>.AddOrSetValue(const AKey: string; AValue: TValue);
begin
  InternalAdd(AKey, AValue, False);
end;

procedure TdxNamedDelegateDictionary<TValue>.Clear;
var
  I: Integer;
  AItem, ATemp: TItem;
begin
  if FCount = 0 then
    Exit;

  for I := Low(FTable) to High(FTable) do
  begin
    AItem := FTable[I];
    while AItem <> nil do
    begin
      ATemp := AItem;
      AItem := AItem.Next;
      ATemp.Free;
    end;
    FTable[I] := nil;
  end;
  FCount := 0;
end;

function TdxNamedDelegateDictionary<TValue>.ContainsKey(const AKey: string): Boolean;
var
  AEntry: TItem;
  AHash: Cardinal;
begin
  if FCount = 0 then
    Exit(False);

  AHash := Hash(AKey);
  AEntry := FTable[AHash mod DefaultHashTableSize];
  if AEntry = nil then
    Exit(False);

  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
      Exit(True);
    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(False);
  until False;
end;

class function TdxNamedDelegateDictionary<TValue>.Hash(const AKey: string): Cardinal;
begin
  Result := dxElfHash(AKey);
end;

procedure TdxNamedDelegateDictionary<TValue>.InternalAdd(const AKey: string; const AValue: TValue; ACheckDuplicates: Boolean);
var
  AIndex: Cardinal;
  AEntry, ATemp: TItem;
  AHash: Cardinal;
begin
  AHash := Hash(AKey);
  AIndex := AHash mod DefaultHashTableSize;
  AEntry := FTable[AIndex];
  if AEntry = nil then
  begin
    FTable[AIndex] := TItem.Create(AKey, AHash, AValue);
    Inc(FCount);
    Exit;
  end;

  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
    begin
      if ACheckDuplicates then
        raise EListError.CreateRes(@SGenericDuplicateItem)
      else
      begin
        AEntry.Value := AValue;
        Exit;
      end;
    end;
    ATemp := AEntry.Next;
    if ATemp = nil then
    begin
      AEntry.Next := TItem.Create(AKey, AHash, AValue);
      Inc(FCount);
      Exit;
    end;
    AEntry := ATemp;
  until False;
end;

function TdxNamedDelegateDictionary<TValue>.Keys: TArray<string>;
var
  I: Integer;
  AIndex: Integer;
  AItem: TItem;
begin
  SetLength(Result, Count);
  AIndex := 0;
  for I := 0 to Length(FTable) - 1 do
  begin
    AItem := FTable[I];
    while AItem <> nil do
    begin
      Result[AIndex] := AItem.Key;
      Inc(AIndex);
      AItem := AItem.Next;
    end;
  end;
end;

function TdxNamedDelegateDictionary<TValue>.TryGetValue(const AKey: string; out AValue: TValue): Boolean;
var
  AEntry: TItem;
  AHash: Cardinal;
begin
  AHash := Hash(AKey);
  AEntry := FTable[AHash mod DefaultHashTableSize];
  if AEntry = nil then
    Exit(False);

  repeat
    if (AHash = AEntry.Hash) and (AKey = AEntry.Key) then
    begin
      AValue := AEntry.Value;
      Exit(True);
    end;

    AEntry := AEntry.Next;
    if AEntry = nil then
      Exit(False);
  until False;
end;

{ TdxEnumeratedMap<TValue> }

constructor TdxEnumeratedMap<TValue>.Create(ATableSize: Cardinal);
var
  I: Integer;
begin
  FTableSize := ATableSize;
  SetLength(FKeyTable, ATableSize);
  for I := 0 to ATableSize - 1 do
    FKeyTable[I] := False;
  SetLength(FValueTable, ATableSize);
end;

destructor TdxEnumeratedMap<TValue>.Destroy;
begin
  Clear;
  FValueTable := nil;
  FKeyTable := nil;
  inherited Destroy;
end;

function TdxEnumeratedMap<TValue>.GetValues: TArray<TValue>;
var
  I, AIndex: Integer;
begin
  if FCount = 0 then
    Exit(nil);

  SetLength(Result, FCount);
  AIndex := 0;
  for I := 0 to FTableSize - 1 do
    if FKeyTable[I] then
    begin
      Result[AIndex] := FValueTable[I];
      Inc(AIndex);
    end;
end;

procedure TdxEnumeratedMap<TValue>.Add(AKey: Cardinal; const AValue: TValue);
begin
  InternalAdd(AKey, AValue, True);
end;

procedure TdxEnumeratedMap<TValue>.AddOrSetValue(AKey: Cardinal; const AValue: TValue);
begin
  InternalAdd(AKey, AValue, False);
end;

procedure TdxEnumeratedMap<TValue>.Clear;
var
  I: Integer;
begin
  for I := 0 to FTableSize - 1 do
    if FKeyTable[I] then
    begin
      FKeyTable[I] := False;
      InternalRemove(I);
    end;
  FCount := 0;
end;

function TdxEnumeratedMap<TValue>.ContainsKey(AKey: Cardinal): Boolean;
begin
  if AKey >= FTableSize then
    Exit(False);
  Result := FKeyTable[AKey];
end;

procedure TdxEnumeratedMap<TValue>.InternalAdd(AKey: Cardinal; const AValue: TValue;
  ACheckDuplicates: Boolean);
begin
  if AKey >= FTableSize then
    raise EArgumentOutOfRangeException.CreateFmt('Key = %d', [AKey]);
  if not FKeyTable[AKey] then
    Inc(FCount)
  else
    if ACheckDuplicates then
      raise EListError.CreateRes(@SGenericDuplicateItem)
    else
      if not TEqualityComparer<TValue>.Default.Equals(FValueTable[AKey], AValue) then
        InternalRemove(AKey);
  FKeyTable[AKey] := True;
  FValueTable[AKey] := AValue;
end;

procedure TdxEnumeratedMap<TValue>.InternalRemove(AKey: Cardinal);
begin
  FValueTable[AKey] := Default(TValue);
end;

procedure TdxEnumeratedMap<TValue>.Remove(AKey: Cardinal);
begin
  if AKey >= FTableSize then
    Exit;
  if FKeyTable[AKey]  then
  begin
    InternalRemove(AKey);
    FKeyTable[AKey] := False;
    Dec(FCount);
  end;
end;

function TdxEnumeratedMap<TValue>.TryGetValue(AKey: Cardinal; out AValue: TValue): Boolean;
begin
  if AKey >= FTableSize then
    Exit(False);
  Result := FKeyTable[AKey];
  if Result then
    AValue := FValueTable[AKey];
end;

{ TdxEnumeratedDictionary<TValue> }

constructor TdxEnumeratedDictionary<TValue>.Create(AKeySize: Cardinal; ATypeInfo: PTypeInfo);
var
  ATypeData: PTypeData;
begin
  if (ATypeInfo = nil) or (ATypeInfo.Kind <> tkEnumeration) then
    raise EArgumentException.Create('Unsupported enumeration type');
  ATypeData := GetTypeData(ATypeInfo);
  if ATypeData.MinValue < 0 then
    raise EArgumentException.Create('Unsupported enumeration type');

  FMinValue := ATypeData.MinValue;
  FMaxValue := ATypeData.MaxValue;
  FKeySize := AKeySize;
  FMap := CreateMap(FMaxValue - FMinValue + 1);
end;

destructor TdxEnumeratedDictionary<TValue>.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

function TdxEnumeratedDictionary<TValue>.CreateMap(ATableSize: Cardinal): TdxEnumeratedMap<TValue>;
begin
  Result := TdxEnumeratedMap<TValue>.Create(ATableSize);
end;

function TdxEnumeratedDictionary<TValue>.GetCount: Integer;
begin
  Result := Map.Count;
end;

function TdxEnumeratedDictionary<TValue>.GetValues: TArray<TValue>;
begin
  Result := Map.Values;
end;

{ TdxEnumeratedDictionary<TEnumeration, TValue> }

constructor TdxEnumeratedDictionary<TEnumeration, TValue>.Create;
var
  ATypeInfo: PTypeInfo;
begin
  ATypeInfo := TypeInfo(TEnumeration);
  inherited Create(SizeOf(TEnumeration), ATypeInfo);
end;

procedure TdxEnumeratedDictionary<TEnumeration, TValue>.Add(AKey: TEnumeration; const AValue: TValue);
begin
  Map.Add(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize], AValue);
end;

procedure TdxEnumeratedDictionary<TEnumeration, TValue>.AddOrSetValue(AKey: TEnumeration; const AValue: TValue);
begin
  Map.AddOrSetValue(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize], AValue);
end;

function TdxEnumeratedDictionary<TEnumeration, TValue>.ContainsKey(AKey: TEnumeration): Boolean;
begin
  Result := Map.ContainsKey(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize]);
end;

procedure TdxEnumeratedDictionary<TEnumeration, TValue>.Remove(AKey: TEnumeration);
begin
  Map.Remove(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize]);
end;

function TdxEnumeratedDictionary<TEnumeration, TValue>.TryGetValue(AKey: TEnumeration;
  out AValue: TValue): Boolean;
begin
  Result := Map.TryGetValue(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize], AValue);
end;

function TdxEnumeratedDictionary<TEnumeration, TValue>.GetItem(AKey: TEnumeration): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);
end;

function TdxEnumeratedDictionary<TEnumeration, TValue>.GetKeys: TArray<TEnumeration>;
var
  I, AIndex: Integer;
begin
  if Count = 0 then
    Exit(nil);

  SetLength(Result, Count);
  AIndex := 0;
  for I := 0 to Map.TableSize - 1 do
    if Map.KeyTable[I] then
    begin
      TdxOrdinalHelper.SetValue(SizeOf(TEnumeration), I, @Result[AIndex]);
      Inc(AIndex);
    end;
end;

procedure TdxEnumeratedDictionary<TEnumeration, TValue>.SetItem(AKey: TEnumeration; const Value: TValue);
begin
  AddOrSetValue(AKey, Value)
end;

{ TdxEnumeratedObjectDictionary<TEnumeration, TValue>.TMap }

procedure TdxEnumeratedObjectDictionary<TEnumeration, TValue>.TMap.InternalRemove(AKey: Cardinal);
begin
  if OwnsObjects then
    ValueTable[AKey].Free;
  inherited InternalRemove(AKey);
end;

{ TdxEnumeratedObjectDictionary<TEnumeration, TValue> }

constructor TdxEnumeratedObjectDictionary<TEnumeration, TValue>.Create(AOwnsObjects: Boolean);
begin
  inherited Create(SizeOf(TEnumeration), TypeInfo(TEnumeration));
  OwnsObjects := AOwnsObjects;
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.CreateMap(ATableSize: Cardinal): TdxEnumeratedMap<TValue>;
begin
  TMap(Result) := TMap.Create(ATableSize);
end;

procedure TdxEnumeratedObjectDictionary<TEnumeration, TValue>.Add(AKey: TEnumeration; AValue: TValue);
begin
  Map.Add(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize], AValue);
end;

procedure TdxEnumeratedObjectDictionary<TEnumeration, TValue>.AddOrSetValue(AKey: TEnumeration;
  AValue: TValue);
begin
  Map.AddOrSetValue(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize], AValue);
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.ContainsKey(AKey: TEnumeration): Boolean;
begin
  Result := Map.ContainsKey(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize]);
end;

procedure TdxEnumeratedObjectDictionary<TEnumeration, TValue>.Remove(AKey: TEnumeration);
begin
  Map.Remove(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize]);
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.TryGetValue(AKey: TEnumeration;
  out AValue: TValue): Boolean;
var
  AValueObject: TObject;
begin
  Result := Map.TryGetValue(PCardinal(@AKey)^ and TdxOrdinalHelper.SizeMask[KeySize], AValueObject);
  if Result then
    TObject(AValue) := AValueObject;
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.GetMap: TMap;
begin
  Result := TMap(inherited Map);
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.GetOwnsObjects: Boolean;
begin
  Result := Map.OwnsObjects;
end;

procedure TdxEnumeratedObjectDictionary<TEnumeration, TValue>.SetOwnsObjects(const Value: Boolean);
begin
  Map.OwnsObjects := Value;
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.GetItem(AKey: TEnumeration): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);
end;

function TdxEnumeratedObjectDictionary<TEnumeration, TValue>.GetKeys: TArray<TEnumeration>;
var
  I, AIndex: Integer;
begin
  if Count = 0 then
    Exit(nil);

  SetLength(Result, Count);
  AIndex := 0;
  for I := 0 to Map.TableSize - 1 do
    if Map.KeyTable[I] then
    begin
      TdxOrdinalHelper.SetValue(SizeOf(TEnumeration), I, @Result[AIndex]);
      Inc(AIndex);
    end;
end;

procedure TdxEnumeratedObjectDictionary<TEnumeration, TValue>.SetItem(AKey: TEnumeration; const Value: TValue);
begin
  AddOrSetValue(AKey, Value)
end;

{ TdxOrdinalList }

constructor TdxOrdinalList.Create(ACapacity: Integer = 0);
begin
  inherited Create;
  Capacity := ACapacity;
end;

destructor TdxOrdinalList.Destroy;
begin
  Clear;
  Capacity := 0;
  inherited Destroy;
end;

function TdxOrdinalList.Add(AItem: NativeUInt): Integer;
begin
  Result := Count;
  if Result = Capacity then
    Grow;
  FList[Result] := AItem;
  Inc(FCount);
  Notify(AItem, cnAdded);
end;

procedure TdxOrdinalList.Clear;
begin
  Count := 0;
  Capacity := 0;
end;

function TdxOrdinalList.BinarySearch(AItem: NativeUInt; out AIndex: Integer;
  const AComparer: IComparer<NativeUInt>): Boolean;
var
  L, H: Integer;
  AMedian, ACompare: Integer;
begin
  AIndex := 0;
  if Count = 0 then
    Exit(False);

  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    AMedian := L + (H - L) shr 1;
    ACompare := AComparer.Compare(FList[AMedian], AItem);
    if ACompare < 0 then
      L := AMedian + 1
    else
    begin
      H := AMedian - 1;
      if ACompare = 0 then
        Result := True;
    end;
  end;
  AIndex := L;
end;

function TdxOrdinalList.Contains(AItem: NativeUInt): Boolean;
begin
  Result := IndexOf(AItem) >= 0;
end;

procedure TdxOrdinalList.Delete(AIndex: Integer);
var
  AItem: NativeUInt;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);

  AItem := FList[AIndex];
  DoDelete(AIndex);
  Notify(AItem, cnRemoved);
end;

procedure TdxOrdinalList.DeleteRange(AIndex, ACount: Integer);
var
  AOldItems: TArray<NativeUInt>;
  I: Integer;
begin
  CheckDeleteRange(AIndex, ACount);

  SetLength(AOldItems, ACount);
  System.Move(FList[AIndex], AOldItems[0], ACount * SizeOf(NativeUInt));

  DoDeleteRange(AIndex, ACount);

  for I := 0 to Length(AOldItems) - 1 do
    Notify(AOldItems[I], cnRemoved);
end;

function TdxOrdinalList.Empty: Boolean;
begin
  Result := (Self = nil) or (Count = 0);
end;

procedure TdxOrdinalList.Exchange(AIndex1, AIndex2: Integer);
var
  ATempItem: NativeUInt;
begin
  if (AIndex1 < 0) or (AIndex1 >= Count) then
    TList.Error(@SListIndexError, AIndex1);
  if (AIndex2 < 0) or (AIndex2 >= Count) then
    TList.Error(@SListIndexError, AIndex2);
  ATempItem := FList[AIndex1];
  FList[AIndex1] := FList[AIndex2];
  FList[AIndex2] := ATempItem;
end;

function TdxOrdinalList.First: NativeUInt;
begin
  Result := GetItem(0);
end;

procedure TdxOrdinalList.CheckDeleteRange(AIndex, ACount: Integer);
begin
  if (AIndex < 0) or (ACount < 0) or (AIndex + ACount > Count) or (AIndex + ACount < 0) then
    TList.Error(@SArgumentOutOfRange, AIndex);
end;

procedure TdxOrdinalList.DoDeleteRange(AIndex, ACount: Integer);
var
  ANextIndex: Integer;
begin
  if ACount = 0 then
    Exit;

  ANextIndex := AIndex + ACount;
  if ANextIndex < FCount then
    System.Move(FList[ANextIndex], FList[AIndex], (FCount - ANextIndex) * SizeOf(NativeUInt));

  Dec(FCount, ACount);
end;

procedure TdxOrdinalList.DoDelete(AIndex: Integer);
begin
  Dec(FCount);
  if AIndex < FCount then
    System.Move(FList[AIndex + 1], FList[AIndex], (FCount - AIndex) * SizeOf(NativeUInt));
  FList[FCount] := 0;
end;

function TdxOrdinalList.GetItem(AIndex: Integer): NativeUInt;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);
  Result := FList[AIndex];
end;

procedure TdxOrdinalList.Grow;
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

procedure TdxOrdinalList.Notify(const AItem: NativeUInt; AAction: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, AItem, AAction);
end;

function TdxOrdinalList.IndexOf(AItem: NativeUInt): Integer;
begin
  for Result := 0 to FCount - 1 do
    if AItem = FList[Result]  then
      Exit;
  Result := -1;
end;

function TdxOrdinalList.LastIndexOf(AItem: NativeUInt): Integer;
begin
  for Result := FCount - 1 downto 0 do
    if AItem = FList[Result]  then
      Exit;
  Result := -1;
end;

procedure TdxOrdinalList.Insert(AIndex: Integer; AItem: NativeUInt);
begin
  if (AIndex < 0) or (AIndex > Count) then
    TList.Error(@SListIndexError, AIndex);
  if Count = Capacity then
    Grow;
  if AIndex < Count then
    System.Move(FList[AIndex], FList[AIndex + 1], (Count - AIndex) * SizeOf(NativeUInt));
  FList[AIndex] := AItem;
  Inc(FCount);
  Notify(AItem, cnAdded);
end;

function TdxOrdinalList.Last: NativeUInt;
begin
  Result := GetItem(Count - 1);
end;

procedure TdxOrdinalList.Move(ACurrentIndex, ANewIndex: Integer);
var
  AMovedItem: NativeUInt;
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

function TdxOrdinalList.Remove(AItem: NativeUInt): Integer;
begin
  Result := IndexOf(AItem);
  if Result >= 0 then
  begin
    DoDelete(Result);
    Notify(AItem, cnRemoved);
  end;
end;

procedure TdxOrdinalList.Reverse;
var
  ATempItem: NativeUInt;
  ALeft, ARight: PNativeUInt;
begin
  if Count < 2 then
    Exit;
  ALeft  := @FList[0];
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

procedure TdxOrdinalList.SetCapacity(AValue: Integer);
begin
  if (AValue < Count) {$IFNDEF DELPHI16} or (AValue > MaxListSize) {$ENDIF} then
    TList.Error(@SListCapacityError, AValue);
  if AValue <> Capacity then
  begin
    SetLength(FList, AValue);
    FCapacity := AValue;
  end;
end;

procedure TdxOrdinalList.SetCount(AValue: Integer);
begin
  if (AValue < 0) {$IFNDEF DELPHI16} or (AValue > MaxListSize) {$ENDIF} then
    TList.Error(@SListCountError, AValue);
  if AValue > Capacity then
    SetCapacity(AValue);
  if AValue < Count then
    DeleteRange(AValue, Count - AValue);
  FCount := AValue;
end;

procedure TdxOrdinalList.SetItem(AIndex: Integer; AItem: NativeUInt);
var
  AOldItem: NativeUInt;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);

  AOldItem := FList[AIndex];
  FList[AIndex] := AItem;

  Notify(AOldItem, cnRemoved);
  Notify(AItem, cnAdded);
end;

function SortOrdinals(Item1, Item2: Pointer): Integer;
begin
  Result := NativeInt(Item1) - NativeInt(Item2);
end;

procedure TdxOrdinalList.Sort;
begin
  dxQuickSortList(@FList[0], Count, SortOrdinals, False);
end;

procedure TdxOrdinalList.Sort(ACompare: TCompareItems; AMultiThreaded: Boolean = False);
begin
  dxQuickSortList(@FList[0], Count, ACompare, AMultiThreaded);
end;

procedure TdxOrdinalList.Sort(ACompare: TListSortCompare; AMultiThreaded: Boolean = False);
begin
  dxQuickSortList(@FList[0], Count, ACompare, AMultiThreaded);
end;

function TdxOrdinalList.Extract(AItem: NativeUInt; ADirection: TList.TDirection = FromBeginning): NativeUInt;
var
  I: Integer;
begin
  Result := 0;

  if ADirection = FromBeginning then
    I := IndexOf(AItem)
  else
    I := LastIndexOf(AItem);

  if I >= 0 then
  begin
    Result := AItem;
    DoDelete(I);
    Notify(Result, cnExtracted);
  end;
end;

function TdxOrdinalList.ExtractByIndex(AIndex: Integer): NativeUInt;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    TList.Error(@SListIndexError, AIndex);

  Result := FList[AIndex];
  DoDelete(AIndex);
  Notify(Result, cnExtracted);
end;

{ TdxOrdinalList<T> }

constructor TdxOrdinalList<T>.Create(ACapacity: Integer = 0);
var
  ATypeInfo: PTypeInfo;
begin
  ATypeInfo := TypeInfo(T);
  if (ATypeInfo = nil) or
    not (ATypeInfo.Kind in [tkInteger, tkChar, tkEnumeration, tkClass, tkWChar, tkPointer]) then
    raise EArgumentException.Create('Unsupported ordinal type');
  FItemSize  := SizeOf(T);
  FValueMask := TdxOrdinalHelper.SizeMask[FItemSize];
  inherited Create(ACapacity);
end;

constructor TdxOrdinalList<T>.Create(ACollection: TdxOrdinalList<T>);
begin
  Create;
  AddRange(ACollection);
end;

procedure TdxOrdinalList<T>.AddRange(AList: TdxOrdinalList<T>);
var
  I, ACount: Integer;
begin
  ACount := AList.Count;
  if ACount = 0 then
    Exit;
  Capacity := Count + ACount;
  for I := 0 to ACount - 1 do
    Add(AList[I]);
end;

procedure TdxOrdinalList<T>.AddRange(const AArray: TArray<T>);
var
  I, ACount: Integer;
begin
  ACount := Length(AArray);
  if ACount = 0 then
    Exit;
  Capacity := Count + ACount;
  for I := 0 to ACount - 1 do
    Add(AArray[I]);
end;

function TdxOrdinalList<T>.BinarySearch(AItem: T; out AIndex: Integer): Boolean;
var
  AHi, AMedian: Integer;
  ACompareResult: Integer;
  APredicate: IComparer<T>;
begin
  APredicate := TComparer<T>.Default;
  AIndex := 0;
  AHi := Count - 1;
  while AIndex <= AHi do
  begin
    AMedian := AIndex + ((AHi - AIndex) shr 1);
    ACompareResult := APredicate.Compare(Self[AMedian], AItem);
    if ACompareResult = 0 then
    begin
      AIndex := AMedian;
      Exit(True);
    end
    else
      if ACompareResult < 0 then
        AIndex := AMedian + 1
      else
        AHi := AMedian - 1;
  end;
  Result := False;
end;

function TdxOrdinalList<T>.Contains(AItem: T): Boolean;
begin
  Result := inherited Contains(PNativeUInt(@AItem)^ and ValueMask);
end;

function TdxOrdinalList<T>.Add(AItem: T): Integer;
begin
  Result := inherited Add(PNativeUInt(@AItem)^ and ValueMask);
end;

function TdxOrdinalList<T>.Extract(AItem: T; ADirection: TList.TDirection): T;
begin
  TdxOrdinalHelper.SetValue(ItemSize, inherited Extract(PNativeUInt(@AItem)^ and ValueMask, ADirection), @Result);
end;

function TdxOrdinalList<T>.ExtractByIndex(AIndex: Integer): T;
begin
  TdxOrdinalHelper.SetValue(ItemSize, inherited ExtractByIndex(AIndex), @Result);
end;

function TdxOrdinalList<T>.First: T;
begin
  TdxOrdinalHelper.SetValue(ItemSize, inherited GetItem(0), @Result);
end;

procedure TdxOrdinalList<T>.Insert(AIndex: Integer; AItem: NativeUInt);
begin
  inherited Insert(AIndex, PNativeUInt(@AItem)^ and ValueMask);
end;

function TdxOrdinalList<T>.GetItem(AIndex: Integer): T;
begin
  TdxOrdinalHelper.SetValue(ItemSize, inherited GetItem(AIndex), @Result);
end;

function TdxOrdinalList<T>.IndexOf(AItem: T): Integer;
begin
  Result := inherited IndexOf(PNativeUInt(@AItem)^ and ValueMask);
end;

function TdxOrdinalList<T>.Last: T;
begin
  TdxOrdinalHelper.SetValue(ItemSize, inherited GetItem(Count - 1), @Result);
end;

function TdxOrdinalList<T>.LastIndexOf(AItem: T): Integer;
begin
  Result := inherited LastIndexOf(PNativeUInt(@AItem)^ and ValueMask);
end;

function TdxOrdinalList<T>.Remove(AItem: T): Integer;
begin
  Result := inherited Remove(PNativeUInt(@AItem)^ and ValueMask);
end;

procedure TdxOrdinalList<T>.SetItem(AIndex: Integer; AValue: T);
begin
  inherited SetItem(AIndex, PNativeUInt(@AValue)^ and ValueMask);
end;

function TdxOrdinalList<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Items[I];
end;

{ TdxObjectStack<T> }

procedure TdxObjectStack<T>.Clear;
begin
  if FOwnsObjects then
  begin
    while Count > 0 do
      Pop
  end
  else
    List.Clear;
end;

constructor TdxObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

destructor TdxObjectStack<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TdxObjectStack<T>.Extract: T;
begin
  Result := Peek;
  List.Extract(TObject(Result));
end;

function TdxObjectStack<T>.Peek: T;
begin
  Result := T(inherited Peek);
end;

procedure TdxObjectStack<T>.Pop;
var
  AObject: TObject;
begin
  AObject := inherited Pop;
  if OwnsObjects then
    AObject.Free;
end;

function TdxObjectStack<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := T(TObject(List[I]));
end;

{ TdxIntegerStack }

procedure TdxIntegerStack.Clear;
begin
  List.Clear;
end;

function TdxIntegerStack.Pop: NativeInt;
begin
  Result := Integer(inherited Pop);
end;

function TdxIntegerStack.Push(AItem: NativeInt): Pointer;
begin
  Result := inherited Push(Pointer(AItem));
end;

function TdxIntegerStack.Peek: NativeInt;
begin
  Result := NativeInt(inherited Peek);
end;

{ TdxTopologicalSorterNode }

constructor TdxTopologicalSorterNode.Create(AOneBasedSuccessorIndex: Integer; ANext: TdxTopologicalSorterNode);
begin
  inherited Create;
  FOneBasedSuccessorIndex := AOneBasedSuccessorIndex;
  FNext := ANext
end;

{ TdxTopologicalSorter<T>.TDefaultDependencyCalculator }

constructor TdxTopologicalSorter<T>.TDefaultDependencyCalculator.Create(
  ASourceObjects: TdxList<T>; AComparer: IComparer<T>);
begin
  FSourceObjects := ASourceObjects;
  FComparer := AComparer;
end;

function TdxTopologicalSorter<T>.TDefaultDependencyCalculator.IsDependOn(Left,
  Right: Integer): Boolean;
begin
  Result := FComparer.Compare(FSourceObjects[Left], FSourceObjects[Right]) > 0;
end;

{ TdxTopologicalSorter<T> }

constructor TdxTopologicalSorter<T>.Create;
begin
  inherited Create;
  FNodeList := TdxObjectList<TdxTopologicalSorterNode>.Create;
end;

destructor TdxTopologicalSorter<T>.Destroy;
begin
  FreeAndNil(FNodeList);
  inherited Destroy;
end;

function TdxTopologicalSorter<T>.DoSort(ASourceObjects: TdxList<T>;
  AComparer: IComparer<T>): TdxList<T>;
var
  ADependencyCalculator: TDefaultDependencyCalculator;
  AInnerComparer: IComparer<T>;
begin
  if AComparer <> nil then
    AInnerComparer := AComparer
  else
    AInnerComparer := TComparer<T>.Default;
  ADependencyCalculator := TDefaultDependencyCalculator.Create(ASourceObjects, AInnerComparer);
  Result := DoSort(ASourceObjects, ADependencyCalculator.IsDependOn);
end;

procedure TdxTopologicalSorter<T>.AppendRelation(ASuccessorIndex,
  APredecessorIndex: Integer);
var
  AOneBasedPredecessorIndex, AOneBasedSuccessorIndex: Integer;
begin
  AOneBasedPredecessorIndex := APredecessorIndex + 1;
  AOneBasedSuccessorIndex := ASuccessorIndex + 1;
  Inc(QLink[AOneBasedSuccessorIndex]);
  FNodeList.Add(TdxTopologicalSorterNode.Create(AOneBasedSuccessorIndex, Nodes[AOneBasedPredecessorIndex]));
  Nodes[AOneBasedPredecessorIndex] := FNodeList.Last;
end;

procedure TdxTopologicalSorter<T>.CalculateRelations(ASourceObjects: TdxList<T>;
  const AIsDependOn: TdxTopologicalSorterIsDependOnDelegate);
var
  N, Y, X: Integer;
begin
  N := ASourceObjects.Count;
  for Y := N - 1 downto 0 do
  begin
    for X := N - 1 downto 0 do
      if AIsDependOn(Y, X) then
        AppendRelation(Y, X);
  end;
end;

function TdxTopologicalSorter<T>.CreateVirtualNoPredecessorsItemList: Integer;
var
  I, N: Integer;
begin
  Result := 0;
  N := Length(QLink);
  for I := 1 to N - 1 do
  begin
    if QLink[I] = 0 then
    begin
      QLink[Result] := I;
      Result := I;
    end;
  end;
end;

procedure TdxTopologicalSorter<T>.Initialize(N: Integer);
begin
  SetLength(FQLink, N + 1);
  SetLength(FNodes, N + 1);
end;

function TdxTopologicalSorter<T>.ProcessNodes(
  ALastNoPredecessorItemIndex: Integer; ASourceObjects: TdxList<T>): TdxList<T>;
var
  F, AItemsLeft: Integer;
  ANode: TdxTopologicalSorterNode;
begin
  AItemsLeft := ASourceObjects.Count;

  F := QLink[0];
  Result := TdxList<T>.Create;
  Result.Capacity := AItemsLeft;
  while F > 0 do
  begin
    Result.Add(ASourceObjects[F - 1]);
    Dec(AItemsLeft);

    ANode := Nodes[F];
    while ANode <> nil do
      ANode := RemoveRelation(ANode, ALastNoPredecessorItemIndex);
    F := QLink[F];
  end;
  FSuccess := AItemsLeft = 0;
end;

function TdxTopologicalSorter<T>.RemoveRelation(ANode: TdxTopologicalSorterNode;
  var ALastNoPredecessorItemIndex: Integer): TdxTopologicalSorterNode;
var
  AIndex: Integer;
begin
  AIndex := ANode.OneBasedSuccessorIndex;
  Dec(QLink[AIndex]);

  if QLink[AIndex] = 0 then
  begin
    QLink[ALastNoPredecessorItemIndex] := AIndex;
    ALastNoPredecessorItemIndex := AIndex;
  end;
  Result := ANode.Next;
end;

class function TdxTopologicalSorter<T>.Sort(ASourceObjects: TdxList<T>;
  AComparer: IComparer<T>): TdxList<T>;
var
  ASorter: TdxTopologicalSorter<T>;
begin
  ASorter := TdxTopologicalSorter<T>.Create;
  try
    Result := ASorter.DoSort(ASourceObjects, AComparer);
  finally
    ASorter.Free;
  end;
end;

function TdxTopologicalSorter<T>.DoSort(ASourceObjects: TdxList<T>;
  const AIsDependOn: TdxTopologicalSorterIsDependOnDelegate): TdxList<T>;
var
  I, N, ALastNoPredecessorItemIndex: Integer;
begin
  N := ASourceObjects.Count;
  if N < 2 then
  begin
    FSuccess := True;
    Result := TdxList<T>.Create;
    Result.AddRange(ASourceObjects);
  end
  else
  begin
    Initialize(N);
    CalculateRelations(ASourceObjects, AIsDependOn);

    ALastNoPredecessorItemIndex := CreateVirtualNoPredecessorsItemList;
    Result := ProcessNodes(ALastNoPredecessorItemIndex, ASourceObjects);
    if Result.Count = 0 then
    begin
      for I := 0 to N - 1 do
        Result.Add(ASourceObjects[I]);
    end;
  end;
end;

{ TdxDefaultComparable<T> }

function TdxDefaultComparable<T>.CompareTo(const AValue: T): Integer;
begin
  if FComparer = nil then
    FComparer := TdxComparer<T>.Default;
  Result := FComparer.Compare(AValue, Value);
end;

constructor TdxDefaultComparable<T>.Create(const AValue: T);
begin
  inherited Create;
  FValue := AValue;
end;

{ TdxComparer<T> }

class function TdxComparer<T>.Construct(const Comparison: TComparison<T>): IComparer<T>;
begin
  Result := TDelegatedComparer<T>.Create(Comparison);
end;

class function TdxComparer<T>.Default: IComparer<T>;
begin
  Result := IComparer<T>(_LookupVtableInfo(giComparer, TypeInfo(T), SizeOf(T)));
end;

{ TdxAlgorithms }

class function TdxAlgorithms<T>.BinarySearch(AList: TList<T>; const AValue: T; out AIndex: Integer): Boolean;
var
  APredicate: IdxComparable<T>;
begin
  APredicate := TdxDefaultComparable<T>.Create(AValue);
  Result := TdxAlgorithms<T>.BinarySearch(AList, APredicate, AIndex);
end;

class function TdxAlgorithms<T>.BinarySearch(AList: TList<T>;
  const APredicate: IdxComparable<T>; AStartIndex, AEndIndex: Integer; out AIndex: Integer): Boolean;
var
  AHi, AMedian: Integer;
  ACompareResult: Integer;
begin
  AIndex := AStartIndex;
  AHi := AEndIndex;
  while AIndex <= AHi do
  begin
    AMedian := AIndex + ((AHi - AIndex) shr 1);
    ACompareResult := APredicate.CompareTo(AList[AMedian]);
    if ACompareResult = 0 then
    begin
      AIndex := AMedian;
      Exit(True);
    end
    else
      if ACompareResult < 0 then
        AIndex := AMedian + 1
      else
        AHi := AMedian - 1;
  end;
  Result := False;
end;

class function TdxAlgorithms<T>.BinarySearch(AList: TList<T>; const APredicate: IdxComparable<T>; out AIndex: Integer): Boolean;
begin
  Result := TdxAlgorithms<T>.BinarySearch(AList, APredicate, 0, AList.Count - 1, AIndex);
end;

{ TdxAlgorithms1 }

class function TdxAlgorithms1<T>.BinarySearch(AList: TdxList<T>; const APredicate: IdxComparable<T>; AStartIndex,
  AEndIndex: Integer; out AIndex: Integer): Boolean;
var
  AHi, AMedian: Integer;
  ACompareResult: Integer;
begin
  AIndex := AStartIndex;
  AHi := AEndIndex;
  while AIndex <= AHi do
  begin
    AMedian := AIndex + ((AHi - AIndex) shr 1);
    ACompareResult := APredicate.CompareTo(AList[AMedian]);
    if ACompareResult = 0 then
    begin
      AIndex := AMedian;
      Exit(True);
    end
    else
      if ACompareResult < 0 then
        AIndex := AMedian + 1
      else
        AHi := AMedian - 1;
  end;
  Result := False;
end;

class function TdxAlgorithms1<T>.BinarySearch(AList: TdxList<T>; const APredicate: IdxComparable<T>; out AIndex: Integer): Boolean;
begin
  Result := BinarySearch(AList, APredicate, 0, AList.Count - 1, AIndex);
end;

class function TdxAlgorithms1<T>.BinarySearchReverseOrder(AList: TdxList<T>; const APredicate: IdxComparable<T>;
  AStartIndex, AEndIndex: Integer; out AIndex: Integer): Boolean;
var
  AHi, AMedian, ACompareResult: Integer;
begin
  AIndex := AStartIndex;
  AHi := AEndIndex;
  while AIndex <= AHi do
  begin
    AMedian := AIndex + (AHi - AIndex) shr 1;
    ACompareResult := APredicate.CompareTo(AList[AMedian]);
    if ACompareResult = 0 then
    begin
      AIndex := AMedian;
      Exit(True);
    end;
    if ACompareResult < 0 then
      AIndex := AMedian + 1
    else
      AHi := AMedian - 1;
  end;
  Result := False;
end;

class function TdxAlgorithms1<T>.BinarySearchReverseOrder(AList: TdxList<T>; const APredicate: IdxComparable<T>;
  out AIndex: Integer): Boolean;
begin
  Result := BinarySearchReverseOrder(AList, APredicate, 0, AList.Count - 1, AIndex);
end;

{ TdxSortedList<T> }

constructor TdxSortedList<T>.Create;
begin
  inherited Create;
  FInnerList := CreateInnerList;
  FComparer := TComparer<T>.Default;
end;

constructor TdxSortedList<T>.Create(const AComparer: IComparer<T>);
begin
  inherited Create;
  Assert(AComparer <> nil);
  FInnerList := CreateInnerList;
  FComparer := AComparer;
end;

destructor TdxSortedList<T>.Destroy;
begin
  FInnerList.Free;
  inherited Destroy;
end;

function TdxSortedList<T>.Count: Integer;
begin
  Result := FInnerList.Count;
end;

function TdxSortedList<T>.First: T;
begin
  if Count <= 0 then
    Result := Default(T)
  else
    Result := FInnerList[0];
end;

function TdxSortedList<T>.Last: T;
begin
  if Count <= 0 then
    Result := Default(T)
  else
    Result := FInnerList[Count - 1];
end;

procedure TdxSortedList<T>.Add(const AValue: T);
var
  AIndex: Integer;
begin
  AIndex := BinarySearch(AValue);
  if AIndex >= 0 then
    Exit;
  Insert(not AIndex, AValue);
end;

procedure TdxSortedList<T>.Insert(AIndex: Integer; const AValue: T);
begin
  FInnerList.Insert(AIndex, AValue);
end;

function TdxSortedList<T>.Contains(const AValue: T): Boolean;
begin
  Result := BinarySearch(AValue) >= 0;
end;

function TdxSortedList<T>.BinarySearch(const AValue: T): Integer;
begin
  if not FInnerList.BinarySearch(AValue, Result, FComparer) then
    Result := not Result;
end;

function TdxSortedList<T>.BinarySearch(const APredicate: IdxComparable<T>): Integer;
begin
  if not TdxAlgorithms<T>.BinarySearch(FInnerList, APredicate, Result) then
    Result := not Result;
end;

procedure TdxSortedList<T>.Delete(AIndex: Integer);
begin
  FInnerList.Delete(AIndex);
end;

function TdxSortedList<T>.Extract(const Value: T): T;
begin
  Result := FInnerList.Extract(Value);
end;

procedure TdxSortedList<T>.Remove(const AValue: T);
var
  AIndex: Integer;
begin
  AIndex := BinarySearch(AValue);
  if AIndex >= 0 then
    Delete(AIndex);
end;

procedure TdxSortedList<T>.RemoveFrom(const AValue: T);
var
  I, AStartIndex: Integer;
begin
  AStartIndex := BinarySearch(AValue);
  if AStartIndex < 0 then
    AStartIndex := not AStartIndex;
  for I := FinnerList.Count - 1 downto AStartIndex do
    Delete(I);
end;

procedure TdxSortedList<T>.Clear;
begin
  FInnerList.Clear;
end;

procedure TdxSortedList<T>.CopyCore(ADestination: TdxSortedList<T>);
begin
  ADestination.FInnerList.AddRange(FInnerList);
end;

function TdxSortedList<T>.GetItem(Index: Integer): T;
begin
  Result := FInnerList[Index];
end;

function TdxSortedList<T>.CreateInnerList: TList<T>;
begin
  Result := TList<T>.Create;
end;

{ TdxObjectSortedList<T> }

function TdxObjectSortedList<T>.CreateInnerList: TList<T>;
begin
  Result := TObjectList<T>.Create;
end;

function TdxObjectSortedList<T>.ExtractByIndex(AIndex: Integer): T;
var
  AList: TObjectList<T>;
begin
  AList := TObjectList<T>(FInnerList);
  Result := AList[AIndex];
  AList.OwnsObjects := False;
  AList.Delete(AIndex);
  AList.OwnsObjects := True;
end;

{ TdxLayoutUnitSortedList }

function TdxLayoutUnitSortedList.Clone: TdxLayoutUnitSortedList;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxLayoutUnitSortedList.Create;
  CopyCore(Result);
end;

{ TdxHashSet<T> }

constructor TdxHashSet<T>.Create(ACapacity: Integer = 0);
begin
  inherited Create;
  FDictionary := TDictionary<T, Integer>.Create(ACapacity);
end;

constructor TdxHashSet<T>.Create(const AComparer: IEqualityComparer<T>);
begin
  inherited Create;
  FDictionary := TDictionary<T, Integer>.Create(AComparer);
end;

constructor TdxHashSet<T>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<T>);
begin
  inherited Create;
  FDictionary := TDictionary<T, Integer>.Create(ACapacity, AComparer);
end;

destructor TdxHashSet<T>.Destroy;
begin
  FDictionary.Free;
  inherited Destroy;
end;

function TdxHashSet<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := FDictionary.Keys.GetEnumerator;
end;

procedure TdxHashSet<T>.Clear;
begin
  FDictionary.Clear;
end;

function TdxHashSet<T>.Contains(const AValue: T): Boolean;
begin
  Result := FDictionary.ContainsKey(AValue);
end;

procedure TdxHashSet<T>.Exclude(const AValue: T);
begin
  FDictionary.Remove(AValue);
end;

procedure TdxHashSet<T>.Exclude(const AValue: TdxHashSet<T>);
var
  I: T;
begin
  for I in AValue do
    Exclude(I);
end;

procedure TdxHashSet<T>.Exclude(const AValue: TArray<T>);
var
  I: T;
begin
  for I in AValue do
    Exclude(I);
end;

procedure TdxHashSet<T>.Exclude(const AValue: TList<T>);
var
  I: T;
begin
  for I in AValue do
    Exclude(I);
end;

function TdxHashSet<T>.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

procedure TdxHashSet<T>.Include(const AValue: T);
begin
  FDictionary.AddOrSetValue(AValue, 0);
end;

procedure TdxHashSet<T>.Include(const AValue: TArray<T>);
var
  I: T;
begin
  for I in AValue do
    Include(I);
end;

function TdxHashSet<T>.ToArray: TArray<T>;
{$IFNDEF DELPHIXE}
var
  AValue: T;
  I: Integer;
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  Result := FDictionary.Keys.ToArray;
{$ELSE}
  I := 0;
  SetLength(Result, Count);
  for AValue in FDictionary.Keys do
  begin
    Result[I] := AValue;
    Inc(I);
  end;
{$ENDIF}
end;

function TdxHashSet<T>.ToList: TList<T>;
begin
  Result := TList<T>.Create(FDictionary.Keys);
end;

procedure TdxHashSet<T>.Include(const AValue: TList<T>);
var
  I: T;
begin
  for I in AValue do
    Include(I);
end;

procedure TdxHashSet<T>.Include(const AValue: TdxHashSet<T>);
var
  I: T;
begin
  for I in AValue do
    Include(I);
end;

{ TdxSmartPointer<T>.TLifetimeWatcher }

constructor TdxSmartPointer<T>.TLifetimeWatcher.Create(AObject: TObject);
begin
  FObject := AObject;
end;

destructor TdxSmartPointer<T>.TLifetimeWatcher.Destroy;
begin
  FObject.Free;
  inherited Destroy;
end;

{ TdxSmartPointer<T> }

constructor TdxSmartPointer<T>.Create(const AValue: T);
begin
  FValue := AValue;
  FLifetime := TLifetimeWatcher.Create(FValue);
end;

class operator TdxSmartPointer<T>.Implicit(const AValue: T): TdxSmartPointer<T>;
begin
  Result := TdxSmartPointer<T>.Create(AValue);
end;

class operator TdxSmartPointer<T>.Implicit(const AValue: TdxSmartPointer<T>): T;
begin
  Result := AValue.Value;
end;

{ TdxMap<TKey, TValue> }

constructor TdxMap<TKey, TValue>.Create;
begin
  Create(nil, nil);
end;

constructor TdxMap<TKey, TValue>.Create(AComparerKey: IEqualityComparer<TKey>;
  AComparerValue: IEqualityComparer<TValue>; AOwnerships: TDictionaryOwnerships; ACapacity: Integer);
begin
  FValueToKey := TDictionary<TValue, TKey>.Create(ACapacity, AComparerValue);
  if AOwnerships <> [] then
    FKeyToValue := TObjectDictionary<TKey, TValue>.Create(AOwnerships, ACapacity, AComparerKey)
  else
    FKeyToValue := TDictionary<TKey, TValue>.Create(ACapacity, AComparerKey);
end;

constructor TdxMap<TKey, TValue>.Create(ACapacity: Integer; AOwnerships: TDictionaryOwnerships);
begin
  Create(nil, nil, AOwnerships, ACapacity);
end;

constructor TdxMap<TKey, TValue>.Create(AOwnerships: TDictionaryOwnerships);
begin
  Create(nil, nil, AOwnerships);
end;

destructor TdxMap<TKey, TValue>.Destroy;
begin
  Clear;
  FreeAndNil(FKeyToValue);
  FreeAndNil(FValueToKey);
  inherited Destroy;
end;

procedure TdxMap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  try
    FKeyToValue.Add(AKey, AValue);
    FValueToKey.Add(AValue, AKey);
  except
    FValueToKey.Remove(AValue);
    FKeyToValue.Remove(AKey);
    raise;
  end;
end;

procedure TdxMap<TKey, TValue>.Clear;
begin
  if FKeyToValue <> nil then
    FKeyToValue.Clear;
  if FValueToKey <> nil then
    FValueToKey.Clear;
end;

procedure TdxMap<TKey, TValue>.DeleteByKey(const AKey: TKey);
var
  AValue: TValue;
begin
  if FKeyToValue.TryGetValue(AKey, AValue) then
  begin
    FValueToKey.Remove(AValue);
    FKeyToValue.Remove(AKey);
  end;
end;

procedure TdxMap<TKey, TValue>.DeleteByValue(const AValue: TValue);
var
  AKey: TKey;
begin
  if FValueToKey.TryGetValue(AValue, AKey) then
  begin
    FKeyToValue.Remove(AKey);
    FValueToKey.Remove(AValue);
  end;
end;

function TdxMap<TKey, TValue>.GetKey(const AValue: TValue): TKey;
begin
  if not TryGetKey(AValue, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);
end;

function TdxMap<TKey, TValue>.TryGetKey(const AValue: TValue; out AKey: TKey): Boolean;
begin
  Result := FValueToKey.TryGetValue(AValue, AKey);
end;

function TdxMap<TKey, TValue>.GetValue(const AKey: TKey): TValue;
begin
  if not TryGetValue(AKey, Result) then
    raise EListError.CreateRes(@SGenericItemNotFound);
end;

function TdxMap<TKey, TValue>.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
begin
  Result := FKeyToValue.TryGetValue(AKey, AValue);
end;


end.
