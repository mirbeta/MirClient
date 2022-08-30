{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit cxDataStorage;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, cxVariants, dxCore, Variants
  {$IFNDEF NONDB},FMTBcd, SqlTimSt{$ENDIF}, AnsiStrings;

type
  LargeInt = Int64;
  PLargeInt = ^LargeInt;

  { Value Types }


  PStringValue = PString;
  PWideStringValue = PWideString;

  TcxValueType = class
  protected
    class function Compare(P1, P2: Pointer): Integer; virtual;
    class procedure FreeBuffer(ABuffer: PAnsiChar); virtual;
    class procedure FreeTextBuffer(ABuffer: PAnsiChar); virtual;
    class function GetDataSize: Integer; virtual;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; virtual;
    class function GetDefaultDisplayText(ABuffer: PAnsiChar): string; virtual;
    class function GetDisplayText(ABuffer: PAnsiChar): string; virtual;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); virtual;
    class procedure ReadDisplayText(ABuffer: PAnsiChar; AStream: TdxStream); virtual;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); virtual;
    class procedure SetDisplayText(ABuffer: PAnsiChar; const DisplayText: string); virtual;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); virtual;
    class procedure WriteDisplayText(ABuffer: PAnsiChar; AStream: TdxStream); virtual;
  public
    class function Caption: string; virtual;
    class function CompareValues(P1, P2: Pointer): Integer; virtual;
    class function GetValue(ABuffer: PAnsiChar): Variant; virtual;
    class function GetVarType: Integer; virtual;
    class function IsValueValid(var{const }Value: Variant): Boolean; virtual;
    class function IsString: Boolean; virtual;
    class procedure PrepareValueBuffer(var ABuffer: PAnsiChar); virtual;
  end;

  TcxValueTypeClass = class of TcxValueType;

  TcxStringValueType = class(TcxValueType)
  protected
    class function Compare(P1, P2: Pointer): Integer; override;
    class procedure FreeBuffer(ABuffer: PAnsiChar); override;
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetValue(ABuffer: PAnsiChar): Variant; override;
    class function GetVarType: Integer; override;
    class function IsString: Boolean; override;
    class procedure PrepareValueBuffer(var ABuffer: PAnsiChar); override;
  end;

  TcxWideStringValueType = class(TcxStringValueType)
  protected
    class function Compare(P1, P2: Pointer): Integer; override;
    class procedure FreeBuffer(ABuffer: PAnsiChar); override;
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetValue(ABuffer: PAnsiChar): Variant; override;
    class function GetVarType: Integer; override;
    class function IsString: Boolean; override;
    class procedure PrepareValueBuffer(var ABuffer: PAnsiChar); override;
  end;

  TcxSmallintValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxIntegerValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxWordValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxBooleanValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class function GetDefaultDisplayText(ABuffer: PAnsiChar): string; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxSingleValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxFloatValueType = class(TcxValueType) // TODO: Double or Extended?
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxCurrencyValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxDateTimeValueType = class(TcxValueType)
  private
    class function GetDateTime(ABuffer: PAnsiChar): TDateTime;
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class function GetDefaultDisplayText(ABuffer: PAnsiChar): string; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxLargeIntValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  {$IFNDEF NONDB}
  TcxFMTBcdValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class function GetDefaultDisplayText(ABuffer: PAnsiChar): string; override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;

  TcxSQLTimeStampValueType = class(TcxValueType)
  protected
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetVarType: Integer; override;
  end;
  {$ENDIF}

  TcxVariantValueType = class(TcxValueType)
  protected
    class function Compare(P1, P2: Pointer): Integer; override;
    class procedure FreeBuffer(ABuffer: PAnsiChar); override;
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetValue(ABuffer: PAnsiChar): Variant; override;
    class procedure PrepareValueBuffer(var ABuffer: PAnsiChar); override;
  end;

  TcxBLOBValueType = class(TcxValueType)
  protected
    class function Compare(P1, P2: Pointer): Integer; override;
    class procedure FreeBuffer(ABuffer: PAnsiChar); override;
    class function GetAnsiString(ABuffer: PAnsiChar): AnsiString;
    class function GetDataSize: Integer; override;
    class function GetDataValue(ABuffer: PAnsiChar): Variant; override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  public
    class function CompareValues(P1, P2: Pointer): Integer; override;
    class function GetValue(ABuffer: PAnsiChar): Variant; override;
    class function GetVarType: Integer; override;
    class function IsString: Boolean; override;
    class procedure PrepareValueBuffer(var ABuffer: PAnsiChar); override;
  end;

  TcxObjectValueType = class({$IFDEF CPUX64}TcxLargeIntValueType{$ELSE}TcxIntegerValueType{$ENDIF})
  protected
    class procedure FreeBuffer(ABuffer: PAnsiChar); override;
    class procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
    class procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant); override;
    class procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream); override;
  end;

  { TcxDataStorage }

  TcxDataStorage = class;
  TcxValueDefs = class;

  TcxValueDef = class
  private
    FBufferSize: Integer;
    FDataOffset: Integer;
    FDataSize: Integer;
    FDisplayTextOffset: Integer;
    FStored: Boolean;
    FLinkObject: TObject;
    FOffset: Integer;
    FStreamStored: Boolean;
    FTextStored: Boolean;
    FValueDefs: TcxValueDefs;
    FValueTypeClass: TcxValueTypeClass;
    function GetIsNeedConversion: Boolean;
    function GetTextStored: Boolean;
    procedure SetStored(Value: Boolean);
    procedure SetTextStored(Value: Boolean);
    procedure SetValueTypeClass(Value: TcxValueTypeClass);
  protected
    procedure Changed(AResyncNeeded: Boolean);
    function Compare(P1, P2: PAnsiChar): Integer;
    procedure FreeBuffer(ABuffer: PAnsiChar);
    procedure FreeTextBuffer(ABuffer: PAnsiChar);
    function GetDataFromBuffer(ABuffer: PAnsiChar): PAnsiChar; inline;
    function GetDataValue(ABuffer: PAnsiChar): Variant;
    function GetDisplayText(ABuffer: PAnsiChar): string;
    function GetDisplayTextFromBuffer(ABuffer: PAnsiChar): PAnsiChar;
    function GetLinkObject: TObject; virtual;
    function GetStored: Boolean; virtual;
    procedure Init(var AOffset: Integer);
    function IsNullValue(ABuffer: PAnsiChar): Boolean; inline;
    function IsNullValueEx(ABuffer: PAnsiChar): Boolean; inline;
    procedure ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
    procedure ReadDisplayText(ABuffer: PAnsiChar; AStream: TdxStream);
    procedure SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
    procedure SetDisplayText(ABuffer: PAnsiChar; const DisplayText: string);
    procedure SetLinkObject(Value: TObject); virtual;
    procedure SetNull(ABuffer: PAnsiChar; AIsNull: Boolean);
    procedure WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
    procedure WriteDisplayText(ABuffer: PAnsiChar; AStream: TdxStream);
  public
    constructor Create(AValueDefs: TcxValueDefs; AValueTypeClass: TcxValueTypeClass); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TcxValueDef); virtual;
    function CompareValues(AIsNull1, AIsNull2: Boolean; P1, P2: PAnsiChar): Integer;

    property BufferSize: Integer read FBufferSize;
    property DataSize: Integer read FDataSize;
    property IsNeedConversion: Boolean read GetIsNeedConversion;
    property LinkObject: TObject read GetLinkObject write SetLinkObject;
    property Offset: Integer read FOffset;
    property Stored: Boolean read GetStored write SetStored default True;
    property StreamStored: Boolean read FStreamStored write FStreamStored default True;
    property TextStored: Boolean read GetTextStored write SetTextStored default False;
    property ValueDefs: TcxValueDefs read FValueDefs;
    property ValueTypeClass: TcxValueTypeClass read FValueTypeClass write SetValueTypeClass;
  end;

  TcxValueDefClass = class of TcxValueDef;

  TcxValueDefs = class
  private
    FDataStorage: TcxDataStorage;
    FItems: TList;
    FRecordOffset: Integer;
    FRecordSize: Integer;
    function GetStoredCount: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxValueDef;
  protected
    procedure Changed(AValueDef: TcxValueDef; AResyncNeeded: Boolean); virtual;
    function GetValueDefClass: TcxValueDefClass; virtual;
    procedure Prepare(AStartOffset: Integer); virtual;
    procedure Remove(AItem: TcxValueDef);
    property DataStorage: TcxDataStorage read FDataStorage;
  public
    constructor Create(ADataStorage: TcxDataStorage); virtual;
    destructor Destroy; override;
    function Add(AValueTypeClass: TcxValueTypeClass; AStored, ATextStored: Boolean; ALinkObject: TObject): TcxValueDef;
    procedure Clear;
    property StoredCount: Integer read GetStoredCount;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxValueDef read GetItem; default;
    property RecordSize: Integer read FRecordSize;
  end;

  { internal value defs }

  TcxInternalValueDef = class(TcxValueDef)
  protected
    function GetLinkObject: TObject; override;
    function GetStored: Boolean; override;
  public
    function GetValueDef: TcxValueDef;
  end;

  TcxInternalValueDefs = class(TcxValueDefs)
  protected
    function GetValueDefClass: TcxValueDefClass; override;
  public
    function FindByLinkObject(ALinkObject: TObject): TcxValueDef;
    procedure RemoveByLinkObject(ALinkObject: TObject);
  end;

  TcxValueDefReader = class
  public
    constructor Create; virtual;
    function GetDisplayText(AValueDef: TcxValueDef): string; virtual;
    function GetValue(AValueDef: TcxValueDef): Variant; virtual;
    function IsInternal(AValueDef: TcxValueDef): Boolean; virtual;
  end;

  TcxValueDefReaderClass = class of TcxValueDefReader;

  TcxValueDefSetProc = procedure (AValueDef: TcxValueDef; AFromRecordIndex, AToRecordIndex: Integer;
    AValueDefReader: TcxValueDefReader) of object;

  TcxDataStorage = class
  private
    FDestroying: Boolean;
    FInternalRecordBuffers: TList;
    FInternalValueDefs: TcxInternalValueDefs;
    FStoredValuesOnly: Boolean;
    FRecordBuffers: TList;
    FRecordIDCounter: Integer;
    FUseRecordID: Boolean;
    FValueDefs: TcxValueDefs;
    FValueDefsList: TList;
//    FValueDefsChanged: Boolean;
    FOnClearInternalRecords: TNotifyEvent;
    function GetRecordBuffer(Index: Integer): PAnsiChar;
    function GetRecordCount: Integer;
    procedure SetStoredValuesOnly(Value: Boolean);
    procedure SetRecordBuffer(Index: Integer; Value: PAnsiChar);
    procedure SetRecordCount(Value: Integer);
    procedure SetUseRecordID(Value: Boolean);
  protected
    function AllocRecordBuffer(Index: Integer): PAnsiChar;
    function CalcRecordOffset: Integer;
    procedure ChangeRecordFlag(ABuffer: PAnsiChar; AFlag: Byte; ATurnOn: Boolean);
    procedure CheckRecordID(ARecordIndex: Integer);
    procedure CheckRecordIDCounter;
    procedure CheckRecordIDCounterAfterLoad(ALoadedID: Integer);
    function CheckValueDef(ARecordIndex: Integer; var AValueDef: TcxValueDef): Boolean;
    procedure DeleteInternalRecord(ARecordIndex: Integer);
    procedure FreeAndNilRecordBuffer(AIndex: Integer);
    procedure InitStructure(AValueDefs: TcxValueDefs); virtual;
    procedure InsertValueDef(AIndex: Integer; AValueDef: TcxValueDef);
    function IsRecordFlag(ABuffer: PAnsiChar; AFlag: Byte): Boolean;
    procedure RemoveValueDef(AValueDef: TcxValueDef);
    procedure ValueDefsChanged(AValueDef: TcxValueDef; AResyncNeeded: Boolean); virtual;
    function ValueDefsByRecordIndex(Index: Integer): TcxValueDefs; virtual;
    property InternalValueDefs: TcxInternalValueDefs read FInternalValueDefs;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddInternalRecord: Integer;
    function AppendRecord: Integer;
    procedure BeforeDestruction; override;
    procedure BeginLoad;
    procedure CheckStructure;
    procedure Clear(AWithoutInternal: Boolean);
    procedure ClearInternalRecords;
    procedure ClearRecords(AClearList: Boolean);
    function CompareRecords(ARecordIndex1, ARecordIndex2: Integer; AValueDef: TcxValueDef): Integer;
    procedure DeleteRecord(ARecordIndex: Integer);
    procedure EndLoad;
    function GetDisplayText(ARecordIndex: Integer; AValueDef: TcxValueDef): string;
    function GetCompareInfo(ARecordIndex: Integer; AValueDef: TcxValueDef; var P: PAnsiChar): Boolean;
    function GetRecordID(ARecordIndex: Integer): Integer;
    function GetValue(ARecordIndex: Integer; AValueDef: TcxValueDef): Variant;
    procedure InsertRecord(ARecordIndex: Integer);
    procedure ReadData(ARecordIndex: Integer; AStream: TStream);
    procedure ReadDataByFields(ARecordIndex: Integer; AValueDefReader: TcxValueDefReader; AFields: TList);
    procedure ReadRecord(ARecordIndex: Integer; AValueDefReader: TcxValueDefReader);
    procedure ReadRecordFrom(AFromRecordIndex, AToRecordIndex: Integer; AValueDefReader: TcxValueDefReader; ASetProc: TcxValueDefSetProc);
    procedure SetDisplayText(ARecordIndex: Integer; AValueDef: TcxValueDef; const Value: string);
    procedure SetRecordID(ARecordIndex, AID: Integer);
    procedure SetValue(ARecordIndex: Integer; AValueDef: TcxValueDef; const Value: Variant);
    procedure WriteData(ARecordIndex: Integer; AStream: TStream);

    procedure BeginStreaming(ACompare: TListSortCompare);
    procedure EndStreaming;

    property StoredValuesOnly: Boolean read FStoredValuesOnly write SetStoredValuesOnly;
    property UseRecordID: Boolean read FUseRecordID write SetUseRecordID;
    property RecordBuffers[Index: Integer]: PAnsiChar read GetRecordBuffer write SetRecordBuffer;
    property RecordCount: Integer read GetRecordCount write SetRecordCount;
    property ValueDefs: TcxValueDefs read FValueDefs;
    property OnClearInternalRecords: TNotifyEvent read FOnClearInternalRecords write FOnClearInternalRecords;
  end;

  TcxDataStorageHelper = class
  public
    class function AllocateRecord(ADataStorage: TcxDataStorage): Pointer;
    class function AppendRecord(ADataStorage: TcxDataStorage; ARecord: Pointer): Integer;
    class function Compare(AValueDef: TcxValueDef; ARecord1, ARecord2: Pointer): Integer;
    class procedure FreeBuffer(AValueDef: TcxValueDef; ARecord: Pointer);
    class function GetDisplayText(AValueDef: TcxValueDef; ARecord: Pointer): string;
    class function GetRecordSize(ADataStorage: TcxDataStorage): Integer;
    class function GetStoredCount(ADataStorage: TcxDataStorage): Integer;
    class function GetValue(AValueDef: TcxValueDef; ARecord: Pointer): Variant;
    class function RemoveRecord(ADataStorage: TcxDataStorage; ARecordIndex: Integer): Pointer;
    class procedure SetDisplayText(ARecord: Pointer; AValueDef: TcxValueDef; const AValue: string);
    class procedure SetRecordsCapacity(ADataStorage: TcxDataStorage; ACapacity: Integer);
    class procedure SetTextStored(AValueDef: TcxValueDef; ATextStored: Boolean);
    class procedure SetValue(ARecord: Pointer; AValueDef: TcxValueDef; const AValue: Variant);
    class procedure FreeRecord(ADataStorage: TcxDataStorage; var ARecord: Pointer);
  end;

  { TcxLookupList }

  TcxLookupListItem = record
    KeyValue: Variant;
    DisplayText: string;
  end;

  PcxLookupListItem = ^TcxLookupListItem;

  TcxLookupList = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): PcxLookupListItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Find(const AKeyValue: Variant; var AIndex: Integer): Boolean;
    procedure Insert(AIndex: Integer; const AKeyValue: Variant; const ADisplayText: string);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PcxLookupListItem read GetItem; default;
  end;

  { TcxValueTypeClassList }

  TcxValueTypeClassList = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxValueTypeClass;
  public
    constructor Create;
    destructor Destroy; override;
    function ItemByCaption(const ACaption: string): TcxValueTypeClass;
    procedure RegisterItem(AValueTypeClass: TcxValueTypeClass);
    procedure UnregisterItem(AValueTypeClass: TcxValueTypeClass);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxValueTypeClass read GetItem; default;
  end;

function cxValueTypeClassList: TcxValueTypeClassList;

function IsDateTimeValueTypeClass(AValueTypeClass: TcxValueTypeClass): Boolean;

implementation

const
  RecordFlagSize = SizeOf(Byte);
  ValueFlagSize  = SizeOf(Byte);
  PointerSize    = SizeOf(Pointer);
  RecordIDSize   = SizeOf(Integer);
  // RecordFlag Bit Masks
  RecordFlag_Busy = $01;

var
  FValueTypeClassList: TcxValueTypeClassList;

function cxValueTypeClassList: TcxValueTypeClassList;
begin
  if FValueTypeClassList = nil then
    FValueTypeClassList := TcxValueTypeClassList.Create;
  Result := FValueTypeClassList;
end;

function IsDateTimeValueTypeClass(AValueTypeClass: TcxValueTypeClass): Boolean;
begin
  Result := (AValueTypeClass = TcxDateTimeValueType)
    {$IFNDEF NONDB} or (AValueTypeClass = TcxSQLTimeStampValueType){$ENDIF};
end;

{ TcxValueType }

class function TcxValueType.Caption: string;
var
  I: Integer;
begin
  Result := ClassName;
  if Result <> '' then
  begin
    if Copy(Result, 1, 3) = 'Tcx' then
      Delete(Result, 1, 3);
    I := Pos('ValueType', Result);
    if I <> 0 then
      Delete(Result, I, Length('ValueType'));
  end;
end;

class function TcxValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  Result := VarCompare(GetDataValue(P1), GetDataValue(P2));
end;

class function TcxValueType.GetValue(ABuffer: PAnsiChar): Variant;
begin
  Result := GetDataValue(ABuffer);
end;

class function TcxValueType.GetVarType: Integer;
begin
  Result := varVariant;
end;

class function TcxValueType.IsValueValid(var Value: Variant): Boolean;
var
  V: Variant;
begin
  if VarIsNull(Value) or (GetVarType = varVariant) then  // not Empty?
    Result := True
  else
  begin
    Result := False;
    try
      //!!! B92835 - Bug in TFMTBcdVariantType.Cast: dest (string variant for example) is not cleared before usage
      VarCast({Value}V, Value, GetVarType);
      Value := V;
      Result := True;
    except
      on E: EVariantError do;
    end;
  end;
end;

class function TcxValueType.IsString: Boolean;
begin
  Result := False;
end;

class procedure TcxValueType.PrepareValueBuffer(var ABuffer: PAnsiChar);
begin
end;

class function TcxValueType.Compare(P1, P2: Pointer): Integer;
begin
  Result := CompareValues(P1, P2);
end;

class procedure TcxValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
end;

class procedure TcxValueType.FreeTextBuffer(ABuffer: PAnsiChar);
var
  P: PStringValue;
begin
  P := PPointer(ABuffer)^;
  if P <> nil then
    Dispose(P);
end;

class function TcxValueType.GetDataSize: Integer;
begin
  Result := 0;
end;

class function TcxValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := Null;
end;

class function TcxValueType.GetDefaultDisplayText(ABuffer: PAnsiChar): string;
begin
  try
    Result := VarToStr(GetDataValue(ABuffer));
  except
    on EVariantError do
      Result := '';
  end;
end;

class function TcxValueType.GetDisplayText(ABuffer: PAnsiChar): string;
var
  P: PStringValue;
begin
  P := PPointer(ABuffer)^;
  if P <> nil then
    Result := P^
  else
    Result := '';
end;


class procedure TcxValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadVariantFunc(AStream));
end;

class procedure TcxValueType.ReadDisplayText(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  if AStream.IsUnicode then
    SetDisplayText(ABuffer, ReadWideStringFunc(AStream))
  else
    SetDisplayText(ABuffer, dxAnsiStringToString(ReadAnsiStringFunc(AStream)));
end;

class procedure TcxValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
end;

class procedure TcxValueType.SetDisplayText(ABuffer: PAnsiChar; const DisplayText: string);
var
  P: PStringValue;
begin
  P := PPointer(ABuffer)^;
  if P = nil then
  begin
    New(P);
    PPointer(ABuffer)^ := P;
  end;
  P^ := DisplayText;
end;

class procedure TcxValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteVariantProc(AStream, GetDataValue(ABuffer));
end;

class procedure TcxValueType.WriteDisplayText(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  if AStream.IsUnicode then
    WriteWideStringProc(AStream, GetDisplayText(ABuffer))
  else
    WriteAnsiStringProc(AStream, dxStringToAnsiString(GetDisplayText(ABuffer)));
end;

{ TcxStringValueType }

class function TcxStringValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  if Assigned(P1) then
  begin
    if Assigned(P2) then
      Result := CompareStr(PStringValue(P1)^, PStringValue(P2)^)
    else
      Result := 1;
  end
  else
  begin
    if Assigned(P2) then
      Result := -1
    else
      Result := 0;
  end;
end;

class function TcxStringValueType.GetValue(ABuffer: PAnsiChar): Variant;
begin
  Result := GetDataValue(@ABuffer);
end;

class function TcxStringValueType.GetVarType: Integer;
begin
  Result := varUString
end;

class function TcxStringValueType.IsString: Boolean;
begin
  Result := True;
end;

class procedure TcxStringValueType.PrepareValueBuffer(var ABuffer: PAnsiChar);
begin
  ABuffer := PPointer(ABuffer)^;
end;

class function TcxStringValueType.Compare(P1, P2: Pointer): Integer;
begin
  Result := CompareValues(PPointer(P1)^, PPointer(P2)^);
end;

class procedure TcxStringValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
  Dispose(PStringValue(PPointer(ABuffer)^));
end;

class function TcxStringValueType.GetDataSize: Integer;
begin
  Result := SizeOf(PStringValue);
end;

class function TcxStringValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
var
  P: PStringValue;
begin
  P := PPointer(ABuffer)^;
  if P <> nil then
    Result := P^
  else
    Result := inherited GetDataValue(ABuffer);
  if not VarIsNull(Result) then
    VarCast(Result, Result, GetVarType);
end;

class procedure TcxStringValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  if AStream.IsUnicode then
    SetDataValue(ABuffer, ReadWideStringFunc(AStream))
  else
    SetDataValue(ABuffer, ReadAnsiStringFunc(AStream));
end;

class procedure TcxStringValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  if VarIsArray(Value) then
    SetDisplayText(ABuffer, StringOf(Value))
  else
    SetDisplayText(ABuffer, VarToStr(Value));
end;

class procedure TcxStringValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  if AStream.IsUnicode then
    WriteWideStringProc(AStream, GetDisplayText(ABuffer))
  else
    WriteAnsiStringProc(AStream, dxStringToAnsiString(GetDisplayText(ABuffer)));
end;

{ TcxWideStringValueType }

class function TcxWideStringValueType.CompareValues(P1, P2: Pointer): Integer;

  function dxVarToWideString(const V: Variant): WideString;
  begin
    if VarIsNull(V) then
      Result := ''
    else
      Result := V;
  end;

var
  WS1, WS2: WideString;
begin
  if Assigned(P1) then
  begin
    if Assigned(P2) then
    begin
      WS1 := dxVarToWideString(GetDataValue(P1));
      WS2 := dxVarToWideString(GetDataValue(P2));
      if WS1 = WS2 then
        Result := 0
      else
        if WS1 < WS2 then
          Result := -1
        else
          Result := 1;
    end
    else
      Result := 1;
  end
  else
  begin
    if Assigned(P2) then
      Result := -1
    else
      Result := 0;
  end;
end;

class function TcxWideStringValueType.GetValue(ABuffer: PAnsiChar): Variant;
begin
  Result := GetDataValue(@ABuffer);
end;

class function TcxWideStringValueType.GetVarType: Integer;
begin
  Result := varOleStr;
end;

class function TcxWideStringValueType.IsString: Boolean;
begin
  Result := True;
end;

class procedure TcxWideStringValueType.PrepareValueBuffer(var ABuffer: PAnsiChar);
begin
  ABuffer := PPointer(ABuffer)^;
end;

class function TcxWideStringValueType.Compare(P1, P2: Pointer): Integer;
begin
  Result := CompareValues(P1, P2);
end;

class procedure TcxWideStringValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
  FreeMem(Pointer(PPointer(ABuffer)^));
end;

class function TcxWideStringValueType.GetDataSize: Integer;
begin
  Result := SizeOf(PWideStringValue);
end;

type
  TcxWideStringValue = record
    Length: Integer;
    Data: WideChar;
  end;

  PcxWideStringValue = ^TcxWideStringValue;

class function TcxWideStringValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
var
  P: PcxWideStringValue;
  W: WideString;
begin
  P := PPointer(ABuffer)^;
  if P <> nil then
  begin
    SetLength(W, P.Length);
    if P.Length > 0 then
      Move(P.Data, W[1], Length(W) * SizeOf(WideChar));
    Result := W;
  end
  else
    Result := inherited GetDataValue(ABuffer);
end;

class procedure TcxWideStringValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadWideStringFunc(AStream));
end;

class procedure TcxWideStringValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
var
  P: PcxWideStringValue;
  W: WideString;
begin
  P := PPointer(ABuffer)^;
  if P <> nil then
  begin
    FreeMem(P);
    P := nil;
  end;
  if not VarIsNull(Value) then
  begin
    W := Value;
    GetMem(P, Length(W) * SizeOf(WideChar) + SizeOf(Integer));
    P.Length := Length(W);
    if P.Length > 0 then
      Move(W[1], P.Data, P.Length * SizeOf(WideChar));
  end;
  PPointer(ABuffer)^ := P;
end;

class procedure TcxWideStringValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteWideStringProc(AStream, VarToStr(GetDataValue(ABuffer)));
end;

{ TcxSmallintValueType }

class function TcxSmallintValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  Result := PSmallInt(P1)^ - PSmallInt(P2)^;
end;

class function TcxSmallintValueType.GetVarType: Integer;
begin
  Result := varSmallint;
end;

class function TcxSmallintValueType.GetDataSize: Integer;
begin
  Result := SizeOf(SmallInt);
end;

class function TcxSmallintValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PSmallInt(ABuffer)^;
end;

class procedure TcxSmallintValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadSmallIntFunc(AStream));
end;

class procedure TcxSmallintValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PSmallInt(ABuffer)^ := Value;
end;

class procedure TcxSmallintValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteSmallIntProc(AStream, SmallInt(GetDataValue(ABuffer)));
end;

{ TcxIntegerValueType }

class function TcxIntegerValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  Result := dxCompareValues(PInteger(P1)^, PInteger(P2)^);
end;

class function TcxIntegerValueType.GetVarType: Integer;
begin
  Result := varInteger;
end;

class function TcxIntegerValueType.GetDataSize: Integer;
begin
  Result := SizeOf(Integer);
end;

class function TcxIntegerValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PInteger(ABuffer)^;
end;

class procedure TcxIntegerValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadIntegerFunc(AStream));
end;

class procedure TcxIntegerValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PInteger(ABuffer)^ := Value;
end;

class procedure TcxIntegerValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteIntegerProc(AStream, Integer(GetDataValue(ABuffer)));
end;

{ TcxWordValueType }

class function TcxWordValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  Result := PWord(P1)^ - PWord(P2)^;
end;

class function TcxWordValueType.GetVarType: Integer;
begin
  Result := varWord;
end;

class function TcxWordValueType.GetDataSize: Integer;
begin
  Result := SizeOf(Word);
end;

class function TcxWordValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PWord(ABuffer)^;
end;

class procedure TcxWordValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadWordFunc(AStream));
end;

class procedure TcxWordValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PWord(ABuffer)^ := Value;
end;

class procedure TcxWordValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteWordProc(AStream, Word(GetDataValue(ABuffer)));
end;

{ TcxBooleanValueType }

class function TcxBooleanValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  Result := Integer(PBoolean(P1)^) - Integer(PBoolean(P2)^);
end;

class function TcxBooleanValueType.GetVarType: Integer;
begin
  Result := varBoolean;
end;

class function TcxBooleanValueType.GetDataSize: Integer;
begin
  Result := SizeOf(Boolean);
end;

class function TcxBooleanValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PBoolean(ABuffer)^;
end;

class function TcxBooleanValueType.GetDefaultDisplayText(ABuffer: PAnsiChar): string;
begin
  try
    Result := BoolToStr(GetDataValue(ABuffer), True);
  except
    on EVariantError do
      Result := '';
  end;
end;

class procedure TcxBooleanValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadBooleanFunc(AStream));
end;

class procedure TcxBooleanValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PBoolean(ABuffer)^ := Value;
end;

class procedure TcxBooleanValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteBooleanProc(AStream, Boolean(GetDataValue(ABuffer)));
end;

  { TcxSingleValueType }

  class function TcxSingleValueType.CompareValues(P1, P2: Pointer): Integer;
  var
    D1, D2: Single;
  begin
    D1 := PSingle(P1)^;
    D2 := PSingle(P2)^;
    if D1 = D2 then
      Result := 0
    else
      if D1 < D2 then
        Result := -1
      else
        Result := 1;
  end;

  class function TcxSingleValueType.GetVarType: Integer;
  begin
    Result := varSingle;
  end;

  class function TcxSingleValueType.GetDataSize: Integer;
  begin
    Result := SizeOf(Single);
  end;

  class function TcxSingleValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
  begin
    Result := PSingle(ABuffer)^;
  end;

  class procedure TcxSingleValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
  var
    E: Single;
  begin
    ReadSingleProc(AStream, E);
    PSingle(ABuffer)^ := E;
  end;

  class procedure TcxSingleValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
  begin
    PSingle(ABuffer)^ := Value;
  end;

  class procedure TcxSingleValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
  begin
    WriteSingleProc(AStream, Single(GetDataValue(ABuffer)));
  end;

{ TcxFloatValueType }

class function TcxFloatValueType.CompareValues(P1, P2: Pointer): Integer;
var
  D1, D2: Double;
begin
  D1 := PDouble(P1)^;
  D2 := PDouble(P2)^;
  if D1 = D2 then
    Result := 0
  else
    if D1 < D2 then
      Result := -1
    else
      Result := 1;
end;

class function TcxFloatValueType.GetVarType: Integer;
begin
  Result := varDouble;
end;

class function TcxFloatValueType.GetDataSize: Integer;
begin
  Result := SizeOf(Double);
end;

class function TcxFloatValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PDouble(ABuffer)^;
end;

class procedure TcxFloatValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
var
  E: Extended;
begin
  ReadFloatProc(AStream, E);
  PDouble(ABuffer)^ := E;
end;

class procedure TcxFloatValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PDouble(ABuffer)^ := Value;
end;

class procedure TcxFloatValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteFloatProc(AStream, Double(GetDataValue(ABuffer)));
end;

{ TcxCurrencyValueType }

class function TcxCurrencyValueType.CompareValues(P1, P2: Pointer): Integer;
var
  C1, C2: Currency;
begin
  C1 := PCurrency(P1)^;
  C2 := PCurrency(P2)^;
  if C1 = C2 then
    Result := 0
  else
    if C1 < C2 then
      Result := -1
    else
      Result := 1;
end;

class function TcxCurrencyValueType.GetVarType: Integer;
begin
  Result := varCurrency;
end;

class function TcxCurrencyValueType.GetDataSize: Integer;
begin
  Result := SizeOf(Currency);
end;

class function TcxCurrencyValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PCurrency(ABuffer)^;
end;

class procedure TcxCurrencyValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadCurrencyFunc(AStream));
end;

class procedure TcxCurrencyValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PCurrency(ABuffer)^ := Value;
end;

class procedure TcxCurrencyValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteCurrencyProc(AStream, Currency(GetDataValue(ABuffer)));
end;

{ TcxDateTimeValueType }

class function TcxDateTimeValueType.CompareValues(P1, P2: Pointer): Integer;
var
  D1, D2: Double;
begin
  D1 := PDateTime(P1)^;
  D2 := PDateTime(P2)^;
  if D1 = D2 then
    Result := 0
  else
    if D1 < D2 then
      Result := -1
    else
      Result := 1;
end;

class function TcxDateTimeValueType.GetVarType: Integer;
begin
  Result := varDate;
end;

class function TcxDateTimeValueType.GetDataSize: Integer;
begin
  Result := SizeOf(TDateTime);
end;

class function TcxDateTimeValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := GetDateTime(ABuffer);
end;

class function TcxDateTimeValueType.GetDefaultDisplayText(ABuffer: PAnsiChar): string;
var
  DT: TDateTime;
begin
  DT := GetDateTime(ABuffer);
  try
    Result := VarToStr(DT);
  except
    on EVariantError do
      Result := DateTimeToStr(DT);
  end;
end;

class procedure TcxDateTimeValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadDateTimeFunc(AStream));
end;

class procedure TcxDateTimeValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PDateTime(ABuffer)^ := VarToDateTime(Value);
end;

class procedure TcxDateTimeValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteDateTimeProc(AStream, TDateTime(GetDataValue(ABuffer)));
end;

class function TcxDateTimeValueType.GetDateTime(ABuffer: PAnsiChar): TDateTime;
begin
  Result := PDateTime(ABuffer)^;
end;

{ TcxLargeIntValueType }

class function TcxLargeIntValueType.CompareValues(P1, P2: Pointer): Integer;
var
  L1, L2: LargeInt;
begin
  L1 := PLargeInt(P1)^;
  L2 := PLargeInt(P2)^;
  if L1 = L2 then
    Result := 0
  else
    if L1 < L2 then
      Result := -1
    else
      Result := 1;
end;

class function TcxLargeIntValueType.GetVarType: Integer;
begin
  Result := varInt64;
end;

class function TcxLargeIntValueType.GetDataSize: Integer;
begin
  Result := SizeOf(LargeInt);
end;

class function TcxLargeIntValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PLargeInt(ABuffer)^;
end;

class procedure TcxLargeIntValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadLargeIntFunc(AStream));
end;

class procedure TcxLargeIntValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PLargeInt(ABuffer)^ := Value;
end;

class procedure TcxLargeIntValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteLargeIntProc(AStream, PLargeInt(ABuffer)^);
end;

{$IFNDEF NONDB}
{ TcxFMTBcdValueType }

class function TcxFMTBcdValueType.CompareValues(P1, P2: Pointer): Integer;
var
  B1, B2: TBcd;
begin
  B1 := PBcd(P1)^;
  B2 := PBcd(P2)^;
  Result := BcdCompare(B1, B2);
end;

class function TcxFMTBcdValueType.GetVarType: Integer;
begin
  Result := VarFMTBcd;
end;

class function TcxFMTBcdValueType.GetDataSize: Integer;
begin
  Result := SizeOf(TBcd);
end;

class function TcxFMTBcdValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := VarFMTBcdCreate(PBcd(ABuffer)^);
//  Result := BcdToDouble(PBcd(ABuffer)^);
end;

class function TcxFMTBcdValueType.GetDefaultDisplayText(ABuffer: PAnsiChar): string;
var
  Bcd: TBcd;
begin
  Bcd := PBcd(ABuffer)^;
{$IFDEF DELPHI16}
  Result := BcdToStrF(Bcd, ffGeneral, Bcd.Precision, Bcd.SignSpecialPlaces); // P, D - must be used in XE2
{$ELSE}
  Result := BcdToStrF(Bcd, ffGeneral, 0, 0); // P, D - ignored in BcdToStrF
{$ENDIF}
end;

class procedure TcxFMTBcdValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PBcd(ABuffer)^ := VarToBcd(Value);
end;

{ TcxSQLTimeStampValueType }

class function TcxSQLTimeStampValueType.CompareValues(P1, P2: Pointer): Integer;
var
  T1, T2: TSQLTimeStamp;
begin
  T1 := PSQLTimeStamp(P1)^;
  T2 := PSQLTimeStamp(P2)^;
  Result := T1.Year - T2.Year;
  if Result = 0 then
  begin
    Result := T1.Month - T2.Month;
    if Result = 0 then
    begin
      Result := T1.Day - T2.Day;
      if Result = 0 then
      begin
        Result := T1.Hour - T2.Hour;
        if Result = 0 then
        begin
          Result := T1.Minute - T2.Minute;
          if Result = 0 then
          begin
            Result := T1.Second - T2.Second;
            if Result = 0 then
              Result := T1.Fractions - T2.Fractions;
          end;
        end;
      end;
    end;
  end;
end;

class function TcxSQLTimeStampValueType.GetVarType: Integer;
begin
  Result := VarSQLTimeStamp;
end;

class function TcxSQLTimeStampValueType.GetDataSize: Integer;
begin
  Result := SizeOf(TSQLTimeStamp);
end;

class function TcxSQLTimeStampValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := SQLTimeStampToDateTime(PSQLTimeStamp(ABuffer)^);
end;

class procedure TcxSQLTimeStampValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  PSQLTimeStamp(ABuffer)^ := VarToSQLTimeStamp(Value);
end;
{$ENDIF}

{ TcxVariantValueType }

class function TcxVariantValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  if Assigned(P1) then
  begin
    if Assigned(P2) then
    begin
      Result := VarCompare(PVariant(P1)^, PVariant(P2)^);
    end
    else
      Result := 1;
  end
  else
  begin
    if Assigned(P2) then
      Result := -1
    else
      Result := 0;
  end;
end;

class function TcxVariantValueType.GetValue(ABuffer: PAnsiChar): Variant;
begin
  Result := GetDataValue(@ABuffer);
end;

class procedure TcxVariantValueType.PrepareValueBuffer(var ABuffer: PAnsiChar);
begin
  ABuffer := PPointer(ABuffer)^;
end;

class function TcxVariantValueType.Compare(P1, P2: Pointer): Integer;
begin
  Result := CompareValues(PPointer(P1)^, PPointer(P2)^);
end;

class procedure TcxVariantValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
  Dispose(PVariant(PPointer(ABuffer)^));
end;

class function TcxVariantValueType.GetDataSize: Integer;
begin
  Result := SizeOf(PVariant);
end;

class function TcxVariantValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  Result := PVariant(PPointer(ABuffer)^)^;
end;

class procedure TcxVariantValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
var
  P: PVariant;
begin
  P := PPointer(ABuffer)^;
  if P = nil then
  begin
    New(P);
    PPointer(ABuffer)^ := P;
  end;
  P^ := Value;
end;

{ TcxBLOBValueType }

class function TcxBLOBValueType.CompareValues(P1, P2: Pointer): Integer;
begin
  if Assigned(P1) then
  begin
    if Assigned(P2) then
      Result := CompareStr(PAnsiString(P1)^, PAnsiString(P2)^)
    else
      Result := 1;
  end
  else
  begin
    if Assigned(P2) then
      Result := -1
    else
      Result := 0;
  end;
end;

class function TcxBLOBValueType.GetValue(ABuffer: PAnsiChar): Variant;
begin
  Result := GetDataValue(@ABuffer);
end;

class function TcxBLOBValueType.GetVarType: Integer;
begin
  Result := varString;
end;

class function TcxBLOBValueType.IsString: Boolean;
begin
  Result := True;
end;

class procedure TcxBLOBValueType.PrepareValueBuffer(var ABuffer: PAnsiChar);
begin
  ABuffer := PPointer(ABuffer)^;
end;

class function TcxBLOBValueType.Compare(P1, P2: Pointer): Integer;
begin
  Result := CompareValues(PPointer(P1)^, PPointer(P2)^);
end;

class procedure TcxBLOBValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
  Dispose(PAnsiString(PPointer(ABuffer)^));
end;

class function TcxBLOBValueType.GetAnsiString(ABuffer: PAnsiChar): AnsiString;
var
  P: PPointer;
begin
  P := PPointer(ABuffer)^;
  if P <> nil then
    Result := PAnsiString(P)^
  else
    Result := ''
end;

class function TcxBLOBValueType.GetDataSize: Integer;
begin
  Result := SizeOf(PStringValue);
end;

class function TcxBLOBValueType.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  if PPointer(ABuffer)^ <> nil then
    Result := GetAnsiString(ABuffer)
  else
    Result := inherited GetDataValue(ABuffer);
end;

class procedure TcxBLOBValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  SetDataValue(ABuffer, ReadAnsiStringFunc(AStream));
end;

class procedure TcxBLOBValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
var
  P: PAnsiString;
begin
  P := PPointer(ABuffer)^;
  if P = nil then
  begin
    New(P);
    PPointer(ABuffer)^ := P;
  end;
  P^ := dxVariantToAnsiString(Value);
end;

class procedure TcxBLOBValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  WriteAnsiStringProc(AStream, GetAnsiString(ABuffer));
end;

{ TcxObjectValueType }

class procedure TcxObjectValueType.FreeBuffer(ABuffer: PAnsiChar);
begin
  TObject(PPointer(ABuffer)^).Free;
end;

class procedure TcxObjectValueType.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  // not supported
end;

class procedure TcxObjectValueType.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  // TODO: if PInteger(ABuffer)^ <> 0 then FreeBuffer(ABuffer);
  inherited SetDataValue(ABuffer, Value);
end;

class procedure TcxObjectValueType.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  // not supported
end;

{ TcxValueDef }

constructor TcxValueDef.Create(AValueDefs: TcxValueDefs; AValueTypeClass: TcxValueTypeClass);
begin
  inherited Create;
  FValueDefs := AValueDefs;
  FValueTypeClass := AValueTypeClass;
  FStored := True;
  FTextStored := False;
  FStreamStored := True;
end;

destructor TcxValueDef.Destroy;
begin
  FValueDefs.Remove(Self);
  inherited Destroy;
end;

procedure TcxValueDef.Assign(ASource: TcxValueDef);
begin
  Stored := ASource.Stored;
  TextStored := ASource.TextStored;
end;

function TcxValueDef.CompareValues(AIsNull1, AIsNull2: Boolean; P1, P2: PAnsiChar): Integer;
begin
  if AIsNull1 then
  begin
    if AIsNull2 then
      Result := 0
    else
      Result := -1
  end
  else
  begin
    if AIsNull2 then
      Result := 1
    else
      Result := ValueTypeClass.CompareValues(P1, P2);
  end;
end;

procedure TcxValueDef.Changed(AResyncNeeded: Boolean);
begin
  if Assigned(ValueDefs) then
    ValueDefs.Changed(Self, AResyncNeeded);
end;

function TcxValueDef.Compare(P1, P2: PAnsiChar): Integer;
begin
  if IsNullValueEx(P1) then
  begin
    if IsNullValueEx(P2) then
      Result := 0
    else
      Result := -1
  end
  else
  begin
    if IsNullValueEx(P2) then
      Result := 1
    else
      Result := ValueTypeClass.Compare(GetDataFromBuffer(P1), GetDataFromBuffer(P2));
  end;
end;

procedure TcxValueDef.FreeBuffer(ABuffer: PAnsiChar);
begin
  if not Stored then Exit;
  if not IsNullValue(ABuffer) then
    ValueTypeClass.FreeBuffer(GetDataFromBuffer(ABuffer));
  if TextStored then
    FreeTextBuffer(GetDisplayTextFromBuffer(ABuffer));
end;

procedure TcxValueDef.FreeTextBuffer(ABuffer: PAnsiChar);
begin
  ValueTypeClass.FreeTextBuffer(ABuffer);
end;

function TcxValueDef.GetDataFromBuffer(ABuffer: PAnsiChar): PAnsiChar;
begin
  Result := ABuffer + FDataOffset;
end;

function TcxValueDef.GetDataValue(ABuffer: PAnsiChar): Variant;
begin
  if IsNullValue(ABuffer) then
    Result := Null
  else
    Result := ValueTypeClass.GetDataValue(GetDataFromBuffer(ABuffer));
end;

function TcxValueDef.GetDisplayText(ABuffer: PAnsiChar): string;
begin
  if TextStored then
    Result := ValueTypeClass.GetDisplayText(GetDisplayTextFromBuffer(ABuffer))
  else
  begin
    if IsNullValue(ABuffer) then
      Result := ''
    else
      Result := ValueTypeClass.GetDefaultDisplayText(GetDataFromBuffer(ABuffer));
  end;
end;

function TcxValueDef.GetDisplayTextFromBuffer(ABuffer: PAnsiChar): PAnsiChar;
begin
  Result := ABuffer + FDisplayTextOffset;
end;

function TcxValueDef.GetLinkObject: TObject;
begin
  Result := FLinkObject;
end;

function TcxValueDef.GetStored: Boolean;
begin
  Result := FStored or not ValueDefs.DataStorage.StoredValuesOnly;
end;

procedure TcxValueDef.Init(var AOffset: Integer);
begin
  FDataSize := ValueTypeClass.GetDataSize;
  FOffset := AOffset;
  FDataOffset := FOffset + ValueFlagSize;
  FDisplayTextOffset := FDataOffset + FDataSize;
  if Stored then
  begin
    Inc(AOffset, ValueFlagSize);
    Inc(AOffset, DataSize);
    if TextStored then
      Inc(AOffset, PointerSize);
    FBufferSize := AOffset - FOffset;
  end
  else
    FBufferSize := 0;
end;

function TcxValueDef.IsNullValue(ABuffer: PAnsiChar): Boolean;
begin
  Result := (ABuffer + Offset)^ = #0;
end;

function TcxValueDef.IsNullValueEx(ABuffer: PAnsiChar): Boolean;
begin
  Result := (ABuffer = nil) or IsNullValue(ABuffer);
end;

procedure TcxValueDef.ReadDataValue(ABuffer: PAnsiChar; AStream: TdxStream);

  function ReadNullFlag: Boolean;
  begin
    Result := ReadBooleanFunc(AStream);
  end;

begin
  if ReadNullFlag then
    SetNull(ABuffer, True)
  else
  begin
    SetNull(ABuffer, False);
    ValueTypeClass.ReadDataValue(GetDataFromBuffer(ABuffer), AStream);
  end;
end;

procedure TcxValueDef.ReadDisplayText(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  if TextStored then
    ValueTypeClass.ReadDisplayText(GetDisplayTextFromBuffer(ABuffer), AStream);
end;

procedure TcxValueDef.SetDataValue(ABuffer: PAnsiChar; const Value: Variant);
begin
  if VarIsNull(Value) then
    SetNull(ABuffer, True)
  else
  begin
    SetNull(ABuffer, False);
    ValueTypeClass.SetDataValue(GetDataFromBuffer(ABuffer), Value);
  end;
end;

procedure TcxValueDef.SetDisplayText(ABuffer: PAnsiChar; const DisplayText: string);
begin
  if TextStored then
    ValueTypeClass.SetDisplayText(GetDisplayTextFromBuffer(ABuffer), DisplayText);
end;

procedure TcxValueDef.SetLinkObject(Value: TObject);
begin
  FLinkObject := Value;
end;

procedure TcxValueDef.SetNull(ABuffer: PAnsiChar; AIsNull: Boolean);
begin
  Inc(ABuffer, Offset);
  if AIsNull then
  begin
    if ABuffer^ <> #0 then
    begin
      ABuffer^ := #0;
      Inc(ABuffer, ValueFlagSize);
      ValueTypeClass.FreeBuffer(ABuffer);
      FillChar(ABuffer^, DataSize, 0);
    end;
  end
  else
    ABuffer^ := #1;
end;

procedure TcxValueDef.WriteDataValue(ABuffer: PAnsiChar; AStream: TdxStream);

  procedure WriteNullFlag(ANull: Boolean);
  begin
    WriteBooleanProc(AStream, ANull);
  end;

begin
  if IsNullValue(ABuffer) then
    WriteNullFlag(True)
  else
  begin
    WriteNullFlag(False);
    ValueTypeClass.WriteDataValue(GetDataFromBuffer(ABuffer), AStream);
  end;
end;

procedure TcxValueDef.WriteDisplayText(ABuffer: PAnsiChar; AStream: TdxStream);
begin
  if TextStored then
    ValueTypeClass.WriteDisplayText(GetDisplayTextFromBuffer(ABuffer), AStream);
end;

function TcxValueDef.GetIsNeedConversion: Boolean;
begin
  Result := ValueTypeClass.IsString;
end;

function TcxValueDef.GetTextStored: Boolean;
begin
  if not Stored then
    Result := False
  else
    Result := FTextStored;
end;

procedure TcxValueDef.SetStored(Value: Boolean);
begin
  if FStored <> Value then
  begin
    FStored := Value;
    Changed(False);
  end;
end;

procedure TcxValueDef.SetTextStored(Value: Boolean);
begin
  if FTextStored <> Value then
  begin
    FTextStored := Value;
    Changed(True);
  end;
end;

procedure TcxValueDef.SetValueTypeClass(Value: TcxValueTypeClass);
begin
  if FValueTypeClass <> Value then
  begin
    FValueTypeClass := Value; // TODO: clear?
    Changed(True);
  end;
end;

{ TcxValueDefs }

constructor TcxValueDefs.Create(ADataStorage: TcxDataStorage);
begin
  inherited Create;
  FDataStorage := ADataStorage;
  FItems := TList.Create;
  DataStorage.InitStructure(Self);
end;

destructor TcxValueDefs.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

function TcxValueDefs.Add(AValueTypeClass: TcxValueTypeClass; AStored, ATextStored: Boolean; ALinkObject: TObject): TcxValueDef;
var
  I: Integer;
begin
  Result := GetValueDefClass.Create(Self, AValueTypeClass);
  Result.LinkObject := ALinkObject;
  Result.Stored := AStored;
  Result.TextStored := ATextStored;
  I := 0;
  Result.Init(I);
  DataStorage.InsertValueDef(FItems.Count, Result);
  FItems.Add(Result);
  DataStorage.InitStructure(Self);
end;

procedure TcxValueDefs.Clear;
begin
  while FItems.Count > 0 do
    TcxValueDef(FItems.Last).Free;
end;

procedure TcxValueDefs.Changed(AValueDef: TcxValueDef; AResyncNeeded: Boolean);
begin
  DataStorage.ValueDefsChanged(AValueDef, AResyncNeeded);
end;

function TcxValueDefs.GetValueDefClass: TcxValueDefClass;
begin
  Result := TcxValueDef;
end;

procedure TcxValueDefs.Prepare(AStartOffset: Integer);
var
  I, AOffset: Integer;
begin
  FRecordOffset := AStartOffset;
  AOffset := FRecordOffset;
  for I := 0 to Count - 1 do
    Items[I].Init(AOffset);
  FRecordSize := AOffset;
end;

procedure TcxValueDefs.Remove(AItem: TcxValueDef);
begin
  DataStorage.RemoveValueDef(AItem);
  FItems.Remove(AItem);
  DataStorage.InitStructure(Self);
end;

function TcxValueDefs.GetStoredCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if Items[I].Stored then
      Inc(Result);
end;

function TcxValueDefs.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxValueDefs.GetItem(Index: Integer): TcxValueDef;
begin
  if DataStorage.FValueDefsList <> nil then
    Result := TcxValueDef(DataStorage.FValueDefsList[Index])
  else
    Result := TcxValueDef(FItems[Index]);
end;

{ TcxInternalValueDef }

function TcxInternalValueDef.GetLinkObject: TObject;
begin
  if Assigned(FLinkObject) then
    Result := TcxValueDef(FLinkObject).LinkObject
  else
    Result := nil;
end;

function TcxInternalValueDef.GetStored: Boolean;
begin
  Result := True;
end;

function TcxInternalValueDef.GetValueDef: TcxValueDef;
begin
  Result := TcxValueDef(FLinkObject);
end;

{ TcxInternalValueDefs }

function TcxInternalValueDefs.FindByLinkObject(ALinkObject: TObject): TcxValueDef;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if Items[I].FLinkObject = ALinkObject then
    begin
      Result := Items[I] as TcxValueDef;
      Break;
    end;
end;

procedure TcxInternalValueDefs.RemoveByLinkObject(ALinkObject: TObject);
var
  AItem: TcxValueDef;
begin
  AItem := FindByLinkObject(ALinkObject);
  if AItem <> nil then
    AItem.Free;
end;

function TcxInternalValueDefs.GetValueDefClass: TcxValueDefClass;
begin
  Result := TcxInternalValueDef;
end;

{ TcxValueDefReader }

constructor TcxValueDefReader.Create;
begin
  inherited Create;
end;

function TcxValueDefReader.GetDisplayText(AValueDef: TcxValueDef): string;
begin
  Result := '';
end;

function TcxValueDefReader.GetValue(AValueDef: TcxValueDef): Variant;
begin
  Result := Null;
end;

function TcxValueDefReader.IsInternal(AValueDef: TcxValueDef): Boolean;
begin
  Result := False;
end;

{ TcxDataStorage }

constructor TcxDataStorage.Create;
begin
  inherited Create;
  FRecordIDCounter := 1; // TODO: reset
  FInternalValueDefs := TcxInternalValueDefs.Create(Self);
  FValueDefs := TcxValueDefs.Create(Self);
  FInternalRecordBuffers := TList.Create;
  FRecordBuffers := TList.Create;
end;

destructor TcxDataStorage.Destroy;
begin
  Clear(False);
  FValueDefs.Free;
  FInternalValueDefs.Free;
  FRecordBuffers.Free;
  FInternalRecordBuffers.Free;
  inherited Destroy;
end;

function TcxDataStorage.AddInternalRecord: Integer;
var
  I: Integer;
  P: PAnsiChar;
begin
  Result := 0;
  for I := -1 downto -FInternalRecordBuffers.Count do
  begin
    if not IsRecordFlag(RecordBuffers[I], RecordFlag_Busy) then
    begin
      Result := I;
      Break;
    end;
  end;
  if Result = 0 then
    Result := -FInternalRecordBuffers.Add(nil) - 1;
  P := AllocRecordBuffer(Result);
  ChangeRecordFlag(P, RecordFlag_Busy, True);
end;

function TcxDataStorage.AppendRecord: Integer;
begin
  Result := FRecordBuffers.Add(nil);
  CheckRecordID(Result);
end;

procedure TcxDataStorage.BeforeDestruction;
begin
  FDestroying := True;
  inherited BeforeDestruction;
end;

procedure TcxDataStorage.BeginLoad;
begin
  CheckStructure;
end;

procedure TcxDataStorage.CheckStructure;
begin
(*
  if FValueDefsChanged then
  begin
    InitStructure(ValueDefs);
    // !
    ClearInternalRecords;
    InitStructure(InternalValueDefs);
    // !
    FValueDefsChanged := False;
  end;
  *)
end;

procedure TcxDataStorage.Clear(AWithoutInternal: Boolean);
begin
  if not AWithoutInternal then
    ClearInternalRecords;
  ClearRecords(True);
end;

procedure TcxDataStorage.ClearInternalRecords;
var
  I: Integer;
begin
  for I := -FInternalRecordBuffers.Count to -1 do
    FreeAndNilRecordBuffer(I);
  FInternalRecordBuffers.Clear;
  if Assigned(FOnClearInternalRecords) then
    FOnClearInternalRecords(Self);
end;

procedure TcxDataStorage.ClearRecords(AClearList: Boolean);
var
  I: Integer;
begin
  for I := 0 to FRecordBuffers.Count - 1 do
    FreeAndNilRecordBuffer(I);
  if AClearList then
    FRecordBuffers.Clear;
  CheckRecordIDCounter;
  CheckRecordID(-1); // all
end;

function TcxDataStorage.CompareRecords(ARecordIndex1, ARecordIndex2: Integer;
  AValueDef: TcxValueDef): Integer;
var
  P1, P2: PAnsiChar;
begin
  P1 := RecordBuffers[ARecordIndex1];
  P2 := RecordBuffers[ARecordIndex2];
  Result := AValueDef.Compare(P1, P2);
end;

procedure TcxDataStorage.DeleteRecord(ARecordIndex: Integer);
begin
  if ARecordIndex < 0 then
    DeleteInternalRecord(ARecordIndex)
  else
  begin
    FreeAndNilRecordBuffer(ARecordIndex);
    FRecordBuffers.Delete(ARecordIndex);
    CheckRecordIDCounter;
  end;
end;

procedure TcxDataStorage.EndLoad;
begin
end;

function TcxDataStorage.GetDisplayText(ARecordIndex: Integer; AValueDef: TcxValueDef): string;
var
  P: PAnsiChar;
begin
  Result := '';
  P := RecordBuffers[ARecordIndex];
  if (P <> nil) and CheckValueDef(ARecordIndex, AValueDef) then
    Result := AValueDef.GetDisplayText(P);
end;

function TcxDataStorage.GetCompareInfo(ARecordIndex: Integer; AValueDef: TcxValueDef;
  var P: PAnsiChar): Boolean;
begin
  P := RecordBuffers[ARecordIndex];
  Result := AValueDef.IsNullValue(P);
  if not Result then
  begin
    P := AValueDef.GetDataFromBuffer(P);
    AValueDef.ValueTypeClass.PrepareValueBuffer(P);
  end;
end;

function TcxDataStorage.GetRecordID(ARecordIndex: Integer): Integer;
var
  P: PAnsiChar;
begin
  P := AllocRecordBuffer(ARecordIndex);
  Inc(P, RecordFlagSize);
  Result := PInteger(P)^;
end;

function TcxDataStorage.GetValue(ARecordIndex: Integer; AValueDef: TcxValueDef): Variant;
var
  P: PAnsiChar;
begin
  Result := Null;
  P := RecordBuffers[ARecordIndex];
  if (P <> nil) and CheckValueDef(ARecordIndex, AValueDef) then
    Result := AValueDef.GetDataValue(P);
end;

procedure TcxDataStorage.InsertRecord(ARecordIndex: Integer);
begin
  FRecordBuffers.Insert(ARecordIndex, nil);
  CheckRecordID(ARecordIndex);
end;

procedure TcxDataStorage.ReadData(ARecordIndex: Integer; AStream: TStream);

  function ReadNilFlag: Boolean;
  begin
    Result := ReadBooleanFunc(AStream);
  end;

var
  P: PAnsiChar;
  I, AID: Integer;
  AValueDef: TcxValueDef;
  AdxStream: TdxStream;
begin
  AdxStream := TdxStream.Create(AStream);
  try
    if ReadNilFlag then
      FreeAndNilRecordBuffer(ARecordIndex)
    else
    begin
      P := AllocRecordBuffer(ARecordIndex);
      if UseRecordID then
      begin
        AID := ReadIntegerFunc(AStream);
        SetRecordID(ARecordIndex, AID);
        CheckRecordIDCounterAfterLoad(AID);
      end;
      for I := 0 to ValueDefs.Count - 1 do
      begin
        AValueDef := ValueDefs[I];
        if AValueDef.StreamStored then
        begin
          AValueDef.ReadDataValue(P, AdxStream);
          if AValueDef.TextStored then
            AValueDef.ReadDisplayText(P, AdxStream);
        end;
      end;
    end;
  finally
    AdxStream.Free;
  end;
end;

procedure TcxDataStorage.ReadDataByFields(ARecordIndex: Integer; AValueDefReader: TcxValueDefReader; AFields: TList);
var
  P: PAnsiChar;
  I: Integer;
  AValueDef: TcxValueDef;
  AValueDefs: TcxValueDefs;
begin
  P := AllocRecordBuffer(ARecordIndex);
  AValueDefs := ValueDefsByRecordIndex(ARecordIndex);
  for I := 0 to AValueDefs.Count - 1 do
  begin
    AValueDef := AValueDefs[I];
    if not AValueDefReader.IsInternal(AValueDef) then
    begin
      if AFields.IndexOf(AValueDef.LinkObject) < 0 then
        Continue;
      AValueDef.SetDataValue(P, AValueDefReader.GetValue(AValueDef));
      if AValueDef.TextStored then
        AValueDef.SetDisplayText(P, AValueDefReader.GetDisplayText(AValueDef));
    end;
  end;
end;

procedure TcxDataStorage.ReadRecord(ARecordIndex: Integer; AValueDefReader: TcxValueDefReader);
var
  P: PAnsiChar;
  I: Integer;
  AValueDef: TcxValueDef;
  AValueDefs: TcxValueDefs;
begin
  P := AllocRecordBuffer(ARecordIndex);
  AValueDefs := ValueDefsByRecordIndex(ARecordIndex);
  for I := 0 to AValueDefs.Count - 1 do
  begin
    AValueDef := AValueDefs[I];
    if not AValueDefReader.IsInternal(AValueDef) then
    begin
      AValueDef.SetDataValue(P, AValueDefReader.GetValue(AValueDef));
      if AValueDef.TextStored then
        AValueDef.SetDisplayText(P, AValueDefReader.GetDisplayText(AValueDef));
    end;
  end;
end;

procedure TcxDataStorage.ReadRecordFrom(AFromRecordIndex, AToRecordIndex: Integer;
  AValueDefReader: TcxValueDefReader; ASetProc: TcxValueDefSetProc);
var
  I: Integer;
  AValueDefs: TcxValueDefs;
begin
  AValueDefs := ValueDefsByRecordIndex(AFromRecordIndex);
  for I := 0 to AValueDefs.Count - 1 do
    ASetProc(AValueDefs[I], AFromRecordIndex, AToRecordIndex, AValueDefReader);
end;

procedure TcxDataStorage.SetDisplayText(ARecordIndex: Integer; AValueDef: TcxValueDef;
  const Value: string);
var
  P: PAnsiChar;
begin
  P := AllocRecordBuffer(ARecordIndex);
  if CheckValueDef(ARecordIndex, AValueDef) and AValueDef.TextStored then
    AValueDef.SetDisplayText(P, Value);
end;

procedure TcxDataStorage.SetRecordID(ARecordIndex, AID: Integer);
var
  P: PAnsiChar;
begin
  P := AllocRecordBuffer(ARecordIndex);
  Inc(P, RecordFlagSize);
  PInteger(P)^ := AID;
end;

procedure TcxDataStorage.SetValue(ARecordIndex: Integer; AValueDef: TcxValueDef;
  const Value: Variant);
var
  P: PAnsiChar;
begin
  P := AllocRecordBuffer(ARecordIndex);
  if CheckValueDef(ARecordIndex, AValueDef) then
    AValueDef.SetDataValue(P, Value);
end;

procedure TcxDataStorage.WriteData(ARecordIndex: Integer; AStream: TStream);

  procedure WriteRecordInfo(ABuffer: PAnsiChar);
  var
    AID: Integer;
  begin
    WriteBooleanProc(AStream, ABuffer = nil);
    if (ABuffer <> nil) and UseRecordID then
    begin
      AID := 0;
      if ABuffer <> nil then
      begin
        Inc(ABuffer, RecordFlagSize);
        AID := PInteger(ABuffer)^;
      end;
      WriteIntegerProc(AStream, AID);
    end;
  end;

var
  P: PAnsiChar;
  I: Integer;
  AValueDef: TcxValueDef;
  AdxStream: TdxStream;
begin
  P := FRecordBuffers[ARecordIndex];
  I := AStream.Position;
  dxWriteStreamType(AStream);
  AStream.Position := I;
  AdxStream := TdxStream.Create(AStream);
  try
    WriteRecordInfo(P);
    if P <> nil then
      for I := 0 to ValueDefs.Count - 1 do
      begin
        AValueDef := ValueDefs[I];
        if AValueDef.StreamStored then
        begin
          AValueDef.WriteDataValue(P, AdxStream);
          if AValueDef.TextStored then
            AValueDef.WriteDisplayText(P, AdxStream);
        end;
      end;
  finally
    AdxStream.Free;
  end;
end;

procedure TcxDataStorage.BeginStreaming(ACompare: TListSortCompare);
var
  I: Integer;
  AList: TList;
begin
  AList := TList.Create;
  for I := 0 to ValueDefs.Count - 1 do
    AList.Add(ValueDefs[I]);
  AList.Sort(ACompare);
  FValueDefsList := AList;
end;

procedure TcxDataStorage.EndStreaming;
begin
  FValueDefsList.Free;
  FValueDefsList := nil;
end;

function TcxDataStorage.AllocRecordBuffer(Index: Integer): PAnsiChar;
var
  AValueDefs: TcxValueDefs;
begin
  Result := RecordBuffers[Index];
  if Result = nil then
  begin
    AValueDefs := ValueDefsByRecordIndex(Index);
    Result := AllocMem(AValueDefs.RecordSize);
    RecordBuffers[Index] := Result;
  end;
end;

function TcxDataStorage.CalcRecordOffset: Integer;
begin
  Result := RecordFlagSize;
  if UseRecordID then
    Inc(Result, RecordIDSize);
end;

procedure TcxDataStorage.ChangeRecordFlag(ABuffer: PAnsiChar; AFlag: Byte; ATurnOn: Boolean);
begin
  if ABuffer <> nil then
    if ATurnOn then
      PByte(ABuffer)^ := PByte(ABuffer)^ or AFlag
    else
      PByte(ABuffer)^ := PByte(ABuffer)^ and not AFlag;
end;

procedure TcxDataStorage.CheckRecordID(ARecordIndex: Integer);

  procedure CheckID(AIndex: Integer);
  begin
    if GetRecordID(AIndex) = 0 then
    begin
      SetRecordID(AIndex, FRecordIDCounter);
      Inc(FRecordIDCounter);
    end;
  end;

var
  I: Integer;
begin
  if not UseRecordID then Exit;
  if ARecordIndex <> -1 then
    CheckID(ARecordIndex)
  else
    for I := 0 to RecordCount - 1 do
      CheckID(I);
end;

procedure TcxDataStorage.CheckRecordIDCounter;
begin
  if FRecordBuffers.Count = 0 then FRecordIDCounter := 1; // TODO: reset
end;

procedure TcxDataStorage.CheckRecordIDCounterAfterLoad(ALoadedID: Integer);
begin
  if FRecordIDCounter <= ALoadedID then
    FRecordIDCounter := ALoadedID + 1;
end;

function TcxDataStorage.CheckValueDef(ARecordIndex: Integer; var AValueDef: TcxValueDef): Boolean;
begin
  if not (AValueDef is TcxInternalValueDef) and
    (ValueDefsByRecordIndex(ARecordIndex) = InternalValueDefs) then
    AValueDef := InternalValueDefs.FindByLinkObject(AValueDef);
  Result := AValueDef <> nil;
end;

procedure TcxDataStorage.DeleteInternalRecord(ARecordIndex: Integer);
//var
//  P: PAnsiChar;
begin
  if ARecordIndex >= 0 then Exit;
//  P := RecordBuffers[ARecordIndex];
//  ChangeRecordFlag(P, RecordFlag_Busy, False);
  FreeAndNilRecordBuffer(ARecordIndex);
end;

procedure TcxDataStorage.FreeAndNilRecordBuffer(AIndex: Integer);
var
  P: PAnsiChar;
  I: Integer;
  AValueDefs: TcxValueDefs;
begin
  P := RecordBuffers[AIndex];
  if P <> nil then
  begin
    AValueDefs := ValueDefsByRecordIndex(AIndex);
    RecordBuffers[AIndex] := nil;
    for I := 0 to AValueDefs.Count - 1 do
      AValueDefs[I].FreeBuffer(P);
    FreeMem(P);
  end;
end;

procedure TcxDataStorage.InitStructure(AValueDefs: TcxValueDefs);
begin
  AValueDefs.Prepare(CalcRecordOffset);
end;

procedure TcxDataStorage.InsertValueDef(AIndex: Integer; AValueDef: TcxValueDef);
var
  I, AStartIndex, AEndIndex: Integer;
  ABuffer, PSource, PDest: PAnsiChar;
  AValueDefs: TcxValueDefs;
begin
  AValueDefs := AValueDef.ValueDefs;
  if AValueDefs = ValueDefs then
  begin
    InternalValueDefs.Add(AValueDef.ValueTypeClass, True, AValueDef.FTextStored, AValueDef);
    AStartIndex := 0;
    AEndIndex := FRecordBuffers.Count - 1;
  end
  else
  begin
    AStartIndex := -FInternalRecordBuffers.Count;
    AEndIndex := -1;
  end;
  for I := AStartIndex to AEndIndex do
  begin
    ABuffer := RecordBuffers[I];
    if ABuffer <> nil then
    begin
      ReallocMem(ABuffer, AValueDefs.RecordSize + AValueDef.BufferSize);
      RecordBuffers[I] := ABuffer;
      if AIndex < AValueDefs.Count then
      begin
        PSource := ABuffer + AValueDefs[AIndex].Offset;
        PDest := PSource + AValueDef.BufferSize;
        System.Move(PSource^, PDest^, AValueDefs.RecordSize - (PSource - ABuffer));
      end
      else
      begin
        PSource := ABuffer;
        if AValueDefs.Count > 0 then
          Inc(PSource, AValueDefs[AValueDefs.Count - 1].Offset + AValueDefs[AValueDefs.Count - 1].BufferSize)
        else
          Inc(PSource, AValueDefs.RecordSize);
      end;
      FillChar(PSource^, AValueDef.BufferSize, 0);
    end;
  end;
end;

function TcxDataStorage.IsRecordFlag(ABuffer: PAnsiChar; AFlag: Byte): Boolean;
begin
  Result := (ABuffer <> nil) and ((PByte(ABuffer)^ and AFlag) = AFlag);
end;

procedure TcxDataStorage.RemoveValueDef(AValueDef: TcxValueDef);
var
  I, AStartIndex, AEndIndex: Integer;
  ABuffer, PSource, PDest: PAnsiChar;
  AValueDefs: TcxValueDefs;
  AFreeAndNil: Boolean;
begin
  AValueDefs := AValueDef.ValueDefs;
  if AValueDefs = ValueDefs then
  begin
    InternalValueDefs.RemoveByLinkObject(AValueDef);
    AStartIndex := 0;
    AEndIndex := FRecordBuffers.Count - 1;
  end
  else
  begin
    AStartIndex := -FInternalRecordBuffers.Count;
    AEndIndex := -1;
  end;
  AFreeAndNil := AValueDef.Stored and (AValueDefs.StoredCount <= 1);
  for I := AStartIndex to AEndIndex do
  begin
    ABuffer := RecordBuffers[I];
    if ABuffer <> nil then
      if AFreeAndNil then
        FreeAndNilRecordBuffer(I)
      else
        if AValueDef.Stored then
        begin
          AValueDef.FreeBuffer(ABuffer);
          PDest := ABuffer + AValueDef.Offset;
          PSource := PDest + AValueDef.BufferSize;
          System.Move(PSource^, PDest^, AValueDefs.RecordSize - (PSource - ABuffer));
          ReallocMem(ABuffer, AValueDefs.RecordSize - AValueDef.BufferSize); // existing data in the block is not affected!
          RecordBuffers[I] := ABuffer;
        end;
  end;
end;

procedure TcxDataStorage.ValueDefsChanged(AValueDef: TcxValueDef; AResyncNeeded: Boolean);
//var
//  AInternalValueDef: TcxValueDef;
begin
  (*
  if FDestroying then Exit;
  if not FValueDefsChanged then
  begin
    ClearRecords(False);
    FValueDefsChanged := True;
    if AResyncNeeded and (AValueDef.ValueDefs = ValueDefs) then
    begin
      AInternalValueDef := InternalValueDefs.FindByLinkObject(AValueDef);
      if AInternalValueDef <> nil then
        AInternalValueDef.Assign(AValueDef);
    end;
  end;
  *)
end;

function TcxDataStorage.ValueDefsByRecordIndex(Index: Integer): TcxValueDefs;
begin
  if Index < 0 then
    Result := FInternalValueDefs
  else
    Result := FValueDefs;
end;

function TcxDataStorage.GetRecordBuffer(Index: Integer): PAnsiChar;
begin
  if Index >= 0 then
    Result := PAnsiChar(FRecordBuffers[Index])
  else
    Result := PAnsiChar(FInternalRecordBuffers[-Index - 1]);
end;

function TcxDataStorage.GetRecordCount: Integer;
begin
  Result := FRecordBuffers.Count;
end;

procedure TcxDataStorage.SetStoredValuesOnly(Value: Boolean);
begin
  if FStoredValuesOnly <> Value then
  begin
    ClearRecords(False);
    FStoredValuesOnly := Value;
    InitStructure(ValueDefs);
  end;
end;

procedure TcxDataStorage.SetRecordBuffer(Index: Integer; Value: PAnsiChar);
begin
  if Index >= 0 then
    FRecordBuffers[Index] := Value
  else
    FInternalRecordBuffers[-Index - 1] := Value;
end;

procedure TcxDataStorage.SetRecordCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if RecordCount <> Value then
  begin
    // TODO: Capacity
    while RecordCount < Value do
      AppendRecord;
    while RecordCount > Value do
      DeleteRecord(RecordCount - 1);
  end;
end;

procedure TcxDataStorage.SetUseRecordID(Value: Boolean);
begin
  if FUseRecordID <> Value then
  begin
    ClearRecords(False);
    FUseRecordID := Value;
    InitStructure(ValueDefs);
  end;
end;

{ TcxDataStorageHelper }

class function TcxDataStorageHelper.AllocateRecord(
  ADataStorage: TcxDataStorage): Pointer;
begin
  Result := AllocMem(ADataStorage.FValueDefs.RecordSize);
end;

class function TcxDataStorageHelper.AppendRecord(
  ADataStorage: TcxDataStorage; ARecord: Pointer): Integer;
begin
  Result := ADataStorage.FRecordBuffers.Add(ARecord);
end;

class function TcxDataStorageHelper.Compare(
  AValueDef: TcxValueDef; ARecord1, ARecord2: Pointer): Integer;
begin
  Result := AValueDef.Compare(ARecord1, ARecord2);
end;

class procedure TcxDataStorageHelper.FreeBuffer(
  AValueDef: TcxValueDef; ARecord: Pointer);
begin
  AValueDef.FreeBuffer(ARecord);
end;

class function TcxDataStorageHelper.GetDisplayText(
  AValueDef: TcxValueDef; ARecord: Pointer): string;
begin
  Result := AValueDef.GetDisplayText(ARecord);
end;

class function TcxDataStorageHelper.GetRecordSize(
  ADataStorage: TcxDataStorage): Integer;
begin
  Result := ADataStorage.ValueDefs.RecordSize;
end;

class function TcxDataStorageHelper.GetStoredCount(
  ADataStorage: TcxDataStorage): Integer;
begin
  Result := ADataStorage.ValueDefs.StoredCount;
end;

class function TcxDataStorageHelper.GetValue(
  AValueDef: TcxValueDef; ARecord: Pointer): Variant;
begin
  if ARecord <> nil then
    Result := AValueDef.GetDataValue(ARecord)
  else
    Result := Null;
end;

class function TcxDataStorageHelper.RemoveRecord(
  ADataStorage: TcxDataStorage; ARecordIndex: Integer): Pointer;
begin
  Result := ADataStorage.RecordBuffers[ARecordIndex];
  ADataStorage.FRecordBuffers.Delete(ARecordIndex);
end;

class procedure TcxDataStorageHelper.SetDisplayText(
  ARecord: Pointer; AValueDef: TcxValueDef; const AValue: string);
begin
  AValueDef.SetDisplayText(ARecord, AValue);
end;

class procedure TcxDataStorageHelper.SetRecordsCapacity(
  ADataStorage: TcxDataStorage; ACapacity: Integer);
begin
  ADataStorage.FRecordBuffers.Capacity := ACapacity;
end;

class procedure TcxDataStorageHelper.SetTextStored(
  AValueDef: TcxValueDef; ATextStored: Boolean);
begin
  if AValueDef.FTextStored <> ATextStored then
  begin
    AValueDef.FTextStored := ATextStored;
    AValueDef.ValueDefs.DataStorage.InitStructure(AValueDef.ValueDefs);
  end;
end;

class procedure TcxDataStorageHelper.SetValue(ARecord: Pointer;
  AValueDef: TcxValueDef; const AValue: Variant);
begin
  AValueDef.SetDataValue(ARecord, AValue);
end;

class procedure TcxDataStorageHelper.FreeRecord(
  ADataStorage: TcxDataStorage; var ARecord: Pointer);
var
  I: Integer;
  P: Pointer;
begin
  P := ARecord;
  ARecord := nil;
  for I := 0 to ADataStorage.FValueDefs.Count - 1 do
    ADataStorage.FValueDefs[I].FreeBuffer(P);
  FreeMem(P);
end;


{ TcxLookupList }

constructor TcxLookupList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxLookupList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TcxLookupList.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Dispose(PcxLookupListItem(FItems[I]));
  FItems.Clear;
end;

function TcxLookupList.Find(const AKeyValue: Variant; var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  AIndex := 0;
  Result := False;
  L := 0;
  H := FItems.Count - 1;
  if L <= H then
    repeat
      I := (L + H) div 2;
      C := VarCompare(PcxLookupListItem(FItems[I]).KeyValue, AKeyValue);
      if C = 0 then
      begin
        AIndex := I;
        Result := True;
        Break;
      end
      else
        if C < 0 then
          L := I + 1
        else
          H := I - 1;
      if L > H then
      begin
        AIndex := L;
        Break;
      end;
    until False;
end;

procedure TcxLookupList.Insert(AIndex: Integer; const AKeyValue: Variant;
  const ADisplayText: string);
var
  P: PcxLookupListItem;
begin
  New(P);
  P.KeyValue := AKeyValue;
  P.DisplayText := ADisplayText;
  FItems.Insert(AIndex, P);
end;

function TcxLookupList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxLookupList.GetItem(Index: Integer): PcxLookupListItem;
begin
  Result := PcxLookupListItem(FItems[Index]);
end;

{ TcxValueTypeClassList }

constructor TcxValueTypeClassList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxValueTypeClassList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TcxValueTypeClassList.ItemByCaption(const ACaption: string): TcxValueTypeClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
    if TcxValueTypeClass(FItems[I]).Caption = ACaption then
    begin
      Result := TcxValueTypeClass(FItems[I]);
      Break;
    end;
end;

procedure TcxValueTypeClassList.RegisterItem(AValueTypeClass: TcxValueTypeClass);
begin
  if FItems.IndexOf(TObject(AValueTypeClass)) = -1 then
    FItems.Add(TObject(AValueTypeClass));
end;

procedure TcxValueTypeClassList.UnregisterItem(AValueTypeClass: TcxValueTypeClass);
begin
  FItems.Remove(TObject(AValueTypeClass));
end;

function TcxValueTypeClassList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxValueTypeClassList.GetItem(Index: Integer): TcxValueTypeClass;
begin
  Result := TcxValueTypeClass(FItems[Index]);
end;

initialization
  cxValueTypeClassList.RegisterItem(TcxStringValueType);
  cxValueTypeClassList.RegisterItem(TcxWideStringValueType);
  cxValueTypeClassList.RegisterItem(TcxSmallintValueType);
  cxValueTypeClassList.RegisterItem(TcxIntegerValueType);
  cxValueTypeClassList.RegisterItem(TcxWordValueType);
  cxValueTypeClassList.RegisterItem(TcxBooleanValueType);
  cxValueTypeClassList.RegisterItem(TcxSingleValueType);
  cxValueTypeClassList.RegisterItem(TcxFloatValueType);
  cxValueTypeClassList.RegisterItem(TcxCurrencyValueType);
  cxValueTypeClassList.RegisterItem(TcxDateTimeValueType);
  cxValueTypeClassList.RegisterItem(TcxLargeIntValueType);
{$IFNDEF NONDB}
  cxValueTypeClassList.RegisterItem(TcxFMTBcdValueType);
  cxValueTypeClassList.RegisterItem(TcxSQLTimeStampValueType);
{$ENDIF}
  cxValueTypeClassList.RegisterItem(TcxVariantValueType);
  cxValueTypeClassList.RegisterItem(TcxObjectValueType);
  cxValueTypeClassList.RegisterItem(TcxBLOBValueType);

finalization
  cxValueTypeClassList.UnregisterItem(TcxBLOBValueType);
  cxValueTypeClassList.UnregisterItem(TcxObjectValueType);
  cxValueTypeClassList.UnregisterItem(TcxVariantValueType);
{$IFNDEF NONDB}
  cxValueTypeClassList.UnregisterItem(TcxSQLTimeStampValueType);
  cxValueTypeClassList.UnregisterItem(TcxFMTBcdValueType);
{$ENDIF}
  cxValueTypeClassList.UnregisterItem(TcxLargeIntValueType);
  cxValueTypeClassList.UnregisterItem(TcxDateTimeValueType);
  cxValueTypeClassList.UnregisterItem(TcxCurrencyValueType);
  cxValueTypeClassList.UnregisterItem(TcxFloatValueType);
  cxValueTypeClassList.UnregisterItem(TcxSingleValueType);
  cxValueTypeClassList.UnregisterItem(TcxBooleanValueType);
  cxValueTypeClassList.UnregisterItem(TcxWordValueType);
  cxValueTypeClassList.UnregisterItem(TcxIntegerValueType);
  cxValueTypeClassList.UnregisterItem(TcxSmallintValueType);
  cxValueTypeClassList.UnregisterItem(TcxWideStringValueType);
  cxValueTypeClassList.UnregisterItem(TcxStringValueType);

  FValueTypeClassList.Free;
  FValueTypeClassList := nil;

end.
