{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxBase;

interface

{$I cxVer.inc}

uses
  Types, Classes, dxPSSngltn;

type
  TdxBaseObject = class;

  TdxLockState = (lsUnlock, lsLock);

  TdxLockUpdateEvent = procedure(Sender: TdxBaseObject; ALockState: TdxLockState) of object;

  TdxBaseObjectClass = class of TdxBaseObject;

  TdxBaseObject = class(TInterfacedPersistent)
  private
    FUpdateCount: Integer;
    FOnLockUpdate: TdxLockUpdateEvent;
  protected
    procedure DoAssign(Source: TdxBaseObject); virtual;
    procedure DoRestoreDefaults; virtual;

    function IsLocked: Boolean;
    procedure LockUpdate(ALockState: TdxLockState); dynamic;

    property UpdateCount: Integer read FUpdateCount;
    property OnLockUpdate: TdxLockUpdateEvent read FOnLockUpdate write FOnLockUpdate;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

    function Clone: TdxBaseObject; virtual;
    function IsEmpty: Boolean; virtual;
    function IsEqual(ABaseObject: TdxBaseObject): Boolean; virtual;
    procedure RestoreDefaults;

    procedure LoadFromFile(const AFileName: string); dynamic;
    procedure LoadFromStream(AStream: TStream); dynamic;
    procedure SaveToFile(const AFileName: string); dynamic;
    procedure SaveToStream(AStream: TStream); dynamic;
  end;

  TdxClassList = class(TList)
  private
    function GetItem(Index: Integer): TClass;
    procedure SetItem(Index: Integer; Value: TClass);
  public
    function Add(AClass: TClass; ACheckExistence: Boolean = True): Integer;
    function Find(AClass: TClass; out AnIndex: Integer): Boolean; overload;
    function Find(AClass: TClass): Boolean; overload;
    function IndexOf(AClass: TClass): Integer;
    procedure Insert(Index: Integer; AClass: TClass; ACheckExistence: Boolean = True);
    function Remove(AClass: TClass): Integer;
    function Extract(AClass: TClass): TClass;
    function First: TClass;
    function Last: TClass;

    property Items[Index: Integer]: TClass read GetItem write SetItem; default;
  end;

  TdxPersistentClassList = class(TdxClassList)
  private
    function GetItem(Index: Integer): TPersistentClass;
    procedure SetItem(Index: Integer; Value: TPersistentClass);
  protected
    procedure UnregisterAll; virtual;
  public
    procedure Clear; override;
    function Extract(AClass: TPersistentClass): TPersistentClass;
    function First: TPersistentClass;
    function Last: TPersistentClass;

    function Register(AClass: TPersistentClass): Integer; overload; virtual;
    procedure Register(AnIndex: Integer; AClass: TPersistentClass); overload; virtual;
    procedure Unregister(AClass: TPersistentClass); virtual;

    property Items[Index: Integer]: TPersistentClass read GetItem write SetItem; default;
  end;

  TdxCustomCache = class
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TObject;
  protected
    function Add(AnObject: TObject): Integer;
    procedure FreeAndNilItems;
    function IndexOfByClass(AClass: TClass): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TObject read GetItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
  end;

  { Class Factories }

  TdxCustomClassFactory = class(TBasedxPSSingleton)
  private
    FItems: TdxClassList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TClass;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
    property Items[Index: Integer]: TClass read GetItem;
  public
    function IndexOf(AClass: TClass): Integer;
    procedure Register(AClass: TClass); virtual;
    procedure Unregister(AClass: TClass); virtual;
    procedure UnregisterAll; virtual;
    property Count: Integer read GetCount;
  end;

  { Maps }

  TdxCustomClassMapItemClass = class of TdxCustomClassMapItem;

  TdxCustomClassMapItem = class
  public
    class function PairClass: TClass; virtual;
  end;

  TdxCustomClassMaps = class(TdxCustomClassFactory)
  private
    function GetItem(Index: Integer): TdxCustomClassMapItemClass;
  protected
    function GetPairClass(AClass: TClass): TdxCustomClassMapItemClass; virtual;

    property Items[Index: Integer]: TdxCustomClassMapItemClass read GetItem;
    property PairClasses[AClass: TClass]: TdxCustomClassMapItemClass read GetPairClass;
  end;

procedure dxSavePersistent(AStream: TStream; APersistent: TPersistent);
procedure dxLoadPersistent(AStream: TStream; APersistent: TPersistent);

implementation

uses
  SysUtils;

type
  TdxSaver = class(TComponent)
  private
    FPersistent: TPersistent;
  published
    property Persistent: TPersistent read FPersistent write FPersistent;
  end;

procedure dxSavePersistent(AStream: TStream; APersistent: TPersistent);
var
  Saver: TdxSaver;
begin
  Assert(APersistent <> nil);
  Saver := TdxSaver.Create(nil);
  try
    Saver.Persistent := APersistent;
    AStream.WriteComponent(Saver);
  finally
    Saver.Free;
  end;
end;

procedure dxLoadPersistent(AStream: TStream; APersistent: TPersistent);
var
  Saver: TdxSaver;
begin
  Assert(APersistent <> nil);
  Saver := TdxSaver.Create(nil);
  try
    Saver.Persistent := APersistent;
    AStream.ReadComponent(Saver);
  finally
    Saver.Free;
  end;
end;

{ TdxBaseObject }

constructor TdxBaseObject.Create;
begin
  inherited Create;
end;

procedure TdxBaseObject.Assign(Source: TPersistent);
begin
  if Source is TdxBaseObject then
  begin
    BeginUpdate;
    try
      DoAssign(TdxBaseObject(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TdxBaseObject.BeginUpdate;
begin
  if UpdateCount = 0 then LockUpdate(lsLock);
  Inc(FUpdateCount);
end;

procedure TdxBaseObject.CancelUpdate;
begin
  if FUpdateCount <> 0 then Dec(FUpdateCount);
end;

procedure TdxBaseObject.EndUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Dec(FUpdateCount);
    if UpdateCount = 0 then LockUpdate(lsUnlock);
  end;
end;

function TdxBaseObject.Clone: TdxBaseObject;
begin
  Result := TdxBaseObjectClass(ClassType).Create;
  try
    Result.Assign(Self);
  except
    Result.Free;
    raise;
  end;
end;

function TdxBaseObject.IsEmpty: Boolean;
begin
  Result := False;
end;

function TdxBaseObject.IsEqual(ABaseObject: TdxBaseObject): Boolean;
begin
  Result := ABaseObject is ClassType;
end;

procedure TdxBaseObject.RestoreDefaults;
begin
  BeginUpdate;
  try
    DoRestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TdxBaseObject.SaveToFile(const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

{$WARN SYMBOL_PLATFORM OFF}

procedure TdxBaseObject.LoadFromFile(const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyRead);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

{$WARN SYMBOL_PLATFORM ON}

procedure TdxBaseObject.SaveToStream(AStream: TStream);
begin
  dxSavePersistent(AStream, Self);
end;

procedure TdxBaseObject.LoadFromStream(AStream: TStream);
begin
  dxLoadPersistent(AStream, Self);
end;

procedure TdxBaseObject.DoAssign(Source: TdxBaseObject);
begin
end;

procedure TdxBaseObject.DoRestoreDefaults;
begin
end;

function TdxBaseObject.IsLocked: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

procedure TdxBaseObject.LockUpdate(ALockState: TdxLockState);
begin
  if Assigned(FOnLockUpdate) then FOnLockUpdate(Self, ALockState);
end;

{ TdxClassList }

function TdxClassList.Add(AClass: TClass; ACheckExistence: Boolean = True): Integer;
begin
  if not ACheckExistence or not Find(AClass, Result) then
    Result := inherited Add(TObject(AClass));
end;

function TdxClassList.Find(AClass: TClass; out AnIndex: Integer): Boolean;
begin
  AnIndex := IndexOf(AClass);
  Result := AnIndex <> -1;
end;

function TdxClassList.Find(AClass: TClass): Boolean;
begin
  Result := IndexOf(AClass) <> -1;
end;

function TdxClassList.IndexOf(AClass: TClass): Integer;
begin
  Result := inherited IndexOf(TObject(AClass));
end;

procedure TdxClassList.Insert(Index: Integer; AClass: TClass; ACheckExistence: Boolean = True);
begin
  if not ACheckExistence or not Find(AClass) then
    inherited Insert(Index, TObject(AClass));
end;

function TdxClassList.Remove(AClass: TClass): Integer;
begin
  Result := inherited Remove(TObject(AClass));
end;

function TdxClassList.Extract(AClass: TClass): TClass;
begin
  Result := TClass(inherited Extract(TObject(AClass)));
end;

function TdxClassList.First: TClass;
begin
  Result := TClass(inherited First);
end;

function TdxClassList.Last: TClass;
begin
  Result := TClass(inherited Last);
end;

function TdxClassList.GetItem(Index: Integer): TClass;
begin
  Result := TClass(inherited Items[Index]);
end;

procedure TdxClassList.SetItem(Index: Integer; Value: TClass);
begin
  inherited Items[Index] := TObject(Value);
end;

{ TdxPersistentClassList }

procedure TdxPersistentClassList.Clear;
begin
  UnregisterAll;
  inherited;
end;

function TdxPersistentClassList.Extract(AClass: TPersistentClass): TPersistentClass;
begin
  Result := TPersistentClass(inherited Extract(AClass));
end;

function TdxPersistentClassList.First: TPersistentClass;
begin
  Result := TPersistentClass(inherited First);
end;

function TdxPersistentClassList.Last: TPersistentClass;
begin
  Result := TPersistentClass(inherited Last);
end;

function TdxPersistentClassList.Register(AClass: TPersistentClass): Integer;
begin
  if (AClass <> nil) and not Find(AClass) then
  begin
    Result := Add(AClass);
    Classes.RegisterClass(AClass);
  end
  else
    Result := -1;
end;

procedure TdxPersistentClassList.Register(AnIndex: Integer; AClass: TPersistentClass);
begin
  if (AClass <> nil) and not Find(AClass) then
  begin
    Insert(AnIndex, AClass);
    Classes.RegisterClass(AClass);
  end;
end;

procedure TdxPersistentClassList.Unregister(AClass: TPersistentClass);
var
  Index: Integer;
begin
  if Find(AClass, Index) then
  begin
    Classes.UnregisterClass(AClass);
    Delete(Index);
  end;
end;

procedure TdxPersistentClassList.UnregisterAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Classes.UnregisterClass(Items[I]);
end;

function TdxPersistentClassList.GetItem(Index: Integer): TPersistentClass;
begin
  Result := TPersistentClass(inherited Items[Index]);
end;

procedure TdxPersistentClassList.SetItem(Index: Integer; Value: TPersistentClass);
begin
  inherited Items[Index] := Value;
end;

{ TdxCustomCache }

constructor TdxCustomCache.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TdxCustomCache.Destroy;
begin
  FreeAndNilItems;
  inherited;
end;

procedure TdxCustomCache.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  FItems.Clear;
end;

function TdxCustomCache.Add(AnObject: TObject): Integer;
begin
  Result := FItems.Add(AnObject);
end;

procedure TdxCustomCache.FreeAndNilItems;
begin
  Clear;
  FreeAndNil(FItems);
end;

function TdxCustomCache.IndexOfByClass(AClass: TClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].ClassType = AClass then Exit;
  Result := -1;
end;

function TdxCustomCache.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxCustomCache.GetItem(Index: Integer): TObject;
begin
  Result := FItems[Index];
end;

{ TdxCustomClassFactory }

procedure TdxCustomClassFactory.Register(AClass: TClass);
begin
  FItems.Insert(0, AClass, True);
end;

procedure TdxCustomClassFactory.Unregister(AClass: TClass);
begin
  FItems.Remove(AClass);
end;

procedure TdxCustomClassFactory.UnregisterAll;
begin
  FItems.Clear;
end;

procedure TdxCustomClassFactory.FinalizeInstance;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxCustomClassFactory.InitializeInstance;
begin
  inherited;
  FItems := TdxClassList.Create;
end;

function TdxCustomClassFactory.IndexOf(AClass: TClass): Integer;
begin
  Result := FItems.IndexOf(AClass);
end;

function TdxCustomClassFactory.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxCustomClassFactory.GetItem(Index: Integer): TClass;
begin
  Result := FItems[Index];
end;

{ TdxCustomClassMapItem }

class function TdxCustomClassMapItem.PairClass: TClass;
begin
  Result := nil;
end;

{ TdxCustomClassMaps }

function TdxCustomClassMaps.GetPairClass(AClass: TClass): TdxCustomClassMapItemClass;
var
  Candidate: TdxCustomClassMapItemClass;
  I: Integer;
begin
  Candidate := nil;
  if AClass <> nil then
    for I := Count - 1 downto 0 do
    begin
      Result := Items[I];
      if AClass.InheritsFrom(Result.PairClass) and
        ((Candidate = nil) or Result.PairClass.InheritsFrom(Candidate.PairClass)) then
        Candidate := Result;
      if (Candidate <> nil) and (Candidate.PairClass = AClass) then
        Break;
    end;
  Result := Candidate;
end;

function TdxCustomClassMaps.GetItem(Index: Integer): TdxCustomClassMapItemClass;
begin
  Result := TdxCustomClassMapItemClass(inherited Items[Index]);
end;

end.
