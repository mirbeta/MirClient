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
{   SECRETS OF DEVELOPER EXPRESS INC. REGISTERED DEVELOPER IS        }
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

unit dxPSSngltn;

interface

{$I cxVer.inc}

uses
  Types, Windows, SysUtils, dxCore, dxHooks;

type
  EdxPSSingleton = class(EdxException);

  TBasedxPSSingletonClass = class of TBasedxPSSingleton;

  TBasedxPSSingleton = class
  private
    class function GetInstance(AccessCode: Integer): TBasedxPSSingleton; virtual;
    class function IndexOf: Integer;
    procedure Register; virtual;
    procedure Unregister; virtual;
  protected
    constructor CreateInstance(Dummy: Integer);
    procedure FinalizeInstance; virtual;
    procedure InitializeInstance; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function Instance: TBasedxPSSingleton; overload; virtual;
    class procedure ReleaseInstance; virtual;
  end;

implementation

uses
  Classes;

const
  SingletonAccess = 0;
  SingletonCreate = 1;
  SingletonRelease = 2;

const
  rsdxSingletonAccessOnlyThroughInstance = 'Access class %s through Instance only';
  rsdxSingletonIllegalAccessCode = 'Illegal AccessCode %d in GetInstance';

type
  TdxPSSingletonFactory = class (TBasedxPSSingleton)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TBasedxPSSingleton;
    function GetItemClass(Index: Integer): TBasedxPSSingletonClass;

    class function GetInstance(AccessCode: Integer): TBasedxPSSingleton; override;
    procedure Register; override;
    procedure Unregister; override;
  protected
    procedure FinalizeInstance; override;
    procedure RegisterSingleton(ASingleton: TBasedxPSSingleton);
    procedure UnregisterSingleton(ASingleton: TBasedxPSSingleton);
  public
    function IndexOf(ASingletonClass: TBasedxPSSingletonClass): Integer;
    class function Instance: TdxPSSingletonFactory; reintroduce; overload;
    property Count: Integer read GetCount;
    property ItemClasses[Index: Integer]: TBasedxPSSingletonClass read GetItemClass;
    property Items[Index: Integer]: TBasedxPSSingleton read GetItem; default;
  end;

function SingletonFactory: TdxPSSingletonFactory;
begin
  Result := TdxPSSingletonFactory.Instance;
end;

function HasSingletonFactory: Boolean;
begin
  Result := TdxPSSingletonFactory.GetInstance(SingletonAccess) <> nil;
end;

procedure SingletonError(const message: string);
begin
  raise EdxPSSingleton.Create(message);
end;

{ TdxPSSingletonFactory }

function TdxPSSingletonFactory.IndexOf(ASingletonClass: TBasedxPSSingletonClass): Integer;
begin
  for Result := 0 to Count - 1 do
    if ItemClasses[Result] = ASingletonClass then Exit;
  Result := -1;
end;

class function TdxPSSingletonFactory.Instance: TdxPSSingletonFactory;
begin
  Result := inherited Instance as TdxPSSingletonFactory;
end;

procedure TdxPSSingletonFactory.FinalizeInstance;
begin
  while Count <> 0 do Items[Count - 1].ReleaseInstance;
  inherited FinalizeInstance;
end;

procedure TdxPSSingletonFactory.RegisterSingleton(ASingleton: TBasedxPSSingleton);
begin
  if FItems = nil then FItems := TList.Create;
  FItems.Add(ASingleton);
end;

procedure TdxPSSingletonFactory.UnregisterSingleton(ASingleton: TBasedxPSSingleton);
begin
  FItems.Remove(ASingleton);
  if Count = 0 then
  begin
    FItems.Free;
    FItems := nil;
  end;
end;

function TdxPSSingletonFactory.GetCount: Integer;
begin
  if FItems <> nil then
    Result := FItems.Count
  else
    Result := 0;
end;

class function TdxPSSingletonFactory.GetInstance(AccessCode: Integer): TBasedxPSSingleton;
const
  FInstance: TdxPSSingletonFactory = nil;
begin
  case AccessCode of
    SingletonAccess:
      ;
    SingletonCreate:
      if FInstance = nil then FInstance := CreateInstance(0);
    SingletonRelease:
      FInstance := nil;
  else
    SingletonError(Format(rsdxSingletonIllegalAccessCode, [AccessCode]));
  end;
  Result := FInstance;
end;

function TdxPSSingletonFactory.GetItem(Index: Integer): TBasedxPSSingleton;
begin
  Result := TBasedxPSSingleton(FItems[Index]);
end;

function TdxPSSingletonFactory.GetItemClass(Index: Integer): TBasedxPSSingletonClass;
begin
  Result := TBasedxPSSingletonClass(Items[Index].ClassType);
end;

procedure TdxPSSingletonFactory.Register;
begin
end;

procedure TdxPSSingletonFactory.Unregister;
begin
  if GetInstance(SingletonAccess) = Self then GetInstance(SingletonRelease);
end;

{ TBasedxPSSingleton }

constructor TBasedxPSSingleton.Create;
begin
  inherited Create;
  SingletonError(Format(rsdxSingletonAccessOnlyThroughInstance, [ClassName]));
end;

destructor TBasedxPSSingleton.Destroy;
begin
  FinalizeInstance;
  Unregister;
  inherited;
end;

class function TBasedxPSSingleton.Instance: TBasedxPSSingleton;
begin
  Result := GetInstance(SingletonCreate);
end;

class procedure TBasedxPSSingleton.ReleaseInstance;
begin
  GetInstance(SingletonAccess).Free;
end;

constructor TBasedxPSSingleton.CreateInstance(Dummy: Integer);
begin
  inherited Create;
  Register;
  InitializeInstance;
end;

procedure TBasedxPSSingleton.FinalizeInstance;
begin
end;

procedure TBasedxPSSingleton.InitializeInstance;
begin
end;

class function TBasedxPSSingleton.GetInstance(AccessCode: Integer): TBasedxPSSingleton;
var
  Index: Integer;
begin
  case AccessCode of
    SingletonAccess:
      ;
    SingletonCreate:
      if IndexOf = -1 then CreateInstance(0);
  else
    SingletonError(Format(rsdxSingletonIllegalAccessCode, [AccessCode]));
  end;

  {Note:
   Just imagine: we have some descendant of TBasedxPSSingleton that had been initialized
   and finalized inside another package. This Singleton MUST be finalized manually.
   One trick: If this Singleton had never ever been initialized before
   (i.e. GetInstance(SingletonCreate) had never been called) and others Singletons
   dont exist then SingletonFactory had never been initialized in one's part.
   This means we MUST return nil here silently }

  Result := nil;
  if HasSingletonFactory then
  begin
    Index := IndexOf;
    if Index <> -1 then
      Result := SingletonFactory[Index];
  end;
end;

class function TBasedxPSSingleton.IndexOf: Integer;
begin
  Result := SingletonFactory.IndexOf(Self);
end;

procedure TBasedxPSSingleton.Register;
begin
  SingletonFactory.RegisterSingleton(Self);
end;

procedure TBasedxPSSingleton.Unregister;
begin
  SingletonFactory.UnregisterSingleton(Self);
end;

initialization

finalization
  TdxPSSingletonFactory.ReleaseInstance;

end.
