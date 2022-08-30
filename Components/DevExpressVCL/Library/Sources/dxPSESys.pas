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

unit dxPSESys;

interface

{$I cxVer.inc}

uses
  Types, Classes, dxPSSngltn, dxBase;

type
  TdxEventSubscriber = class;

  TdxEventClass = class of TdxEvent;
  TdxEvent = class;

  TdxPSEventSystem = class(TBasedxPSSingleton)
  private
    FEventClasses: TdxClassList;
    FSubscribers: TList;
    function GetEventClass(Index: Integer): TdxEventClass;
    function GetEventCount: Integer;
    function GetSubscriber(Index: Integer): TdxEventSubscriber;
    function GetSubscriberCount: Integer;
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;

    procedure MoveSubscriber(ACurIndex, ANewIndex: Integer);
  public
    class function Instance: TdxPSEventSystem; reintroduce; overload;

    procedure ProcessEvent(var AEvent: TdxEvent);

    function IndexOfEventClass(AEventClass: TdxEventClass): Integer;
    procedure RegisterEventClass(AEventClass: TdxEventClass);
    procedure UnregisterEventClass(AEventClass: TdxEventClass);

    function IndexOfSubscriber(ASubscriber: TdxEventSubscriber): Integer;
    procedure RegisterSubscriber(ASubscriber: TdxEventSubscriber);
    procedure UnregisterSubscriber(ASubscriber: TdxEventSubscriber);

    property EventClasses[Index: Integer]: TdxEventClass read GetEventClass;
    property EventCount: Integer read GetEventCount;
    property SubscriberCount: Integer read GetSubscriberCount;
    property Subscribers[Index: Integer]: TdxEventSubscriber read GetSubscriber;
  end;

  TdxEventSubscriber = class
  private
    FActiveEvent: TdxEvent;
    FEnabled: Boolean;
    FEventClasses: TdxClassList;
    FRegistered: Boolean;
    function GetEventClass(Index: Integer): TdxEventClass;
    function GetEventCount: Integer;
    function GetIndex: Integer;
    procedure SetIndex(Value: Integer);
    procedure SetRegistered(Value: Boolean);

    procedure ProcessEvent(AEvent: TdxEvent);
  protected
    procedure DoProcessEvent; virtual; abstract;
  public
    constructor Create(const AEventClasses: array of TdxEventClass);
    destructor Destroy; override;

    procedure Add(AEventClass: TdxEventClass);
    procedure Remove(AEventClass: TdxEventClass);
    function SupportsEventClass(AEventClass: TdxEventClass): Boolean;

    property ActiveEvent: TdxEvent read FActiveEvent;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property EventClasses[Index: Integer]: TdxEventClass read GetEventClass;
    property EventCount: Integer read GetEventCount;
    property Index: Integer read GetIndex write SetIndex;
    property Registered: Boolean read FRegistered write SetRegistered default True;
  end;

  TdxEvent = class
  private
    FBreak: Boolean;
    FRegistered: Boolean;
    FSender: TObject;
    procedure SetRegistered(Value: Boolean);
  public
    constructor Create(ASender: TObject);
    function EventClass: TdxEventClass;

    property Break: Boolean read FBreak write FBreak;
    property Registered: Boolean read FRegistered write SetRegistered;
    property Sender: TObject read FSender;
  end;

function dxPSEventSystem: TdxPSEventSystem;
procedure dxPSProcessEvent(var AEvent: TdxEvent);

implementation

uses
  SysUtils, Forms, TypInfo;

function dxPSEventSystem: TdxPSEventSystem;
begin
  Result := TdxPSEventSystem.Instance;
end;

procedure dxPSProcessEvent(var AEvent: TdxEvent);
begin
  dxPSEventSystem.ProcessEvent(AEvent);
end;

{ TdxPSEventSystem }

class function TdxPSEventSystem.Instance: TdxPSEventSystem;
begin
  Result := inherited Instance as TdxPSEventSystem;
end;

procedure TdxPSEventSystem.ProcessEvent(var AEvent: TdxEvent);
var
  I: Integer;
  Subscriber: TdxEventSubscriber;
begin
  if (AEvent <> nil) and AEvent.Registered then
  try
    for I := 0 to SubscriberCount - 1 do
    begin
      Subscriber := Subscribers[I];
      if Subscriber.Enabled and Subscriber.SupportsEventClass(AEvent.EventClass) then
      begin
        try
          Subscriber.ProcessEvent(AEvent);
        except
          Application.HandleException(Self);
        end;
        if AEvent.Break then Break;
      end;
    end;
  finally
    FreeAndNil(AEvent);
  end;
end;

function TdxPSEventSystem.IndexOfEventClass(AEventClass: TdxEventClass): Integer;
begin
  Result := FEventClasses.IndexOf(AEventClass);
end;

procedure TdxPSEventSystem.RegisterEventClass(AEventClass: TdxEventClass);
begin
  if AEventClass <> nil then FEventClasses.Add(AEventClass, True);
end;

procedure TdxPSEventSystem.UnregisterEventClass(AEventClass: TdxEventClass);
begin
  FEventClasses.Remove(AEventClass);
end;

function TdxPSEventSystem.IndexOfSubscriber(ASubscriber: TdxEventSubscriber): Integer;
begin
  Result := FSubscribers.IndexOf(ASubscriber);
end;

procedure TdxPSEventSystem.RegisterSubscriber(ASubscriber: TdxEventSubscriber);
begin
  if (ASubscriber <> nil) and (IndexOfSubscriber(ASubscriber) = -1) then
    FSubscribers.Add(ASubscriber);
end;

procedure TdxPSEventSystem.UnregisterSubscriber(ASubscriber: TdxEventSubscriber);
begin
  FSubscribers.Remove(ASubscriber);
end;

procedure TdxPSEventSystem.InitializeInstance;
begin
  inherited;
  FEventClasses := TdxClassList.Create;
  FSubscribers := TList.Create;
end;

procedure TdxPSEventSystem.FinalizeInstance;
begin
  while EventCount > 0 do
    UnregisterEventClass(EventClasses[EventCount - 1]);
  FreeAndNil(FEventClasses);

  while SubscriberCount > 0 do
    UnregisterSubscriber(Subscribers[SubscriberCount - 1]);
  FreeAndNil(FSubscribers);

  inherited;
end;

function TdxPSEventSystem.GetEventClass(Index: Integer): TdxEventClass;
begin
  Result := TdxEventClass(FEventClasses[Index]);
end;

function TdxPSEventSystem.GetEventCount: Integer;
begin
  Result := FEventClasses.Count;
end;

function TdxPSEventSystem.GetSubscriber(Index: Integer): TdxEventSubscriber;
begin
  Result := TdxEventSubscriber(FSubscribers[Index]);
end;

function TdxPSEventSystem.GetSubscriberCount: Integer;
begin
  Result := FSubscribers.Count;
end;

procedure TdxPSEventSystem.MoveSubscriber(ACurIndex, ANewIndex: Integer);
begin
  FSubscribers.Move(ACurIndex, ANewIndex);
end;

{ TdxEventSubscriber }

constructor TdxEventSubscriber.Create(const AEventClasses: array of TdxEventClass);
var
  I: Integer;
begin
  inherited Create;
  FEnabled := True;
  FEventClasses := TdxClassList.Create;
  for I := Low(AEventClasses) to High(AEventClasses) do
    Add(AEventClasses[I]);
  Registered := True;
end;

destructor TdxEventSubscriber.Destroy;
begin
  Registered := False;
  while EventCount <> 0 do
    Remove(EventClasses[EventCount - 1]);
  FreeAndNil(FEventClasses);
  inherited;
end;

procedure TdxEventSubscriber.SetRegistered(Value: Boolean);
begin
  if FRegistered <> Value then
  begin
    FRegistered := Value;
    if FRegistered then
      dxPSEventSystem.RegisterSubscriber(Self)
    else
      dxPSEventSystem.UnregisterSubscriber(Self);
  end;
end;

function TdxEventSubscriber.GetEventClass(Index: Integer): TdxEventClass;
begin
  Result := TdxEventClass(FEventClasses[Index]);
end;

function TdxEventSubscriber.GetEventCount: Integer;
begin
  Result := FEventClasses.Count;
end;

function TdxEventSubscriber.GetIndex: Integer;
begin
  Result := dxPSEventSystem.IndexOfSubscriber(Self);
end;

procedure TdxEventSubscriber.SetIndex(Value: Integer);
var
  CurIndex: Integer;
begin
  if Registered then
  begin
    if Value < 0 then Value := 0;
    if Value > dxPSEventSystem.SubscriberCount - 1 then
      Value := dxPSEventSystem.SubscriberCount - 1;
    CurIndex := GetIndex;
    if CurIndex <> Value then
      dxPSEventSystem.MoveSubscriber(CurIndex, Value);
  end;
end;

procedure TdxEventSubscriber.Add(AEventClass: TdxEventClass);
begin
  if not SupportsEventClass(AEventClass) then FEventClasses.Add(AEventClass);
end;

procedure TdxEventSubscriber.Remove(AEventClass: TdxEventClass);
begin
   FEventClasses.Remove(AEventClass);
end;

function TdxEventSubscriber.SupportsEventClass(AEventClass: TdxEventClass): Boolean;
begin
  Result := (AEventClass <> nil) and (FEventClasses.IndexOf(AEventClass) <> -1);
end;

procedure TdxEventSubscriber.ProcessEvent(AEvent: TdxEvent);
begin
  FActiveEvent := AEvent;
  try
    DoProcessEvent;
  finally
    FActiveEvent := nil;
  end;
end;

{ TdxEvent }

constructor TdxEvent.Create(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
  SetRegistered(True);
end;

function TdxEvent.EventClass: TdxEventClass;
begin
  Result := TdxEventClass(ClassType);
end;

procedure TdxEvent.SetRegistered(Value: Boolean);
begin
  if FRegistered <> Value then
  begin
    FRegistered := Value;
    if FRegistered then
      dxPSEventSystem.RegisterEventClass(EventClass)
    else
      dxPSEventSystem.UnregisterEventClass(EventClass);
  end;
end;

end.
