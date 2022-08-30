unit PXL.Events;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.TypeDef;

type
  TEventNotifier = class
  private const
    StartingCallbackID = 1;
  public type
    TCallbackMethod = procedure(const Sender: TObject; const EventData, UserData: Pointer) of object;
  private type
    TObserverEntry = record
      CallbackID: Cardinal;
      CallbackMethod: TCallbackMethod;
      UserData: Pointer;
    end;
  private
    Entries: array of TObserverEntry;
    CurrentCallbackID: Cardinal;

    function GetNextCallbackID: Cardinal;

    procedure Remove(const Index: Integer);
    procedure Clear;
    function IndexOf(const CallbackID: Cardinal): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Subscribe(const CallbackMethod: TCallbackMethod; const UserData: Pointer = nil): Cardinal;
    procedure Unsubscribe(var CallbackID: Cardinal);

    procedure Notify(const Sender: TObject = nil; const EventData: Pointer = nil);
  end;

implementation

constructor TEventNotifier.Create;
begin
  inherited;

  Increment_PXL_ClassInstances;
  CurrentCallbackID := StartingCallbackID;
end;

destructor TEventNotifier.Destroy;
begin
  try
    Clear;
  finally
    Decrement_PXL_ClassInstances;
  end;

  inherited;
end;

function TEventNotifier.GetNextCallbackID: Cardinal;
begin
  Result := CurrentCallbackID;

  if CurrentCallbackID <> High(Cardinal) then
    Inc(CurrentCallbackID)
  else
    CurrentCallbackID := StartingCallbackID;
end;

procedure TEventNotifier.Remove(const Index: Integer);
var
  I: Integer;
begin
  if (Index < 0) or (Index >= Length(Entries)) then
    Exit;

  for I := Index to Length(Entries) - 2 do
    Entries[I] := Entries[I + 1];

  SetLength(Entries, Length(Entries) - 1);
end;

procedure TEventNotifier.Clear;
begin
  SetLength(Entries, 0);
  CurrentCallbackID := 0;
end;

function TEventNotifier.IndexOf(const CallbackID: Cardinal): Integer;
var
  Left, Right, Pivot: Integer;
begin
  Left := 0;
  Right := Length(Entries) - 1;

  while Left <= Right do
  begin
    Pivot := (Left + Right) div 2;

    if Entries[Pivot].CallbackID = CallbackID then
      Exit(Pivot);

    if Entries[Pivot].CallbackID > CallbackID then
      Right := Pivot - 1
    else
      Left := Pivot + 1;
  end;

  Result := -1;
end;

function TEventNotifier.Subscribe(const CallbackMethod: TCallbackMethod; const UserData: Pointer): Cardinal;
var
  Index, I: Integer;
  CallbackID: Cardinal;
begin
  CallbackID := GetNextCallbackID;

  if (Length(Entries) < 1) or (Entries[Length(Entries) - 1].CallbackID < CallbackID) then
  begin // Add element to the end of the list (fast).
    Index := Length(Entries);
    SetLength(Entries, Index + 1);
  end
  else
  begin // Add element to the start of the list (slow).
    SetLength(Entries, Length(Entries) + 1);

    for I := Length(Entries) - 1 downto 1 do
      Entries[I] := Entries[I - 1];

    Index := 0;
  end;

  Entries[Index].CallbackID := CallbackID;
  Entries[Index].CallbackMethod := CallbackMethod;
  Entries[Index].UserData := UserData;

  Result := CallbackID;
end;

procedure TEventNotifier.Unsubscribe(var CallbackID: Cardinal);
var
  Index: Integer;
begin
  if CallbackID <> 0 then
  begin
    Index := IndexOf(CallbackID);
    if Index <> -1 then
      Remove(Index);

    CallbackID := 0;
  end;
end;

procedure TEventNotifier.Notify(const Sender: TObject; const EventData: Pointer);
var
  I: Integer;
begin
  for I := 0 to Length(Entries) - 1 do
    if Assigned(Entries[I].CallbackMethod) then
      Entries[I].CallbackMethod(Sender, EventData, Entries[I].UserData);
end;

end.
