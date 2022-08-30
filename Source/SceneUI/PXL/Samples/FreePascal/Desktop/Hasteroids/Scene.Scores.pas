unit Scene.Scores;
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

{ Special note: this code was ported multiple times from earliest framework releases predating Asphyre. }

uses
  Classes, PXL.TypeDef;

type
  THighScore = class(TCollectionItem)
  private
    FScore: Integer;
    FPlayer: StdString;
  public
    property Player: StdString read FPlayer write FPlayer;
    property Score: Integer read FScore write FScore;
  end;

  THighScores = class(TCollection)
  private
    function GetItem(const Index: Integer): THighScore;
    procedure SetItem(const Index: Integer; const Value: THighScore);
  public
    constructor Create;

    function Add: THighScore;
    function AddItem(const Item: THighScore; Index: Integer): THighScore;
    function Insert(const Index: Integer): THighScore;
    procedure Exchange(const Item1, Item2: Integer);

    procedure Sort;

    procedure LoadFromFile(const FileName: StdString);
    procedure SaveToFile(const FileName: StdString);

    property Items[const Index: Integer]: THighScore read GetItem write SetItem; default;
  end;

implementation

uses
  SysUtils, PXL.Classes;

constructor THighScores.Create;
begin
  inherited Create(THighScore);
end;

function THighScores.GetItem(const Index: Integer): THighScore;
begin
  Result := THighScore(inherited GetItem(Index));
end;

procedure THighScores.SetItem(const Index: Integer; const Value: THighScore);
begin
  inherited SetItem(Index, Value);
end;

function THighScores.Add: THighScore;
begin
  Result := THighScore(inherited Add);
end;

function THighScores.AddItem(const Item: THighScore; Index: Integer): THighScore;
begin
  if Item = nil then
    Result := THighScore.Create(Self)
  else
    Result := Item;

  if Result <> nil then
  begin
    Result.Collection := Self;

    if Index < 0 then
      Index := Count - 1;

    Result.Index := Index;
  end;
end;

function THighScores.Insert(const Index: Integer): THighScore;
begin
  Result := AddItem(nil, Index);
end;

procedure THighScores.Exchange(const Item1, Item2: Integer);
var
  Aux: THighScore;
begin
  Aux := Items[Item1];
  Items[Item1] := Items[Item2];
  Items[Item2] := Aux;
end;

procedure THighScores.Sort;
var
  I, J: Integer;
begin
  // Simple Bubble-sort
  for J := 0 to Count - 1 do
    for I := 0 to Count - 2 do
      if Items[I].Score < Items[I + 1].Score then
        Items[I].Index := Items[I].Index + 1;
end;

procedure THighScores.LoadFromFile(const FileName: StdString);
var
  Stream: TFileStream;
  ItemCount, I: Integer;
  NewItem: THighScore;
begin
  Clear;

  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      // retrieve item count
      ItemCount := Stream.GetLongInt;

      // retrieve individual items
      for I := 0 to ItemCount - 1 do
      begin
        NewItem := Add;
        NewItem.Player := Stream.GetLongString;
        NewItem.Score := Stream.GetLongInt;
      end;
    finally
      Stream.Free;
    end;
  except
    Exit;
  end;

  Sort;
end;

procedure THighScores.SaveToFile(const FileName: StdString);
var
  Stream: TFileStream;
  I: Integer;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
    try
      // item count
      Stream.PutLongint(Count);

      // individual items
      for I := 0 to Count - 1 do
      begin
        Stream.PutLongString(Items[I].Player);
        Stream.PutLongint(Items[I].Score);
      end;
    finally
      Stream.Free;
    end;
  except
    Exit;
  end;
end;

end.
