{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{           Copyright (c) 1995-2007 CodeGear            }
{                                                       }
{*******************************************************}

unit HashList;

{$R-,T-,H+,X+}

interface

uses SysUtils, Classes, Windows;

type
  EIniFileException = class(Exception);

  { TStringHash - used internally by TMemIniFile to optimize searches. }

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: Integer;
  end;

  TStringHash = class
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: string): PPHashItem;
    function HashOf(const Key: string): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: string; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: string);
    function Modify(const Key: string; Value: Integer): Boolean;
    function ValueOf(const Key: string): Integer;
  end;

  { THStringList - A TStringList that uses TStringHash to improve the
    speed of Find }
  THStringList = class(TStringList)
  private
    FValueHash: TStringHash;
    FNameHash: TStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    destructor Destroy; override;
    function IndexOf(const s: string): Integer; override;
    function IndexOfName(const Name: string): Integer; override;
  end;

  THStringList2 = class(TStringList)
  private
    FValueHash: TStringHash;
    FValueHashValid: Boolean;
  public
    destructor Destroy; override;
    function IndexOf(const s: string): Integer; override;
    function AddObject(const s: string; AObject: TObject): Integer; override;
    procedure Delete(Index: Integer); override;
  end;

  TGHStringList = class(THStringList)
  private
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure UnLock;
  end;

implementation

//uses M2Share; //RTLConsts;

{ TStringHash }

procedure TStringHash.Add(const Key: string; Value: Integer);
var
  Hash                      : Integer;
  Bucket                    : PHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;

  //MainOutMessageAPI('TStringHash.Add(' + Key + ')');
end;

procedure TStringHash.Clear;
var
  i                         : Integer;
  p, n                      : PHashItem;
begin
  for i := 0 to Length(Buckets) - 1 do begin
    p := Buckets[i];
    while p <> nil do begin
      n := p^.Next;
      Dispose(p);
      p := n;
    end;
    Buckets[i] := nil;
  end;
end;

constructor TStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function CompareStrings(const S1, S2: string): Integer;
//{ // From Fast Code
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Fastcode
 *
 * The Initial Developer of the Original Code is Fastcode
 *
 * Portions created by the Initial Developer are Copyright (C) 2002-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * Charalabos Michael <chmichael@creationpower.com>
 * John O'Harrow <john@elmcrest.demon.co.uk>
 *
 * ***** END LICENSE BLOCK ***** *)
type
  PByte = ^Byte;
  TByteArray = array[0..0] of Byte;
  PByteArray = ^TByteArray;
  PInteger = ^Integer;
var
  LStr1, LStr2, LStr1Char1, LStr2Char1, LLength1, LLength2,
    LCompInd, LLengthDif, LChars1, LChars2: Integer;
begin
  LStr1 := Integer(S1);
  LStr2 := Integer(S2);
  if LStr1 <> LStr2 then begin
    if LStr1 <> 0 then begin
      if LStr2 <> 0 then begin
        LStr1Char1 := PByte(LStr1)^;
        LStr2Char1 := PByte(LStr2)^;
        if LStr1Char1 <> LStr2Char1 then begin
          Result := LStr1Char1 - LStr2Char1;
        end
        else begin
          LLength1 := PInteger(LStr1 - 4)^;
          LLength2 := PInteger(LStr2 - 4)^;
          LLengthDif := LLength1 - LLength2;
          if LLengthDif >= 0 then
            LCompInd := -LLength2
          else
            LCompInd := -LLength1;
          if LCompInd < 0 then begin
            Dec(LStr1, LCompInd);
            Dec(LStr2, LCompInd);
            repeat
              LChars1 := PInteger(@PByteArray(LStr1)[LCompInd])^;
              LChars2 := PInteger(@PByteArray(LStr2)[LCompInd])^;
              if LChars1 <> LChars2 then begin
                if SmallInt(LChars1) <> SmallInt(LChars2) then begin
                  Result := (Byte(LChars1) shl 8) + Byte(LChars1 shr 8)
                    - (Byte(LChars2) shl 8) - Byte(LChars2 shr 8);
                  exit;
                end
                else begin
                  if LCompInd > -3 then
                    break;
                  Result := (LChars1 shr 24) + ((LChars1 shr 8) and $FF00)
                    - (LChars2 shr 24) - ((LChars2 shr 8) and $FF00);
                  exit;
                end;
              end;
              Inc(LCompInd, 4);
            until LCompInd >= 0;
          end;
          Result := LLengthDif;
        end;
      end
      else begin
        Result := PInteger(LStr1 - 4)^;
      end;
    end
    else begin
      Result := LStr1 - PInteger(LStr2 - 4)^;
    end;
  end
  else begin
    Result := 0;
  end;
end;

function TStringHash.Find(const Key: string): PPHashItem;
var
  Hash                      : Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do begin
    //if Result^.Key = Key then
    if CompareStrings(Result^.Key, Key) = 0 then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TStringHash.HashOf(const Key: string): Cardinal;
var
  i                         : Integer;
begin
  Result := 0;
  for i := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(Key[i]);
end;

function TStringHash.Modify(const Key: string; Value: Integer): Boolean;
var
  p                         : PHashItem;
begin
  p := Find(Key)^;
  if p <> nil then begin
    Result := True;
    p^.Value := Value;
  end
  else
    Result := False;
end;

procedure TStringHash.Remove(const Key: string);
var
  p                         : PHashItem;
  Prev                      : PPHashItem;
begin
  Prev := Find(Key);
  p := Prev^;
  if p <> nil then begin
    Prev^ := p^.Next;
    Dispose(p);
    //MainOutMessageAPI('TStringHash.Remove(' + Key + ')');
  end;
end;

function TStringHash.ValueOf(const Key: string): Integer;
var
  p                         : PHashItem;
begin
  p := Find(Key)^;
  if p <> nil then
    Result := p^.Value
  else
    Result := -1;
end;

{ THStringList }

procedure THStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor THStringList.Destroy;
begin
  FValueHash.Free;
  FNameHash.Free;
  inherited Destroy;
end;

function THStringList.IndexOf(const s: string): Integer;
begin
  UpdateValueHash;
  if not CaseSensitive then
    Result := FValueHash.ValueOf(UpperCase(s))
  else
    Result := FValueHash.ValueOf(s);
end;

function THStringList.IndexOfName(const Name: string): Integer;
begin
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(UpperCase(Name))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure THStringList.UpdateNameHash;
var
  i                         : Integer;
  p                         : Integer;
  Key                       : string;
begin
  if FNameHashValid then Exit;

  if FNameHash = nil then
    FNameHash := TStringHash.Create
  else
    FNameHash.Clear;
  for i := 0 to count - 1 do begin
    Key := Get(i);
    p := Pos(NameValueSeparator, Key);
    if p <> 0 then begin
      if not CaseSensitive then
        Key := UpperCase(Copy(Key, 1, p - 1))
      else
        Key := Copy(Key, 1, p - 1);
      FNameHash.Add(Key, i);
    end;
  end;
  FNameHashValid := True;
end;

procedure THStringList.UpdateValueHash;
var
  i                         : Integer;
begin
  if FValueHashValid then Exit;

  if FValueHash = nil then
    FValueHash := TStringHash.Create
  else
    FValueHash.Clear;
  for i := 0 to count - 1 do
    if not CaseSensitive then
      FValueHash.Add(UpperCase(Self[i]), i)
    else
      FValueHash.Add(Self[i], i);
  FValueHashValid := True;
end;

/////////////////////
destructor THStringList2.Destroy;
begin
  if FValueHash <> nil then FValueHash.Free;
  inherited Destroy;
end;

function THStringList2.IndexOf(const s: string): Integer;
begin
  if FValueHash = nil then
    FValueHash := TStringHash.Create;
    
  if not CaseSensitive then
    Result := FValueHash.ValueOf(UpperCase(s))
  else
    Result := FValueHash.ValueOf(s);
end;

function THStringList2.AddObject(const s: string; AObject: TObject): Integer;
begin
  if FValueHash = nil then
    FValueHash := TStringHash.Create;
    
  Result := inherited AddObject(s, AObject);

  if not CaseSensitive then
    FValueHash.Add(UpperCase(s), Result)
  else
    FValueHash.Add(s, Result);

end;

procedure THStringList2.Delete(Index: Integer);
begin
  if FValueHash = nil then
    FValueHash := TStringHash.Create;
    
  if not CaseSensitive then
    FValueHash.Remove(UpperCase(Self[Index]))
  else
    FValueHash.Remove(Self[Index]);
  inherited Delete(Index);
end;

{ TGStringList }

constructor TGHStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TGHStringList.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TGHStringList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TGHStringList.UnLock;
begin
  LeaveCriticalSection(FLock);
end;

end.

