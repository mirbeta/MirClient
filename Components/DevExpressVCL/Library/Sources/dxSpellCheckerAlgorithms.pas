{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpellChecker                                      }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPELLCHECKER AND ALL           }
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

unit dxSpellCheckerAlgorithms;

{$I cxVer.inc}

interface

const
  MaxSoundLen = 4;

type
  { TdxDoubleMetaphone }

  TdxDoubleMetaphone = class
  private
    FIsSlavoGermanic: Boolean;
    FIsSlavoGermanicReady: Boolean;
    FLast: PWideChar;
    FLength: Integer;
    FPrimary: PWideChar;
    FPrimaryLen: Integer;
    FAlternate: PWideChar;
    FAlternateLen: Integer;
    FValue: PWideChar;

    function GetAlternate: string;
    function GetAlternateKey: Word;
    function GetPrimary: string;
    function GetPrimaryKey: Word;
    procedure MetaphPrimaryAdd(AChar: WideChar); inline;
    procedure MetaphAlternateAdd(AChar: WideChar); inline;
    procedure ProcessB; inline;
    procedure ProcessC; inline;
    procedure ProcessD; inline;
    procedure ProcessF; inline;
    procedure ProcessG; inline;
    procedure ProcessH; inline;
    procedure ProcessJ; inline;
    procedure ProcessK; inline;
    procedure ProcessL; inline;
    procedure ProcessM; inline;
    procedure ProcessN; inline;
    procedure ProcessP; inline;
    procedure ProcessQ; inline;
    procedure ProcessR; inline;
    procedure ProcessS; inline;
    procedure ProcessT; inline;
    procedure ProcessV; inline;
    procedure ProcessW; inline;
    procedure ProcessX; inline;
    procedure ProcessZ; inline;
    procedure Reset(P: PWideChar; ALength: Integer);
  protected
    Current: PWideChar;

    function AtEnd: Boolean; inline;
    function AtStart: Boolean; inline;
    function Contains(AStart: PWideChar; ALength: Integer; APatterns: PWideChar): Boolean; inline;
    function CurrentIndex: Integer; inline;
    function IsSlavoGermanic: Boolean;
    function IsVowel(P: PWideChar): Boolean; inline;
    procedure MetaphAdd(APrimary: WideChar; AAlternate: WideChar = #0); inline;

    property Last: PWideChar read FLast;
    property Length: Integer read FLength;
    property Value: PWideChar read FValue write FValue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoubleMetaphone(P: PWideChar; ALength: Integer);

    property Alternate: string read GetAlternate;
    property AlternateKey: Word read GetAlternateKey;
    property Primary: string read GetPrimary;
    property PrimaryKey: Word read GetPrimaryKey;
  end;

  { TdxStringSimilarityCalculator }

  TdxStringSimilarityCalculator = class
  protected
    function DeleteCost(S: PWideChar): Integer; inline;
    function InsertCost(S: PWideChar): Integer; inline;
    function SubstitutionCost(S1: PWideChar; S2: PWideChar): Integer; inline;
  public
    function GetDistance(AStr1: PWideChar; ALen1: Integer; AStr2: PWideChar; ALen2: Integer): Integer;
    function GetLongestCommonSubsequenceLength(S1, S2: PWideChar): Integer;
  end;

implementation

uses
  SysUtils, dxSpellCheckerUtils, Windows;

function SameChars(P1, P2: PWideChar; ALength: Integer): Boolean; inline;
begin
  repeat
    if P1^ <> P2^ then
    begin
      Result := False;
      Exit;
    end;
    Inc(P1);
    Inc(P2);
    Dec(ALength);
  until ALength = 0;
  Result := True;
end;

function CharInString(AChar: WideChar; AString: PWideChar): Boolean; inline;
begin
  repeat
    if AString^ = AChar then
    begin
      Result := True;
      Exit;
    end;
    Inc(AString);
  until AString^ = #0;
  Result := False;
end;

function ContainsSubStr(const AString, ASubString: PWideChar): Boolean;
var
  P, AStr, ASubStr: PWideChar;
  AChar: WideChar;
begin
  P := AString;
  AChar := ASubString^;
  repeat
    if P^ = AChar then
    begin
      AStr := P;
      ASubStr := ASubString;
      repeat
        Inc(AStr);
        Inc(ASubStr);
        if ASubStr^ = #0 then
        begin
          Result := True;
          Exit;
        end;
        if AStr^ = #0 then
        begin
          Result := False;
          Exit;
        end;
        if AStr^ <> ASubStr^ then
          Break;
      until False;
    end;
    Inc(P);
  until (P^ = #0);
  Result := False;
end;

function GetMetaphoneKey(P: PWideChar; ALength: Integer): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALength - 1 do
    case (P + I)^ of
      'S': Result := Result or (1 shl (4 * I));
      'A': Result := Result or (2 shl (4 * I));
      'P': Result := Result or (3 shl (4 * I));
      'K': Result := Result or (4 shl (4 * I));
      'X': Result := Result or (5 shl (4 * I));
      'J': Result := Result or (6 shl (4 * I));
      'T': Result := Result or (7 shl (4 * I));
      'F': Result := Result or (8 shl (4 * I));
      'N': Result := Result or (9 shl (4 * I));
      'L': Result := Result or (10 shl (4 * I));
      'H': Result := Result or (11 shl (4 * I));
      ' ': Result := Result or (12 shl (4 * I));
      'M': Result := Result or (13 shl (4 * I));
      'R': Result := Result or (14 shl (4 * I));
      '0': Result := Result or (15 shl (4 * I));
    else
      Result := 0;
      Break;
    end;
end;

function Minimum(A, B, C: Integer): Integer; inline;
begin
  Result := A;
  if B < A then
    Result := B;
  if C < Result then
    Result := C;
end;

{ TdxDoubleMetaphone }

constructor TdxDoubleMetaphone.Create;
begin
  inherited Create;
  GetMem(FPrimary, (MaxSoundLen + 1) * SizeOf(WideChar));
  GetMem(FAlternate, (MaxSoundLen + 1) * SizeOf(WideChar));
  GetMem(FValue, 256 * SizeOf(WideChar));
end;

destructor TdxDoubleMetaphone.Destroy;
begin
  FreeMem(FPrimary);
  FreeMem(FAlternate);
  FreeMem(FValue);
  inherited Destroy;
end;

procedure TdxDoubleMetaphone.DoubleMetaphone(P: PWideChar; ALength: Integer);
begin
  //pad the original string so that we can index beyond the edge of the world
  if ALength = 0 then Exit;
  Reset(P, ALength);

  //skip these when at start of word
  if Contains(Value, 2, 'GN;KN;PN;WR;PS;') then
    Inc(Current)
  else
  begin
    case Value^ of
      'A', 'E', 'I', 'O', 'U', 'Y':
        begin
          //all init vowels now map to 'A'
          MetaphAdd('A');
          Inc(Current);
        end;
      'X': //Initial 'X' is pronounced 'Z' e.g. 'Xavier'
        begin
          MetaphAdd('S'); //'Z' maps to 'S'
          Inc(Current);
        end;
      'W':
        begin
          if IsVowel(Value + 1) then //Wasserman should match Vasserman
          begin
            MetaphAdd('A', 'F');
            Inc(Current);
          end
          else
            if Contains(Current, 2, 'WH;')  then
            begin
              //need Uomo to match Womo
              MetaphAdd('A');
              Inc(Current);
            end;
        end;
    end;
  end;

  // main loop
  while ((FPrimaryLen < MaxSoundLen) or (FAlternateLen < MaxSoundLen)) and (Current <= Last) do
  begin
    case Current^ of
      'B': ProcessB;
      'C': ProcessC;
      'D': ProcessD;
      'F': ProcessF;
      'G': ProcessG;
      'H': ProcessH;
      'J': ProcessJ;
      'K': ProcessK;
      'L': ProcessL;
      'M': ProcessM;
      'N': ProcessN;
      'P': ProcessP;
      'Q': ProcessQ;
      'R': ProcessR;
      'S': ProcessS;
      'T': ProcessT;
      'V': ProcessV;
      'W': ProcessW;
      'X': ProcessX;
      'Z': ProcessZ;
      //special case
      #$00C7: // A C with a Cedilla
        MetaphAdd('S');
      #$00D1: // N with a tilde (spanish ene)
        MetaphAdd('N');
    end;
    Inc(Current);
  end;
end;

function TdxDoubleMetaphone.IsSlavoGermanic: Boolean;
begin
  if not FIsSlavoGermanicReady then
  begin
    FIsSlavoGermanic := ContainsSubStr(Value, 'W') or ContainsSubStr(Value, 'K') or
      ContainsSubStr(Value, 'CZ') or ContainsSubStr(Value, 'WITZ');
    FIsSlavoGermanicReady := True;
  end;
  Result := FIsSlavoGermanic;
end;

function TdxDoubleMetaphone.IsVowel(P: PWideChar): Boolean;
begin
  if (P >= Value) and (P <= Last) then
    case P^ of
      'A', 'E', 'I', 'O', 'U', 'Y': Result := True;
    else
      Result := False;
    end
  else
    Result := False;
end;

procedure TdxDoubleMetaphone.MetaphAdd(APrimary: WideChar; AAlternate: WideChar = #0);
begin
  if APrimary <> #0 then
  begin
    MetaphPrimaryAdd(APrimary);
    if AAlternate = #0 then
      MetaphAlternateAdd(APrimary)
    else
      MetaphAlternateAdd(AAlternate);
  end
  else
    MetaphAlternateAdd(AAlternate);
end;

procedure TdxDoubleMetaphone.MetaphPrimaryAdd(AChar: WideChar);
begin
  if FPrimaryLen < MaxSoundLen then
  begin
    (FPrimary + FPrimaryLen)^ := AChar;
    Inc(FPrimaryLen);
  end;
end;

procedure TdxDoubleMetaphone.MetaphAlternateAdd(AChar: WideChar);
begin
  if FAlternateLen < MaxSoundLen then
  begin
    (FAlternate + FAlternateLen)^ := AChar;
    Inc(FAlternateLen);
  end;
end;

function TdxDoubleMetaphone.AtEnd: Boolean;
begin
  Result := Current = Last;
end;

function TdxDoubleMetaphone.AtStart: Boolean;
begin
  Result := Current = Value;
end;

function TdxDoubleMetaphone.Contains(AStart: PWideChar; ALength: Integer;
  APatterns: PWideChar): Boolean;
begin
  Result := (AStart >= Value) and (AStart + ALength - 1 <= Last);
  if Result then
  begin
    repeat
      if SameChars(AStart, APatterns, ALength) then
      begin
        Result := True;
        Exit;
      end;
      Inc(APatterns, ALength + 1);
    until APatterns^ = #0;
    Result := False;
  end;
end;

function TdxDoubleMetaphone.CurrentIndex: Integer;
begin
  Result := Current - Value;
end;

function TdxDoubleMetaphone.GetAlternate: string;
begin
  SetString(Result, FAlternate, FAlternateLen);
end;

function TdxDoubleMetaphone.GetAlternateKey: Word;
begin
  Result := GetMetaphoneKey(FAlternate, FAlternateLen);
end;

function TdxDoubleMetaphone.GetPrimary: string;
begin
  SetString(Result, FPrimary, FPrimaryLen);
end;

function TdxDoubleMetaphone.GetPrimaryKey: Word;
begin
  Result := GetMetaphoneKey(FPrimary, FPrimaryLen);
end;

procedure TdxDoubleMetaphone.ProcessB;
begin
  //"-mb", e.g", "dumb", already skipped over...
  MetaphAdd('P');
  if (Current + 1)^ = 'B' then
    Inc(Current);
end;

procedure TdxDoubleMetaphone.ProcessC;
begin
  //various germanic
  if (CurrentIndex > 1) and not IsVowel(Current - 2) and Contains(Current - 1, 3, 'ACH;') and
    (((Current + 2)^ <> 'I') and (((Current + 2)^ <> 'E') or Contains(Current - 2, 6, 'BACHER;MACHER;'))) then
  begin
    MetaphAdd('K');
    Inc(Current);
    Exit;
  end;

  //special case 'caesar'
  if AtStart and Contains(Current, 6, 'CAESAR;') then
  begin
    MetaphAdd('S');
    Inc(Current);
    Exit;
  end;

  //italian 'chianti'
  if Contains(Current, 4, 'CHIA;') then
  begin
    MetaphAdd('K');
    Inc(Current);
    Exit;
  end;

  if Contains(Current, 2, 'CH;') then
  begin
    //find 'michael'
    if (Current > Value) and Contains(Current, 4, 'CHAE;') then
    begin
      MetaphAdd('K', 'X');
      Inc(Current);
      Exit;
    end;

    //greek roots e.g. 'chemistry', 'chorus'
    if (AtStart and (Contains(Current + 1, 5, 'HARAC;HARIS;') or
      Contains(Current + 1, 3, 'HOR;HYM;HIA;HEM;')) and not Contains(Current, 5, 'CHORE;')) then
    begin
      MetaphAdd('K');
      Inc(Current);
      Exit;
    end;

    //germanic, greek, or otherwise 'ch' for 'kh' sound
    if (Contains(Current, 4, 'VAN ;VON ;') or Contains(Current, 3, 'SCH;')) or
       // 'architect but not 'arch', 'orchestra', 'orchid'
       Contains(Current - 2, 6, 'ORCHES;ARCHIT;ORCHID;') or
       CharInString((Current + 2)^, 'TS') or
       (AtStart or CharInString((Current - 1)^, 'AOUE')) and
       //e.g., 'wachtler', 'wechsler', but not 'tichner'
       CharInString((Current + 2)^, 'LRNMBHFVW ') then
      MetaphAdd('K')
    else
    begin
      if Current > Value then
      begin
        if Contains(Current, 2, 'MC;') then
          //e.g., 'McHugh'
          MetaphAdd('K')
        else
          MetaphAdd('X', 'K');
      end
      else
        MetaphAdd('X');
    end;
    Inc(Current);
    Exit;
  end;

  //e.g, 'czerny'
  if Contains(Current, 2, 'CZ;') and not Contains(Current - 2, 4, 'WICZ;') then
  begin
    MetaphAdd('S', 'X');
    Inc(Current);
    Exit;
  end;

  //e.g., 'focaccia'
  if Contains(Current + 1, 3, 'CIA;') then
  begin
    MetaphAdd('X');
    Inc(Current, 2);
    Exit;
  end;

  //double 'C', but not if e.g. 'McClellan'
  if Contains(Current, 2, 'CC;') and not ((CurrentIndex = 1) and (Value^ = 'M')) then
  begin
    //'bellocchio' but not 'bacchus'
    if CharInString((Current + 2)^, 'IEH') and not Contains(Current + 2, 2, 'HU;') then
    begin
      //'accident', 'accede' 'succeed'
      if (((CurrentIndex = 1) and ((Current - 1)^ = 'A'))
         or Contains(Current - 1, 5, 'UCCEE;UCCES;')) then
      begin
        MetaphAdd('K');
        MetaphAdd('S');
      end
      else
        //'bacci', 'bertucci', other italian
        MetaphAdd('X');
      Inc(Current, 2);
    end
    else
    begin
      //Pierce's rule
      MetaphAdd('K');
      Inc(Current);
    end;
    Exit;
  end;

  if Contains(Current, 2, 'CK;CG;CQ;') then
  begin
    MetaphAdd('K');
    Inc(Current);
    Exit;
  end;

  if Contains(Current, 2, 'CI;CE;CY;') then
  begin
    //italian vs. english
    if Contains(Current, 3, 'CIO;CIE;CIA;') then
      MetaphAdd('S', 'X')
    else
      MetaphAdd('S');
    Inc(Current);
    Exit;
  end;

  //else
  MetaphAdd('K');

  //name sent in 'mac caffrey', 'mac gregor
  if Contains(Current + 1, 2, ' C; Q; G;') then
    Inc(Current, 2)
  else
    if CharInString((Value + 1)^, 'CKQ') and not Contains(Value + 1, 2, 'CE;CI;') then
      Inc(Current);
end;

procedure TdxDoubleMetaphone.ProcessD;
begin
  if Contains(Current, 2, 'DG;') then
  begin
    if CharInString((Current + 2)^, 'IEY') then
    begin
      //e.g. 'edge'
      MetaphAdd('J');
      Inc(Current, 2);
    end
    else
    begin
      //e.g. 'edgar'
      MetaphAdd('T');
      MetaphAdd('K');
      Inc(Current);
    end;
    Exit;
  end;

  if Contains(Current, 2, 'DT;DD;') then
  begin
    MetaphAdd('T');
    Inc(Current);
  end
  else
    MetaphAdd('T');
end;

procedure TdxDoubleMetaphone.ProcessF;
begin
  if (Current + 1)^ = 'F' then
    Inc(Current);
  MetaphAdd('F');
end;

procedure TdxDoubleMetaphone.ProcessG;
begin
  if (Current + 1)^ = 'H' then
  begin
    if (CurrentIndex > 0) and not IsVowel(Current - 1) then
    begin
      MetaphAdd('K');
      Inc(Current);
      Exit;
    end;

    //'ghislane', ghiradelli
    if Current = Value then
    begin
      if (Current + 2)^ = 'I' then
        MetaphAdd('J')
      else
        MetaphAdd('K');
      Inc(Current);
      Exit;
    end;

    //Parker's rule (with some further refinements) - e.g., 'hugh'
    if (CurrentIndex > 1) and CharInString((Current - 2)^, 'BHD') or
       //e.g., 'bough'
       (CurrentIndex > 2) and CharInString((Current - 3)^, 'BHD') or
       //e.g., 'broughton'
       (CurrentIndex > 3) and CharInString((Current - 4)^, 'BH') then
      Inc(Current)
    else
    begin
      //e.g., 'laugh', 'McLaughlin', 'cough', 'gough', 'rough', 'tough'
      if (CurrentIndex > 2) and ((Current - 1)^ = 'U') and CharInString((Current - 3)^, 'CGLRT') then
        MetaphAdd('F')
      else
        if (CurrentIndex > 0) and ((Current - 1)^ <> 'I') then
          MetaphAdd('K');
      Inc(Current);
    end;
    Exit;
  end;

  if (Current + 1)^ = 'N' then
  begin
    if (CurrentIndex = 1) and IsVowel(Value) and not IsSlavoGermanic then
    begin
      MetaphAdd('K', 'N');
      MetaphAdd('N', #0);
    end
    else
      //not e.g. 'cagney'
      if not Contains(Current + 2, 2, 'EY;') and ((Current + 1)^ <> 'Y') and not IsSlavoGermanic then
      begin
        MetaphAdd('N', 'K');
        MetaphAdd(#0, 'N');
      end
      else
      begin
        MetaphAdd('K');
        MetaphAdd('N');
      end;
    Inc(Current);
    Exit;
  end;

  //'tagliaro'
  if Contains(Current + 1, 2, 'LI;') and not IsSlavoGermanic then
  begin
    MetaphAdd('K', 'L');
    MetaphAdd('L', #0);
    Inc(Current);
    Exit;
  end;

  //-ges-,-gep-,-gel-, -gie- at beginning
  if (AtStart and (((Current + 1)^ = 'Y')
    or Contains(Current + 1, 2, 'ES;EP;EB;EL;EY;IB;IL;IN;IE;EI;ER;'))) then
  begin
    MetaphAdd('K', 'J');
    Inc(Current);
    Exit;
  end;

  // -ger-,  -gy-
  if (Contains(Current + 1, 2, 'ER;') or ((Current + 1)^ = 'Y')) and
     not Contains(Current, 6, 'DANGER;RANGER;MANGER;') and
     not ((CurrentIndex > 0) and ((Current - 1)^ = 'E') or ((Current - 1)^ = 'I')) and
     not Contains(Current - 1, 3, 'RGY;OGY;') then
  begin
    MetaphAdd('K', 'J');
    Inc(Current);
    Exit;
  end;

  // italian e.g, 'biaggi'
  if CharInString((Current + 1)^, 'EIY') or Contains(Current - 1, 4, 'AGGI;OGGI;') then
  begin
    //obvious germanic
    if Contains(Current, 4, 'VAN ;VON ;') or
       Contains(Current, 3, 'SCH;') or Contains(Current + 1, 2, 'ET;') then
      MetaphAdd('K')
    else
      //always soft if french ending
      if Contains(Current + 1, 4, 'IER ;') then
        MetaphAdd('J')
      else
        MetaphAdd('J', 'K');
    Inc(Current);
    Exit;
  end;

  if (Current + 1)^ = 'G' then
    Inc(Current);
  MetaphAdd('K');
end;

procedure TdxDoubleMetaphone.ProcessH;
begin
  //only keep if first & before vowel or btw. 2 vowels
  if (AtStart or IsVowel(Current - 1)) and IsVowel(Current + 1) then
  begin
    MetaphAdd('H');
    Inc(Current);
  end;
end;

procedure TdxDoubleMetaphone.ProcessJ;
begin
  //obvious spanish, 'jose', 'san jacinto'
  if Contains(Current, 4, 'JOSE;') or Contains(Current, 4, 'SAN ;') then
  begin
    if (AtStart and ((Current + 4)^ = ' ')) or Contains(Current, 4, 'SAN ;') then
      MetaphAdd('H')
    else
      MetaphAdd('J', 'H');
    Exit;
  end;

  if AtStart and not Contains(Current, 4, 'JOSE;') then
    MetaphAdd('J', 'A') //Yankelovich/Jankelowicz
  else
    //spanish pron. of e.g. 'bajador'
    if IsVowel(Current - 1) and not IsSlavoGermanic and (((Current + 1)^ = 'A') or ((Current + 1)^ = 'O')) then
      MetaphAdd('J', 'H')
    else
      if AtEnd then
        MetaphAdd('J', ' ')
      else
        if not CharInString((Current + 1)^, 'LTKSNMBZ') and
           not ((CurrentIndex > 0) and CharInString((Current - 1)^, 'SKL')) then
          MetaphAdd('J');

  if (Current + 1)^ = 'J' then //it could happened
    Inc(Current);
end;

procedure TdxDoubleMetaphone.ProcessK;
begin
  if (Current + 1)^ = 'K' then
    Inc(Current);
  MetaphAdd('K');
end;

procedure TdxDoubleMetaphone.ProcessL;
begin
  if (Current + 1)^ = 'L' then
  begin
    //spanish e.g. 'cabrillo', 'gallegos'
    if ((Current = Last - 2) and Contains(Current - 1, 4, 'ILLO;ILLA;ALLE;')) or
       ((Contains(Last - 1, 2, 'AS;OS;') or (Last^ = 'A') or (Last^ = 'O'))
       and Contains(Current - 1, 4, 'ALLE;')) then
    begin
      MetaphAdd('L', ' ');
      Inc(Current);
      Exit;
    end;
    Inc(Current);
  end;
  MetaphAdd('L');
end;

procedure TdxDoubleMetaphone.ProcessM;
begin
  if Contains(Current - 1, 3, 'UMB;') and
    ((Current + 1 = Last) or Contains(Current + 2, 2, 'ER;'))
      //'dumb','thumb'
      or  ((Current + 1)^ = 'M') then
    Inc(Current);
  MetaphAdd('M');
end;

procedure TdxDoubleMetaphone.ProcessN;
begin
  if (Current + 1)^ = 'N' then
    Inc(Current);
  MetaphAdd('N');
end;

procedure TdxDoubleMetaphone.ProcessP;
begin
  if (Current + 1)^ = 'H' then
  begin
    MetaphAdd('F');
    Inc(Current);
    Exit;
  end;

  //also account for 'campbell', 'raspberry'
  if ((Current + 1)^ = 'P') or ((Current + 1)^ = 'B') then
    Inc(Current);
  MetaphAdd('P');
end;

procedure TdxDoubleMetaphone.ProcessQ;
begin
  if (Current + 1)^ = 'Q' then
    Inc(Current);
  MetaphAdd('K');
end;

procedure TdxDoubleMetaphone.ProcessR;
begin
  //french e.g. 'rogier', but exclude 'hochmeier'
  if AtEnd and not IsSlavoGermanic and Contains(Current - 2, 2, 'IE;')
     and not Contains(Current - 4, 2, 'ME;MA;') then
    MetaphAdd(#0, 'R')
  else
    MetaphAdd('R');

  if (Current + 1)^ = 'R' then
    Inc(Current);
end;

procedure TdxDoubleMetaphone.ProcessS;
begin
  //special cases 'island', 'isle', 'carlisle', 'carlysle'
  if Contains(Current - 1, 3, 'ISL;YSL;') then
    Exit;

  //special case 'sugar-'
  if AtStart and Contains(Current, 5, 'SUGAR;') then
  begin
    MetaphAdd('X', 'S');
    Exit;
  end;

  if Contains(Current, 2, 'SH;') then
  begin
    //germanic
    if Contains(Current + 1, 4, 'HEIM;HOEK;HOLM;HOLZ;') then
      MetaphAdd('S')
    else
      MetaphAdd('X');
    Inc(Current);
    Exit;
  end;

  //italian & armenian
  if Contains(Current, 3, 'SIO;SIA;') or Contains(Current, 4, 'SIAN;') then
  begin
    if not IsSlavoGermanic then
      MetaphAdd('S', 'X')
    else
      MetaphAdd('S');
    Inc(Current, 2);
    Exit;
  end;

  //german & anglicisations, e.g. 'smith' match 'schmidt', 'snider' match 'schneider'
  //also, -sz- in slavic language altho in hungarian it is pronounced 's'
  if (AtStart and CharInString((Current + 1)^, 'MNLW')) or ((Current + 1)^ = 'Z') then
  begin
    MetaphAdd('S', 'X');
    if (Current + 1)^ = 'Z' then
      Inc(Current);
    Exit;
  end;

  if Contains(Current, 2, 'SC;') then
  begin
    //Schlesinger's rule
    if (Current + 2)^ = 'H' then
    begin
      //dutch origin, e.g. 'school', 'schooner'
      if Contains(Current + 3, 2, 'OO;ER;EN;UY;ED;EM;') then
      begin
        //'schermerhorn', 'schenker'
        if Contains(Current + 3, 2, 'ER;EN;') then
        begin
          MetaphAdd('X', 'S');
          MetaphAdd(#0, 'K');
        end
        else
        begin
          MetaphAdd('S');
          MetaphAdd('K');
        end;
      end
      else
      begin
        if AtStart and not IsVowel(Value + 3) and ((Current + 3)^ <> 'W') then
          MetaphAdd('X', 'S')
        else
          MetaphAdd('X');
      end;
      Inc(Current, 2);
      Exit;
    end;

    if CharInString((Current + 2)^, 'IEY') then
      MetaphAdd('S')
    else
    begin
      MetaphAdd('S');
      MetaphAdd('K');
    end;
    Inc(Current, 2);
    Exit;
  end;

  //french e.g. 'resnais', 'artois'
  if AtEnd and Contains(Current - 2, 2, 'AI;OI;') then
    MetaphAdd(#0, 'S')
  else
    MetaphAdd('S');

  if ((Current + 1)^ = 'S') or ((Current + 1)^ = 'Z') then
    Inc(Current);
end;

procedure TdxDoubleMetaphone.ProcessT;
begin
  if Contains(Current, 4, 'TION;') or Contains(Current, 3, 'TIA;TCH;') then
  begin
    MetaphAdd('X');
    Inc(Current, 2);
    Exit;
  end;

  if Contains(Current, 2, 'TH;') or Contains(Current, 3, 'TTH;') then
  begin
    //special case 'thomas', 'thames' or germanic
    if Contains(Current + 2, 2, 'OM;AM;') or Contains(Current, 4, 'VAN ;VON ;')
       or Contains(Current, 3, 'SCH;') then
      MetaphAdd('T')
    else
      MetaphAdd('0', 'T');
    Inc(Current);
    Exit;
  end;

  if ((Current + 1)^ = 'T') or ((Current + 1)^ = 'D') then
    Inc(Current);
  MetaphAdd('T');
end;

procedure TdxDoubleMetaphone.ProcessV;
begin
  if (Current + 1)^ = 'V' then
    Inc(Current);
  MetaphAdd('F');
end;

procedure TdxDoubleMetaphone.ProcessW;
begin
  //can also be in middle of word
  if Contains(Current, 2, 'WR;') then
  begin
    MetaphAdd('R');
    Inc(Current);
    Exit;
  end;

  //Arnow should match Arnoff
  if (AtEnd and IsVowel(Current - 1)) or
    Contains(Current - 1, 5, 'EWSKI;EWSKY;OWSKI;OWSKY;') or
    Contains(Current, 3, 'SCH;') then
  begin
    MetaphAdd(#0, 'F');
    Exit;
  end;

  //polish e.g. 'filipowicz'
  if Contains(Current, 4, 'WICZ;WITZ;') then
  begin
    MetaphAdd('T', 'F');
    MetaphAdd('S', 'X');
    Inc(Current, 3);
  end;
end;

procedure TdxDoubleMetaphone.ProcessX;
begin
  //french e.g. breaux
  if not (AtEnd and (Contains(Current - 3, 3, 'IAU;EAU;')
    or Contains(Current - 2, 2, 'AU;OU;'))) then
  begin
    MetaphAdd('K');
    MetaphAdd('S');
  end;
  if ((Current + 1)^ = 'C') or ((Current + 1)^ = 'X') then
    Inc(Current);
end;

procedure TdxDoubleMetaphone.ProcessZ;
begin
  //chinese pinyin e.g. 'zhao'
  if (Current + 1)^ = 'H' then
  begin
    MetaphAdd('J');
    Inc(Current);
    Exit;
  end
  else
    if Contains(Current + 1, 2, 'ZO;ZI;ZA;')
      or (IsSlavoGermanic and ((CurrentIndex > 0) and ((Current - 1)^ <> 'T'))) then
    begin
      MetaphAdd('S', 'T');
      MetaphAdd(#0, 'S');
    end
    else
      MetaphAdd('S');

  if (Current + 1)^ = 'Z' then
    Inc(Current);
end;

procedure TdxDoubleMetaphone.Reset(P: PWideChar; ALength: Integer);
var
  ATemp: PWideChar;
  I: Integer;
begin
  FLength := ALength;
  ATemp := FValue;
  I := 0;
  while I < (Length + 6) do
  begin
    if I < Length then
      (ATemp + I)^ := (P + I)^
    else
      if I = Length + 5 then
        (ATemp + I)^ := #0
      else
        (ATemp + I)^ := ' ';
    Inc(I);
  end;
  FLast := FValue + ALength - 1;
  FPrimaryLen := 0;
  FAlternateLen := 0;
  Current := FValue;
  FIsSlavoGermanicReady := False;
end;

{ TdxStringSimilarityCalculator }

function TdxStringSimilarityCalculator.GetDistance(AStr1: PWideChar; ALen1: Integer;
  AStr2: PWideChar; ALen2: Integer): Integer;
var
  AMatrix: array of array of Integer;
  I, J: Integer;
begin
  if (ALen1 = 0) or (ALen2 = 0) then
  begin
    Result := 0;
    Exit;
  end;
  SetLength(AMatrix, ALen1 + 1, ALen2 + 1);
  AMatrix[0, 0] := 0;
  for I := 0 to ALen2 - 1 do
    AMatrix[0, I + 1] := AMatrix[0, I] + InsertCost(AStr2 + I);
  for I := 0 to ALen1 - 1 do
  begin
    AMatrix[I + 1, 0] := AMatrix[I, 0] + DeleteCost(AStr1 + I);
    for J := 0 to ALen2 - 1 do
    begin
      AMatrix[I + 1, J + 1] :=  Minimum(
        AMatrix[I, J] + SubstitutionCost(AStr1 + I, AStr2 + J),
        AMatrix[I, J + 1] + DeleteCost(AStr1 + I),
        AMatrix[I + 1, J] + InsertCost(AStr2 + J));
    end;
  end;
  Result := AMatrix[ALen1, ALen2];
  if Result > 0 then
  begin
    if AStr1^ <> AStr2^ then
      Inc(Result);
    if AStr1[ALen1 - 1] <> AStr2[ALen2 - 1] then
      Inc(Result);
  end;
end;

function TdxStringSimilarityCalculator.GetLongestCommonSubsequenceLength(S1, S2: PWideChar): Integer;
type
  TBacktrackingDirection = (bdUp, bdLeft, bdUpLeft);

  TMatrixItem = record
    Weight: Integer;
    Direction: TBacktrackingDirection;
  end;

var
  I, J, M, N: Integer;
  AMatrix: array of array of TMatrixItem;
  P2: PWideChar;
begin
  Result := 0;
  M := StrLen(S1);
  N := StrLen(S2);
  if (M = 0) or (N = 0) then
    Exit;
  SetLength(AMatrix, M + 1, N + 1);
  for I := 1 to M do
  begin
    P2 := S2;
    for J := 1 to N do
      with AMatrix[I, J] do
      begin
        if S1^ = P2^ then
        begin
          Weight := AMatrix[I - 1, J - 1].Weight + 1;
          Direction := bdUpLeft;
        end
        else
          if AMatrix[I - 1, J].Weight >= AMatrix[I, J - 1].Weight then
          begin
            Weight := AMatrix[I - 1, J].Weight;
            Direction := bdUp;
          end
          else
          begin
            Weight := AMatrix[I, J - 1].Weight;
            Direction := bdLeft;
          end;
        Inc(P2);
      end;
    Inc(S1);
  end;
  while (M <> 0) and (N <> 0) do
    case AMatrix[M, N].Direction of
      bdUpLeft:
        begin
          Inc(Result);
          Dec(M);
          Dec(N);
        end;
      bdUp:
        Dec(M);
    else
      Dec(N);
    end
end;

function TdxStringSimilarityCalculator.DeleteCost(S: PWideChar): Integer;
begin
  Result := 2;
end;

function TdxStringSimilarityCalculator.InsertCost(S: PWideChar): Integer;
begin
  Result := 2;
end;

function TdxStringSimilarityCalculator.SubstitutionCost(S1: PWideChar; S2: PWideChar): Integer;
begin
  if S1^ = S2^ then
    Result := 0
  else
    Result := 2;
end;

end.
