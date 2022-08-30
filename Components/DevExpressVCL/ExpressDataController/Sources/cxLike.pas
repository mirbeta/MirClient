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

unit cxLike;

{$I cxVer.inc}

interface

uses
  SysUtils;

type
  TcxFilterLikeOperator = (floLike, floBeginsWith, floEndsWith, floContains);

function LikeOperatorByPattern(var APatternStr: string; APercent: Char): TcxFilterLikeOperator;
function LikeStr(const AStr, APatternStr: string; APercent, AUnderline: Char; AEscape: Char = #0): Boolean;

implementation

procedure PreparePatternStr(var PatternStr: string; APercent: Char);
var
  I: Integer;
  S: string;
begin
  // delete '%%', because '%%' = '%'
  S := APercent + APercent;
  repeat
    I := Pos(S, PatternStr);
    if I > 0 then
      PatternStr := Copy(PatternStr, 1, I - 1) + APercent + Copy(PatternStr, I + 2, MaxInt);
  until I = 0;
end;

function LikeOperatorByPattern(var APatternStr: string; APercent: Char): TcxFilterLikeOperator;
var
  ABeginFlag, AEndFlag: Boolean;
begin
  Result := floLike;
  PreparePatternStr(APatternStr, APercent);
  if Length(APatternStr) > 1 then
  begin
    ABeginFlag := APatternStr[1] = APercent;
    AEndFlag := APatternStr[Length(APatternStr)] = APercent;
    if ABeginFlag then
    begin
      Delete(APatternStr, 1, 1);
      if AEndFlag then
      begin
        Result := floContains;
        Delete(APatternStr, Length(APatternStr), 1);
      end
      else
        Result := floEndsWith;
    end
    else
      if AEndFlag then
      begin
        Result := floBeginsWith;
        Delete(APatternStr, Length(APatternStr), 1);
      end;
  end;
end;

function Like(AText: PChar; ATextLen: Integer; APattern: PChar; APatternLen: Integer;
  const APercentChar, AUnderlineChar, AEscapeChar: Char): Boolean;
var
  C: Char;
  AEscapeFlag: Boolean;
begin
  AEscapeFlag := False;
  repeat
    Dec(APatternLen);
    if APatternLen < 0 then Break;
    C := APattern^;
    Inc(APattern);
    if (AEscapeChar <> #0) and not AEscapeFlag and (C = AEscapeChar) then
    begin
      AEscapeFlag := True;
      Continue;
    end;
    if not AEscapeFlag and (C = APercentChar) then
    begin
      if APatternLen = 0 then
      begin
        Result := True;
        Exit;
      end;
      while ATextLen > 0 do
      begin
        if Like(AText, ATextLen, APattern, APatternLen, APercentChar, AUnderlineChar, AEscapeChar) then
        begin
          Result := True;
          Exit;
        end;
        Inc(AText);
        Dec(ATextLen);
      end;
      Result := False;
      Exit;
    end;
    Dec(ATextLen);
    if ATextLen < 0 then
    begin
      Result := False;
      Exit;
    end;
    if (AEscapeFlag or (C <> AUnderlineChar)) and (C <> AText^) then
    begin
      Result := False;
      Exit;
    end;
    AEscapeFlag := False;
    Inc(AText);
  until False;
  Result := ATextLen = 0;
end;

function LikeStr(const AStr, APatternStr: string; APercent, AUnderline: Char; AEscape: Char = #0): Boolean;
begin
  Result := Like(PChar(AStr), Length(AStr), PChar(APatternStr),
    Length(APatternStr), APercent, AUnderline, AEscape);
end;

end.
