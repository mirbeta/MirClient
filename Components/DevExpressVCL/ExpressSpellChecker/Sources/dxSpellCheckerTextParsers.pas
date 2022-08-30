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

unit dxSpellCheckerTextParsers;

{$I cxVer.inc}

interface

uses
  dxCore, dxSpellCheckerCore;

type

  { IdxSpellCheckerMeaningfulCharacters }

  IdxSpellCheckerMeaningfulCharacters = interface
  ['{622B550A-B8D1-4568-A14C-DEA1F0A5098F}']
    procedure UpdateMeaningfulCharacters(const AValue: string);
  end;

  { TdxSpellCheckerTextController }

  TdxSpellCheckerTextController = class(TdxSpellCheckCustomSimpleTextController,
    IdxSpellCheckerMeaningfulCharacters)
  strict private
    FDelimiters: string;
    FMeaningfulCharacters: string;
    FUrlDelimiters: string;

    function InternalGetNextWord(var AStart, ALength, APos: Integer; var ADelimiters: string): string;
  protected
    function ContainsOnlyDelimiters(const AText: string): Boolean;
    function DoGetNextWord(var AStart, ALength: Integer; var ADelimiters: string): Boolean;
    function GetWordNextPosition(APosition, ADirection: Integer): Integer;
    function IsDelimiter(AChar: WideChar): Boolean;
    function IsMeaningfulCharacters(AChar: WideChar): Boolean;
    function IsSentenceDelimiters(ACh: WideChar): Boolean;
    function IsUrlDelimiter(AChar: WideChar): Boolean;
    procedure SkipFirstDelimiters(var AStart: Integer; var ADelimiters: string);
  public
    constructor Create(const AText: string);
    function GetNextWord(var AStart, AFinish: IdxSpellCheckerPosition): Boolean; override;
    function GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function GetWordFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;
    function GetWordStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition; override;

    // IdxSpellCheckerMeaningfulCharacters
    procedure UpdateMeaningfulCharacters(const AValue: string);

    property Delimiters: string read FDelimiters;
    property MeaningfulCharacters: string read FMeaningfulCharacters;
    property UrlDelimiters: string read FUrlDelimiters;
  end;

implementation

uses
  Character, dxCharacters, dxSpellCheckerUtils, Math;

{ TdxSpellCheckerTextController }

constructor TdxSpellCheckerTextController.Create(const AText: string);
begin
  inherited Create(AText);
  FUrlDelimiters := #0#9#10#11#13#32',<>!;()[]{}|';
  FDelimiters    := #0#9#10#11#13#32'.,<>=!?:;"()[]{}+|-/\'#$201C#$201D;
end;

function TdxSpellCheckerTextController.GetNextWord(var AStart, AFinish: IdxSpellCheckerPosition): Boolean;
var
  ADelimiters: string;
  AIndex, ACount: Integer;
begin
  Result := DecodePositions(AStart, AFinish, AIndex, ACount) and
    InRange(AIndex, FSpellingAreaStart, FSpellingAreaFinish) and
    DoGetNextWord(AIndex, ACount, ADelimiters);
  if Result then
  begin
    AStart := EncodePosition(AIndex);
    AFinish := EncodePosition(AIndex + ACount);
  end;
end;

function TdxSpellCheckerTextController.GetSentenceFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  AEndPos, ALength: Integer;
begin
  AEndPos := DecodePosition(APos);
  ALength := Length(FText);
  while (AEndPos < ALength) and not IsSentenceDelimiters(Text[AEndPos]) do
    Inc(AEndPos);
  Result := EncodePosition(AEndPos + 1);
end;

function TdxSpellCheckerTextController.GetSentenceStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
var
  AStartPos: Integer;
begin
  AStartPos := DecodePosition(APos);
  while (AStartPos > 1) and not IsSentenceDelimiters(Text[AStartPos]) do
    Dec(AStartPos);
  while (AStartPos > 1) and (IsSentenceDelimiters(Text[AStartPos]) or (Text[AStartPos] = ' ')) do
    Inc(AStartPos);
  Result := EncodePosition(AStartPos);
end;

function TdxSpellCheckerTextController.GetWordStartPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  Result := EncodePosition(GetWordNextPosition(DecodePosition(APos), -1));
end;

procedure TdxSpellCheckerTextController.UpdateMeaningfulCharacters(const AValue: string);
begin
  FMeaningfulCharacters := AValue;
end;

function TdxSpellCheckerTextController.ContainsOnlyDelimiters(const AText: string): Boolean;
var
  AChar: PChar;
begin
  AChar := PChar(AText);
  while AChar^ <> #0 do
  begin
    if not IsDelimiter(AChar^) then
      Exit(False);
    Inc(AChar);
  end;
  Result := True;
end;

function TdxSpellCheckerTextController.DoGetNextWord(var AStart, ALength: Integer; var ADelimiters: string): Boolean;
var
  APos: Integer;
  AWord: string;
begin
  ADelimiters := '';
  ALength := 0;
  repeat
    SkipFirstDelimiters(AStart, ADelimiters);
    APos := AStart;
    AWord := InternalGetNextWord(AStart, ALength, APos, ADelimiters);
    if APos >= FSpellingAreaFinish then
      Break;
    if ((Length(ADelimiters) = 0) or TdxCharacters.IsWhiteSpace(ADelimiters[Length(ADelimiters)])) and (IsUrl(AWord) or IsMail(AWord)) then
      Break;
    if ContainsOnlyDelimiters(AWord) then
    begin
      ADelimiters := ADelimiters + AWord;
      Inc(AStart, ALength);
      Continue;
    end;
    if ALength > 0 then
      Break;
  until False;
  Result := ALength > 0;
end;

function TdxSpellCheckerTextController.IsMeaningfulCharacters(AChar: WideChar): Boolean;
begin
  Result := WideCharPos(AChar, FMeaningfulCharacters) > 0;
end;

function TdxSpellCheckerTextController.IsSentenceDelimiters(ACh: WideChar): Boolean;
begin
  Result := (ACh = '.') or TdxCharacters.IsWhiteSpace(ACh) and (ACh <> ' ');
end;

function TdxSpellCheckerTextController.IsUrlDelimiter(AChar: WideChar): Boolean;
begin
  Result := WideCharPos(AChar, FUrlDelimiters) > 0;
end;

function TdxSpellCheckerTextController.GetWordFinishPosition(const APos: IdxSpellCheckerPosition): IdxSpellCheckerPosition;
begin
  Result := EncodePosition(GetWordNextPosition(DecodePosition(APos), 1));
end;

function TdxSpellCheckerTextController.GetWordNextPosition(APosition, ADirection: Integer): Integer;
begin
  Result := APosition;
  while InRange(Result + ADirection, FSpellingAreaStart, FSpellingAreaFinish) and not TdxCharacters.IsWhiteSpace(Text[Result]) do
    Inc(Result, ADirection);
end;

function TdxSpellCheckerTextController.IsDelimiter(AChar: WideChar): Boolean;
begin
  Result := WideCharPos(AChar, FDelimiters) > 0;
end;

procedure TdxSpellCheckerTextController.SkipFirstDelimiters(var AStart: Integer; var ADelimiters: string);
begin
  if (AStart <= FSpellingAreaFinish) and TdxCharacters.IsWhiteSpace(FText[AStart]) then
  begin
    while (AStart < FSpellingAreaFinish) and TdxCharacters.IsWhiteSpace(FText[AStart]) do
    begin
      ADelimiters := ADelimiters + FText[AStart];
      Inc(AStart);
    end;
  end
  else
    while (AStart <= FSpellingAreaFinish) and not (WideIsAlphaNumeric(FText[AStart]) or IsMeaningfulCharacters(FText[AStart])) do
    begin
      ADelimiters := ADelimiters + FText[AStart];
      Inc(AStart);
    end;
end;

function TdxSpellCheckerTextController.InternalGetNextWord(var AStart, ALength, APos: Integer; var ADelimiters: string): string;
begin
  ALength := 0;
  while (APos <= FSpellingAreaFinish) and not TdxCharacters.IsWhiteSpace(FText[APos]) do
    Inc(APos);
  ALength := APos - AStart;
  if ALength = 0 then
    Result := ''
  else
  begin
    while (AStart <= FSpellingAreaFinish) and (IsDelimiter(FText[AStart]) and ((ALength = 1) or
      (((AStart < APos) and not (WideIsAlphaNumeric(FText[AStart + 1]) or IsMeaningfulCharacters(FText[AStart + 1]))) or
        not IsMeaningfulCharacters(FText[AStart])))) do
    begin
      ADelimiters := ADelimiters + FText[AStart];
      Inc(AStart);
      Dec(ALength);
    end;
    while (APos >= AStart) and (IsDelimiter(FText[APos]) and not IsMeaningfulCharacters(FText[APos])) do
      Dec(APos);
    Result := Copy(FText, AStart, APos - AStart + 1);
    if IsUrl(Result) or IsMail(Result) then
    begin
      while (APos >= AStart) and IsUrlDelimiter(FText[APos]) do
        Dec(APos);
      ALength := APos - AStart + 1;
      Result := Copy(FText, AStart, ALength);
    end
    else
    begin
      if ContainsOnlyDelimiters(Result) then
        Exit;
      APos := AStart;
      while (APos <= AStart + ALength) and not (IsDelimiter(FText[APos]) and not IsMeaningfulCharacters(FText[APos])) do
        Inc(APos);
      ALength := APos - AStart;
      Result := Copy(FText, AStart, ALength);
    end;
  end;
end;

end.
