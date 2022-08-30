{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2007                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit AdvMAddict;

interface

uses
  AdvMemo, Classes, Dialogs,
  ad3ThesaurusBase, ad3Thesaurus, ad3SpellBase, ad3Spell,ad3StringParser,
  ad3ParserBase,ad3ParseEngine;


type

  TAdvMemoAddictChecker = class(TAdvMemoChecker)
  private
    FAddictSpell: TAddictSpell3;
    FShowDialog: Boolean;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure CheckLine(LineNo: Integer); override;
    procedure CheckWord(LineNo,LinePos: Integer; s: string); override;

    procedure CorrectLine(LineNo: Integer); override;
    procedure CorrectWord(LineNo,LinePos: Integer; var s: string); override;
  published
    property AutoCorrectType;
    property AddictSpell: TAddictSpell3 read FAddictSpell write FAddictSpell;
    property ShowDialog: Boolean read FShowDialog write FShowDialog;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Memo',[TAdvMemoAddictChecker]);
end;

{ TAdvMemoAddictChecker }

procedure TAdvMemoAddictChecker.CheckLine(LineNo: Integer);
var
  FStringParser: TStringParser;
  FParsingEngine : TMainParsingEngine;
  s, Word,Corr, Res:string;
  x,y: integer;

begin
  if not Assigned(Memo) then
    Exit;

  if not Assigned(FAddictSpell) then
    Exit;

  Corr := '';

  s := Memo.Lines[LineNo];

  Res := s;

  FStringParser := TStringParser.Create;
  FParsingEngine := TMainParsingEngine.Create;
  FParsingEngine.Initialize( FStringParser, CheckType_All );

  FStringParser.Initialize(@S);

  Memo.ClearLineErrors(LineNo); 

  Word := FParsingEngine.NextWord;

  while Word <> '' do
  begin
     FStringParser.GetPosition(x,y);
    if not FAddictSpell.WordAcceptable(Word) then
    begin
      Memo.SetError(LineNo, x - Length(Word) - 1, Length(word));
    end;
    FStringParser.GetPosition(x,y);
    Word := FParsingEngine.NextWord;
  end;

  FParsingEngine.Free;
  FStringParser.Free;
end;

procedure TAdvMemoAddictChecker.CheckWord(LineNo, LinePos: Integer;
  s: string);
begin
  if not Assigned(Memo) then
    Exit;

  if not Assigned(FAddictSpell) then
    Exit;

  if not FAddictSpell.WordAcceptable(s) then
  begin
    CheckLine(LineNo);
//    Memo.ClearWordError(LineNo, Memo.CurX - length(s));
//    Memo.SetError(LineNo, Memo.CurX - length(s), Length(s));
//    Memo.Refresh;
  end
  else
    Memo.ClearWordError(LineNo, Memo.CurX - length(s));

end;

procedure TAdvMemoAddictChecker.CorrectLine(LineNo: Integer);
var
  FStringParser: TStringParser;
  FParsingEngine : TMainParsingEngine;
  s, Word,Corr,Repl, Res, FUndoText:string;

begin
  if not Assigned(Memo) then
    Exit;

  if not Assigned(FAddictSpell) then
    Exit;

  Corr := '';

  s := Memo.Lines[LineNo];

  FUndoText := s;

  Res := s;

  if FShowDialog then
  begin
    FAddictSpell.CheckString(s);
    Memo.Lines[LineNo] := s;
    if (s <> FUndoText) then
      AddUndo(LineNo, FUndoText);
    Exit;
  end;

  FStringParser := TStringParser.Create;

  FParsingEngine := TMainParsingEngine.Create;
  FParsingEngine.Initialize( FStringParser, CheckType_All );

  FStringParser.Initialize(@S);

  Word := FParsingEngine.NextWord;

  while Word <> '' do
  begin
    if not FAddictSpell.WordAcceptable(Word) then
    begin
      if FAddictSpell.WordHasCorrection(Word,Repl) then
        Res := ReplaceOnce(Res,Corr,Repl);
      {
      else
        Res := HiLight(Result,Word,'e',False);
      }
    end;
    Word := FParsingEngine.NextWord;
  end;

  FParsingEngine.Free;
  FStringParser.Free;

  if (Res <> Memo.Lines[LineNo]) then
  begin
    Memo.Lines[LineNo] := Res;
    if (Res <> FUndoText) then
      AddUndo(LineNo, FUndoText);
  end;
end;

procedure TAdvMemoAddictChecker.CorrectWord(LineNo, LinePos: Integer;
  var s: string);
var
  Repl: string;
begin
  if not Assigned(Memo) then
    Exit;

  if not Assigned(FAddictSpell) then
    Exit;

  if not FAddictSpell.WordAcceptable(s) then
  begin
    if ShowDialog then
    begin
      FAddictSpell.CheckString(s);
      Exit;
    end;

    if FAddictSpell.WordHasCorrection(s,Repl) then
    begin
      s := Repl;
    end;
  end;

end;

procedure TAdvMemoAddictChecker.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FAddictSpell) then
    FAddictSpell := nil;
end;

end.
