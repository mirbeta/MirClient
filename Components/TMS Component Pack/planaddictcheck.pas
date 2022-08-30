{***************************************************************************}
{ TPlanner Checker component for Addict Spell Checker v3.0                  }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3.0,4.0,5.0,6.0                   }
{ version 1.1 - rel. July 2002                                              }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2002                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit PlanAddictCheck;

interface

uses
  PlanCheck, Classes, PlanUtil,
  ad3ThesaurusBase, ad3Thesaurus, ad3SpellBase, ad3Spell,ad3StringParser,
  ad3ParserBase,ad3ParseEngine;

type
   TAddictPlannerCheck = class(TPlannerCheck)
   private
     FAddictSpell: TAddictSpell3;
     FShowDialog: Boolean;
   protected
     procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
   public
     function MarkError(s:string):string; override;
     function Correct(s:string):string; override;
     procedure StartCheck; override;
     procedure StopCheck; override;
   published
     property AddictSpell: TAddictSpell3 read FAddictSpell write FAddictSpell;
     property ShowDialog: Boolean read FShowDialog write FShowDialog;
   end;

implementation

{ TAddictPlannerCheck }

function TAddictPlannerCheck.Correct(s: string): string;
var
  FStringParser: TStringParser;
  FParsingEngine : TMainParsingEngine;
  Word,Corr,Repl:string;

begin
  Result := s;
  Corr := '';

  if not Assigned(FAddictSpell) then Exit;

  if FShowDialog then
  begin
    FAddictSpell.CheckString(s);
    Result := s;
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
        Result := ReplaceOnce(Result,Corr,Repl)
      else
        Result := HiLight(Result,Word,'e',False);
    end;
    Word := FParsingEngine.NextWord;
  end;

  FParsingEngine.Free;
  FStringParser.Free;
end;

function TAddictPlannerCheck.MarkError(s: string): string;
var
  FStringParser: TStringParser;
  FParsingEngine : TMainParsingEngine;
  Word,Corr:string;

begin
  Result := s;
  Corr := s;

  if not Assigned(FAddictSpell) then Exit;

  FStringParser := TStringParser.Create;

  FParsingEngine := TMainParsingEngine.Create;
  FParsingEngine.Initialize( FStringParser, CheckType_All );

  FStringParser.Initialize(@S);

  Word := FParsingEngine.NextWord;
  while Word <> '' do
  begin
    if not FAddictSpell.WordAcceptable(Word) then
       Corr := HiLight(Corr,Word,'e',False);
    Word := FParsingEngine.NextWord;
  end;

  FParsingEngine.Free;
  FStringParser.Free;

  Result := Corr;
end;

procedure TAddictPlannerCheck.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FAddictSpell) then
    FAddictSpell := nil;
  inherited;
end;

procedure TAddictPlannerCheck.StartCheck;
begin
  if Assigned(FAddictSpell) then
    FAddictSpell.StartSequenceCheck;
end;

procedure TAddictPlannerCheck.StopCheck;
begin
  if Assigned(FAddictSpell) then
    FAddictSpell.StopSequenceCheck;
end;

end.
