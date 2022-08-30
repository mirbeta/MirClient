{***************************************************************************}
{ TAdvStringGrid Spell checker component                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
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

unit AsgSpellCheck;

interface

uses
  AsgCheck, Classes, AdvUtil, AdvGrid, TMSSpellCheck, TMSSpellParser,
  TMSSpellCheckCorrectLinesForm;

type
  TAdvStringGridSpellCheck = class(TAdvStringGridCheck)
  private
    FSpellCheck: TAdvSpellCheck;
    FShowDialog: Boolean;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    function MarkError(ACol, ARow: Integer; s: string): string; override;
    function Correct(ACol, ARow: Integer; s: string): string; override;
    procedure StartCheck; override;
    procedure StopCheck; override;
  published
    property Grid;
    property SpellCheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
    property ShowDialog: Boolean read FShowDialog write FShowDialog;
  end;

procedure Register;

implementation

uses
  Windows, StrUtils, SysUtils, Controls;


procedure Register;
begin
  RegisterComponents('TMS Grids',[TAdvStringGridSpellCheck]);
end;

{ TAdvStringGridSpellCheck }

function TAdvStringGridSpellCheck.Correct(ACol, ARow: Integer; s: string): string;
var
  Corr, Repl: string;
  WordList, ErrorList: TStringList;
  i: integer;
  res: boolean;
  SpellCheckDlg: TAdvSpellCheckCorrectLinesDialog;
  r: TRect;
  xy: TPoint;
begin
  Result := s;
  Corr := '';
  LastResult := mrOK;

  if not Assigned(FSpellCheck) then
    Exit;

  xy := Point(-1,-1);

  if Assigned(Grid) then
  begin
    r := (Grid as TAdvStringGrid).CelLRect(ACol,ARow);
    xy := Point(r.Left, r.Bottom);
    xy := (Grid as TAdvStringGrid).ClientToScreen(xy);
  end;

  WordList := TStringList.Create;
  ErrorList := TStringList.Create;

  try
    ParseStringToWords(s, WordList);

    for i := 0 to WordList.Count - 1 do
    begin
      res := FSpellCheck.Validate(LowerCase(WordList.Strings[i])) = wvrValidated;
      if not res then
        ErrorList.Add(WordList.Strings[i]);
    end;

    if ErrorList.Count > 0 then
    begin
      if FShowDialog then
      begin
        SpellCheckDlg := TAdvSpellCheckCorrectLinesDialog.Create(nil);
        try
          SpellCheckDlg.SpellCheck := FSpellCheck;
          LastResult := SpellCheckDlg.Execute(xy.x, xy.y, s);
          if LastResult = mrOk then
            Result := s;
        finally
          SpellCheckDlg.Free;
        end;
      end
      else
      begin
        for i := 0 to ErrorList.Count - 1 do
        begin
          Repl := FSpellCheck.FirstSuggestion(LowerCase(ErrorList.Strings[i]));

          if Repl <> '' then
            Result := ReplaceOnce(Result, ErrorList.Strings[i], Repl)
          else
            Result := HiLight(Result, ErrorList.Strings[i], 'e', False);
        end;
      end;
    end;

  finally
    WordList.Free;
    ErrorList.Free;
  end;
end;

function TAdvStringGridSpellCheck.MarkError(ACol, ARow: Integer; s: string): string;
var
  Corr: string;
  WordList: TStringList;
  i: Integer;
  res: boolean;
begin
  Result := s;
  Corr := s;

  if not Assigned(FSpellCheck) then
    Exit;

  WordList := TStringList.Create;

  try
    ParseStringToWords(s, WordList);

    for i := 0 to WordList.Count - 1 do
    begin
      res := FSpellCheck.Validate(LowerCase(WordList.Strings[i])) = wvrValidated;
      if not res then
      begin
        Corr := HiLight(Corr, WordList.Strings[i], 'e', False);
      end;
    end;
  finally
    WordList.Free;
  end;

  Result := Corr;
end;

procedure TAdvStringGridSpellCheck.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if (AOperation = opRemove) and (AComponent = FSpellCheck) then
    FSpellCheck := nil;
  inherited;
end;

procedure TAdvStringGridSpellCheck.StartCheck;
begin
end;

procedure TAdvStringGridSpellCheck.StopCheck;
begin
end;

end.
