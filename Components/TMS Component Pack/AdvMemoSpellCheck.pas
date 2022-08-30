{***************************************************************************}
{ TAdvMemo spell check interface component                                  }
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
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit AdvMemoSpellCheck;

interface

uses
  AdvMemo, Classes, SysUtils, Controls,
  TMSSpellCheck, TMSSpellParser, TMSSpellCheckCorrectLinesForm,
  TMSSpellCheckCorrectForm, Windows;

type

  TAdvMemoSpellChecker = class(TAdvMemoChecker)
  private
    FSpellCheck: TAdvSpellCheck;
    FShowDialog: Boolean;
    FDialogCaption: string;
  protected
    procedure CheckSubComps;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
  public
    procedure CheckLine(LineNo: Integer); override;
    procedure CheckWord(LineNo,LinePos: Integer; s: string); override;

    procedure CorrectLine(LineNo: Integer); override;
    procedure CorrectWord(LineNo,LinePos: Integer; var s: string); override;
  published
    property AutoCorrectType;
    property SpellCheck: TAdvSpellCheck read FSpellCheck write FSpellCheck;
    property DialogCaption: string read FDialogCaption write FDialogCaption;
    property ShowDialog: Boolean read FShowDialog write FShowDialog;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Memo',[TAdvMemoSpellChecker]);
end;

{ TAdvMemoSpellChecker }

procedure TAdvMemoSpellChecker.CheckLine(LineNo: Integer);
var
  sl: TStringList;
  s: string;
  i,x: integer;
begin
  CheckSubComps;

  s := Memo.Lines[LineNo];

  sl := TStringList.Create;

  try
    ParseStringToWords(s,sl);

    for i := 0 to sl.Count - 1 do
    begin
      if (SpellCheck.Validate(sl.Strings[i]) = wvrNotValidated) then
      begin
        x := integer(sl.Objects[i]);
        Memo.SetError(LineNo, x, Length(sl.Strings[i]));
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TAdvMemoSpellChecker.CheckSubComps;
begin
  if not Assigned(Memo) then
    raise Exception.Create('No memo assigned');
  if not Assigned(SpellCheck) then
    raise Exception.Create('No Spell Check engine assigned');
end;

procedure TAdvMemoSpellChecker.CheckWord(LineNo, LinePos: Integer; s: string);
begin
  CheckSubComps;

  if SpellCheck.Validate(s) = wvrNotValidated then
    CheckLine(LineNo)
  else
    Memo.ClearWordError(LineNo, Memo.CurX - length(s));
end;

procedure TAdvMemoSpellChecker.CorrectLine(LineNo: Integer);
var
  sl: TStringList;
  s, corr, res: string;
  i: integer;
  cd: TAdvSpellCheckCorrectLinesDialog;
  pt: TPoint;
begin
  CheckSubComps;

  s := Memo.Lines[LineNo];

  if ShowDialog then
  begin
    cd := TAdvSpellCheckCorrectLinesDialog.Create(Self);
    cd.SpellCheck := SpellCheck;
    try
      pt := CurrentWordPos(0, LineNo);
      pt.X := Memo.Width div 2;
      pt.Y := pt.Y + 10;
      pt := Memo.ClientToScreen(pt);

      cd.Caption := DialogCaption;
      if cd.Execute(pt.X, pt.Y, s) = mrOK then
        Memo.Lines[LineNo] := s;
    finally
      cd.Free;
    end;
  end
  else
  begin
    sl := TStringList.Create;
    try
      res := s;
      ParseStringToWords(s, sl);
      for i := 0 to sl.Count - 1 do
      begin
        corr := SpellCheck.FirstSuggestion(sl.Strings[i]);

        if (corr <> '') and (corr <> sl.Strings[i]) then
          res := ReplaceOnce(res, sl.Strings[i], corr);
      end;

      Memo.Lines[LineNo] := res;

    finally
      sl.Free;
    end;
  end;
end;

procedure TAdvMemoSpellChecker.CorrectWord(LineNo, LinePos: Integer;
  var s: string);
var
  cd: TAdvSpellCheckCorrectDialog;
  corr: string;
  pt: TPoint;
begin
  CheckSubComps;

  if SpellCheck.Validate(s) = wvrNotValidated then
  begin
    if ShowDialog then
    begin
      cd := TAdvSpellCheckCorrectDialog.Create(Self);
      try
        pt := CurrentWordPos(LinePos, LineNo);
        pt.Y := pt.Y + 10;
        pt := Memo.ClientToScreen(pt);

        cd.Caption := DialogCaption;
        cd.SpellCheck  := SpellCheck;
        cd.UI.ShowNext := false;
        cd.UI.ShowPrevious := false;
        try
          corr := s;
          if cd.Execute(pt.X, pt.Y, Corr) = mrOK then
            s := corr;
        finally
          cd.Free;
        end;
      finally

      end;
    end
    else
    begin
      corr := SpellCheck.FirstSuggestion(s);

      if (corr <> '') and (corr <> s) then
        s := corr;
    end;
  end;
end;

procedure TAdvMemoSpellChecker.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FSpellCheck) and (AOperation = opRemove) then
    FSpellCheck := nil;
end;

end.
