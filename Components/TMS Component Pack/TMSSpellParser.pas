{***************************************************************************}
{ TMS Spell Check component                                                 }
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

unit TMSSpellParser;

interface

{$I TMSDEFS.INC}

uses
  Classes, TMSSpellCheckUtil;

function ParseStringToWords(Sentence: string; WordList: TStrings): integer;

implementation

uses
  SysUtils;

function IsSeparator(ch: Char): boolean;
begin
  Result := (ch = ' ') or (ch = ';') or (ch = '.') or (ch = ',') or (ch = ':')
    or (ch = '!') or (ch = '?') or (ch = '"') or (ch='''') or (ch = '-') or (ch = #13) or (ch = #10);
end;

function ParseStringToWords(Sentence: string; WordList: TStrings): integer;
var
  s,tmp: string;
  i: integer;
  ch: char;
  offs,l: integer;
begin
  s := Sentence;

  tmp := '';

  offs := 0;

  for i := 1 to Length(Sentence) do
  begin
    ch := CharInStr(s,i);
    if IsSeparator(ch) then
    begin
      if tmp <> '' then
      begin
        l := offs - Length(tmp);
        WordList.AddObject(tmp, TObject(l));
      end;
      tmp := '';
      inc(offs);
    end
    else
    begin
      tmp := tmp + ch;
      inc(offs);
    end;
  end;

  if tmp <> '' then
  begin
    l := offs - Length(tmp);
    WordList.AddObject(tmp, TObject(l));
  end;

  Result := WordList.Count;
end;

end.
