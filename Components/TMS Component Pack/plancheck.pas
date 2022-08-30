{***********************************************************************}
{ TPlanner Checker components                                           }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3.0,4.0,5.0,6.0               }
{ version 1.6, July 2002                                                }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2002                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

{$I TMSDEFS.INC}

unit plancheck;

interface

uses
  Classes, SysUtils;

type

  // base class
  TPlannerCheck = class(TComponent)
  private
    FGotoItem: Boolean;
    FAutoCorrect: Boolean;
    FAutoMarkError: Boolean;
    FUseCorrect: Boolean;
    FUseMarkError: Boolean;
  protected
    function HiLight(s,h,tag:string;DoCase:boolean):string;
    function ReplaceOnce(const S, OldPattern, NewPattern: string): string;
  public
    function MarkError(s:string):string; virtual;
    function Correct(s:string):string; virtual;
    procedure StartCheck; virtual;
    procedure StopCheck; virtual;
  published
    property AutoCorrect: Boolean read FAutoCorrect write FAutoCorrect;
    property AutoMarkError: Boolean read FAutoMarkError write FAutoMarkError;
    property GotoItem: Boolean read FGotoItem write FGotoItem;
    property UseCorrect: Boolean read FUseCorrect write FUseCorrect;
    property UseMarkError: Boolean read FUseMarkError write FUseMarkError;
  end;

  TCapitalPlannerCheck = class(TPlannerCheck)
  public
    function Correct(s:string):string; override;
  end;

implementation

{ TPlannerCheck }

// hilighter utility functions

function VarPos(su,s:string;var Res:Integer):Integer;
begin
  Res := Pos(su,s);
  Result := Res;
end;


function HTMLStrip(s:string):string;
var
  TagPos: integer;
begin
  Result := '';
  // replace line breaks by linefeeds
  while Pos('<BR>',UpperCase(s)) > 0 do
    s := StringReplace(s,'<BR>',chr(13)+chr(10),[rfIgnoreCase]);
  while Pos('<HR>',UpperCase(s)) > 0 do
    s := StringReplace(s,'<HR>',chr(13)+chr(10),[rfIgnoreCase]);

  // remove all other tags
  while (VarPos('<',s,TagPos)>0) do
  begin
    Result := Result + Copy(s,1,TagPos-1);
    if (VarPos('>',s,TagPos)>0) then
      Delete(s,1,TagPos);
  end;
  Result := Result + s;
end;

function PosFrom(su,s:string; h: Integer;DoCase: boolean; var Res: Integer): Integer;
var
  r: Integer;
begin
  Result := 0;
  Res := 0;

  if h > 0 then
    Delete(s,1,h);

  if DoCase then
    r := Pos(su,s)
  else
    r := Pos(UpperCase(su),UpperCase(s));

  if r > 0 then
  begin
    Res := h + r;
    Result := Res;
  end;
end;

function StripPos2HTMLPos(s:string; i: Integer): Integer;
var
  j,k: Integer;
  Skip: Boolean;
begin
  Result := 0;
  k := 1;
  Skip := False;

  for j := 1 to Length(s) do
  begin
    if s[j] = '<' then
      Skip := True;

    if k = i then
    begin
      Result := j;
      Exit;
    end;

    if not Skip then
      Inc(k);

    if s[j] = '>' then
      Skip := False;

  end;

  if k = i then
  begin
    Result := Length(s) + 1;
  end;
end;

function TPlannerCheck.ReplaceOnce(const S, OldPattern, NewPattern: string): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  SearchStr := S;
  Patt := OldPattern;

  NewStr := S;
  Result := '';

  Offset := AnsiPos(Patt, SearchStr);

  if Offset = 0 then
    Result := Result + NewStr
  else
  begin
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    Result := Result + NewStr;
  end;  
end;


function TPlannerCheck.HiLight(s,h,tag:string;DoCase:boolean):string;
var
  hs: string;
  l,k: Integer;
begin
  hs := HTMLStrip(s);
  l := 0;
  while PosFrom(h,hs,l,DoCase,k) > 0 do
  begin
    l := k + Length(h);
    Insert('<'+tag+'>',s,StripPos2HTMLPos(s,k));
    Insert('</'+tag+'>',s,StripPos2HTMLPos(s,l));
  end;
  Result := s;
end;

function TPlannerCheck.Correct(s: string): string;
begin
  Result := s;
end;

function TPlannerCheck.MarkError(s: string): string;
begin
  Result := s;
end;

procedure TPlannerCheck.StartCheck;
begin

end;

procedure TPlannerCheck.StopCheck;
begin

end;

{ TCapitalCheck }

function TCapitalPlannerCheck.Correct(s: string): string;
var
  Prev,PrevPrev:Char;
  i: Integer;
begin
  Prev := ' ';
  PrevPrev := '.';

  for i := 1 to Length(s) do
  begin
    if (Prev = ' ') and ( (PrevPrev = '!') or (PrevPrev = '?') or (PrevPrev = '.') ) and (s[i] <> Upcase(s[i])) then
      s[i] := UpCase(s[i]);
    PrevPrev := Prev;
    Prev := s[i];  
  end;

  Result := s;
end;



end.
