{***************************************************************************}
{ TAdvMemo styler component                                                 }
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

{$I TMSDEFS.INC}

unit advmjson;

interface

uses
  Classes, AdvMemo;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvJSONMemoStyler = class(TAdvCustomMemoStyler)
  private
    FVersion: string;
    FAutoFormat: boolean;
    FAutoIndent: integer;
  protected
    function ForceLineBreakChars: string; override;
    function Format(s: string): string; override;
    function HasFormatting: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Version: string read FVersion;
    property AutoFormat: boolean read FAutoFormat write FAutoFormat default true;
    property AutoIndent: integer read FAutoIndent write FAutoIndent default 4;
    property BlockStart;
    property BlockEnd;
    property LineComment;
    property MultiCommentLeft;
    property MultiCommentRight;
    property CommentStyle;
    property NumberStyle;
    property HighlightStyle;
    property AllStyles;
    property AutoCompletion;
    property HintParameter;
    property HexIdentifier;
   
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
    property RegionDefinitions;
  end;

implementation

uses
  Graphics, StrUtils, SysUtils;

{ TAdvLuaMemoStyler }
constructor TAdvJSONMemoStyler.Create(AOwner: TComponent);
var
  itm: TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  FAutoFormat := true;
  FAutoIndent := 4;
  Description := 'JSON';
  Filter := 'JSON Files (*.json)|*.json';
  DefaultExtension := '.json';
  StylerName := 'JSON';
  Extensions := 'json';
  EscapeChar := '\';
  LineComment := '';
  MultiCommentLeft := '';
  MultiCommentRight := '';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  HexIdentifier := '0x';

  //------------JSON Keywords-------------
  itm := AllStyles.Add;
  itm.Info := 'JSON Standard Default';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
    Add('NULL');
    Add('TRUE');
    Add('FALSE');
  end;


  //------------Simple Quote ' '----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Simple Quote';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  //------------Double Quote " "----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Double Quote';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := '"';
  itm.BracketEnd := '"';

  //----------SYMBOL --------------
  itm := AllStyles.Add;
  itm.StyleType := stSymbol;
  itm.Info := 'Symbols';
  itm.Font.Color := clred;
  itm.Font.Style := [];
  itm.Symbols := ',:(){}[]=';

  //-------------- Region Definition
  with RegionDefinitions.Add do
  begin
    Identifier := '{';
    RegionStart := '{';
    RegionEnd := '}';
    RegionType := rtClosed;
  end;

end;

function TAdvJSONMemoStyler.ForceLineBreakChars: string;
begin
  Result := '{[,';
end;

function TAdvJSONMemoStyler.Format(s: string): string;
var
  sl: TStringList;
  i,ind: integer;
  q: integer;
begin
  sl := TStringList.Create;
  sl.Text := s;

  s := '';
  for i := 0 to sl.Count - 1 do
    s := s + sl.Strings[i];

  s := StringReplace(s, ']', #13#10']', [rfReplaceAll]);
  s := StringReplace(s, '}', #13#10'}', [rfReplaceAll]);
  s := StringReplace(s, '{', #13#10'{'#13#10, [rfReplaceAll]);
  s := StringReplace(s, '[', #13#10'['#13#10, [rfReplaceAll]);

  q := 0;

  for i := 0 to Length(s) do
  begin
    if s[i] = '"' then
      inc(q);
    if (s[i] = ',') and not odd(q) then
      s[i] := #8;
  end;

  s := StringReplace(s, #8, ','#13#10, [rfReplaceAll]);

  ind := 0;

  sl.Text := s;

  for i := sl.Count - 1 downto 0 do
  begin
    if Trim(sl.Strings[i]) = '' then
      sl.Delete(i);
  end;

  for i := 0 to sl.Count - 1 do
  begin
    if pos('}',sl.Strings[i]) > 0 then
      dec(ind);
    if pos(']',sl.Strings[i]) > 0 then
      dec(ind);

    sl.Strings[i] := StringOfChar(#32,ind * FAutoIndent) + Trim(sl.Strings[i]);

    if pos('{',sl.Strings[i]) > 0 then
      inc(ind);
    if pos('[',sl.Strings[i]) > 0 then
      inc(ind);
  end;

  Result := sl.Text;
  sl.Free;
end;

function TAdvJSONMemoStyler.HasFormatting: boolean;
begin
  Result := FAutoFormat;
end;

end.



