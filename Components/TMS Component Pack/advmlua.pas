{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2015                                        }
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

unit AdvmLUA;

interface

uses
  Classes, AdvMemo;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvLuaMemoStyler = class(TAdvCustomMemoStyler)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Version: string read FVersion;
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
  Graphics;

{ TAdvLuaMemoStyler }
constructor TAdvLuaMemoStyler.Create(AOwner: TComponent);
var
  itm: TElementStyle;
begin
  inherited;
  FVersion := '3.0';
  Description := 'Lua';
  Filter := 'Lua Files (*.lua)|*.lua';
  DefaultExtension := '.lua';
  StylerName := 'Lua';
  Extensions := 'lua';

  LineComment := '--';
  MultiCommentLeft := '---[[';
  MultiCommentRight := ']]';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  BlockStart := 'begin,function,if,for,while,repeat,do';
  BlockEnd := 'end,until';
  HexIdentifier := '0x';
  //------------Pascal Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'Lua Standard Default';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
    Add('AND');
    Add('END');
    Add('IN');
    Add('REPEAT');
    Add('BREAK');
    Add('FALSE');
    Add('LOCAL');
    Add('RETURN');
    Add('DO');
    Add('FOR');
    Add('NIL');
    Add('THEN');
    Add('ELSE');
    Add('FUNCTION');
    Add('NOT');
    Add('ELSEIF');
    Add('IF');
    Add('OR');
    Add('UNTIL');
    Add('WHILE');
  end;
  
  itm := AllStyles.Add;
  itm.Info := 'Lua functions';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
     Add('print');
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
  itm.Info := 'Symbols Delimiters';
  itm.Font.Color := clred;
  itm.Font.Style := [];
  itm.Symbols := #32+',;:.(){}[]=+-*/^%&^<>|!~'+#13+#10;

  with HintParameter.Parameters do
  begin
    Add('print(···)');
  end;

  with AutoCompletion do
  begin
    Add('print');
  end;

  //-------------- Region Definition
  with RegionDefinitions.Add do
  begin
    Identifier := 'function';
    RegionStart := 'function';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'if';
    RegionStart := 'if';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'for';
    RegionStart := 'for';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'repeat';
    RegionStart := 'repeat';
    RegionEnd := 'until';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'while';
    RegionStart := 'while';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'do';
    RegionStart := 'do';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;

end;

end.



