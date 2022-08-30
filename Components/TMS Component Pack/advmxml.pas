{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2011                                        }
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
unit Advmxml;

interface

uses
  Classes, AdvMemo,
{$IFDEF TMSCLX}
  QGraphics
{$ENDIF}
{$IFNDEF TMSCLX}
  Graphics
{$ENDIF}
  ;  

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
   TAdvXMLMemoStyler=class(TAdvCustomMemoStyler)
   private
    FVersion: string;
   public
    constructor Create(AOwner: TComponent); override;
   published
    property LineComment;
    property MultiCommentLeft;
    property MultiCommentRight;
    property CommentStyle;
    property NumberStyle;
    property AllStyles;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
   end;


implementation

{ TAdvXMLMemoStyler }
constructor TAdvXMLMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'XML Files (XML)';
  Filter := 'XML Files (*.xml)|*.xml';
  DefaultExtension := '.xml';
  StylerName := 'XML Files';
  Extensions := 'xml';

  MultiCommentLeft := '<!--';
  MultiCommentRight := '-->';

  LineComment := '//';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  //------------CSS Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'XML Default';
  itm.Font.Color := clGreen;
  itm.Font.Style := [];
  with itm.KeyWords do
    begin
      Add('val');
      Add('encoding');
      Add('xml version');
      Add(']]');
      Add('![CDATA[');
      Add('![IGNORE[');
      Add('![INCLUDE[');
      Add('!ELEMENT');
      Add('!DOCTYPE');
      Add('PCDATA');
      Add('REQUIRED');
      Add('IMPLIED');
      Add('FIXED');
      Add('ENTITY');
      Add('NMTOKEN');
      Add('NMTOKENS');
      Add('NOTATION');
      Add('ID');
      Add('IDREF');
      Add('IDREFS');
      Add('xml:space');
      Add('xml:lang');
    end;
  //------------XML Comment ----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Single quote';
  itm.Font.Color:=clBlue;
  itm.Font.Style:=[];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  //------------Double Quote " "----------------
  itm:=AllStyles.Add;
  itm.StyleType:=stBracket;
  itm.Info:='Double Quote';
  itm.Font.Color:=clBlack;
  itm.Font.Style:=[fsBold];
  itm.BracketStart:='"';
  itm.BracketEnd := '"';
  //----------SYMBOL --------------
  itm:=AllStyles.Add;
  itm.StyleType:=stSymbol;
  itm.Info:='XML delimiters';
  itm.Font.Color:=clTeal;
  itm.Font.Style:=[];
  itm.Symbols:= #32+'=<?/>#'+#13+#10;


end;

end.

