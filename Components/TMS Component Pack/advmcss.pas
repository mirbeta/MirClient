{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{ version 1.5                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004                                               }
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
unit AdvmCSS;

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
   TAdvCSSMemoStyler=class(TAdvCustomMemoStyler)
   private
    FVersion: string;
   public
    constructor Create(AOwner: TComponent); override;
   published
    property    LineComment;
    property    MultiCommentLeft;
    property    MultiCommentRight;
    property    CommentStyle;
    property    NumberStyle;
    property    AllStyles;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
   end;


implementation

{ TAdvCSSMemoStyler }
constructor TAdvCSSMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'Cascading Style Sheets (CSS)';
  Filter := 'Cascading Stylesheets (*.css)|*.css';
  DefaultExtension := '.css';
  StylerName := 'Cascading Style Sheets';
  Extensions := 'css';

  LineComment := '//';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  //------------CSS Standard Default-------------
  itm:=AllStyles.Add;
  itm.Info:='CSS Standard Default';
  itm.Font.Color:=clGreen;
  itm.Font.Style:=[];
  with itm.KeyWords do
    begin
      add('.H1');
      add('.H2');
      add('.H3');
      add('.H4');
      add('.H5');
      add('.BORDER');
      add('.COMMENT');
    end;
  //------------Simple Quote ' '----------------
  itm:=AllStyles.Add;
  itm.StyleType:=stBracket;
  itm.Info:='Simple Quote';
  itm.Font.Color:=clBlue;
  itm.Font.Style:=[];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  //------------Double Quote " "----------------
  itm:=AllStyles.Add;
  itm.StyleType:=stBracket;
  itm.Info:='Double Quote';
  itm.Font.Color:=clBlue;
  itm.Font.Style:=[];
  itm.BracketStart:='"';
  itm.BracketEnd := '"';
  //----------SYMBOL --------------
  itm:=AllStyles.Add;
  itm.StyleType:=stSymbol;
  itm.Info:='Symbols Delimiters';
  itm.Font.Color:=clTeal;
  itm.Font.Style:=[];
  itm.Symbols:= #32+',;:.(){}[]=-*/^%<>#'+#13+#10;


end;

end.
