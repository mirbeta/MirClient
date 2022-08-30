{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{ version 1.5                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2004                                        }
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
unit AdvmPYS;

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
  TAdvPythonMemoStyler = class(TAdvCustomMemoStyler)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BlockStart;
    property BlockEnd;
    property LineComment;
    property MultiCommentLeft;
    property MultiCommentRight;
    property CommentStyle;
    property NumberStyle;
    property AllStyles;
    property AutoCompletion;
    property HintParameter;
    property HexIdentifier;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;


implementation

{ TAdvPhytonMemoStyler }
constructor TAdvPythonMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'Python Scripts';
  Filter := 'Python Files (*.py)|*.py';
  DefaultExtension := '.py';
  StylerName := 'Python';
  Extensions := 'py';

  LineComment := '#';
  MultiCommentLeft := '"""';
  MultiCommentRight := '"""';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  BlockStart := '';
  BlockEnd := '';
  HexIdentifier := '$';
  //------------Python Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'Python keywords';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
    Add('AND');
    Add('ASSERT');
    Add('BREAK');
    Add('CLASS');
    Add('CONTINUE');
    Add('DEF');
    Add('DEL');
    Add('ELIF');
    Add('ELSE');
    Add('EXCEPT');
    Add('EXEC');
    Add('FINALLY');
    Add('FOR');
    Add('FROM');
    Add('GLOBAL');
    Add('IF');
    Add('IMPORT');
    Add('IN');
    Add('IS');
    Add('LAMBDA');
    Add('NOT');
    Add('OR');
    Add('PASS');
    Add('PRINT');
    Add('RAISE');
    Add('RETURN');
    Add('TRY');
    Add('WHILE');
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
  itm.Symbols := #32+';.,:"{}[]()?!@#$%^&*-+=|\/'''+#13+#10;

end;


end.



