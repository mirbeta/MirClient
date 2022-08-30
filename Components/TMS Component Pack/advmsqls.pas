{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{ version 1.0                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002                                               }
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

unit AdvmSQLS;

interface

uses
  Classes, Windows, SysUtils, AdvMemo,
{$IFDEF DELPHI6_LVL}
  Types,
{$ENDIF}    
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
   TAdvSQLMemoStyler=class(TAdvCustomMemoStyler)
   private
    FVersion: string;
   public
    constructor Create(AOwner: TComponent); override;
   published
    property AutoCompletion;
    property HintParameter;
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

{ TAdvSQLMemoStyler }
constructor TAdvSQLMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'SQL';
  Filter := 'SQL Files (*.sql)|*.sql';
  DefaultExtension := '.sql';
  StylerName := 'SQL';
  Extensions := 'sql';

  LineComment := #39;
  MultiCommentLeft := '{';
  MultiCommentRight := '}';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  //------------Pascal Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'SQL Standard Default';
  itm.Font.Color := clGreen;
  itm.Font.Style := [];
  with itm.KeyWords do
    begin
      Add('Proc');
      Add('Procedure');
      Add('Begin');
      Add('End');
      Add('While');
      Add('BEGIN');
      Add('END');
      Add('FOR');
      Add('TO');
      Add('DO');
      Add('NOT');
      Add('IF');
      Add('ELSE');
      Add('WHILE');
      Add('REPEAT');
      Add('UNTIL');
      Add('BREAK');
      Add('CONTINUE');
      Add('EXEC');
      Add('Insert');
      Add('Values');
      Add('Update');
      Add('From');
      Add('Delete');
      Add('Declare');
      Add('Set');
      Add('Open');
      Add('Fetch');
      Add('Close');
      Add('Deallocate');
      Add('Return');
      Add('Rollback');
      Add('Transaction');
      Add('Trans');
      Add('and');
      Add('or');
      Add('Order');
      Add('By');
      Add('Group');
      Add('Having');
      Add('Where');
      Add('Left');
      Add('Right');
      Add('Join');
      Add('Inner');
      Add('Outer');
      Add('On');
      Add('Create');
      Add('Delete');
      Add('Select');
      Add('Like');
    end;

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

  AutoCompletion.Add('SELECT');
  AutoCompletion.Add('WHERE');  
end;


end.



