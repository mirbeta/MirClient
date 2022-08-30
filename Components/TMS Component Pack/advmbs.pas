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
unit AdvmBS;

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
  TAdvBasicMemoStyler=class(TAdvCustomMemoStyler)
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
    property HexIdentifier;
    property AutoCompletion;
    property HintParameter;
    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;

implementation

{ TAdvBasicMemoStyler }
constructor TAdvBasicMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  LineComment := #39;
  FVersion := '1.0';
  Description := 'MS Visual Basic';
  Filter := 'Visual Basic Files (*.bas)|*.bas';
  DefaultExtension := '.bas';
  StylerName := 'MS Visual Basic';
  Extensions := 'bas';
  MultiCommentLeft := '{';
  MultiCommentRight := '}';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  HexIdentifier := '0x';
  BlockStart := 'begin';
  BlockEnd := 'end';
  //------------Pascal Standard Default-------------
  itm:=AllStyles.Add;
  itm.Info:='Basic Standard Default';
  itm.Font.Color:=clGreen;
  itm.Font.Style:=[];
  itm.BGColor := clNone;
  with itm.KeyWords do
    begin
      add('ALIAS');
      add('ALL');
      add('AND');
      add('AS');
      add('ATTACH');
      add('AUTO');
      add('AUTOX');
      add('CALL');      
      add('CASE');
      add('CFUNCTION');
      add('CLEAR');
      add('DCOMPLEX');
      add('DEC');
      add('DECLARE');
      add('DEFAULT');
      add('DIM');
      add('DO');
      add('DOUBLE');
      add('EACH');
      add('ELSE');
      add('ELSEIF');
      add('END');
      add('ENDIF');
      add('ERROR');
      add('EXIT');
      add('EXPLICIT');
      add('EXPORT');
      add('EXTERNAL');
      add('FALSE');
      add('FINALLY');
      add('FOR');
      add('FOR NEXT');
      add('FUNCADDR');
      add('FUNCTION');
      add('GIANT');
      add('GOADDR');
      add('GOSUB');
      add('GOTO');
      add('IF');
      add('IFF');
      add('IFT');
      add('IFZ');
      add('IMPORT');
      add('IN');
      add('INC');
      add('INTERNAL');
      add('IS');
      add('LIBRARYS');
      add('LOOP');
      add('MOD');
      add('MODULE');
      add('NEXT');
      add('NEW');
      add('NOT');
      add('NOTHING');
      add('OFF');
      add('ON');
      add('OPTION');
      add('OR');
      add('PRINT');
      add('PROGRAM');
      add('PROTECTED');
      add('PUBLIC');
      add('QUIT');
      add('READ');
      add('REDIM');
      add('RETURN');
      add('SBYTE');
      add('SCOMPLEX');
      add('SELECT');
      add('SHARED');
      add('SFUNCTION');
      add('SHARED');
      add('SINGLE');
      add('SLONG');
      add('SSHORT');
      add('STATIC');
      add('STEP');
      add('STOP');
      add('STRING');
      add('SUB');
      add('SUBADDR');
      add('SWAP');
      add('THEN');
      add('TO');
      add('TRUE');
      add('TRY');      
      add('TYPE');
      add('TYPEOF');
      add('UBYTE');
      add('ULONG');
      add('UNION');
      add('UNTIL');
      add('USHORT');
      add('VERSION');
      add('VOID');
      add('WHEN');
      add('WHILE');
      add('WRITE');
      add('XLONG');
      add('XOR');
    end;

  //------------Simple Quote ' '----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Simple Quote';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  itm.BGColor := clNone;
  //------------Double Quote " "----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Double Quote';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := '"';
  itm.BracketEnd := '"';
  itm.BGColor := clNone;
  //----------SYMBOL --------------
  itm := AllStyles.Add;
  itm.StyleType := stSymbol;
  itm.Info := 'Symbols Delimiters';
  itm.Font.Color := clTeal;
  itm.Font.Style := [];
  itm.Symbols := #32+',;:.(){}[]=-*/^%<>#'+#13+#10;
  itm.BGColor := clNone;

  with AutoCompletion do
  begin
    Add('ShowMessage');
    Add('MessageDlg');
  end;

  HintParameter.HintCharDelimiter := ';';
  HintParameter.HintCharWriteDelimiter := ',';

  with HintParameter.Parameters do
  begin
    Add('ShowMessage(string Msg)');
    Add('MessageDlg(string Msg; TMsgDlgType DlgType; TMsgDlgButtons Buttons; LongInt: HelpCtx)');
  end;

  
end;


end.



