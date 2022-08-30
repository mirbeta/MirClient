{***************************************************************************}
{ TAdvMemo styler component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2002 - 2013                                        }
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
unit AdvmCSHS;

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
  TAdvCSharpMemoStyler=class(TAdvCustomMemoStyler)
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
    property RegionDefinitions;
  end;


implementation

{ TAdvCSharpMemoStyler }

constructor TAdvCSharpMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'C# Styler';
  Filter := 'C# files|*.cs';
  DefaultExtension := '.';
  StylerName := 'C# Styler';
  Extensions := 'cs';

  LineComment := '//';
  MultiCommentLeft := '/*';
  MultiCommentRight := '*/';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  BlockStart := '{';
  BlockEnd := '}';
  Literal := '\';
  HexIdentifier := '0x';

  //------------C# Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'C# Standard Default';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
    Add('NAMESPACE');
    Add('USING');
    Add('GET');
    Add('SET');
    Add('PRIVATE');
    Add('PUBLIC');
    Add('INTERNAL');

    Add('TYPEOF');
    Add('TYPE');
    Add('INTERFACE');
    Add('DELEGATE');    
    Add('EVENT');

    Add('IF');
    Add('ELSE');

    Add('SWITCH');
    Add('CASE');
    Add('DEFAULT');    

    Add('DO');    
    Add('WHILE');

    Add('FOR');
    Add('FOREACH');
    Add('BREAK');
    Add('CONTINUE');    
    Add('RETURN');
    Add('THROW');
    Add('CATCH');
    Add('TRY');
    Add('FINALLY');
    Add('CHECKED');
    Add('UNCHECKED');
    Add('LOCK');

    Add('THIS');
    Add('VALUE');
    Add('TRUE');
    Add('FALSE');
    Add('NULL');    

    Add('CLASS');
    Add('STRUCT');

    Add('STATIC');
    Add('VOID');
    Add('UNSAFE');
    Add('INT');
    Add('BOOL');
    Add('STRING');
    Add('OBJECT');
    Add('SBYTE');
    Add('SHORT');
    Add('LONG');
    Add('USHORT');
    Add('UBYTE');
    Add('UINT');
    Add('ULONG');
    Add('FLOAT');
    Add('DOUBLE');
    Add('CHAR');
    Add('DECIMAL');
    Add('ENUM');

    Add('IMPLEMENTATION');
    Add('CONST');
    Add('PROGRAM');
    Add('PUBLISHED');
    Add('PROTECTED');
    Add('SEALED');    
    Add('PROPERTY');

    Add('FUNCTION');

    Add('PARAMS');
    Add('REF');
    Add('BASE');
    Add('EXTERN');
    Add('OPERATOR');

    Add('GOTO');

    Add('NOT');
    Add('TO');

    Add('IS');

    Add('EXPLICIT');
    Add('IMPLICIT');
    Add('ABSTRACT');
    Add('VIRTUAL');
    Add('OVERRIDE');
    Add('READONLY');
    Add('DEFAULT');
    Add('FIXED');
  end;

  itm := AllStyles.Add;
  itm.Info := 'C# Regions';
  itm.Font.Color := clBlue;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
    Add('#region');
    Add('#endregion');
  end;

  itm := AllStyles.Add;
  itm.Info := 'C# Conditional defines';
  itm.Font.Color := clNavy;
  itm.Font.Style := [fsBold];
  with itm.KeyWords do
  begin
    Add('#if');
    Add('#else');
    Add('#endif');
    Add('#define');
    Add('#undef');
    Add('#error');
    Add('#warning');
    Add('#line');                
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
  itm.Symbols := #32+',;:.()[]=-*/^%&^<>|!~'+#13+#10;

  //-------------- Region Definition
  with RegionDefinitions.Add do
  begin
    Identifier := 'namespace';
    RegionStart := '{';
    RegionEnd := '}';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := '#region';
    RegionStart := '#region';
    RegionEnd := '#endregion';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'public';
    RegionStart := '{';
    RegionEnd := '}';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'private';
    RegionStart := '{';
    RegionEnd := '}';
    RegionType := rtClosed;
  end;

end;


end.



