{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{ version 1.6                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2004                                        }
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
unit AdvmINIs;

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
  TAdvINIMemoStyler = class(TAdvCustomMemoStyler)
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
    property AllStyles;
    property AutoCompletion;
    property HintParameter;
    property HexIdentifier;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;


implementation

{ TAdvINIStyler }
constructor TAdvINIMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'Ini and Inf files';
  Filter := 'Configuration files (*.ini;*.inf)|*.ini;*.inf|All files (*.*)|*.*';
  DefaultExtension := '.ini';
  StylerName := 'Configuration Styler';
  Extensions := 'ini;inf;reg';

  LineComment := '#0';
  MultiCommentLeft := '#0';
  MultiCommentRight := '#0';
  CommentStyle.TextColor := clSilver;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clSilver;
  NumberStyle.BKColor := clWhite;
  NumberStyle.Style := [fsItalic];
  BlockStart := #0;
  BlockEnd := #0;
  HexIdentifier := '$';

//--------------Bracket--------------
  itm := AllStyles.Add;
  itm.Info := 'Bracket';
  itm.Font.Color := clBlack;
  itm.StyleType := stBracket;
  itm.Font.Style := [];
  itm.BracketStart := '[';
  itm.BracketEnd := ']';

//--------------Equal--------------
  itm := AllStyles.Add;
  itm.Info := 'Equal';
  itm.Font.Color := clBlack;
  itm.Font.Style := [];
  itm.StyleType := stSymbol;
  itm.Symbols := '=';
end;
end.
