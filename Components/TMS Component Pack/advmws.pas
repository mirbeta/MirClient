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

unit AdvmWS;

interface

uses
  Classes, AdvMemo, Graphics;

const
  AllDelimiters = #32',;:.()[]=-*/^%<>#';//All the symbols
  //Next constants (keywords) is defined as comma delimmited
  
  AllHTMLKeyWordsDefaults = '"HEAD","META","BODY","HTML","TITLE","COMMENT"';

  AllHTMLKeyWordsStandard = '"A","B","I","U","P","BASE","BLINK","LINK","FONT","STRONG","IMG",' +
    '"BASEFONT","BGSOUND","DD","DEL","DFN","DIR","DIV","DL","DT","COL",' +
    '"BR","HR","COLGROUP","TABLE","MULTICOL","TBODY","TD","TEXTAREA",' +
    '"TFOOT","TH","THEAD","TR","TT","CAPTION","CENTER","CITE","CODE",' +
    '"BLOCKQUOTE","FORM","FRAME","IFRAME","ILAYER","FRAMESET","BUTTON",' +
    '"LABEL","LAYER","OPTION","ARTICLE","ASIDE","SECTION","NAV","FIELDSET"';

  AllJSKeyWords = '"SCRIPT","OBJECT","FOR","IF","THEN","THIS","DO",' +
    '"WHILE","BREAK","{","}","(",")","SWITCH","ELSE",' +
    '"FUNCTION","WINDOW","DOCUMENT",";","RETURN","STYLE","VAR","WINDOW","LOCATION"';

  AllJSFunctions = '"alert","confirm","prompt","indexOf","select","write","focus"';

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvHTMLMemoStyler = class(TAdvCustomMemoStyler)
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
    property HighlightStyle;
    property AllStyles;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
    property RegionDefinitions;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvJSMemoStyler = class(TAdvCustomMemoStyler)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BlockStart;
    property BlockEnd;
    property EscapeChar;
    property LineComment;
    property MultiCommentLeft;
    property MultiCommentRight;
    property CommentStyle;
    property NumberStyle;
    property HighlightStyle;
    property AllStyles;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvWebMemoStyler = class(TAdvCustomMemoStyler)
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
    property HighlightStyle;
    property AllStyles;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
    property RegionDefinitions;
  end;

implementation

constructor TAdvWebMemoStyler.Create(AOwner: TComponent);
var
  itm: TElementStyle;
begin
  inherited;
  FVersion := '3.0';
  Description := 'Web pages';
  Filter := 'HTML Document (*.htm,*.html)|*.htm;*.html';
  DefaultExtension := '.html';
  StylerName := 'HTML document';
  Extensions := 'html;htm';

  LineComment := '//';
  MultiCommentLeft := '<!--';
  MultiCommentRight := '-->';
  CommentStyle.TextColor := clSilver;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clNavy;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  BlockStart := '{';
  BlockEnd := '}';
  //---------Script Standard Default--------------
  itm := AllStyles.Add;
  itm.Info := 'Script Standard Default';
  itm.Font.Color := clFuchsia;
  itm.Font.Style := [fsBold];
  itm.KeyWords.CommaText := AllJSKeyWords;
  //---------HTML Standard Default--------------
  itm := AllStyles.Add;
  itm.Info := 'HTML Standard Default';
  itm.Font.Color := clFuchsia;
  itm.Font.Style := [fsBold];
  itm.KeyWords.CommaText := AllHTMLKeyWordsStandard;
  //----------Single Quote ' ' ----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Single Quote';
  itm.Font.Color := clGreen;
  itm.Font.Style := [];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  //------------Double Quote " "----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Double Quote';
  itm.Font.Color := clTeal;
  itm.Font.Style := [];
  itm.BracketStart := '"';
  itm.BracketEnd := '"';
  //----------SYMBOL --------------
  itm := AllStyles.Add;
  itm.StyleType := stSymbol;
  itm.Info := 'Symbols Delimiters';
  itm.Font.Color := clTeal;
  itm.Font.Style := [];
  itm.Symbols := AllDelimiters + #13 + #10;
  //----------Javascript functions --------------
  itm := AllStyles.Add;
  itm.Info := 'JavaScript Functions';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  itm.KeyWords.CommaText := AllJSFunctions;

  with HintParameter.Parameters do
  begin
    Add('alert(message)');
    Add('confirm(message)');
    Add('prompt(message,defaultvalue)');
  end;

  with AutoCompletion do
  begin
    Add('alert');
    Add('confirm');
    Add('prompt');
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<BODY>';
    RegionStart := '<BODY>';
    RegionEnd := '</BODY>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<HEAD>';
    RegionStart := '<HEAD>';
    RegionEnd := '</HEAD>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<HTML>';
    RegionStart := '<HTML>';
    RegionEnd := '</HTML>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<SCRIPT>';
    RegionStart := '<SCRIPT>';
    RegionEnd := '</SCRIPT>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<STYLE>';
    RegionStart := '<STYLE>';
    RegionEnd := '</STYLE>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<FORM>';
    RegionStart := '<FORM>';
    RegionEnd := '</FORM>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<DIV>';
    RegionStart := '<DIV>';
    RegionEnd := '</DIV>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<ARTICLE>';
    RegionStart := '<ARTICLE>';
    RegionEnd := '</ARTICLE>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<ASIDE>';
    RegionStart := '<ASIDE>';
    RegionEnd := '</ASIDE>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<FIELDSET>';
    RegionStart := '<FIELDSET>';
    RegionEnd := '</FIELDSET>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<TABLE>';
    RegionStart := '<TABLE>';
    RegionEnd := '</TABLE>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<SECTION>';
    RegionStart := '<SECTION>';
    RegionEnd := '</SECTION>';
    RegionType := rtClosed;
  end;
end;


{ TAdvHTMLMemoStyler }

constructor TAdvHTMLMemoStyler.Create(AOwner: TComponent);
var
  itm: TElementStyle;
begin
  inherited;
  FVersion := '3.0';
  Description := 'Web pages';
  Filter := 'HTML Document (*.htm,*.html)|*.htm;*.html';
  DefaultExtension := '.html';
  StylerName := 'HTML document';
  Extensions := 'htm;html';

  LineComment := '//';
  MultiCommentLeft := '<!--';
  MultiCommentRight := '-->';
  CommentStyle.TextColor := clSilver;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clBlack;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [];
  //------------HTML Standard Default ------------------
  itm := AllStyles.Add;
  itm.StyleType := stKeyword;
  itm.Info := 'HTML Standard Default';
  itm.Font.Color := clNavy;
  itm.Font.Style := [];
  with itm.KeyWords do
  begin
    Add('&lt');
    Add('ABREV');
    Add('ACRONYM');
    Add('ADDRESS');
    Add('APPLET');
    Add('AREA');
    Add('AU');
    Add('AUTHOR');
    Add('B');
    Add('BANNER');
    Add('BASE');
    Add('BASEFONT');
    Add('BGSOUND');
    Add('BIG');
    Add('BLINK');
    Add('BLOCQUOTE');
    Add('BODY');
    Add('BQ');
    Add('BR');
    Add('CAPTION');
    Add('CENTER');
    Add('CITE');
    Add('CODE');
    Add('COL');
    Add('COLGROUP');
    Add('COMMENT');
    Add('CREDIT');
    Add('DEL');
    Add('DFN');
    Add('DIR');
    Add('DIV');
    Add('DL');
    Add('DT');
    Add('DD');
    Add('EM');
    Add('EMBED');
    Add('FIG');
    Add('FN');
    Add('FONT');
    Add('FORM');
    Add('FRAME');
    Add('FRAMESET');
    Add('HEAD');
    Add('H1');
    Add('H2');
    Add('H3');
    Add('H4');
    Add('H5');
    Add('H6');
    Add('HR');
    Add('HTML');
    Add('I');
    Add('IFRAME');
    Add('IMG');
    Add('INPUT');
    Add('INS');
    Add('ISINDEX');
    Add('KBD');
    Add('LANG');
    Add('LH');
    Add('LI');
    Add('LINK');
    Add('LISTING');
    Add('MAP');
    Add('MARQUEE');
    Add('MATH');
    Add('MENU');
    Add('META');
    Add('MULTICOL');
    Add('NOBR');
    Add('NOFRAMES');
    Add('NOTE');
    Add('OL');
    Add('OVERLAY');
    Add('P');
    Add('PARAM');
    Add('PERSON');
    Add('PLAINTEXT');
    Add('PRE');
    Add('Q');
    Add('RANGE');
    Add('SAMP');
    Add('SCRIPT');
    Add('SELECT');
    Add('SMALL');
    Add('SPACER');
    Add('SPOT');
    Add('STRIKE');
    Add('STRONG');
    Add('SUB');
    Add('SUP');
    Add('TAB');
    Add('TBODY');
    Add('TEXTAREA');
    Add('TEXTFLOW');
    Add('TFOOT');
    Add('TH');
    Add('THEAD');
    Add('TITLE');
    Add('TT');
    Add('U');
    Add('UL');
    Add('VAR');
    Add('WBR');
    Add('XMP');
    Add('DOCTYPE');
    Add('PUBLIC');
  end;
  //---------HTML Table Keywords--------------
  itm := AllStyles.Add;
  itm.Info := 'HTML Table Keywords';
  itm.Font.Color := clOlive;
  itm.Font.Style := [];
  with itm.KeyWords do
  begin
    Add('TABLE');
    Add('BORDER');
    Add('TD');
    Add('TR');
    Add('STYLE');
    Add('BORDERCOLOR');
    Add('WIDTH');
    Add('ID');
    Add('BORDERCOLORLIGHT');
    Add('BORDERCOLORDARK');
  end;
  //---------HTML Link Keywords--------------
  itm := AllStyles.Add;
  itm.Info := 'HTML Link Keywords';
  itm.Font.Color := clRed;
  itm.Font.Style := [];
  with itm.KeyWords do
  begin
    Add('A');
    Add('HREF');
  end;
  //------------Simple Bracket ' '----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Simple Bracket';
  itm.Font.Color := clBackground;
  itm.Font.Style := [];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  //------------Double Bracket " "----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Double Bracket';
  itm.Font.Color := clBlue;
  itm.Font.Style := [];
  itm.BracketStart := '"';
  itm.BracketEnd := '"';
  //----------SYMBOL --------------
  itm := AllStyles.Add;
  itm.StyleType := stSymbol;
  itm.Info := 'Symbols Delimiters';
  itm.Font.Color := clTeal;
  itm.Font.Style := [];
  itm.Symbols := AllDelimiters + #13 + #10;
  //------------------------------

  with RegionDefinitions.Add do
  begin
    Identifier := '<BODY>';
    RegionStart := '<BODY>';
    RegionEnd := '</BODY>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<HEAD>';
    RegionStart := '<HEAD>';
    RegionEnd := '</HEAD>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<HTML>';
    RegionStart := '<HTML>';
    RegionEnd := '</HTML>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<SCRIPT>';
    RegionStart := '<SCRIPT>';
    RegionEnd := '</SCRIPT>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<STYLE>';
    RegionStart := '<STYLE>';
    RegionEnd := '</STYLE>';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := '<FORM>';
    RegionStart := '<FORM>';
    RegionEnd := '</FORM>';
    RegionType := rtClosed;
  end;

end;


{ TAdvJSMemoStyler }

constructor TAdvJSMemoStyler.Create(AOwner: TComponent);
var
  itm: TElementStyle;
begin
  inherited;
  FVersion := '3.0';
  Description := 'JavaScript';
  Filter := 'Javascript Files (*.js)|*.js';
  DefaultExtension := '.js';
  StylerName := 'JavaScript';
  Extensions := 'js';
  EscapeChar := '\';

  LineComment := '//';
  MultiCommentLeft := '<!--';
  MultiCommentRight := '-->';
  BlockStart := '{';
  BlockEnd := '}';
  CommentStyle.TextColor := clSilver;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clNavy;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];

  //------------Script Standard Default----------------
  itm := AllStyles.Add;
  itm.Info := 'Script Standard Default';
  itm.Font.Color := clFuchsia;
  itm.Font.Style := [fsBold];
  itm.KeyWords.CommaText := AllJSKeyWords;
  //------------Simple Quote ' '----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Simple Quote';
  itm.Font.Color := clGreen;
  itm.Font.Style := [];
  itm.BracketStart := #39;
  itm.BracketEnd := #39;
  //------------Double Quote " "----------------
  itm := AllStyles.Add;
  itm.StyleType := stBracket;
  itm.Info := 'Double Quote';
  itm.Font.Color := clTeal;
  itm.Font.Style := [];
  itm.BracketStart := '"';
  itm.BracketEnd := '"';
  //----------SYMBOL --------------
  itm := AllStyles.Add;
  itm.StyleType := stSymbol;
  itm.Info := 'Symbols Delimiters';
  itm.Font.Color := clTeal;
  itm.Font.Style := [];
  itm.Symbols := AllDelimiters + #13 + #10;
  //----------Javascript functions --------------
  itm := AllStyles.Add;
  itm.Info := 'JavaScript Functions';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  itm.KeyWords.CommaText := AllJSFunctions;

  with HintParameter.Parameters do
  begin
    Add('alert(message)');
    Add('confirm(message)');
    Add('prompt(message,defaultvalue)');
  end;

  with AutoCompletion do
  begin
    Add('alert');
    Add('confirm');
    Add('prompt');
  end;


end;


end.
