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

unit AdvmPS;

interface

uses
  Classes, AdvMemo;

type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvPascalMemoStyler = class(TAdvCustomMemoStyler)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromDelphiSettings;
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
  Graphics, Registry, Windows;

{ TAdvPascalMemoStyler }
constructor TAdvPascalMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '3.0';
  Description := 'Pascal';
  Filter := 'Pascal Files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc';
  DefaultExtension := '.pas';
  StylerName := 'Pascal';
  Extensions := 'pas;dpr;dpk;inc';

  LineComment := '//';
  MultiCommentLeft := '{';
  MultiCommentRight := '}';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clNone;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clNone;
  NumberStyle.Style := [fsBold];
  BlockStart := 'begin,try,case,class,record,interface';
  BlockEnd := 'end';
  HexIdentifier := '$';
  //------------Pascal Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'Pascal Standard Default';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  itm.BGColor := clNone;
  with itm.KeyWords do
  begin
    Add('UNIT');
    Add('INTERFACE');
    Add('IMPLEMENTATION');
    Add('USES');
    Add('CONST');
    Add('PROGRAM');
    Add('PRIVATE');
    Add('PUBLIC');
    Add('PUBLISHED');
    Add('PROTECTED');
    Add('PROPERTY');
    Add('FUNCTION');
    Add('FINALISE');
    Add('INITIALISE');
    Add('VAR');
    Add('BEGIN');
    Add('WITH');
    Add('END');
    Add('FOR');
    Add('TO');
    Add('DO');
    Add('NOT');
    Add('IF');
    Add('THEN');
    Add('ELSE');
    Add('TYPE');
    Add('WHILE');
    Add('REPEAT');
    Add('UNTIL');
    Add('BREAK');
    Add('CONTINUE');
    Add('VIRTUAL');
    Add('OVERRIDE');
    Add('DEFAULT');
    Add('CLASS');
    Add('STORED');
    Add('INHERITED');
    Add('PROCEDURE');
    Add('CONSTRUCTOR');
    Add('DESTRUCTOR');
    Add('FINALLY');
    Add('RAISE');
    Add('STRING');
    Add('TRY');
    Add('EXCEPT');
    Add('STDCALL');
    Add('CDECL');
    Add('PASCAL');
    Add('NIL');
    Add('CASE');
    Add('REINTRODUCE');
    Add('PACKED');
    Add('RECORD');
    Add('MESSAGE');
    Add('IN');
    Add('IS');
    Add('SHL');
    Add('SHR');
    Add('MOD');
    Add('DIV');
    Add('XOR');
    Add('OR');
    Add('AND');
    Add('OF');
    Add('SET');
    Add('DOWNTO');
    Add('EXPORTS');
    Add('LIBRARY');
    Add('AS');
    Add('ASM');
    Add('DYNAMIC');
    Add('OBJECT');
    Add('THREADVAR');
    Add('FILE');
    Add('ABSTRACT');
    Add('OVERLOAD');
    Add('ASSEMBLER');
    Add('ABSOLUTE');
    Add('AUTOMATED');
    Add('EXTERNAL');
    Add('REGISTER');
    Add('DISPINTERFACE');
    Add('RESOURCESTRING');
    Add('NEAR');
    Add('FAR');
    Add('LABEL');
    Add('OUT');
    Add('SAFECALL');
    Add('DISPID');
    Add('ARRAY');
    Add('INLINE');
    Add('FORWARD');
    Add('PLATFORM');
    Add('DEPRECATED');
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
  itm.Font.Color := clred;
  itm.Font.Style := [];
  itm.Symbols := #32+',;:.(){}[]=+-*/^%<>#'+#13+#10;
  itm.BGColor := clNone;
  //----------MULTI LINE COMMENT --------------
  itm := AllStyles.Add;
  itm.StyleType := stComment;
  itm.Info := 'Multi line comment';
  itm.CommentLeft := '(*';
  itm.CommentRight := '*)';
  itm.BGColor := clNone;
  itm.Font.Color := clGray;
  itm.Font.Style := [fsItalic];

  with HintParameter.Parameters do
  begin
    Add('ShowMessage(const Msg: string);');
    Add('MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer);');
  end;

  with AutoCompletion do
  begin
    Add('ShowMessage');
    Add('MessageDlg');
  end;

  //-------------- Region Definition
  with RegionDefinitions.Add do
  begin
    Identifier := 'procedure';
    RegionStart := 'begin';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'procedure';
    RegionStart := '';
    RegionEnd := 'forward';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'constructor';
    RegionStart := 'begin';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'destructor';
    RegionStart := 'begin';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;

  with RegionDefinitions.Add do
  begin
    Identifier := 'interface';
    RegionStart := 'interface';
    RegionType := rtOpen;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'unit';
    RegionStart := 'unit';
    RegionType := rtFile;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'implementation';
    RegionStart := 'implementation';
    RegionType := rtOpen;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'case';
    RegionStart := 'case';
    RegionEnd := 'end';
    RegionType := rtIgnore;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'try';
    RegionStart := 'try';
    RegionEnd := 'end';
    RegionType := rtIgnore;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := 'function';
    RegionStart := 'begin';
    RegionEnd := 'end';
    RegionType := rtClosed;
  end;
  with RegionDefinitions.Add do
  begin
    Identifier := '{$region';
    RegionStart := '{$region';
    RegionEnd := '{$endregion';
    RegionType := rtClosed;
  end;
end;


procedure TAdvPascalMemoStyler.LoadFromDelphiSettings;
var
  r: TRegistry;
  fg,bk: string;
  rkey: string;
begin

  r := TRegistry.Create;
  r.RootKey := HKEY_CURRENT_USER;

  rkey := '';

  if r.KeyExists('Software\Embarcadero\BDS\16.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\16.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\15.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\15.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\14.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\14.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\12.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\12.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\11.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\11.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\10.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\10.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\9.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\9.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\Embarcadero\BDS\8.0\Editor\Highlight') then
    rkey := 'Software\Embarcadero\BDS\8.0\Editor\Highlight\'
  else
  if r.KeyExists('Software\CodeGear\BDS\7.0\Editor\Highlight') then
    rkey := 'Software\CodeGear\BDS\7.0\Editor\Highlight\';

  if (rkey <> '') then
  begin
    r.OpenKey(rkey + 'Reserved Word',false);
    fg := r.ReadString('Foreground Color New');
    bk := r.ReadString('Background Color New');

    AllStyles[0].Font.Color := StringToColor(fg);
    AllStyles[0].BGColor := StringToColor(bk);
    r.CloseKey;

    r.OpenKey(rkey + 'Symbol',false);
    fg := r.ReadString('Foreground Color New');
    bk := r.ReadString('Background Color New');

    AllStyles[3].Font.Color := StringToColor(fg);
    AllStyles[3].BGColor := StringToColor(bk);
    r.CloseKey;

    r.OpenKey(rkey + 'String',false);
    fg := r.ReadString('Foreground Color New');
    bk := r.ReadString('Background Color New');

    AllStyles[1].Font.Color := StringToColor(fg);
    AllStyles[1].BGColor := StringToColor(bk);
    r.CloseKey;

    r.OpenKey(rkey + 'Comment',false);
    fg := r.ReadString('Foreground Color New');
    bk := r.ReadString('Background Color New');

    CommentStyle.TextColor := StringToColor(fg);
    CommentStyle.BkColor := StringToColor(bk);
    r.CloseKey;

    r.OpenKey(rkey + 'Number',false);
    fg := r.ReadString('Foreground Color New');
    bk := r.ReadString('Background Color New');

    NumberStyle.TextColor := StringToColor(fg);
    NumberStyle.BkColor := StringToColor(bk);
    r.CloseKey;
  end;

  r.Free;
end;

end.



