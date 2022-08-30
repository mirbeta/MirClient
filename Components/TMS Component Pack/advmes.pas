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
unit AdvmES;

interface

uses
  Classes, Windows, SysUtils, AdvMemo, Types, Graphics
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

{$R advmes.res}


type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvEmoticonMemoStyler=class(TAdvCustomMemoStyler)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DrawKeyword(Canvas: TCanvas; AKeyword: string; var ARect: TRect); override;
  published
    property AllStyles;

    property Version: string read FVersion;
    property Description;
    property Filter;
    property DefaultExtension;
    property StylerName;
    property Extensions;
  end;


implementation


{ TAdvPascalMemoStyler }
constructor TAdvEmoticonMemoStyler.Create(AOwner: TComponent);
var
  itm:TElementStyle;
begin
  inherited;
  FVersion := '1.0';
  Description := 'Emoticon Styler';
  Filter := 'Emoticon|*.gif| All files|*.*';
  DefaultExtension := '*';
  StylerName := 'Emoticon';
  Extensions := 'gif';

  LineComment := '';
  MultiCommentLeft := '';
  MultiCommentRight := '';
  CommentStyle.TextColor := clNavy;
  CommentStyle.BkColor := clWhite;
  CommentStyle.Style := [fsItalic];
  NumberStyle.TextColor := clFuchsia;
  NumberStyle.BkColor := clWhite;
  NumberStyle.Style := [fsBold];
  BlockStart := '';
  BlockEnd := '';
  HexIdentifier := '';
  NumericChars := '';
  CustomDraw := True;

  //------------Pascal Standard Default-------------
  itm := AllStyles.Add;
  itm.Info := 'Emoticons';
  itm.Font.Color := clGreen;
  itm.Font.Style := [fsBold];
  itm.StyleType := stKeyword;

  with itm.KeyWords do
  begin
    Add(':)');
    Add(':(');
    Add(';)');
    Add(':o');
    Add(':s');
    Add(':d');
    Add(':p');
    Add(':|');
    Add(':$');
    Add(':@');
  end;
end;


procedure TAdvEmoticonMemoStyler.DrawKeyword(Canvas: TCanvas;
  AKeyword: string; var ARect: TRect);
var
  bmp: tbitmap;
  resname: string;
  r: TRect;
begin
  bmp := tbitmap.create;

  AKeyword := lowercase(AKeyword);
  resname := '';

  if AKeyword = ':(' then
    resname := 'TMSEMO1';
  if AKeyword = ':)' then
    resname := 'TMSEMO2';
  if AKeyword = ';)' then
    resname := 'TMSEMO3';
  if AKeyword = ':o' then
    resname := 'TMSEMO4';
  if AKeyword = ':s' then
    resname := 'TMSEMO5';
  if AKeyword = ':d' then
    resname := 'TMSEMO6';
  if AKeyword = ':p' then
    resname := 'TMSEMO7';
  if AKeyword = ':|' then
    resname := 'TMSEMO8';
  if AKeyword = ':$' then
    resname := 'TMSEMO9';
  if AKeyword = ':@' then
    resname := 'TMSEMO10';

  if resname = '' then
    Exit;

  r := arect;
  r.Right := arect.left + 2*canvas.textwidth('w');
  r.Bottom := arect.Top + canvas.textheight('h');

  Canvas.Pen.Color := Canvas.Brush.Color;
  Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);

  bmp.LoadFromResourceName(HInstance,pchar(resname));
  bmp.TransparentMode := tmAuto;
  bmp.Transparent := true;

  Canvas.StretchDraw(r,bmp);

  bmp.Free;

  arect.left := arect.left + 2*canvas.textwidth('w');
end;

end.



