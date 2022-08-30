{***************************************************************************}
{ RichLabel design time editor                                              }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2004 - 2013                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit frtfbox;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, ImgList, ToolWin, RichEdit;

type
  TRTFBox = class(TForm)
    RTFControl: TRichEdit;
    Ok: TButton;
    Cancel: TButton;
    ToolBar1: TToolBar;
    Fontname: TComboBox;
    FontSize: TComboBox;
    BoldButton: TToolButton;
    ImageList1: TImageList;
    ItalicButton: TToolButton;
    UnderlineButton: TToolButton;
    LeftAlign: TToolButton;
    CenterAlign: TToolButton;
    RightAlign: TToolButton;
    BulletsButton: TToolButton;
    ToolButton1: TToolButton;
    ColorDialog1: TColorDialog;
    Superscript: TToolButton;
    Subscript: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure BoldButtonClick(Sender: TObject);
    procedure RTFControlSelectionChange(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FontnameChange(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure LeftAlignClick(Sender: TObject);
    procedure BulletsButtonClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure SuperscriptClick(Sender: TObject);
    procedure SubscriptClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure RTFControlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RTFControlKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
    italic_flag : boolean;
    procedure GetFontNames;
    function GetOffset:integer;
    procedure SetOffset(offset:integer);
  public
    { Public declarations }
  end;

var
  RTFBox: TRTFBox;

implementation

{$IFDEF DELPHIXE2_LVL}
type
  TCharFormat = TCharFormatW;
{$ENDIF}

{$R *.DFM}

procedure TRTFBox.BoldButtonClick(Sender: TObject);
begin
 if BoldButton.Down then
  RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style + [fsBold]
 else
  RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style - [fsBold];
end;

procedure TRTFBox.RTFControlSelectionChange(Sender: TObject);
begin
 BoldButton.Down := fsBold in RTFControl.SelAttributes.Style;
 ItalicButton.Down := fsItalic in RTFControl.SelAttributes.Style;
 UnderlineButton.Down := fsUnderline in RTFControl.SelAttributes.Style;
 BulletsButton.Down := Boolean(RTFControl.Paragraph.Numbering);
 FontSize.Text := IntToStr(RTFControl.SelAttributes.Size);
 FontName.Text := RTFControl.SelAttributes.Name;

 case Ord(RTFControl.Paragraph.Alignment) of
   0: LeftAlign.Down := True;
   1: RightAlign.Down := True;
   2: CenterAlign.Down := True;
 end;
 if GetOffset<0 then
   begin
    Superscript.Down:=false;
    SubScript.Down:=true;
   end;
 if GetOffset=0 then
   begin
    Superscript.Down:=false;
    SubScript.Down:=false;
   end;
 if GetOffset>0 then
   begin
    Superscript.Down:=true;
    SubScript.Down:=false;
   end;

end;

procedure TRTFBox.ItalicButtonClick(Sender: TObject);
begin
 if ItalicButton.Down then
  RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style + [fsItalic]
 else
  RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style - [fsItalic];
end;

procedure TRTFBox.UnderlineButtonClick(Sender: TObject);
begin
 if UnderlineButton.Down then
  RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style + [fsUnderline]
 else
  RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style - [fsUnderline];

end;

procedure TRTFBox.FormCreate(Sender: TObject);
begin
 GetFontNames;
 RTFControl.SelStart:=0;
 RTFControl.SelLength:=0;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;
            
procedure TRTFBox.GetFontNames;
var
  DC: HDC;
begin
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontName.Items));
  ReleaseDC(0, DC);
  FontName.Sorted := True;
end;


procedure TRTFBox.FontnameChange(Sender: TObject);
begin
 RTFControl.SelAttributes.Name := FontName.Items[FontName.ItemIndex];
end;

procedure TRTFBox.FontSizeChange(Sender: TObject);
var
 i,c:integer;
begin
 val(FontSize.text,i,c);
 if (c<>0) then
   FontSize.Text := IntToStr(RTFControl.SelAttributes.Size)
 else
   RTFControl.SelAttributes.Size :=i;

end;

procedure TRTFBox.LeftAlignClick(Sender: TObject);
begin
  RTFControl.Paragraph.Alignment := TAlignment(TControl(Sender).Tag);
end;

procedure TRTFBox.BulletsButtonClick(Sender: TObject);
begin
 RTFControl.Paragraph.LeftIndent:=10;
 RTFControl.Paragraph.Numbering := TNumberingStyle(BulletsButton.Down);
end;

procedure TRTFBox.ToolButton1Click(Sender: TObject);
begin
  ColorDialog1.Color:=RTFControl.SelAttributes.Color;
  if ColorDialog1.Execute then
    RTFControl.SelAttributes.Color:=ColorDialog1.Color;
end;

function TRTFBox.GetOffset:integer;
var
  format: TCharFormat; { defined in Unit RichEdit }
begin
  format.cbSize:= Sizeof(format);
  format.dwMask:= CFM_OFFSET;
  Sendmessage(RTFControl.Handle,EM_GETCHARFORMAT, SCF_SELECTION,LParam(@format));
  Result := format.yoffset;
end;

procedure TRTFBox.SetOffset(offset:integer);
var
  format: TCharFormat; { defined in Unit RichEdit }
begin
  FillChar(format, sizeof(format), 0);
  format.cbSize:= Sizeof(format);
  format.dwMask:= CFM_OFFSET;
  format.yOffset:= offset; { superscript by 40 twips, negative values give subscripts}
  Sendmessage(RTFControl.handle,EM_SETCHARFORMAT, SCF_SELECTION,LParam(@format));
end;


procedure TRTFBox.SuperscriptClick(Sender: TObject);
begin
  if Superscript.Down then
    SetOffset(40)
  else
    SetOffset(0);

  SubScript.Down := false;
end;

procedure TRTFBox.SubscriptClick(Sender: TObject);
begin
  if Subscript.Down then
    SetOffset(-40)
  else
    SetOffset(0);
  SuperScript.Down := false;
end;

procedure TRTFBox.ToolButton2Click(Sender: TObject);
begin
 if RTFControl.Paragraph.FirstIndent>=10 then
 RTFControl.Paragraph.FirstIndent:=RTFControl.Paragraph.FirstIndent-10;
end;

procedure TRTFBox.ToolButton3Click(Sender: TObject);
begin
 RTFControl.Paragraph.FirstIndent:=RTFControl.Paragraph.FirstIndent+10;
end;

procedure TRTFBox.RTFControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

begin
 if (ssAlt in Shift) or (ssCtrl in Shift) then
 case key of
 ord('B'):
   begin
   if fsBold in RTFControl.SelAttributes.Style then
    RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style - [fsBold]
   else
    RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style + [fsBold];
   RTFControlSelectionChange(self);
   end;
 ord('I'):
   begin
    italic_flag := True;
   if fsItalic in RTFControl.SelAttributes.Style then
    RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style - [fsItalic]
   else
    RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style + [fsItalic];
   RTFControlSelectionChange(self);
   end;
 ord('U'):
   begin
   if fsUnderline in RTFControl.SelAttributes.Style then
    RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style - [fsUnderline]
   else
    RTFControl.SelAttributes.Style := RTFControl.SelAttributes.Style + [fsUnderline];
   RTFControlSelectionChange(self);
   end;
 end;
end;

procedure TRTFBox.RTFControlKeyPress(Sender: TObject; var Key: Char);
begin
 if italic_flag then
  begin
   italic_flag := False;
   key := #0;  // Neutralize Ctrl+I a.k.a. tab character
  end;
end;

end.
