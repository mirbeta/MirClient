{*************************************************************************}
{ HTMLText property editor                                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2000-2013                                        }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

{$I TMSDEFS.INC}

unit htmlprop;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, htmltext, ExtCtrls, ComCtrls, ToolWin, Menus, ImgList, Types;

type
  Thtmleditor = class(TForm)
    ToolBar1: TToolBar;
    Fontname: TComboBox;
    FontSize: TComboBox;
    BoldButton: TToolButton;
    ItalicButton: TToolButton;
    UnderlineButton: TToolButton;
    LeftAlign: TToolButton;
    CenterAlign: TToolButton;
    RightAlign: TToolButton;
    URLButton: TToolButton;
    ToolButton1: TToolButton;
    Superscript: TToolButton;
    Subscript: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ImageList1: TImageList;
    ColorDialog1: TColorDialog;
    Memo1: TMemo;
    Splitter1: TSplitter;
    HTMLStaticText1: THTMLStaticText;
    StatusBar1: TStatusBar;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ListButton: TToolButton;
    ToolButton8: TToolButton;
    dbfields: TToolButton;
    PopupMenu1: TPopupMenu;
    Test1: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N441: TMenuItem;
    ToolButton7: TToolButton;
    AcceptBtn: TToolButton;
    CancelBtn: TToolButton;
    ToolButton9: TToolButton;
    procedure Memo1Change(Sender: TObject);
    procedure BoldButtonClick(Sender: TObject);
    procedure ItalicButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure UnderlineButtonClick(Sender: TObject);
    procedure CenterAlignClick(Sender: TObject);
    procedure RightAlignClick(Sender: TObject);
    procedure FontnameChange(Sender: TObject);
    procedure URLButtonClick(Sender: TObject);
    procedure SuperscriptClick(Sender: TObject);
    procedure SubscriptClick(Sender: TObject);
    procedure HTMLStaticText1AnchorEnter(Sender: TObject; Anchor: String);
    procedure HTMLStaticText1AnchorExit(Sender: TObject; Anchor: String);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ToolButton2Click(Sender: TObject);
    procedure Memo1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LeftAlignClick(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ListButtonClick(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure dbfieldsClick(Sender: TObject);
    procedure PopupClick(Sender: TObject);
    procedure AcceptBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private
    { Private declarations }
    FFieldNames: TStringList;
    procedure GetFontNames;
    procedure SetFieldNames(const Value: TStringList);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InsertTags(starttag,endtag:string);
    function HasTags(starttag,endtag:string):boolean;
    procedure RemoveTags(starttag,endtag:string);
    function ScanForward(starttag,endtag:string):boolean;
    function ScanBackward(starttag,endtag:string):boolean;
    procedure ChangeTagProp(starttag,endtag,propname,propval:string);
    procedure UpdateButtons;
    property FieldNames: TStringList read FFieldNames write SetFieldNames;
    function GetHTMLString:string;
  end;

var
  htmleditor: Thtmleditor;

implementation

uses
  Consts;

{$R *.DFM}

{not case sensitive pos function}
function IPos(substr,s:string):integer;
begin
  Result := Pos(uppercase(substr),uppercase(s));
end;

function PosFrom(selpos:integer; substr,s:string):integer;
begin
  Delete(s,1,selpos-1);
  Result := IPos(substr,s);
end;

procedure Thtmleditor.Memo1Change(Sender: TObject);
begin
  if ToolButton9.Down then
    htmlstatictext1.HTMLText.Assign(memo1.lines);
end;

procedure Thtmleditor.InsertTags(starttag,endtag:string);
var
  s:string;
  ss,sl:integer;
begin
  ss := memo1.SelStart + 1;
  sl := memo1.SelLength;
  s := memo1.Lines.Text;

  Insert(starttag,s,ss);
  Insert(endtag,s,ss+memo1.sellength+length(starttag));

  memo1.lines.text:=s;
  memo1.selstart:=ss-1+length(starttag);
  memo1.sellength:=sl;
end;

function Thtmleditor.ScanBackward(starttag,endtag:string):boolean;
var
  i:integer;
  s:string;
begin
  i:=memo1.selstart;
  s:=memo1.lines.text;
  s:=copy(s,1,i);
  result:=false;
  while (i>=1) do
  begin
    if PosFrom(i,starttag,s)>PosFrom(i,endtag,s) then
    begin
      if pos('>',starttag)=0 then result:=pos('>',s)<>0 else
      result:=true;
      break;
    end;
    dec(i);
  end;
end;

function Thtmleditor.ScanForward(starttag,endtag:string):boolean;
var
  i:integer;
  s:string;
  st,et:integer;
begin
  Result := false;

  i := memo1.SelStart + 1;
  s := memo1.Lines.Text;
  s := Copy(s,i,length(s));
  st := IPos(starttag,s);
  et := IPos(endtag,s);

  if (st = 0) and (et > 0) then Result := true;

  if (st > 0) and (et > 0) and (et < st) then Result := true;
end;

function Thtmleditor.HasTags(starttag, endtag: string): boolean;
begin
  Result := ScanBackward(starttag, endtag) and ScanForward(starttag,endtag);
end;

procedure Thtmleditor.RemoveTags(starttag, endtag: string);
var
  i,j,k:integer;
  s:string;
begin
  i := memo1.selstart;
  j := memo1.sellength;
  s := memo1.lines.text;
  s := Copy(s,1,i);
  k := i;
  while (i >= 1) do
  begin
    if PosFrom(i,starttag,s)>PosFrom(i,endtag,s) then
    begin
      s := memo1.Lines.Text;
      delete(s,i,length(starttag));
      if (pos('>',starttag) = 0) then
        Delete(s,i,posfrom(i,'>',s));
      break;
    end;
    dec(i);
  end;
  {first tag is removed}

  delete(s,PosFrom(i,endtag,s) + i - 1,length(endtag));

  memo1.lines.text := s;
  memo1.selstart := k - Length(starttag);
  memo1.sellength := j;
end;

procedure Thtmleditor.ChangeTagProp(starttag,endtag,propname,propval:string);
var
  i,j,k,l,pp: integer;
  s,tag: string;
begin
  i := memo1.selstart;
  j := memo1.sellength;
  s := memo1.lines.text;
  k := length(s);
  l := i;
  s := Copy(s,1,i);

  tag := starttag;

  while (i >= 1) do
  begin
    if PosFrom(i,starttag,s) > PosFrom(i,endtag,s) then
    begin
      {found the tag here}
      s := memo1.lines.text;
      tag := copy(s,i,posfrom(i,'>',s));

      pp := IPos(' '+propname,tag);

      if (pp > 0) then
      begin
        inc(pp);
        {found the tag}
        delete(s,i+pp-1,length(propname));
        delete(s,i+pp-1,posfrom(i+pp,'"',s)+1);
        delete(s,i+pp-1,posfrom(i+pp,'"',s)+1);
      end;

      insert(propname+'="'+propval+'" ',s,i+length(starttag));
      break;
    end;
    dec(i);
  end;

  memo1.lines.text := s;
  memo1.selstart := l + length(s)-k;
  memo1.sellength := j;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure Thtmleditor.GetFontNames;
var
  DC: HDC;
begin
  DC := GetDC(0);
  EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontName.Items));
  ReleaseDC(0, DC);
  FontName.Sorted := True;
  FontName.ItemIndex:=0;
  FontSize.ItemIndex:=0;
end;

procedure Thtmleditor.FormCreate(Sender: TObject);
begin
  GetFontNames;
end;

procedure Thtmleditor.ToolButton1Click(Sender: TObject);
var
  l:integer;
begin
// ColorDialog1.Color:=RTFControl.SelAttributes.Color;
  if ColorDialog1.Execute then
  begin
    l:=colortorgb(colordialog1.color);
    l:=((l and $FF0000) shr 16) or ((l and $FF) shl 16) or (l and $FF00);
    if HasTags('<FONT ','</FONT>') then
      ChangeTagProp('<FONT ','</FONT>','color','#'+inttohex(l,6))
    else
      InsertTags('<FONT color="#'+inttohex(l,6)+'">','</FONT>');
  end;
end;

procedure Thtmleditor.FontnameChange(Sender: TObject);
begin
  if HasTags('<FONT ','</FONT>') then
    ChangeTagProp('<FONT ','</FONT>','face',fontname.items[fontname.itemindex])
  else
    InsertTags('<FONT face="'+fontname.items[fontname.itemindex]+'">','</FONT>');
  memo1.setfocus;
end;

procedure Thtmleditor.URLButtonClick(Sender: TObject);
var
  tag:string;
begin
  tag:='';
  if HasTags('<A ','</A>') then RemoveTags('<A ','</A>') else
  if InputQuery('Anchor', 'Anchor value',tag) then
    InsertTags('<A href="'+tag+'">','</A>');
  memo1.setfocus;
end;

procedure Thtmleditor.UnderlineButtonClick(Sender: TObject);
begin
  if not HasTags('<U>','</U>') then InsertTags('<U>','</U>') else RemoveTags('<U>','</U>');
end;

procedure Thtmleditor.BoldButtonClick(Sender: TObject);
begin
  if not HasTags('<B>','</B>') then InsertTags('<B>','</B>') else RemoveTags('<B>','</B>');
end;

procedure Thtmleditor.ItalicButtonClick(Sender: TObject);
begin
  if not HasTags('<I>','</I>') then InsertTags('<I>','</I>') else RemoveTags('<I>','</I>');
end;

procedure Thtmleditor.SuperscriptClick(Sender: TObject);
begin
  if not HasTags('<SUP>','</SUP>') then InsertTags('<SUP>','</SUP>') else RemoveTags('<SUP>','</SUP>');
end;

procedure Thtmleditor.SubscriptClick(Sender: TObject);
begin
  if not HasTags('<SUB>','</SUB>') then InsertTags('<SUB>','</SUB>') else RemoveTags('<SUB>','</SUB>');
end;

procedure Thtmleditor.HTMLStaticText1AnchorEnter(Sender: TObject;
  Anchor: String);
begin
  Statusbar1.SimpleText := Anchor;
end;

procedure Thtmleditor.HTMLStaticText1AnchorExit(Sender: TObject;
  Anchor: String);
begin
  Statusbar1.SimpleText := '';
end;

procedure Thtmleditor.ToolButton2Click(Sender: TObject);
begin
  InsertTags('<BR>','');
end;

procedure Thtmleditor.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateButtons;
end;

procedure Thtmleditor.Memo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateButtons;
end;

procedure Thtmleditor.UpdateButtons;
begin
  BoldButton.Down:=HasTags('<B>','</B>');
  ItalicButton.Down:=HasTags('<I>','</I>');
  UnderlineButton.Down:=HasTags('<U>','</U>');
  LeftAlign.Down:=not HasTags('<P align="right">','</P>') and not HasTags('<P align="center">','</P>');
  RightAlign.Down:=HasTags('<P align="right">','</P>');
  CenterAlign.Down:=HasTags('<P align="center">','</P>');
  LeftAlign.Down:=HasTags('<P align="left">','</P>');
  URLButton.Down:=HasTags('<A ','</A>');
  SuperScript.Down:=HasTags('<SUP>','</SUP>');
  SubScript.Down:=HasTags('<SUB>','</SUB>');
  ListButton.Down:=HasTags('<UL>','</UL>');
end;

procedure Thtmleditor.LeftAlignClick(Sender: TObject);
begin
  if HasTags('<P align="right">','</P>') then RemoveTags('<P align="right">','</P>');
  if HasTags('<P align="center">','</P>') then RemoveTags('<P align="center">','</P>');
  InsertTags('<P align="left">','</P>');
end;

procedure Thtmleditor.CenterAlignClick(Sender: TObject);
begin
  if HasTags('<P align="right">','</P>') then RemoveTags('<P align="right">','</P>');
  if HasTags('<P align="left">','</P>') then RemoveTags('<P align="left">','</P>');
  InsertTags('<P align="center">','</P>');
end;

procedure Thtmleditor.RightAlignClick(Sender: TObject);
begin
  if HasTags('<P align="center">','</P>') then RemoveTags('<P align="center">','</P>');
  if HasTags('<P align="left">','</P>') then RemoveTags('<P align="left">','</P>');
  InsertTags('<P align="right">','</P>');
end;


procedure Thtmleditor.FontSizeChange(Sender: TObject);
begin
  if HasTags('<FONT ','</FONT>') then
    ChangeTagProp('<FONT ','</FONT>',' size',fontsize.items[fontsize.itemindex])
  else
    InsertTags('<FONT size="'+fontsize.items[fontsize.itemindex]+'">','</FONT>');
  memo1.setfocus;
end;

procedure Thtmleditor.ToolButton3Click(Sender: TObject);
var
  l:integer;
begin
  if ColorDialog1.Execute then
  begin
    l:=colortorgb(colordialog1.color);
    l:=((l and $FF0000) shr 16) or ((l and $FF) shl 16) or (l and $FF00);
    if HasTags('<FONT ','</FONT>') then
      ChangeTagProp('<FONT ','</FONT>','bgcolor','#'+inttohex(l,6))
    else
      InsertTags('<FONT bgcolor="#'+inttohex(l,6)+'">','</FONT>');
  end;
end;

procedure Thtmleditor.ToolButton5Click(Sender: TObject);
var
  tag:string;
begin
  tag:='0';
  if InputQuery('Set indent', 'Indent value',tag) then
    InsertTags('<IND x="'+tag+'">','');
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;


function InputListQuery(const ACaption, APrompt: string; SL: TStringList;
  var Value: string): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TComboBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      {$IFDEF DELPHIXE_LVL}
      PopupMode := pmAuto;
      {$ENDIF}
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        Caption := APrompt;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Constraints.MaxWidth := MulDiv(164, DialogUnits.X, 4);
        WordWrap := True;
      end;
      Edit := TComboBox.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Edit.Items.Assign(SL);
        Left := Prompt.Left;
        Top := Prompt.Top + Prompt.Height + 5;
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
        Edit.Style := csDropDown;
      end;
      ButtonTop := Edit.Top + Edit.Height + 15;
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), Edit.Top + Edit.Height + 15,
          ButtonWidth, ButtonHeight);
        Form.ClientHeight := Top + Height + 13;
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;


procedure Thtmleditor.ToolButton4Click(Sender: TObject);
var
  tag: string;
  sl: TStringList;
  i: integer;
begin
  tag := '0';

  sl := TStringList.Create;

  if Assigned(HTMLStaticText1.PictureContainer) then
  begin
    for i := 0 to HTMLStaticText1.PictureContainer.Items.Count - 1 do
    begin
      sl.Add(HTMLStaticText1.PictureContainer.Items[i].Name);
    end;
  end;

  try
    if InputListQuery('Image', 'Image reference',sl,tag) then
      InsertTags('<IMG src="'+tag+'">','');
  finally
    sl.Free;
  end;
end;

procedure Thtmleditor.ToolButton6Click(Sender: TObject);
begin
  if not HasTags('<BLINK>','</BLINK>') then
    InsertTags('<BLINK>','</BLINK>')
  else
    RemoveTags('<BLINK>','</BLINK>');
end;

procedure Thtmleditor.ListButtonClick(Sender: TObject);
var
  i: integer;
  sl: TStringList;
begin
  if memo1.SelLength = 0 then InsertTags('<LI>','')
  else
  begin
    if not HasTags('<UL>','</UL>') then
    begin
      InsertTags('<UL>','</UL>');

      if not HasTags('<LI>','</LI>') then
      begin
        sl := TStringList.Create;
        sl.Text := memo1.SelText;

        if sl.Count > 1 then
        begin
          for i := 0 to sl.Count - 1 do
            sl.Strings[i] := '<LI>' + sl.Strings[i];

          memo1.SelText := sl.Text;
        end;
        sl.Free;
      end;

    end
    else
      RemoveTags('<UL>','</UL>');
  end;
end;

procedure Thtmleditor.ToolButton8Click(Sender: TObject);
begin
  if not HasTags('<SHAD>','</SHAD>') then
    InsertTags('<SHAD>','</SHAD>')
  else
    RemoveTags('<SHAD>','</SHAD>');
end;

constructor Thtmleditor.Create(AOwner: TComponent);
begin
  inherited;
  FFieldNames := TStringList.Create;
end;

destructor Thtmleditor.Destroy;
begin
  FFieldNames.Free;
  inherited;
end;

procedure Thtmleditor.SetFieldNames(const Value: TStringList);
begin
  FFieldNames.Assign(Value);
end;

procedure Thtmleditor.dbfieldsClick(Sender: TObject);
var
  pt:tpoint;
  i:integer;
  menuitem:tmenuitem;

begin
  while popupmenu1.Items.Count>0 do
    popupmenu1.items.items[0].Free;

  for i:=1 to fFieldNames.Count do
  begin
    menuitem:=tmenuitem.Create(self);
    menuitem.Caption:=fFieldNames.Strings[i-1];
    menuitem.OnClick:=PopupClick;
    popupmenu1.Items.add(menuitem);
  end;

  pt:=clienttoscreen(point(dbfields.left,dbfields.top+dbfields.height));
  popupmenu1.Popup(pt.x,pt.y);
end;

procedure Thtmleditor.PopupClick(Sender: TObject);
var
  s:string;
begin
  s:=(sender as TMenuItem).Caption;
  while (pos('&',s)>0) do delete(s,pos('&',s),1);
  InsertTags('<#'+s+'>','');
end;

procedure Thtmleditor.AcceptBtnClick(Sender: TObject);
begin
  modalresult:=mrOk;
end;

procedure Thtmleditor.CancelBtnClick(Sender: TObject);
begin
  modalresult:=mrCancel;
end;

function Thtmleditor.GetHTMLString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Memo1.Lines.Count do
  begin
    Result := Result + Memo1.Lines[i - 1];
  end;
end;

procedure Thtmleditor.ToolButton9Click(Sender: TObject);
begin
  if (Sender as TToolButton).Down then
  begin
    htmlstatictext1.HTMLText.Assign(memo1.lines);
  end;
end;

end.
