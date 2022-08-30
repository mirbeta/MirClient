{*************************************************************************}
{ Param TreeView property editor                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2000-2012                                        }
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

unit paramtreeprop;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ImgList, ComCtrls,  Buttons,
  ToolWin, Menus, ParamEdit, ParamLabel, ParamListbox, ParamTreeView;

type

  Tparamtreeeditor = class(TForm)
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
    ToolButton12: TToolButton;
    Panel1: TPanel;
    Memo1: TMemo;
    ParamLabel1: TParamLabel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ParamTreeview1: TParamTreeview;
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
    procedure ParamLabel1ParamEnter(Sender: TObject; href, value: String);
    procedure ParamLabel1ParamExit(Sender: TObject; href, value: String);
    procedure ToolButton12Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure ParamTreeview1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure ParamTreeview1Change(Sender: TObject; Node: TTreeNode);
    procedure ParamTreeview1DragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ParamTreeview1DragDrop(Sender, Source: TObject; X,
      Y: Integer);
  private
    { Private declarations }
    fFieldNames:tstringlist;
    procedure GetFontNames;

    procedure SetFieldNames(const Value: TStringList);
  public
    { Public declarations }
    constructor Create(aOwner:TComponent); override;
    destructor Destroy; override;
    procedure InsertTags(starttag,endtag:string);
    function HasTags(starttag,endtag:string):boolean;
    procedure RemoveTags(starttag,endtag:string);
    function ScanForward(starttag,endtag:string):boolean;
    function ScanBackward(starttag,endtag:string):boolean;
    procedure ChangeTagProp(starttag,endtag,propname,propval:string);
    procedure UpdateButtons;
    property FieldNames:TStringList read fFieldNames write SetFieldNames;
    function GetHTMLString:string;
  end;
var
  paramtreeeditor: Tparamtreeeditor;

implementation

{$R *.DFM}

{not case sensitive pos function}
function IPos(substr,s:string):integer;
begin
  Result := pos(uppercase(substr),uppercase(s));
end;

function PosFrom(selpos:integer; substr,s:string):integer;
begin
  Delete(s,1,selpos-1);
  Result := IPos(substr,s);
end;

procedure Tparamtreeeditor.Memo1Change(Sender: TObject);
begin
  if ToolButton9.Down then
    paramlabel1.HTMLText.Assign(memo1.lines);
end;

procedure Tparamtreeeditor.InsertTags(starttag,endtag:string);
var
  s:string;
  ss,sl:integer;
begin
  ss:=memo1.selstart+1;
  sl:=memo1.sellength;
  s:=memo1.lines.text;
  insert(starttag,s,ss);
  insert(endtag,s,ss+memo1.sellength+length(starttag));
  memo1.lines.text:=s;
  memo1.selstart:=ss-1+length(starttag);
  memo1.sellength:=sl;
end;

function Tparamtreeeditor.ScanBackward(starttag,endtag:string):boolean;
var
  i:integer;
  s:string;
begin
  i := memo1.selstart;
  s := memo1.lines.text;
  s := copy(s,1,i);
  Result := False;
  while (i >= 1) do
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

function Tparamtreeeditor.ScanForward(starttag,endtag:string):boolean;
var
  i:integer;
  s:string;
  st,et:integer;
begin
  i:=memo1.selstart+1;
  s:=memo1.lines.text;
  s:=copy(s,i,length(s));
  st:=IPos(starttag,s);
  et:=IPos(endtag,s);

  Result:=false;
  if (st=0) and (et>0) then result:=true;
  if (st>0) and (et>0) and (et<st) then result:=true;
end;

function Tparamtreeeditor.HasTags(starttag, endtag: string): boolean;
begin
  Result := ScanBackward(starttag, endtag) and ScanForward(starttag,endtag);
end;

procedure Tparamtreeeditor.RemoveTags(starttag, endtag: string);
var
  i,j,k:integer;
  s:string;
begin
  i := memo1.selstart;
  j := memo1.sellength;
  s := memo1.lines.text;
  s := copy(s,1,i);
  k := i;
  while (i >= 1) do
  begin
    if PosFrom(i,starttag,s)>PosFrom(i,endtag,s) then
    begin
      s:=memo1.lines.text;
      delete(s,i,length(starttag));
      if (pos('>',starttag)=0) then delete(s,i,posfrom(i,'>',s));
      break;
    end;
    dec(i);
  end;
  {first tag is removed}

  delete(s,PosFrom(i,endtag,s)+i-1,length(endtag));

  memo1.lines.text:=s;
  memo1.selstart:=k-length(starttag);
  memo1.sellength:=j;
end;

procedure Tparamtreeeditor.ChangeTagProp(starttag,endtag,propname,propval:string);
var
  i,j,k,l,pp:integer;
  s,tag:string;
begin
  i:=memo1.selstart;
  j:=memo1.sellength;
  s:=memo1.lines.text;
  k:=length(s); l:=i;
  s:=copy(s,1,i);

  tag:=starttag;

  while (i>=1) do
  begin
    if PosFrom(i,starttag,s)>PosFrom(i,endtag,s) then
    begin
      {found the tag here}
      s:=memo1.lines.text;
      tag:=copy(s,i,posfrom(i,'>',s));

      pp:=ipos(' '+propname,tag);

      if (pp>0) then
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
  memo1.lines.text:=s;
  memo1.selstart:=l+length(s)-k;
  memo1.sellength:=j;
end;

function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
begin
  TStrings(Data).Add(LogFont.lfFaceName);
  Result := 1;
end;

procedure Tparamtreeeditor.GetFontNames;
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

procedure Tparamtreeeditor.FormCreate(Sender: TObject);
begin
  GetFontNames;
end;

procedure Tparamtreeeditor.ToolButton1Click(Sender: TObject);
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

procedure Tparamtreeeditor.FontnameChange(Sender: TObject);
begin
  if HasTags('<FONT ','</FONT>') then
    ChangeTagProp('<FONT ','</FONT>','face',fontname.items[fontname.itemindex])
  else
    InsertTags('<FONT face="'+fontname.items[fontname.itemindex]+'">','</FONT>');
  memo1.setfocus;
end;

procedure Tparamtreeeditor.URLButtonClick(Sender: TObject);
var
  tag:string;
begin
  tag:='';
  if HasTags('<A ','</A>') then RemoveTags('<A ','</A>') else
  if InputQuery('Anchor', 'Anchor value',tag) then
    InsertTags('<A href="'+tag+'">','</A>');
  memo1.setfocus;
end;

procedure Tparamtreeeditor.UnderlineButtonClick(Sender: TObject);
begin
  if not HasTags('<U>','</U>') then InsertTags('<U>','</U>') else RemoveTags('<U>','</U>');
end;

procedure Tparamtreeeditor.BoldButtonClick(Sender: TObject);
begin
  if not HasTags('<B>','</B>') then InsertTags('<B>','</B>') else RemoveTags('<B>','</B>');
end;

procedure Tparamtreeeditor.ItalicButtonClick(Sender: TObject);
begin
  if not HasTags('<I>','</I>') then InsertTags('<I>','</I>') else RemoveTags('<I>','</I>');
end;

procedure Tparamtreeeditor.SuperscriptClick(Sender: TObject);
begin
  if not HasTags('<SUP>','</SUP>') then InsertTags('<SUP>','</SUP>') else RemoveTags('<SUP>','</SUP>');
end;

procedure Tparamtreeeditor.SubscriptClick(Sender: TObject);
begin
  if not HasTags('<SUB>','</SUB>') then InsertTags('<SUB>','</SUB>') else RemoveTags('<SUB>','</SUB>');
end;

procedure Tparamtreeeditor.ToolButton2Click(Sender: TObject);
begin
  InsertTags('<BR>','');
end;

procedure Tparamtreeeditor.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateButtons;
end;

procedure Tparamtreeeditor.Memo1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateButtons;
end;

procedure Tparamtreeeditor.UpdateButtons;
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

procedure Tparamtreeeditor.LeftAlignClick(Sender: TObject);
begin
  if HasTags('<P align="right">','</P>') then RemoveTags('<P align="right">','</P>');
  if HasTags('<P align="center">','</P>') then RemoveTags('<P align="center">','</P>');
  InsertTags('<P align="left">','</P>');
end;

procedure Tparamtreeeditor.CenterAlignClick(Sender: TObject);
begin
  if HasTags('<P align="right">','</P>') then RemoveTags('<P align="right">','</P>');
  if HasTags('<P align="left">','</P>') then RemoveTags('<P align="left">','</P>');
  InsertTags('<P align="center">','</P>');
end;

procedure Tparamtreeeditor.RightAlignClick(Sender: TObject);
begin
  if HasTags('<P align="center">','</P>') then RemoveTags('<P align="center">','</P>');
  if HasTags('<P align="left">','</P>') then RemoveTags('<P align="left">','</P>');
  InsertTags('<P align="right">','</P>');
end;


procedure Tparamtreeeditor.FontSizeChange(Sender: TObject);
begin
  if HasTags('<FONT ','</FONT>') then
    ChangeTagProp('<FONT ','</FONT>',' size',fontsize.items[fontsize.itemindex])
  else
    InsertTags('<FONT size="'+fontsize.items[fontsize.itemindex]+'">','</FONT>');
  memo1.setfocus;
end;

procedure Tparamtreeeditor.ToolButton3Click(Sender: TObject);
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

procedure Tparamtreeeditor.ToolButton5Click(Sender: TObject);
var
  tag:string;
begin
  tag:='0';
  if InputQuery('Set indent', 'Indent value',tag) then
    InsertTags('<IND x="'+tag+'">','');
end;

procedure Tparamtreeeditor.ToolButton4Click(Sender: TObject);
var
  tag:string;
begin
  tag:='0';
  if InputQuery('Image', 'Image reference',tag) then
    InsertTags('<IMG src="'+tag+'">','');
end;

procedure Tparamtreeeditor.ToolButton6Click(Sender: TObject);
begin
  if not HasTags('<BLINK>','</BLINK>') then
    InsertTags('<BLINK>','</BLINK>')
  else
    RemoveTags('<BLINK>','</BLINK>');
end;

procedure Tparamtreeeditor.ListButtonClick(Sender: TObject);
begin
  if memo1.SelLength=0 then InsertTags('<LI>','')
  else
  if not HasTags('<UL>','</UL>') then
    InsertTags('<UL>','</UL>')
  else
    RemoveTags('<UL>','</UL>');
end;

procedure Tparamtreeeditor.ToolButton8Click(Sender: TObject);
begin
  if not HasTags('<SHAD>','</SHAD>') then
    InsertTags('<SHAD>','</SHAD>')
  else
    RemoveTags('<SHAD>','</SHAD>');
end;

constructor Tparamtreeeditor.Create(aOwner: TComponent);
begin
  inherited;
  FFieldNames := TStringList.Create;
end;

destructor Tparamtreeeditor.Destroy;
begin
  FFieldNames.Free;
  inherited;
end;

procedure Tparamtreeeditor.SetFieldNames(const Value: TStringList);
begin
  FFieldNames.Assign(Value);
end;

procedure Tparamtreeeditor.dbfieldsClick(Sender: TObject);
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

procedure Tparamtreeeditor.PopupClick(Sender: TObject);
var
  s:string;
begin
  s:=(sender as TMenuItem).Caption;
  while (pos('&',s)>0) do delete(s,pos('&',s),1);
  InsertTags('<#'+s+'>','');
end;

procedure Tparamtreeeditor.AcceptBtnClick(Sender: TObject);
begin
  modalresult:=mrOk;
end;

procedure Tparamtreeeditor.CancelBtnClick(Sender: TObject);
begin
  modalresult:=mrCancel;
end;

function Tparamtreeeditor.GetHTMLString: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Memo1.Lines.Count do
  begin
    Result := Result + Memo1.Lines[i - 1];
  end;
end;

procedure Tparamtreeeditor.ToolButton9Click(Sender: TObject);
begin
  if (Sender as TToolButton).Down then
  begin
    paramlabel1.HTMLText.Assign(memo1.lines);
  end;
end;

procedure Tparamtreeeditor.ParamLabel1ParamEnter(Sender: TObject; href,
  value: String);
begin
  statusbar1.simpletext:=href;
end;

procedure Tparamtreeeditor.ParamLabel1ParamExit(Sender: TObject; href,
  value: String);
begin
  statusbar1.simpletext:='';
end;

procedure Tparamtreeeditor.ToolButton12Click(Sender: TObject);
var
  pinfo : TParamInfo;
  s,v,pt:string;
begin
  pinfo := TParamInfo.Create(self);
  try
    if pinfo.ShowModal = mrOk then
    begin
      if (pinfo.pid.Text = '') then
        showmessage('No ID specified for parameter')
      else
      begin
        case pinfo.ptype.itemindex of
        0:pt := 'EDIT';
        1:pt := 'SPIN';
        2:pt := 'MASK';
        3:pt := 'TIME';
        4:pt := 'DATE';
        5:pt := 'LIST';
        6:pt := 'MENU';
        7:pt := 'DIR';
        8:pt := 'QUERY';
        9:pt := 'CUSTOM';
        10:pt := '';
        11:pt := 'TOGGLE';
        end;

        s := 'href="'+pinfo.pid.text+'"';

        if pt <> '' then
          s := s + ' class="'+pt+'"';

        if pinfo.pval.text <> '' then
          v := pinfo.pval.Text
        else
          v := '?';  

        if pinfo.pextra.text <> '' then
          s := s + ' props="'+pinfo.pextra.Text+'"';

        if pinfo.phint.Text <> '' then
          s := s + ' hint="'+pinfo.phint.Text+'"';

        InsertTags('<a '+s+'>'+v+'</a>','');
        paramlabel1.HTMLText.Assign(memo1.lines);
      end;
    end;
  finally
    pinfo.Free;
  end;

end;

procedure Tparamtreeeditor.SpeedButton1Click(Sender: TObject);
begin
  paramtreeview1.Selected := Paramtreeview1.Items.Add(nil,'');
end;

procedure Tparamtreeeditor.SpeedButton2Click(Sender: TObject);
begin
  if Assigned(paramtreeview1.Selected) then
    paramtreeview1.Selected.Free;
end;

procedure Tparamtreeeditor.SpeedButton3Click(Sender: TObject);
begin
  if Assigned(ParamTreeView1.Selected) then
    paramtreeview1.Selected.Text := memo1.Lines.Text;
end;

procedure Tparamtreeeditor.ParamTreeview1Click(Sender: TObject);
begin
  if Assigned(ParamTreeView1.Selected) then
    memo1.Lines.Text := paramtreeview1.Selected.Text;
end;

procedure Tparamtreeeditor.SpeedButton4Click(Sender: TObject);
begin
  if Assigned(ParamTreeview1.Selected) then
  begin
    paramtreeview1.Selected.Expand(true);
    paramtreeview1.Selected := paramtreeview1.Items.AddChild(paramtreeview1.Selected,'');
  end
  else
    paramtreeview1.Selected := paramtreeview1.Items.Add(nil,'');
end;

procedure Tparamtreeeditor.ParamTreeview1Change(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node) then
  begin
    memo1.Lines.Text := Node.Text;
  end;
end;

procedure Tparamtreeeditor.ParamTreeview1DragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept :=  Assigned(ParamTreeView1.GetNodeAt(x,y));
end;

procedure Tparamtreeeditor.ParamTreeview1DragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  s:string;
  node: ttreenode;
begin
  if Assigned(ParamTreeview1.GetNodeAt(x,y)) then
  begin
     s := ParamTreeview1.Selected.Text;
     node := ParamTreeview1.GetNodeAt(x,y);
     ParamTreeview1.Selected.Text := node.Text;
     node.Text := s;
     ParamTreeview1.Selected := Node;
  end;

end;

end.