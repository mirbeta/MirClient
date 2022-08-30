unit DetailDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, ComCtrls;

type
  Tdlg_db_Detail = class(TForm)
    ButtonPanel: TPanel;
    Panel: TPanel;
    bOK: TButton;
    ClientPanel: TPanel;
    lv: TListView;
    Icon: TImage;
    eName: TEdit;
    procedure clbClickCheck(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lvAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure lvDblClick(Sender: TObject);
  private
  public
  end;

var
  dlg_db_Detail: Tdlg_db_Detail;

implementation

uses MiTeC_CtrlRtns, TypInfo;

{$R *.DFM}

procedure Tdlg_db_Detail.clbClickCheck(Sender: TObject);
var
  OCC: TNotifyEvent;
  idx: integer;
  p: TPoint;
begin
  with TCheckListBox(Sender) do begin
    OCC:=OnClickCheck;
    OnClickCheck:=nil;
    GetCursorPos(p);
    p:=ScreenToClient(p);
    idx:=ItemAtPos(p,True);
    if idx>-1 then
      Checked[idx]:=not Checked[idx];
    OnClickCheck:=OCC;
  end;
end;

procedure Tdlg_db_Detail.FormActivate(Sender: TObject);
begin
  lv.Width:=0;
end;

procedure Tdlg_db_Detail.lvAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    Canvas.Font.Color:=clBlack;
    if cdsHot in State then begin
      Canvas.Font.Color:=clBlue;
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderLine];
    end else
      Canvas.Font.Style:=Canvas.Font.Style-[fsUnderLine];
    if Item.ImageIndex=-2 then
      ListView_DrawLine(Sender,Item,State,DefaultDraw,clGray);
  end;
end;

procedure Tdlg_db_Detail.lvAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    Canvas.Font.Color:=clBlack;
    if Item.ImageIndex=-4 then
      ListView_DrawCheckBox(Sender,Item,SubItem,State,DefaultDraw,'1')
    else
      Canvas.Brush.Color:=Color;
    if cdsHot in State then begin
      Canvas.Font.Color:=clBlue;
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline]
    end else
      Canvas.Font.Style:=Canvas.Font.Style-[fsUnderline];
  end;
end;

procedure Tdlg_db_Detail.lvDblClick(Sender: TObject);
var
  s: string;
begin
  if Assigned(lv.Selected) then begin
    s:=lv.Selected.SubItems[0];
    InputQuery(lv.Selected.Caption,'',s);
  end;
end;

end.
