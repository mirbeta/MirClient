unit WinDetails;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, MiTeC_Routines;

type
  Tdlg_WinDetails = class(TForm)
    Bevel1: TBevel;
    bClose: TButton;
    pc: TPageControl;
    imgIcon: TImage;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GenList: TListView;
    eName: TEdit;
    CIList: TListView;
    SList: TListView;
    procedure GenListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure GenListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FWI: TWindowInfo;
  public
    procedure RefreshData;
  end;

procedure ShowWinDetails(AWI: TWindowInfo);

var
  dlg_WinDetails: Tdlg_WinDetails;

implementation

uses MiTeC_CtrlRtns;

{$R *.lfm}

procedure ShowWinDetails;
begin
  with Tdlg_WinDetails.Create(Application.Mainform) do
    try
      FWI:=AWI;

      RefreshData;

      ShowModal;

    finally
      Free;
    end;
end;

procedure Tdlg_WinDetails.GenListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    Canvas.Font.Color:=clBlack;
    if Item.ImageIndex=-4 then
      ListView_DrawCheckBox(Sender,Item,SubItem,State,DefaultDraw,'1')
    else
      Canvas.Brush.Color:=Color;
    if cdsHot in State then
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline]
    else
      Canvas.Font.Style:=Canvas.Font.Style-[fsUnderline];
  end;
end;

procedure Tdlg_WinDetails.GenListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    Canvas.Font.Color:=clBlack;
    if cdsHot in State then
      Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline]
    else
      Canvas.Font.Style:=Canvas.Font.Style-[fsUnderline];
    if Item.ImageIndex=-2 then
      ListView_DrawLine(Sender,Item,State,DefaultDraw,clGray);
  end;
end;

procedure Tdlg_WinDetails.RefreshData;
var
  i: Integer;
begin
  eName.Text:=Format('"%s": %s',[FWI.Text,FWI.ClassName]);
  if FWI.Icon>0 then
    imgIcon.Picture.Icon.Handle:=FWI.Icon;

  GenList.Items.Clear;
  with GenList.Items.Add do begin
    Caption:='Handle';
    SubItems.Add(Format('0x%x',[FWI.Handle]));
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Text';
    SubItems.Add(FWI.Text);
  end;
  with GenList.Items.Add do begin
    Caption:='Process';
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      SubItems.Add(Format('%d',[FWI.Process]))
    else
      SubItems.Add(Format('0x%x',[FWI.Process]));
  end;
  with GenList.Items.Add do begin
    Caption:='Thread';
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      SubItems.Add(Format('%d',[FWI.Thread]))
    else
      SubItems.Add(Format('0x%x',[FWI.Thread]));
  end;
  with GenList.Items.Add do begin
    Caption:='Parent Windows';
    SubItems.Add(Format('0x%x',[FWI.ParentWin]));
  end;
  with GenList.Items.Add do begin
    Caption:='Window Procedure';
    SubItems.Add(Format('0x%x',[FWI.WndProc]));
  end;
  with GenList.Items.Add do begin
    Caption:='Instance Handle';
    SubItems.Add(Format('0x%x',[FWI.Instance]));
  end;
  with GenList.Items.Add do begin
    Caption:='Identifier (ID)';
    SubItems.Add(Format('0x%x',[FWI.ID]));
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with GenList.Items.Add do begin
    Caption:='Rectangle';
    SubItems.Add(Format('(%d,%d)-(%d,%d), %dx%d',[FWI.Rect.Left,FWI.Rect.Top,FWI.Rect.Right,FWI.Rect.Bottom,
                                                  FWI.Rect.Right-FWI.Rect.Left,FWI.Rect.Bottom-FWI.Rect.Top]));
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Client Rectangle';
    SubItems.Add(Format('(%d,%d)-(%d,%d), %dx%d',[FWI.ClientRect.Left,FWI.ClientRect.Top,FWI.ClientRect.Right,FWI.ClientRect.Bottom,
                                                  FWI.ClientRect.Right-FWI.ClientRect.Left,FWI.ClientRect.Bottom-FWI.ClientRect.Top]));
  end;
  with GenList.Items.Add do begin
    Caption:='User Data';
    SubItems.Add(Format('0x%x',[FWI.UserData]));
  end;

  CIList.Items.Clear;
  with CIList.Items.Add do begin
    Caption:='Class Name';
    SubItems.Add(FWI.ClassName);
    ImageIndex:=-3;
  end;
  with CIList.Items.Add do begin
    Caption:='Class Atom';
    SubItems.Add(Format('0x%x',[FWI.Atom]));
  end;
  with CIList.Items.Add do begin
    Caption:='Class Bytes';
    SubItems.Add(Format('0x%x',[FWI.ClassBytes]));
  end;
  with CIList.Items.Add do begin
    Caption:='Win Bytes';
    SubItems.Add(Format('0x%x',[FWI.WinBytes]));
  end;
  with CIList.Items.Add do begin
    Caption:='Window Procedure';
    SubItems.Add(Format('0x%x',[FWI.ClassWndProc]));
  end;
  with CIList.Items.Add do begin
    Caption:='Instance Handle';
    SubItems.Add(Format('0x%x',[FWI.ClassInstance]));
  end;
  with CIList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with CIList.Items.Add do begin
    Caption:='Background Brush';
    SubItems.Add(Format('0x%x',[FWI.Background]));
    ImageIndex:=-3;
  end;
  with CIList.Items.Add do begin
    Caption:='Cursor Handle';
    SubItems.Add(Format('0x%x',[FWI.Cursor]));
  end;
  with CIList.Items.Add do begin
    Caption:='Icon Handle';
    SubItems.Add(Format('0x%x',[FWI.Icon]));
  end;
  with CIList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with CIList.Items.Add do begin
    Caption:='Class Styles';
    SubItems.Add(Format('0x%x',[FWI.ClassStyle]));
    ImageIndex:=-3;
  end;
  if Assigned(FWI.ClassStyles) then
    for i:=0 to FWI.ClassStyles.Count-1 do
      with CIList.Items.Add do begin
        Caption:=FWI.ClassStyles[i];
        SubItems.Add('1');
        ImageIndex:=-4;
      end;

  SList.Items.Clear;
  with SList.Items.Add do begin
    Caption:='Styles';
    SubItems.Add(Format('0x%x',[FWI.Style]));
    ImageIndex:=-3;
  end;
  if Assigned(FWI.Styles) then
    for i:=0 to FWI.Styles.Count-1 do
      with SList.Items.Add do begin
        Caption:=FWI.Styles[i];
        SubItems.Add('1');
        ImageIndex:=-4;
      end;
  with SList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with SList.Items.Add do begin
    Caption:='Extra Styles';
    SubItems.Add(Format('0x%x',[FWI.ExStyle]));
    ImageIndex:=-3;
  end;
  if Assigned(FWI.ExStyles) then
    for i:=0 to FWI.ExStyles.Count-1 do
      with SList.Items.Add do begin
        Caption:=FWI.ExStyles[i];
        SubItems.Add('1');
        ImageIndex:=-4;
      end;
end;

procedure Tdlg_WinDetails.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=TabSheet1;
end;

end.
