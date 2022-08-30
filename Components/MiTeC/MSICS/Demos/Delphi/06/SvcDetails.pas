unit SvcDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ImgList, MSI_SvcListMon;

type
  Tdlg_SvcDetails = class(TForm)
    Bevel1: TBevel;
    bClose: TButton;
    pc: TPageControl;
    imgIcon: TImage;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    GenList: TListView;
    eName: TEdit;
    Dep1Tree: TTreeView;
    Label1: TLabel;
    l: TLabel;
    Label2: TLabel;
    Dep2Tree: TTreeView;
    ImageList1: TImageList;
    procedure GenListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure GenListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure Dep1TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Dep2TreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
  private
    FMon: TSvcListMonThread;
    FRecord: TServiceRecord;
    sl: TStringList;
  public
    procedure RefreshData;
  end;

procedure ShowSvcDetails(ARecord: TServiceRecord; AMonitor: TSvcListMonThread);

var
  dlg_SvcDetails: Tdlg_SvcDetails;

implementation

uses MiTeC_CtrlRtns, MiTeC_AdvAPI, MiTeC_Routines;

{$R *.dfm}

procedure ShowSvcDetails;
begin
  with Tdlg_SvcDetails.Create(Application.Mainform) do
    try
      FRecord:=ARecord;
      FMon:=AMonitor;

      RefreshData;

      ShowModal;

    finally
      Free;
    end;
end;

procedure Tdlg_SvcDetails.GenListAdvancedCustomDrawSubItem(
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

procedure Tdlg_SvcDetails.GenListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  DefaultDraw:=True;
  with TListView(Sender) do begin
    Canvas.Font.Style:=[];
    Canvas.Font.Color:=Font.Color;
    Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
    if not HotTrack or not(htUnderlineCold in HotTrackStyles) then
      Canvas.Font.Style:=Font.Style;
    if (cdsHot in State) and HotTrack then begin
      if htUnderlineHot in HotTrackStyles then
        Canvas.Font.Style:=Canvas.Font.Style+[fsUnderline];
      Canvas.Font.Color:=clHotLight;
    end;
    if Item.ImageIndex=-3 then
      Canvas.Font.Style:=[fsBold];
    if Item.ImageIndex=-2 then
      ListView_DrawLine(Sender,Item,State,DefaultDraw,clSilver);
  end;
  {$IFDEF THEMESUPPORT}
  if UseThemes then
    SetBkMode(Sender.Canvas.Handle, TRANSPARENT);
  {$ENDIF}
end;

procedure Tdlg_SvcDetails.RefreshData;
var
  i: Integer;
  n: TTreeNode;
  r: TServiceRecord;
begin
  eName.Text:=FRecord.DisplayName;

  GenList.Items.Clear;
  imgIcon.Picture.Icon.Handle:=GetFileIcon(FRecord.ImageName);

  GenList.Items.Clear;
  with GenList.Items.Add do begin
    Caption:='Description';
    SubItems.Add(FRecord.VersionInfo.Description);
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Version';
    SubItems.Add(FRecord.VersionInfo.FileVersion);
  end;
  with GenList.Items.Add do begin
    Caption:='Product Name';
    SubItems.Add(FRecord.VersionInfo.ProductName);
  end;
  with GenList.Items.Add do begin
    Caption:='Company Name';
    SubItems.Add(FRecord.VersionInfo.CompanyName);
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;

  with GenList.Items.Add do begin
    Caption:='Service Name';
    SubItems.Add(FRecord.Name);
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Command line';
    SubItems.Add(FRecord.CmdLine);
  end;
  with GenList.Items.Add do begin
    Caption:='Log On As ';
    SubItems.Add(FRecord.ObjectName);
  end;
  with GenList.Items.Add do begin
    Caption:='Load Order Group';
    SubItems.Add(FRecord.Group);
  end;
  with GenList.Items.Add do begin
    Caption:='Type';
    SubItems.Add(cSvcType[FRecord.Typ]);
  end;
  with GenList.Items.Add do begin
    Caption:='';
    ImageIndex:=-2;
  end;
  with GenList.Items.Add do begin
    Caption:='Status';
    SubItems.Add(cSvcStatus[FRecord.Status]);
    ImageIndex:=-3;
  end;
  with GenList.Items.Add do begin
    Caption:='Startup Mode';
    SubItems.Add(cSvcStartup[FRecord.Startup]);
  end;
  with GenList.Items.Add do begin
    Caption:='Error Control';
    SubItems.Add(cSvcErrorControl[FRecord.ErrCtrl]);
  end;

  Dep1Tree.Items.Clear;
  sl.CommaText:=FRecord.DependOnService;
  for i:=0 to sl.Count-1 do begin
    FMon.GetRecordByName(sl[i],r);
    if r.Name<>'' then begin
      n:=Dep1Tree.Items.AddChild(nil,r.DisplayName);
      n.ImageIndex:=0;
      n.HasChildren:=True;
    end;
  end;
  Dep1Tree.AlphaSort;

  Dep2Tree.Items.Clear;
  FMon.GetServiceDependants(FRecord.Name,sl);
  for i:=0 to sl.Count-1 do begin
    n:=Dep2Tree.Items.AddChild(nil,sl.ValueFromIndex[i]);
    n.ImageIndex:=0;
    n.HasChildren:=True;
  end;
  Dep2Tree.AlphaSort;
end;

procedure Tdlg_SvcDetails.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=TabSheet1;
  sl:=TStringList.Create;
end;

procedure Tdlg_SvcDetails.Dep1TreeExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  i: Integer;
  n: TTreeNode;
  r: TServiceRecord;
begin
  if Node.GetFirstChild<>nil then
    Exit;
  FMon.GetRecordByDisplayName(Node.Text,r);
  if r.Name<>'' then begin
    sl.CommaText:=r.DependOnService;
    for i:=0 to sl.Count-1 do begin
      FMon.GetRecordByName(sl[i],r);
      if r.Name<>'' then begin
        n:=TTreeView(Sender).Items.AddChild(Node,r.DisplayName);
        n.ImageIndex:=0;
        n.HasChildren:=True;
      end;
    end;
    Node.AlphaSort;
  end;
  Node.HasChildren:=(Node.getFirstChild<>nil);
end;

procedure Tdlg_SvcDetails.FormDestroy(Sender: TObject);
begin
  sl.Free;
end;

procedure Tdlg_SvcDetails.Dep2TreeExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
  i: Integer;
  n: TTreeNode;
  r: TServiceRecord;
begin
  if Node.GetFirstChild<>nil then
    Exit;
  FMon.GetRecordByDisplayName(Node.Text,r);
  if r.Name<>'' then begin
    FMon.GetServiceDependants(r.Name,sl);
    for i:=0 to sl.Count-1 do begin
      n:=TTreeView(Sender).Items.AddChild(Node,sl.ValueFromIndex[i]);
      n.ImageIndex:=0;
      n.HasChildren:=True;
    end;
    Node.AlphaSort;
  end;
  Node.HasChildren:=(Node.getFirstChild<>nil);
end;

end.
