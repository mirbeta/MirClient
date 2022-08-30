unit SMDetails;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, MSI_SysModListMon, ImgList;

type
  Tdlg_SMDetails = class(TForm)
    Bevel1: TBevel;
    bClose: TButton;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    GenList: TListView;
    imgIcon: TImage;
    TabSheet6: TTabSheet;
    HList: TListView;
    ImageList: TImageList;
    eName: TEdit;
    procedure ListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure ListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure ListColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure GenListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure GenListAdvancedCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FRecord: TSystemModuleRecord;
  public
    procedure RefreshData;
  end;

procedure ShowDrvDetails(ARecord: TSystemModuleRecord);

var
  dlg_SMDetails: Tdlg_SMDetails;

implementation

uses
  MiTeC_CtrlRtns, MiTeC_Datetime, MiTeC_Routines, MiTeC_Dialogs, MiTeC_NativeDefs,
  MiTeC_NativeAPI, WinDetails;

{$R *.dfm}

procedure ShowDrvDetails;
begin
  with Tdlg_SMDetails.Create(Application.Mainform) do
    try
      FRecord:=ARecord;

      RefreshData;

      ShowModal;
    finally
      Free;
    end;
end;

procedure Tdlg_SMDetails.RefreshData;
begin
  eName.Text:=FRecord.Name;

  imgIcon.Picture.Icon.Handle:=GetFileIcon(FRecord.Name);
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
end;

procedure Tdlg_SMDetails.ListAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Tdlg_SMDetails.ListAdvancedCustomDrawSubItem(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  if Abs(TListView(Sender).Tag)=SubItem+1 then
    Sender.Canvas.Brush.Color:=clInfoBk
  else
    Sender.Canvas.Brush.Color:=clWhite
end;

procedure Tdlg_SMDetails.ListColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  TListView(Sender).SortType:=stNone;
  if Column.Index+1<>abs(TListView(Sender).Tag) then
    TListView(Sender).Tag:=Column.Index+1
  else
    TListView(Sender).Tag:=-TListView(Sender).Tag;
  TListView(Sender).SortType:=stText;
end;

procedure Tdlg_SMDetails.ListCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=ListView_CustomSort(Item1,Item2,abs(TListView(Sender).Tag)-1);
  if TListView(Sender).Tag<0 then
    Compare:=-Compare;
end;

procedure Tdlg_SMDetails.GenListAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
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

procedure Tdlg_SMDetails.GenListAdvancedCustomDrawSubItem(
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

procedure Tdlg_SMDetails.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=TabSheet1;
end;

end.
