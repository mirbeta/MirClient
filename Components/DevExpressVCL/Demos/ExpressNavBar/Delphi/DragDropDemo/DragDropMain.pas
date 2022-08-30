unit DragDropMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ExtCtrls, StdCtrls, ImgList,
  dxNavBarCollns, dxNavBarBase, dxNavBar, ActnList, Menus, NavBarUtils;

type
  TfmDragDropMain = class(TForm)
    nbMain: TdxNavBar;
    Label1: TLabel;
    lvComponents: TListView;
    Label2: TLabel;
    Panel1: TPanel;
    imRecycleBin: TImage;
    Label3: TLabel;
    bgStandard: TdxNavBarGroup;
    bgSystem: TdxNavBarGroup;
    bgDX: TdxNavBarGroup;
    bgTemp: TdxNavBarGroup;
    iComponents: TImageList;
    biLabel: TdxNavBarItem;
    biEdit: TdxNavBarItem;
    biButton: TdxNavBarItem;
    biCheckBox: TdxNavBarItem;
    biRadioButton: TdxNavBarItem;
    biGroupBox: TdxNavBarItem;
    biPanel: TdxNavBarItem;
    biImage: TdxNavBarItem;
    biMainMenu: TdxNavBarItem;
    biTimer: TdxNavBarItem;
    biGrid: TdxNavBarItem;
    biTreeList: TdxNavBarItem;
    biBarManager: TdxNavBarItem;
    biDBNavigatorBar: TdxNavBarItem;
    biCalcEdit: TdxNavBarItem;
    biButtonEdit: TdxNavBarItem;
    biNavBar: TdxNavBarItem;
    mmMain: TMainMenu;
    miFile: TMenuItem;
    miExit: TMenuItem;
    miDragDropOptions: TMenuItem;
    miHelp: TMenuItem;
    miProducts: TMenuItem;
    miDownloads: TMenuItem;
    miSupport: TMenuItem;
    miDeveloperExpressontheweb: TMenuItem;
    alMain: TActionList;
    actDragLink: TAction;
    actDropLink: TAction;
    actDragGroup: TAction;
    actDropGroup: TAction;
    actSelectLinks: TAction;
    miAllowDragLink: TMenuItem;
    miAllowDropLink: TMenuItem;
    miAllowDragGroup: TMenuItem;
    miAllowDropGroup: TMenuItem;
    miDragAndDrop: TMenuItem;
    SelectLinks1: TMenuItem;
    procedure imRecycleBinDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure nbMainEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure actDragDropOptionExecute(Sender: TObject);
    procedure actSelectLinksExecute(Sender: TObject);
    procedure lvComponentsStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure lvComponentsEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmDragDropMain: TfmDragDropMain;

implementation

uses
  ShellAPI;

{$R *.res}
{$R *.dfm}

procedure TfmDragDropMain.imRecycleBinDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if (dxNavBarDragObject <> nil) and ((dxNavBarDragObject.SourceLink <> nil) or
    (dxNavBarDragObject.SourceGroup <> nil)) then
  begin
    Accept := True;
    if State = dsDragEnter then
      imRecycleBin.Picture.Bitmap.LoadFromResourceName(HInstance, 'RECYCLE2')
    else if State = dsDragLeave then
      imRecycleBin.Picture.Bitmap.LoadFromResourceName(HInstance, 'RECYCLE1');
  end
  else Accept := False;
end;

procedure TfmDragDropMain.nbMainEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target is TImage then
  begin
    if dxNavBarDragObject.SourceGroup <> nil then
      nbMain.Groups.Delete(dxNavBarDragObject.SourceGroup.Index);
    if dxNavBarDragObject.SourceLink <> nil then
      dxNavBarDragObject.SourceLink.Group.RemoveLink(dxNavBarDragObject.SourceLink.Index);
    imRecycleBin.Picture.Bitmap.LoadFromResourceName(HInstance, 'RECYCLE1');
  end;
end;

procedure TfmDragDropMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to nbMain.Items.Count - 1 do
    with lvComponents.Items.Add do
    begin
      Caption := nbMain.Items[I].Caption;
      ImageIndex := nbMain.Items[I].SmallImageIndex;
      Data := nbMain.Items[I];
    end;
  actDragLink.Checked := fAllowDragLink in nbMain.DragDropFlags;
  actDragGroup.Checked := fAllowDragGroup in nbMain.DragDropFlags;
  actDropLink.Checked := fAllowDropLink in nbMain.DragDropFlags;
  actDropGroup.Checked := fAllowDropGroup in nbMain.DragDropFlags;
  actSelectLinks.Checked := nbMain.AllowSelectLinks;
end;

procedure TfmDragDropMain.actDragDropOptionExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  if TAction(Sender).Checked then
    nbMain.DragDropFlags := nbMain.DragDropFlags +
      [TdxNavBarDragDropFlag(TAction(Sender).Tag)]
  else
    nbMain.DragDropFlags := nbMain.DragDropFlags -
      [TdxNavBarDragDropFlag(TAction(Sender).Tag)];
end;

procedure TfmDragDropMain.actSelectLinksExecute(Sender: TObject);
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  nbMain.AllowSelectLinks := TAction(Sender).Checked;
end;

procedure TfmDragDropMain.lvComponentsStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if lvComponents.Selected <> nil then
    dxNavBarDragObject := TdxNavBarDragObject.Create(nbMain, DragObject, nil, nil,
      TdxNavBarItem(lvComponents.Selected.Data));
end;

procedure TfmDragDropMain.lvComponentsEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  dxNavBarDragObject.Free;
  dxNavBarDragObject := nil;
end;

end.

