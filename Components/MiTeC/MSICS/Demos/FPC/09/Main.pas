unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, MSI_DiskMonitor, Menus, StdCtrls, ExtCtrls;

type

  { Twnd_dm_Main }

  Twnd_dm_Main = class(TForm)
    sb: TStatusBar;
    List: TListView;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    mmSetdirectory: TMenuItem;
    N1: TMenuItem;
    mmActive: TMenuItem;
    N2: TMenuItem;
    mmExit: TMenuItem;
    Filter1: TMenuItem;
    Actions1: TMenuItem;
    Notifications1: TMenuItem;
    Add1: TMenuItem;
    Remove1: TMenuItem;
    Modify1: TMenuItem;
    RenameOld1: TMenuItem;
    RenameNew1: TMenuItem;
    Filename1: TMenuItem;
    Directoryname1: TMenuItem;
    Attributes1: TMenuItem;
    Size1: TMenuItem;
    Lastwrite1: TMenuItem;
    Lastaccess1: TMenuItem;
    Creation1: TMenuItem;
    Security1: TMenuItem;
    Clearlog1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Panel3: TPanel;
    Label2: TLabel;
    Panel4: TPanel;
    Label3: TLabel;
    PopupMenu: TPopupMenu;
    pmProps: TMenuItem;
    N3: TMenuItem;
    mmProps: TMenuItem;
    N4: TMenuItem;
    mmWS: TMenuItem;
    procedure ListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure mmWSClick(Sender: TObject);
    procedure DiskMonChange(Sender: TObject; Action: TWatchAction;
      FileName: String);
    procedure mmSetdirectoryClick(Sender: TObject);
    procedure mmActiveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Clearlog1Click(Sender: TObject);
    procedure mmExitClick(Sender: TObject);
    procedure cmAction(Sender: TObject);
    procedure cmNotify(Sender: TObject);
    procedure ListAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure cmProps(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
  private
    DiskMon: TMiTeC_DiskMonitor;
  public
    procedure SetControls;
  end;

var
  wnd_dm_Main: Twnd_dm_Main;

implementation

uses MiTeC_Dialogs;

{$R *.lfm}

procedure Twnd_dm_Main.DiskMonChange(Sender: TObject; Action: TWatchAction;
  FileName: String);
var
  FI: TSearchrec;
begin
  FileName:=IncludeTrailingPathdelimiter(DiskMon.Directory)+FileName;
  FindFirst(FileName,faAnyFile,FI);
  with List.Items.Insert(0) do begin
    Caption:=FormatDateTime('yyyy-mm-dd hh.nn.ss',now);
    case Action of
      waAdd: SubItems.Add('Add');
      waRemove: SubItems.Add('Remove');
      waModify: SubItems.Add('Modify');
      waRenameOld: SubItems.Add('Rename (Old)');
      waRenameNew: SubItems.Add('Rename (New)');
    end;
    SubItems.Add(ExtractFileName(FileName));
    SubItems.Add(ExtractFilePath(FileName));
    if (Action=waRemove) then
      ImageIndex:=2
    else begin
      if (FI.Attr and faDirectory)<>0 then begin
        SubItems.Add('Folder');
        ImageIndex:=0;
      end else begin
        SubItems.Add(Format('%d',[FI.Size]));
        ImageIndex:=1;
      end;
      try
        SubItems.Add(FormatDateTime('yyyy-mm-dd hh.nn.ss',FileDateToDateTime(FI.Time)));
      except
        SubItems.Add('');
      end;
      SubItems.Add(Format('0x%x',[FI.Attr]));
    end;
  end;
  FindClose(FI);
  sb.Panels[0].Text:=Format('Event count: %d',[List.Items.Count]);
end;

procedure Twnd_dm_Main.mmSetdirectoryClick(Sender: TObject);
var
  dma: boolean;
  s: string;
begin
  dma:=DiskMon.Active;
  s:=DiskMon.Directory;
  if SelectDirectory('Select directory to watch','',s) then begin
    DiskMon.Active:=False;
    DiskMon.Directory:=s;
    Caption:=Format('Disk Monitor - [%s]',[DiskMon.Directory]);
    try
      DiskMon.Active:=dma;
    except
      on e: Exception do begin
        MessageDlg(e.Message,mtError,[mbOK],0);
        DiskMon.Active:=False;
      end;
    end;
  end;
end;

procedure Twnd_dm_Main.mmWSClick(Sender: TObject);
var
  dma: Boolean;
begin
  dma:=DiskMon.Active;
  DiskMon.Active:=False;
  DiskMon.WatchSubtree:=TMenuItem(Sender).Checked;
  try
    DiskMon.Active:=dma;
  except
    on e: Exception do begin
      MessageDlg(e.Message,mtError,[mbOK],0);
      DiskMon.Active:=False;
    end;
  end;
  SetControls;
end;

procedure Twnd_dm_Main.ListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  SetControls;
end;

procedure Twnd_dm_Main.mmActiveClick(Sender: TObject);
begin
  try
    DiskMon.Active:=not DiskMon.Active;
  except
    on e: Exception do begin
      MessageDlg(e.Message,mtError,[mbOK],0);
      DiskMon.Active:=False;
    end;
  end;
  SetControls;
end;

procedure Twnd_dm_Main.FormCreate(Sender: TObject);
begin
  DiskMon := TMiTeC_DiskMonitor.Create(Self);
  with DiskMon do begin
    Active:=False;
    Directory:='c:\';
    WatchSubtree:=True;
    WatchFilter:=[wfFilename, wfDirname, wfAttrs, wfSize, wfLastWrite, wfLastAccess, wfCreation, wfSecurity];
    WatchAction:=[waAdd, waRemove, waModify, waRenameOld, waRenameNew];
    OnChange:=DiskMonChange;
  end;
  SetControls;

  DiskMon.Active:=True;
  Caption:=Format('Disk Monitor - [%s]',[DiskMon.Directory]);
end;

procedure Twnd_dm_Main.Clearlog1Click(Sender: TObject);
begin
  if MessageDlg('Clear log?',mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
    List.Items.Clear;
    sb.Panels[0].Text:=Format('Event count: %d',[List.Items.Count]);
  end;
end;

procedure Twnd_dm_Main.mmExitClick(Sender: TObject);
begin
  Close;
end;

procedure Twnd_dm_Main.cmAction(Sender: TObject);
var
  dma: Boolean;
begin
  dma:=DiskMon.Active;
  DiskMon.Active:=False;
  if TMenuItem(Sender).Checked then
    DiskMon.WatchAction:=DiskMon.WatchAction+[TWatchAction(TMenuItem(Sender).Tag)]
  else
    DiskMon.WatchAction:=DiskMon.WatchAction-[TWatchAction(TMenuItem(Sender).Tag)];
  try
    DiskMon.Active:=dma;
  except
    on e: Exception do begin
      MessageDlg(e.Message,mtError,[mbOK],0);
      DiskMon.Active:=False;
    end;
  end;
  SetControls;
end;

procedure Twnd_dm_Main.cmNotify(Sender: TObject);
var
  dma: Boolean;
begin
  dma:=DiskMon.Active;
  DiskMon.Active:=False;
  if TMenuItem(Sender).Checked then
    DiskMon.WatchFilter:=DiskMon.WatchFilter+[TWatchFilter(TMenuItem(Sender).Tag)]
  else
    DiskMon.WatchFilter:=DiskMon.WatchFilter-[TWatchFilter(TMenuItem(Sender).Tag)];
  try
    DiskMon.Active:=dma;
  except
    on e: Exception do begin
      MessageDlg(e.Message,mtError,[mbOK],0);
      DiskMon.Active:=False;
    end;
  end;
end;

procedure Twnd_dm_Main.ListAdvancedCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
begin
  case Item.ImageIndex of
    0: Sender.Canvas.Font.Color:=clGreen;
    1: Sender.Canvas.Font.Color:=clBlue;
    2: begin
      Sender.Canvas.Font.Color:=clRed;
      //Sender.Canvas.Font.Style:=[fsBold];
    end;
  end;
end;

procedure Twnd_dm_Main.SetControls;
begin
  mmActive.Checked:=DiskMon.Active;
  mmProps.Enabled:=Assigned(List.Selected) and (List.Selected.ImageIndex<>2);
  mmWS.Checked:=DiskMon.WatchSubtree;
end;

procedure Twnd_dm_Main.cmProps(Sender: TObject);
begin
  if Assigned(List.Selected) and (List.Selected.ImageIndex<>2) then
    ShellPropDlg(Handle,List.Selected.SubItems[2]+List.Selected.SubItems[1]);
end;

procedure Twnd_dm_Main.PopupMenuPopup(Sender: TObject);
begin
  pmProps.Enabled:=Assigned(List.Selected) and (List.Selected.ImageIndex<>2);
end;

end.
