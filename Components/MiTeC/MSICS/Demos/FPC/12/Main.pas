unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MiTeC_EventLogNT, MSI_EventLog, ComCtrls, StdCtrls, ImgList, ExtCtrls;

type
  TForm1 = class(TForm)
    cb: TComboBox;
    lv: TListView;
    EventImages: TImageList;
    Memo: TMemo;
    Panel1: TPanel;
    bSave: TButton;
    bLoad: TButton;
    od: TOpenDialog;
    sd: TSaveDialog;
    bAction: TButton;
    eFilter: TEdit;
    cbxExpand: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cbChange(Sender: TObject);
    procedure lvSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure bSaveClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure cmRefresh(Sender: TObject);
    procedure ELReadEventLog(Sender: TObject; ARecord: TLogRecord; var Cancel: Boolean);
    procedure cmCancel(Sender: TObject);
  private
    FCancel: boolean;
    EL: TMiTeC_EventLog;
    et: comp;
  public
  end;

var
  Form1: TForm1;

implementation

uses MiTeC_Routines;

{$R *.lfm}

procedure TForm1.bLoadClick(Sender: TObject);
var
  i: Integer;
  h: Boolean;
begin
  if not od.Execute then
    Exit;
  cb.Items.Clear;
  lv.Items.Clear;
  h:=True;
  EL.LoadFromStorage(od.FileName,h);
  for i:=0 to EL.ContainerCount-1 do
    cb.Items.Add(EL.Containers[i].Name);
end;

procedure TForm1.bSaveClick(Sender: TObject);
var
  i: Integer;
  h: Boolean;
begin
  sd.FileName:=Format('%s.sif',[Machinename]);
  if not sd.Execute then
    Exit;
  EL.SourceFilter:='';
  h:=True;
  for i:=0 to EL.ContainerCount-1 do
    try
      EL.SourceName:=EL.Containers[i].Name;
      EL.SaveToStorage(sd.FileName,h);
    except
    end;
end;

procedure TForm1.cmCancel(Sender: TObject);
begin
  FCancel:=True;
end;

procedure TForm1.cmRefresh(Sender: TObject);
var
  i: Integer;
begin
  cb.Items.Clear;
  lv.Items.Clear;
  EL.RefreshData;
  for i:=0 to EL.ContainerCount-1 do
    cb.Items.Add(EL.Containers[i].Name);
end;

procedure TForm1.cbChange(Sender: TObject);
var
  i: Integer;
  h: Boolean;
begin
  Memo.Lines.Clear;
  if cb.ItemIndex=-1 then
    Exit;

  bAction.Caption:='Cancel';
  bAction.OnClick:=cmCancel;
  bLoad.Enabled:=False;
  bSave.Enabled:=False;
  cbxExpand.Enabled:=False;
  cb.Enabled:=False;
  eFilter.Enabled:=False;
  lv.Enabled:=False;
  Memo.Enabled:=False;
  FCancel:=False;
  try
    EL.SourceFilter:=eFilter.Text;
    EL.ExpandMessages:=cbxExpand.Checked;
    EL.SourceName:=cb.Text;
    et:=GetTickCount64;
    h:=True;
    FCancel:=False;
    Caption:=Format('EventLog Viewer - %d records / %1.2f s',[EL.RecordCount,(GetTickCount64-et)/1000]);

    with lv.Items do begin
      BeginUpdate;
      try
        Clear;
        Update;
        for i:=0 to EL.RecordCount-1 do
          with Add do begin
            Caption:=DatetimeToStr(EL.Records[i].DateTime);
            SubItems.Add(EL.Records[i].Category);
            SubItems.Add(EL.Records[i].Source);
            SubItems.Add(IntToStr(EL.Records[i].EventID));
            SubItems.Add(EL.Records[i].Username);
            SubItems.Add(EL.Records[i].Computer);
            SubItems.Add(EL.Records[i].Description);
            ImageIndex:=Integer(EL.Records[i].EventType);
          end;
      finally
        EndUpdate;
      end;
    end;
  finally
    bAction.Caption:='Refresh';
    bAction.OnClick:=cmRefresh;
    bLoad.Enabled:=True;
    bSave.Enabled:=True;
    cbxExpand.Enabled:=True;
    cb.Enabled:=True;
    eFilter.Enabled:=True;
    lv.Enabled:=True;
    Memo.Enabled:=True;
  end;
  lv.SetFocus;
end;

procedure TForm1.ELReadEventLog(Sender: TObject; ARecord: TLogRecord;
  var Cancel: Boolean);
begin
  Cancel:=FCancel;
  Caption:=Format('EventLog Viewer - %d records',[EL.RecordCount]);
  Application.ProcessMessages;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  EL:=TMiTeC_EventLog.Create(nil);
  EL.OnReadEventLog:=ELReadEventLog;
  for i:=0 to EL.ContainerCount-1 do
    cb.Items.Add(EL.Containers[i].Name);
end;

procedure TForm1.lvSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  Memo.Lines.Text:=Item.SubItems[Item.SubItems.Count-1];
end;

end.
