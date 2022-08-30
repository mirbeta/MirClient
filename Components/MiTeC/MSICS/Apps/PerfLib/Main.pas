unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, ImgList, Menus, MiTeC_WinPerf, MiTeC_Pdh,
  MSI_PerfMon;

const
  WM_START = WM_USER+1;

type
  TfrmMain = class(TForm)
    il: TImageList;
    tvPerf: TTreeView;
    Panel1: TPanel;
    Label1: TLabel;
    eServer: TEdit;
    Button1: TButton;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Memo: TMemo;
    Splitter2: TSplitter;
    ePath: TEdit;
    lInfo: TLabel;
    eType: TEdit;
    lvPerf: TListView;
    eEPath: TEdit;
    procedure tvPerfChange(Sender: TObject; Node: TTreeNode);
    procedure cmRefresh(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvPerfDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMachine: string;
    LocalHelp,EngHelp: TStringList;
  protected
    procedure WMSTART(var AMsg: TMessage); message WM_START;
  public
    function GetLocalizedPerfCounterHelp(AIndex: Cardinal): string;
    function GetEnglishPerfCounterHelp(AIndex: Cardinal): string;
    procedure RefreshObjects(const AMachine: string = '');
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses MiTeC_Routines, MiTeC_StrUtils, Clipbrd, Math;

procedure TfrmMain.RefreshObjects(const AMachine: string);
var
  ro,rc,ri: TTreeNode;
  so,sc,si :string;
  no,nc,ni,r,c: Cardinal;
  po,pc,pi,m: PChar;
  em: {$IFDEF RAD9PLUS}TArithmeticExceptionMask{$ELSE}TFPUExceptionMask{$ENDIF};
begin
  FMachine:=AMachine;
  if AMachine='' then
    m:=nil
  else
    m:=PChar(AMachine);
  no:=0;

  em:=GetExceptionMask;
  SetExceptionMask({$IFDEF RAD9PLUS}exAllArithmeticExceptions{$ELSE}[exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]{$ENDIF});
  PdhEnumObjects(nil,m,nil,no,PERF_DETAIL_WIZARD,True);
  SetExceptionMask(em);

  SetLength(so,no);
  if PdhEnumObjects(nil,m,PChar(so),no,PERF_DETAIL_WIZARD,False)<>ERROR_SUCCESS then
    Exit;

  Memo.Clear;
  lvPerf.Items.Clear;
  eType.Text:='';
  ePath.Text:='';
  eEPath.Text:='';
  tvPerf.Items.BeginUpdate;
  try
    tvPerf.Items.Clear;
    with tvPerf.Items do begin
      po:=@so[1];
      while po^<>#0 do begin
        ro:=Add(nil,po);
        ro.imageindex:=0;
        ro.selectedindex:=ro.imageindex;
        nc:=0;
        ni:=0;
        PdhEnumObjectItems(nil,m,po,nil,nc,nil,ni,PERF_DETAIL_WIZARD,0);
        SetLength(sc,nc);
        SetLength(si,ni);
        r:=PdhEnumObjectItems(nil,m,po,PChar(sc),nc,PChar(si),ni,PERF_DETAIL_WIZARD,0);
        if r=ERROR_SUCCESS then begin
          c:=0;
          if ni>0 then begin
            pi:=@si[1];
            while pi^<>#0 do begin
              Inc(c);
              Inc(pi,Length(pi)+1);
            end;
            ri:=AddChild(ro,Format('Number of instances: %d',[c]));
            ri.imageindex:=1;
            ri.selectedindex:=ri.imageindex;
          end;
          pc:=@sc[1];
          while pc^<>#0 do begin
            rc:=AddChild(ro,pc);
            rc.imageindex:=2;
            rc.selectedindex:=rc.imageindex;
            Inc(pc,Length(pc)+1);
          end;
        end;
        Inc(po,Length(po)+1);
        Finalize(sc);
        Finalize(si);
      end;
    end;
  finally
    tvPerf.Items.EndUpdate;
  end;
  Memo.Clear;
  lvPerf.Items.Clear;
end;

procedure TfrmMain.cmRefresh(Sender: TObject);
begin
  Screen.Cursor:=crHourGlass;
  lInfo.Show;
  Update;
  try
    RefreshObjects(eServer.Text);
  finally
    lInfo.Hide;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LocalHelp:=TStringList.Create;
  EngHelp:=TStringList.Create;
  TPerfMonThread.ReadPerfTexts(True,True,LocalHelp);
  TPerfMonThread.ReadPerfTexts(False,True,EngHelp);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  PostMessage(Handle,WM_START,0,0);
end;

function TfrmMain.GetEnglishPerfCounterHelp(AIndex: Cardinal): string;
var
  idx: Integer;
begin
  Result:='';
  idx:=EngHelp.IndexOfName(IntToStr(AIndex));
  if idx>-1 then
    Result:=EngHelp.ValueFromIndex[idx];
end;

function TfrmMain.GetLocalizedPerfCounterHelp(AIndex: Cardinal): string;
var
  idx: Integer;
begin
  Result:='';
  idx:=LocalHelp.IndexOfName(IntToStr(AIndex));
  if idx>-1 then
    Result:=LocalHelp.ValueFromIndex[idx];
end;


procedure TfrmMain.lvPerfDblClick(Sender: TObject);
begin
  if not Assigned(lvPerf.Selected) then
    Exit;
  Clipboard.AsText:=lvPerf.Selected.SubItems[0];
end;

procedure TfrmMain.tvPerfChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
  hca: array of PDH_HCOUNTER;
  s,sc,si,v,fp,cp,cn,mn,t: string;
  idx,nc,ni,n,c,ct: Cardinal;
  p,m: PChar;
  hq: PDH_HQUERY;
  hc: PDH_HCOUNTER;
  ci: PPdhCounterInfo;
  cpe: TPdhCounterPathElements;
  cv: TPdhFmtCounterValue;
  r: PDH_STATUS;
begin
  if not Assigned(Node) then
    Exit;
  Screen.Cursor:=crHourGlass;
  try
    if FMachine='' then begin
      m:=nil;
      mn:='';
    end else begin
      m:=PChar(FMachine);
      mn:='\\'+FMachine;
    end;
    cp:='';
    lvPerf.Items.Clear;
    Memo.Clear;
    eType.Clear;
    ePath.Clear;
    eEPath.Clear;
    nc:=0;
    ni:=0;
    cp:='';
    if Node.Level=1 then
      s:=Node.Parent.Text
    else
      s:=Node.Text;
    PdhEnumObjectItems(nil,m,PChar(s),nil,nc,nil,ni,PERF_DETAIL_WIZARD,0);
    SetLength(sc,nc);
    SetLength(si,ni);
    if PdhEnumObjectItems(nil,m,PChar(s),PChar(sc),nc,PChar(si),ni,PERF_DETAIL_WIZARD,0)<>ERROR_SUCCESS then
      Exit;

    c:=0;
    n:=0;
    if (ni>0) then begin
      p:=@si[1];
      while p^<>#0 do begin
        v:=p;
        with lvPerf.Items.Add do begin
          Caption:=v;
          SubItems.Add('');
        end;
        Inc(c);
        Inc(p,Length(p)+1);
      end;
    end;
    if (c=0) and (ni>0) then
      c:=ni;

    if (Node.ImageIndex=0) then begin
      if PdhLookupPerfIndexByName(m,PChar(s),idx)=ERROR_SUCCESS then begin
        t:=GetLocalizedPerfCounterHelp(idx+1);
        Memo.Text:=t;
      end;
    end else if (Node.ImageIndex=2) then begin
      PdhOpenQuery(nil,0,hq);
      if c>0 then
        cp:=mn+Format('\%s(*)\%s',[Node.Parent.Text,Node.Text])
      else
        cp:=mn+Format('\%s\%s',[Node.Parent.Text,Node.Text]);
      try
        r:=PdhAddCounter(hq,PChar(cp),0,hc);
        if r=ERROR_SUCCESS then begin
          r:=PdhCollectQueryData(hq);
          n:=0;
          PdhGetCounterInfo(hc,True,n,nil);
          ci:=AllocMem(n);
          try
            if PdhGetCounterInfo(hc,True,n,ci)=ERROR_SUCCESS then begin
              if Assigned(ci^.szExplainText) then
                Memo.Lines.Add(ci^.szExplainText);
              cpe.szMachineName:=ci^.Union.szMachineName;
              cpe.szObjectName:=ci^.Union.szObjectName;
              cpe.szInstanceName:=ci^.Union.szInstanceName;
              cpe.szParentInstance:=ci^.Union.szParentInstance;
              cpe.dwInstanceIndex:=DWORD(-1);
              cpe.szCounterName:=ci^.Union.szCounterName;
              n:=0;
              PdhMakeCounterPath(@cpe,nil,n,0);
              SetLength(fp,n);
              PdhMakeCounterPath(@cpe,PChar(fp),n,0);
              eType.Text:=GetCounterTypeStr(ci.dwType);
              ct:=ci.dwType;
            end;
            if c=0 then begin
              sleep(300);
              PdhCollectQueryData(hq);
              v:='';
              if (PdhGetFormattedCounterValue(hc,PDH_FMT_DOUBLE,@ct,cv)=ERROR_SUCCESS) and (cv.CStatus<>PDH_CSTATUS_INVALID_DATA) then
                v:=Format('%1.6n',[cv.doubleValue]);
              with lvPerf.Items.Add do begin
                Caption:='(none)';
                SubItems.Add(v);
              end;
            end;
          finally
            if Assigned(ci) then
              Freemem(ci);
          end;
        end else
            fp:=cp;
        ePath.Text:=fp;
        eEPath.Text:=TPerfMonThread.GetEnglishPerfCounterName(fp);
        PdhRemoveCounter(hc);
        if (c>0) and (r=ERROR_SUCCESS) then begin
          SetLength(hca,c);
          try
            for i:=0 to lvPerf.Items.Count-1 do begin
              cn:=mn+Format('\%s(%s)\%s',[Node.Parent.Text,lvPerf.Items[i].Caption,Node.Text]);
              PdhAddCounter(hq,PChar(cn),0,hca[i]);
            end;
            PdhCollectQueryData(hq);
            sleep(300);
            PdhCollectQueryData(hq);
            for i:=0 to High(hca) do begin
              v:='';
              if (PdhGetFormattedCounterValue(hca[i],PDH_FMT_DOUBLE,@ct,cv)=ERROR_SUCCESS) and (cv.CStatus<>PDH_CSTATUS_INVALID_DATA) then
                v:=Format('%1.6n',[cv.doubleValue]);
              lvPerf.Items[i].SubItems[0]:=v;
            end;
            for i:=0 to High(hca) do
              PdhRemoveCounter(hca[i]);
          finally
            Finalize(hca);
          end;
        end;
      finally
        PdhCloseQuery(hq);
      end;
      if (Memo.Lines.Count=0) and (PdhLookupPerfIndexByName(m,PChar(Node.Text),idx)=ERROR_SUCCESS) then begin
        t:=GetLocalizedPerfCounterHelp(idx+1);
        Memo.Text:=t;
      end;
    end;
    Memo.SelStart:=0;
    Memo.SelLength:=0;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TfrmMain.WMSTART(var AMsg: TMessage);
begin
  cmRefresh(nil);
end;

end.
