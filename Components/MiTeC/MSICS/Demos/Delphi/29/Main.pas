unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ShellCtrls, ExtCtrls;

type
  Twnd_gai_Main = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    MACMemo: TMemo;
    eMachine: TEdit;
    Button1: TButton;
    Icon: TImage;
    Label3: TLabel;
    eOS: TEdit;
    Label4: TLabel;
    eGroup: TEdit;
    lCopy: TLabel;
    Label6: TLabel;
    IPMemo: TMemo;
    ShareMemo: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    eUser: TEdit;
    Label5: TLabel;
    eDT: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    eDesc: TEdit;
    FlagMemo: TMemo;
    procedure cmGet(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    sl: TStringList;
  public
  end;

var
  wnd_gai_Main: Twnd_gai_Main;

implementation

uses MiTeC_NetUtils, MiTeC_NetAPI32, MiTeC_Routines, MiTeC_StrUtils, MiTeC_IpHlpAPI;

{$R *.dfm}

procedure DetectShares(AName: string; AList: TStrings);
var
  i: Integer;
  sr: TSharedResources;
  s: string;
begin
  AList.Clear;

  AList.Add('Name                       Comment                                   Type          ');
  AList.Add('-----------------------------------------------------------------------------------');

  ShareDetect(AName,sr);

  for i:=0 to High(sr) do begin
    case sr[i].Typ of
      STYPE_DISKTREE: s:='Disk';
      STYPE_PRINTQ: s:='Print';
      STYPE_DEVICE: s:='Device';
      STYPE_IPC: s:='IPC';
      STYPE_SPECIAL: s:='Special';
    end;
    AList.Add(Format('%-25s  %-40s  %s',[sr[i].Name,sr[i].Comment,s]));
  end;
end;

procedure Twnd_gai_Main.cmGet(Sender: TObject);
var
  f,osmaj,osmin,osp: Cardinal;
  dt: TDateTime;
  s,m,d,u,mac: string;
  data: TNetObject;
begin
  Screen.Cursor:=crHourglass;
  ShareMemo.Lines.Clear;
  Update;
  eDesc.Text:='';
  eOS.Text:=d;
  eDT.Text:=u;
  Flagmemo.Lines.Clear;
  Sharememo.Lines.Clear;
  try
    s:=eMachine.Text;
    DetectNetObject(s,data,False,False,True);

    MACMemo.Lines.CommaText:=data.MACAddress;
    IPMemo.Lines.CommaText:=data.IPv4Address;
    eGroup.Text:=data.Domain;
    eOS.Text:=data.OS;
    eDesc.Text:=data.Desc;
    eDT.Text:=DateTimeToStr(data.RemoteTime);
    eUser.Text:=data.User;
    Flagmemo.Lines.CommaText:=GetServerTypeFlagString(data.Flags);
    DetectShares(s,Sharememo.Lines);
    ShareMemo.SelStart:=0;
    ShareMemo.SelLength:=0;
  finally
    eMachine.SetFocus;
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_gai_Main.FormCreate(Sender: TObject);
begin
  sl:=TStringList.Create;
  Caption:=Format('%s %s',[EXEVersionInfo.Description,ModuleInfo.FileVersion]);
  lCopy.Caption:=EXEVersionInfo.Copyright;
  Icon.Picture.Icon.Handle:=Application.Icon.Handle;
  InitIpHlpAPI;
end;

procedure Twnd_gai_Main.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[]) and (Key=vk_escape) then
    Close;
end;

end.
