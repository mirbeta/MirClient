{$INCLUDE ..\..\Compilers.inc}

unit Main;

interface

uses
  {$IFDEF D7PLUS}XPMan,{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MiTeC_ActiveDs_TLB, MiTeC_ADSI, MSI_AD, StdCtrls, ExtCtrls, ComCtrls,
  ImgList, MSI_Common;

type
  Twnd_ade_Main = class(TForm)
    Panel1: TPanel;
    Label2: TLabel;
    eUser: TEdit;
    Label3: TLabel;
    ePwd: TEdit;
    Button1: TButton;
    eADDB: TEdit;
    Label4: TLabel;
    List: TListView;
    sb: TStatusBar;
    Splitter1: TSplitter;
    od: TOpenDialog;
    sd: TSaveDialog;
    bSave: TButton;
    bLoad: TButton;
    ilAD: TImageList;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Tree: TTreeView;
    HTree: TTreeView;
    Panel2: TPanel;
    Panel3: TPanel;
    Button2: TButton;
    TabSheet3: TTabSheet;
    GTree: TTreeView;
    ed: TSaveDialog;
    pTitle: TPanel;
    Bevel1: TBevel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    Memo: TMemo;
    eProp: TEdit;
    TabSheet4: TTabSheet;
    pcA: TPageControl;
    TabSheet8: TTabSheet;
    ETree: TTreeView;
    TabSheet5: TTabSheet;
    LTree: TTreeView;
    TabSheet6: TTabSheet;
    DTree: TTreeView;
    EL: TMemo;
    Panel5: TPanel;
    Label1: TLabel;
    eSearch: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure cmConnect(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure bSaveClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure pcChange(Sender: TObject);
    procedure ListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormShow(Sender: TObject);
    procedure TreeKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure ADDebug(Sender: TMiTeC_Component; Msg: string);
    procedure eSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ePwdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    ad: TMiTeC_AD;
    sl: TStringList;
  public
    procedure RefreshData;
    procedure DisplayData;
    procedure DoSearch(AText: string);
  end;

var
  wnd_ade_Main: Twnd_ade_Main;

implementation

uses MiTeC_Routines, MiTeC_Datetime, MiTeC_StrUtils, MiteC_CtrlRtns, MiTeC_Dialogs;

{$R *.dfm}

procedure Twnd_ade_Main.FormCreate(Sender: TObject);
begin
  sl:=TStringList.Create;
  sl.Delimiter:=';';
  {$IFDEF BDS3PLUS}
  sl.StrictDelimiter:=True;
  Memo.Lines.Delimiter:=',';
  Memo.Lines.StrictDelimiter:=True;
  {$ENDIF}
  ad:=TMiTeC_AD.Create(nil);
  sb.Panels[sb.Panels.Count-1].Text:=ModuleInfo.Copyright+'      ';
  pc.ActivePageIndex:=0;
end;

procedure Twnd_ade_Main.FormShow(Sender: TObject);
begin
  //cmConnect(nil);
end;

procedure Twnd_ade_Main.HTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  idx,uac: Integer;  
begin
  if not Assigned(Node.Data) then
    Exit;
  idx:=PInteger(Node.Data)^;
  uac:=StrToIntDef(GetProp(ad.Users[idx],'userAccountControl').Value,0);
  if uac and ADS_UF_ACCOUNTDISABLE>0 then
    Sender.Canvas.Font.Color:=clGray;
end;

procedure Twnd_ade_Main.ListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  i: Integer;
begin
  Memo.Lines.Clear;
  if not Assigned(Item) then
    Exit;
  eProp.Text:=Item.Caption;
  if (Pos(',',Item.SubItems[0])>0) and (Pos('DC=',Item.SubItems[0])=0) then
    Memo.Lines.DelimitedText:=Item.SubItems[0]
  else if SameText(Item.Caption,'userAccountControl') then begin
    i:=StrToIntDef(Item.SubItems[0],0);
    if i and ADS_UF_ACCOUNTDISABLE>0 then
      Memo.Lines.Add('- Account disabled');
    if i and ADS_UF_HOMEDIR_REQUIRED>0 then
      Memo.Lines.Add('- HomeDir required');
    if i and ADS_UF_PASSWD_NOTREQD=0 then
      Memo.Lines.Add('- Password required');
    if i and ADS_UF_LOCKOUT>0 then
      Memo.Lines.Add('- Account locked');
    if i and ADS_UF_PASSWD_CANT_CHANGE>0 then
      Memo.Lines.Add('- User cannot change the password');
    if i and ADS_UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED>0 then
      Memo.Lines.Add('- User can send an encrypted password');
    if i and ADS_UF_TEMP_DUPLICATE_ACCOUNT>0 then
      Memo.Lines.Add('- Account for users whose primary account is in another domain');
    if i and ADS_UF_NORMAL_ACCOUNT>0 then
      Memo.Lines.Add('- Default account type that represents a typical user');
    if i and ADS_UF_INTERDOMAIN_TRUST_ACCOUNT>0 then
      Memo.Lines.Add('- Permit to trust account for a system domain that trusts other domains');
    if i and ADS_UF_WORKSTATION_TRUST_ACCOUNT>0 then
      Memo.Lines.Add('- Computer account for a Microsoft Windows NT Workstation/Windows 2000 Professional or Windows NT Server/Windows 2000 Server that is a member of this domain');
    if i and ADS_UF_SERVER_TRUST_ACCOUNT>0 then
      Memo.Lines.Add('- Computer account for a system backup domain controller that is a member of this domain');
    if i and ADS_UF_DONT_EXPIRE_PASSWD>0 then
      Memo.Lines.Add('- Password will not expire');
    if i and ADS_UF_MNS_LOGON_ACCOUNT>0 then
      Memo.Lines.Add('- Majority Node Set (MNS) logon account');
    if i and ADS_UF_SMARTCARD_REQUIRED>0 then
      Memo.Lines.Add('- User is forced to log on using a smart card');
    if i and ADS_UF_TRUSTED_FOR_DELEGATION>0 then
      Memo.Lines.Add('- Service account trusted for Kerberos delegation');
    if i and ADS_UF_NOT_DELEGATED>0 then
      Memo.Lines.Add('- Security context of the user will not be delegated to a service');
    if i and ADS_UF_USE_DES_KEY_ONLY>0 then
      Memo.Lines.Add('- Restrict this principal to use only Data Encryption Standard (DES) encryption types for keys');
    if i and ADS_UF_DONT_REQUIRE_PREAUTH>0 then
      Memo.Lines.Add('- Account does not require Kerberos preauthentication for logon');
    if i and ADS_UF_PASSWORD_EXPIRED>0 then
      Memo.Lines.Add('- User password has expired');
    if i and ADS_UF_TRUSTED_TO_AUTHENTICATE_FOR_DELEGATION>0 then
      Memo.Lines.Add('- Account is enabled for delegation');
  end else
    Memo.Lines.Text:=Item.SubItems[0];

end;

procedure Twnd_ade_Main.pcChange(Sender: TObject);
begin
  case pc.ActivePageIndex of
    0: TreeChange(Tree,Tree.Selected);
    1: TreeChange(HTree,HTree.Selected);
    2: TreeChange(GTree,GTree.Selected);
    3: case pcA.ActivePageIndex of
         0: TreeChange(ETree,ETree.Selected);
         1: TreeChange(LTree,LTree.Selected);
         2: TreeChange(DTree,DTree.Selected);
       end;
  end;
end;

procedure Twnd_ade_Main.RefreshData;
begin
  Screen.Cursor:=crHourglass;
  try
    ad.ADDatabase:=eADDB.Text;
    if ad.ADDatabase='' then begin
      ad.RefreshDomain;
      ad.ADDatabase:=GetName(ad.Domain);
    end;
    ad.User:=eUser.Text;
    ad.Password:=ePwd.Text;
    ad.RefreshData;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_ade_Main.TreeChange(Sender: TObject; Node: TTreeNode);
var
  i: Integer;
  s: string;
  ado: TADObject;
begin
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    Memo.Lines.Clear;
    eProp.Text:='';
    if not Assigned(Node) then
      Exit;
    List.SortType:=stText;
    case Node.ImageIndex of
      1: with List.Items.Add do begin
           pTitle.Caption:=Tree.Selected.Text;
           Caption:='Count';
           SubItems.Add(IntToStr(Node.Count));
         end;
      0,2..4: begin
         case Node.ImageIndex of
           0: ado:=ad.Domain;
           2: ado:=ad.Users[PInteger(Node.Data)^];
           3: ado:=ad.Computers[PInteger(Node.Data)^];
           4: ado:=ad.Groups[PInteger(Node.Data)^];
         end;
         if (Node.ImageIndex=4) and (pc.ActivePageIndex=2) then begin
           pTitle.Caption:=CreatePathFromDN(GetDN(ado));
           sl.DelimitedText:=GetPropAsDelimitedText(ado,'member');
           for i:=0 to sl.Count-1 do
             with List.Items.Add do begin
               Caption:=GetDNPart(sl[i],'CN');
               SubItems.Add(sl[i]);
             end;
         end else begin
           pTitle.Caption:=Format('%s (%d items)',[GetName(ado),Length(ado.Props)]);
           for i:=0 to High(ado.Props) do
             with List.Items.Add do begin
               Caption:=ado.Props[i].Name;
               s:=ado.Props[i].Value;
               if ado.Props[i].Typ=ADSTYPE_LARGE_INTEGER then
                 s:=Format('%s = %s',[s,DateTimeToStr(Int64ToDatetime(ado.Props[i].Value))]);
               SubItems.Add(s);
             end;
         end;
      end;
    end;
  finally
    List.Items.Endupdate;
  end;
end;

procedure Twnd_ade_Main.TreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
    Dispose(Node.Data);
end;

procedure Twnd_ade_Main.TreeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift=[ssCtrl]) then begin
    case Key of
      vk_prior: TTreeView(Sender).FullCollapse;
      vk_next: TTreeView(Sender).FullExpand;
    end;
  end;
end;

procedure Twnd_ade_Main.ADDebug(Sender: TMiTeC_Component; Msg: string);
begin
  ;
end;

procedure Twnd_ade_Main.bLoadClick(Sender: TObject);
var
  h: Boolean;
begin
  if not od.Execute then
    Exit;
  h:=True;
  AD.LoadFromStorage(od.FileName,h);
  DisplayData;
end;

procedure Twnd_ade_Main.bSaveClick(Sender: TObject);
var
  h: Boolean;
begin
  sd.FileName:=ChangeFileExt(GetName(ad.Domain),'.sif');
  if not sd.Execute then
    Exit;
  h:=True;
  AD.SaveToStorage(sd.FileName,h);
end;

procedure Twnd_ade_Main.cmConnect(Sender: TObject);
begin
  sb.Panels[0].Text:='Working...please wait';
  Screen.Cursor:=crHourglass;
  Update;
  try
    SetADLastError;
    RefreshData;
    Update;
    DisplayData;
    {$IFDEF DEBUG}
    if Trim(AD_LastError)<>'' then
      EL.Text:=EL.Text+#13#10+AD_LastError;
    EL.Visible:=EL.Lines.Count>0;
    {$ENDIF}
    try Tree.SetFocus except end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Twnd_ade_Main.Button2Click(Sender: TObject);
var
  i,idx,uac: Integer;
begin
  try
    case pc.ActivePageIndex of
      0: ed.FileName:=ChangeFileExt(Tree.Selected.Text,'.csv');
      1: ed.FileName:=ChangeFileExt(HTree.Selected.Text,'.csv');
      2: ed.FileName:=ChangeFileExt(GTree.Selected.Text,'.csv');
      3: case pcA.ActivePageIndex of
         0: ed.FileName:=ChangeFileExt(ETree.Selected.Text,'.csv');
         1: ed.FileName:=ChangeFileExt(LTree.Selected.Text,'.csv');
         2: ed.FileName:=ChangeFileExt(DTree.Selected.Text,'.csv');
       end;
    end;
  except
  end;
  if not ed.Execute then
    Exit;
  ListView_ExportToCSV(List,ed.FileName);
  if pc.ActivePageIndex=1 then begin
    sl.Clear;
    for i:=0 to HTree.Items.Count-1 do begin
      idx:=PInteger(HTree.Items[i].Data)^;
      uac:=StrToIntDef(GetProp(ad.Users[idx],'userAccountControl').Value,0);
      if uac and ADS_UF_ACCOUNTDISABLE=0 then 
        sl.Add(StringOfChar(' ',HTree.Items[i].Level*2)+HTree.Items[i].Text);
    end;
    sl.SaveToFile(ExtractFilepath(ed.FileName)+'Hierarchy.txt');
  end;
end;

procedure Twnd_ade_Main.DisplayData;

procedure CreateHierarchyPath(ACN: string; var APath: string);
var
  i: Integer;
begin
  if ACN='' then
    Exit;
  APath:=IncludeTrailingPathDelimiter(ACN)+APath;
  for i:=0 to ad.UserCount-1 do
    if SameText(ACN,GetName(ad.Users[i])) then begin
      CreateHierarchyPath(GetDNPart(GetProp(ad.Users[i],'manager').Value,'CN'),APath);
      Break;
    end;
end;

var
  i,uac: Integer;
  n: TTreeNode;
  pi: PInteger;
  s,cn,mn: string;
  dt: TDateTime;
begin
  sl.Clear;
  Tree.OnChange:=nil;
  HTree.OnChange:=nil;
  GTree.OnChange:=nil;
  ETree.OnChange:=nil;
  LTree.OnChange:=nil;
  DTree.OnChange:=nil;

  Tree.Items.BeginUpdate;
  HTree.Items.BeginUpdate;
  GTree.Items.BeginUpdate;
  ETree.Items.BeginUpdate;
  DTree.Items.BeginUpdate;
  LTree.Items.BeginUpdate;
  try
    Tree.Items.Clear;
    HTree.Items.Clear;
    ETree.Items.Clear;
    LTree.Items.Clear;
    DTree.Items.Clear;
    for i:=0 to ad.UserCount-1 do begin
      s:=CreatePathFromDN(GetDN(ad.Users[i]));
      n:=Tree_CreateNodeByPath(Tree,s,nil,1);
      n.ImageIndex:=2;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;

      cn:=GetName(ad.Users[i]);

      mn:=GetDNPart(GetProp(ad.Users[i],'manager').Value,'CN');

      if (Pos('\',cn)>0) then
        Continue;

      if not SameText(cn,mn) then begin
        s:=cn;
        CreateHierarchyPath(mn,s);
        n:=Tree_CreateNodeByPath(HTree,IncludeTrailingPathDelimiter(s),nil,3);
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=2;
        n.SelectedIndex:=n.ImageIndex;
      end;  

      dt:=Int64ToDatetime(GetProp(ad.Users[i],'accountExpires').Value);
      if (dt>21916) and (dt<=date) then begin
        n:=ETree.Items.AddChild(nil,GetName(ad.Users[i]));
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=2;
        n.SelectedIndex:=n.ImageIndex;
      end;

      uac:=StrToIntDef(GetProp(ad.Users[i],'userAccountControl').Value,0);
      if uac and ADS_UF_LOCKOUT>0 then begin
        n:=LTree.Items.AddChild(nil,GetName(ad.Users[i]));
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=2;
        n.SelectedIndex:=n.ImageIndex;
      end;
      if uac and ADS_UF_ACCOUNTDISABLE>0 then begin
        n:=DTree.Items.AddChild(nil,GetName(ad.Users[i]));
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=2;
        n.SelectedIndex:=n.ImageIndex;
      end;
    end;

    for i:=0 to ad.ComputerCount-1 do begin
      s:=CreatePathFromDN(GetDN(ad.Computers[i]));
      n:=Tree_CreateNodeByPath(Tree,s,nil,1);
      n.ImageIndex:=3;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;

      dt:=Int64ToDatetime(GetProp(ad.Computers[i],'accountExpires').Value);
      if (dt>21916) and (dt<date) then begin
        n:=ETree.Items.AddChild(nil,GetName(ad.Computers[i]));
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=3;
        n.SelectedIndex:=n.ImageIndex;
      end;

      uac:=StrToIntDef(GetProp(ad.Computers[i],'ComputerAccountControl').Value,0);
      if uac and ADS_UF_LOCKOUT>0 then begin
        n:=LTree.Items.AddChild(nil,GetName(ad.Computers[i]));
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=3;
        n.SelectedIndex:=n.ImageIndex;
      end;
      if uac and ADS_UF_ACCOUNTDISABLE>0 then begin
        n:=DTree.Items.AddChild(nil,GetName(ad.Computers[i]));
        new(pi);
        pi^:=i;
        n.Data:=pi;
        n.ImageIndex:=3;
        n.SelectedIndex:=n.ImageIndex;
      end;
    end;
    
    GTree.Items.Clear;
    for i:=0 to ad.GroupCount-1 do begin
      s:=CreatePathFromDN(GetDN(ad.Groups[i]));
      n:=Tree_CreateNodeByPath(Tree,s,nil,1);
      n.ImageIndex:=4;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;

      n:=GTree.Items.AddChild(nil,GetName(ad.Groups[i]));
      n.ImageIndex:=4;
      n.SelectedIndex:=n.ImageIndex;
      new(pi);
      pi^:=i;
      n.Data:=pi;
    end;

    GTree.AlphaSort(True);
    GTree.Selected:=GTree.Items.GetFirstNode;

    ETree.AlphaSort(True);
    ETree.Selected:=ETree.Items.GetFirstNode;

    LTree.AlphaSort(True);
    LTree.Selected:=LTree.Items.GetFirstNode;

    DTree.AlphaSort(True);
    DTree.Selected:=DTree.Items.GetFirstNode;

    HTree.AlphaSort(True);
    HTree.FullExpand;
    HTree.Selected:=HTree.Items.GetFirstNode;

    Tree.Selected:=Tree.Items.GetFirstNode;
    if Tree.Selected=nil then
      Exit;
    Tree.Selected.ImageIndex:=0;
    Tree.Selected.SelectedIndex:=Tree.Selected.ImageIndex;
    TreeChange(Tree,Tree.Selected);
    Tree.Selected.AlphaSort(True);
    Tree.Selected.Expand(False);
  finally
    Tree.Items.EndUpdate;
    HTree.Items.EndUpdate;
    GTree.Items.EndUpdate;
    ETree.Items.EndUpdate;
    DTree.Items.EndUpdate;
    LTree.Items.EndUpdate;

    Tree.OnChange:=TreeChange;
    HTree.OnChange:=TreeChange;
    GTree.OnChange:=TreeChange;
    ETree.OnChange:=TreeChange;
    DTree.OnChange:=TreeChange;
    LTree.OnChange:=TreeChange;
    sb.Panels[0].Text:=Format('Users: %d   Computers: %d   Groups: %d',[ad.UserCount,ad.ComputerCount,ad.GroupCount]);
  end;
end;

procedure Twnd_ade_Main.DoSearch(AText: string);
var
  i: Integer;
  b: Boolean;
begin
  try
    if AText='' then
      Exit;
    b:=False;
    for i:=0 to Tree.Items.Count-1 do
      if PosText(AText,Tree.Items[i].Text)>0 then begin
        Tree.Items[i].MakeVisible;
        Tree.Selected:=Tree.Items[i];
        b:=True;
        Break;
      end;
    if (AText<>'') and not b then
      Info('Text not found');
  finally
    try Tree.SetFocus except end;
  end;
end;

procedure Twnd_ade_Main.ePwdKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift=[]) and (Key=vk_return) then
    cmConnect(nil);
end;

procedure Twnd_ade_Main.eSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Shift=[]) and (Key=vk_return) then
    DoSearch(Trim(eSearch.Text));
end;

end.
