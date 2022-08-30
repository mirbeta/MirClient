{$INCLUDE ..\..\Compilers.inc}

unit Viewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MiTeC_WbemScripting_TLB, MiTeC_WMI, ComCtrls, ExtCtrls, ActiveX,
  ImgList, StdCtrls, Grids, ActnList, Menus, StdActns;

type
  Tmdi_wmie_Viewer = class(TForm)
    Panel1: TPanel;
    TreeHeader: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    ListHeader: TPanel;
    List: TListView;
    ImageList1: TImageList;
    Splitter2: TSplitter;
    MainMenu1: TMainMenu;
    ActionList1: TActionList;
    acExecute: TAction;
    WQLEditor1: TMenuItem;
    Execute1: TMenuItem;
    acSave: TAction;
    SaveresultsettoCSV1: TMenuItem;
    Panel3: TPanel;
    Editor: TMemo;
    sg: TStringGrid;
    Splitter3: TSplitter;
    WQLHeader: TPanel;
    Image1: TImage;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Tree: TTreeView;
    FavList: TListView;
    sd: TSaveDialog;
    procedure FavListDblClick(Sender: TObject);
    procedure FavListChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure TreeDblClick(Sender: TObject);
    procedure acSaveAccept(Sender: TObject);
    procedure acExecuteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    sl: TStringList;
    FMachine, FRoot: string;
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
  public
    function Connect(AMachine,AUser,APwd,ARoot: string): boolean;
    procedure GetClassProps(AName: string; var AList: TStringList);
    procedure DisplayClassProps(AName: string);
    procedure WMIQuery;
  end;

var
  mdi_wmie_Viewer: Tmdi_wmie_Viewer;

implementation

uses MiTeC_Routines, MiTeC_CtrlRtns, MiTeC_Windows;

{$R *.dfm}

{ Tmdi_wmie_Viewer }

procedure Tmdi_wmie_Viewer.acExecuteExecute(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  try
    WMIQuery;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure Tmdi_wmie_Viewer.acSaveAccept(Sender: TObject);
begin
  if sd.Execute then
  StringGrid_ExportToCSV(sg,sd.FileName);
end;

function Tmdi_wmie_Viewer.Connect;
var
  r: TTreeNode;

procedure EnumClasses(AName: string; ARoot: TTreeNode);
var
  n: TTreeNode;
  wmiObjectSet: ISWbemObjectSet;
  wmiObject: ISWbemObject;
  Enum: IEnumVariant;
  v: OleVariant;
  c: Longword;
begin
  wmiObjectSet:=wmiServices.SubclassesOf(AName,wbemFlagReturnImmediately or wbemQueryFlagShallow,nil);
  if wmiObjectSet.Count=0 then
    Exit;
  Enum:=(wmiObjectSet._NewEnum) as IEnumVariant;
  while (Enum.Next(1,v,c)=S_OK) do begin
    wmiObject:=IUnknown(v) as SWBemObject;
    if (wmiObject.Path_.IsClass) then begin
      n:=Tree.Items.AddChild(ARoot,wmiObject.Path_.Class_);
      n.ImageIndex:=1;
      n.SelectedIndex:=n.ImageIndex;
      if Pos(widestring('Win32_'),wmiObject.Path_.Class_)=1 then
        with FavList.Items.Add do begin
          Caption:=wmiObject.Path_.Class_;
          ImageIndex:=2;
        end;
      EnumClasses(wmiObject.Path_.Class_,n);
    end;
  end;
  WQLheader.Update;
end;

begin
  FMachine:=AMachine;
  FRoot:=ARoot;
  Result:=False;
  wmiLocator:=TSWbemLocator.Create(nil);
  Tree.Items.BeginUpdate;
  FavList.Items.BeginUpdate;
  try
    Tree.Items.Clear;
    try
      r:=Tree.Items.AddChild(nil,ARoot);
      r.ImageIndex:=0;
      r.SelectedIndex:=r.ImageIndex;
      wmiServices:=wmiLocator.ConnectServer(AMachine,ARoot,AUser,APwd,'','',0,nil);
      EnumClasses('',r);
      Tree.AlphaSort({$IFDEF DELPHI6UP} True {$ENDIF});
      r.Expand(False);
      Result:=True;
    except
    end;
  finally
    FavList.Items.EndUpdate;
    Tree.Items.EndUpdate;
    TreeHeader.Caption:=Format('  Classes in namespace: %d',[Tree.Items.Count-1]);
  end;
end;

procedure Tmdi_wmie_Viewer.DisplayClassProps(AName: string);
var
  i: Integer;
begin
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    GetClassProps(AName,sl);
    for i:=0 to sl.Count-1 do
      with List.Items.Add do begin
        if Pos('*',sl.Names[i])=1 then begin
          ImageIndex:=2;
          Caption:=Copy(sl.Names[i],2,255);
        end else begin
          ImageIndex:=-1;
          Caption:=sl.Names[i];
        end;
        SubItems.Add(sl.Values[sl.Names[i]]);
      end;
  finally
    List.Items.EndUpdate;
    ListHeader.Caption:=Format('  Class properties: %d',[List.Items.Count]);
  end;
end;

procedure Tmdi_wmie_Viewer.FavListChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if not Assigned(FavList.Selected) then
    Exit;
  DisplayClassProps(FavList.Selected.Caption);
end;

procedure Tmdi_wmie_Viewer.FavListDblClick(Sender: TObject);
begin
  if not Assigned(FavList.Selected) then
    Exit;
  Editor.Text:='select * from '+FavList.Selected.Caption;
end;

procedure Tmdi_wmie_Viewer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  sl.Free;
  try
    wmiLocator.Free;
  except
  end;
  Action:=caFree;
end;

procedure Tmdi_wmie_Viewer.FormCreate(Sender: TObject);
begin
  pc.ActivePage:=Tabsheet1;
  sl:=TStringList.Create;
  {$IFDEF THEMESUPPORT}
  TreeHeader.ParentBackground:=False;
  WQLHeader.ParentBackground:=False;
  ListHeader.ParentBackground:=False;
  {$ENDIF}
end;

procedure Tmdi_wmie_Viewer.GetClassProps(AName: string; var AList: TStringList);
var
  wmiObject: ISWbemObject;
  wmiProp: ISWbemProperty;
  wmiPropSet: ISWbemPropertySet;
  v: OleVariant;
  n: LongWord;
  Enum: IEnumVariant;
begin
  AList.Clear;
  wmiObject:=wmiServices.Get(AName,wbemFlagUseAmendedQualifiers,nil);
  wmiPropSet:=wmiObject.Properties_;
  Enum:=(wmiPropSet._NewEnum) as IEnumVariant;
  while (Enum.Next(1,v,n)=S_OK) do begin
    wmiProp:=IUnknown(v) as SWBemProperty;
    AList.Add(Format('%s=%s',[wmiProp.Name,wmiGetTypeStr(wmiProp)]))
  end;
end;

procedure Tmdi_wmie_Viewer.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned(Tree.Selected) or (Tree.Selected.Level=0) then
    Exit;
  DisplayClassProps(Tree.Selected.Text);
end;

procedure Tmdi_wmie_Viewer.TreeDblClick(Sender: TObject);
begin
  if not Assigned(Tree.Selected) or (Tree.Selected.Level=0) then
    Exit;
  Editor.Text:='select * from '+Tree.Selected.Text;
end;

procedure Tmdi_wmie_Viewer.WMIQuery;
var
  i,j,c: Integer;
  wmiObjectSet: ISWbemObjectSet;
  wmiObject: ISWbemObject;
  wmiProp: ISWbemProperty;
  propSet: ISWbemPropertySet;
  v: OleVariant;
  n: LongWord;
  propEnum,Enum: IEnumVariant;
  s: string;
  et: comp;
begin
  sg.RowCount:=2;
  sg.ColCount:=2;
  sg.Cells[0,0]:='Executing...';
  sg.Cells[0,1]:='';
  sg.Cells[1,1]:='';
  sg.Cells[1,0]:='';
  sg.Update;
  et:=GetTickCount64;
  try
    if Editor.SelLength>0 then
      s:=Editor.SelText
    else
      s:=Editor.Text;
    try
      if Pos('SELECT',Uppercase(s))=1 then
        wmiObjectSet:=wmiServices.ExecQuery(s,'WQL',wbemFlagReturnImmediately,nil)
      else
        wmiObjectSet:=wmiServices.InstancesOf(s,wbemFlagReturnImmediately or wbemQueryFlagShallow,nil);
      c:=wmiObjectSet.Count;
      if c>0 then
        sg.ColCount:=c+1;
      sg.Cells[0,0]:='Fetching...';
      sg.Update;
      Enum:=(wmiObjectSet._NewEnum) as IEnumVariant;
      i:=1;
      while (Enum.Next(1,v,n)=S_OK) do begin
        wmiObject:=IUnknown(v) as SWBemObject;
        if sg.RowCount<>wmiObject.Properties_.Count+1 then
          sg.RowCount:=wmiObject.Properties_.Count+1;
        propSet:=wmiObject.Properties_;
        sg.Cells[i,0]:=IntToStr(i);
        propEnum:=(propSet._NewEnum) as IEnumVariant;
        j:=1;
        while (propEnum.Next(1,v,n)=S_OK) do begin
          wmiProp:=IUnknown(v) as SWBemProperty;
          sg.Cells[0,j]:=wmiProp.Name;
          sg.Cells[i,j]:=WmiGetPropStr(wmiProp);
          Inc(j);
        end;
        Inc(i);
      end;
      sg.Cells[0,0]:=Format('Instances: %d',[c]);
    except on e: Exception do begin
      sg.Cells[0,0]:=e.Message;
    end end;
  finally
    WQLHeader.Caption:=Format('  WQL [Elapsed time: %1.2f s]',[(GetTickCount64-et)/1000]);
  end;
end;

end.
